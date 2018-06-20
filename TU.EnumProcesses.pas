unit TU.EnumProcesses;

interface

type
  TProcessItem = record
    ImageName: String;
    PID: Cardinal;
    ParentPID: Cardinal;
  end;

  TProcessList = class
  private
    Buffer: Pointer;
    FCount: integer;
    FItems: array of TProcessItem;
    function GetItem(i: integer): TProcessItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: integer read FCount;
    property Items[i: integer]: TProcessItem read GetItem; default;
  end;

implementation

uses Winapi.Windows;

{$MINENUMSIZE 4}

type
  TByteArray = array [Word] of Byte;
  PByteArray = ^TByteArray;

  NTSTATUS = LongWord;

  UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWChar;
  end;

  TSystemProcessInformation = record
    NextEntryOffset: ULONG;
    NumberOfThreads: ULONG;
    Reserved: array [0 .. 5] of LARGE_INTEGER;
    ImageName: UNICODE_STRING;
    BasePriority: ULONG;
    ProcessId: THandle;
    InheritedFromProcessId: THandle;
  end;

  PSystemProcessInformation = ^TSystemProcessInformation;

  TSystemInformationClass = (SystemProcessInformation = 5);

function NtQuerySystemInformation(SystemInformationClass
  : TSystemInformationClass; SystemInformation: PVOID;
  SystemInformationLength: ULONG; var ReturnLength: ULONG): NTSTATUS; stdcall;
  external 'ntdll.dll' name 'NtQuerySystemInformation';

const
  STATUS_SUCCESS = $00000000;
  STATUS_INFO_LENGTH_MISMATCH = $C0000004;
  STATUS_BUFFER_TOO_SMALL = $C0000023;

  { TProcessList }

constructor TProcessList.Create;
var
  BufferSize: NativeUInt;
  ReturnLength: Cardinal;
  status: NTSTATUS;
  SysInfo: PSystemProcessInformation;
  i: integer;
begin
  FCount := 0;
  ReturnLength := 0;
  NtQuerySystemInformation(SystemProcessInformation, nil, 0, ReturnLength);
  BufferSize := ReturnLength;
  Buffer := AllocMem(BufferSize);

  while True do
  begin
    status := NtQuerySystemInformation(SystemProcessInformation, Buffer,
      BufferSize, ReturnLength);
    if (status = STATUS_BUFFER_TOO_SMALL) or
      (status = STATUS_INFO_LENGTH_MISMATCH) then
    begin
      FreeMem(Buffer);
      BufferSize := ReturnLength;
      Buffer := AllocMem(BufferSize);
    end
    else
      Break;
  end;

  if status <> STATUS_SUCCESS then
  begin
    FreeMem(Buffer);
    Exit;
  end;

  SysInfo := PSystemProcessInformation(Buffer);
  while True do
  begin
    Inc(FCount);
    if SysInfo.NextEntryOffset = 0 then
      Break;
    SysInfo := PSystemProcessInformation(@PByteArray(SysInfo)
      [SysInfo.NextEntryOffset]);
  end;

  i := 0;
  SysInfo := PSystemProcessInformation(Buffer);
  SetLength(FItems, FCount);
  while True do
  begin
    SetString(FItems[i].ImageName, SysInfo.ImageName.Buffer,
      SysInfo.ImageName.Length div SizeOf(WChar));
    if FItems[i].ImageName = '' then
      FItems[i].ImageName := 'System Idle Process';

    FItems[i].PID := SysInfo.ProcessId;
    FItems[i].ParentPID := SysInfo.InheritedFromProcessId;

    if SysInfo.NextEntryOffset = 0 then
      Break;

    SysInfo := PSystemProcessInformation(@PByteArray(SysInfo)
      [SysInfo.NextEntryOffset]);
    Inc(i);
  end;
end;

destructor TProcessList.Destroy;
begin
  FreeMem(Buffer);
end;

function TProcessList.GetItem(i: integer): TProcessItem;
begin
  if (i >= Low(FItems)) and (i <= High(FItems)) then
    Result := FItems[i]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

end.
