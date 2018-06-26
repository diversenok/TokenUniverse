unit TU.Processes;

interface

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
  NT_FILENAME_MAX = 32768;

type
  TProcessItem = record
    ImageName: String;
    PID: Cardinal;
    ParentPID: Cardinal;
  end;
  PProcessItem = ^TProcessItem;

  /// <summary> Stores a snapshot of all processes in the system. </summary>
  TProcessList = class
  protected
    FCount: integer;
    FItems: array of TProcessItem;
    function GetItem(i: integer): TProcessItem;
  public
    /// <summary> Create a snapshot of all processes in the system. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create;
    property Count: integer read FCount;
    property Items[i: integer]: TProcessItem read GetItem; default;
    function FindName(PID: Cardinal): String;
  end;

function QueryFullProcessImageNameW(hProcess: THandle; dwFlags: Cardinal;
  lpExeName: PWideChar; var lpdwSize: Cardinal): LongBool; stdcall;
  external 'kernel32.dll' delayed;

implementation

uses
  Winapi.Windows, TU.NativeAPI;

  { TProcessList }

constructor TProcessList.Create;
var
  Buffer, SysInfo: PSystemProcessInformation;
  BufferSize: NativeUInt;
  ReturnLength: Cardinal;
  status: NTSTATUS;
  i: integer;
begin
  Buffer := nil;
  BufferSize := 0;

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

  try
    // Count processes
    FCount := 0;
    SysInfo := Buffer;
    while True do
    begin
      Inc(FCount);
      if SysInfo.NextEntryOffset = 0 then
        Break;
      SysInfo := @PByteArray(SysInfo)[SysInfo.NextEntryOffset];
    end;

    // Allocate memory
    SetLength(FItems, FCount);

    // Save each process information
    i := 0;
    SysInfo := Buffer;
    while True do
    with FItems[i] do
    begin
      SetString(ImageName, SysInfo.ImageName.Buffer,
        SysInfo.ImageName.Length div SizeOf(WChar));

      if ImageName = '' then
        ImageName := 'System Idle Process';

      PID := SysInfo.ProcessId;
      ParentPID := SysInfo.InheritedFromProcessId;

      if SysInfo.NextEntryOffset = 0 then
        Break;

      SysInfo := @PByteArray(SysInfo)[SysInfo.NextEntryOffset];
      Inc(i);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function TProcessList.FindName(PID: Cardinal): String;
var
  i: integer;
begin
  for i := 0 to High(FItems) do
    if FItems[i].PID = PID then
      Exit(FItems[i].ImageName);

  Result := 'Unknown process';
end;

function TProcessList.GetItem(i: integer): TProcessItem;
begin
  Result := FItems[i];
end;

end.
