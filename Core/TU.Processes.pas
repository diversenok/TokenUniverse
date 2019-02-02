unit TU.Processes;

interface

uses
  Winapi.Windows, Ntapi.ntexapi;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

type
  PSystemProcessInformation = Ntapi.ntexapi.PSystemProcessInformation;

  /// <summary> Snapshots all processes on the system. </summary>
  TProcessSnapshot = class
  private
    function GetItem(i: Cardinal): PSystemProcessInformation;
  protected
    Buffer: PSystemProcessInformation;
    BufferSize: Cardinal;
    FCount: Cardinal;
    FProcesses: array of PSystemProcessInformation;
  public
    /// <summary> Create a snapshot of all processes on the system. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create;
    destructor Destroy; override;
    property Count: Cardinal read FCount;
    property Process[i: Cardinal]: PSystemProcessInformation read GetItem; default;
    function FindByPID(PID: Cardinal): PSystemProcessInformation;
    function FindByName(ImageName: String): PSystemProcessInformation;
  end;

implementation

uses
  TU.Common, System.SysUtils, Ntapi.ntdef, Ntapi.ntstatus;

{ TProcessSnapshot }

constructor TProcessSnapshot.Create;
var
  Process: PSystemProcessInformation;
  ReturnLength: Cardinal;
  Status: NTSTATUS;
  i: integer;
begin
  BufferSize := 0;

  // TODO: Spanshot only processes of current session

  // Query the information or it's size until we pass a suitable buffer for a
  // system call or get an unexpected error
  while True do
  begin
    Status := NtQuerySystemInformation(SystemProcessInformation, Buffer,
      BufferSize, @ReturnLength);
    if (Status = STATUS_BUFFER_TOO_SMALL) or
      (Status = STATUS_INFO_LENGTH_MISMATCH) then
    begin
      FreeMem(Buffer);

      // Do not allocate too big buffers.
      if ReturnLength > BUFFER_LIMIT then
      begin
        Status := STATUS_IMPLEMENTATION_LIMIT;
        Break;
      end;

      BufferSize := ReturnLength;
      Buffer := AllocMem(BufferSize);
    end
    else
      Break;
  end;

  // We have exited the loop. It means that we either succeeded (and the buffer
  // is valid) or failed (and the buffer should be cleaned up).

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(Buffer);
    Buffer := nil;
    BufferSize := 0;

    OutputDebugString(PChar(Format('Process snapshot failed with 0x%0.8x',
      [Status])));
    Exit;
  end;

  FCount := 0;
  Process := Buffer;

  // FCount the processes
  while True do
  begin
    Inc(FCount);
    if Process.NextEntryOffset = 0 then
      Break;
    Process := @PByteArray(Process)[Process.NextEntryOffset];
  end;

  // Allocate memory to store pointers to all process items
  SetLength(FProcesses, FCount);

  // Save each process information
  i := 0;
  Process := Buffer;

  while True do
  begin
    FProcesses[i] := Process;
    if Process.NextEntryOffset = 0 then
      Break;
    Process := @PByteArray(Process)[Process.NextEntryOffset];
    Inc(i);
  end;
end;

destructor TProcessSnapshot.Destroy;
begin
  // Overwrite the buffer to catch possible access violations earlier
  {$IFDEF DEBUG}
  FillChar(Buffer^, BufferSize, 0);
  {$ENDIF}

  FreeMem(Buffer);
  inherited;
end;

function TProcessSnapshot.FindByName(ImageName: String): PSystemProcessInformation;
var
  i: Integer;
begin
  for i := 0 to High(FProcesses) do
    if FProcesses[i].GetImageName = ImageName then
      Exit(FProcesses[i]);

  Result := nil;
end;

function TProcessSnapshot.FindByPID(PID: Cardinal): PSystemProcessInformation;
var
  i: Integer;
begin
  for i := 0 to High(FProcesses) do
    if FProcesses[i].ProcessId = PID then
      Exit(FProcesses[i]);

  Result := nil;
end;

function TProcessSnapshot.GetItem(i: Cardinal): PSystemProcessInformation;
begin
  Result := FProcesses[i];
end;

end.
