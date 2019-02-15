unit NtUtils.Processes;

interface

uses
  Ntapi.ntdef, Ntapi.ntexapi;

type
  PProcessInfo = Ntapi.ntexapi.PSystemProcessInformation;
  PThreadInfo = Ntapi.ntexapi.PSystemThreadInformation;

  /// <summary> Snapshots all processes on the system. </summary>
  TProcessSnapshot = class
  private
    function GetItem(i: Cardinal): PSystemProcessInformation;
  protected
    Buffer: PSystemProcessInformation;
    BufferSize: Cardinal;
    FCount: Cardinal;
    FProcesses: array of PProcessInfo;
    Status: NTSTATUS;
  public
    property DetailedStatus: NTSTATUS read Status;
    /// <summary> Create a snapshot of all processes on the system. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create;
    destructor Destroy; override;
    property Count: Cardinal read FCount;
    property Process[i: Cardinal]: PProcessInfo read GetItem; default;
    function FindByPID(PID: Cardinal): PProcessInfo;
    function FindByName(ImageName: String): PProcessInfo;
  end;

implementation

uses
  Ntapi.ntstatus, NtUtils.Exceptions;

function AddToPointer(P: Pointer; Size: NativeUInt): Pointer;
begin
  Result := Pointer(NativeUInt(P) + Size);
end;

{ TProcessSnapshot }

constructor TProcessSnapshot.Create;
var
  Process: PProcessInfo;
  ReturnLength: Cardinal;
  i: integer;
begin
  // Some calculations:
  //  x86: Memory = 184 bytes * Processes + 64 bytes * Threads + ImageName
  //  x64: Memory = 256 bytes * Processes + 80 bytes * Threads + ImageName
  //
  // On my notebook I usually have ~75 processes with ~850 threads, so it's
  // about 100 KB of data.

  // Start querying with 256 KB.
  BufferSize := 256 * 1024;
  Buffer := AllocMem(BufferSize);

  // TODO: Spanshot only processes of current session

  // Query the information or its size until we pass a suitable buffer for the
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

      // Use a 10% addition to be sure to fit despite the fluctuations
      BufferSize := ReturnLength + ReturnLength div 10;
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
    ENtError.Format(Status, 'Process snapshot');
    Exit;
  end;

  FCount := 0;
  Process := Buffer;

  // Count the processes
  while True do
  begin
    Inc(FCount);
    if Process.NextEntryOffset = 0 then
      Break;
    Process := AddToPointer(Process, Process.NextEntryOffset);
  end;

  // Allocate memory to store pointers to all process items
  SetLength(FProcesses, FCount);

  i := 0;
  Process := Buffer;

  // Save each process information
  while True do
  begin
    FProcesses[i] := Process;
    if Process.NextEntryOffset = 0 then
      Break;
    Process := AddToPointer(Process, Process.NextEntryOffset);
    Inc(i);
  end;
end;

destructor TProcessSnapshot.Destroy;
begin
  // Overwrite the buffer to catch potential access violations earlier
  {$IFDEF DEBUG}
  FillChar(Buffer^, BufferSize, 0);
  {$ENDIF}

  FreeMem(Buffer);
  inherited;
end;

function TProcessSnapshot.FindByName(ImageName: String): PProcessInfo;
var
  i: Integer;
begin
  for i := 0 to High(FProcesses) do
    if FProcesses[i].GetImageName = ImageName then
      Exit(FProcesses[i]);

  Result := nil;
end;

function TProcessSnapshot.FindByPID(PID: Cardinal): PProcessInfo;
var
  i: Integer;
begin
  for i := 0 to High(FProcesses) do
    if FProcesses[i].ProcessId = PID then
      Exit(FProcesses[i]);

  Result := nil;
end;

function TProcessSnapshot.GetItem(i: Cardinal): PProcessInfo;
begin
  Result := FProcesses[i];
end;

end.
