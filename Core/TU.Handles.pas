unit TU.Handles;

interface

uses
  Winapi.Windows, Ntapi.ntexapi;

type
  THandleInfo = Ntapi.ntexapi.TSystemHandleTableEntryInfoEx;
  PHandleInfo = Ntapi.ntexapi.PSystemHandleTableEntryInfoEx;

  THandleInfoArray = array of THandleInfo;

  // TODO: Are these values fixed or should they be somehow obtained in runtime?
  TObjectType = (objToken = 5);

  /// <summary> Snapshots all handles on the system. </summary>
  THandleSnapshot = class
  protected
    Buffer: PSystemHandleInformationEx;
    BufferSize: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    function FilterByProcess(PID: NativeUInt;
      ObjectType: TObjectType = objToken): THandleInfoArray; overload;
    function FilterByObject(ObjectAddress: Pointer): THandleInfoArray; overload;
  public
    /// <summary>
    ///  Retrieves all handles of the specific type opened by a process.
    /// </summary>
    class function OfProcess(PID: NativeUInt;
      ObjectType: TObjectType = objToken): THandleInfoArray; overload; static;

    /// <summary>
    ///  Retrieves all handles to the specified kernel object.
    /// </summary>
    class function OfObject(ObjectAddress: Pointer): THandleInfoArray;
      overload; static;
  end;

implementation

uses
  TU.Common, Ntapi.ntdef, Ntapi.ntstatus;

{ THandleSnapshot }

constructor THandleSnapshot.Create;
var
  ReturnLength: Cardinal;
  Status: NTSTATUS;
begin
  // Some calculations:
  //  x86: Memory = 28 bytes * Handle
  //  x64: Memory = 40 bytes * Handle
  //
  // On my notebook I usually have ~25k handles, so it's about 1 MB of data.

  // Start querying with 3 MB.
  BufferSize := 3 * 1024 * 1024;
  Buffer := AllocMem(BufferSize);

  // Query the information or its size until we pass a suitable buffer for a
  // system call or get an unexpected error
  while True do
  begin
    Status := NtQuerySystemInformation(SystemExtendedHandleInformation, Buffer,
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
    ReportStatus(Status, 'Handle snapshot');
  end;
end;

destructor THandleSnapshot.Destroy;
begin
  // Overwrite the buffer to catch potential access violations earlier
  {$IFDEF DEBUG}
  FillChar(Buffer^, BufferSize, 0);
  {$ENDIF}

  FreeMem(Buffer);
  inherited;
end;

function THandleSnapshot.FilterByObject(
  ObjectAddress: Pointer): THandleInfoArray;
var
  i, j: NativeInt;
  Count: Cardinal;
begin
  if not Assigned(Buffer) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  {$R-}

  // Count suitable handles
  Count := 0;
  for i := 0 to Buffer.NumberOfHandles - 1 do
    if Buffer.Handles[i].PObject = ObjectAddress then
      Inc(Count);

  // Allocate storage
  SetLength(Result, Count);

  // Save references
  j := 0;
  for i := 0 to Buffer.NumberOfHandles - 1 do
    if Buffer.Handles[i].PObject = ObjectAddress then
      begin
        Result[j] := Buffer.Handles[i];
        Inc(j);
      end;

  {$R+}
end;

function THandleSnapshot.FilterByProcess(PID: NativeUInt;
  ObjectType: TObjectType): THandleInfoArray;
var
  i, j: NativeInt;
  Count: Cardinal;
begin
  if not Assigned(Buffer) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  {$R-}

  // Count suitable handles
  Count := 0;
  for i := 0 to Buffer.NumberOfHandles - 1 do
    if (Buffer.Handles[i].ObjectTypeIndex = Cardinal(ObjectType))
      and (Buffer.Handles[i].UniqueProcessId = PID) then
      Inc(Count);

  // Allocate storage
  SetLength(Result, Count);

  // Save references
  j := 0;
  for i := 0 to Buffer.NumberOfHandles - 1 do
    if (Buffer.Handles[i].ObjectTypeIndex = Cardinal(ObjectType))
      and (Buffer.Handles[i].UniqueProcessId = PID) then
      begin
        Result[j] := Buffer.Handles[i];
        Inc(j);
      end;

  {$R+}
end;

class function THandleSnapshot.OfObject(
  ObjectAddress: Pointer): THandleInfoArray;
begin
  with THandleSnapshot.Create do
  begin
    Result := FilterByObject(ObjectAddress);
    Free;
  end;
end;

class function THandleSnapshot.OfProcess(PID: NativeUInt;
  ObjectType: TObjectType): THandleInfoArray;
begin
  with THandleSnapshot.Create do
  begin
    Result := FilterByProcess(PID, ObjectType);
    Free;
  end;
end;

end.
