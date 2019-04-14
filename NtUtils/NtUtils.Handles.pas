unit NtUtils.Handles;

interface

uses
  Ntapi.ntdef, Ntapi.ntexapi, DelphiUtils.Events;

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
    Status: NTSTATUS;
    class var FOnSnapshot: TEvent<THandleSnapshot>;
  public
    property DetailedStatus: NTSTATUS read Status;
    constructor Create;
    destructor Destroy; override;
    function FilterByProcess(PID: NativeUInt;
      ObjectType: TObjectType = objToken): THandleInfoArray; overload;
    function FilterByObject(ObjectAddress: Pointer): THandleInfoArray; overload;
  public
    class property OnSnapshot: TEvent<THandleSnapshot> read FOnSnapshot;

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

    class function Compare(hObject1, hObject2: THandle): NTSTATUS; static;
  end;

  PObjectInfo = Ntapi.ntexapi.PSystemObjectInformation;

  /// <summary> Snapshots objects on the system. </summary>
  /// <remarks> Requires a specific flag set at boottime. </remarks>
  TObjectSnapshot = class
  protected
    Buffer: PSystemObjectTypeInformation;
    BufferSize: Cardinal;
    Status: NTSTATUS;
    function GetSuccess: Boolean;
  public
    property DetailedStatus: NTSTATUS read Status;
    property IsSuccessful: Boolean read GetSuccess;
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///  Checks whether object snapshotting is supported on the system.
    /// </summary>
    class function FeatureSupported: Boolean; static;

    /// <summary>
    ///  Finds an object information by its kernel address.
    /// </summary>
    function FindObject(ObjectType: TObjectType; Address: Pointer): PObjectInfo;
  end;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntrtl, Ntapi.ntpsapi, NtUtils.Exceptions;

function AddToPointer(P: Pointer; Size: NativeUInt): Pointer;
begin
  Result := Pointer(NativeUInt(P) + Size);
end;

{ THandleSnapshot }

class function THandleSnapshot.Compare(hObject1, hObject2: THandle): NTSTATUS;
var
  i, j: Integer;
  Handles: THandleInfoArray;
begin
  with THandleSnapshot.Create do
  try
    Handles := FilterByProcess(NtCurrentProcessId);
    Result := Status;
  finally
    Free;
  end;

  if not NT_SUCCESS(Result) then
    Exit;

  for i := 0 to High(Handles) do
    if Handles[i].HandleValue = hObject1 then
    begin
      for j := 0 to High(Handles) do
        if Handles[j].HandleValue = hObject2 then
        begin
          if Handles[i].PObject = Handles[j].PObject then
            Exit(STATUS_SUCCESS)
          else
            Exit(STATUS_NOT_SAME_OBJECT);
        end;
      Break;
    end;

  Result := STATUS_NOT_FOUND;
end;

constructor THandleSnapshot.Create;
var
  ReturnLength: Cardinal;
begin
  // Some calculations:
  //  x86: Memory = 28 bytes * Handle
  //  x64: Memory = 40 bytes * Handle
  //
  // On my notebook I usually have ~25k handles, so it's about 1 MB of data.

  // Start querying with 3 MB.
  BufferSize := 3 * 1024 * 1024;
  Buffer := AllocMem(BufferSize);

  // Query the information or its size until we pass a suitable buffer for the
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
    ENtError.Report(Status, 'Handle snapshot');
  end
  else
    OnSnapshot.Invoke(Self);
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

{ TObjectSnapshot }

constructor TObjectSnapshot.Create;
var
  ReturnLength: Cardinal;
begin
  // Check the flag
  if not FeatureSupported then
  begin
    Status := STATUS_UNSUCCESSFUL;
    Exit;
  end;

  // On my system it is usually about 800 KB of data.

  // Start querying with 3 MB.
  BufferSize := 3 * 1024 * 1024;
  Buffer := AllocMem(BufferSize);

  // Query the information or its size until we pass a suitable buffer for the
  // system call or get an unexpected error
  while True do
  begin
    Status := NtQuerySystemInformation(SystemObjectInformation, Buffer,
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

      // Use a 20% addition to be sure to fit despite huge fluctuations
      BufferSize := ReturnLength + ReturnLength div 5;
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
    ENtError.Report(Status, 'Object snapshot');
  end;
end;

destructor TObjectSnapshot.Destroy;
begin
  // Overwrite the buffer to catch potential access violations earlier
  {$IFDEF DEBUG}
  FillChar(Buffer^, BufferSize, 0);
  {$ENDIF}

  FreeMem(Buffer);
  inherited;
end;

class function TObjectSnapshot.FeatureSupported: Boolean;
begin
  // Querying objects requires a specific flag set at boottime
  Result := (RtlGetNtGlobalFlags and FLG_MAINTAIN_OBJECT_TYPELIST <> 0);
end;

function TObjectSnapshot.FindObject(ObjectType: TObjectType; Address: Pointer):
  PObjectInfo;
var
  TypeInfo: PSystemObjectTypeInformation;
  ObjInfo: PSystemObjectInformation;
begin
  Result := nil;
  if not Assigned(Buffer) then
    Exit;

  // Iterate through all available types
  TypeInfo := Buffer;
  while True do
  begin
    // Check if this is the required object type
    if TypeInfo.TypeIndex = Cardinal(ObjectType) then
    begin
      // Point to the first object
      ObjInfo := AddToPointer(TypeInfo, SizeOf(TSystemObjectTypeInformation) +
        TypeInfo.TypeName.MaximumLength);

      // Iterate through all objects of this type
      while True do
      begin
        // Check for matching object
        if ObjInfo.ObjectAddress = Address then
          Exit(ObjInfo);

        // Skip to the next object within the type
        if ObjInfo.NextEntryOffset = 0 then
          Break
        else
          ObjInfo := AddToPointer(Buffer, ObjInfo.NextEntryOffset);
      end;
    end;

    // Skip to the next type
    if TypeInfo.NextEntryOffset = 0 then
      Break
    else
      TypeInfo := AddToPointer(Buffer, TypeInfo.NextEntryOffset);
  end;
end;

function TObjectSnapshot.GetSuccess: Boolean;
begin
  Result := NT_SUCCESS(Status);
end;

end.
