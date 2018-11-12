unit TU.Handles;

interface

uses
  Winapi.Windows;

type
  /// <summary> Represents information about a handle. </summary>
  THandleInformation = record
    /// <remarks>
    ///  The value is valid only in context of a process with a specific PID
    ///  of <see cref="ContextPID"/>.
    /// </remarks>
    Handle: NativeUInt;
    ContextPID: NativeUInt;
    Access: ACCESS_MASK;
    KernelObjectAddress: NativeUInt;
  end;

  TNativeUIntArray = array of NativeUInt;
  THandleItemArray = array of THandleInformation;

  // TODO: Are these values fixed or should they be somehow obtained in runtime?
  TObjectType = (objToken = 5);

  /// <summary>
  ///  Stores a list of handles of a specific type opened in the specified
  ///  processes.
  /// </summary>
  THandleList = class
  protected
    FItems: array of THandleInformation;
    function GetItem(Ind: Integer): THandleInformation;
    function GetCount: Integer;
    function GetProcesses: TNativeUIntArray;
    function GetProcessHandles(PID: NativeUInt): THandleItemArray;
  public
    /// <summary>
    ///  Creates a list of all handles on the system of the specific type.
    /// </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create(FilterType: TObjectType = objToken);

    /// <summary>
    ///  Creates a list of all handles of a process of the specific type.
    /// </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor CreateOnly(PID: NativeUInt; FilterType: TObjectType = objToken);

    property Handles[Ind: Integer]: THandleInformation read GetItem; default;
    property Count: Integer read GetCount;

    /// <returns>
    ///   Returns an array of all processes with opened handles.
    /// </returns>
    property Processes: TNativeUIntArray read GetProcesses;

    /// <returns>
    ///   Returns an array of all handles opened by the specified process.
    /// </returns>
    property ProcessHandles[PID: NativeUInt]: THandleItemArray read GetProcessHandles;
  end;

implementation

uses
  System.SysUtils, TU.Common, TU.NativeAPI;

type
  /// <summary>
  ///  A helper class to collect the list of all opened handles on the system.
  /// </summary>
  THandleSnapshot = class
    Buffer: PSystemHandleInformationEx;
    constructor Create;
    destructor Destroy; override;
  end;

{ THandleList }

constructor THandleList.Create(FilterType: TObjectType = objToken);
begin
  // Zero PID is reserved as a flag to collect all the processes
  CreateOnly(0);
end;

constructor THandleList.CreateOnly(PID: NativeUInt;
  FilterType: TObjectType = objToken);
var
  Snap: THandleSnapshot;
  i, ItemsCount: integer;
begin
  Snap := THandleSnapshot.Create;

  with Snap do
  try
    // Check if the snapshot is successful.
    // If not then exit with 0 entries
    if not Assigned(Buffer) then
      Exit;

    // Count all handles of the specified process with the specified type.
    // Zero PID is reserved as a flag to collect all the processes
    ItemsCount := 0;
    for i := 0 to Buffer.NumberOfHandles - 1 do
    with Buffer.Handles[i] do
      if ObjectTypeIndex = Word(FilterType) then
        if (UniqueProcessId = PID) or (PID = 0) then
          Inc(ItemsCount);

    // Allocate memory
    SetLength(FItems, ItemsCount);

    // Save all these handles and additional information
    ItemsCount := 0;
    for i := 0 to Buffer.NumberOfHandles - 1 do
    with Buffer.Handles[i] do
      if ObjectTypeIndex = Word(FilterType) then
        if (UniqueProcessId = PID) or (PID = 0) then
        with FItems[ItemsCount] do
        begin
          // btw: Access field can be also queried separately via
          // NtQueryObject with ObjectBasicInformation

          ContextPID := UniqueProcessId;
          Handle := HandleValue;
          Access := GrantedAccess;
          KernelObjectAddress := NativeUInt(PObject);
          Inc(ItemsCount);
        end;
  finally
    Snap.Free;
  end;
end;

function THandleList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function THandleList.GetItem(Ind: Integer): THandleInformation;
begin
  Result := FItems[Ind];
end;

function THandleList.GetProcesses: TNativeUIntArray;
var
  i, j: integer;
  Exists: Boolean;
begin
  // Collect all processes in our list
  for i := 0 to High(FItems) do
  with FItems[i] do
  begin
    // Also prevent duplicates
    Exists := False;
    for j := 0 to High(Result) do
      if Result[j] = ContextPID then
      begin
        Exists := True;
        Break;
      end;

    if not Exists then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := ContextPID;
    end;
  end;
end;

function THandleList.GetProcessHandles(PID: NativeUInt): THandleItemArray;
var
  i, Count: Integer;
begin
  // Count all the handles that belong to the specified process
  Count := 0;
  for i := 0 to High(FItems) do
    if FItems[i].ContextPID = PID then
      Inc(Count);

  // Allocate enough memory
  SetLength(Result, Count);

  // Save them
  Count := 0;
  for i := 0 to High(FItems) do
    if FItems[i].ContextPID = PID then
    begin
      Result[Count] := FItems[i];
      Inc(Count);
    end;
end;

{ THandleSnapshot }

constructor THandleSnapshot.Create;
var
  BufferSize, ReturnLength: Cardinal;
  Status: NTSTATUS;
begin
  Buffer := nil;
  BufferSize := 0;

  // Query the information or it's size until we pass a suitable buffer for a
  // system call or get an unexpected error
  while True do
  begin
    Status := NtQuerySystemInformation(SystemExtendedHandleInformation, Buffer,
      BufferSize, ReturnLength);

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

    OutputDebugString(PChar(Format('Handle snapshot failed with 0x%0.8x',
      [Status])));
    Exit;
  end;
end;

destructor THandleSnapshot.Destroy;
begin
  FreeMem(Buffer);
  inherited;
end;

end.
