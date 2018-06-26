unit TU.Handles;

interface

uses
  Winapi.Windows;

type
  /// <summary> Represents a token handle from another process. </summary>
  /// <remarks>
  ///   The handle value from <c>hToken</c> field is valid only in context of
  ///   the process with <c>OwnerPID</c>.
  ///  </remarks>
  THandleItem = record
    OwnerPID: NativeUInt;
    /// <remarks> Valid only in context of <c>OwnerPID</c>.</remarks>
    hToken: NativeUInt;
    Access: ACCESS_MASK;
    KernelObjectAddress: Pointer;
  end;

  TNativeUIntArray = array of NativeUInt;
  THandleItemArray = array of THandleItem;

  THandleList = class
  protected
    FItems: array of THandleItem;
    function GetItem(Ind: Integer): THandleItem;
    function GetCount: Integer;
    function GetProcesses: TNativeUIntArray;
    function GetProcessHandles(PID: NativeUInt): THandleItemArray;
  public
    /// <summary>
    ///  Creates a list of all handles on the system (excluding current process)
    ///  that represent token objects.
    /// </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create;
    property Handles[Ind: Integer]: THandleItem read GetItem; default;
    property Count: Integer read GetCount;
    /// <returns>
    ///   Returns an array of all processes that have opened token handles.
    /// </returns>
    property Processes: TNativeUIntArray read GetProcesses;
    /// <returns>
    ///   Returns an array of all token handles opened by the specified process.
    /// </returns>
    property ProcessHandles[PID: NativeUInt]: THandleItemArray read GetProcessHandles;
  end;

implementation

uses
  System.SysUtils, TU.Common, TU.NativeAPI;

const
  // TODO: Is this value fixed or should be somehow obtained in runtime?
  TokenObjectTypeIndex = 5;

{ THandleList }

constructor THandleList.Create;
var
  Buffer: PSystemHandleInformationEx;
  BufferSize, ReturnLength: Cardinal;
  Status: NTSTATUS;
  i, Count: integer;
begin
  Buffer := nil;
  BufferSize := 0;

  while True do
  begin
    Status := NtQuerySystemInformation(SystemExtendedHandleInformation, Buffer,
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
    // Count all token handles from other processes
    Count := 0;
    for i := 0 to Buffer.NumberOfHandles - 1 do
    with Buffer.Handles[i] do
      if (ObjectTypeIndex = TokenObjectTypeIndex) and
        (UniqueProcessId <> GetCurrentProcessId) then
        Inc(Count);

    // Allocate memory
    SetLength(FItems, Count);

    // Save all these handles and additional information
    Count := 0;
    for i := 0 to Buffer.NumberOfHandles - 1 do
    with Buffer.Handles[i] do
      if (ObjectTypeIndex = TokenObjectTypeIndex) and
        (UniqueProcessId <> GetCurrentProcessId) then
      with FItems[Count] do
      begin
        OwnerPID := UniqueProcessId;
        hToken := HandleValue;
        Access := GrantedAccess;
        KernelObjectAddress := PObject;
        Inc(Count);
      end;
  finally
    FreeMem(Buffer);
  end;
end;

function THandleList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function THandleList.GetItem(Ind: Integer): THandleItem;
begin
  Result := FItems[Ind];
end;

function THandleList.GetProcesses: TNativeUIntArray;
var
  i, j: integer;
  Exists: Boolean;
begin
  for i := 0 to High(FItems) do
  with FItems[i] do
  begin

    Exists := False;
    for j := 0 to High(Result) do
      if Result[j] = OwnerPID then
      begin
        Exists := True;
        Break;
      end;

    if not Exists then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := OwnerPID;
    end;
  end;
end;

function THandleList.GetProcessHandles(PID: NativeUInt): THandleItemArray;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 0 to High(FItems) do
    if FItems[i].OwnerPID = PID then
      Inc(Count);

  SetLength(Result, Count);

  Count := 0;
  for i := 0 to High(FItems) do
    if FItems[i].OwnerPID = PID then
    begin
      Result[Count] := FItems[i];
      Inc(Count);
    end;
end;

end.
