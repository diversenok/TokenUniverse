unit TU.Handles;

interface

uses
  Winapi.Windows;

type
  THandleItem = record
    OwnerPID: NativeUInt;
    hToken: NativeUInt; // Valid only in context of OwnerPID
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
    constructor Create;
    property Handles[Ind: Integer]: THandleItem read GetItem; default;
    property Count: Integer read GetCount;
    property Processes: TNativeUIntArray read GetProcesses;
    property ProcessHandles[PID: NativeUInt]: THandleItemArray read GetProcessHandles;
  end;

implementation

uses
  System.SysUtils, TU.Common, TU.NativeAPI;

const
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

  Count := 0;
  for i := 0 to Buffer.NumberOfHandles - 1 do
  with Buffer.Handles[i] do
    if (ObjectTypeIndex = TokenObjectTypeIndex) and
      (UniqueProcessId <> GetCurrentProcessId) then
      Inc(Count);

  SetLength(FItems, Count);

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

  FreeMem(Buffer);
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
