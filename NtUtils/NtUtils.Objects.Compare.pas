unit NtUtils.Objects.Compare;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, NtUtils.Exceptions;

  { Helper functions }

type
  THashingRoutine = function(Handle: THandle; out Hash: UInt64): NTSTATUS;

// Compute an object hash. Can reopen the object for required access.
function NtxQueryHandleHash(hObject: THandle; HashingRoutine: THashingRoutine;
  RequiredAccess: TAccessMask; out Hash: UInt64): NTSTATUS;

// Compare two objects by computing their hashes
function NtxCompareHandlesByHash(hObject1, hObject2: THandle;
  HashingRoutine: THashingRoutine; RequiredAccess: TAccessMask): NTSTATUS;

// Hashing routines
function NtxHashToken(hToken: THandle; out Hash: UInt64): NTSTATUS;
function NtxHashProcess(hProcess: THandle; out Hash: UInt64): NTSTATUS;
function NtxHashThread(hThread: THandle; out Hash: UInt64): NTSTATUS;

  { Generic comparison }

// Check whether two handles point to the same kernel object.
// Returns STATUS_SUCCESS (same), STATUS_NOT_SAME_OBJECT, or other error
function NtxCompareObjects(hObject1, hObject2: THandle;
  ObjectTypeName: String = ''): NTSTATUS;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntobapi, Ntapi.ntpsapi, Ntapi.ntseapi, NtUtils.Objects,
  NtUtils.Ldr, NtUtils.Objects.Snapshots;

function NtxQueryHandleHash(hObject: THandle; HashingRoutine: THashingRoutine;
  RequiredAccess: TAccessMask; out Hash: UInt64): NTSTATUS;
var
  hRef: THandle;
begin
  // Try to peform hashing
  Result := HashingRoutine(hObject, Hash);

  // If necessary, reopen the object and try again
  if (Result = STATUS_ACCESS_DENIED) and NT_SUCCESS(NtDuplicateObject(
    NtCurrentProcess, hObject, NtCurrentProcess, hRef, RequiredAccess, 0, 0))
    then
  begin
    Result := HashingRoutine(hRef, Hash);
    NtxSafeClose(hRef);
  end;
end;

function NtxCompareHandlesByHash(hObject1, hObject2: THandle;
  HashingRoutine: THashingRoutine; RequiredAccess: TAccessMask): NTSTATUS;
var
  Hash1, Hash2: UInt64;
begin
  // Hash the first handle
  Result := NtxQueryHandleHash(hObject1, HashingRoutine, RequiredAccess, Hash1);

  if not NT_SUCCESS(Result) then
    Exit;

  // Hash the second handle
  Result := NtxQueryHandleHash(hObject2, HashingRoutine, RequiredAccess, Hash2);

  if not NT_SUCCESS(Result) then
    Exit;

  // Compare
  if Hash1 = Hash2 then
    Result := STATUS_SUCCESS
  else
    Result := STATUS_NOT_SAME_OBJECT;
end;

function NtxHashToken(hToken: THandle; out Hash: UInt64): NTSTATUS;
var
  Statistics: TTokenStatistics;
  Required: Cardinal;
begin
  // Use TokenId as a hash value
  Result := NtQueryInformationToken(hToken, TokenStatistics, @Statistics,
    SizeOf(Statistics), Required);

  if NT_SUCCESS(Result) then
    Hash := UInt64(Statistics.TokenId);
end;

function NtxHashProcess(hProcess: THandle; out Hash: UInt64): NTSTATUS;
var
  Info: TProcessBasinInformation;
begin
  // Use ProcessId as a hash value
  Result := NtQueryInformationProcess(hProcess, ProcessBasicInformation,
    @Info, SizeOf(Info), nil);

  if NT_SUCCESS(Result) then
    Hash := UInt64(Info.UniqueProcessId);
end;

function NtxHashThread(hThread: THandle; out Hash: UInt64): NTSTATUS;
var
  Info: TThreadBasicInformation;
begin
  // Use ThreadId as a hash value
  Result := NtQueryInformationThread(hThread, ThreadBasicInformation,
    @Info, SizeOf(Info), nil);

  if NT_SUCCESS(Result) then
    Hash := UInt64(Info.ClientId.UniqueThread);
end;

function NtxCompareObjects(hObject1, hObject2: THandle;
  ObjectTypeName: String = ''): NTSTATUS;
var
  Type1, Type2: TObjectTypeInfo;
  Name1, Name2: String;
  Handles: TArray<THandleEntry>;
  i, j: Integer;
begin
  if hObject1 = hObject2 then
    Exit(STATUS_SUCCESS);

  // Win 10 TH+ makes things way easier
  if LdrxCheckNtDelayedImport('NtCompareObjects').IsSuccess then
    Exit(NtCompareObjects(hObject1, hObject2));

  // Get object's type if the caller didn't specify it
  if ObjectTypeName = '' then
    if NtxQueryTypeObject(hObject1, Type1).IsSuccess and
      NtxQueryTypeObject(hObject2, Type2).IsSuccess then
    begin
      if Type1.TypeName <> Type2.TypeName then
        Exit(STATUS_NOT_SAME_OBJECT);

      ObjectTypeName := Type1.TypeName;
    end;

  // Perform type-specific comparison
  if ObjectTypeName <> '' then
  begin
    Result := STATUS_OBJECT_TYPE_MISMATCH;

    if ObjectTypeName = 'Token' then
      Result := NtxCompareHandlesByHash(hObject1, hObject2, NtxHashToken,
        TOKEN_QUERY)
    else
    if ObjectTypeName = 'Process' then
      Result := NtxCompareHandlesByHash(hObject1, hObject2, NtxHashProcess,
        PROCESS_QUERY_LIMITED_INFORMATION)
    else
    if ObjectTypeName = 'Thread' then
      Result := NtxCompareHandlesByHash(hObject1, hObject2, NtxHashThread,
        THREAD_QUERY_LIMITED_INFORMATION);

    case Result of
      STATUS_SUCCESS, STATUS_NOT_SAME_OBJECT:
        Exit;
    end;
  end;

  // Note: desktops with the same name located in different window
  // station appear the same, although they are not.

  // Compare named objects
  if NtxQueryNameObject(hObject1, Name1).IsSuccess and
    NtxQueryNameObject(hObject2, Name2).IsSuccess then
    if (Name1 <> Name2) then
      Exit(STATUS_NOT_SAME_OBJECT)
    else if (Name1 <> '') and (ObjectTypeName <> 'Desktop') then
      Exit(STATUS_SUCCESS);

  // The last resort is to proceed via a handle snapshot
  Result := NtxEnumerateSystemHandles(Handles).Status;

  if not NT_SUCCESS(Result) then
    Exit;

  NtxFilterHandles(Handles, FilterByProcess, NtCurrentProcessId);

  for i := 0 to High(Handles) do
    if Handles[i].HandleValue = hObject1 then
      for j := i + 1 to High(Handles) do
        if Handles[j].HandleValue = hObject2 then
        begin
          if Handles[i].PObject = Handles[j].PObject then
            Exit(STATUS_SUCCESS)
          else
            Exit(STATUS_NOT_SAME_OBJECT)
        end;

  Result := STATUS_NOT_FOUND;
end;

end.
