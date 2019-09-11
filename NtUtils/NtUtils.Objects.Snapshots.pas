unit NtUtils.Objects.Snapshots;

interface

uses
  Ntapi.ntexapi, NtUtils.Objects, NtUtils.Exceptions;

type
  THandleEntry = Ntapi.ntexapi.TSystemHandleTableEntryInfoEx;

  THandleFilter = function (const HandleEntry: THandleEntry;
    Parameter: NativeUInt): Boolean;

  TObjectEntry = record
    ObjectName: String;
    Other: TSystemObjectInformation;
  end;
  PObjectEntry = ^TObjectEntry;

  TObjectTypeEntry = record
    TypeName: String;
    Other: TSystemObjectTypeInformation;
    Objects: array of TObjectEntry;
  end;

{ Handles }

// Snapshot all handles on the system
function NtxEnumerateSystemHandles(out Handles: TArray<THandleEntry>):
  TNtxStatus;

// Filter specific handles from the snapshot
procedure NtxFilterHandles(var Handles: TArray<THandleEntry>;
  Filter: THandleFilter; Parameter: NativeUInt);

function FilterByProcess(const HandleEntry: THandleEntry;
  PID: NativeUInt): Boolean;
function FilterByAddress(const HandleEntry: THandleEntry;
  ObjectAddress: NativeUInt): Boolean;
function FilterByType(const HandleEntry: THandleEntry;
  TypeIndex: NativeUInt): Boolean;
function FilterByAccess(const HandleEntry: THandleEntry;
  AccessMask: NativeUInt): Boolean;

function NtxFindHandleEntry(Handles: TArray<THandleEntry>;
  PID: NativeUInt; Handle: THandle; out Entry: THandleEntry): Boolean;

{ Objects }

// Check if object snapshoting is supported
function NtxObjectEnumerationSupported: Boolean;

// Snapshot objects on the system
function NtxEnumerateObjects(out Types: TArray<TObjectTypeEntry>): TNtxStatus;

// Find object entry by a object's address
function NtxFindObjectByAddress(Types: TArray<TObjectTypeEntry>;
  Address: Pointer): PObjectEntry;

{ Types }

// Enumerate kernel object types on the system
function NtxEnumerateTypes(out Types: TArray<TObjectTypeInfo>): TNtxStatus;

implementation

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntrtl, Ntapi.ntpsapi,
  Ntapi.ntobapi;

{ Handles }

function NtxEnumerateSystemHandles(out Handles: TArray<THandleEntry>):
  TNtxStatus;
var
  BufferSize, ReturnLength: Cardinal;
  Buffer: PSystemHandleInformationEx;
  i: Integer;
begin
  Result.Location := 'NtQuerySystemInformation';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(SystemExtendedHandleInformation);
  Result.LastCall.InfoClassType := TypeInfo(TSystemInformationClass);

  // - x86: 28 bytes per handle
  // - x64: 40 bytes per handle
  // On my notebook I usually have ~25k handles, so it's about 1 MB of data.
  //
  // We don't want to use a huge initial buffer since system spends
  // more time probing it rather than coollecting the handles.

  BufferSize := 1024 * 1024;
  repeat
    Buffer := AllocMem(BufferSize);

    ReturnLength := 0;
    Result.Status := NtQuerySystemInformation(SystemExtendedHandleInformation,
      Buffer, BufferSize, @ReturnLength);

    if not Result.IsSuccess then
      FreeMem(Buffer);

  until not NtxExpandBuffer(Result, BufferSize, ReturnLength, True);

  if not Result.IsSuccess then
    Exit;

  SetLength(Handles, Buffer.NumberOfHandles);

  for i := 0 to High(Handles) do
    Handles[i] := Buffer.Handles{$R-}[i]{$R+};

  FreeMem(Buffer);
end;

procedure NtxFilterHandles(var Handles: TArray<THandleEntry>;
  Filter: THandleFilter; Parameter: NativeUInt);
var
  FilteredHandles: TArray<THandleEntry>;
  Count, i, j: Integer;
begin
  Assert(Assigned(Filter));

  Count := 0;
  for i := 0 to High(Handles) do
    if Filter(Handles[i], Parameter) then
      Inc(Count);

  SetLength(FilteredHandles, Count);

  j := 0;
  for i := 0 to High(Handles) do
    if Filter(Handles[i], Parameter) then
    begin
      FilteredHandles[j] := Handles[i];
      Inc(j);
    end;

  Handles := FilteredHandles;
end;

function FilterByProcess(const HandleEntry: THandleEntry;
  PID: NativeUInt): Boolean;
begin
  Result := (HandleEntry.UniqueProcessId = PID);
end;

function FilterByAddress(const HandleEntry: THandleEntry;
  ObjectAddress: NativeUInt): Boolean;
begin
  Result := (HandleEntry.PObject = Pointer(ObjectAddress));
end;

function FilterByType(const HandleEntry: THandleEntry;
  TypeIndex: NativeUInt): Boolean;
begin
  Result := (HandleEntry.ObjectTypeIndex = Word(TypeIndex));
end;

function FilterByAccess(const HandleEntry: THandleEntry;
  AccessMask: NativeUInt): Boolean;
begin
  Result := (HandleEntry.GrantedAccess = TAccessMask(AccessMask));
end;

function NtxFindHandleEntry(Handles: TArray<THandleEntry>;
  PID: NativeUInt; Handle: THandle; out Entry: THandleEntry): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(Handles) do
    if (Handles[i].UniqueProcessId = PID) and (Handles[i].HandleValue = Handle)
      then
    begin
      Entry := Handles[i];
      Exit(True);
    end;

  Result := False;
end;

{ Objects }

function NtxObjectEnumerationSupported: Boolean;
begin
  Result := (RtlGetNtGlobalFlags and FLG_MAINTAIN_OBJECT_TYPELIST <> 0);
end;

function NtxEnumerateObjects(out Types: TArray<TObjectTypeEntry>): TNtxStatus;
var
  BufferSize, Required: Cardinal;
  Buffer, pTypeEntry: PSystemObjectTypeInformation;
  pObjEntry: PSystemObjectInformation;
  Count, i, j: Integer;
begin
  Result.Location := 'NtQuerySystemInformation';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(SystemObjectInformation);
  Result.LastCall.InfoClassType := TypeInfo(TSystemInformationClass);

  // On my system it is usually about 800 KB of data. But we don't want
  // to use a huge initial buffer since system spends more time probing it
  // rather than collecting the objects
  BufferSize := 2 * 1024 * 1024;

  repeat
    Buffer := AllocMem(BufferSize);

    Required := 0;
    Result.Status := NtQuerySystemInformation(SystemObjectInformation, Buffer,
      BufferSize, @Required);

    if not Result.IsSuccess then
      FreeMem(Buffer);

    // The call usually does not calculate the required
    // size in one pass, we need to speed it up.
    Required := Required shl 1 + 64 * 1024 // x2 + 64 kB

  until not NtxExpandBuffer(Result, BufferSize, Required);

  if not Result.IsSuccess then
    Exit;

  // Count returned types
  Count := 0;
  pTypeEntry := Buffer;

  repeat
    Inc(Count);

    if pTypeEntry.NextEntryOffset = 0 then
      Break
    else
      pTypeEntry := Offset(Buffer, pTypeEntry.NextEntryOffset);
  until False;

  SetLength(Types, Count);

  // Iterarate through each type
  j := 0;
  pTypeEntry := Buffer;

  repeat
    // Copy type information
    Types[j].TypeName := pTypeEntry.TypeName.ToString;
    Types[j].Other := pTypeEntry^;
    Types[j].Other.TypeName.Buffer := PWideChar(Types[j].TypeName);

    // Count objects of this type
    Count := 0;
    pObjEntry := Offset(pTypeEntry, SizeOf(TSystemObjectTypeInformation) +
        pTypeEntry.TypeName.MaximumLength);

    repeat
      Inc(Count);

      if pObjEntry.NextEntryOffset = 0 then
        Break
      else
        pObjEntry := Offset(Buffer, pObjEntry.NextEntryOffset);
    until False;

    SetLength(Types[j].Objects, Count);

    // Iterate trough objects
    i := 0;
    pObjEntry := Offset(pTypeEntry, SizeOf(TSystemObjectTypeInformation) +
        pTypeEntry.TypeName.MaximumLength);

    repeat
      // Copy object information
      Types[j].Objects[i].ObjectName := pObjEntry.NameInfo.ToString;
      Types[j].Objects[i].Other := pObjEntry^;
      Types[j].Objects[i].Other.NameInfo.Buffer :=
        PWideChar(Types[j].Objects[i].ObjectName);

      if pObjEntry.NextEntryOffset = 0 then
        Break
      else
        pObjEntry := Offset(Buffer, pObjEntry.NextEntryOffset);

      Inc(i);
    until False;

    // Skip to the next type
    if pTypeEntry.NextEntryOffset = 0 then
      Break
    else
      pTypeEntry := Offset(Buffer, pTypeEntry.NextEntryOffset);

    Inc(j);
  until False;

  FreeMem(Buffer);
end;

function NtxFindObjectByAddress(Types: TArray<TObjectTypeEntry>;
  Address: Pointer): PObjectEntry;
var
  i, j: Integer;
begin
  for i := 0 to High(Types) do
    for j := 0 to High(Types[i].Objects) do
      if Types[i].Objects[j].Other.ObjectAddress = Address then
        Exit(@Types[i].Objects[j]);

  Result := nil;
end;

{ Types }

function NtxEnumerateTypes(out Types: TArray<TObjectTypeInfo>): TNtxStatus;
var
  Buffer: PObjectTypesInformation;
  pType: PObjectTypeInformation;
  BufferSize, Required: Cardinal;
  i: Integer;
begin
  Result.Location := 'NtQueryObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ObjectTypesInformation);
  Result.LastCall.InfoClassType := TypeInfo(TObjectInformationClass);

  BufferSize := SizeOf(TObjectTypesInformation);
  repeat
    Buffer := AllocMem(BufferSize);

    Required := 0;
    Result.Status := NtQueryObject(0, ObjectTypesInformation, Buffer,
      BufferSize, @Required);

    if not Result.IsSuccess then
      FreeMem(Buffer);

  until not NtxExpandBuffer(Result, BufferSize, Required, True);

  if not Result.IsSuccess then
    Exit;

  SetLength(Types, Buffer.NumberOfTypes);

  i := 0;
  pType := Offset(Buffer, SizeOf(NativeUInt));

  repeat
    Types[i].Other := pType^;
    Types[i].TypeName := pType.TypeName.ToString;
    Types[i].Other.TypeName.Buffer := PWideChar(Types[i].TypeName);

    // Until Win 8.1 ObQueryTypeInfo didn't write anything to TypeIndex field.
    // Fix it by manually calculating this value.

    // Note: NtQueryObject iterates through ObpObjectTypes which is zero-based;
    // but TypeIndex is an index in ObTypeIndexTable which starts with 2.

    if Types[i].Other.TypeIndex = 0 then
      Types[i].Other.TypeIndex := OB_TYPE_INDEX_TABLE_TYPE_OFFSET + i;

    pType := Offset(pType, AlighUp(SizeOf(TObjectTypeInformation)) +
      AlighUp(pType.TypeName.MaximumLength));

    Inc(i);
  until i > High(Types);

  FreeMem(Buffer);
end;

end.
