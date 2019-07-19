unit NtUtils.Objects;

interface
{$WARN SYMBOL_PLATFORM OFF}

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntobapi, NtUtils.Exceptions;

type
  TObjectTypeInfo = record
    TypeName: String;
    Other: TObjectTypeInformation;
  end;

// Close a handle safely and set it to zero
function NtxSafeClose(var hObject: THandle): NTSTATUS;

// Duplicate handle to an object. Supports MAXIMUM_ALLOWED.
function NtxDuplicateObject(SourceProcessHandle: THandle;
  SourceHandle: THandle; TargetProcessHandle: THandle;
  out TargetHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Options: Cardinal): TNtxStatus;

// Query name of an object
function NtxQueryNameObject(hObject: THandle; out Name: String): TNtxStatus;

// Query basic information about an object
function NtxQueryBasicInfoObject(hObject: THandle;
  out Info: TObjectBasicInformaion): TNtxStatus;

// Query object type information
function NtxQueryTypeObject(hObject: THandle;
  out Info: TObjectTypeInfo): TNtxStatus;

// Wait for an object to enter signaled state
function NtxWaitForSingleObject(hObject: THandle; Alertable: Boolean;
  Timeout: Int64): TNtxStatus; overload;
function NtxWaitForSingleObject(hObject: THandle): TNtxStatus; overload;

// Check whether two handles point to the same kernel object
function NtxCompareObjects(hObject1, hObject2: THandle;
  ObjectTypeName: String = ''): NTSTATUS;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntpsapi, System.SysUtils, NtUtils.Ldr,
  NtUtils.Objects.Snapshots, NtUtils.Tokens;

function NtxSafeClose(var hObject: THandle): NTSTATUS;
begin
  if hObject = NtCurrentProcess then
    Exit(STATUS_INVALID_HANDLE);

  if hObject = NtCurrentThread then
    Exit(STATUS_INVALID_HANDLE);

  Result := STATUS_UNSUCCESSFUL;
  try
    // NtClose can raise errors, we should capture them
    Result := NtClose(hObject);
  except
    on E: EExternalException do
      if Assigned(E.ExceptionRecord) then
        Result := E.ExceptionRecord.ExceptionCode;
  end;

  // Log failed close attempts
  if not NT_SUCCESS(Result) then
    ENtError.Report(Result, 'NtClose 0x' + IntToHex(hObject, 0));

  // Prevent future use
  hObject := 0;
end;

function NtxDuplicateObject(SourceProcessHandle: THandle;
  SourceHandle: THandle; TargetProcessHandle: THandle;
  out TargetHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Options: Cardinal): TNtxStatus;
var
  hSameAccess, hTemp: THandle;
  objInfo: TObjectBasicInformaion;
  handleInfo: TObjectHandleFlagInformation;
  bit: Integer;
label
  MaskExpandingDone;
begin
  // NtDuplicateObject does not support MAXIMUM_ALLOWED (it returns zero
  // access instead). We will implement this feature by probing additional
  // access masks.

  Result.Location := 'NtDuplicateObject';

  if (DesiredAccess = MAXIMUM_ALLOWED) and
    (Options and DUPLICATE_SAME_ACCESS = 0) then
  begin
    // To prevent race conditions we duplicate the handle to the current process
    // with the same access and attributes to perform all further probing on it.
    // This operation might close the source handle if DUPLICATE_CLOSE_SOURCE is
    // specified.

    Result.Status := NtDuplicateObject(SourceProcessHandle, SourceHandle,
      NtCurrentProcess, hSameAccess, 0, HandleAttributes,
      Options or DUPLICATE_SAME_ACCESS);

    // If we can't do it we are finished
    if not Result.IsSuccess then
      Exit;

    // Start probing. Try full access first.
    DesiredAccess := STANDARD_RIGHTS_ALL or SPECIFIC_RIGHTS_ALL;

    Result.Status := NtDuplicateObject(NtCurrentProcess, hSameAccess,
      NtCurrentProcess, hTemp, DesiredAccess, 0, 0);

    // Was the guess correct?
    if Result.IsSuccess then
    begin
      NtxSafeClose(hTemp);
      goto MaskExpandingDone;
    end;

    // Did something else happen?
    if Result.Status <> STATUS_ACCESS_DENIED then
      Exit;

    // Query what access we already have based on DUPLICATE_SAME_ACCESS flag
    if NT_SUCCESS(NtQueryObject(hSameAccess, ObjectBasicInformation, @objInfo,
      SizeOf(objInfo), nil)) then
      DesiredAccess := objInfo.GrantedAccess and not ACCESS_SYSTEM_SECURITY
    else
      DesiredAccess := 0;

    // Try each one standard or specific access right that is not granted yet
    for bit := 0 to 31 do
      if ((STANDARD_RIGHTS_ALL or SPECIFIC_RIGHTS_ALL) and (1 shl bit)
        and not DesiredAccess) <> 0 then
        if NT_SUCCESS(NtDuplicateObject(NtCurrentProcess, hSameAccess,
          NtCurrentProcess, hTemp, (1 shl bit), 0, 0)) then
        begin
          // Yes, this access can be granted, add it
          DesiredAccess := DesiredAccess or (1 shl bit);
          NtxSafeClose(hTemp);
        end;

    // Finally, duplicate the handle to the target process with the requested
    // attributes and expanded maximum access
    MaskExpandingDone:

    Result.Status := NtDuplicateObject(NtCurrentProcess, hSameAccess,
      TargetProcessHandle, TargetHandle, DesiredAccess, HandleAttributes,
      Options and not DUPLICATE_CLOSE_SOURCE);

    // Make sure our copy is closable by clearing protection
    if (Options and DUPLICATE_SAME_ATTRIBUTES <> 0) or
      (HandleAttributes and OBJ_PROTECT_CLOSE <> 0) then
    begin
      handleInfo.Inherit := False;
      handleInfo.ProtectFromClose := False;

      NtSetInformationObject(hSameAccess, ObjectHandleFlagInformation,
        @handleInfo, SizeOf(handleInfo));
    end;

    // Close local copy
    NtxSafeClose(hSameAccess);
  end
  else
  begin
    // Usual case
    Result.Status := NtDuplicateObject(SourceProcessHandle, SourceHandle,
      TargetProcessHandle, TargetHandle, DesiredAccess, HandleAttributes,
      Options);
  end;
end;

function NtxQueryNameObject(hObject: THandle; out Name: String): TNtxStatus;
var
  Buffer: PUNICODE_STRING;
  BufferSize: Cardinal;
begin
  Result.Location := 'NtQueryObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ObjectNameInformation);
  Result.LastCall.InfoClassType := TypeInfo(TObjectInformationClass);

  BufferSize := 0;
  Result.Status := NtQueryObject(hObject, ObjectNameInformation, nil, 0,
    @BufferSize);

  if not NtxTryCheckBuffer(Result.Status, BufferSize) then
    Exit;

  Buffer := AllocMem(BufferSize);

  Result.Status := NtQueryObject(hObject, ObjectNameInformation, Buffer,
    BufferSize, nil);

  if Result.IsSuccess then
    Name := Buffer.ToString;

  FreeMem(Buffer);
end;

function NtxQueryBasicInfoObject(hObject: THandle;
  out Info: TObjectBasicInformaion): TNtxStatus;
begin
  Result.Location := 'NtQueryObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ObjectBasicInformation);
  Result.LastCall.InfoClassType := TypeInfo(TObjectInformationClass);

  Result.Status := NtQueryObject(hObject, ObjectBasicInformation, @Info,
    SizeOf(Info), nil);
end;

function NtxQueryTypeObject(hObject: THandle;
  out Info: TObjectTypeInfo): TNtxStatus;
var
  Buffer: PObjectTypeInformation;
  BufferSize: Cardinal;
begin
  Result.Location := 'NtQueryObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ObjectTypeInformation);
  Result.LastCall.InfoClassType := TypeInfo(TObjectInformationClass);

  BufferSize := 0;
  Result.Status := NtQueryObject(hObject, ObjectTypeInformation, nil, 0,
    @BufferSize);

  if not NtxTryCheckBuffer(Result.Status, BufferSize) then
    Exit;

  if BufferSize < SizeOf(TObjectTypeInformation) then
  begin
    Result.Status := STATUS_INFO_LENGTH_MISMATCH;
    Exit;
  end;

  Buffer := AllocMem(BufferSize);
  Result.Status := NtQueryObject(hObject, ObjectTypeInformation, Buffer,
    BufferSize, nil);

  if Result.IsSuccess then
  begin
    Info.TypeName := Buffer.TypeName.ToString;
    Info.Other := Buffer^;

    // Fix a UNICODE_STRNING reference by making it point to the local string
    Info.Other.TypeName.Buffer := PWideChar(Info.TypeName);
  end;

  FreeMem(Buffer);
end;

function NtxWaitForSingleObject(hObject: THandle; Alertable: Boolean;
  Timeout: Int64): TNtxStatus; overload;
var
  TimeOutValue: TLargeInteger;
begin
  TimeOutValue.QuadPart := Timeout;

  Result.Location := 'NtWaitForSingleObject';
  Result.Status := NtWaitForSingleObject(hObject, Alertable, TimeOutValue);
end;

function NtxWaitForSingleObject(hObject: THandle): TNtxStatus; overload;
begin
  Result.Location := 'NtWaitForSingleObject';
  Result.Status := NtWaitForSingleObject(hObject, True, nil);
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
      if Type1.Other.TypeIndex <> Type2.Other.TypeIndex then
        Exit(STATUS_NOT_SAME_OBJECT);

      ObjectTypeName := Type1.TypeName;
    end;

  // Perform type-specific comparison
  if ObjectTypeName <> '' then
  begin
    Result := STATUS_OBJECT_TYPE_MISMATCH;

    if ObjectTypeName = 'Token' then
      Result := NtxpCompareTokenIds(hObject1, hObject2);

    // TODO: add more types

    case Result of
      STATUS_SUCCESS, STATUS_NOT_SAME_OBJECT:
        Exit;
    end;
  end;

  // Compare named objects
  if NtxQueryNameObject(hObject1, Name1).IsSuccess and
    NtxQueryNameObject(hObject2, Name2).IsSuccess then
    if (Name1 <> Name2) then
      Exit(STATUS_NOT_SAME_OBJECT)
    else if Name1 <> '' then
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
