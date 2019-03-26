unit NtUtils.ApiExtension;

interface
{$WARN SYMBOL_PLATFORM OFF}

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntobapi;

// NtClose without exceptions on protected handles
function NtxSafeClose(hObject: THandle): NTSTATUS;

// NtQueryObject with ObjectNameInformation
function NtxQueryNameObject(hObject: THandle; out Name: String): NTSTATUS;

// NtDuplicateObject that supports MAXIMUM_ALLOWED
function NtxDuplicateObject(SourceProcessHandle: THandle;
  SourceHandle: THandle; TargetProcessHandle: THandle;
  out TargetHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Options: Cardinal): NTSTATUS;

// NtQueryInformationToken for variable-sized buffers without race conditions
function NtxQueryBufferToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Status: NTSTATUS; ReturnedSize: PCardinal = nil): Pointer;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntseapi, Ntapi.ntpsapi,
   NtUtils.Exceptions, System.SysUtils;

{ TNtObject }

function NtxSafeClose(hObject: THandle): NTSTATUS;
begin
  Result := STATUS_UNSUCCESSFUL;
  try
    // NtClose can raise errors, we should capture them
    Result := NtClose(hObject);
  except
    on E: EExternalException do
      if Assigned(E.ExceptionRecord) then
        Result := E.ExceptionRecord.ExceptionCode;
  end;
end;

function NtxQueryNameObject(hObject: THandle; out Name: String): NTSTATUS;
var
  Buffer: PUNICODE_STRING;
  BufferSize: Cardinal;
begin
  BufferSize := 0;
  Result := NtQueryObject(hObject, ObjectNameInformation, nil, 0, @BufferSize);

  if not NativeTryCheckBuffer(Result, BufferSize) then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    Result := NtQueryObject(hObject, ObjectNameInformation, Buffer, BufferSize,
      nil);

    if NT_SUCCESS(Result) then
      Name := Buffer.ToString;
  finally
    FreeMem(Buffer);
  end;
end;

function NtxDuplicateObject(SourceProcessHandle: THandle;
  SourceHandle: THandle; TargetProcessHandle: THandle;
  out TargetHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Options: Cardinal): NTSTATUS;
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

  if (DesiredAccess = MAXIMUM_ALLOWED) and
    (Options and DUPLICATE_SAME_ACCESS = 0) then
  begin
    // To prevent race conditions we duplicate the handle to the current process
    // with the same access and attributes to perform all further probing on it.
    // This operation might close the source handle if DUPLICATE_CLOSE_SOURCE is
    // specified.

    Result := NtDuplicateObject(SourceProcessHandle, SourceHandle,
      NtCurrentProcess, hSameAccess, 0, HandleAttributes,
      Options or DUPLICATE_SAME_ACCESS);

    // If we can't do it we are finished
    if not NT_SUCCESS(Result) then
      Exit;

    // Start probing. Try full access first.
    DesiredAccess := STANDARD_RIGHTS_ALL or SPECIFIC_RIGHTS_ALL;

    Result := NtDuplicateObject(NtCurrentProcess, hSameAccess, NtCurrentProcess,
      hTemp, DesiredAccess, 0, 0);

    // Was the guess correct?
    if NT_SUCCESS(Result) then
    begin
      NtxSafeClose(hTemp);
      goto MaskExpandingDone;
    end;

    // Did something else happen?
    if Result <> STATUS_ACCESS_DENIED then
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

    Result := NtDuplicateObject(NtCurrentProcess, hSameAccess,
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
    Result := NtDuplicateObject(SourceProcessHandle, SourceHandle,
      TargetProcessHandle, TargetHandle, DesiredAccess, HandleAttributes,
      Options);
  end;
end;

function NtxQueryBufferToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Status: NTSTATUS; ReturnedSize: PCardinal): Pointer;
var
  BufferSize, RequiredSize: Cardinal;
begin
  Result := nil;
  BufferSize := 0;
  RequiredSize := 0;

  // The requested information length might change between calls. Prevent
  // the race condition with a loop.
  while True do
  begin
    Status := NtQueryInformationToken(hToken, InfoClass, Result, BufferSize,
      RequiredSize);

    // Quit the loop on success
    if NT_SUCCESS(Status) then
    begin
      if Assigned(ReturnedSize) then
        ReturnedSize^ := BufferSize;
      Exit;
    end;

    // Quit on errors that are not related to the buffer size
    if not NativeTryCheckBuffer(Status, RequiredSize) then
      Exit(nil);

    // Free previous buffer and allocate a new one
    FreeMem(Result);

    BufferSize := RequiredSize;
    Result := AllocMem(BufferSize);
  end;
end;

end.
