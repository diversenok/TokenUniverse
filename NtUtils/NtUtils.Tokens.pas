unit NtUtils.Tokens;

interface

uses
  Winapi.WinNt, Ntapi.ntseapi, Winapi.WinSafer, NtUtils.Exceptions,
  NtUtils.Security.Sid, NtUtils.Security.Acl;

{ ------------------------------ Creation ---------------------------------- }

// Open a token of a process
function NtxOpenProcessToken(out hToken: THandle; hProcess: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;

function NtxOpenProcessTokenById(out hToken: THandle; PID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;

// Open a token of a thread
function NtxOpenThreadToken(out hToken: THandle; hThread: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;

function NtxOpenThreadTokenById(out hToken: THandle; TID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;

// Copy an effective security context of a thread via direct impersonation
function NtxOpenEffectiveToken(out hToken: THandle; hThread: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; EffectiveOnly: Boolean): TNtxStatus;

function NtxOpenEffectiveTokenById(out hToken: THandle; TID: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; EffectiveOnly: Boolean): TNtxStatus;

// Duplicate existing token
function NtxDuplicateToken(out hToken: THandle; hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType; ImpersonationLevel:
  TSecurityImpersonationLevel; EffectiveOnly: LongBool): TNtxStatus;

// Open anonymous token
function NtxOpenAnonymousToken(out hToken: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal): TNtxStatus;

// Filter a token
function NtxFilterToken(out hNewToken: THandle; hToken: THandle;
  Flags: Cardinal; SidsToDisable: ISidArray; PrivilegesToDelete: TLuidDynArray;
  SidsToRestrict: ISidArray): TNtxStatus;

// Restrict a token using Safer api
function NtxRestrictSaferToken(out hToken: THandle; hTokenToRestict: THandle;
  ScopeId: TSaferScopeId; LevelId: TSaferLevelId; MakeSanboxInert: Boolean):
  TNtxStatus;

// Create a new token from scratch. Requires SeCreateTokenPrivilege.
function NtxCreateToken(out hToken: THandle; DesiredAccess: TAccessMask;
  TokenType: TTokenType; ImpersonationLevel: TSecurityImpersonationLevel;
  AuthenticationId: TLuid; ExpirationTime: TLargeInteger; User: TGroup;
  Groups: TGroupArray; Privileges: TPrivilegeArray; Owner: ISid;
  PrimaryGroup: ISid; DefaultDacl: IAcl; const TokenSource: TTokenSource):
  TNtxStatus;

{ ------------------------- Query / set information ------------------------ }

type
  NtxToken = class
    // Query fixed-size information
    class function Query<T>(hToken: THandle;
      InfoClass: TTokenInformationClass; out Buffer: T): TNtxStatus; static;

    // Set fixed-size information
    class function SetInfo<T>(hToken: THandle;
      InfoClass: TTokenInformationClass; const Buffer: T): TNtxStatus; static;
  end;

// Query variable-length token information without race conditions
function NtxQueryBufferToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Status: TNtxStatus; ReturnedSize: PCardinal = nil): Pointer;

// Set variable-length token information
function NtxSetInformationToken(hToken: THandle;
  InfoClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal): TNtxStatus;

// Query an SID (Owner, Primary group, ...)
function NtxQuerySidToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Sid: ISid): TNtxStatus;

// Query SID and attributes (User, ...)
function NtxQueryGroupToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Group: TGroup): TNtxStatus;

// Query groups (Groups, RestrictingSIDs, LogonSIDs, ...)
function NtxQueryGroupsToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Groups: TGroupArray): TNtxStatus;

// Query privileges
function NtxQueryPrivilegesToken(hToken: THandle;
  out Privileges: TPrivilegeArray): TNtxStatus;

// Query token statistic (requires either Query or Duplicate access)
function NtxQueryStatisticsToken(hToken: THandle;
  out Statistics: TTokenStatistics): TNtxStatus;

// Query integrity level of a token
function NtxQueryIntegrityToken(hToken: THandle; out  IntegrityLevel: Cardinal):
  TNtxStatus;

// Set integrity level of a token
function NtxSetIntegrityToken(hToken: THandle; IntegrityLevel: Cardinal):
  TNtxStatus;

{ --------------------------- Other operations ---------------------------- }

// Check whether two token handles reference the same kernel object
function NtxCompareTokens(hToken1, hToken2: THandle): TNtxStatus;

// Adjust privileges
function NtxAdjustPrivileges(hToken: THandle; Privileges: TLuidDynArray;
  NewAttribute: Cardinal): TNtxStatus;
function NtxAdjustPrivilege(hToken: THandle; Privilege: TLuid;
  NewAttribute: Cardinal): TNtxStatus;

// Adjust groups
function NtxAdjustGroups(hToken: THandle; Sids: ISidArray;
  NewAttribute: Cardinal; ResetToDefault: Boolean): TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntobapi, Ntapi.ntpsapi, NtUtils.Objects,
  NtUtils.DelayedImport, NtUtils.Snapshots.Handles, NtUtils.Tokens.Misc,
  NtUtils.Processes, NtUtils.Tokens.Impersonate, NtUtils.AccessMasks;

{ Creation }

function NtxOpenProcessToken(out hToken: THandle; hProcess: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
begin
  Result.Location := 'NtOpenProcessTokenEx for ' + FormatAccess(DesiredAccess,
    objToken);
  Result.Status := NtOpenProcessTokenEx(hProcess, DesiredAccess,
    HandleAttributes, hToken);
end;

function NtxOpenProcessTokenById(out hToken: THandle; PID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
var
  hProcess: THandle;
begin
  Result := NtxOpenProcess(hProcess, PROCESS_QUERY_LIMITED_INFORMATION, PID);

  if not Result.IsSuccess then
    Exit;

  Result := NtxOpenProcessToken(hToken, hProcess, DesiredAccess,
    HandleAttributes);

  NtxSafeClose(hProcess);
end;

function NtxOpenThreadToken(out hToken: THandle; hThread: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
begin
  // When opening other threads use effective (thread) security context. When
  // reading a token from the current thread use the process security context

  Result.Location := 'NtOpenThreadTokenEx for ' + FormatAccess(DesiredAccess,
    objThread);
  Result.Status := NtOpenThreadTokenEx(hThread, DesiredAccess,
    (hThread = NtCurrentThread), HandleAttributes, hToken);
end;

function NtxOpenThreadTokenById(out hToken: THandle; TID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
var
  hThread: THandle;
begin
  Result := NtxOpenThread(hThread, THREAD_QUERY_LIMITED_INFORMATION, TID);

  if not Result.IsSuccess then
    Exit;

  Result := NtxOpenThreadToken(hToken, hThread, DesiredAccess,
    HandleAttributes);

  NtxSafeClose(hThread);
end;

function NtxOpenEffectiveToken(out hToken: THandle; hThread: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; EffectiveOnly: Boolean): TNtxStatus;
var
  hOldToken: THandle;
  QoS: TSecurityQualityOfService;
begin
  // Backup our impersonation token
  hOldToken := NtxBackupImpersonation(NtCurrentThread);

  InitializaQoS(QoS, ImpersonationLevel, EffectiveOnly);

  // Direct impersonation makes the server thread to impersonate the effective
  // security context of the client thread. We use our thead as a server and the
  // target thread as a client, and then read the token from our thread.

  // The server thread handle requires THREAD_IMPERSONATE access.
  // The client thread handle requires THREAD_DIRECT_IMPERSONATION access.
  // No access checks are performed on the token, we obtain a copy of it.

  Result.Location := 'NtImpersonateThread';
  Result.Status := NtImpersonateThread(NtCurrentThread, hThread, QoS);

  if not Result.IsSuccess then
    Exit;

  // Read it back from our thread
  Result := NtxOpenThreadToken(hToken, NtCurrentThread, DesiredAccess,
    HandleAttributes);

  // Restore our previous impersonation
  NtxRestoreImpersonation(NtCurrentThread, hOldToken);

  if hOldToken <> 0  then
    NtxSafeClose(hOldToken);
end;

function NtxOpenEffectiveTokenById(out hToken: THandle; TID: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; EffectiveOnly: Boolean): TNtxStatus;
var
  hThread: THandle;
begin
  Result := NtxOpenThread(hThread, THREAD_DIRECT_IMPERSONATION, TID);

  if not Result.IsSuccess then
    Exit;

  Result := NtxOpenEffectiveToken(hToken, hThread, ImpersonationLevel,
    DesiredAccess, HandleAttributes, EffectiveOnly);

  NtxSafeClose(hThread);
end;

function NtxDuplicateToken(out hToken: THandle; hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType; ImpersonationLevel:
  TSecurityImpersonationLevel; EffectiveOnly: LongBool): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  QoS: TSecurityQualityOfService;
begin
  InitializaQoS(QoS, ImpersonationLevel, EffectiveOnly);
  InitializeObjectAttributes(ObjAttr, nil, 0, 0, @QoS);

  Result.Location := 'NtDuplicateToken for ' + FormatAccess(DesiredAccess,
    objToken);
  Result.Status := NtDuplicateToken(hExistingToken, DesiredAccess, @ObjAttr,
    EffectiveOnly, TokenType, hToken);
end;

function NtxOpenAnonymousToken(out hToken: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal): TNtxStatus;
var
  hOldToken: THandle;
begin
  // Backup our impersonation context
  hOldToken := NtxBackupImpersonation(NtCurrentThread);

  // Set our thread to impersonate anonymous token
  Result.Location := 'NtImpersonateAnonymousToken';
  Result.Status := NtImpersonateAnonymousToken(NtCurrentThread);

  // Read the token from the thread
  if Result.IsSuccess then
    Result := NtxOpenThreadToken(hToken, NtCurrentThread, DesiredAccess,
      HandleAttributes);

  // Restore previous impersonation
  NtxRestoreImpersonation(NtCurrentThread, hOldToken);

  if hOldToken <> 0 then
    NtxSafeClose(hOldToken);
end;

function NtxFilterToken(out hNewToken: THandle; hToken: THandle;
  Flags: Cardinal; SidsToDisable: ISidArray; PrivilegesToDelete: TLuidDynArray;
  SidsToRestrict: ISidArray): TNtxStatus;
var
  DisableSids, RestrictSids: PTokenGroups;
  DeletePrivileges: PTokenPrivileges;
begin
  DisableSids := NtxpAllocGroups(SidsToDisable, 0);
  RestrictSids := NtxpAllocGroups(SidsToRestrict, 0);
  DeletePrivileges := NtxpAllocPrivileges(PrivilegesToDelete, 0);

  Result.Location := 'NtFilterToken';
  Result.Status := NtFilterToken(hToken, Flags, DisableSids, DeletePrivileges,
    RestrictSids, hNewToken);

  FreeMem(DisableSids);
  FreeMem(RestrictSids);
  FreeMem(DeletePrivileges);
end;

function NtxRestrictSaferToken(out hToken: THandle; hTokenToRestict: THandle;
  ScopeId: TSaferScopeId; LevelId: TSaferLevelId; MakeSanboxInert: Boolean):
  TNtxStatus;
var
  hLevel: TSaferLevelHandle;
  Flags: Cardinal;
begin
  Result.Location := 'SaferCreateLevel';
  Result.Win32Result := SaferCreateLevel(ScopeId, LevelId, SAFER_LEVEL_OPEN,
    hLevel);

  if not Result.IsSuccess then
    Exit;

  Flags := 0;
  if MakeSanboxInert then
    Flags := Flags or SAFER_TOKEN_MAKE_INERT;

  Result.Location := 'SaferComputeTokenFromLevel';
  Result.Win32Result := SaferComputeTokenFromLevel(hLevel, hTokenToRestict,
    hToken, Flags, nil);

  SaferCloseLevel(hLevel);
end;

function NtxCreateToken(out hToken: THandle; DesiredAccess: TAccessMask;
  TokenType: TTokenType; ImpersonationLevel: TSecurityImpersonationLevel;
  AuthenticationId: TLuid; ExpirationTime: TLargeInteger; User: TGroup;
  Groups: TGroupArray; Privileges: TPrivilegeArray; Owner: ISid;
  PrimaryGroup: ISid; DefaultDacl: IAcl; const TokenSource: TTokenSource):
  TNtxStatus;
var
  QoS: TSecurityQualityOfService;
  ObjAttr: TObjectAttributes;
  TokenUser: TTokenUser;
  TokenGroups: PTokenGroups;
  TokenPrivileges: PTokenPrivileges;
  TokenOwner: TTokenOwner;
  pTokenOwnerRef: PTokenOwner;
  TokenPrimaryGroup: TTokenPrimaryGroup;
  TokenDefaultDacl: TTokenDefaultDacl;
  pTokenDefaultDaclRef: PTokenDefaultDacl;
begin
  InitializaQoS(QoS, ImpersonationLevel);
  InitializeObjectAttributes(ObjAttr, nil, 0, 0, @QoS);

  // Prepare user
  Assert(Assigned(User.SecurityIdentifier));
  TokenUser.User.Sid := User.SecurityIdentifier.Sid;
  TokenUser.User.Attributes := User.Attributes;

  // Prepare groups and privileges
  TokenGroups := NtxpAllocGroups2(Groups);
  TokenPrivileges:= NtxpAllocPrivileges2(Privileges);

  // Owner is optional
  if Assigned(Owner) then
  begin
    TokenOwner.Owner := Owner.Sid;
    pTokenOwnerRef := @TokenOwner;
  end
  else
    pTokenOwnerRef := nil;

  // Prepare primary group
  Assert(Assigned(PrimaryGroup));
  TokenPrimaryGroup.PrimaryGroup := PrimaryGroup.Sid;

  // Default Dacl is optional
  if Assigned(DefaultDacl) then
  begin
    TokenDefaultDacl.DefaultDacl := DefaultDacl.Acl;
    pTokenDefaultDaclRef := @TokenDefaultDacl;
  end
  else
    pTokenDefaultDaclRef := nil;

  Result.Location := 'NtCreateToken';
  Result.Status := NtCreateToken(hToken, DesiredAccess, @ObjAttr, TokenType,
    AuthenticationId, ExpirationTime, TokenUser, TokenGroups, TokenPrivileges,
    pTokenOwnerRef, TokenPrimaryGroup, pTokenDefaultDaclRef, TokenSource);

  // Clean up
  FreeMem(TokenGroups);
  FreeMem(TokenPrivileges);
end;

{ Query / set operations }

class function NtxToken.Query<T>(hToken: THandle;
  InfoClass: TTokenInformationClass; out Buffer: T): TNtxStatus;
var
  ReturnedBytes: Cardinal;
begin
  Result.Location := NtxFormatTokenQuery(InfoClass);
  Result.Status := NtQueryInformationToken(hToken, InfoClass, @Buffer,
    SizeOf(Buffer), ReturnedBytes);
end;

class function NtxToken.SetInfo<T>(hToken: THandle;
  InfoClass: TTokenInformationClass; const Buffer: T): TNtxStatus;
begin
  Result.Location := NtxFormatTokenSet(InfoClass);
  Result.Status := NtSetInformationToken(hToken, InfoClass, @Buffer,
    SizeOf(Buffer));
end;

function NtxQueryBufferToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Status: TNtxStatus; ReturnedSize: PCardinal): Pointer;
var
  BufferSize, Required: Cardinal;
begin
  Result := nil;
  BufferSize := 0;
  Required := 0;

  // The requested information length might change between calls. Prevent
  // the race condition with a loop.
  while True do
  begin
    Status.Location := NtxFormatTokenQuery(InfoClass);
    Status.Status := NtQueryInformationToken(hToken, InfoClass, Result,
      BufferSize, Required);

    // Quit the loop on success
    if Status.IsSuccess then
    begin
      if Assigned(ReturnedSize) then
        ReturnedSize^ := BufferSize;
      Exit;
    end;

    // Quit on errors that are not related to the buffer size
    if not NtxTryCheckBuffer(Status.Status, Required) then
      Exit(nil);

    // Free previous buffer and allocate a new one
    FreeMem(Result);

    BufferSize := Required;
    Result := AllocMem(BufferSize);
  end;
end;

function NtxSetInformationToken(hToken: THandle;
  InfoClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal): TNtxStatus;
begin
  Result.Location := NtxFormatTokenSet(InfoClass);
  Result.Status := NtSetInformationToken(hToken, InfoClass, TokenInformation,
    TokenInformationLength);
end;

function NtxQuerySidToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Sid: ISid): TNtxStatus;
var
  Buffer: PTokenOwner; // aka PTokenPrimaryGroup and ^PSid
begin
  Buffer := NtxQueryBufferToken(hToken, InfoClass, Result);

  if not Result.IsSuccess then
    Exit;

  try
    Sid := TSid.CreateCopy(Buffer.Owner);
  finally
    FreeMem(Buffer);
  end;
end;

function NtxQueryGroupToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Group: TGroup): TNtxStatus;
var
  Buffer: PSidAndAttributes; // aka PTokenUser
begin
  Buffer := NtxQueryBufferToken(hToken, InfoClass, Result);

  if not Result.IsSuccess then
    Exit;

  try
    Group.SecurityIdentifier := TSid.CreateCopy(Buffer.Sid);
    Group.Attributes := Buffer.Attributes;
  finally
    FreeMem(Buffer);
  end;
end;

function NtxQueryGroupsToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Groups: TGroupArray): TNtxStatus;
var
  Buffer: PTokenGroups;
  i: Integer;
begin
  Buffer := NtxQueryBufferToken(hToken, InfoClass, Result);

  if not Result.IsSuccess then
    Exit;

  try
    SetLength(Groups, Buffer.GroupCount);
    for i := 0 to High(Groups) do
    begin
      Groups[i].SecurityIdentifier := TSid.CreateCopy(Buffer.Groups[i].Sid);
      Groups[i].Attributes := Buffer.Groups[i].Attributes;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function NtxQueryPrivilegesToken(hToken: THandle;
  out Privileges: TPrivilegeArray): TNtxStatus;
var
  Buffer: PTokenPrivileges;
  i: Integer;
begin
  Buffer := NtxQueryBufferToken(hToken, TokenPrivileges, Result);

  if not Result.IsSuccess then
    Exit;

  try
    SetLength(Privileges, Buffer.PrivilegeCount);

    for i := 0 to High(Privileges) do
      Privileges[i] := Buffer.Privileges[i];
  finally
    FreeMem(Buffer);
  end;
end;

function NtxQueryStatisticsToken(hToken: THandle;
  out Statistics: TTokenStatistics): TNtxStatus;
var
  Returned: Cardinal;
  hTokenRef: THandle;
begin
  Result.Location := NtxFormatTokenQuery(TokenStatistics);
  Result.Status := NtQueryInformationToken(hToken, TokenStatistics, @Statistics,
    SizeOf(Statistics), Returned);

  // Try to process the case of a handle with no TOKEN_QUERY access
  if (Result.Status = STATUS_ACCESS_DENIED) and
    NT_SUCCESS(NtDuplicateObject(NtCurrentProcess, hToken,
    NtCurrentProcess, hTokenRef, TOKEN_QUERY, 0, 0)) then
  begin
    Result.Status := NtQueryInformationToken(hTokenRef, TokenStatistics,
      @Statistics, SizeOf(Statistics), Returned);

    NtxSafeClose(hTokenRef);
  end;
end;

function NtxQueryIntegrityToken(hToken: THandle; out IntegrityLevel: Cardinal):
  TNtxStatus;
var
  Integrity: TGroup;
begin
  Result := NtxQueryGroupToken(hToken, TokenIntegrityLevel, Integrity);

  if not Result.IsSuccess then
    Exit;

  // Integrity level is the last sub-authority (RID) of the integrity SID
  with Integrity.SecurityIdentifier do
    if SubAuthorities > 0 then
      IntegrityLevel := Rid
    else
      IntegrityLevel := SECURITY_MANDATORY_UNTRUSTED_RID
end;

function NtxSetIntegrityToken(hToken: THandle; IntegrityLevel: Cardinal):
  TNtxStatus;
var
  LabelSid: ISid;
  MandatoryLabel: TSidAndAttributes;
begin
  // Prepare SID for integrity level with 1 sub authority: S-1-16-X.

  LabelSid := TSid.CreateNew(SECURITY_MANDATORY_LABEL_AUTHORITY, 1,
    IntegrityLevel);

  MandatoryLabel.Sid := LabelSid.Sid;
  MandatoryLabel.Attributes := SE_GROUP_INTEGRITY_ENABLED;

  Result.Location := NtxFormatTokenSet(TokenIntegrityLevel);
  Result.Status := NtSetInformationToken(hToken, TokenIntegrityLevel,
    @MandatoryLabel, SizeOf(MandatoryLabel));
end;

{ Other opeations }

function NtxCompareTokens(hToken1, hToken2: THandle): TNtxStatus;
var
  Statistics1, Statistics2: TTokenStatistics;
begin
  if hToken1 = hToken2 then
  begin
    Result.Status := STATUS_SUCCESS;
    Exit;
  end;

  // Win 10 TH+ makes things way easier
  if NtxCheckNtDelayedImport('NtCompareObjects').IsSuccess then
  begin
    Result.Location := 'NtCompareObjects';
    Result.Status := NtCompareObjects(hToken1, hToken2);
    Exit;
  end;

  // Try to perform a comparison based on TokenIDs. NtxQueryStatisticsToken
  // might be capable of handling it even without TOKEN_QUERY access.

  Result := NtxQueryStatisticsToken(hToken1, Statistics1);
  if Result.IsSuccess then
  begin
    Result := NtxQueryStatisticsToken(hToken2, Statistics2);

    if Result.IsSuccess then
    begin
      if Statistics1.TokenId = Statistics2.TokenId then
        Result.Status := STATUS_SUCCESS
      else
      begin
        Result.Location := 'NtxCompareTokens';
        Result.Status := STATUS_NOT_SAME_OBJECT;
      end;

      Exit;
    end;
  end;

  if Result.Status <> STATUS_ACCESS_DENIED then
    Exit;

  // The only way to proceed is via a handle snaphot
  Result.Location := 'NtQuerySystemInformation [SystemExtendedHandleInformation]';
  Result.Status := THandleSnapshot.Compare(hToken1, hToken2);
end;

function NtxAdjustPrivileges(hToken: THandle; Privileges: TLuidDynArray;
  NewAttribute: Cardinal): TNtxStatus;
var
  Buffer: PTokenPrivileges;
begin
  Buffer := NtxpAllocPrivileges(Privileges, NewAttribute);

  Result.Location := 'NtAdjustPrivilegesToken';
  Result.Status := NtAdjustPrivilegesToken(hToken, False, Buffer, 0, nil, nil);

  FreeMem(Buffer);
end;

function NtxAdjustPrivilege(hToken: THandle; Privilege: TLuid;
  NewAttribute: Cardinal): TNtxStatus;
var
  Privileges: TLuidDynArray;
begin
  SetLength(Privileges, 1);
  Privileges[0] := Privilege;
  Result := NtxAdjustPrivileges(hToken, Privileges, NewAttribute);
end;

function NtxAdjustGroups(hToken: THandle; Sids: ISidArray;
  NewAttribute: Cardinal; ResetToDefault: Boolean): TNtxStatus;
var
  Buffer: PTokenGroups;
begin
  Buffer := NtxpAllocGroups(Sids, NewAttribute);

  Result.Location := 'NtAdjustGroupsToken';
  Result.Status := NtAdjustGroupsToken(hToken, ResetToDefault, Buffer, 0, nil,
    nil);

  FreeMem(Buffer);
end;

end.
