unit NtUtils.Tokens;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntseapi, Winapi.WinSafer, NtUtils.Exceptions,
  NtUtils.Security.Sid, NtUtils.Security.Acl;

{ ------------------------------ Creation ---------------------------------- }

// Open a token of a process
function NtxOpenProcessToken(out hToken: THandle; hProcess: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;

function NtxOpenProcessTokenById(out hToken: THandle; PID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;

// Open a token of a thread
function NtxOpenThreadToken(out hToken: THandle; hThread: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;

function NtxOpenThreadTokenById(out hToken: THandle; TID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;

// Copy an effective security context of a thread via direct impersonation
function NtxOpenEffectiveToken(out hToken: THandle; hThread: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal = 0; EffectiveOnly: Boolean = False): TNtxStatus;

function NtxOpenEffectiveTokenById(out hToken: THandle; TID: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal = 0; EffectiveOnly: Boolean = False): TNtxStatus;

// Duplicate existing token
function NtxDuplicateToken(out hToken: THandle; hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType; ImpersonationLevel:
  TSecurityImpersonationLevel = SecurityImpersonation;
  HandleAttributes: Cardinal = 0; EffectiveOnly: Boolean = False): TNtxStatus;

// Open anonymous token
function NtxOpenAnonymousToken(out hToken: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal = 0): TNtxStatus;

// Filter a token
function NtxFilterToken(out hNewToken: THandle; hToken: THandle;
  Flags: Cardinal; SidsToDisable: TArray<ISid>;
  PrivilegesToDelete: TArray<TLuid>; SidsToRestrict: TArray<ISid>): TNtxStatus;

// Restrict a token using Safer api
function NtxRestrictSaferToken(out hToken: THandle; hTokenToRestict: THandle;
  ScopeId: TSaferScopeId; LevelId: TSaferLevelId; MakeSanboxInert: Boolean):
  TNtxStatus;

// Create a new token from scratch. Requires SeCreateTokenPrivilege.
function NtxCreateToken(out hToken: THandle; TokenType: TTokenType;
  ImpersonationLevel: TSecurityImpersonationLevel; AuthenticationId: TLuid;
  ExpirationTime: TLargeInteger; User: TGroup; Groups: TArray<TGroup>;
  Privileges: TArray<TPrivilege>; Owner: ISid; PrimaryGroup: ISid;
  DefaultDacl: IAcl; const TokenSource: TTokenSource;
  DesiredAccess: TAccessMask = TOKEN_ALL_ACCESS; HandleAttributes: Cardinal = 0)
  : TNtxStatus;

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
  out Groups: TArray<TGroup>): TNtxStatus;

// Query privileges
function NtxQueryPrivilegesToken(hToken: THandle;
  out Privileges: TArray<TPrivilege>): TNtxStatus;

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

// Compare token objects using Token IDs
function NtxpCompareTokenIds(hToken1, hToken2: THandle): NTSTATUS;

// Adjust privileges
function NtxAdjustPrivileges(hToken: THandle; Privileges: TArray<TLuid>;
  NewAttribute: Cardinal): TNtxStatus;
function NtxAdjustPrivilege(hToken: THandle; Privilege: TLuid;
  NewAttribute: Cardinal): TNtxStatus;

// Adjust groups
function NtxAdjustGroups(hToken: THandle; Sids: TArray<ISid>;
  NewAttribute: Cardinal; ResetToDefault: Boolean): TNtxStatus;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntobapi, Ntapi.ntpsapi, NtUtils.Objects,
  NtUtils.Tokens.Misc, NtUtils.Processes, NtUtils.Tokens.Impersonate;

{ Creation }

function NtxOpenProcessToken(out hToken: THandle; hProcess: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
begin
  Result.Location := 'NtOpenProcessTokenEx';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objNtToken;

  Result.Status := NtOpenProcessTokenEx(hProcess, DesiredAccess,
    HandleAttributes, hToken);
end;

function NtxOpenProcessTokenById(out hToken: THandle; PID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
var
  hProcess: THandle;
begin
  Result := NtxOpenProcess(hProcess, PID, PROCESS_QUERY_LIMITED_INFORMATION);

  if not Result.IsSuccess then
    Exit;

  Result := NtxOpenProcessToken(hToken, hProcess, DesiredAccess,
    HandleAttributes);

  NtxSafeClose(hProcess);
end;

function NtxOpenThreadToken(out hToken: THandle; hThread: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
begin
  Result.Location := 'NtOpenThreadTokenEx';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objNtToken;

  // When opening other threads use effective (thread) security context. When
  // reading a token from the current thread use the process security context

  Result.Status := NtOpenThreadTokenEx(hThread, DesiredAccess,
    (hThread = NtCurrentThread), HandleAttributes, hToken);
end;

function NtxOpenThreadTokenById(out hToken: THandle; TID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
var
  hThread: THandle;
begin
  Result := NtxOpenThread(hThread, TID, THREAD_QUERY_LIMITED_INFORMATION);

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
  Result := NtxOpenThread(hThread, TID, THREAD_DIRECT_IMPERSONATION);

  if not Result.IsSuccess then
    Exit;

  Result := NtxOpenEffectiveToken(hToken, hThread, ImpersonationLevel,
    DesiredAccess, HandleAttributes, EffectiveOnly);

  NtxSafeClose(hThread);
end;

function NtxDuplicateToken(out hToken: THandle; hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType; ImpersonationLevel:
  TSecurityImpersonationLevel; HandleAttributes: Cardinal;
  EffectiveOnly: Boolean): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  QoS: TSecurityQualityOfService;
begin
  InitializaQoS(QoS, ImpersonationLevel, EffectiveOnly);
  InitializeObjectAttributes(ObjAttr, nil, HandleAttributes, 0, @QoS);

  Result.Location := 'NtDuplicateToken';
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
  Flags: Cardinal; SidsToDisable: TArray<ISid>;
  PrivilegesToDelete: TArray<TLuid>; SidsToRestrict: TArray<ISid>): TNtxStatus;
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

function NtxCreateToken(out hToken: THandle; TokenType: TTokenType;
  ImpersonationLevel: TSecurityImpersonationLevel; AuthenticationId: TLuid;
  ExpirationTime: TLargeInteger; User: TGroup; Groups: TArray<TGroup>;
  Privileges: TArray<TPrivilege>; Owner: ISid; PrimaryGroup: ISid;
  DefaultDacl: IAcl; const TokenSource: TTokenSource;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
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
  InitializeObjectAttributes(ObjAttr, nil, HandleAttributes, 0, @QoS);

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
  NtxFormatTokenQuery(Result, InfoClass);
  Result.Status := NtQueryInformationToken(hToken, InfoClass, @Buffer,
    SizeOf(Buffer), ReturnedBytes);
end;

class function NtxToken.SetInfo<T>(hToken: THandle;
  InfoClass: TTokenInformationClass; const Buffer: T): TNtxStatus;
begin
  NtxFormatTokenSet(Result, InfoClass);
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
  NtxFormatTokenQuery(Status, InfoClass);

  BufferSize := 0;
  repeat
    Result := AllocMem(BufferSize);

    Required := 0;
    Status.Status := NtQueryInformationToken(hToken, InfoClass, Result,
      BufferSize, Required);

    if not Status.IsSuccess then
    begin
      FreeMem(Result);
      Result := nil;
    end;

  until not NtxExpandBuffer(Status, BufferSize, Required);

  if Status.IsSuccess and Assigned(ReturnedSize) then
    ReturnedSize^ := BufferSize;
end;

function NtxSetInformationToken(hToken: THandle;
  InfoClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal): TNtxStatus;
begin
  NtxFormatTokenSet(Result, InfoClass);
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
  out Groups: TArray<TGroup>): TNtxStatus;
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
  out Privileges: TArray<TPrivilege>): TNtxStatus;
var
  Buffer: PTokenPrivileges;
  i: Integer;
begin
  Buffer := NtxQueryBufferToken(hToken, TokenPrivileges, Result);

  if not Result.IsSuccess then
    Exit;

  SetLength(Privileges, Buffer.PrivilegeCount);

  for i := 0 to High(Privileges) do
    Privileges[i] := Buffer.Privileges{$R-}[i]{$R+};

  FreeMem(Buffer);
end;

function NtxQueryStatisticsToken(hToken: THandle;
  out Statistics: TTokenStatistics): TNtxStatus;
var
  hTokenRef: THandle;
begin
  Result := NtxToken.Query<TTokenStatistics>(hToken, TokenStatistics,
    Statistics);

  // Try to process the case of a handle with no TOKEN_QUERY access
  if (Result.Status = STATUS_ACCESS_DENIED) and
    NT_SUCCESS(NtDuplicateObject(NtCurrentProcess, hToken,
    NtCurrentProcess, hTokenRef, TOKEN_QUERY, 0, 0)) then
  begin
    Result := NtxToken.Query<TTokenStatistics>(hToken, TokenStatistics,
      Statistics);

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

  Result := NtxToken.SetInfo<TSidAndAttributes>(hToken, TokenIntegrityLevel,
    MandatoryLabel);
end;

{ Other opeations }

function NtxpCompareTokenIds(hToken1, hToken2: THandle): NTSTATUS;
var
  Statistics1, Statistics2: TTokenStatistics;
begin
  Result := NtxQueryStatisticsToken(hToken1, Statistics1).Status;

  if NT_SUCCESS(Result) then
  begin
    Result := NtxQueryStatisticsToken(hToken2, Statistics2).Status;

    if NT_SUCCESS(Result)  then
    begin
      if Statistics1.TokenId = Statistics2.TokenId then
        Exit(STATUS_SUCCESS)
      else
        Exit(STATUS_NOT_SAME_OBJECT);
    end;
  end;
end;

function NtxAdjustPrivileges(hToken: THandle; Privileges: TArray<TLuid>;
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
  Privileges: TArray<TLuid>;
begin
  SetLength(Privileges, 1);
  Privileges[0] := Privilege;
  Result := NtxAdjustPrivileges(hToken, Privileges, NewAttribute);
end;

function NtxAdjustGroups(hToken: THandle; Sids: TArray<ISid>;
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
