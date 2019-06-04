unit NtUtils.Tokens;

interface

uses
  Winapi.WinNt, Ntapi.ntseapi, Winapi.WinSafer, NtUtils.Exceptions;

{ Creation }

// Duplicate existing token
function NtxDuplicateToken(out hToken: THandle; hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType; ImpersonationLevel:
  TSecurityImpersonationLevel; EffectiveOnly: LongBool): TNtxStatus;

// Open a token of a thread
function NtxOpenThreadToken(out hToken: THandle; hThread: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;

// Open an effective security context of a thread
function NtxOpenEffectiveToken(out hToken: THandle; hThread: THandle;
  ImpersonationLevel: TSecurityImpersonationLevel; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; EffectiveOnly: Boolean): TNtxStatus;

// Open anonymous token
function NtxOpenAnonymousToken(out hToken: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal): TNtxStatus;

// Restrict a token using Safer api
function NtxRestrictSaferToken(out hToken: THandle; hTokenToRestict: THandle;
  ScopeId: TSaferScopeId; LevelId: TSaferLevelId; MakeSanboxInert: Boolean):
  TNtxStatus;

{ Basic operations }

// Query variable token information without race conditions
function NtxQueryBufferToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Status: TNtxStatus; ReturnedSize: PCardinal = nil): Pointer;

// Query token statistic (requires either Query or Duplicate access)
function NtxQueryStatisticsToken(hToken: THandle;
  out Statistics: TTokenStatistics): TNtxStatus;

// Check whether two token handles reference the same kernel object
function NtxCompareTokens(hToken1, hToken2: THandle): TNtxStatus;

{ Error formatting }

// Format error locations for querying/setting token information
function NtxFormatTokenQuery(InfoClass: TTokenInformationClass): String;
function NtxFormatTokenSet(InfoClass: TTokenInformationClass): String;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntobapi, Ntapi.ntpsapi, NtUtils.Objects,
  NtUtils.DelayedImport, NtUtils.Snapshots.Handles, System.TypInfo,
  NtUtils.Tokens.Impersonate;

{ Creation }

function NtxDuplicateToken(out hToken: THandle; hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType; ImpersonationLevel:
  TSecurityImpersonationLevel; EffectiveOnly: LongBool): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  QoS: TSecurityQualityOfService;
begin
  InitializaQoS(QoS, ImpersonationLevel, EffectiveOnly);
  InitializeObjectAttributes(ObjAttr, nil, 0, 0, @QoS);

  Result.Location := 'NtDuplicateToken';
  Result.Status := NtDuplicateToken(hExistingToken, DesiredAccess, @ObjAttr,
    EffectiveOnly, TokenType, hToken);
end;

function NtxOpenThreadToken(out hToken: THandle; hThread: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
begin
  // When opening other threads use effective (thread) security context. When
  // reading a token from the current thread use the process security context

  Result.Location := 'NtOpenThreadTokenEx';
  Result.Status := NtOpenThreadTokenEx(hThread, DesiredAccess,
    (hThread = NtCurrentThread), HandleAttributes, hToken);
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

  // Read it back from our thread
  Result := NtxOpenThreadToken(hToken, NtCurrentThread, DesiredAccess,
    HandleAttributes);

  // Restor our previous impersonation
  NtxRestoreImpersonation(NtCurrentThread, hOldToken);

  if hOldToken <> 0  then
    NtxSafeClose(hOldToken);
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

{ Basic operations }

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

{ Error formatting }

function NtxFormatTokenQuery(InfoClass: TTokenInformationClass): String;
begin
  // Use the name of the info class from the enumeration definition
  Result := 'NtQueryInformationToken [' +
    GetEnumName(TypeInfo(TTokenInformationClass), Integer(InfoClass)) + ']';
end;

function NtxFormatTokenSet(InfoClass: TTokenInformationClass): String;
begin
  // Use the name of the info class from the enumeration definition
  Result := 'NtSetInformationToken [' +
    GetEnumName(TypeInfo(TTokenInformationClass), Integer(InfoClass)) + ']';
end;

end.
