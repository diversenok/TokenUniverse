unit NtUtils.ApiExtension;

interface
{$WARN SYMBOL_PLATFORM OFF}

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntobapi, Ntapi.ntseapi, NtUtils.Exceptions;

{ -------------------------------- Tokens ---------------------------------- }

// NtQueryInformationToken for variable-sized buffers without race conditions
function NtxQueryBufferToken(hToken: THandle; InfoClass: TTokenInformationClass;
  out Status: NTSTATUS; ReturnedSize: PCardinal = nil): Pointer;

// NtCompareObjects for comparing tokens on all versions of Windows
function NtxCompareTokens(hToken1, hToken2: THandle): NTSTATUS;

// NtDuplicateToken
function NtxDuplicateToken(hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType;
  ImpersonationLevel: TSecurityImpersonationLevel; EffectiveOnly: LongBool;
  out hNewToken: THandle): NTSTATUS;

// NtSetInformationThread that doesn't duplicate tokens to Identification level
function NtxSafeSetThreadToken(hThread: THandle; hToken: THandle): NTSTATUS;

// NtSetInformationThread + NtOpenThreadTokenEx
function NtxImpersonateToken(hToken: THandle; out hOldToken: THandle):
  TNtxStatus;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntpsapi, Ntapi.ntrtl, NtUtils.Objects,
  NtUtils.Snapshots.Handles, NtUtils.DelayedImport, System.SysUtils;

{ Tokens }

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
    if not NtxTryCheckBuffer(Status, RequiredSize) then
      Exit(nil);

    // Free previous buffer and allocate a new one
    FreeMem(Result);

    BufferSize := RequiredSize;
    Result := AllocMem(BufferSize);
  end;
end;

function NtxpQueryStatisticsToken(hToken: THandle;
  out Statistics: TTokenStatistics): NTSTATUS;
var
  Returned: Cardinal;
  hTemp: THandle;
begin
  Result := NtQueryInformationToken(hToken, TokenStatistics, @Statistics,
    SizeOf(Statistics), Returned);

  // Process the case of a handle with no QUERY access
  if Result = STATUS_ACCESS_DENIED then
  begin
    Result := NtDuplicateObject(NtCurrentProcess, hToken, NtCurrentProcess,
      hTemp, TOKEN_QUERY, 0, 0);

    if NT_SUCCESS(Result) then
    begin
      Result := NtQueryInformationToken(hTemp, TokenStatistics, @Statistics,
        SizeOf(Statistics), Returned);

      NtxSafeClose(hTemp);
    end;
  end;
end;

function NtxCompareTokens(hToken1, hToken2: THandle): NTSTATUS;
var
  Statistics1, Statistics2: TTokenStatistics;
begin
  if hToken1 = hToken2 then
    Exit(STATUS_SUCCESS);

  // Win 10 TH+ makes things way easier
  if NtxCheckNtDelayedImport('NtCompareObjects').IsSuccess then
    Exit(NtCompareObjects(hToken1, hToken2));

  // Try to perform a comparison based on TokenIDs. NtxpQueryStatisticsToken
  // might be capable of handling it even without TOKEN_QUERY access.

  Result := NtxpQueryStatisticsToken(hToken1, Statistics1);
  if NT_SUCCESS(Result) then
  begin
    Result := NtxpQueryStatisticsToken(hToken2, Statistics2);

    if NT_SUCCESS(Result) then
    begin
      if Statistics1.TokenId = Statistics2.TokenId then
        Exit(STATUS_SUCCESS)
      else
        Exit(STATUS_NOT_SAME_OBJECT);
    end;
  end;

  if Result <> STATUS_ACCESS_DENIED then
    Exit;

  // The only way to proceed is via a handle snaphot
  Result := THandleSnapshot.Compare(hToken1, hToken2);
end;

function NtxDuplicateToken(hExistingToken: THandle;
  DesiredAccess: TAccessMask; TokenType: TTokenType;
  ImpersonationLevel: TSecurityImpersonationLevel; EffectiveOnly: LongBool;
  out hNewToken: THandle): NTSTATUS;
var
  ObjAttr: TObjectAttributes;
  QoS: TSecurityQualityOfService;
begin
  InitializaQoS(QoS, ImpersonationLevel, EffectiveOnly);
  InitializeObjectAttributes(ObjAttr, nil, 0, 0, @QoS);

  Result := NtDuplicateToken(hExistingToken, DesiredAccess, @ObjAttr,
    EffectiveOnly, TokenType, hNewToken);
end;

{ Some notes about impersonation...

 * In case of absence of SeImpersonatePrivilege some security contexts
   might cause the system to duplicate the token to Identification level
   which fails all access checks. The result of NtSetInformationThread
   does not provide information whether it happened.
   The goal is to detect and avoid such situations.

 * NtxSafeSetThreadToken sets the token, queries it back, and compares them.
   Anything but success causes the routine to revoke the token.

 * Although it tries to, the function does not guarantee the secutity
   context of the target thread to return to the state before the call.
   It is potentially possible to user NtImpersonateThread to retrive a copy
   of the original security context if NtOpenThreadTokenEx fails with
   ACCESS_DENIED.

 * Remark: NtImpersonateThread fails with BAD_IMPERSONATION_LEVEL when we
   request Impersonation-level token while the thread's token is Identification
   and less. This in another way to implement the check.
}

function NtxSafeSetThreadToken(hThread: THandle; hToken: THandle): NTSTATUS;
var
  hOldStateToken, hNewToken: THandle;
begin
  // Backup old state
  if not NT_SUCCESS(NtOpenThreadTokenEx(hThread, TOKEN_IMPERSONATE, False, 0,
    hOldStateToken)) then
    hOldStateToken := 0;

  // Set our token
  Result := NtSetInformationThread(hThread, ThreadImpersonationToken, @hToken,
    SizeOf(hToken));

  if not NT_SUCCESS(Result) then
    Exit;

  // Query what was actually set
  Result := NtOpenThreadTokenEx(hThread, MAXIMUM_ALLOWED,
    (hThread = NtCurrentThread), 0, hNewToken);

  if not NT_SUCCESS(Result) then
  begin
    // Reset and exit
    NtSetInformationThread(hThread, ThreadImpersonationToken, @hOldStateToken,
      SizeOf(hOldStateToken));
    Exit;
  end;

  if hThread = NtCurrentThread then
  begin
    // Revert to self to perform comparison
    NtSetInformationThread(hThread, ThreadImpersonationToken, @hOldStateToken,
      SizeOf(hOldStateToken));
  end;

  // Compare
  Result := NtxCompareTokens(hToken, hNewToken);
  NtxSafeClose(hNewToken);

  // STATUS_SUCCESS => Impersonation works fine, use it.
  // STATUS_NOT_SAME_OBJECT => Duplication happened, reset and exit
  // Oher errors => Reset and exit

  // SeImpersonatePrivilege can help
  if Result = STATUS_NOT_SAME_OBJECT then
    Result := STATUS_PRIVILEGE_NOT_HELD;

  if Result = STATUS_SUCCESS then
  begin
    // Repeat in case of current thread
    if hThread = NtCurrentThread then
      Result := NtSetInformationThread(hThread, ThreadImpersonationToken,
        @hToken, SizeOf(hToken));
  end
  else
  begin
    // Reset impersonation
    NtSetInformationThread(hThread, ThreadImpersonationToken, @hOldStateToken,
      SizeOf(hOldStateToken));
  end;

  if hOldStateToken <> 0 then
    NtxSafeClose(hOldStateToken);
end;

function NtxImpersonateToken(hToken: THandle; out hOldToken: THandle):
  TNtxStatus;
var
  hTokenDuplicate: THandle;
begin
  Result.Status := STATUS_SUCCESS;

  // Save old token
  if not NT_SUCCESS(NtOpenThreadTokenEx(NtCurrentThread, TOKEN_IMPERSONATE,
    True, 0, hOldToken)) then
    hOldToken := 0;

  // Impersonate
  Result.Location := 'NtSetInformationThread';
  Result.Status := NtSetInformationThread(NtCurrentThread,
    ThreadImpersonationToken, @hToken, SizeOf(hToken));

  if Result.Status = STATUS_BAD_TOKEN_TYPE then
  begin
    // This was a primary token, duplicate it
    Result.Location := 'NtDuplicateToken';
    Result.Status := NtxDuplicateToken(hToken, TOKEN_IMPERSONATE,
      TokenImpersonation, SecurityImpersonation, False, hTokenDuplicate);

    if not NT_SUCCESS(Result.Status) then
      Exit;

    // Impersonate, second attempt
    Result.Location := 'NtSetInformationThread';
    Result.Status := NtSetInformationThread(NtCurrentThread,
      ThreadImpersonationToken, @hTokenDuplicate, SizeOf(hTokenDuplicate));

    NtClose(hTokenDuplicate);
  end;
end;

end.
