unit NtUtils.Tokens.Impersonate;

interface

uses
  NtUtils.Exceptions;

// Save current impersonation token before operations that can alter it
function NtxBackupImpersonation(hThread: THandle): THandle;
procedure NtxRestoreImpersonation(hThread: THandle; hToken: THandle);

// Set thread token
function NtxSetThreadToken(hThread: THandle; hToken: THandle): TNtxStatus;
function NtxSetThreadTokenById(TID: NativeUInt; hToken: THandle): TNtxStatus;

// Set thread token and make sure it was not duplicated to Identification level
function NtxSafeSetThreadToken(hThread: THandle; hToken: THandle): TNtxStatus;
function NtxSafeSetThreadTokenById(TID: NativeUInt; hToken: THandle): TNtxStatus;

// Impersonate the token of any type on the current thread
function NtxImpersonateAnyToken(hToken: THandle): TNtxStatus;

// Assign primary token to a process
function NtxAssignPrimaryToken(hProcess: THandle; hToken: THandle): TNtxStatus;
function NtxAssignPrimaryTokenById(PID: NativeUInt; hToken: THandle): TNtxStatus;

implementation

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntpsapi, Ntapi.ntseapi,
  NtUtils.Objects, NtUtils.Tokens, NtUtils.Ldr, NtUtils.Processes;

{ Impersonation }

function NtxBackupImpersonation(hThread: THandle): THandle;
var
  Status: NTSTATUS;
begin
  // Open the thread's token
  Status := NtxOpenThreadToken(Result, hThread, TOKEN_IMPERSONATE).Status;

  if Status = STATUS_NO_TOKEN then
    Result := 0
  else if not NT_SUCCESS(Status) then
  begin
    // Most likely the token is here, but we can't access it. Although we can
    // make a copy via direct impersonation, I am not sure we should do it.
    // Currently, just clear the token as most of Winapi functions do in this
    // situation
    Result := 0;

    if hThread = NtCurrentThread then
      ENtError.Report(Status, 'NtxBackupImpersonation');
  end;
end;

procedure NtxRestoreImpersonation(hThread: THandle; hToken: THandle);
var
  Status: NTSTATUS;
begin
  // Try to establish the previous token
  Status := NtxSetThreadToken(hThread, hToken).Status;

  if not NT_SUCCESS(Status) then
  begin
    // On failure clear the token (if it's still here)
    if hToken <> 0 then
      NtxSetThreadToken(hThread, 0);

    if hThread = NtCurrentThread then
      ENtError.Report(Status, 'NtxRestoreImpersonation');
  end;
end;

function NtxSetThreadToken(hThread: THandle; hToken: THandle): TNtxStatus;
begin
  Result.Location := 'NtSetInformationThread';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ThreadImpersonationToken);
  Result.LastCall.InfoClassType := TypeInfo(TThreadInfoClass);

  Result.Status := NtSetInformationThread(hThread, ThreadImpersonationToken,
    @hToken, SizeOf(hToken));

  // TODO: what about inconsistency with NtCurrentTeb.IsImpersonating ?
end;

function NtxSetThreadTokenById(TID: NativeUInt; hToken: THandle): TNtxStatus;
var
  hThread: THandle;
begin
  Result := NtxOpenThread(hThread, TID, THREAD_SET_THREAD_TOKEN);

  if not Result.IsSuccess then
    Exit;

  Result := NtxSetThreadToken(hThread, hToken);
  NtxSafeClose(hThread);
end;

{ Some notes about safe impersonation...

   In case of absence of SeImpersonatePrivilege some security contexts
   might cause the system to duplicate the token to Identification level
   which fails all access checks. The result of NtSetInformationThread
   does not provide information whether it happened.
   The goal is to detect and avoid such situations.

   NtxSafeSetThreadToken sets the token, queries it back, and compares these
   two. Anything but success causes the routine to undo the work.

   NOTE: The secutity context of the target thread is not guaranteed to return
   to its previous state. It might happen if the target thread is impersonating
   a token that the caller can't open. In this case after the failed call the
   target thread will have no token.

   To address this issue the caller can make a copy of the target thread's
   security context by using NtImpersonateThread. See implementation of
   NtxOpenEffectiveToken for more details.

Other possible implementations:

 * NtImpersonateThread fails with BAD_IMPERSONATION_LEVEL when we request
   Impersonation-level token while the thread's token is Identification or less.

}

function NtxSafeSetThreadToken(hThread: THandle; hToken: THandle): TNtxStatus;
var
  hOldStateToken, hActuallySetToken: THandle;
begin
  // No need to use safe impersonation to revoke tokens
  if hToken = 0 then
    Exit(NtxSetThreadToken(hThread, hToken));

  // Backup old state
  hOldStateToken := NtxBackupImpersonation(hThread);

  // Set the token
  Result := NtxSetThreadToken(hThread, hToken);

  if not Result.IsSuccess then
  begin
    if hOldStateToken <> 0 then
      NtxSafeClose(hOldStateToken);

    Exit;
  end;

  // Read it back for comparison. Any access works for us.
  Result := NtxOpenThreadToken(hActuallySetToken, hThread, MAXIMUM_ALLOWED);

  if not Result.IsSuccess then
  begin
    // Reset and exit
    NtxRestoreImpersonation(hThread, hOldStateToken);

    if hOldStateToken <> 0 then
      NtxSafeClose(hOldStateToken);

    Exit;
  end;

  // Revert the current thread (if it's the target) to perform comparison
  if hThread = NtCurrentThread then
    NtxRestoreImpersonation(hThread, hOldStateToken);

  // Compare the one we were trying to set with the one actually set
  Result.Location := 'NtxCompareObjects';
  Result.Status := NtxCompareObjects(hToken, hActuallySetToken, 'Token');
  NtxSafeClose(hActuallySetToken);

  // STATUS_SUCCESS => Impersonation works fine, use it.
  // STATUS_NOT_SAME_OBJECT => Duplication happened, reset and exit
  // Oher errors => Reset and exit

  // SeImpersonatePrivilege on the target process can help
  if Result.Status = STATUS_NOT_SAME_OBJECT then
  begin
    Result.Location := 'NtxSafeSetThreadToken';
    Result.Status := STATUS_PRIVILEGE_NOT_HELD;
  end;

  if Result.Status = STATUS_SUCCESS then
  begin
    // Repeat in case of the current thread (we reverted it for comparison)
    if hThread = NtCurrentThread then
      Result := NtxSetThreadToken(hThread, hToken);
  end
  else
  begin
    // Failed, reset the security context if we haven't done it yet
    if hThread <> NtCurrentThread then
      NtxRestoreImpersonation(hThread, hOldStateToken);
  end;

  if hOldStateToken <> 0 then
    NtxSafeClose(hOldStateToken);
end;

function NtxSafeSetThreadTokenById(TID: NativeUInt; hToken: THandle):
  TNtxStatus;
var
  hThread: THandle;
begin
  Result := NtxOpenThread(hThread, TID, THREAD_QUERY_LIMITED_INFORMATION or
    THREAD_SET_THREAD_TOKEN);

  if not Result.IsSuccess then
    Exit;

  Result := NtxSafeSetThreadToken(hThread, hToken);
  NtxSafeClose(hThread);
end;

function NtxImpersonateAnyToken(hToken: THandle): TNtxStatus;
var
  hImpToken: THandle;
begin
  // Try to impersonate (in case it is an impersonation-type token)
  Result := NtxSetThreadToken(NtCurrentThread, hToken);

  if Result.Status = STATUS_BAD_TOKEN_TYPE then
  begin
    // Nope, it is a primary token, duplicate it
    Result := NtxDuplicateToken(hImpToken, hToken, TOKEN_IMPERSONATE,
      TokenImpersonation, SecurityImpersonation);

    if Result.IsSuccess then
    begin
      // Impersonate, second attempt
      Result := NtxSetThreadToken(NtCurrentThread, hImpToken);

      NtxSafeClose(hImpToken);
    end;
  end;
end;

function NtxAssignPrimaryToken(hProcess: THandle;
  hToken: THandle): TNtxStatus;
var
  AccessToken: TProcessAccessToken;
begin
  // Check delayed import for ReactOS
  Result := LdrxCheckNtDelayedImport('NtGetNextThread');

  if not Result.IsSuccess then
    Exit;

  // Despite the process handle, we need a handle to the initial thread
  Result.Location := 'NtGetNextThread';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := THREAD_QUERY_LIMITED_INFORMATION;
  Result.LastCall.AccessMaskType := TAccessMaskType.objNtThread;

  Result.Status := NtGetNextThread(hProcess, 0,
    THREAD_QUERY_LIMITED_INFORMATION, 0, 0, AccessToken.Thread);

  if not Result.IsSuccess then
    Exit;

  // Prepare the token's handle. The thread's handle is already in here.
  AccessToken.Token := hToken;

  // Assign the token for the process
  Result.Location := 'NtSetInformationProcess';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ProcessAccessToken);
  Result.LastCall.InfoClassType := TypeInfo(TProcessInfoClass);

  Result.Status := NtSetInformationProcess(hProcess, ProcessAccessToken,
    @AccessToken, SizeOf(AccessToken));

  NtxSafeClose(AccessToken.Thread);
end;

function NtxAssignPrimaryTokenById(PID: NativeUInt;
  hToken: THandle): TNtxStatus;
var
  hProcess: THandle;
begin
  Result := NtxOpenProcess(hProcess, PID, PROCESS_QUERY_INFORMATION or
    PROCESS_SET_INFORMATION);

  if not Result.IsSuccess then
    Exit;

  Result := NtxAssignPrimaryToken(hProcess, hToken);
  NtxSafeClose(hProcess);
end;

end.
