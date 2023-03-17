unit TU.DesktopAccess;

{
  This module contains logic for suggesting the user to adjust access to the
  desktop or window station when creating processes.
}

interface

uses
  Ntapi.ntpsapi, Ntapi.ntseapi, Ntapi.WinUser, NtUtils, DelphiApi.Reflection;

// Check if the logon SID of the token grants access to a desktop and adjust it
// if necessary
procedure TuSuggestDesktopAccess(
  ParentHwnd: THwnd;
  [in] FullDesktopName: String;
  [Access(TOKEN_QUERY)] const hxToken: IHandle
);

// Check if the logon SID of the process's token grants access to a desktop and
// adjust it if necessary
procedure TuSuggestDesktopAccessByProcess(
  ParentHwnd: THwnd;
  const FullDesktopName: String;
  [Access(PROCESS_QUERY_LIMITED_INFORMATION)] hProcess: THandle;
  [Access(THREAD_DIRECT_IMPERSONATION)] hThread: THandle
);

implementation

uses
  Ntapi.WinNt, NtUtils.SysUtils, NtUtils.WinUser, Ntapi.ntpebteb,
  NtUtils.Tokens, NtUtils.Tokens.Info, NtUtils.Tokens.Impersonate,
  NtUtils.Security, NtUtils.Security.Sid, NtUtils.Security.Acl, NtUtils.Lsa.Sid,
  NtUtils.Objects, NtUiLib.TaskDialog, NtUiLib.Exceptions.Dialog;

function IsLogonSidInAcl(
  const Acl: IAcl;
  const LogonSid: ISid
): Boolean;
var
  i: Integer;
  Ace: TAceData;
begin
  for i := 0 to Pred(RtlxSizeAcl(Acl).AceCount) do
    if RtlxGetAce(Acl, i, Ace).IsSuccess and
      RtlxEqualSids(Ace.Sid, LogonSid) then
      Exit(True);

  Result := False;
end;

function ShouldSuggestAddingLogonSid(
  hObject: THandle;
  const LogonSid: ISid
): Boolean;
var
  Dacl: IAcl;
begin
  Result := RtlxQueryDaclObject(hObject, NtxQuerySecurityObject, Dacl).IsSuccess
    and Assigned(Dacl) and not IsLogonSidInAcl(Dacl, LogonSid);
end;

function AddLogonSidToObject(
  hObject: THandle;
  const LogonSid: ISid;
  AccessMask: TAccessMask
): TNtxStatus;
var
  Dacl: IAcl;
  Ace: TAceData;
begin
  // Take existing DACL
  Result := RtlxQueryDaclObject(hObject, NtxQuerySecurityObject, Dacl);

  if not Result.IsSuccess or not Assigned(Dacl) then
    Exit;

  Ace := Default(TAceData);
  Ace.AceType := ACCESS_ALLOWED_ACE_TYPE;
  Ace.Mask := AccessMask;
  Ace.SID := LogonSid;

  // Insert an ACE preserving canonical order
  Result := RtlxAddAce(Dacl, Ace);

  if not Result.IsSuccess then
    Exit;

  // Apply the modified DACL
  Result := RtlxSetDaclObject(hObject, NtxSetSecurityObject, Dacl);
end;

procedure TuSuggestDesktopAccess;
const
  MESSAGE_TEXT = 'The logon SID of the new process doesn''t provide access ' +
    'to the desktop and/or the window station object. Do you want to grant ' +
    'such access to %s? Otherwise, the program might not start correctly. ';
var
  SuggestForWinSta, SuggestForDesktop: Boolean;
  Status: TNtxStatus;
  SessionId: TSessionId;
  User, LogonSid: TGroup;
  hxDesktop, hxWinSta: IHandle;
begin
  // Fallback to desktop inheritance
  if FullDesktopName = '' then
    FullDesktopName := RtlGetCurrentPeb.ProcessParameters.DesktopInfo.ToString;

  // Skip non-interactive window stations
  if not RtlxEqualStrings(RtlxExtractRootPath(FullDesktopName), 'WinSta0') then
    Exit;

  // Skip cross-session process spawning
  Status := NtxToken.Query(hxToken, TokenSessionId, SessionId);

  if not Status.IsSuccess or (SessionId <> RtlGetCurrentPeb.SessionID) then
    Exit;

  // Check the user to exclude SYSTEM since it gets full access anyway
  Status := NtxQueryGroupToken(hxToken, TokenUser, User);

  if not Status.IsSuccess then
    Exit;

  if RtlxEqualSids(User.Sid, RtlxMakeSid(
    SECURITY_NT_AUTHORITY, [SECURITY_LOCAL_SYSTEM_RID])) and
    not BitTest(User.Attributes and SE_GROUP_USE_FOR_DENY_ONLY) then
    Exit;

  // Lookup the logon SID of the token (if there is one)
  Status := NtxQueryLogonSidToken(hxToken, LogonSid);

  if not Status.IsSuccess then
    Exit;

  // Check the DACLs for the presence of this logon SID
  SuggestForWinSta := UsrxOpenWindowStation(hxWinSta, 'WinSta0',
    READ_CONTROL).IsSuccess and ShouldSuggestAddingLogonSid(hxWinSta.Handle,
    LogonSid.Sid);

  SuggestForDesktop := UsrxOpenDesktop(hxDesktop,
    RtlxExtractNamePath(FullDesktopName), READ_CONTROL).IsSuccess and
    ShouldSuggestAddingLogonSid(hxDesktop.Handle, LogonSid.Sid);

  if not SuggestForWinSta and not SuggestForDesktop then
    Exit;

  // Ask the user for confirmation
  if UsrxShowTaskDialog(ParentHwnd, 'Token Universe',
    'Allow access to the desktop?', RtlxFormatString(MESSAGE_TEXT,
    [LsaxSidToString(LogonSid.Sid)]), diInfo, dbYesIgnore) <> IDYES then
    Exit;

  if SuggestForWinSta then
  begin
    // Insert the logon SID into the window station's DACL
    Status := UsrxOpenWindowStation(hxWinSta, 'WinSta0', READ_CONTROL or
      WRITE_DAC);

    if Status.IsSuccess then
      Status := AddLogonSidToObject(hxWinSta.Handle, LogonSid.Sid,
        WINSTA_ALL_ACCESS);

    if not Status.IsSuccess then
      ShowNtxStatus(ParentHwnd, Status);
  end;

  if SuggestForDesktop then
  begin
    // Insert the logon SID into the desktop's DACL
    Status := UsrxOpenDesktop(hxDesktop,
      RtlxExtractNamePath(FullDesktopName), READ_CONTROL or WRITE_DAC);

    if Status.IsSuccess then
      Status := AddLogonSidToObject(hxDesktop.Handle, LogonSid.Sid,
        DESKTOP_ALL_ACCESS);

    if not Status.IsSuccess then
      ShowNtxStatus(ParentHwnd, Status);
  end;
end;

procedure TuSuggestDesktopAccessByProcess;
var
  Status: TNtxStatus;
  hxToken: IHAndle;
begin
  // Try to open the token
  Status := NtxOpenProcessToken(hxToken, hProcess, TOKEN_QUERY);

  // If it didn't work, try reading its copy via direct impersonation
  if not Status.IsSuccess then
    Status := NtxCopyEffectiveToken(hxToken, hThread, SecurityIdentification,
      TOKEN_QUERY);

  if Status.IsSuccess then
    TuSuggestDesktopAccess(ParentHwnd, FullDesktopName, hxToken);
end;

end.
