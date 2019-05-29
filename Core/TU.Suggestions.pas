unit TU.Suggestions;

interface

uses
  System.SysUtils, Winapi.Windows;

procedure ShowErrorSuggestions(ParentWnd: HWND; E: Exception);

const
  USE_TYPE_MISMATCH = 'Wrong token type';
  USE_NEED_PRIMARY = 'This action requires a primary token while you ' +
   'are trying to use an impersonation one. Do you want to duplicate it first?';
  USE_NEED_IMPERSONATION = 'This is not an impersonation token (which ' +
   'includes everything from Anonymous up to Delegation). ' +
   'Do you want to duplicate it first?';

  NO_SANBOX_INERT = 'The resulting token doesn''t contain SandboxInert flag ' +
    'despite you tried to enable it. Looks like this action requires ' +
    'SeTcbPrivilege on your system.';

implementation

uses
  TU.Tokens, TU.Tokens.Types, Winapi.WinError, Winapi.CommCtrl,
  Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntseapi,
  NtUtils.Exceptions, NtUtils.ErrorMsg;

resourcestring
  TITLE_OS_ERROR = 'System error';
  TITLE_CONVERT = 'Conversion error';
  TITLE_BUG = 'This is definitely a bug...';
  ERR_FMT = 'Last call: %s' + #$D#$A +
   'Result: %s' + #$D#$A#$D#$A + '%s';

  BUGTRACKER = #$D#$A#$D#$A'If you known how to reproduce this error please ' +
    'help the project by opening an issue on our GitHub page'#$D#$A +
    'https://github.com/diversenok/TokenUniverse';

  SUGGEST_TEMPLATE = #$D#$A#$D#$A'--- Suggestions ---'#$D#$A'%s';

  NEW_DUPLICATE_IMP = 'You can''t duplicate a token from a lower impersonation ' +
    'level to a higher one. Although, you can create a primary token from ' +
    'Impersonation and Delegation ones. If you are working with linked ' +
    'tokens you can obtain a primary linked token if you have ' +
    '`SeTcbPrivilege` privilege.';
  NEW_RESTCICT_ACCESS = 'The hande must grant `Duplicate` access to create ' +
    'resticted tokens.';
  NEW_NT_CREATE = 'Creation of a token from the scratch requires `SeCreateTokenPrivilege`';
  NEW_SAFER_IMP = 'Since Safer API always returns primary tokens the rules ' +
   'are the same as while performing duplication. This means only ' +
   'Impersonation, Delegation, and Primary tokens are suitable.';

  GETTER_QUERY = 'You need `Query` access right to obtain this information ' +
    'from the token';
  GETTER_QUERY_SOURCE = 'You need `Query Source` access right to obtain ' +
    'this information from the token';

  SETTER_DEFAULT = '`Adjust default` access right is required to change this ' +
  'information class for the token';
  SETTER_SESSION = 'To change the session of a token you need to ' +
    'have `SeTcbPrivilege` and also `Adjust SessionId` and `Adjust default` '+
    'access rights for the token.';
  SETTER_OWNER = 'Only those groups from the token that are marked with ' +
    '`Owner` flag and the user itself can be set as an owner.';
  SETTER_PRIMARY = 'The SID must present in the group list of the token to be ' +
    'suitable as a primary group.';
  SETTER_INTEGRITY_TCB = 'To raise the integrity level of a token you need to ' +
    'have `SeTcbPrivilege`.';
  SETTER_UIACCESS_TCB = 'You need `SeTcbPrivilege` to enable UIAccess flag.';
  SETTER_POLICY_TCB = 'Changing of mandatory integrity policy requires ' +
    '`SeTcbPrivilege`.';
  SETTER_ORIGIN_TCB = 'Altering token origin requires `SeTcbPrivilege`. Also ' +
    'note, that once set, the value can not be changed.';
  SETTOR_VIRT_PRIV = 'You must possess `SeCreateTokenPrivilege` to allow / ' +
    'disallow virtualization for tokens.';
  SETTER_PRIVILEGES_ACCESS = 'You need to have `Adjust privileges` access ' +
    'right for the token.';
  SETTER_PRIVILEGES_OTHER = 'You can''t enable some privileges if the ' +
   'integrity level of the token is too low.';
  SETTER_GROUPS_ACCESS = 'This action requires `Adjust groups` access right.';
  SETTER_GROUPS_MODIFY = 'You can''t disable `Mandatory` groups ' +
    'just like you can''t enable `Use for deny only` groups.';
  SETTER_AUDIT_PRIV = 'Changing auditing policy requires `SeTcbPrivilege`.';
  SETTER_AUDIT_ONCE = 'This is not an informative error, but the problem ' +
    'might be caused by an already set auditing policy for the token.';

  ACTION_ASSIGN_NOT_SUPPORTED = 'A token can be assigned only on an early ' +
    'stage of a process lifetime. Try this action on a newly created ' +
    'suspended process.';
  ACTION_ASSIGN_PRIVILEGE = '`SeAssignPrimaryTokenPrivilege` is required to ' +
    'assign tokens that are not derived from your current token. ';
  ACTION_ASSIGN_TYPE = 'Only a primary token can be assigned to a process';

  ACTION_SAFE_IMP = 'Not all security contexts can be impersonated if the ' +
    'target process does not possess SeImpersonatePrivilege. In this case ' +
    'NtSetInformationThread succeeds, but the target thread gets an ' +
    'Identification-level token that is not suitable for any access checks. ' +
    'The "Use safe impersonation technique" setting prevents such situations ' +
    'and shows this message instead.';

function SuggestConstructor(E: ENtError): String;
begin
  if E.Matches('NtDuplicateToken', STATUS_BAD_IMPERSONATION_LEVEL) then
    Exit(NEW_DUPLICATE_IMP);

  if E.Matches('NtFilterToken', STATUS_ACCESS_DENIED)  then
    Exit(NEW_RESTCICT_ACCESS);

  if E.Matches('NtCreateToken', STATUS_PRIVILEGE_NOT_HELD) then
    Exit(NEW_NT_CREATE);

  if E.Matches('SaferComputeTokenFromLevel', ERROR_BAD_IMPERSONATION_LEVEL) then
    Exit(NEW_SAFER_IMP);
end;

function SuggestGetter(E: ENtError): String;
begin
  if E.ErrorCode = ERROR_ACCESS_DENIED then
  begin
    if E.ErrorLocation = GetterMessage(TokenSource) then
      Exit(GETTER_QUERY_SOURCE);

    if E.ErrorLocation.StartsWith('GetTokenInformation:') then
      Exit(GETTER_QUERY);
  end;
end;

function SuggestSetter(E: ENtError): String;
begin
  if E.ErrorCode = ERROR_ACCESS_DENIED then
  begin
    if E.ErrorLocation = SetterMessage(TokenSessionId) then
      Exit(SETTER_SESSION);

    if (E.ErrorLocation = SetterMessage(TokenIntegrityLevel))
      or (E.ErrorLocation = SetterMessage(TokenUIAccess))
      or (E.ErrorLocation = SetterMessage(TokenMandatoryPolicy))
      or (E.ErrorLocation = SetterMessage(TokenOwner))
      or (E.ErrorLocation = SetterMessage(TokenPrimaryGroup))
      or (E.ErrorLocation = SetterMessage(TokenOrigin))
      or (E.ErrorLocation = SetterMessage(TokenVirtualizationAllowed))
      or (E.ErrorLocation = SetterMessage(TokenVirtualizationEnabled)) then
      Exit(SETTER_DEFAULT);
  end;

  if E.Matches(SetterMessage(TokenOwner), ERROR_INVALID_OWNER) then
    Exit(SETTER_OWNER);

  if E.Matches(SetterMessage(TokenPrimaryGroup), ERROR_INVALID_PRIMARY_GROUP) then
    Exit(SETTER_PRIMARY);

  if E.Matches(SetterMessage(TokenAuditPolicy), ERROR_INVALID_PARAMETER) then
    Exit(SETTER_AUDIT_ONCE);

  if E.ErrorCode = ERROR_PRIVILEGE_NOT_HELD then
  begin
    if E.ErrorLocation = SetterMessage(TokenSessionId) then
      Exit(SETTER_SESSION);

    if E.ErrorLocation = SetterMessage(TokenIntegrityLevel) then
      Exit(SETTER_INTEGRITY_TCB);

    if E.ErrorLocation = SetterMessage(TokenUIAccess) then
      Exit(SETTER_UIACCESS_TCB);

    if E.ErrorLocation = SetterMessage(TokenMandatoryPolicy) then
      Exit(SETTER_POLICY_TCB);

    if E.ErrorLocation = SetterMessage(TokenVirtualizationAllowed) then
      Exit(SETTOR_VIRT_PRIV);

    if E.ErrorLocation = SetterMessage(TokenOrigin) then
      Exit(SETTER_ORIGIN_TCB);

    if E.ErrorLocation = SetterMessage(TokenAuditPolicy) then
      Exit(SETTER_AUDIT_PRIV);
  end;

  if E.ErrorLocation = 'NtAdjustPrivilegesToken' then
  begin
    if E.ErrorCode = STATUS_ACCESS_DENIED then
      Exit(SETTER_PRIVILEGES_ACCESS);

    if E.ErrorCode = STATUS_NOT_ALL_ASSIGNED then
      Exit(SETTER_PRIVILEGES_OTHER);
  end;

  if E.ErrorLocation = 'NtAdjustGroupsToken' then
  begin
    if E.ErrorCode = STATUS_ACCESS_DENIED then
      Exit(SETTER_GROUPS_ACCESS);

    if E.ErrorCode = STATUS_CANT_ENABLE_DENY_ONLY then
      Exit(SETTER_GROUPS_MODIFY);

    if E.ErrorCode = STATUS_CANT_DISABLE_MANDATORY then
      Exit(SETTER_GROUPS_MODIFY);
  end;

  if E.ErrorLocation = 'NtSetInformationProcess#ProcessAccessToken' then
  begin
   if E.ErrorCode = STATUS_NOT_SUPPORTED then
     Exit(ACTION_ASSIGN_NOT_SUPPORTED);

   if E.ErrorCode = STATUS_PRIVILEGE_NOT_HELD then
     Exit(ACTION_ASSIGN_PRIVILEGE);

   if E.ErrorCode = STATUS_BAD_IMPERSONATION_LEVEL then
     Exit(ACTION_ASSIGN_TYPE);
  end;

  if E.Matches('NtxSafeSetThreadToken', STATUS_PRIVILEGE_NOT_HELD) then
    Exit(ACTION_SAFE_IMP);
end;

function SuggestAll(E: ENtError): String;
begin
  Result := SuggestGetter(ENtError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

  Result := SuggestSetter(ENtError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

  Result := SuggestConstructor(ENtError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

end;

procedure ShowErrorSuggestions(ParentWnd: HWND; E: Exception);
var
  Dlg: TASKDIALOGCONFIG;
begin
  FillChar(Dlg, SizeOf(Dlg), 0);
  Dlg.cbSize := SizeOf(Dlg);
  Dlg.pszMainIcon := TD_ERROR_ICON;
  Dlg.dwFlags := TDF_ALLOW_DIALOG_CANCELLATION;
  Dlg.hwndParent := ParentWnd;
  Dlg.pszWindowTitle := 'Error';

  if (E is EAccessViolation) or (E is EInvalidPointer) or
    (E is EAssertionFailed) then
  begin
    Dlg.pszMainInstruction := PWideChar(TITLE_BUG);
    Dlg.pszContent := PWideChar(E.Message + BUGTRACKER);
  end
  else if E is EConvertError then
  begin
    Dlg.pszMainInstruction := PWideChar(TITLE_CONVERT);
    Dlg.pszContent := PWideChar(E.Message);
  end
  else if E is ENtError then
  begin
    if not NT_ERROR(ENtError(E).ErrorCode) then
      Dlg.pszMainIcon := TD_WARNING_ICON;

    Dlg.pszMainInstruction := PWideChar(
      StatusToDescription(ENtError(E).ErrorCode));

    if Dlg.pszMainInstruction = '' then
      Dlg.pszMainInstruction := PWideChar(TITLE_OS_ERROR);

    Dlg.pszContent := PWideChar(Format(ERR_FMT, [ENtError(E).ErrorLocation,
      StatusToString(ENtError(E).ErrorCode),
      SysNativeErrorMessage(ENtError(E).ErrorCode)]) + SuggestAll(ENtError(E)));
  end
  else
  begin
    Dlg.pszMainInstruction := PWideChar(E.ClassName);
    Dlg.pszContent := PWideChar(E.Message);
  end;

  if not Succeeded(TaskDialogIndirect(Dlg, nil, nil, nil)) then
    MessageBoxW(Dlg.hwndParent, Dlg.pszContent, Dlg.pszWindowTitle,
      MB_OK or MB_ICONERROR);
end;

end.
