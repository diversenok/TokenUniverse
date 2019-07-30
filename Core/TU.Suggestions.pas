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
    'Tcb privilege on your system.';

implementation

uses
  Ntapi.ntseapi, Winapi.WinError, Winapi.CommCtrl, Ntapi.ntdef, Ntapi.ntstatus,
  NtUtils.Exceptions, NtUtils.ErrorMsg, Ntapi.ntpsapi, NtUtils.Access,
  System.TypInfo, DelphiUtils.Strings;

resourcestring
  TITLE_OS_ERROR = 'System error';
  TITLE_CONVERT = 'Conversion error';
  TITLE_BUG = 'This is definitely a bug...';

  BUGTRACKER = #$D#$A#$D#$A'If you known how to reproduce this error please ' +
    'help the project by opening an issue on our GitHub page'#$D#$A +
    'https://github.com/diversenok/TokenUniverse';

  SUGGEST_TEMPLATE = #$D#$A#$D#$A'--- Suggestions ---'#$D#$A'%s';

  NEW_DUPLICATE_IMP = 'You can''t duplicate a token from a lower impersonation ' +
    'level to a higher one. Although, you can create a primary token from ' +
    'Impersonation and Delegation ones. If you are working with linked ' +
    'tokens you can obtain a primary linked token if you have Tcb privilege.';
  NEW_SAFER_IMP = 'Since Safer API always returns primary tokens the rules ' +
   'are the same as while performing duplication. This means only ' +
   'Impersonation, Delegation, and Primary tokens are suitable.';

  SETTER_OWNER = 'Only those groups from the token that are marked with ' +
    '`Owner` flag and the user itself can be set as an owner.';
  SETTER_PRIMARY = 'The SID must present in the group list of the token to be ' +
    'suitable as a primary group.';
  SETTER_PRIVILEGES_OTHER = 'You can''t enable some privileges if the ' +
   'integrity level of the token is too low.';
  SETTER_GROUPS_MODIFY = 'You can''t disable Mandatory groups ' +
    'just like you can''t enable Use-for-deny-only groups.';
  SETTER_AUDIT_ONCE = 'This is not an informative error, but the problem ' +
    'might be caused by an already set auditing policy for the token.';

  ACTION_ASSIGN_NOT_SUPPORTED = 'A token can be assigned only on an early ' +
    'stage of a process lifetime. Try this action on a newly created ' +
    'suspended process.';
  ACTION_ASSIGN_TYPE = 'Only a primary token can be assigned to a process';

  ACTION_SAFE_IMP = 'Not all security contexts can be impersonated if the ' +
    'target process does not possess Impersonate privilege. In this case ' +
    'NtSetInformationThread succeeds, but the target thread gets an ' +
    'Identification-level token that is not suitable for any access checks. ' +
    'The "Use safe impersonation technique" setting prevents such situations ' +
    'and shows this message instead.';

function SuggestConstructor(E: ENtError): String;
begin
  if E.Matches('NtDuplicateToken', STATUS_BAD_IMPERSONATION_LEVEL) then
    Exit(NEW_DUPLICATE_IMP);

  if E.Matches('SaferComputeTokenFromLevel', ERROR_BAD_IMPERSONATION_LEVEL) then
    Exit(NEW_SAFER_IMP);
end;

function MatchesInfoClass(E: ENtError; InfoClass: TTokenInformationClass):
  Boolean;
begin
  Result := (E.LastCall.CallType = lcQuerySetCall) and
    (E.LastCall.InfoClass = Cardinal(InfoClass)) and
    (E.LastCall.InfoClassType = TypeInfo(TTokenInformationClass));
end;

function MatchesInfoClassAndCode(E: ENtError; InfoClass: TTokenInformationClass;
  ErrorCode: Cardinal): Boolean;
begin
  Result := (E.ErrorCode = ErrorCode) and MatchesInfoClass(E, InfoClass);
end;

function SuggestSetter(E: ENtError): String;
begin
  if MatchesInfoClassAndCode(E, TokenOwner, ERROR_INVALID_OWNER) then
    Exit(SETTER_OWNER);

  if MatchesInfoClassAndCode(E, TokenPrimaryGroup, ERROR_INVALID_PRIMARY_GROUP) then
    Exit(SETTER_PRIMARY);

  if MatchesInfoClassAndCode(E, TokenAuditPolicy, ERROR_INVALID_PARAMETER) then
    Exit(SETTER_AUDIT_ONCE);

  if E.Matches('NtAdjustPrivilegesToken', STATUS_NOT_ALL_ASSIGNED) then
    Exit(SETTER_PRIVILEGES_OTHER);

  if E.ErrorLocation = 'NtAdjustGroupsToken' then
  begin
    if E.ErrorCode = STATUS_CANT_ENABLE_DENY_ONLY then
      Exit(SETTER_GROUPS_MODIFY);

    if E.ErrorCode = STATUS_CANT_DISABLE_MANDATORY then
      Exit(SETTER_GROUPS_MODIFY);
  end;

  if (E.ErrorLocation = 'NtSetInformationProcess') and
    (E.LastCall.CallType = lcQuerySetCall) and
    (E.LastCall.InfoClass = Cardinal(ProcessAccessToken)) then
  begin
   if E.ErrorCode = STATUS_NOT_SUPPORTED then
     Exit(ACTION_ASSIGN_NOT_SUPPORTED);

   if E.ErrorCode = STATUS_BAD_IMPERSONATION_LEVEL then
     Exit(ACTION_ASSIGN_TYPE);
  end;

  if E.Matches('NtxSafeSetThreadToken', STATUS_PRIVILEGE_NOT_HELD) then
    Exit(ACTION_SAFE_IMP);
end;

function SuggestAll(E: ENtError): String;
begin
  Result := SuggestSetter(ENtError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

  Result := SuggestConstructor(ENtError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

end;

procedure AddExpectedAccessList(ENt: ENtError; var Msg: String);
var
  i: Integer;
begin
  with ENt.LastCall do
    for i := 0 to High(ExpectedAccess) do
      with ExpectedAccess[i] do
        Msg := Msg + #$D#$A + 'Expected ' + GetAccessTypeName(AccessMaskType) +
          ' access: ' + FormatAccess(AccessMask, AccessMaskType);
end;

function GetPrivilegeName(Value: TSeWellKnownPrivilege): string;
begin
  if (Value <= SE_RESERVED_LUID_1) or (Value > High(TSeWellKnownPrivilege)) then
    Exit('');

  Result := ': "' + PrettifyCapsUnderscoreEnum('SE_',
    TypeInfo(TSeWellKnownPrivilege), Integer(Value)) + '"';
end;

procedure ShowErrorSuggestions(ParentWnd: HWND; E: Exception);
var
  Dlg: TASKDIALOGCONFIG;
  ENt: ENtError;
  Msg: String;
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
    ENt := ENtError(E);

    if not NT_ERROR(ENt.ErrorCode) then
      Dlg.pszMainIcon := TD_WARNING_ICON;

    Dlg.pszMainInstruction := PWideChar(NtxStatusDescription(ENt.ErrorCode));

    if Dlg.pszMainInstruction = '' then
      Dlg.pszMainInstruction := PWideChar(TITLE_OS_ERROR);

    Msg := 'Last call: ' + ENt.ErrorLocation;

    case ENt.LastCall.CallType of
      lcOpenCall:
      begin
        Msg := Msg + #$D#$A + 'Desired ' +
          GetAccessTypeName(ENt.LastCall.AccessMaskType) + ' access: ' +
          FormatAccess(ENt.LastCall.AccessMask, ENt.LastCall.AccessMaskType);

        AddExpectedAccessList(ENt, Msg);
      end;
      lcQuerySetCall:
        Msg := Msg + #$D#$A + 'Information class: ' + GetEnumName(
          ENt.LastCall.InfoClassType, Integer(ENt.LastCall.InfoClass));
    end;

    Msg := Msg + #$D#$A + 'Result: ' + NtxStatusToString(ENt.ErrorCode);
    Msg := Msg + #$D#$A#$D#$A + NtxFormatErrorMessage(ENt.ErrorCode);

    if ENt.ErrorCode = STATUS_PRIVILEGE_NOT_HELD then
      Msg := Msg + GetPrivilegeName(ENt.LastCall.ExpectedPrivilege);

    if (ENt.ErrorCode = STATUS_ACCESS_DENIED) and
      (ENt.LastCall.CallType <> lcOpenCall) and
      (Length(ENt.LastCall.ExpectedAccess) > 0) then
    begin
      Msg := Msg + #$D#$A;
      AddExpectedAccessList(ENt, Msg);
    end;

    Msg := Msg + SuggestAll(ENt);

    Dlg.pszContent := PWideChar(Msg);
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
