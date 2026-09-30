unit TU.Suggestions;

interface

uses
  Ntapi.WinUser, Ntapi.ntseapi, TU.Tokens;

// Default messages
procedure ShowSuccessMessage(Parent: THwnd; const Text: String);
function AskForConfirmation(Parent: THwnd; const Text: String): Boolean;

// Operation suggestions
function AskConvertToPrimary(Parent: THwnd; var Token: IToken): Boolean;
function AskConvertToImpersonation(Parent: THwnd; var Token: IToken): Boolean;
procedure CheckSandboxInert(Parent: THwnd; const Token: IToken);
procedure CheckPrivilege(Parent: THwnd; Privilege: TSeWellKnownPrivilege);

implementation

uses
  Ntapi.ntstatus, Ntapi.ntdef, Ntapi.ntpsapi, Ntapi.ntpebteb, Ntapi.NtSecApi,
  NtUtils, NtUiLib.Errors.Dialog, NtUiLib.Exceptions.Dialog, NtUiLib.TaskDialog,
  TU.Tokens.Open, TU.Tokens.Old.Types, DelphiUiLib.LiteReflection,
  System.SysUtils;

const
  BUGTRACKER = 'If you known how to reproduce this error please ' +
    'help the project by opening an issue on our GitHub page'#$D#$A +
    'https://github.com/diversenok/TokenUniverse';

  SUGGESTION_TITLE = 'Pro Tip';
  TOKEN_TYPE_MISMATCH = 'Wrong Token Type';
  TOKEN_NEED_PRIMARY = 'This action requires a primary token while you ' +
   'are trying to use an impersonation one. Do you want to duplicate it first?';
  TOKEN_NEED_IMPERSONATION = 'This action requires an impersonation token ' +
   '(which includes everything from Anonymous up to Delegation) while you ' +
   'are trying to use a primary one. Do you want to duplicate it first?';

  TOKEN_NO_SANDBOX_INERT = 'Not Sandbox Inert';
  TOKEN_NO_SANDBOX_INERT_TEXT = 'The operation did not enable the Sandbox ' +
    'Inert flag despite being requested to because doing so requires system ' +
    'privileges.';

  TOKEN_NO_PRIVILEGE = 'Privilege Not Enabled';
  TOKEN_NO_PRIVILEGE_TEXT = 'The required privilege (%s) is not enabled in ' +
   'the current token. Do you want to enable it first?';

procedure ShowSuccessMessage;
begin
  UsrxShowTaskDialog(Parent, 'Success', 'Success', Text, diInfo, dbOk);
end;

function AskForConfirmation;
begin
  Result := UsrxShowTaskDialog(Parent, 'Confirmation', 'Confirmation Required',
    Text, diConfirmation, dbYesNoCancel) = IDYES;
end;

function AskConvertToPrimary;
var
  CurrentType: TTokenType;
  NewToken: IToken;
begin
  Result := False;

  if not Token.QueryType(CurrentType).IsSuccess then
    Exit;

  if CurrentType = TokenPrimary then
    Exit;

  case UsrxShowTaskDialog(Parent, SUGGESTION_TITLE, TOKEN_TYPE_MISMATCH,
    TOKEN_NEED_PRIMARY, diWarning, dbYesAbortIgnore) of

    IDABORT, IDCANCEL:
      Abort;

    IDYES:
      begin
        MakeDuplicateToken(NewToken, Token, ttPrimary).RaiseOnError;
        Token := NewToken;
        Result := True;
      end;
  end;
end;

function AskConvertToImpersonation;
var
  CurrentType: TTokenType;
  NewToken: IToken;
begin
  Result := False;

  if not Token.QueryType(CurrentType).IsSuccess then
    Exit;

  if CurrentType = TokenImpersonation then
    Exit;

  case UsrxShowTaskDialog(Parent, SUGGESTION_TITLE, TOKEN_TYPE_MISMATCH,
    TOKEN_NEED_IMPERSONATION, diWarning, dbYesAbortIgnore) of

    IDABORT, IDCANCEL:
      Abort;

    IDYES:
      begin
        MakeDuplicateToken(NewToken, Token, ttImpersonation).RaiseOnError;
        Token := NewToken;
        Result := True;
      end;
  end;
end;

procedure CheckSandboxInert;
var
  SandboxInert: LongBool;
begin
  if Token.QuerySandboxInert(SandboxInert).IsSuccess and
    not SandboxInert then
    UsrxShowTaskDialog(Parent, SUGGESTION_TITLE, TOKEN_NO_SANDBOX_INERT,
      TOKEN_NO_SANDBOX_INERT_TEXT, diWarning);
end;

procedure CheckPrivilege;
var
  Token: IToken;
  Privileges: TArray<TPrivilege>;
  i: Integer;
begin
  if not MakeOpenEffectiveToken(Token, NtCurrentTeb.ClientID, TOKEN_QUERY or
    TOKEN_ADJUST_PRIVILEGES).IsSuccess then
    Exit;

  if not Token.QueryPrivileges(Privileges).IsSuccess then
    Exit;

  for i := 0 to High(Privileges) do
    if (Privileges[i].Luid = TPrivilegeId(Privilege)) and
      not BitTest(Privileges[i].Attributes and SE_PRIVILEGE_ENABLED) then
    begin
      case UsrxShowTaskDialog(Parent, SUGGESTION_TITLE, TOKEN_NO_PRIVILEGE,
        Format(TOKEN_NO_PRIVILEGE_TEXT, [Rttix.Format(Privilege)]),
        diWarning, dbYesAbortIgnore) of

        IDYES:
          Token.AdjustPrivileges([Privilege],
            SE_PRIVILEGE_ENABLED).RaiseOnError;

        IDABORT:
          Abort;
      end;

      Break;
    end;
end;

type
  TSuggestion = record
    Location: String;
    CallType: TLastCallType;
    InfoClass: Cardinal;
    Status: NTSTATUS;
    Text: String;
  end;

const
  KnownSuggesions: array [0..9] of TSuggestion = (
    (Location: 'NtDuplicateToken';
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'You can''t duplicate a token from a lower impersonation ' +
      'level to a higher one. Although, you can create primary tokens ' +
      'from impersonation- and delegation-level tokens.'),

    (Location: 'NtCreateLowBoxToken';
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'This function always returns a primary token, so the input must ' +
      'be either a primary token, or an impersonation/delegation-level token.'),

    (Location: 'SaferComputeTokenFromLevel';
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'Since Safer API always returns primary tokens the rules are the ' +
       'the same as while performing duplication. This means only primary or ' +
       'impersonation- and delegation-level tokens are suitable.'),

    (Location: 'NtAdjustPrivilegesToken';
     Status: STATUS_NOT_ALL_ASSIGNED;
     Text: 'You can''t enable some privileges if the integrity level of the ' +
       'token is too low.'),

    (Location: 'NtxSafeSetThreadToken'; // custom
     Status: STATUS_PRIVILEGE_NOT_HELD;
     Text: 'Not all security contexts can be impersonated if the target ' +
      'process does not have the Impersonate privilege. In this case ' +
      'NtSetInformationThread succeeds, but the target thread gets an ' +
      'identification-level copy of the token which is not suitable for any ' +
      'access checks. Safe impersonation technique prevents it from ' +
      'happening.'),

    (Location: 'NtSetInformationToken';
     CallType: lcQuerySetCall;
     InfoClass: Cardinal(TokenOwner);
     Status: STATUS_INVALID_OWNER;
     Text: 'Only the user SID and groups marked with the `Owner` flag can be ' +
      'set as an owner of a token.'),

    (Location: 'NtSetInformationToken';
     CallType: lcQuerySetCall;
     InfoClass: Cardinal(TokenPrimaryGroup);
     Status: STATUS_INVALID_PRIMARY_GROUP;
     Text: 'The SID must present in the group list of the token to ' +
      'be suitable as a primary group.'),

    (Location: 'NtSetInformationToken';
     CallType: lcQuerySetCall;
     InfoClass: Cardinal(TokenAuditPolicy);
     Status: STATUS_INVALID_PARAMETER;
     Text: 'The error code is not very informative, but it might be caused ' +
      'by limitations enforced on this information class: auditing policy ' +
      'can be set on a token only once.'),

    (Location: 'NtSetInformationProcess';
     CallType: lcQuerySetCall;
     InfoClass: Cardinal(ProcessAccessToken);
     Status: STATUS_NOT_SUPPORTED;
     Text: 'Tokens can be assigned to processes only on early stages of ' +
      'their lifetime. Try this action on a newly created suspended process.'),

    (Location: 'LsaCallAuthenticationPackage';
     CallType: lcQuerySetCall;
     InfoClass: Cardinal(MsV1_0LookupToken);
     Status: STATUS_ACCESS_DENIED;
     Text: 'Querying logon session tokens requires NT SERVICE\CscService ' +
       'group membership and SeTcbPrivilege.')
  );

function SuggestionProvider(
  const Status: TNtxStatus;
  out Suggestion: String
): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(KnownSuggesions) do
    if (Status.Status = KnownSuggesions[i].Status) and
      (Status.LastCall.CallType = KnownSuggesions[i].CallType) and
      (Status.LastCall.InfoClass = KnownSuggesions[i].InfoClass) and
      (Status.LastCall.Location = KnownSuggesions[i].Location) then
    begin
      Suggestion := KnownSuggesions[i].Text;
      Exit(True);
    end;

  Result := False;
end;

initialization
  // Overwrite bug report address
  BUG_MESSAGE := BUGTRACKER;

  // Register our error suggestions
  UiLibRegisterSuggestionProvider(SuggestionProvider);
finalization

end.
