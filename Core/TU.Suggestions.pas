unit TU.Suggestions;

interface

uses
  Ntapi.WinUser, TU.Tokens3;

// Default messages
procedure ShowSuccessMessage(Parent: THwnd; const Text: String);
function AskForConfirmation(Parent: THwnd; const Text: String): Boolean;

// Operation suggestions
function AskConvertToPrimary(Parent: THwnd; var Token: IToken3): Boolean;
function AskConvertToImpersonation(Parent: THwnd; var Token: IToken3): Boolean;
procedure CheckSandboxInert(Parent: THwnd; const Token: IToken3);

implementation

uses
  Ntapi.ntstatus, Ntapi.WinError, Ntapi.ntdef, Ntapi.ntseapi, Ntapi.ntpsapi,
  System.TypInfo, NtUtils, NtUiLib.Exceptions.Dialog, NtUiLib.TaskDialog,
  TU.Tokens3.Open, TU.Tokens.Old.Types, NtUiLib.Errors, System.SysUtils;

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
    'Inert flag despite being requested to beacuse doing so requires system ' +
    'privileges.';

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
  NewToken: IToken3;
begin
  Result := False;

  if not (Token as IToken3).QueryType(CurrentType).IsSuccess then
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
  NewToken: IToken3;
begin
  Result := False;

  if not (Token as IToken3).QueryType(CurrentType).IsSuccess then
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
  if (Token as IToken3).QuerySandboxInert(SandboxInert).IsSuccess and
    not SandboxInert then
    UsrxShowTaskDialog(Parent, SUGGESTION_TITLE, TOKEN_NO_SANDBOX_INERT,
      TOKEN_NO_SANDBOX_INERT_TEXT, diWarning);
end;

type
  TSuggestion = record
    Location: String;
    Status: NTSTATUS;
    Text: String;
  end;

  TInfoClassSuggestion = record
    InfoClass: Cardinal;
    Status: NTSTATUS;
    Text: String;
  end;

const
  TokenOperations: array [0..4] of TSuggestion = (
    (Location: 'NtDuplicateToken';
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'You can''t duplicate a token from a lower impersonation ' +
      'level to a higher one. Although, you can create primary tokens ' +
      'from impersonation/delegation-level tokens.'),

    (Location: 'NtCreateLowBoxToken';
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'This function always returns a primary token, so the input must ' +
      'be either a primary token, or an impersonation/delegation-level token.'),

    (Location: 'SaferComputeTokenFromLevel';
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'Since Safer API always returns primary tokens the rules are the ' +
       'the same as while performing duplication. This means only primary or ' +
       'impersonation/delegation-level tokens are suitable.'),

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
      'happening.')
  );

  TokenSetters: array [0..2] of TInfoClassSuggestion = (
    (InfoClass: Cardinal(TokenOwner);
     Status: STATUS_INVALID_OWNER;
     Text: 'Only the user itself and those groups that are marked with the ' +
      '`Owner` flag can be set as an owner of a token.'),

    (InfoClass: Cardinal(TokenPrimaryGroup);
     Status: STATUS_INVALID_PRIMARY_GROUP;
     Text: 'The Security ID must present in the group list of the token to ' +
      'be suitable as a primary group.'),

    (InfoClass: Cardinal(TokenAuditPolicy);
     Status: STATUS_INVALID_PARAMETER;
     Text: 'It''s not very informative, but the problem might be caused by ' +
      'limitations enforced on this information class: auditing policy can ' +
      'be set on a token only once.')
  );

  ProcessSetters: array [0..1] of TInfoClassSuggestion = (
    (InfoClass: Cardinal(ProcessAccessToken);
     Status: STATUS_NOT_SUPPORTED;
     Text: 'Tokens can be assigned to processes only on early stages of ' +
      'their lifetime. Try this action on a newly created suspended process.'),

    (InfoClass: Cardinal(ProcessAccessToken);
     Status: STATUS_BAD_IMPERSONATION_LEVEL;
     Text: 'Only a primary token can be assigned to a process.')
  );

function IsQuerySetCall(const NtxStatus: TNtxStatus; Location: String;
  InfoClassType: PTypeInfo): Boolean;
begin
  Result := (NtxStatus.LastCall.CallType = lcQuerySetCall) and
    (NtxStatus.LastCall.InfoClassType = InfoClassType) and
    (NtxStatus.Location = Location);
end;

function Suggestions(const NtxStatus: TNtxStatus): String;
var
  i: Integer;
begin
  for i := 0 to High(TokenOperations) do
    if NtxStatus.Matches(TokenOperations[i].Status,
      TokenOperations[i].Location) then
      Exit(TokenOperations[i].Text);

  if IsQuerySetCall(NtxStatus, 'NtSetInformationToken',
    TypeInfo(TTokenInformationClass)) then
    for i := 0 to High(TokenSetters) do
      if (NtxStatus.LastCall.InfoClass = Cardinal(TokenSetters[i].InfoClass))
        and (NtxStatus.Status = TokenSetters[i].Status) then
        Exit(TokenSetters[i].Text);

  if IsQuerySetCall(NtxStatus, 'NtSetInformationProcess',
    TypeInfo(TProcessInfoClass)) then
    for i := 0 to High(ProcessSetters) do
      if (NtxStatus.LastCall.InfoClass = Cardinal(ProcessSetters[i].InfoClass))
        and (NtxStatus.Status = ProcessSetters[i].Status) then
        Exit(ProcessSetters[i].Text);
end;

initialization
  // Overwrite bug report address
  BUG_MESSAGE := BUGTRACKER;

  // Register our error suggestions
  RegisterSuggestions(Suggestions);
finalization

end.
