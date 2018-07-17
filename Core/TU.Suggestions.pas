unit TU.Suggestions;

interface

uses
  System.SysUtils;

procedure ShowErrorSuggestions(E: Exception);

implementation

uses
  Winapi.Windows, System.UITypes, Vcl.Dialogs,
  TU.Common, TU.Tokens.Winapi, TU.Tokens;

resourcestring
  TITLE_OS_ERROR = 'System Error';
  TITLE_CONVERT = 'Conversion error';
  TITLE_BUG = 'This is definitely a bug...';
  BUGTRACKER = #$D#$A#$D#$A'If you known how to reproduce this error please ' +
    'help the project by opening an issue on our GitHub page'#$D#$A +
    'https://github.com/diversenok/TokenUniverse';

  SUGGEST_TEMPLATE = #$D#$A#$D#$A'--- Suggestions ---'#$D#$A'%s';

  DUPLICATE_IMP = 'You can''t raise a token from Anonymous and Identification ' +
    'up to Impersonation, Delegation, or Primary one. If you are working with ' +
    'linked tokens you can obtain a primary linked token if you have ' +
    'SeTcbPrivilege privilege. Or, if you have SeCreateTokenPrivilege you can ' +
    'cheat by creating the required token by yourself =)';

  WTS_QUERY_TOKEN_PRIV = 'WTSQueryUserToken requires SeTcbPrivilege.';
  WTS_QUERY_TOKEN_NO_TOKEN = 'You can''t query a token of a session that ' +
    'doesn''t belong to any user.';

  GETTER_QUERY = 'You need `Query` access right to obtain this information ' +
    'from the token';
  GETTER_QUERY_SOURCE = 'You need `Query Source` access right to obtain ' +
    'this information from the token';

  SETTER_SESSION = 'To change the session of a token you need to ' +
    'have SeTcbPrivilege and `Adjust SessionId` access right for the token.';
  SETTER_INTEGRITY = 'To raise the integrity level of a token you need to ' +
    'have SeTcbPrivilege.';

function SuggestConstructor(E: ELocatedOSError): String;
begin
  Result := '';

  if (E.ErrorOrigin = 'DuplicateTokenEx') and (E.ErrorContext is TToken) and
    (E.ErrorCode = ERROR_BAD_IMPERSONATION_LEVEL) then
    with TToken(E.ErrorContext).TokenTypeInfo do
      if IsValid and (Value.Impersonation <= SecurityImpersonation) then
        Exit(DUPLICATE_IMP);

  if E.ErrorOrigin = 'WTSQueryUserToken' then
  begin
    if E.ErrorCode = ERROR_PRIVILEGE_NOT_HELD then
      Exit(WTS_QUERY_TOKEN_PRIV);

    if E.ErrorCode = ERROR_NO_TOKEN then
      Exit(WTS_QUERY_TOKEN_NO_TOKEN);
  end;
end;

function SuggestGetter(E: ELocatedOSError): String;
begin
  if E.ErrorCode = ERROR_ACCESS_DENIED then
  begin
    if E.ErrorOrigin = GetterMessage(TokenSource) then
      Exit(GETTER_QUERY_SOURCE);

    if E.ErrorOrigin.StartsWith('GetTokenInformation:') then
      Exit(GETTER_QUERY);
  end;
end;

function SuggestSetter(E: ELocatedOSError): String;
begin
  Result := '';

  if E.ErrorCode = ERROR_ACCESS_DENIED then
  begin
    if E.ErrorOrigin = SetterMessage(TokenSessionId) then
      Exit(SETTER_SESSION);
  end;

  if E.ErrorCode = ERROR_PRIVILEGE_NOT_HELD then
  begin
    if E.ErrorOrigin = SetterMessage(TokenSessionId) then
      Exit(SETTER_SESSION);

    if E.ErrorOrigin = SetterMessage(TokenIntegrityLevel) then
      Exit(SETTER_INTEGRITY);
  end;
end;

function SuggestAll(E: ELocatedOSError): String;
begin
  Result := SuggestGetter(ELocatedOSError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

  Result := SuggestSetter(ELocatedOSError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

  Result := SuggestConstructor(ELocatedOSError(E));
  if Result <> '' then
    Exit(Format(SUGGEST_TEMPLATE, [Result]));

end;

procedure ShowErrorSuggestions(E: Exception);
begin
  if (E is EAccessViolation) or (E is EInvalidPointer) then
    TaskMessageDlg(TITLE_BUG, E.Message + BUGTRACKER, mtError, [mbOk], 0)
  else if E is EConvertError then
    TaskMessageDlg(TITLE_CONVERT, E.Message, mtError, [mbOk], 0)
  else if E is ELocatedOSError then
    TaskMessageDlg(TITLE_OS_ERROR, E.Message + SuggestAll(ELocatedOSError(E)),
      mtError, [mbOk], 0)
  else
    TaskMessageDlg(E.ClassName, E.Message, mtError, [mbOk], 0);
end;

end.
