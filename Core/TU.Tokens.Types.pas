unit TU.Tokens.Types;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  Ntapi.WinNt, Ntapi.ntseapi, Ntapi.ntrtl, NtUtils.Security.Sid, NtUtils;

type
  TTokenTypeEx = (ttAnonymous, ttIdentification, ttImpersonation, ttDelegation,
   ttPrimary);

  TTokenTypeExHelper = record helper for TTokenTypeEx
    function ToString: String;
    function TokenTypeValue: TTokenType;
    function SecurityImpersonationLevel: TSecurityImpersonationLevel;
  end;

function FormatCurrentState: String;

function TokenGenericMapping: TGenericMapping;

implementation

uses
  System.SysUtils, Ntapi.ntpebteb, NtUtils.Lsa, NtUtils.Lsa.Sid,
  NtUtils.Objects.Snapshots, DelphiUtils.Arrays, NtUtils.Objects;

{ TTokenTypeExHelper }

function TTokenTypeExHelper.SecurityImpersonationLevel:
  TSecurityImpersonationLevel;
begin
  if Self = ttPrimary then
    Result := SecurityImpersonation
  else
    Result := TSecurityImpersonationLevel(Self);
end;

function TTokenTypeExHelper.TokenTypeValue: TTokenType;
begin
  if Self = ttPrimary then
    Result := TokenPrimary
  else
    Result := TokenImpersonation;
end;

function TTokenTypeExHelper.ToString: String;
begin
  case Self of
    ttAnonymous: Result :=  'Anonymous';
    ttIdentification: Result := 'Identification';
    ttImpersonation: Result := 'Impersonation';
    ttDelegation: Result := 'Delegation';
    ttPrimary: Result := 'Primary token';
  end
end;

function FormatCurrentState: String;
begin
  if not LsaxGetFullUserName(Result).IsSuccess then
    Result := 'Unknown user';

  Result := Result + ' @ ' + IntToStr(RtlGetCurrentPeb.SessionId);
end;

function TokenGenericMapping: TGenericMapping;
var
  Types: TArray<TObjectTypeInfo>;
begin
  NtxEnumerateTypes(Types).IsSuccess;

  Result.GenericRead := TOKEN_DUPLICATE or TOKEN_QUERY or TOKEN_QUERY_SOURCE;
  Result.GenericWrite := TOKEN_ADJUST_PRIVILEGES or TOKEN_ADJUST_GROUPS or
    TOKEN_ADJUST_DEFAULT or TOKEN_ADJUST_SESSIONID;
  Result.GenericExecute := TOKEN_ASSIGN_PRIMARY or TOKEN_IMPERSONATE;
  Result.GenericAll := TOKEN_ALL_ACCESS;

  // Try to use the type snapshot, but fall back to local definition if needed
  Result := TArray.ConvertFirstOrDefault<TObjectTypeInfo, TGenericMapping>(
    Types,
    function (const Entry: TObjectTypeInfo; out Mapping: TGenericMapping):
      Boolean
    begin
      Result := Entry.TypeName = 'Token';

      if Result then
        Mapping := Entry.Other.GenericMapping;
    end,
    Result
    );
end;

end.
