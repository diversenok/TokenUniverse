unit TU.Tokens.Types;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  Winapi.WinNt, Winapi.NtSecApi, NtUtils.Security.Sid, Ntapi.ntseapi;

type
  TGroupAdjustAction = (gaResetDefault, gaEnable, gaDisable);

  TPrivilegeAdjustAction = (paEnable, paDisable, paRemove);

  TTokenTypeEx = (ttAnonymous, ttIdentification, ttImpersonation, ttDelegation,
   ttPrimary);

  TTokenTypeExHelper = record helper for TTokenTypeEx
    function ToString: String;
    function TokenTypeValue: TTokenType;
    function SecurityImpersonationLevel: TSecurityImpersonationLevel;
  end;

  TTokenIntegrityLevel = (
    ilUntrusted = $0000,
    ilLow = $1000,
    ilMedium = $2000,
    ilMediumPlus = $2100,
    ilHigh = $3000,
    ilSystem = $4000,
    ilProtected = $5000
  );

  TTokenIntegrity = record
    Group: TGroup;
    Level: TTokenIntegrityLevel;
    function IsWellKnown: Boolean;
    function ToString: String;
  end;

function CreateTokenSource(SourceName: String; SourceLuid: TLuid):
  TTokenSource;

function FormatCurrentState: String;

{ Comparison function used by cached event handling system }
function CompareSIDs(Value1, Value2: ISid): Boolean;
function CompareCardinals(Value1, Value2: Cardinal): Boolean;
function CompareLUIDs(Value1, Value2: TLuid): Boolean;
function CompareIntegrities(Value1, Value2: TTokenIntegrity): Boolean;
function CompareLongBools(Value1, Value2: LongBool): Boolean;
function ComparePrivileges(Value1, Value2: TPrivilegeArray): Boolean;
function CompareGroups(Value1, Value2: TGroupArray): Boolean;
function CompareStatistics(Value1, Value2: TTokenStatistics): Boolean;

{ Conversion functions }
function TokeSourceNameToString(TokenSource: TTokenSource): String;
function ObjectAttributesToString(ObjAttributes: Cardinal): String;
function NativeTimeToString(NativeTime: TLargeInteger): String;

implementation

uses
  System.SysUtils, DelphiUtils.Strings,
  Winapi.ntlsa, Ntapi.ntdef, Ntapi.ntrtl, TU.Winapi;


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

{ TTokenIntegrity }

function TTokenIntegrity.IsWellKnown: Boolean;
begin
  case Level of
    ilUntrusted, ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem, ilProtected:
      Result := True;
  else
    Result := False;
  end;
end;

function TTokenIntegrity.ToString: String;
begin
  case Level of
    ilUntrusted: Result := 'Untrusted';
    ilLow: Result := 'Low';
    ilMedium: Result := 'Medium';
    ilMediumPlus: Result := 'Medium +';
    ilHigh: Result := 'High';
    ilSystem: Result := 'System';
    ilProtected: Result := 'Protected';
  else
    Result := IntToHexEx(UInt64(Level));
  end;
end;

{ TTokenSource }

function CreateTokenSource(SourceName: String; SourceLuid: TLuid):
  TTokenSource;
var
  i, Count: integer;
begin
  FillChar(Result, SizeOf(Result), 0);

  Count := Length(SourceName);
  if Count > 8 then
    Count := 8;

  for i := 1 to Count do
    Result.sourcename[i] := AnsiChar(SourceName[Low(SourceName) + i - 1]);

  Result.SourceIdentifier := SourceLuid;
end;

function FormatCurrentState: String;
var
  User, Domain: PLsaUnicodeString;
begin
  if NT_SUCCESS(LsaGetUserName(User, Domain))  then
  begin
    if (Domain.Length > 0) and (User.Length > 0) then
      Result := Domain.ToString + '\' + User.ToString
    else if Domain.Length > 0 then
      Result := Domain.ToString
    else if User.Length > 0 then
      Result := User.ToString
    else
      Result := 'N/A';

    LsaFreeMemory(User);
    LsaFreeMemory(Domain);
  end
  else
    Result := 'Unknown user';

  Result := Result + ' @ ' + IntToStr(RtlGetCurrentPeb.SessionId);
end;

{ Comparison functions }

function CompareSIDs(Value1, Value2: ISid): Boolean;
begin
  Result := Value1.EqualsTo(Value2);
end;

function CompareCardinals(Value1, Value2: Cardinal): Boolean;
begin
  Result := Value1 = Value2;
end;

function CompareLUIDs(Value1, Value2: TLuid): Boolean;
begin
  Result := Value1 = Value2;
end;

function CompareGroups(Value1, Value2: TGroupArray): Boolean;
var
  i: integer;
begin
  Result := Length(Value1) = Length(Value2);
  if Result then
    for i := 0 to High(Value1) do
      if (Value1[i].SecurityIdentifier.Lookup.SDDL <>
        Value2[i].SecurityIdentifier.Lookup.SDDL)
        or (Value1[i].Attributes <> Value2[i].Attributes) then
          Exit(False);
end;

function CompareIntegrities(Value1, Value2: TTokenIntegrity): Boolean;
begin
  Result := (Value1.Group.SecurityIdentifier.Lookup.SDDL =
    Value2.Group.SecurityIdentifier.Lookup.SDDL) and
    (Value1.Group.Attributes = Value2.Group.Attributes);
end;

function CompareLongBools(Value1, Value2: LongBool): Boolean;
begin
  Result := Value1 = Value2;
end;

function ComparePrivileges(Value1, Value2: TPrivilegeArray): Boolean;
var
  i: integer;
begin
  Result := Length(Value1) = Length(Value2);
  if Result then
    for i := 0 to High(Value1) do
      if (Value1[i].Attributes <> Value2[i].Attributes) or
        (Value1[i].Luid <> Value2[i].Luid) then
        Exit(False);
end;

function CompareStatistics(Value1, Value2: TTokenStatistics): Boolean;
begin
  Result := (Value1.ModifiedId = Value2.ModifiedId);
end;

{ Conversion functions }

function TokeSourceNameToString(TokenSource: TTokenSource): String;
begin
  // sourcename field may or may not contain zero-termination byte
  Result := String(PAnsiChar(AnsiString(TokenSource.sourcename)));
end;

function ObjectAttributesToString(ObjAttributes: Cardinal): String;
begin
  if ObjAttributes and (OBJ_PERMANENT or OBJ_EXCLUSIVE) =
    (OBJ_PERMANENT or OBJ_EXCLUSIVE) then
    Result := 'Permanent, Exclusive'
  else if ObjAttributes and OBJ_PERMANENT <> 0 then
    Result := 'Permanent'
  else if ObjAttributes and OBJ_EXCLUSIVE <> 0 then
    Result := 'Exclusive'
  else
    Result := 'None';
end;

function NativeTimeToString(NativeTime: TLargeInteger): String;
begin
  if NativeTime.QuadPart = 0 then
    Result := 'Never'
  else if NativeTime.QuadPart = Int64.MaxValue then
    Result := 'Infinite'
  else
    Result := DateTimeToStr(NativeTime.ToDateTime);
end;

end.
