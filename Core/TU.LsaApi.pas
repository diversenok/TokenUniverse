unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  TU.Tokens.Types, NtUtils.Exceptions, NtUtils.Types,
  Winapi.WinNt, Winapi.WinBase, Ntapi.ntseapi, Winapi.NtSecApi;

type
  TLogonDataClass = (lsLogonId, lsSecurityIdentifier, lsUserName, lsLogonDomain,
    lsAuthPackage, lsLogonType, lsSession, lsLogonTime, lsLogonServer,
    lsDnsDomainName, lsUpn, lsUserFlags, lsLastSuccessfulLogon,
    lsLastFailedLogon, lsFailedAttemptSinceSuccess, lsLogonScript,
    lsProfilePath, lsHomeDirectory, lsHomeDirectoryDrive, lsLogoffTime,
    lsKickOffTime, lsPasswordLastSet, lsPasswordCanChange, lsPasswordMustChange
  );

  TLuidDynArray = array of TLuid;

  TLogonSessionInfo = class
  private
    FData: PSecurityLogonSessionData;
    function GetUser: ISid;
    function GetUserPresent: Boolean;
  public
    constructor Create(OwnedData: PSecurityLogonSessionData);
    destructor Destroy; override;

    property UserPresent: Boolean read GetUserPresent;
    property User: ISid read GetUser;
    property Data: PSecurityLogonSessionData read FData;
    function GetString(InfoClass: TLogonDataClass): String;
    function UserFlagsHint: String;

    class function Query(LogonId: TLuid): TLogonSessionInfo; static;
    class function Enumerate: TLuidDynArray; static;
  end;

  TPrivilegeRec = record
    NameValid, DisplayNameValid: Boolean;
    Name, DisplayName: String;
  end;

  TPrivilegeCache = class
  private
    class var Cache: array [SE_MIN_WELL_KNOWN_PRIVILEGE ..
      SE_MAX_WELL_KNOWN_PRIVILEGE] of TPrivilegeRec;
    class function InRange(Value: TLuid): Boolean; static; inline;
    class function TryQueryName(Value: Int64; out Name: String): Boolean;
      static;
  public
    class function QueryName(Value: Int64): String; static;
    class function QueryDisplayName(Value: Int64): String; static;
    class function AllPrivileges: TLuidDynArray;
  end;

implementation

uses
  Ntapi.ntdef, Ntutils.Lsa, System.SysUtils, DelphiUtils.Strings;

{ TLogonSessionInfo }

const
  USER_FLAGS_COUNT = 19;
  FlagValues: array [1 .. USER_FLAGS_COUNT] of Cardinal = (LOGON_GUEST,
    LOGON_NOENCRYPTION, LOGON_CACHED_ACCOUNT, LOGON_USED_LM_PASSWORD,
    LOGON_EXTRA_SIDS, LOGON_SUBAUTH_SESSION_KEY, LOGON_SERVER_TRUST_ACCOUNT,
    LOGON_NTLMV2_ENABLED, LOGON_RESOURCE_GROUPS, LOGON_PROFILE_PATH_RETURNED,
    LOGON_NT_V2, LOGON_LM_V2, LOGON_NTLM_V2, LOGON_OPTIMIZED, LOGON_WINLOGON,
    LOGON_PKINIT, LOGON_NO_OPTIMIZED, LOGON_NO_ELEVATION, LOGON_MANAGED_SERVICE
  );
  FlagStrings: array [1 .. USER_FLAGS_COUNT] of String = ('Guest',
    'No Encryption', 'Cached Account', 'Used LM Password', 'Extra SIDs',
    'Subauth Session Key', 'Server Trust Account', 'NTLMv2 Enabled',
    'Resource Groups', 'Profile Path Returned', 'NTv2', 'LMv2', 'NTLMv2',
    'Optimized', 'Winlogon', 'Pkinit', 'No Optimized', 'No Elevation',
    'Managed Service'
  );

function UserFlagsToString(UserFlags: Cardinal): String;
var
  Strings: array of string;
  FlagInd, StrInd: Integer;
begin
  if UserFlags = 0 then
    Exit('');

  SetLength(Strings, USER_FLAGS_COUNT);
  StrInd := 0;
  for FlagInd := 1 to USER_FLAGS_COUNT do
    if UserFlags and FlagValues[FlagInd] = FlagValues[FlagInd] then
    begin
      Strings[StrInd] := FlagStrings[FlagInd];
      Inc(StrInd);
    end;
  SetLength(Strings, StrInd);
  Result := String.Join(', ', Strings);
end;

function LogonTypeToString(LogonType: TSecurityLogonType): String;
const
  Mapping: array [TSecurityLogonType] of String = ('System', 'Reserved',
    'Interactive', 'Network', 'Batch', 'Service', 'Proxy', 'Unlock',
    'Network clear text', 'New credentials', 'Remote interactive',
    'Cached interactive', 'Cached remote interactive', 'Cached unlock');
begin
  if (LogonType >= Low(TSecurityLogonType)) and (LogonType <=
    High(TSecurityLogonType)) then
    Result := Mapping[LogonType]
  else
    Result := IntToStr(Integer(LogonType)) + ' (Out of bound)';
end;

constructor TLogonSessionInfo.Create(OwnedData: PSecurityLogonSessionData);
begin
  Assert(Assigned(OwnedData));
  FData := OwnedData;
end;

destructor TLogonSessionInfo.Destroy;
begin
  LsaFreeReturnBuffer(FData);
  inherited;
end;

class function TLogonSessionInfo.Enumerate: TLuidDynArray;
var
  Count, i: Integer;
  Sessions: PLuidArray;
begin
  SetLength(Result, 0);

  if NT_SUCCESS(LsaEnumerateLogonSessions(Count, Sessions)) then
  try
    SetLength(Result, Count);

    // Invert the order so that later logons appear later in the list
    for i := 0 to Count - 1 do
      Result[i] := Sessions[Count - 1 - i];
  finally
    LsaFreeReturnBuffer(Sessions);
  end;
end;

function TLogonSessionInfo.GetString(InfoClass: TLogonDataClass): String;
begin
  if not Assigned(Self) then
    Exit('Unknown');

  case InfoClass of
    lsLogonId:
      Result := IntToHexEx(Data.LogonId);

    lsSecurityIdentifier:
      if UserPresent then
        Result := GetUser.Lookup.FullName
      else
        Result := 'No User';

    lsUserName:
      Result := Data.UserName.ToString;

    lsLogonDomain:
      Result := Data.LogonDomain.ToString;

    lsAuthPackage:
      Result := Data.AuthenticationPackage.ToString;

    lsLogonType:
      Result := LogonTypeToString(Data.LogonType);

    lsSession:
      Result := Data.Session.ToString;

    lsLogonTime:
      Result := NativeTimeToString(Data.LogonTime);

    lsLogonServer:
      Result := Data.LogonServer.ToString;

    lsDnsDomainName:
      Result := Data.DnsDomainName.ToString;

    lsUpn:
      Result := Data.Upn.ToString;

    lsUserFlags:
      Result := UserFlagsToString(Data.UserFlags);

    lsLastSuccessfulLogon:
      Result := NativeTimeToString(Data.LastLogonInfo.LastSuccessfulLogon);

    lsLastFailedLogon:
      Result := NativeTimeToString(Data.LastLogonInfo.LastFailedLogon);

    lsFailedAttemptSinceSuccess:
      Result := Data.LastLogonInfo.FailedAttemptCountSinceLastSuccessfulLogon.
        ToString;

    lsLogonScript:
      Result := Data.LogonScript.ToString;

    lsProfilePath:
      Result := Data.ProfilePath.ToString;

    lsHomeDirectory:
      Result := Data.HomeDirectory.ToString;

    lsHomeDirectoryDrive:
      Result := Data.HomeDirectoryDrive.ToString;

    lsLogoffTime:
      Result := NativeTimeToString(Data.LogoffTime);

    lsKickOffTime:
      Result := NativeTimeToString(Data.KickOffTime);

    lsPasswordLastSet:
      Result := NativeTimeToString(Data.PasswordLastSet);

    lsPasswordCanChange:
      Result := NativeTimeToString(Data.PasswordCanChange);

    lsPasswordMustChange:
      Result := NativeTimeToString(Data.PasswordMustChange);

  end;
end;

function TLogonSessionInfo.GetUser: ISid;
begin
  Assert(UserPresent);
  Result := TSid.CreateCopy(Data.Sid);
end;

function TLogonSessionInfo.GetUserPresent: Boolean;
begin
  Result := Assigned(Self) and Assigned(Data.Sid)
end;

class function TLogonSessionInfo.Query(LogonId: TLuid): TLogonSessionInfo;
var
  Buffer: PSecurityLogonSessionData;
begin
  // TODO -c WoW64: LsaGetLogonSessionData returns a weird pointer
  if NT_SUCCESS(LsaGetLogonSessionData(LogonId, Buffer)) then
    Result := TLogonSessionInfo.Create(Buffer)
  else
    Result := nil;
end;

function TLogonSessionInfo.UserFlagsHint: String;
var
  Strings: array of string;
  i: Integer;
begin
  if not Assigned(Self) then
    Exit('');

  SetLength(Strings, Length(FlagValues));
  for i := 1 to USER_FLAGS_COUNT do
    Strings[i - 1] := CheckboxToString(Data.UserFlags and FlagValues[i] =
      FlagValues[i]) + ' ' + FlagStrings[i];

  Result := String.Join(#$D#$A, Strings);
end;

{ TPrivilegeCache }

class function TPrivilegeCache.AllPrivileges: TLuidDynArray;
var
  i: Integer;
  Privileges: TPrivDefArray;
  Value: TLuid;
begin
  // Ask LSA to enumerate the privileges.
  if NT_SUCCESS(LsaxEnumeratePrivileges(Privileges)) then
  begin
    SetLength(Result, Length(Privileges));

    for i := 0 to High(Privileges) do
    begin
      Value := Privileges[i].LocalValue;

      // Save into the result list
      Result[i] := Value;

      // Cache privilege name
      if InRange(Value) then
      begin
        Cache[Value].NameValid := True;
        Cache[Value].Name := Privileges[i].Name;
      end;
    end;
  end
  else
  begin
    // Query was unsuccessful. Just return the range from min to max
    SetLength(Result, SE_MAX_WELL_KNOWN_PRIVILEGE - SE_MIN_WELL_KNOWN_PRIVILEGE);

    for i := 0 to High(Result) do
      Result[i] := SE_MIN_WELL_KNOWN_PRIVILEGE + i;
  end;
end;

class function TPrivilegeCache.InRange(Value: TLuid): Boolean;
begin
  Result := (SE_MIN_WELL_KNOWN_PRIVILEGE <= Value) and
    (Value <= SE_MAX_WELL_KNOWN_PRIVILEGE);
end;

class function TPrivilegeCache.QueryDisplayName(Value: Int64): String;
var
  Name: String;
begin
  // Note: we need privilege name to obtain its display name

  if InRange(Value) and Cache[Value].DisplayNameValid then
    Result := Cache[Value].DisplayName
  else if TryQueryName(Value, Name) and
    NT_SUCCESS(LsaxQueryDescriptionPrivilege(Name, Result)) then
  begin
    // Cache it if applicable
    if InRange(Value) then
    begin
      Cache[Value].DisplayNameValid := True;
      Cache[Value].DisplayName := Result;
    end;
  end
  else
    Result := '';
end;

class function TPrivilegeCache.QueryName(Value: Int64): String;
begin
  if not TryQueryName(Value, Result) then
    Result := 'Unknown privilege ' + IntToStr(Value);
end;

class function TPrivilegeCache.TryQueryName(Value: Int64; out Name: String):
  Boolean;
begin
  Result := True;

  // Try cache first, then query LSA
  if InRange(Value) and Cache[Value].NameValid then
    Name := Cache[Value].Name
  else if NT_SUCCESS(LsaxQueryNamePrivilege(Value, Name)) then
  begin
    // Cache it if applicable
    if InRange(Value) then
    begin
      Cache[Value].NameValid := True;
      Cache[Value].Name := Name;
    end;
  end
  else
    Result := False;
end;

end.
