unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  TU.Tokens.Types, NtUtils.Exceptions,
  Winapi.WinNt, Winapi.WinBase, Ntapi.ntseapi, Winapi.NtSecApi;

type
  TLogonDataClass = (lsLogonId, lsUserName, lsLogonDomain, lsAuthPackage,
    lsLogonType, lsSession, lsLogonTime, lsLogonServer, lsDnsDomainName, lsUpn,
    lsUserFlags, lsLastSuccessfulLogon, lsLastFailedLogon,
    lsFailedAttemptSinceSuccess, lsLogonScript, lsProfilePath, lsHomeDirectory,
    lsHomeDirectoryDrive, lsLogoffTime, lsKickOffTime, lsPasswordLastSet,
    lsPasswordCanChange, lsPasswordMustChange);

  TLuidDynArray = array of TLuid;

  TLogonSessionInfo = class
  private
    FData: PSecurityLogonSessionData;
    function GetUser: TSecurityIdentifier;
    function GetUserPresent: Boolean;
  public
    constructor Create(OwnedData: PSecurityLogonSessionData);
    destructor Destroy; override;

    property UserPresent: Boolean read GetUserPresent;
    property User: TSecurityIdentifier read GetUser;
    property Data: PSecurityLogonSessionData read FData;
    function GetString(InfoClass: TLogonDataClass): String;

    class function Query(LogonId: TLuid): TLogonSessionInfo; static;
    class function Enumerate: TLuidDynArray; static;
  end;

  TPrivilegeRec = record
  private
    IsChecked: Boolean;
  public
    Value: Int64;
    IsValid: Boolean;
    Name: String;
    DisplayName: String;
    constructor Create(Value: Int64);
  end;

  TPrivilegeRecArray = array of TPrivilegeRec;

  TPrivilegeCache = class
  private
    class var Cache: array [SE_MIN_WELL_KNOWN_PRIVILEGE ..
      SE_MAX_WELL_KNOWN_PRIVILEGE] of TPrivilegeRec;
    class var CacheEx: TPrivilegeRecArray;
  public
    class function Lookup(Value: Int64): TPrivilegeRec; static;
    class function AllPrivileges: TPrivilegeRecArray;
  end;

implementation

uses
  Ntapi.ntdef, System.SysUtils, TU.Common;

{ TLogonSessionInfo }

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

function UserFlagsToString(UserFlags: Cardinal): String;
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
      Result := LuidToString(Data.LogonId);

    lsUserName:
      if UserPresent then
        Result := Data.UserName.ToString
      else
        Result := 'No User';

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
      Result := YesNoToString(LongBool(Data.PasswordCanChange));

    lsPasswordMustChange:
      Result := YesNoToString(LongBool(Data.PasswordMustChange));

  end;
end;

function TLogonSessionInfo.GetUser: TSecurityIdentifier;
begin
  Assert(UserPresent);
  Result := TSecurityIdentifier.CreateFromSid(Data.Sid);
end;

function TLogonSessionInfo.GetUserPresent: Boolean;
begin
  Result := Assigned(Data.Sid)
end;

class function TLogonSessionInfo.Query(LogonId: TLuid): TLogonSessionInfo;
var
  Buffer: PSecurityLogonSessionData;
begin
  if NT_SUCCESS(LsaGetLogonSessionData(LogonId, Buffer)) then
    Result := TLogonSessionInfo.Create(Buffer)
  else
    Result := nil;
end;

{ TPrivilegeCache }

// TODO: Switch to LsaLookupPrivilege*

function DoLookupDisplayName(Name: String; out DisplayName: String): Boolean;
var
  Buffer: PWideChar;
  BufferChars: Cardinal;
  LangId: Cardinal;
begin
  Result := False;
  BufferChars := 0;
  LookupPrivilegeDisplayNameW(nil, PWideChar(Name), nil, BufferChars, LangId);

  if not WinTryCheckBuffer(BufferChars) then
    Exit;

  Buffer := AllocMem((BufferChars + 1) * SizeOf(WideChar));
  try
    if LookupPrivilegeDisplayNameW(nil, PWideChar(Name), Buffer, BufferChars,
      LangId) then
    begin
      SetString(DisplayName, Buffer, BufferChars);
      Result := True;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function DoLookupName(Value: Int64; out Name: String): Boolean;
var
  Buffer: PWideChar;
  BufferChars: Cardinal;
begin
  Result := False;
  BufferChars := 0;
  LookupPrivilegeNameW(nil, Value, nil, BufferChars);

  if not WinTryCheckBuffer(BufferChars) then
    Exit;

  Buffer := AllocMem((BufferChars + 1) * SizeOf(WideChar));
  try
    if LookupPrivilegeNameW(nil, Value, Buffer, BufferChars) then
    begin
      SetString(Name, Buffer, BufferChars);
      Result := True;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

constructor TPrivilegeRec.Create(Value: Int64);
const
  UNKNOWN_PRIV_FMT = 'Unknown privilege %d';
begin
  Self.Value := Value;
  IsChecked := True;
  DisplayName := '';

  if DoLookupName(Value, Name) then
  begin
    IsValid := True;
    DoLookupDisplayName(Name, DisplayName);
  end
  else
  begin
    IsValid := False;
    Name := Format(UNKNOWN_PRIV_FMT, [Value]);
  end;
end;

class function TPrivilegeCache.AllPrivileges: TPrivilegeRecArray;
var
  i, j, Count: Integer;
begin
  // TODO: Switch to LsaEnumeratePrivileges (POLICY_VIEW_LOCAL_INFORMATION)

  // Make sure that all privileges in the cache are already looked up
  for i := Low(Cache) to High(Cache) do
    if not Cache[i].IsChecked then
      Cache[i].Create(i);

  // Count availible privileges in usual cache
  Count := 0;
  for i := Low(Cache) to High(Cache) do
    if Cache[i].IsValid then
      Inc(Count);

  // Save valid privileges
  j := 0;
  SetLength(Result, Count);
  for i := Low(Cache) to High(Cache) do
    if Cache[i].IsValid then
    begin
      Result[j] := Cache[i];
      Inc(j);
    end;

  // And add custom ones
  Result := Concat(Result, CacheEx);
end;

class function TPrivilegeCache.Lookup(Value: Int64): TPrivilegeRec;
var
  i: Integer;
begin
  // Normally all privileges on the system should be within the range of Cache
  // array. However, as a backup plan we have a dynamically allocated CacheEx
  // that will hold all custom privileges.

  // If the privilege is in the range then use the usual cache.
  if (Low(Cache) <= Value) and (Value <= High(Cache)) then
  begin
    // Lookup the privilege if necessary
    if not Cache[Value].IsChecked then
      Cache[Value].Create(Value);

    Result := Cache[Value];
  end
  else
  begin
    // Preferably, this code should never be called since it is a backup plan
    // for encounting a strange privilege that is not in the usual system range.
    // This also migh happen if Microsoft adds a new privilege. In this case
    // increase SE_MAX_WELL_KNOWN_PRIVILEGE to place it in the usual cache.

    // Try to find it in the custom list
    for i := 0 to High(CacheEx) do
      if CacheEx[i].Value = Value then
      begin
        Result := CacheEx[i];
        Exit;
      end;

    // Allocate a new entry and lookup this privilege
    SetLength(CacheEx, Length(CacheEx) + 1);
    CacheEx[High(CacheEx)].Create(Value);
    Result := CacheEx[High(CacheEx)];
  end;
end;

end.
