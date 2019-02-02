unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, TU.Common, TU.Tokens.Types, Ntapi.ntseapi;

type
  TLogonType = (ltSystem, ltReserved, ltInteractive, ltNetwork, ltBatch,
    ltService, ltProxy, ltUnlock, ltNetworkCleartext, ltNewCredentials,
    ltRemoteInteractive, ltCachedInteractive, ltCachedRemoteInteractive,
    ltCachedUnlock);

  TLogonProvider = (lpDefault, lpWinNT35, lpWinNT40, lpWinNT50, lpVirtual);

  /// <summary>
  ///  Stores the information about a logon session.
  /// </summary>
  TLogonSessionInfo = record
    /// <remarks>
    ///  This field is valid only if <see cref="UserPresent"/> is true.
    /// </remarks>
    User: TSecurityIdentifier;
    UserPresent: Boolean;

    AuthPackage, LogonServer: String;
    LogonType: TLogonType;
    Session: Cardinal;
    LogonTime: TDateTime;
  end;

  TLuidDynArray = array of LUID;

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

function LogonTypeToString(LogonType: TLogonType): String;
function QueryLogonSession(LogonId: LUID): CanFail<TLogonSessionInfo>;
function EnumerateLogonSessions: TLuidDynArray;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, System.SysUtils;

const
  secur32 = 'secur32.dll';

type
  TLsaUnicodeString = UNICODE_STRING;

  TSecurityLogonSessionData = record
    Size: Cardinal;
    LogonId: LUID;
    UserName: TLsaUnicodeString;
    LogonDomain: TLsaUnicodeString;
    AuthenticationPackage: TLsaUnicodeString;
    LogonType: Cardinal;
    Session: Cardinal;
    Sid: PSID;
    LogonTime: TLargeInteger;
    LogonServer: TLsaUnicodeString;
    DnsDomainName: TLsaUnicodeString;
    Upn: TLsaUnicodeString;
  end;

  TLuidArray = array [Word] of LUID;
  PLuidArray = ^TLuidArray;

  PSecurityLogonSessionData = ^TSecurityLogonSessionData;

function LsaFreeReturnBuffer(Buffer: Pointer): LongWord; stdcall;
  external secur32;

function LsaGetLogonSessionData(var LogonId: LUID;
  out ppLogonSessionData: PSecurityLogonSessionData): LongWord; stdcall;
  external secur32;

function LsaEnumerateLogonSessions(out LogonSessionCount: Integer;
  out LogonSessionList: PLuidArray): LongWord; stdcall; external secur32;

function QueryLogonSession(LogonId: LUID): CanFail<TLogonSessionInfo>;
var
  Buffer: PSecurityLogonSessionData;
begin
  Result.Init;

  // Query the information
  if Result.CheckNativeError(LsaGetLogonSessionData(LogonId, Buffer),
    'LsaGetLogonSessionData') then
    with Result do
      try
        // The Sid field might be null if we have no permissions to query it.
        Value.UserPresent := Buffer.Sid <> nil;
        if Value.UserPresent then
          Value.User := TSecurityIdentifier.CreateFromSid(Buffer.Sid);

        Value.AuthPackage := Buffer.AuthenticationPackage.ToString;
        Value.LogonType := TLogonType(Buffer.LogonType);
        Value.Session := Buffer.Session;
        Value.LogonTime := NativeTimeToLocalDateTime(Buffer.LogonTime);
        Value.LogonServer := Buffer.LogonServer.ToString;
      finally
        LsaFreeReturnBuffer(Buffer);
      end;
end;

function EnumerateLogonSessions: TLuidDynArray;
var
  Count, i: Integer;
  Sessions: PLuidArray;
begin
  SetLength(Result, 0);

  if NT_SUCCESS(LsaEnumerateLogonSessions(Count, Sessions)) then
  try
    SetLength(Result, Count);
    for i := 0 to Count - 1 do
      Result[i] := Sessions[i];
  finally
    LsaFreeReturnBuffer(Sessions);
  end;
end;

{ Helper functions }

function LogonTypeToString(LogonType: TLogonType): String;
const
  Mapping: array [TLogonType] of String = ('System', 'Reserved',
    'Interactive', 'Network', 'Batch', 'Service', 'Proxy', 'Unlock',
    'Network clear text', 'New credentials', 'Remote interactive',
    'Cached interactive', 'Cached remote interactive', 'Cached unlock');
begin
  if (LogonType >= Low(TLogonType)) and (LogonType <= High(TLogonType)) then
    Result := Mapping[LogonType]
  else
    Result := '(Out of bound)';
end;

{ TPrivilegeCache }

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
