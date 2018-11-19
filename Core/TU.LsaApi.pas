unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, TU.Common, TU.Tokens.Types;

type
  TLogonType = (ltUndefined, ltReserved, ltInteractive, ltNetwork, ltBatch,
    ltService, ltProxy, ltUnlock, ltNetworkCleartext, ltNewCredentials,
    ltRemoteInteractive, ltCachedInteractive, ltCachedRemoteInteractive,
    ltCachedUnlock);

  TLogonProvider = (lpDefault, lpWinNT50, lpWinNT40, lpWinNT35);

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

function LogonTypeToString(LogonType: TLogonType): String;
function QueryLogonSession(LogonId: LUID): CanFail<TLogonSessionInfo>;
function EnumerateLogonSessions: TLuidDynArray;

implementation

uses
  TU.NativeApi;

const
  secur32 = 'secur32.dll';

type
  TLsaUnicodeString = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;

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

        SetString(Value.AuthPackage, Buffer.AuthenticationPackage.Buffer,
          Buffer.AuthenticationPackage.Length div SizeOf(WideChar));
        Value.LogonType := TLogonType(Buffer.LogonType);
        Value.Session := Buffer.Session;
        Value.LogonTime := NativeTimeToLocalDateTime(Buffer.LogonTime);
        SetString(Value.LogonServer, Buffer.LogonServer.Buffer,
          Buffer.LogonServer.Length div SizeOf(WideChar));
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
  Mapping: array [TLogonType] of String = ('Undefined', 'Reserved',
    'Interactive', 'Network', 'Batch', 'Service', 'Proxy', 'Unlock',
    'Network clear text', 'New credentials', 'Remote interactive',
    'Cached interactive', 'Cached remote interactive', 'Cached unlock');
begin
  if (LogonType >= Low(TLogonType)) and (LogonType <= High(TLogonType)) then
    Result := Mapping[LogonType]
  else
    Result := '(Out of bound)';
end;

end.
