unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, TU.Common, TU.Tokens;

type
  TLogonType = (UndefinedLogonType, Reserved, Interactive, Network, Batch,
    Service, Proxy, Unlock, NetworkCleartext, NewCredentials, RemoteInteractive,
    CachedInteractive, CachedRemoteInteractive, CachedUnlock);

  TSecurotyLogonTypeHelper = record helper for TLogonType
    function ToString: String;
  end;

  TLogonSessionInfo = record
    User: TSecurityIdentifier;
    AuthPackage, LogonServer: String;
    LogonType: TLogonType;
    Session: Cardinal;
    LogonTime: TDateTime;
  end;

function GetLogonSessionInformation(LogonId: LUID): CanFail<TLogonSessionInfo>;

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

  PSecurityLogonSessionData = ^TSecurityLogonSessionData;

function LsaFreeReturnBuffer(Buffer: Pointer): LongWord; stdcall;
  external secur32;

function LsaGetLogonSessionData(var LogonId: LUID;
  out ppLogonSessionData: PSecurityLogonSessionData): LongWord; stdcall;
  external secur32;

function GetLogonSessionInformation(LogonId: LUID): CanFail<TLogonSessionInfo>;
var
  Buffer: PSecurityLogonSessionData;
begin
  Result.Init;

  if Result.CheckNativeError(LsaGetLogonSessionData(LogonId, Buffer),
    'LsaGetLogonSessionData') then
    with Result do
      try
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

{ TSecurotyLogonTypeHelper }

function TSecurotyLogonTypeHelper.ToString: String;
const
  Mapping: array [TLogonType] of String = ('Undefined', 'Reserved',
    'Interactive', 'Network', 'Batch', 'Service', 'Proxy', 'Unlock',
    'Network cleartext', 'New credentials', 'Remote interactive',
    'Cached interactive', 'Cached remote interactive', 'Cached unlock');
begin
  if (Self >= UndefinedLogonType) and (Self <= CachedUnlock) then
    Result := Mapping[Self]
  else
    Result := '(Out of bound)';
end;

end.
