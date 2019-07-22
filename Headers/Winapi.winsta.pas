unit Winapi.winsta;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

const
  winsta = 'winsta.dll';

  // 40
  USERNAME_LENGTH = 20;
  DOMAIN_LENGTH = 17;

  // 58
  WINSTATIONNAME_LENGTH = 32;

  // 805
  LOGONID_CURRENT = Cardinal(-1);
  SERVERNAME_CURRENT = nil;
  SERVER_CURRENT = 0;

type
  TWinStaHandle = NativeUInt;

  TWinStationName = array [0..WINSTATIONNAME_LENGTH] of WideChar;

  // 84
  TWinStationStateClass = (
    State_Active = 0,
    State_Connected = 1,
    State_ConnectQuery = 2,
    State_Shadow = 3,
    State_Disconnected = 4,
    State_Idle = 5,
    State_Listen = 6,
    State_Reset = 7,
    State_Down = 8,
    State_Init = 9
  );

  // 98
  TSessionIdW = record
    SessionId: Cardinal;
    WinStationName: TWinStationName;
    State: TWinStationStateClass;
  end;

  TSessionIdArrayW = array [ANYSIZE_ARRAY] of TSessionIdW;
  PSessionIdArrayW = ^TSessionIdArrayW;

  // 110
  TWinStationInfoClass = (
    WinStationCreateData = 0,                // q:
    WinStationConfiguration = 1,             // q, s:
    WinStationPdParams = 2,                  // q, s:
    WinStationWd = 3,                        // q:
    WinStationPd = 4,                        // q:
    WinStationPrinter = 5,                   // q:
    WinStationClient = 6,                    // q:
    WinStationModules = 7,                   // q:
    WinStationInformation = 8,               // q: TWinStationInformation
    WinStationTrace = 9,                     // s:
    WinStationBeep = 10,                     // s:
    WinStationEncryptionOff = 11,            // s:
    WinStationEncryptionPerm = 12,           // s:
    WinStationNtSecurity = 13,               // s: < anything >
    WinStationUserToken = 14,                // q: TWinStationUserToken
    WinStationUnused1 = 15,
    WinStationVideoData = 16,                // q:
    WinStationInitialProgram = 17,           // s:
    WinStationCd = 18,                       // q:
    WinStationSystemTrace = 19,              // s:
    WinStationVirtualData = 20,              // q:
    WinStationClientData = 21,               // s:
    WinStationSecureDesktopEnter = 22,       // s:
    WinStationSecureDesktopExit = 23,        // s:
    WinStationLoadBalanceSessionTarget = 24, // q:
    WinStationLoadIndicator = 25,            // q:
    WinStationShadowInfo = 26,               // q, s:
    WinStationDigProductId = 27,             // q:
    WinStationLockedState = 28,              // q, s: LongBool
    WinStationRemoteAddress = 29,            // q:
    WinStationIdleTime = 30,                 // q:
    WinStationLastReconnectType = 31,        // q:
    WinStationDisallowAutoReconnect = 32,    // q, s: LongBool
    WinStationMprNotifyInfo = 33,
    WinStationExecSrvSystemPipe = 34,
    WinStationSmartCardAutoLogon = 35,
    WinStationIsAdminLoggedOn = 36,
    WinStationReconnectedFromId = 37,        // q:
    WinStationEffectsPolicy = 38,            // q:
    WinStationType = 39,                     // q:
    WinStationInformationEx = 40,            // q:
    WinStationValidationInf = 41
  );

  // 460
  TProtocolCounters = record
    WdBytes: Cardinal;
    WdFrames: Cardinal;
    WaitForOutBuf: Cardinal;
    Frames: Cardinal;
    Bytes: Cardinal;
    CompressedBytes: Cardinal;
    CompressFlushes: Cardinal;
    Errors: Cardinal;
    Timeouts: Cardinal;
    AsyncFramingError: Cardinal;
    AsyncOverrunError: Cardinal;
    AsyncOverflowError: Cardinal;
    AsyncParityError: Cardinal;
    TdErrors: Cardinal;
    ProtocolType: Word;
    Length: Word;
    Reserved: array [0..99] of Cardinal;
  end;

  // 503
  TCaheStatistics = record
    ProtocolType: Word;
    Length: Word;
    Reserved: array [0..19] of Cardinal;
  end;

  // 515
  TProtocolStatus = record
    Output: TProtocolCounters;
    Input: TProtocolCounters;
    Cache: TCaheStatistics;
    AsyncSignal: Cardinal;
    AsyncSignalMask: Cardinal;
  end;

  // 525
  TWinStationInformation = record
    ConnectState: TWinStationStateClass;
    WinStationName: TWinStationName;
    LogonId: Cardinal;
    ConnectTime: TLargeInteger;
    DisconnectTime: TLargeInteger;
    LastInputTime: TLargeInteger;
    LogonTime: TLargeInteger;
    Status: TProtocolStatus;
    Domain: array [0..DOMAIN_LENGTH] of WideChar;
    UserName: array [0..USERNAME_LENGTH] of WideChar;
    CurrentTime: TLargeInteger;
    function FullUserName: String;
  end;
  PWinStationInformation = ^TWinStationInformation;

  // 541
  TWinStationUserToken = record
    ClientID: TClientID;
    UserToken: THandle;
  end;
  PWinStationUserToken = ^TWinStationUserToken;

// 811
function WinStationFreeMemory(Buffer: Pointer): Boolean; stdcall;
  external winsta;

// 818
function WinStationOpenServerW(ServerName: PWideChar): TWinStaHandle; stdcall;
  external winsta;

// 825
function WinStationCloseServer(hServer: TWinStaHandle): Boolean; stdcall;
  external winsta;

// 881
function WinStationEnumerateW(ServerHandle: TWinStaHandle; out SessionIds:
  PSessionIdArrayW; out Count: Integer): Boolean; stdcall; external winsta;

// 891
function WinStationQueryInformationW(ServerHandle: TWinStaHandle;
  SessionId: Cardinal; WinStationInformationClass: TWinStationInfoClass;
  pWinStationInformation: Pointer; WinStationInformationLength: Cardinal;
  out ReturnLength: Cardinal): Boolean; stdcall; external winsta;

// 922
function WinStationSendMessageW(ServerHandle: TWinStaHandle;
  SessionId: Cardinal; Title: PWideChar; TitleLength: Cardinal;
  MessageStr: PWideChar; MessageLength: Cardinal;
  Style: Cardinal; Timeout: Cardinal; out Response: Cardinal;
  DoNotWait: Boolean): Boolean; stdcall; external winsta;

// 937
function WinStationConnectW(ServerHandle: TWinStaHandle; SessionId: Cardinal;
  TargetSessionId: Cardinal; pPassword: PWideChar; bWait: Boolean): Boolean;
  stdcall; external winsta;

// 947
function WinStationDisconnect(ServerHandle: TWinStaHandle; SessionId: Cardinal;
  bWait: Boolean): Boolean; stdcall; external winsta;

// 965
function WinStationShadow(ServerHandle: TWinStaHandle;
  TargetServerName: PWideChar; TargetSessionId: Cardinal;
  HotKeyVk: Byte; HotkeyModifiers: Word): Boolean; stdcall; external winsta;

// 976
function WinStationShadowStop(ServerHandle: TWinStaHandle; SessionId: Cardinal;
  bWait: Boolean): Boolean; stdcall; external winsta;

// 1037
function WinStationSwitchToServicesSession: Boolean; stdcall; external winsta;

// 1044
function WinStationRevertFromServicesSession: Boolean; stdcall; external winsta;

implementation

{ TWinStationInformation }

function TWinStationInformation.FullUserName: String;
begin
  if (Domain = '') and (UserName = '') then
    Result := 'No user'
  else if Domain = '' then
    Result := UserName
  else if UserName = '' then
    Result := Domain
  else
    Result := String(Domain) + '\' + String(UserName);
end;

end.
