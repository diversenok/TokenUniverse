unit Winapi.winsta;

interface
{$MINENUMSIZE 4}

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

  TSessionIdArrayW = array [Word] of TSessionIdW;
  PSessionIdArrayW = ^TSessionIdArrayW;

  // 110
  TWinStationInfoClass = (
    WinStationCreateData,
    WinStationConfiguration,
    WinStationPdParams,
    WinStationWd,
    WinStationPd,
    WinStationPrinter,
    WinStationClient,
    WinStationModules,
    WinStationInformation = 8, // TWinStationInformation
    WinStationTrace,
    WinStationBeep,
    WinStationEncryptionOff,
    WinStationEncryptionPerm,
    WinStationNtSecurity,
    WinStationUserToken = 14, // TWinStationUserToken
    WinStationUnused1,
    WinStationVideoData,
    WinStationInitialProgram,
    WinStationCd,
    WinStationSystemTrace,
    WinStationVirtualData,
    WinStationClientData,
    WinStationSecureDesktopEnter,
    WinStationSecureDesktopExit,
    WinStationLoadBalanceSessionTarget,
    WinStationLoadIndicator,
    WinStationShadowInfo,
    WinStationDigProductId,
    WinStationLockedState,
    WinStationRemoteAddress,
    WinStationIdleTime,
    WinStationLastReconnectType,
    WinStationDisallowAutoReconnect,
    WinStationMprNotifyInfo,
    WinStationExecSrvSystemPipe,
    WinStationSmartCardAutoLogon,
    WinStationIsAdminLoggedOn,
    WinStationReconnectedFromId,
    WinStationEffectsPolicy,
    WinStationType,
    WinStationInformationEx,
    WinStationValidationInf
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
