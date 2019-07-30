unit Winapi.Svc;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt;

const
  // 88
  SERVICE_NO_CHANGE = Cardinal(-1);

  // 138
  SERVICE_ACCEPT_STOP = $00000001;
  SERVICE_ACCEPT_PAUSE_CONTINUE = $00000002;
  SERVICE_ACCEPT_SHUTDOWN = $00000004;
  SERVICE_ACCEPT_PARAMCHANGE = $00000008;
  SERVICE_ACCEPT_NETBINDCHANGE = $00000010;
  SERVICE_ACCEPT_HARDWAREPROFILECHANGE = $00000020;
  SERVICE_ACCEPT_POWEREVENT = $00000040;
  SERVICE_ACCEPT_SESSIONCHANGE = $00000080;
  SERVICE_ACCEPT_PRESHUTDOWN = $00000100;
  SERVICE_ACCEPT_TIMECHANGE = $00000200;
  SERVICE_ACCEPT_TRIGGEREVENT = $00000400;
  SERVICE_ACCEPT_USER_LOGOFF = $00000800;
  SERVICE_ACCEPT_LOWRESOURCES = $00002000;
  SERVICE_ACCEPT_SYSTEMLOWRESOURCES = $00004000;

  // 157
  SC_MANAGER_CONNECT = $0001;
  SC_MANAGER_CREATE_SERVICE = $0002;
  SC_MANAGER_ENUMERATE_SERVICE = $0004;
  SC_MANAGER_LOCK = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG = $0020;

  SC_MANAGER_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $3F;

  // 177
  SERVICE_QUERY_CONFIG = $0001;
  SERVICE_CHANGE_CONFIG = $0002;
  SERVICE_QUERY_STATUS = $0004;
  SERVICE_ENUMERATE_DEPENDENTS = $0008;
  SERVICE_START = $0010;
  SERVICE_STOP = $0020;
  SERVICE_PAUSE_CONTINUE = $0040;
  SERVICE_INTERROGATE = $0080;
  SERVICE_USER_DEFINED_CONTROL = $0100;

  SERVICE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1FF;

  // WinNt.21364
  SERVICE_WIN32_OWN_PROCESS = $00000010;
  SERVICE_WIN32_SHARE_PROCESS = $00000020;

type
  TScmHandle = NativeUInt;
  TServiceStatusHandle = NativeUInt;
  TScLock = NativeUInt;

  // WinNt.21392
  TServiceStartType = (
    ServiceBootStart = 0,
    ServiceSystemStart = 1,
    ServiceAutoStart = 2,
    ServiceDemandStart = 3,
    ServiceDisabled = 4
  );

  // WinNt.21401
  TServiceErrorControl = (
    ServiceErrorIgnore = 0,
    ServiceErrorNormal = 1,
    ServiceErrorSevere = 2,
    ServiceErrorCritical = 3
  );

  // 101
  TServiceControl = (
    ServiceControlReserved = 0,
    ServiceControlStop = 1,
    ServiceControlPause = 2,
    ServiceControlContinue = 3,
    ServiceControlInterrogate = 4,
    ServiceControlShutdown = 5,
    ServiceControlParamChange = 6,
    ServiceControlNetbindAdd = 7,
    ServiceControlNetbindRemove = 8,
    ServiceControlNetbindEnable = 9,
    ServiceControlNetbindDisable = 10,
    ServiceControlDeviceEvent = 11,
    ServiceControlHardwareProfileChange = 12,
    ServiceControlPowerEvent = 13,
    ServiceControlSessionChange = 14,
    ServiceControlPreshutdown = 15,
    ServiceControlTimeChange = 16,
    ServiceControlUserLogoff = 17
  );

  // 127
  TServiceState = (
    ServiceStopped = 1,
    ServiceStartPending = 2,
    ServiceStopPending = 3,
    ServiceRunning = 4,
    ServiceContinuePending = 5,
    ServicePausePending = 6,
    ServicePaused = 7
  );

  // 206
  TServiceConfigLevel = (
    ServiceConfigDescription = 1,            // PWideChar
    ServiceConfigFailureActions = 2,
    ServiceConfigDelayedAutoStartInfo = 3,   // LongBool
    ServiceConfigFailureActionsFlag = 4,     // LongBool
    ServiceConfigServiceSidInfo = 5,
    ServiceConfigRequiredPrivilegesInfo = 6, // multi-sz
    ServiceConfigPreshutdownInfo = 7,        // Cardinal (timeout in ms)
    ServiceConfigTriggerInfo = 8,
    ServiceConfigPreferredNode = 9,
    ServiceConfigReserved1 = 10,
    ServiceConfigReserved2 = 11,
    ServiceConfigLaunchProtected = 12
  );

  // 306
  TServiceContolLevel = (
    ServiceContolStatusReasonInfo = 1 // TServiceControlStatusReasonParamsW
  );

  // 311
  TServiceSidType = (
    ServiceSidTypeNone = 0,
    ServiceSidTypeUnrestricted = 1,
    ServiceSidTypeRestricted = 3
  );

  // 707
  TScStatusType = (
    ScStatusProcessInfo = 0 // TServiceStatusProcess
  );

  // 723
  TServiceStatus = record
    ServiceType: Cardinal;
    CurrentState: TServiceState;
    ControlsAccepted: Cardinal;
    Win32ExitCode: Cardinal;
    ServiceSpecificExitCode: Cardinal;
    CheckPoint: Cardinal;
    WaitHint: Cardinal;
  end;
  PServiceStatus = ^TServiceStatus;

  // 733
  TServiceStatusProcess = record
    ServiceType: Cardinal;
    CurrentState: TServiceState;
    ControlsAccepted: Cardinal;
    Win32ExitCode: Cardinal;
    ServiceSpecificExitCode: Cardinal;
    CheckPoint: Cardinal;
    WaitHint: Cardinal;
    ProcessId: Cardinal;
    ServiceFlags: Cardinal;
  end;
  PServiceStatusProcess = ^TServiceStatusProcess;

  // 827
  TQueryServiceConfigW = record
    ServiceType: Cardinal;
    StartType: TServiceStartType;
    ErrorControl: TServiceErrorControl;
    BinaryPathName: PWideChar;
    LoadOrderGroup: PWideChar;
    TagId: Cardinal;
    Dependencies: PWideChar;
    ServiceStartName: PWideChar;
    DisplayName: PWideChar;
  end;
  PQueryServiceConfigW = ^TQueryServiceConfigW;

  TServiceArgsW = array [ANYSIZE_ARRAY] of PWideChar;
  PServiceArgsW = ^TServiceArgsW;

  // 868
  TServiceMainFunction = procedure (NumServicesArgs: Integer;
    ServiceArgVectors: PServiceArgsW) stdcall;

  // 893
  TServiceTableEntryW = record
    ServiceName: PWideChar;
    ServiceProc: TServiceMainFunction;
  end;
  PServiceTableEntryW = ^TServiceTableEntryW;

  // 924
  THandlerFunctionEx = function(Control: TServiceControl; EventType: Cardinal;
    EventData: Pointer; Context: Pointer): Cardinal; stdcall;

  // 991
  TServiceControlStatusReasonParamsW = record
    Reason: Cardinal;
    Comment: PWideChar;
    ServiceStatus: TServiceStatusProcess;
  end;
  PServiceControlStatusReasonParamsW = ^TServiceControlStatusReasonParamsW;

// 1041
function ChangeServiceConfigW(hService: TScmHandle; dwServiceType: Cardinal;
  dwStartType: TServiceStartType; dwErrorControl: TServiceErrorControl;
  pBinaryPathName: PWideChar; pLoadOrderGroup: PWideChar; pdwTagId: PCardinal;
  pDependencies: PWideChar; pServiceStartName: PWideChar; pPassword: PWideChar;
  pDisplayName: PWideChar): LongBool; stdcall; external advapi32;

// 1071
function ChangeServiceConfig2W(hService: TScmHandle;
  InfoLevel: TServiceConfigLevel; pInfo: Pointer): LongBool; stdcall;
  external advapi32;

// 1083
function CloseServiceHandle(hSCObject: TScmHandle): LongBool; stdcall;
  external advapi32;

// 1092
function ControlService(hService: TScmHandle; dwControl: TServiceControl;
  out lpServiceStatus: TServiceStatus): LongBool; stdcall; external advapi32;

// 1121
function CreateServiceW(hSCManager: TScmHandle; lpServiceName: PWideChar;
  lpDisplayName: PWideChar; dwDesiredAccess: TAccessMask;
  dwServiceType: Cardinal; dwStartType: TServiceStartType; dwErrorControl:
  TServiceErrorControl; lpBinaryPathName: PWideChar; lpLoadOrderGroup:
  PWideChar; lpdwTagId: PCardinal; lpDependencies: PWideChar;
  lpServiceStartName: PWideChar; lpPassword: PWideChar): TScmHandle; stdcall;
  external advapi32;

// 1145
function DeleteService(hService: TScmHandle): LongBool; stdcall;
  external advapi32;

// 1312
function GetServiceDisplayNameW(hSCManager: TScmHandle;
  lpServiceName: PWideChar; lpDisplayName: PWideChar; var cchBuffer: Cardinal):
  LongBool; stdcall; external advapi32;

// 1334
function LockServiceDatabase(hSCManager: TScmHandle): TScLock; stdcall;
  external advapi32;

// 1364
function OpenSCManagerW(lpMachineName: PWideChar; lpDatabaseName: PWideChar;
  dwDesiredAccess: TAccessMask): TScmHandle; stdcall; external advapi32;

// 1388
function OpenServiceW(hSCManager: TScmHandle; lpServiceName: PWideChar;
  dwDesiredAccess: Cardinal): TScmHandle; stdcall; external advapi32;

// 1414
function QueryServiceConfigW(hService: TScmHandle;
  pServiceConfig: PQueryServiceConfigW; cbBufSize: Cardinal;
  out BytesNeeded: Cardinal): LongBool; stdcall; external advapi32;

// 1457
function QueryServiceConfig2W(hService: TScmHandle;
  InfoLevel: TServiceConfigLevel; Buffer: Pointer; BufSize: Cardinal;
  out BytesNeeded: Cardinal): LongBool; stdcall; external advapi32;

// 1515
function QueryServiceObjectSecurity(hService: TScmHandle; SecurityInformation:
  TSecurityInformation; SecurityDescriptor: PSecurityDescriptor;
  cbBufSize: Cardinal; out cbBytesNeeded: Cardinal): LongBool; stdcall;
  external advapi32;

// 1528
function QueryServiceStatus(hService: TScmHandle;
  out ServiceStatus: TServiceStatus): LongBool; stdcall; external advapi32;

// 1537
function QueryServiceStatusEx(hService: TScmHandle; InfoLevel: TScStatusType;
  Buffer: Pointer; BufSize: Cardinal; out BytesNeeded: Cardinal): LongBool;
  stdcall; external advapi32;

// 1584
function RegisterServiceCtrlHandlerExW(lpServiceName: PWideChar;
  lpHandlerProc: THandlerFunctionEx; lpContext: Pointer): TServiceStatusHandle;
  stdcall; external advapi32;

// 1599
function SetServiceObjectSecurity(hService: TScmHandle;
  SecurityInformation: TSecurityInformation; const SecurityDescriptor:
  TSecurityDescriptor): LongBool; stdcall; external advapi32;

// 1608
function SetServiceStatus(hServiceStatus: TServiceStatusHandle;
  const ServiceStatus: TServiceStatus): LongBool; stdcall; external advapi32;

// 1622
function StartServiceCtrlDispatcherW(lpServiceStartTable: PServiceTableEntryW):
  LongBool; stdcall; external advapi32;

// 1644
function StartServiceW(hService: TScmHandle; dwNumServiceArgs: Cardinal;
  lpServiceArgVectors: TArray<PWideChar>): LongBool; stdcall; external advapi32;

// 1665
function UnlockServiceDatabase(ScLock: TScLock): LongBool; stdcall;
  external advapi32;

// 1711
function ControlServiceExW(hService: TScmHandle; dwControl: TServiceControl;
  InfoLevel: TServiceContolLevel; pControlParams: Pointer): LongBool; stdcall;
  external advapi32;

implementation

end.
