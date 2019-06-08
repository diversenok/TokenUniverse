unit Winapi.Svc;

interface

uses
  Winapi.WinNt;

const
  // 101
  SERVICE_CONTROL_STOP = $00000001;
  SERVICE_CONTROL_PAUSE = $00000002;
  SERVICE_CONTROL_CONTINUE = $00000003;
  SERVICE_CONTROL_INTERROGATE = $00000004;
  SERVICE_CONTROL_SHUTDOWN = $00000005;
  SERVICE_CONTROL_PARAMCHANGE = $00000006;
  SERVICE_CONTROL_NETBINDADD = $00000007;
  SERVICE_CONTROL_NETBINDREMOVE = $00000008;
  SERVICE_CONTROL_NETBINDENABLE = $00000009;
  SERVICE_CONTROL_NETBINDDISABLE = $0000000A;
  SERVICE_CONTROL_DEVICEEVENT = $0000000B;
  SERVICE_CONTROL_HARDWAREPROFILECHANGE = $0000000C;
  SERVICE_CONTROL_POWEREVENT = $0000000D;
  SERVICE_CONTROL_SESSIONCHANGE = $0000000E;
  SERVICE_CONTROL_PRESHUTDOWN = $0000000F;
  SERVICE_CONTROL_TIMECHANGE = $00000010;
  SERVICE_CONTROL_USER_LOGOFF = $00000011;
  SERVICE_CONTROL_TRIGGEREVENT = $00000020;
  SERVICE_CONTROL_LOWRESOURCES = $00000060;
  SERVICE_CONTROL_SYSTEMLOWRESOURCES = $00000061;

  // 127
  SERVICE_STOPPED = $00000001;
  SERVICE_START_PENDING = $00000002;
  SERVICE_STOP_PENDING = $00000003;
  SERVICE_RUNNING = $00000004;
  SERVICE_CONTINUE_PENDING = $00000005;
  SERVICE_PAUSE_PENDING = $00000006;
  SERVICE_PAUSED = $00000007;

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

  // 311
  SERVICE_SID_TYPE_NONE = $00000000;
  SERVICE_SID_TYPE_UNRESTRICTED = $00000001;
  SERVICE_SID_TYPE_RESTRICTED = $00000003;

  // WinNt.21364
  SERVICE_WIN32_OWN_PROCESS = $00000010;
  SERVICE_WIN32_SHARE_PROCESS = $00000020;

  // WinNt.21392
  SERVICE_BOOT_START = $00000000;
  SERVICE_SYSTEM_START = $00000001;
  SERVICE_AUTO_START = $00000002;
  SERVICE_DEMAND_START = $00000003;
  SERVICE_DISABLED = $00000004;

  // WinNt.21401
  SERVICE_ERROR_IGNORE = $00000000;
  SERVICE_ERROR_NORMAL = $00000001;
  SERVICE_ERROR_SEVERE = $00000002;
  SERVICE_ERROR_CRITICAL = $00000003;

type
  TScmHandle = NativeUInt;
  TServiceStatusHandle = NativeUInt;

  TServiceDynArgsW = array of PWideChar;

  // 723
  TServiceStatus = record
    dwServiceType: Cardinal;
    dwCurrentState: Cardinal;
    dwControlsAccepted: Cardinal;
    dwWin32ExitCode: Cardinal;
    dwServiceSpecificExitCode: Cardinal;
    dwCheckPoint: Cardinal;
    dwWaitHint: Cardinal;
  end;
  PServiceStatus = ^TServiceStatus;

  TServiceArgsW = array [Byte] of PWideChar;
  PServiceArgsW = ^TServiceArgsW;

  // 868
  TServiceMainFunction = procedure (dwNumServicesArgs: Cardinal;
    lpServiceArgVectors: PServiceArgsW) stdcall;

  // 893
  TServiceTableEntryW = record
    lpServiceName: PWideChar;
    lpServiceProc: TServiceMainFunction;
  end;
  PServiceTableEntryW = ^TServiceTableEntryW;

  // 924
  THandlerFunctionEx = function(dwControl: Cardinal; dwEventType: Cardinal;
    lpEventData: Pointer; lpContext: Pointer): Cardinal; stdcall;

// 1083
function CloseServiceHandle(hSCObject: TScmHandle): LongBool; stdcall;
  external advapi32;

// 1092
function ControlService(hService: TScmHandle; dwControl: Cardinal;
  out lpServiceStatus: TServiceStatus): LongBool; stdcall; external advapi32;

// 1121
function CreateServiceW(hSCManager: TScmHandle; lpServiceName: PWideChar;
  lpDisplayName: PWideChar; dwDesiredAccess: TAccessMask;
  dwServiceType: Cardinal; dwStartType: Cardinal; dwErrorControl: Cardinal;
  lpBinaryPathName: PWideChar; lpLoadOrderGroup: PWideChar;
  lpdwTagId: PCardinal; lpDependencies: PWideChar;
  lpServiceStartName: PWideChar; lpPassword: PWideChar): TScmHandle; stdcall;
  external advapi32;

// 1145
function DeleteService(hService: TScmHandle): LongBool; stdcall;
  external advapi32;

// 1312
function GetServiceDisplayNameW(hSCManager: TScmHandle;
  lpServiceName: PWideChar; lpDisplayName: PWideChar; var cchBuffer: Cardinal):
  LongBool; stdcall; external advapi32;

// 1364
function OpenSCManagerW(lpMachineName: PWideChar; lpDatabaseName: PWideChar;
  dwDesiredAccess: TAccessMask): TScmHandle; stdcall; external advapi32;

// 1388
function OpenServiceW(hSCManager: TScmHandle; lpServiceName: PWideChar;
  dwDesiredAccess: Cardinal): TScmHandle; stdcall; external advapi32;

// 1515
function QueryServiceObjectSecurity(hService: TScmHandle; SecurityInformation:
  TSecurityInformation; SecurityDescriptor: PSecurityDescriptor;
  cbBufSize: Cardinal; out cbBytesNeeded: Cardinal): LongBool; stdcall;
  external advapi32;

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
  lpServiceArgVectors: TServiceDynArgsW): LongBool; stdcall; external advapi32;

implementation

end.
