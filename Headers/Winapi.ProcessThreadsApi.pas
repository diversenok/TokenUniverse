unit Winapi.ProcessThreadsApi;

interface

uses
  Winapi.WinNt, Winapi.WinBase;

const
  // WinBase.573
  DEBUG_PROCESS = $00000001;
  DEBUG_ONLY_THIS_PROCESS = $00000002;
  CREATE_SUSPENDED = $00000004;
  DETACHED_PROCESS = $00000008;
  CREATE_NEW_CONSOLE = $00000010;
  CREATE_NEW_PROCESS_GROUP = $00000200;
  CREATE_UNICODE_ENVIRONMENT = $00000400;
  CREATE_PROTECTED_PROCESS = $00040000;
  EXTENDED_STARTUPINFO_PRESENT = $00080000;
  CREATE_SECURE_PROCESS = $00400000;
  CREATE_BREAKAWAY_FROM_JOB = $01000000;
  CREATE_DEFAULT_ERROR_MODE = $04000000;
  CREATE_NO_WINDOW = $08000000;
  PROFILE_USER = $10000000;
  PROFILE_KERNEL = $20000000;
  PROFILE_SERVER = $40000000;
  CREATE_IGNORE_SYSTEM_DEFAULT = $80000000;

  // WinBase.3010
  STARTF_USESHOWWINDOW = $00000001;
  STARTF_USESIZE = $00000002;
  STARTF_USEPOSITION = $00000004;
  STARTF_FORCEONFEEDBACK = $00000040;
  STARTF_FORCEOFFFEEDBACK = $00000080;
  STARTF_USESTDHANDLES = $00000100;
  STARTF_UNTRUSTEDSOURCE = $00008000;

  // WinBase.3398
  PROC_THREAD_ATTRIBUTE_PARENT_PROCESS = $20000;

  // WinBase.7268
  LOGON_WITH_PROFILE = $00000001;
  LOGON_NETCREDENTIALS_ONLY = $00000002;
  LOGON_ZERO_PASSWORD_BUFFER = $80000000;

type
  // 28
  TProcessInformation = record
    hProcess: THandle;
    hThread: THandle;
    dwProcessId: Cardinal;
    dwThreadId: Cardinal;
  end;
  PProcessInformation = ^TProcessInformation;

  // 55
  TStartupInfoW = record
    cb: Cardinal;
    lpReserved: PWideChar;
    lpDesktop: PWideChar;
    lpTitle: PWideChar;
    dwX: Cardinal;
    dwY: Cardinal;
    dwXSize: Cardinal;
    dwYSize: Cardinal;
    dwXCountChars: Cardinal;
    dwYCountChars: Cardinal;
    dwFillAttribute: Cardinal;
    dwFlags: Cardinal;
    wShowWindow: Word;
    cbReserved2: Word;
    lpReserved2: PByte;
    hStdInput: THandle;
    hStdOutput: THandle;
    hStdError: THandle;
  end;
  PStartupInfoW = ^TStartupInfoW;

  // 573
  PProcThreadAttributeList = Pointer;

  // WinBase.3038
  TStartupInfoExW = record
    StartupInfo: TStartupInfoW;
    lpAttributeList: PProcThreadAttributeList;
  end;
  PStartupInfoExW = ^TStartupInfoExW;

// 377
function CreateProcessW(lpApplicationName: PWideChar;
  lpCommandLine: PWideChar; lpProcessAttributes: PSecurityAttributes;
  lpThreadAttributes: PSecurityAttributes; bInheritHandles: LongBool;
  dwCreationFlags: Cardinal; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const StartupInfo: TStartupInfoExW;
  out ProcessInformation: TProcessInformation): LongBool; stdcall;
  external kernel32;

// 422
procedure GetStartupInfoW(out StartupInfo: TStartupInfoW); stdcall;
  external kernel32;

// 433
function CreateProcessAsUserW(hToken: THandle; lpApplicationName: PWideChar;
  lpCommandLine: PWideChar; lpProcessAttributes: PSecurityAttributes;
  lpThreadAttributes: PSecurityAttributes; bInheritHandles: LongBool;
  dwCreationFlags: Cardinal; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const StartupInfo: TStartupInfoExW;
  out ProcessInformation: TProcessInformation): LongBool; stdcall;
  external advapi32;

// 637
function InitializeProcThreadAttributeList(
  lpAttributeList: PProcThreadAttributeList; dwAttributeCount: Cardinal;
  dwFlags: Cardinal; var lpSize: NativeUInt): LongBool; stdcall;
  external kernel32;

// 648
procedure DeleteProcThreadAttributeList(
  lpAttributeList: PProcThreadAttributeList); stdcall; external kernel32;

// 678
function UpdateProcThreadAttribute(
  lpAttributeList: PProcThreadAttributeList; dwFlags: Cardinal;
  Attribute: NativeUInt; lpValue: Pointer; cbSize: NativeUInt;
  lpPreviousValue: Pointer = nil; lpReturnSize: PNativeUInt = nil): LongBool;
  stdcall; external kernel32;

// WinBase.7276
function CreateProcessWithLogonW(lpUsername: PWideChar;
  lpDomain: PWideChar; lpPassword: PWideChar; dwLogonFlags: Cardinal;
  pApplicationName: PWideChar; lpCommandLine: PWideChar;
  dwCreationFlags: Cardinal; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const StartupInfo: TStartupInfoExW;
  out ProcessInformation: TProcessInformation): LongBool; stdcall;
  external advapi32;

// WinBase.7293
function CreateProcessWithTokenW(hToken: THandle; dwLogonFlags: Cardinal;
  pApplicationName: PWideChar; lpCommandLine: PWideChar;
  dwCreationFlags: Cardinal; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const StartupInfo: TStartupInfoExW;
  out ProcessInformation: TProcessInformation): LongBool; stdcall;
  external advapi32;

implementation

end.
