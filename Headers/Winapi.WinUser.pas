unit Winapi.WinUser;

interface
{$MINENUMSIZE 4}

uses
  Winapi.WinNt;

const
  user32 = 'user32.dll';

  // 371
  SW_HIDE = 0;
  SW_SHOWNORMAL = 1;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMAXIMIZED = 3;
  SW_SHOWNOACTIVATE = 4;

  // 1353
  DESKTOP_READOBJECTS = $0001;
  DESKTOP_CREATEWINDOW = $0002;
  DESKTOP_CREATEMENU = $0004;
  DESKTOP_HOOKCONTROL = $0008;
  DESKTOP_JOURNALRECORD = $0010;
  DESKTOP_JOURNALPLAYBACK = $0020;
  DESKTOP_ENUMERATE = $0040;
  DESKTOP_WRITEOBJECTS = $0080;
  DESKTOP_SWITCHDESKTOP = $0100;

  // 1533
  WINSTA_ENUMDESKTOPS = $0001;
  WINSTA_READATTRIBUTES = $0002;
  WINSTA_ACCESSCLIPBOARD = $0004;
  WINSTA_CREATEDESKTOP = $0008;
  WINSTA_WRITEATTRIBUTES = $0010;
  WINSTA_ACCESSGLOBALATOMS = $0020;
  WINSTA_EXITWINDOWS = $0040;
  WINSTA_ENUMERATE = $0100;
  WINSTA_READSCREEN = $0200;

type
  HWND = NativeUInt;
  HICON = NativeUInt;
  HDESK = NativeUInt;
  HWINSTA = NativeUInt;

  WPARAM = NativeUInt;
  LPARAM = NativeInt;

  TStringArray = array of String;
  TStringEnumProcW = function (Name: PWideChar; var Context: TStringArray):
    LongBool; stdcall;

  TUserObjectInfoClass = (
    UserObjectFlags = 1,    // q, s:
    UserObjectName = 2,     // q: PWideChar
    UserObjectType = 3,     // q: PWideChar
    UserObjectUserSid = 4,  // q: SID
    UserObjectHeapSize = 5, // q: Cardinal
    UserObjectIO = 6        // q: LongBool
  );

// Desktops

// 1450
function OpenDesktopW(pszDesktop: PWideChar; dwFlags: Cardinal;
  fInherit: LongBool; DesiredAccess: TAccessMask): HDESK; stdcall;
  external user32;

// 1480
function EnumDesktopsW(hWinStation: HWINSTA; lpEnumFunc: TStringEnumProcW;
  var Context: TStringArray): LongBool; stdcall; external user32;

// 1515
function CloseDesktop(hDesktop: HDESK): LongBool; stdcall; external user32;

// 1521
function GetThreadDesktop(dwThreadId: Cardinal): HDESK; stdcall;
  external user32;

// Window Stations

// 1592
function OpenWindowStationW(pszWinSta: PWideChar; fInherit: LongBool;
  DesiredAccess: TAccessMask): HWINSTA; stdcall; external user32;

// 1611
function EnumWindowStationsW(lpEnumFunc: TStringEnumProcW; var Context:
  TStringArray): LongBool; stdcall; external user32;

// 1623
function CloseWindowStation(hWinStation: HWINSTA): LongBool; stdcall;
  external user32;

// 1635
function GetProcessWindowStation: HWINSTA; stdcall; external user32;

// User objects

// 1700
function GetUserObjectInformationW(hObj: THandle;
  InfoClass: TUserObjectInfoClass; pvInfo: Pointer; nLength: Cardinal;
  pnLengthNeeded: PCardinal): LongBool; stdcall; external user32;

implementation

end.
