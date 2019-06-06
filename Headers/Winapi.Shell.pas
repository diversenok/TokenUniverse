unit Winapi.Shell;

interface

uses
  Winapi.WinUser;

const
  shell32 = 'shell32.dll';

  SEE_MASK_DEFAULT = $00000000;
  SEE_MASK_NOCLOSEPROCESS = $00000040;
  SEE_MASK_NOASYNC = $00000100;
  SEE_MASK_FLAG_NO_UI = $00000400;
  SEE_MASK_UNICODE = $000004000;
  SEE_MASK_NOZONECHECKS = $00800000;

type
  TShellExecuteInfoW = record
    cbSize: Cardinal;
    fMask: Cardinal;
    Wnd: HWND;
    lpVerb: PWideChar;
    lpFile: PWideChar;
    lpParameters: PWideChar;
    lpDirectory: PWideChar;
    nShow: Integer;
    hInstApp: HINST;
    lpIDList: Pointer;
    lpClass: PWideChar;
    hkeyClass: THandle;
    dwHotKey: Cardinal;
    hMonitor: THandle;
    hProcess: THandle;
  end;

function ShellExecuteExW(var ExecInfo: TShellExecuteInfoW): LongBool; stdcall;
  external shell32;

function ExtractIconExW(lpszFile: PWideChar; nIconIndex: Integer;
  var phiconLarge, phiconSmall: HICON; nIcons: Cardinal): Cardinal; stdcall;
  external shell32;

implementation

end.
