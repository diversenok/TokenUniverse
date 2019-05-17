unit Winapi.WinUser;

interface

const
  user32 = 'user32.dll';

  // 371
  SW_HIDE = 0;
  SW_SHOWNORMAL = 1;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMAXIMIZED = 3;
  SW_SHOWNOACTIVATE = 4;

type
  HWND = NativeUInt;
  HICON = NativeUInt;
  HWINSTA = NativeUInt;

  WPARAM = NativeUInt;
  LPARAM = NativeInt;

  TDesktopEnumProcW = function (Name: PWideChar; Context: TObject): LongBool;
    stdcall;

// 1480
function EnumDesktopsW(hwinsta: HWINSTA; lpEnumFunc: TDesktopEnumProcW;
  Context: TObject): LongBool; stdcall; external user32;

// 1635
function GetProcessWindowStation: HWINSTA; stdcall; external user32;

implementation

end.
