unit Winapi.Shlwapi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinUser;

const
  shlwapi = 'shlwapi.dll';

  // 2394
  SHACF_URLMRU = $00000004;
  SHACF_FILESYS_ONLY = $00000010;
  SHACF_FILESYS_DIRS = $00000020;

// 2412
function SHAutoComplete(hwndEdit: HWND; dwFlags: Cardinal): HRESULT; stdcall;
  external shlwapi;

implementation

end.
