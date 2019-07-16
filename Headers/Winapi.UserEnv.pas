unit Winapi.UserEnv;

{$WARN SYMBOL_PLATFORM OFF}
{$MINENUMSIZE 4}

interface

const
  userenv = 'userenv.dll';

const
  // 172
  PT_TEMPORARY = $00000001;
  PT_ROAMING = $00000002;
  PT_MANDATORY = $00000004;
  PT_ROAMING_PREEXISTING = $00000008;

type
  // profinfo.38
  TProfileInfoW = record
    dwSize: Cardinal;
    dwFlags: Cardinal; // PT_*
    lpUserName: PWideChar;
    lpProfilePath: PWideChar;
    lpDefaultPath: PWideChar;
    lpServerName: PWideChar;
    lpPolicyPath: PWideChar;
    hProfile: THandle;
  end;
  PProfileInfoW = ^TProfileInfoW;

// 80
function LoadUserProfileW(hToken: THandle; var ProfileInfo: TProfileInfoW):
  LongBool; stdcall; external userenv delayed;

// 108
function UnloadUserProfile(hToken: THandle; hProfile: THandle): LongBool;
  stdcall; external userenv delayed;

// 140
function GetProfilesDirectoryW(lpProfileDir: PWideChar; var lpcchSize: Cardinal)
  : LongBool; stdcall; external userenv delayed;

// 180
function GetProfileType(out dwFlags: Cardinal): LongBool; stdcall;
  external userenv delayed;

// 412
function CreateEnvironmentBlock(out Environment: Pointer; hToken: THandle;
  bInherit: LongBool): LongBool; stdcall; external userenv delayed;

implementation

end.
