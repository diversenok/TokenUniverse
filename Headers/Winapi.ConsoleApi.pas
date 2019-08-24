unit Winapi.ConsoleApi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt;

type
  TCtrlEvent = (
    CtrlCEvent = 0,
    CtrlBreakEvent = 1,
    CtrlCloseEvent = 2,
    CtrlLogoffEvent = 5,
    CtrlShutdownEvent = 6
  );

  THandlerRoutine = function (CtrlType: TCtrlEvent): LongBool; stdcall;

function AllocConsole: LongBool; stdcall; external kernel32;

function FreeConsole: LongBool; stdcall; external kernel32;

function AttachConsole(dwProcessId: Cardinal): LongBool; stdcall;
  external kernel32;

function SetConsoleCtrlHandler(HandlerRoutine: THandlerRoutine;
  Add: LongBool): LongBool; stdcall; external kernel32;

implementation

end.
