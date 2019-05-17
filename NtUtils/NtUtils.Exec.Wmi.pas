unit NtUtils.Exec.Wmi;

interface

uses
  NtUtils.Exec;

type
  TExecCallWmi = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    procedure Execute(ParamSet: IExecProvider);
  end;

implementation

uses
  Winapi.ActiveX, System.Win.ComObj, System.SysUtils,
  Winapi.ProcessThreadsApi, NtUtils.Exec.Win32;

function GetWMIObject(const objectName: String): IDispatch;
var
  chEaten: Integer;
  BindCtx: IBindCtx;
  Moniker: IMoniker;
begin
  OleCheck(CreateBindCtx(0, BindCtx));
  OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten,
    Moniker));
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
end;

{ TExecCallWmi }

procedure TExecCallWmi.Execute(ParamSet: IExecProvider);
var
  objProcess: OleVariant;
  objProcessStartup: OleVariant;
  CurrentDir: String;
  ProcessId: Integer;
begin

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := ParamSet.CurrentDircetory
  else
    CurrentDir := GetCurrentDir;

  objProcessStartup := GetWMIObject('winmgmts:Win32_ProcessStartup');

  // For some reason when specifing Win32_ProcessStartup.CreateFlags
  // processes would not start without CREATE_BREAKAWAY_FROM_JOB.
  objProcessStartup.CreateFlags := PrepareCreationFlags(ParamSet) or
    CREATE_BREAKAWAY_FROM_JOB;

  if ParamSet.Provides(ppShowWindowMode) then
    objProcessStartup.ShowWindow := ParamSet.ShowWindowMode;

  objProcess := GetWMIObject('winmgmts:Win32_Process');
  objProcess.Create(
    PrepareCommandLine(ParamSet),
    CurrentDir,
    objProcessStartup,
    ProcessId
    );
end;

function TExecCallWmi.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppCreateSuspended, ppShowWindowMode:
      Result := True;
  else
    Result := False;
  end;
end;

end.
