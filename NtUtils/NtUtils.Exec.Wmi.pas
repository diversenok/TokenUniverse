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
  Winapi.ProcessThreadsApi, NtUtils.Exec.Win32, NtUtils.ApiExtension,
  Ntapi.ntpsapi;

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

function PrepareProcessStartup(ParamSet: IExecProvider): OleVariant;
var
  Flags: Cardinal;
begin
  Result := GetWMIObject('winmgmts:Win32_ProcessStartup');

  // For some reason when specifing Win32_ProcessStartup.CreateFlags
  // processes would not start without CREATE_BREAKAWAY_FROM_JOB.
  Flags := CREATE_BREAKAWAY_FROM_JOB;

  if ParamSet.Provides(ppCreateSuspended) and ParamSet.CreateSuspended then
    Flags := Flags or CREATE_SUSPENDED;

  Result.CreateFlags := Flags;

  if ParamSet.Provides(ppShowWindowMode) then
    Result.ShowWindow := ParamSet.ShowWindowMode;
end;

function PrepareCurrentDir(ParamSet: IExecProvider): String;
begin
  if ParamSet.Provides(ppCurrentDirectory) then
    Result := ParamSet.CurrentDircetory
  else
    Result := GetCurrentDir;
end;

{ TExecCallWmi }

procedure TExecCallWmi.Execute(ParamSet: IExecProvider);
var
  objProcess: OleVariant;
  hOldToken: THandle;
  ProcessId: Integer;
begin
  if ParamSet.Provides(ppToken) then
    NtxImpersonateToken(ParamSet.Token, hOldToken).RaiseOnError;

  try
    objProcess := GetWMIObject('winmgmts:Win32_Process');
    objProcess.Create(
      PrepareCommandLine(ParamSet),
      PrepareCurrentDir(ParamSet),
      PrepareProcessStartup(ParamSet),
      ProcessId
    );
  finally
    // Revert impersonation
    if ParamSet.Provides(ppToken) then
      NtSetInformationThread(NtCurrentThread, ThreadImpersonationToken,
        @hOldToken, SizeOf(hOldToken));
  end;
end;

function TExecCallWmi.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppToken, ppCreateSuspended,
    ppShowWindowMode:
      Result := True;
  else
    Result := False;
  end;
end;

end.
