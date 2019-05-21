unit NtUtils.Exec.Wmi;

interface

uses
  NtUtils.Exec;

type
  TExecCallWmi = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean; virtual;
    procedure Execute(ParamSet: IExecProvider); virtual;
  end;

  TExecCallWmiImpersonated = class(TExecCallWmi, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean; override;
    procedure Execute(ParamSet: IExecProvider); override;
  end;

implementation

uses
  Winapi.ActiveX, System.Win.ComObj, System.SysUtils,
  Winapi.ProcessThreadsApi, NtUtils.Exec.Win32, NtUtils.Exceptions,
  Ntapi.ntdef, Ntapi.ntseapi, Ntapi.ntpsapi;

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
begin
  Result := GetWMIObject('winmgmts:Win32_ProcessStartup');

  // For some reason when specifing Win32_ProcessStartup.CreateFlags
  // processes would not start without CREATE_BREAKAWAY_FROM_JOB.
  Result.CreateFlags := PrepareCreationFlags(ParamSet) or
    CREATE_BREAKAWAY_FROM_JOB;

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
  ProcessId: Integer;
begin
  objProcess := GetWMIObject('winmgmts:Win32_Process');
  objProcess.Create(
    PrepareCommandLine(ParamSet),
    PrepareCurrentDir(ParamSet),
    PrepareProcessStartup(ParamSet),
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

{ TExecCallWmiImpersonated }

procedure TExecCallWmiImpersonated.Execute(ParamSet: IExecProvider);
var
  hOldToken, hNewToken: THandle;
begin
  // Save old impersonation context
  if not NT_SUCCESS(NtOpenThreadTokenEx(NtCurrentThread, TOKEN_IMPERSONATE,
    True, 0, hOldToken)) then
    hOldToken := 0;

  if ParamSet.Provides(ppToken) then
    hNewToken := ParamSet.Token
  else
    hNewToken := 0;

  NativeCheck(NtSetInformationThread(NtCurrentThread, ThreadImpersonationToken,
    @hNewToken, SizeOf(hNewToken)), 'NtSetInformationThread');

  try
    inherited;
  finally
    // Revert impersonation
    NtSetInformationThread(NtCurrentThread, ThreadImpersonationToken,
      @hOldToken, SizeOf(hOldToken));
  end;
end;

function TExecCallWmiImpersonated.Supports(Parameter: TExecParam): Boolean;
begin
  if Parameter = ppToken then
    Result := True
  else
    Result := inherited;
end;

end.
