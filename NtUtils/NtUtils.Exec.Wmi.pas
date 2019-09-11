unit NtUtils.Exec.Wmi;

interface

uses
  NtUtils.Exec;

type
  TExecCallWmi = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    function Execute(ParamSet: IExecProvider): TProcessInfo;
  end;

implementation

uses
  Winapi.ActiveX, System.Win.ComObj, System.SysUtils, Ntapi.ntpsapi,
  Winapi.ProcessThreadsApi, NtUtils.Exec.Win32, NtUtils.Tokens.Impersonate,
  NtUtils.Objects, NtUtils.Exceptions;

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

function TExecCallWmi.Execute(ParamSet: IExecProvider): TProcessInfo;
var
  objProcess: OleVariant;
  hOldToken: THandle;
  ProcessId: Integer;
begin
  if ParamSet.Provides(ppToken) then
  begin
    // Backup current impersonation
    hOldToken := NtxBackupImpersonation(NtCurrentThread);

    // Impersonate the passed token
    NtxImpersonateAnyToken(ParamSet.Token).RaiseOnError;
  end;

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
    begin
      NtxRestoreImpersonation(NtCurrentThread, hOldToken);

      if hOldToken <> 0 then
        NtxSafeClose(hOldToken);
    end;
  end;

  // Only process ID is available to return to the caller
  FillChar(Result, SizeOf(Result), 0);
  Result.dwProcessId := Cardinal(ProcessId);
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
