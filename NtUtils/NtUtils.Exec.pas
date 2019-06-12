unit NtUtils.Exec;

interface

uses
  NtUtils.Exceptions, Winapi.ProcessThreadsApi;

type
  TExecParam = (
    ppParameters, ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
    ppLogonFlags, ppInheritHandles, ppCreateSuspended, ppBreakaway,
    ppRequireElevation, ppShowWindowMode
  );

  IExecProvider = interface
    function Provides(Parameter: TExecParam): Boolean;
    function Application: String;
    function Parameters: String;
    function CurrentDircetory: String;
    function Desktop: String;
    function Token: THandle;
    function ParentProcess: THandle;
    function LogonFlags: Cardinal;
    function InheritHandles: Boolean;
    function CreateSuspended: Boolean;
    function Breakaway: Boolean;
    function RequireElevation: Boolean;
    function ShowWindowMode: Word;
  end;

  TProcessInfo = Winapi.ProcessThreadsApi.TProcessInformation;

  IExecMethod = interface
    function Supports(Parameter: TExecParam): Boolean;
    function Execute(ParamSet: IExecProvider): TProcessInfo;
  end;

  TExecParamSet = set of TExecParam;

  TDefaultExecProvider = class(TInterfacedObject, IExecProvider)
  public
    UseParams: TExecParamSet;
    strApplication: String;
    strParameters: String;
    strCurrentDircetory: String;
    strDesktop: String;
    hToken: THandle;
    hParentProcess: THandle;
    dwLogonFlags: Cardinal;
    bInheritHandles: Boolean;
    bCreateSuspended: Boolean;
    bBreakaway: Boolean;
    bRequireElevation: Boolean;
    wShowWindowMode: Word;
  public
    function Provides(Parameter: TExecParam): Boolean; virtual;
    function Application: String; virtual;
    function Parameters: String; virtual;
    function CurrentDircetory: String; virtual;
    function Desktop: String; virtual;
    function Token: THandle; virtual;
    function ParentProcess: THandle; virtual;
    function LogonFlags: Cardinal; virtual;
    function InheritHandles: Boolean; virtual;
    function CreateSuspended: Boolean; virtual;
    function Breakaway: Boolean; virtual;
    function RequireElevation: Boolean; virtual;
    function ShowWindowMode: Word; virtual;
  end;

function PrepareCommandLine(ParamSet: IExecProvider): String;
procedure FreeProcessInfo(var ProcessInfo: TProcessInfo);

implementation

uses
  Winapi.WinUser, NtUtils.Objects;

{ TDefaultExecProvider }

function TDefaultExecProvider.Application: String;
begin
  Result := strApplication;
end;

function TDefaultExecProvider.Breakaway: Boolean;
begin
  if ppBreakaway in UseParams then
    Result := bBreakaway
  else
    Result := False;
end;

function TDefaultExecProvider.CreateSuspended: Boolean;
begin
  if ppCreateSuspended in UseParams then
    Result := bCreateSuspended
  else
    Result := False;
end;

function TDefaultExecProvider.CurrentDircetory: String;
begin
  if ppCurrentDirectory in UseParams then
    Result := strCurrentDircetory
  else
    Result := '';
end;

function TDefaultExecProvider.Desktop: String;
begin
  if ppDesktop in UseParams then
    Result := strDesktop
  else
    Result := '';
end;

function TDefaultExecProvider.InheritHandles: Boolean;
begin
  if ppInheritHandles in UseParams then
    Result := bInheritHandles
  else
    Result := False;
end;

function TDefaultExecProvider.LogonFlags: Cardinal;
begin
  if ppLogonFlags in UseParams then
    Result := dwLogonFlags
  else
    Result := 0;
end;

function TDefaultExecProvider.Parameters: String;
begin
  if ppParameters in UseParams then
    Result := strParameters
  else
    Result := '';
end;

function TDefaultExecProvider.ParentProcess: THandle;
begin
  if ppParentProcess in UseParams then
    Result := hParentProcess
  else
    Result := 0;
end;

function TDefaultExecProvider.Provides(Parameter: TExecParam): Boolean;
begin
  Result := Parameter in UseParams;
end;

function TDefaultExecProvider.RequireElevation: Boolean;
begin
  if ppRequireElevation in UseParams then
    Result := bRequireElevation
  else
    Result := False;
end;

function TDefaultExecProvider.ShowWindowMode: Word;
begin
  if ppShowWindowMode in UseParams then
    Result := wShowWindowMode
  else
    Result := SW_SHOWNORMAL;
end;

function TDefaultExecProvider.Token: THandle;
begin
  if ppToken in UseParams then
    Result := hToken
  else
    Result := 0;
end;

{ Functions }

function PrepareCommandLine(ParamSet: IExecProvider): String;
begin
  Result := '"' + ParamSet.Application + '"';
  if ParamSet.Provides(ppParameters) and (ParamSet.Parameters <> '') then
    Result := Result + ' ' + ParamSet.Parameters;
end;

procedure FreeProcessInfo(var ProcessInfo: TProcessInfo);
begin
  if ProcessInfo.hProcess <> 0 then
    NtxSafeClose(ProcessInfo.hProcess);

  if ProcessInfo.hThread <> 0 then
    NtxSafeClose(ProcessInfo.hThread);
end;

end.
