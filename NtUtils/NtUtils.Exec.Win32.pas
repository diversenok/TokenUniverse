unit NtUtils.Exec.Win32;

interface

uses
  NtUtils.Exec, Winapi.ProcessThreadsApi, NtUtils.Exceptions;

type
  TExecCreateProcessAsUser = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    function Execute(ParamSet: IExecProvider): TProcessInfo;
  end;

  TExecCreateProcessWithToken = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    function Execute(ParamSet: IExecProvider): TProcessInfo;
  end;

  IStartupInfo = interface
    function StartupInfoEx: PStartupInfoExW;
    function CreationFlags: Cardinal;
    function HasExtendedAttbutes: Boolean;
  end;

  TStartupInfoHolder = class(TInterfacedObject, IStartupInfo)
  strict protected
    SIEX: TStartupInfoExW;
    strDesktop: String;
    hParent: THandle;
    dwCreationFlags: Cardinal;
    procedure PrepateAttributes(ParamSet: IExecProvider; Method: IExecMethod);
  public
    constructor Create(ParamSet: IExecProvider; Method: IExecMethod);
    function StartupInfoEx: PStartupInfoExW;
    function CreationFlags: Cardinal;
    function HasExtendedAttbutes: Boolean;
    destructor Destroy; override;
  end;

  TRunAsInvoker = class(TInterfacedObject, IInterface)
  private const
    COMPAT_NAME = '__COMPAT_LAYER';
    COMPAT_VALUE = 'RunAsInvoker';
  private var
    OldValue: String;
    OldValuePresent: Boolean;
  public
    constructor SetCompatState(Enabled: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  Winapi.WinError, Ntapi.ntobapi, Ntapi.ntstatus, NtUtils.Environment;

{ TStartupInfoHolder }

constructor TStartupInfoHolder.Create(ParamSet: IExecProvider;
  Method: IExecMethod);
begin
  GetStartupInfoW(SIEX.StartupInfo);
  SIEX.StartupInfo.dwFlags := 0;

  if Method.Supports(ppDesktop) and ParamSet.Provides(ppDesktop) then
  begin
    // Store the string in our memory before we reference it as PWideChar
    strDesktop := ParamSet.Desktop;
    SIEX.StartupInfo.lpDesktop := PWideChar(strDesktop);
  end;

  if Method.Supports(ppShowWindowMode) and ParamSet.Provides(ppShowWindowMode) then
  begin
    SIEX.StartupInfo.dwFlags := SIEX.StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
    SIEX.StartupInfo.wShowWindow := ParamSet.ShowWindowMode
  end;

  if Method.Supports(ppCreateSuspended) and ParamSet.Provides(ppCreateSuspended)
    and ParamSet.CreateSuspended then
    dwCreationFlags := dwCreationFlags or CREATE_SUSPENDED;

  if Method.Supports(ppBreakaway) and ParamSet.Provides(ppBreakaway) and
    ParamSet.Breakaway then
    dwCreationFlags := dwCreationFlags or CREATE_BREAKAWAY_FROM_JOB;

  if Method.Supports(ppNewConsole) and ParamSet.Provides(ppNewConsole) and
    ParamSet.NewConsole then
    dwCreationFlags := dwCreationFlags or CREATE_NEW_CONSOLE;

  // Extended attributes
  PrepateAttributes(ParamSet, Method);
  if Assigned(SIEX.lpAttributeList) then
  begin
    SIEX.StartupInfo.cb := SizeOf(TStartupInfoExW);
    dwCreationFlags := dwCreationFlags or EXTENDED_STARTUPINFO_PRESENT;
  end
  else
    SIEX.StartupInfo.cb := SizeOf(TStartupInfoW);
end;

function TStartupInfoHolder.CreationFlags: Cardinal;
begin
  Result := dwCreationFlags;
end;

destructor TStartupInfoHolder.Destroy;
begin
  if Assigned(SIEX.lpAttributeList) then
  begin
    DeleteProcThreadAttributeList(SIEX.lpAttributeList);
    FreeMem(SIEX.lpAttributeList);
    SIEX.lpAttributeList := nil;
  end;
  inherited;
end;

function TStartupInfoHolder.HasExtendedAttbutes: Boolean;
begin
  Result := Assigned(SIEX.lpAttributeList);
end;

procedure TStartupInfoHolder.PrepateAttributes(ParamSet: IExecProvider;
  Method: IExecMethod);
var
  BufferSize: NativeUInt;
begin
  if Method.Supports(ppParentProcess) and ParamSet.Provides(ppParentProcess)
    then
  begin
    hParent := ParamSet.ParentProcess;

    BufferSize := 0;
    InitializeProcThreadAttributeList(nil, 1, 0, BufferSize);
    if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BufferSize = 0) then
      Exit;

    SIEX.lpAttributeList := AllocMem(BufferSize);
    if not InitializeProcThreadAttributeList(SIEX.lpAttributeList, 1, 0,
      BufferSize) then
    begin
      FreeMem(SIEX.lpAttributeList);
      SIEX.lpAttributeList := nil;
      Exit;
    end;

    // NOTE: ProcThreadAttributeList stores pointers istead of storing the
    // data. By referencing the value in the object's field we make sure it
    // does not go anywhere.
    if not UpdateProcThreadAttribute(SIEX.lpAttributeList, 0,
      PROC_THREAD_ATTRIBUTE_PARENT_PROCESS, @hParent, SizeOf(hParent)) then
    begin
      DeleteProcThreadAttributeList(SIEX.lpAttributeList);
      FreeMem(SIEX.lpAttributeList);
      SIEX.lpAttributeList := nil;
      Exit;
    end;
  end
  else
    SIEX.lpAttributeList := nil;
end;

function TStartupInfoHolder.StartupInfoEx: PStartupInfoExW;
begin
  Result := @SIEX;
end;

{ TExecCreateProcessAsUser }

function TExecCreateProcessAsUser.Execute(ParamSet: IExecProvider):
  TProcessInfo;
var
  CommandLine: String;
  CurrentDir: PWideChar;
  Startup: IStartupInfo;
  RunAsInvoker: IInterface;
begin
  // Command line should be in writable memory
  CommandLine := PrepareCommandLine(ParamSet);

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  // Set RunAsInvoker compatibility mode. It will be reverted
  // after exiting from the current function.
  if ParamSet.Provides(ppRunAsInvoker) then
    RunAsInvoker := TRunAsInvoker.SetCompatState(ParamSet.RunAsInvoker);

  Startup := TStartupInfoHolder.Create(ParamSet, Self);

  WinCheck(CreateProcessAsUserW(
    ParamSet.Token, // Zero to fall back to CreateProcessW behavior
    PWideChar(ParamSet.Application),
    PWideChar(CommandLine),
    nil,
    nil,
    ParamSet.Provides(ppInheritHandles) and ParamSet.InheritHandles,
    Startup.CreationFlags,
    nil,
    CurrentDir,
    Startup.StartupInfoEx,
    Result
    ), 'CreateProcessAsUserW'
  );

  // The caller must close handles passed via TProcessInfo
end;

function TExecCreateProcessAsUser.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
    ppInheritHandles, ppCreateSuspended, ppBreakaway, ppNewConsole,
    ppShowWindowMode, ppRunAsInvoker:
      Result := True;
  else
    Result := False;
  end;
end;

{ TExecCreateProcessWithToken }

function TExecCreateProcessWithToken.Execute(ParamSet: IExecProvider):
  TProcessInfo;
var
  CurrentDir: PWideChar;
  Startup: TStartupInfoHolder;
begin
  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  Startup := TStartupInfoHolder.Create(ParamSet, Self);

  try
    WinCheck(CreateProcessWithTokenW(
      ParamSet.Token,
      ParamSet.LogonFlags,
      PWideChar(ParamSet.Application),
      PWideChar(PrepareCommandLine(ParamSet)),
      Startup.CreationFlags,
      nil,
      CurrentDir,
      Startup.StartupInfoEx,
      Result
      ), 'CreateProcessWithTokenW'
    );
  finally
    Startup.Free;
  end;

  // The caller must close handles passed via TProcessInfo
end;

function TExecCreateProcessWithToken.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppDesktop, ppToken, ppLogonFlags,
    ppCreateSuspended, ppBreakaway, ppShowWindowMode:
      Result := True;
  else
    Result := False;
  end;
end;

{ TRunAsInvoker }

destructor TRunAsInvoker.Destroy;
var
  Environment: IEnvironment;
begin
  Environment := TEnvironment.OpenCurrent;

  if OldValuePresent then
    Environment.SetVariable(COMPAT_NAME, OldValue)
  else
    Environment.DeleteVariable(COMPAT_NAME);

  inherited;
end;

constructor TRunAsInvoker.SetCompatState(Enabled: Boolean);
var
  Environment: IEnvironment;
  Status: TNtxStatus;
begin
  Environment := TEnvironment.OpenCurrent;

  // Save the current state
  Status := Environment.QueryVariableWithStatus(COMPAT_NAME, OldValue);

  if Status.IsSuccess then
    OldValuePresent := True
  else if Status.Status = STATUS_VARIABLE_NOT_FOUND then
    OldValuePresent := False
  else
    Status.RaiseOnError;

  // Set the new state
  if Enabled then
    Environment.SetVariable(COMPAT_NAME, COMPAT_VALUE).RaiseOnError
  else if OldValuePresent then
    Environment.DeleteVariable(COMPAT_NAME).RaiseOnError;
end;

end.
