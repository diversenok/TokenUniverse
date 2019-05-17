unit NtUtils.Exec.Win32;

interface

uses
  NtUtils.Exec;

type
  TExecCreateProcess = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    procedure Execute(ParamSet: IExecProvider);
  end;

  TExecCreateProcessAsUser = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    procedure Execute(ParamSet: IExecProvider);
  end;

  TExecCreateProcessWithToken = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    procedure Execute(ParamSet: IExecProvider);
  end;

function PrepareCreationFlags(ParamSet: IExecProvider): Cardinal;

implementation

uses
  Winapi.ProcessThreadsApi, NtUtils.Exceptions;

function PrepareCreationFlags(ParamSet: IExecProvider): Cardinal;
begin
  Result := 0;

  if ParamSet.Provides(ppCreateSuspended) and ParamSet.CreateSuspended then
    Result := Result or CREATE_SUSPENDED;

  if ParamSet.Provides(ppBreakaway) and ParamSet.Breakaway then
    Result := Result or CREATE_BREAKAWAY_FROM_JOB;
end;

function PrepareStartupInfo(ParamSet: IExecProvider): TStartupInfoW;
begin
  GetStartupInfoW(Result);

  if ParamSet.Provides(ppDesktop) then
    Result.lpDesktop := PWideChar(ParamSet.Desktop);

  if ParamSet.Provides(ppShowWindowMode) then
  begin
    Result.dwFlags := STARTF_USESHOWWINDOW;
    Result.wShowWindow := ParamSet.ShowWindowMode
  end;
end;

{ TExecCreateProcess }

procedure TExecCreateProcess.Execute(ParamSet: IExecProvider);
var
  CommandLine: String;
  CurrentDir: PWideChar;
  ProcessInfo: TProcessInformation;
begin
  // Command line should be in writable memory
  CommandLine := PrepareCommandLine(ParamSet);

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  WinCheck(CreateProcessW(
    PWideChar(ParamSet.Application),
    PWideChar(CommandLine),
    nil,
    nil,
    ParamSet.Provides(ppInheritHandles) and ParamSet.InheritHandles,
    PrepareCreationFlags(ParamSet),
    nil,
    CurrentDir,
    PrepareStartupInfo(ParamSet),
    ProcessInfo
    ), 'CreateProcessW'
  );
end;

function TExecCreateProcess.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppDesktop,
    ppInheritHandles, ppCreateSuspended, ppBreakaway, ppShowWindowMode:
      Result := True;
  else
    Result := False;
  end;
end;

{ TExecCreateProcessAsUser }

procedure TExecCreateProcessAsUser.Execute(ParamSet: IExecProvider);
var
  CommandLine: String;
  CurrentDir: PWideChar;
  ProcessInfo: TProcessInformation;
begin
  // Command line should be in writable memory
  CommandLine := PrepareCommandLine(ParamSet);

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  WinCheck(CreateProcessAsUserW(
    ParamSet.Token,
    PWideChar(ParamSet.Application),
    PWideChar(CommandLine),
    nil,
    nil,
    ParamSet.Provides(ppInheritHandles) and ParamSet.InheritHandles,
    PrepareCreationFlags(ParamSet),
    nil,
    CurrentDir,
    PrepareStartupInfo(ParamSet),
    ProcessInfo
    ), 'CreateProcessAsUserW'
  );
end;

function TExecCreateProcessAsUser.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppDesktop, ppToken,
    ppInheritHandles, ppCreateSuspended, ppBreakaway, ppShowWindowMode:
      Result := True;
  else
    Result := False;
  end;
end;

{ TExecCreateProcessWithToken }

procedure TExecCreateProcessWithToken.Execute(ParamSet: IExecProvider);
var
  CommandLine: String;
  CurrentDir: PWideChar;
  ProcessInfo: TProcessInformation;
begin
  // Command line should be in writable memory
  CommandLine := PrepareCommandLine(ParamSet);

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  WinCheck(CreateProcessWithTokenW(
    ParamSet.Token,
    ParamSet.LogonFlags,
    PWideChar(ParamSet.Application),
    PWideChar(CommandLine),
    PrepareCreationFlags(ParamSet),
    nil,
    CurrentDir,
    PrepareStartupInfo(ParamSet),
    ProcessInfo
    ), 'CreateProcessWithTokenW'
  );
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

end.
