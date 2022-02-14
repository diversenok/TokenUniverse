unit TU.Exec;

interface

uses
  NtUtils.Processes.Create;

type
  TExecParamSet = set of (
    ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
    ppLogonFlags, ppInheritHandles, ppCreateSuspended, ppBreakaway,
    ppNewConsole, ppRequireElevation, ppShowWindowMode, ppRunAsInvoker,
    ppEnvironment, ppAppContainer, ppJob
  );

// Determine if a process creation method supports an option
function ExecSupports(Method: TCreateProcessMethod): TExecParamSet;

implementation

uses
  NtUtils.Processes.Create.Win32, NtUtils.Processes.Create.Shell,
  NtUtils.Processes.Create.Native, NtUtils.Processes.Create.Com,
  NtUtils.Processes.Create.Remote, NtUtils.Processes.Create.Manual;

function ExecSupports(Method: TCreateProcessMethod): TExecParamSet;
begin
  if Pointer(@Method) = Pointer(@AdvxCreateProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppBreakaway, ppNewConsole,
      ppShowWindowMode, ppRunAsInvoker, ppEnvironment, ppAppContainer, ppJob]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessWithToken) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppLogonFlags,
      ppCreateSuspended, ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessWithLogon) then
    Result := [ppCurrentDirectory, ppDesktop, ppLogonFlags,
      ppCreateSuspended, ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessRemote) then
    Result := [ppCurrentDirectory, ppDesktop, ppParentProcess, ppInheritHandles,
      ppCreateSuspended, ppBreakaway, ppNewConsole]

  else if Pointer(@Method) = Pointer(@ShlxExecute) then
    Result := [ppCurrentDirectory, ppNewConsole, ppRequireElevation,
      ppShowWindowMode, ppRunAsInvoker]

  else if Pointer(@Method) = Pointer(@RtlxCreateUserProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@RtlxCreateUserProcessEx) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppShowWindowMode, ppEnvironment,
      ppJob]

  else if Pointer(@Method) = Pointer(@NtxCreateUserProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppBreakaway, ppShowWindowMode,
      ppEnvironment, ppJob]

  else if Pointer(@Method) = Pointer(@NtxCreateProcessEx) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppBreakaway, ppShowWindowMode,
      ppEnvironment]

  else if Pointer(@Method) = Pointer(@WmixCreateProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppCreateSuspended,
      ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@WdcxCreateProcess) then
    Result := [ppCurrentDirectory, ppRequireElevation]

  else if Pointer(@Method) = Pointer(@ComxShellExecute) then
    Result := [ppCurrentDirectory, ppRequireElevation, ppShowWindowMode]

  else
    Result := [];
end;

end.
