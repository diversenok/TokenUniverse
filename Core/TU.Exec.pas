unit TU.Exec;

interface

uses
  NtUtils.Processes.Create;

type
  TExecParam = (
    ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
    ppLogonFlags, ppInheritHandles, ppCreateSuspended, ppBreakaway,
    ppNewConsole, ppRequireElevation, ppShowWindowMode, ppRunAsInvoker,
    ppEnvironment, ppAppContainer
  );

  TExecParamSet = set of TExecParam;

// Determine if a process creation method supports an option
function ExecSupports(Method: TCreateProcessMethod): TExecParamSet;

implementation

uses
  NtUtils.Processes.Create.Win32, NtUtils.Processes.Create.Shell,
  NtUtils.Processes.Create.Native, NtUtils.Processes.Create.Wmi,
  NtUtils.Processes.Create.Wdc;

function ExecSupports(Method: TCreateProcessMethod): TExecParamSet;
begin
  if Pointer(@Method) = Pointer(@AdvxCreateProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppBreakaway, ppNewConsole,
      ppShowWindowMode, ppRunAsInvoker, ppEnvironment, ppAppContainer]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessWithToken) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppLogonFlags,
      ppCreateSuspended, ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessWithLogon) then
    Result := [ppCurrentDirectory, ppDesktop, ppLogonFlags,
      ppCreateSuspended, ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@ShlxExecute) then
    Result := [ppCurrentDirectory, ppNewConsole, ppRequireElevation,
      ppShowWindowMode, ppRunAsInvoker]

  else if Pointer(@Method) = Pointer(@RtlxCreateUserProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppParentProcess,
      ppInheritHandles, ppCreateSuspended, ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@WmixCreateProcess) then
    Result := [ppCurrentDirectory, ppDesktop, ppToken, ppCreateSuspended,
      ppShowWindowMode, ppEnvironment]

  else if Pointer(@Method) = Pointer(@WdcxCreateProcess) then
    Result := [ppCurrentDirectory]

  else
    Result := [];
end;

end.
