unit TU.Exec;

interface

uses
  NtUtils.Processes.Create;

type
  TExecParamSet = set of TSupportedCreateProcessOptions;

// Determine if a process creation method supports an option
function ExecSupports(Method: TCreateProcessMethod): TExecParamSet;

implementation

uses
  NtUtils.Processes.Create.Win32, NtUtils.Processes.Create.Shell,
  NtUtils.Processes.Create.Native, NtUtils.Processes.Create.Com,
  NtUtils.Processes.Create.Remote, NtUtils.Processes.Create.Manual;

function ExecSupports;
begin
  if Pointer(@Method) = Pointer(@AdvxCreateProcess) then
    Result := [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
      spoForceBreakaway, spoNewConsole, spoRunAsInvoker, spoIgnoreElevation,
      spoEnvironment, spoSecurity, spoWindowMode, spoDesktop, spoToken,
      spoParentProcess, spoJob, spoHandleList, spoMitigationPolicies,
      spoChildPolicy, spoLPAC, spoAppContainer, spoProtection,
      spoDetectManifest]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessWithToken) then
    Result := [spoSuspended, spoEnvironment, spoWindowMode, spoDesktop,
      spoToken, spoLogonFlags]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessWithLogon) then
    Result := [spoSuspended, spoEnvironment, spoWindowMode, spoDesktop,
      spoLogonFlags, spoCredentials]

  else if Pointer(@Method) = Pointer(@AdvxCreateProcessRemote) then
    Result := [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
      spoNewConsole, spoDesktop, spoParentProcess, spoTimeout]

  else if Pointer(@Method) = Pointer(@ShlxExecute) then
    Result := [spoSuspended, spoBreakawayFromJob, spoNewConsole,
      spoRequireElevation, spoRunAsInvoker, spoWindowMode]

  else if Pointer(@Method) = Pointer(@RtlxCreateUserProcess) then
    Result := [spoSuspended, spoInheritHandles, spoEnvironment, spoSecurity,
      spoWindowMode, spoDesktop, spoToken, spoParentProcess, spoDetectManifest]

  else if Pointer(@Method) = Pointer(@RtlxCreateUserProcessEx) then
    Result := [spoSuspended, spoInheritHandles, spoEnvironment, spoSecurity,
      spoWindowMode, spoDesktop, spoToken, spoParentProcess, spoJob,
      spoDetectManifest]

  else if Pointer(@Method) = Pointer(@NtxCreateUserProcess) then
    Result := [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
      spoForceBreakaway, spoEnvironment, spoSecurity, spoWindowMode, spoDesktop,
      spoToken, spoParentProcess, spoJob, spoHandleList, spoChildPolicy,
      spoLPAC, spoProtection, spoAdditinalFileAccess, spoDetectManifest]

  else if Pointer(@Method) = Pointer(@NtxCreateProcessEx) then
    Result := [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
      spoForceBreakaway, spoEnvironment, spoSecurity, spoWindowMode, spoDesktop,
      spoToken, spoParentProcess, spoSection, spoAdditinalFileAccess,
      spoDetectManifest]

  else if Pointer(@Method) = Pointer(@WmixCreateProcess) then
    Result := [spoSuspended, spoWindowMode, spoDesktop, spoToken]

  else if Pointer(@Method) = Pointer(@WdcxCreateProcess) then
    Result := [spoRequireElevation]

  else if Pointer(@Method) = Pointer(@ComxShellExecute) then
    Result := [spoRequireElevation, spoWindowMode]

  else
    Result := [];
end;

end.
