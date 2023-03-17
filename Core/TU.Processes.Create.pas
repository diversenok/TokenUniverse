unit TU.Processes.Create;

{
  This module provides the back-end logic for the process creation dialog.
}

interface

uses
  NtUtils, NtUtils.Processes.Create, NtUtils.Manifests,
  DelphiApi.Reflection;

{$MINENUMSIZE 4}

type
  TSupportedCreateParameters = set of TSupportedCreateProcessOptions;

  [NamingStyle(nsCamelCase, 'cm'), Range(1)]
  TKnownCreateMethod = (
    [Reserved] cmInvalid = 0,
    cmCreateProcessAsUser,
    cmCreateProcessWithToken,
    cmCreateProcessWithLogon,
    cmCreateProcessViaInjection,
    cmRtlCreateUserProcess,
    cmRtlCreateUserProcessEx,
    cmNtCreateUserProcess,
    cmNtCreateProcessEx,
    cmShellExecuteEx,
    cmIShellDispatch,
    cmWDC,
    cmWMI
  );

  [NamingStyle(nsCamelCase, 'mm')]
  TManifestMode = (
    mmNoRegistration,
    mmUseEmbedded,
    mmUseFromPE,
    mmUseFromXML,
    mmCustom
  );

  TTuCreateProcessOptions = record
    ManifestMode: TManifestMode;
    ManifestFilename: String;
    UseRuntimeThemes: Boolean;
    UseGdiScaling: Boolean;
    UseLongPathAware: Boolean;
    DpiAwareness: TDpiAwareness;
  end;

const
  MANIFEST_MODE_REGISTER = [mmUseEmbedded..mmCustom];

// Get a process creation method callback
[Result: MayReturnNil]
function TuGetPsMethod(
  KnownMethod: TKnownCreateMethod
): TCreateProcessMethod;

// Determine if a process creation KnownMethod supports an option
function TuPsMethodSupports(
  KnownMethod: TKnownCreateMethod
): TSupportedCreateParameters;

// Create a process via the specified method
function TuCreateProcess(
  [in] Options: TCreateProcessOptions;
  const OptionsEx: TTuCreateProcessOptions;
  const KnownMethod: TKnownCreateMethod;
  out Info: TProcessInfo
): TNtxStatus;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntcsrapi, Ntapi.Versions, NtUtils.Threads,
  NtUtils.Files.Open, NtUtils.Sections, NtUtils.Processes, NtUtils.Csr,
  NtUtils.Processes.Create.Win32, NtUtils.Processes.Create.Shell,
  NtUtils.Processes.Create.Native, NtUtils.Processes.Create.Com,
  NtUtils.Processes.Create.Remote, NtUtils.Processes.Create.Manual;

function TuGetPsMethod;
begin
  case KnownMethod of
    cmCreateProcessAsUser:       Result := AdvxCreateProcess;
    cmCreateProcessWithToken:    Result := AdvxCreateProcessWithToken;
    cmCreateProcessWithLogon:    Result := AdvxCreateProcessWithLogon;
    cmCreateProcessViaInjection: Result := AdvxCreateProcessRemote;
    cmRtlCreateUserProcess:      Result := RtlxCreateUserProcess;
    cmRtlCreateUserProcessEx:    Result := RtlxCreateUserProcessEx;
    cmNtCreateUserProcess:       Result := NtxCreateUserProcess;
    cmNtCreateProcessEx:         Result := NtxCreateProcessEx;
    cmShellExecuteEx:            Result := ShlxExecute;
    cmIShellDispatch:            Result := ComxShellExecute;
    cmWDC:                       Result := WdcxCreateProcess;
    cmWMI:                       Result := WmixCreateProcess;
  else
    Result := nil;
  end;
end;

const PS_SUPPORTS: array [TKnownCreateMethod] of TSupportedCreateParameters = (
  [],

  // CreateProcessAsUser
  [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
    spoForceBreakaway, spoNewConsole, spoRunAsInvoker, spoIgnoreElevation,
    spoEnvironment, spoSecurity, spoWindowMode, spoDesktop, spoToken,
    spoParentProcess, spoJob, spoHandleList, spoMitigationPolicies,
    spoChildPolicy, spoLPAC, spoAppContainer, spoProtection,
    spoDetectManifest],

  // CreateProcessWithToken
  [spoSuspended, spoEnvironment, spoWindowMode, spoDesktop,
    spoToken, spoLogonFlags],

  // CreateProcessWithLogon
  [spoSuspended, spoEnvironment, spoWindowMode, spoDesktop,
    spoLogonFlags, spoCredentials],

  // CreateProcess via code injection
  [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
    spoNewConsole, spoDesktop, spoParentProcess, spoTimeout],

  // RtlCreateUserProcess
  [spoSuspended, spoInheritHandles, spoEnvironment, spoSecurity,
    spoWindowMode, spoDesktop, spoToken, spoParentProcess, spoDetectManifest],

  // RtlCreateUserProcessEx
  [spoSuspended, spoInheritHandles, spoEnvironment, spoSecurity,
    spoWindowMode, spoDesktop, spoToken, spoParentProcess, spoJob,
    spoDetectManifest],

  // NtCreateUserProcess
  [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
    spoForceBreakaway, spoEnvironment, spoSecurity, spoWindowMode, spoDesktop,
    spoToken, spoParentProcess, spoJob, spoHandleList, spoChildPolicy,
    spoLPAC, spoProtection, spoAdditinalFileAccess, spoDetectManifest],

  // NtCreateProcessEx
  [spoSuspended, spoInheritHandles, spoBreakawayFromJob,
    spoForceBreakaway, spoEnvironment, spoSecurity, spoWindowMode, spoDesktop,
    spoToken, spoParentProcess, spoSection, spoAdditinalFileAccess,
    spoDetectManifest],

  // ShellExecuteEx
  [spoSuspended, spoBreakawayFromJob, spoNewConsole,
    spoRequireElevation, spoRunAsInvoker, spoWindowMode],

  // IShellDispatch
  [spoRequireElevation, spoWindowMode],

  // WDC
  [spoRequireElevation],

  // WMI
  [spoSuspended, spoWindowMode, spoDesktop, spoToken]
);

function TuPsMethodSupports;
begin
  if (KnownMethod > cmInvalid) and (KnownMethod <= High(TKnownCreateMethod)) then
    Result := PS_SUPPORTS[KnownMethod]
  else
    Result := [];
end;

function RequiresSxSRegistration(
  KnownMethod: TKnownCreateMethod;
  Mode: TManifestMode
): Boolean;
begin
  // CreateProcessAsUser uses the embedded manifiest on its own
  if (Mode = mmUseEmbedded) and (KnownMethod = cmCreateProcessAsUser) then
    Exit(False);

  Result := (spoDetectManifest in TuPsMethodSupports(KnownMethod)) and
    (Mode in [mmUseEmbedded..mmCustom]);
end;

function TuCreateProcess;
var
  Method: TCreateProcessMethod;
  ResumeLater: Boolean;
  AutoTerminate: IAutoReleasable;
  hxManifestSection: IHandle;
  ManifestRva: TMemory;
  ManifestBuilfer: IManifestBuilder;
begin
  Method := TuGetPsMethod(KnownMethod);

  if not Assigned(Method) then
  begin
    Result.Location := 'TuCreateProcess';
    Result.LastCall.UsesInfoClass(KnownMethod, icPerform);
    Result.Status := STATUS_INVALID_INFO_CLASS;
    Exit;
  end;

  if OptionsEx.ManifestMode = mmUseEmbedded then
    Include(Options.Flags, poDetectManifest);

  // Always suspend the process when donig SxS registration
  if RequiresSxSRegistration(KnownMethod, OptionsEx.ManifestMode) then
  begin
    ResumeLater := not (poSuspended in Options.Flags);
    Include(Options.Flags, poSuspended);
  end
  else
    ResumeLater := False;

  // Create the process
  Result := Method(Options, Info);

  if not Result.IsSuccess then
    Exit;

  // Perform the CSR/SxS registration if necessary
  if RequiresSxSRegistration(KnownMethod, OptionsEx.ManifestMode) and
    (piProcessHandle in Info.ValidFields) then
  begin
    // Automatically terminate on failure
    AutoTerminate := NtxDelayedTerminateProcess(Info.hxProcess, STATUS_CANCELLED);

    case OptionsEx.ManifestMode of
      mmUseEmbedded:
      begin
        Result := CsrxRegisterProcessCreation(Options, Info);

        if not Result.IsSuccess then
          Exit;
      end;

      mmUseFromPE:
      begin
        Result := RtlxCreateFileSection(hxManifestSection,
          FileParameters.UseFileName(OptionsEx.ManifestFilename, fnWin32),
          RtlxSecImageNoExecute);

        if not Result.IsSuccess then
          Exit;

        Result := RtlxFindManifestInSection(hxManifestSection.Handle,
          ManifestRva);

        if not Result.IsSuccess then
          Exit;

        Result := CsrxRegisterProcessManifest(
          Info.hxProcess.Handle,
          HandleOrDefault(Info.hxThread),
          Info.ClientId,
          hxManifestSection.Handle,
          BASE_MSG_HANDLETYPE_SECTION,
          ManifestRva,
          Options.ApplicationWin32
        );

        if not Result.IsSuccess then
          Exit;
      end;

      mmUseFromXML:
      begin
        Result := CsrxRegisterProcessManifestFromFile(
          Info.hxProcess.Handle,
          HandleOrDefault(Info.hxThread),
          Info.ClientId,
          OptionsEx.ManifestFilename,
          Options.ApplicationWin32
        );

        if not Result.IsSuccess then
          Exit;
      end;

      mmCustom:
      begin
        ManifestBuilfer := NewManifestBuilder
          .UseRuntimeThemes(OptionsEx.UseRuntimeThemes)
          .UseGdiScaling(OptionsEx.UseGdiScaling)
          .UseLongPathAware(OptionsEx.UseLongPathAware)
            .UseDpiAwareness(OptionsEx.DpiAwareness);

        case OptionsEx.DpiAwareness of
          dpiUnaware:
            ManifestBuilfer := ManifestBuilfer.UseDpiAware(dpiAwareFalse);

          dpiSystem:
            ManifestBuilfer := ManifestBuilfer.UseDpiAware(dpiAwareTrue);

          dpiPerMonitor, dpiPerMonitorV2:
            ManifestBuilfer := ManifestBuilfer.UseDpiAware(dpiAwareTruePerMonitor);
        end;

        Result := CsrxRegisterProcessManifestFromString(
          Info.hxProcess.Handle,
          HandleOrDefault(Info.hxThread),
          Info.ClientId,
          ManifestBuilfer.Build,
          Options.ApplicationWin32
        );

        if not Result.IsSuccess then
          Exit;
      end;
    end;

    // Cancel auto-termination since the registration succeeded
    AutoTerminate.AutoRelease := False;
    AutoTerminate := nil;
  end;

  if ResumeLater and (piThreadHandle in Info.ValidFields) then
    NtxResumeThread(Info.hxThread.Handle);
end;


end.
