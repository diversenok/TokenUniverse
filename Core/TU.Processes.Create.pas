unit TU.Processes.Create;

{
  This module provides the back-end logic for the process creation dialog.
}

interface

uses
  Ntapi.WinUser, NtUtils, NtUtils.Processes.Create, NtUtils.Manifests,
  DelphiApi.Reflection;

{$MINENUMSIZE 4}

type
  TSupportedCreateParameters = set of TSupportedCreateProcessOptions;

  [NamingStyle(nsCamelCase, 'cm'), MinValue(1)]
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
    cmIDesktopAppxActivator,
    cmIShellDispatch,
    cmMMC20Application,
    cmICMLuaUtil,
    cmIHxHelpPaneServer,
    cmIBackgroundCopyJob,
    cmSbApiPort,
    cmWerNonElevatedStart,
    cmWerSilentProcessExit,
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
  ParentHwnd: THwnd;
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
  NtUtils.Processes.Create.Remote, NtUtils.Processes.Create.Manual,
  NtUtils.Processes.Create.Package, NtUtils.Processes.Create.Csr,
  NtUtils.Processes.Create.Wer, TU.DesktopAccess;

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
    cmIDesktopAppxActivator:     Result := PkgxCreateProcessInPackage;
    cmIShellDispatch:            Result := ComxShellDispatchExecute;
    cmMMC20Application:          Result := MmcxExecuteShellCommand;
    cmICMLuaUtil:                Result := CmxShellExecute;
    cmIHxHelpPaneServer:         Result := HlpxShellExecute;
    cmIBackgroundCopyJob:        Result := ComxCreateProcessBITS;
    cmSbApiPort:                 Result := CsrxCreateProcess;
    cmWerNonElevatedStart:       Result := WerxExecuteNonElevated;
    cmWerSilentProcessExit:      Result := WerxExecuteSilentProcessExit;
    cmWDC:                       Result := SchxRunAsInteractive;
    cmWMI:                       Result := WmixCreateProcess;
  else
    Result := nil;
  end;
end;

const PS_SUPPORTS: array [TKnownCreateMethod] of TSupportedCreateParameters = (
  [],

  // CreateProcessAsUser
  [spoParameters, spoCurrentDirectory, spoSuspended, spoInheritHandles,
    spoBreakawayFromJob, spoForceBreakaway, spoInheritConsole, spoRunAsInvoker,
    spoIgnoreElevation, spoEnvironment, spoObjectInherit, spoSecurity,
    spoWindowMode, spoWindowTitle, spoStdHandles, spoDesktop, spoToken,
    spoParentProcess, spoJob, spoDebugPort, spoHandleList, spoMitigations,
    spoChildPolicy, spoLPAC, spoAppContainer, spoPackage, spoPackageBreakaway,
    spoProtection, spoSafeOpenPromptOriginClaim, spoDetectManifest],

  // CreateProcessWithToken
  [spoParameters, spoCurrentDirectory, spoSuspended, spoEnvironment,
    spoWindowMode, spoWindowTitle, spoStdHandles, spoDesktop, spoToken,
    spoParentProcess, spoPriorityClass, spoLogonFlags],

  // CreateProcessWithLogon
  [spoParameters, spoCurrentDirectory, spoSuspended, spoEnvironment,
    spoWindowMode, spoWindowTitle, spoStdHandles, spoDesktop, spoParentProcess,
    spoPriorityClass, spoLogonFlags, spoCredentials],

  // CreateProcess via code injection
  [spoParameters, spoCurrentDirectory, spoSuspended, spoInheritHandles,
    spoBreakawayFromJob, spoInheritConsole, spoDesktop, spoParentProcess,
    spoTimeout],

  // RtlCreateUserProcess
  [spoParameters, spoCurrentDirectory, spoSuspended, spoInheritHandles,
    spoEnvironment, spoSecurity, spoWindowMode, spoWindowTitle, spoStdHandles,
    spoDesktop, spoToken, spoParentProcess, spoDebugPort, spoDetectManifest],

  // RtlCreateUserProcessEx
  [spoParameters, spoCurrentDirectory, spoSuspended, spoInheritHandles,
    spoEnvironment, spoSecurity, spoWindowMode, spoWindowTitle, spoStdHandles,
    spoDesktop, spoToken, spoParentProcess, spoJob, spoDebugPort,
    spoDetectManifest],

  // NtCreateUserProcess
  [spoParameters, spoCurrentDirectory, spoSuspended, spoInheritHandles,
    spoBreakawayFromJob, spoForceBreakaway, spoInheritConsole, spoEnvironment,
    spoObjectInherit, spoDesiredAccess, spoSecurity, spoWindowMode,
    spoWindowTitle, spoStdHandles, spoDesktop, spoToken, spoParentProcess,
    spoJob, spoDebugPort, spoHandleList, spoMemoryReserve, spoPriorityClass,
    spoMitigations, spoChildPolicy, spoLPAC, spoPackageBreakaway, spoProtection,
    spoSafeOpenPromptOriginClaim, spoAdditionalFileAccess, spoDetectManifest],

  // NtCreateProcessEx
  [spoParameters, spoCurrentDirectory, spoSuspended, spoInheritHandles,
    spoBreakawayFromJob, spoForceBreakaway, spoEnvironment, spoObjectInherit,
    spoDesiredAccess, spoSecurity, spoWindowMode, spoWindowTitle, spoStdHandles,
    spoDesktop, spoToken, spoParentProcess, spoSection, spoDebugPort,
    spoAdditionalFileAccess, spoDetectManifest],

  // ShellExecuteEx
  [spoParameters, spoCurrentDirectory, spoSuspended, spoBreakawayFromJob,
    spoInheritConsole, spoRequireElevation, spoRunAsInvoker, spoOwnerWindow,
    spoWindowMode],

  // IDesktopAppXActivator
  [spoParameters, spoCurrentDirectory, spoSuspended, spoRequireElevation,
    spoWindowMode, spoToken, spoParentProcess, spoPackageBreakaway,
    spoAppUserModeId],

  // IShellDispatch
  [spoParameters, spoCurrentDirectory, spoRequireElevation, spoWindowMode],

  // MMC20.Application
  [spoParameters, spoCurrentDirectory, spoWindowMode],

  // ICMLuaUtil
  [spoParameters, spoCurrentDirectory, spoRequireElevation, spoOwnerWindow,
    spoWindowMode],

  // IHxHelpPaneServer
  [spoSessionId],

  // IBackgroundCopyJob2
  [spoParameters],

  // SbApiPort
  [spoParameters, spoCurrentDirectory, spoSessionID],

  // WER::NonElevatedProcessStart
  [spoParameters],

  // WER::SilentProcessExitReport
  [spoParameters, spoRequireElevation],

  // WDC
  [spoParameters, spoCurrentDirectory, spoRequireElevation, spoSessionID],

  // WMI
  [spoParameters, spoCurrentDirectory, spoSuspended, spoEnvironment,
    spoWindowMode, spoDesktop, spoToken]
);

function TuPsMethodSupports;
begin
  if (KnownMethod <= cmInvalid) or (KnownMethod > High(TKnownCreateMethod)) then
    Exit([]);

  Result := PS_SUPPORTS[KnownMethod];
end;

function RequiresSxSRegistration(
  KnownMethod: TKnownCreateMethod;
  Mode: TManifestMode
): Boolean;
begin
  // CreateProcessAsUser uses the embedded manifest on its own
  if (Mode = mmUseEmbedded) and (KnownMethod = cmCreateProcessAsUser) then
    Exit(False);

  Result := (spoDetectManifest in TuPsMethodSupports(KnownMethod)) and
    (Mode in [mmUseEmbedded..mmCustom]);
end;

function RequiresPostCreationTokenCheck(
  KnownMethod: TKnownCreateMethod;
  const Options: TCreateProcessOptions
): Boolean;
begin
  // Any method with tokens except for WMI since it's better to do pre-creation
  Result := (spoToken in TuPsMethodSupports(KnownMethod)) and
    (KnownMethod <> cmWMI) and Assigned(Options.hxToken);
end;

function TuCreateProcess;
var
  Method: TCreateProcessMethod;
  ResumeLater: Boolean;
  AutoTerminate: IDeferredOperation;
  hxManifestSection: IHandle;
  ManifestRva: TMemory;
  ManifestBuilder: IManifestBuilder;
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

  // Always suspend the process when doing SxS registration or when to checking
  // logon SID's access to the desktop
  if RequiresSxSRegistration(KnownMethod, OptionsEx.ManifestMode) or
    RequiresPostCreationTokenCheck(KnownMethod, Options) or
    (KnownMethod = cmCreateProcessWithLogon) then
  begin
    ResumeLater := not (poSuspended in Options.Flags);
    Include(Options.Flags, poSuspended);
  end
  else
    ResumeLater := False;

  // When using WMI, check if the token grants desktop access before spawning
  // process because we won't get a handle to suspend/resume it later
  if (KnownMethod = cmWMI) and Assigned(Options.hxToken) then
    TuSuggestDesktopAccess(ParentHwnd, Options.Desktop, Options.hxToken);

  // Create the process
  Result := Method(Options, Info);

  if not Result.IsSuccess then
    Exit;

  // Logon generated a new token with a new logon SID; read it from the process
  // and ask if the user wants to adjust access.
  if (KnownMethod = cmCreateProcessWithLogon) and (Info.ValidFields *
    [piProcessHandle, piThreadHandle] = [piProcessHandle, piThreadHandle]) then
    TuSuggestDesktopAccessByProcess(ParentHwnd, Options.Desktop, Info.hxProcess,
      Info.hxThread)

  // Similar, for existing tokens: check if the logon SID grants desktop access
  else if RequiresPostCreationTokenCheck(KnownMethod, Options) then
    TuSuggestDesktopAccess(ParentHwnd, Options.Desktop, Options.hxToken);

  // Perform the CSR/SxS registration if necessary
  if RequiresSxSRegistration(KnownMethod, OptionsEx.ManifestMode) and
    (piProcessHandle in Info.ValidFields) then
  begin
    // Automatically terminate on failure
    AutoTerminate := NtxDeferTerminateProcess(Info.hxProcess, STATUS_CANCELLED);

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
          PAGE_READONLY, RtlxSecImageNoExecute);

        if not Result.IsSuccess then
          Exit;

        Result := RtlxFindManifestInSection(hxManifestSection, ManifestRva);

        if not Result.IsSuccess then
          Exit;

        Result := CsrxRegisterProcessManifest(
          Info.hxProcess,
          Info.hxThread,
          Info.ClientId,
          hxManifestSection,
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
          Info.hxProcess,
          Info.hxThread,
          Info.ClientId,
          OptionsEx.ManifestFilename,
          Options.ApplicationWin32
        );

        if not Result.IsSuccess then
          Exit;
      end;

      mmCustom:
      begin
        ManifestBuilder := NewManifestBuilder
          .UseRuntimeThemes(OptionsEx.UseRuntimeThemes)
          .UseGdiScaling(OptionsEx.UseGdiScaling)
          .UseLongPathAware(OptionsEx.UseLongPathAware)
          .UseDpiAwareness(OptionsEx.DpiAwareness);

        case OptionsEx.DpiAwareness of
          dpiUnaware:
            ManifestBuilder := ManifestBuilder.UseDpiAware(dpiAwareFalse);

          dpiSystem:
            ManifestBuilder := ManifestBuilder.UseDpiAware(dpiAwareTrue);

          dpiPerMonitor, dpiPerMonitorV2:
            ManifestBuilder := ManifestBuilder.UseDpiAware(dpiAwareTruePerMonitor);
        end;

        Result := CsrxRegisterProcessManifestFromString(
          Info.hxProcess,
          Info.hxThread,
          Info.ClientId,
          ManifestBuilder.Build,
          Options.ApplicationWin32
        );

        if not Result.IsSuccess then
          Exit;
      end;
    end;

    // Cancel auto-termination since the registration succeeded
    AutoTerminate.Cancel;
    AutoTerminate := nil;
  end;

  if ResumeLater and (piThreadHandle in Info.ValidFields) then
    NtxResumeThread(Info.hxThread);
end;


end.
