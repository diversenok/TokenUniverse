unit TU.RestartSvc;

interface

uses
  NtUtils, NtUiLib.Exceptions, NtUtils.Svc;

const
  RESVC_PARAM = '/service';
  RESVC_SYSPLUS_PARAM = '/plus';
  RESVC_NAME = 'TokenUniverseSvc';
  RESVC_DIPLAY_NAME = 'Token Universe Run-As-System Service';

  DELEGATE_PARAM = '/delegate';
  DELEGATE_PARAM_SYSPLUS = DELEGATE_PARAM + ' ' + RESVC_SYSPLUS_PARAM;

type
  TRestartMethod = (rmElevate, rmDelegateSystem, rmDelegateSystemPlus);

//  Create and invoke TokenUniverse Run-As-System Service.
function ReSvcCreateService(IsSystemPlus: Boolean): TNtxStatus;

// Restart elevated or restart as SYSTEM from a non-elevated process
procedure ReSvcDelegate(RestartMethod: TRestartMethod);

// The payload of TokenUniverse Run-As-System Service.
procedure ReSvcRunInSession(ScvParams: TArray<String>);

implementation

uses
  Winapi.WinNt, Winapi.WinBase, Ntapi.ntstatus, Ntapi.ntseapi,
  Ntapi.ntpebteb, NtUtils.Objects, System.SysUtils, NtUtils.WinUser,
  NtUtils.Processes.Snapshots, NtUtils.Tokens, NtUtils.Exec, NtUtils.Exec.Shell,
  NtUtils.Exec.Win32, NtUtils.Processes.Query, Ntapi.ntpsapi,
  NtUtils.Tokens.Query;

{ Restart Service client functions }

function ReSvcCreateService(IsSystemPlus: Boolean): TNtxStatus;
var
  CommandLine: String;
  hxSvc: IScmHandle;
  Parameters: TArray<String>;
begin
  CommandLine := '"' + ParamStr(0) + '" ' + RESVC_PARAM;
  if IsSystemPlus then
    CommandLine := CommandLine + ' ' + RESVC_SYSPLUS_PARAM;

  // Create Run-as-system service
  Result := ScmxCreateService(hxSvc, CommandLine, RESVC_NAME,
    RESVC_DIPLAY_NAME);

  if not Result.IsSuccess then
    Exit;

  // Prepare the service parameters (session and desktop) so it would know who
  // requested the restart action
  SetLength(Parameters, 2);
  Parameters[0] := IntToStr(RtlGetCurrentPeb.SessionId);
  Parameters[1] := UsrxCurrentDesktopName;

  // Start the service
  Result := ScmxStartService(hxSvc.Handle, Parameters);

  ScmxDeleteService(hxSvc.Handle).ReportOnError;
end;

procedure ReSvcDelegate(RestartMethod: TRestartMethod);
var
  Provider: TDefaultExecProvider;
  ProcessInfo: TProcessInfo;
begin
  Provider := TDefaultExecProvider.Create;

  Provider.UseParams := [ppParameters, ppRequireElevation];
  Provider.strApplication := ParamStr(0);
  Provider.bRequireElevation := True;

  // The parameter states that the execution of the service was delegated
  case RestartMethod of
    rmElevate:
      Provider.strParameters := '';

    rmDelegateSystem:
      Provider.strParameters := DELEGATE_PARAM;

    rmDelegateSystemPlus:
      Provider.strParameters := DELEGATE_PARAM_SYSPLUS;
  end;

  TExecShellExecute.Execute(Provider, ProcessInfo).RaiseOnError;
  // No need to free Provider since the interface will free it automatically
end;

{ Restart Service server functions }

function GetCsrssToken: IHandle;
const
  SrcProcess = 'csrss.exe';
var
  Processes: TArray<TProcessEntry>;
  i: Integer;
begin
  NtxEnumerateProcesses(Processes).RaiseOnError;

  for i := 0 to High(Processes) do
    if Processes[i].ImageName = SrcProcess then
    begin
      NtxOpenProcessTokenById(Result, Processes[i].Basic.ProcessId,
        TOKEN_DUPLICATE).RaiseOnError;
      Exit;
    end;

  raise Exception.Create(SrcProcess + ' is not found on the system.');
end;

function PrepareToken(SessionID: Integer): IHandle;
var
  hxToken: IHandle;
begin
  // We are already running as SYSTEM, but if we want to obtain a rare
  // `SeCreateTokenPrivilege` (aka SYSTEM+ token) we need to steal it from
  // csrss.exe
  if ParamStr(2) = RESVC_SYSPLUS_PARAM then
    hxToken := GetCsrssToken
  else
    NtxOpenProcessToken(hxToken, NtCurrentProcess,
      TOKEN_DUPLICATE).RaiseOnError;

  // Duplicate
  NtxDuplicateToken(Result, hxToken.Handle, TOKEN_ADJUST_DEFAULT or
    TOKEN_ADJUST_SESSIONID or TOKEN_QUERY or TOKEN_DUPLICATE or
    TOKEN_ASSIGN_PRIMARY, TokenPrimary).RaiseOnError;

  // Change session
  NtxSetToken(Result.Handle, TokenSessionId, @SessionId,
    SizeOf(SessionId)).RaiseOnError;
end;

procedure ReSvcRunInSession(ScvParams: TArray<String>);
var
  Provider: TDefaultExecProvider;
  ProcessInfo: TProcessInfo;
  Session: Integer;
  {$IFDEF DEBUG}
  BasicInfo: TProcessBasicInformation;
  Status: TNtxStatus;
  {$ENDIF}
begin
  Provider := TDefaultExecProvider.Create;

  Provider.UseParams := [ppDesktop, ppToken];
  Provider.strApplication := ParamStr(0);

  if (Length(ScvParams) >= 2) and TryStrToInt(ScvParams[1], Session) then
    Provider.hxToken := PrepareToken(Session)
  else
    Provider.hxToken := PrepareToken(0);

  // TODO: determine interactive session and active desktop

  if Length(ScvParams) >= 3 then
    Provider.strDesktop := ScvParams[2]
  else
    Provider.strDesktop := 'WinSta0\Default';

  TExecCreateProcessAsUser.Execute(Provider, ProcessInfo).RaiseOnError;

  {$IFDEF DEBUG}
  // Check that the process didn't crash immediately
  Status := NtxWaitForSingleObject(ProcessInfo.hxProcess.Handle, 200 * MILLISEC);

  case Status.Status of
    STATUS_TIMEOUT: ; // Nothing
    STATUS_SUCCESS:
      begin
        OutputDebugStringW('Abnormal process termination');

        Status := NtxProcess.Query<TProcessBasicInformation>(
          ProcessInfo.hxProcess.Handle, ProcessBasicInformation, BasicInfo);

        if Status.IsSuccess then
          OutputDebugStringW(PWideChar('Exit code: 0x' +
            IntToHex(BasicInfo.ExitStatus, 8)))
        else
          Status.ReportOnError;
      end;
    else
      Status.ReportOnError;
  end;
  {$ENDIF}
end;

end.

