unit TU.RestartSvc;

interface

uses
  NtUtils.Exceptions, NtUtils.Svc;

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
procedure ReSvcRunInSession(ScvParams: TStringArray);

implementation

uses
  Winapi.WinNt, Winapi.WinBase, Ntapi.ntstatus, Ntapi.ntseapi, Ntapi.ntobapi,
  Ntapi.ntrtl, NtUtils.Objects, System.SysUtils, Ntapi.ntdef, NtUtils.WinUser,
  NtUtils.Snapshots.Processes, NtUtils.Tokens, NtUtils.Exec, NtUtils.Exec.Shell,
  NtUtils.Exec.Win32, NtUtils.Svc.SingleTaskSvc, NtUtils.Processes;

{ Restart Service client functions }

function ReSvcCreateService(IsSystemPlus: Boolean): TNtxStatus;
var
  CommandLine: String;
  hSvc: TScmHandle;
  Parameters: TStringArray;
begin
  CommandLine := '"' + ParamStr(0) + '" ' + RESVC_PARAM;
  if IsSystemPlus then
    CommandLine := CommandLine + ' ' + RESVC_SYSPLUS_PARAM;

  // Create Run-as-system service
  Result := ScmxCreateService(hSvc, CommandLine, RESVC_NAME, RESVC_DIPLAY_NAME);

  if not Result.IsSuccess then
    Exit;

  // Prepare the service parameters (session and desktop) so it would know who
  // requested the restart action
  SetLength(Parameters, 2);
  Parameters[0] := IntToStr(RtlGetCurrentPeb.SessionId);
  Parameters[1] := UsrxCurrentDesktopName;

  // Start the service
  Result := ScmxStartService(hSvc, Parameters);

  if not Result.IsSuccess then
    Exit;

  ScmxDeleteService(hSvc).ReportOnError;

  ScmxClose(hSvc);
end;

procedure ReSvcDelegate(RestartMethod: TRestartMethod);
var
  Provider: TDefaultExecProvider;
  Method: IExecMethod;
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

  Method := TExecShellExecute.Create;
  ProcessInfo := Method.Execute(Provider);

  FreeProcessInfo(ProcessInfo);
  // No need to free Provider since the interface will free it automatically
end;

{ Restart Service server functions }

function GetCsrssToken: THandle;
const
  SrcProcess = 'csrss.exe';
var
  Csrss: PProcessInfo;
begin
  with TProcessSnapshot.Create do
  begin
    Csrss := FindByName(SrcProcess);
    try
      if Assigned(Csrss) then
        NtxOpenProcessTokenById(Result, Csrss.ProcessId, TOKEN_DUPLICATE,
          0).RaiseOnError
      else
        raise Exception.Create(SrcProcess + ' is not found on the system.');
    finally
      Free;
    end;
  end;
end;

function PrepareToken(SessionID: Integer): THandle;
var
  hToken: THandle;
begin
  // We are already running as SYSTEM, but if we want to obtain a rare
  // `SeCreateTokenPrivilege` (aka SYSTEM+ token) we need to steal it from
  // csrss.exe
  if ParamStr(2) = RESVC_SYSPLUS_PARAM then
    hToken := GetCsrssToken
  else
    NtxOpenProcessToken(hToken, NtCurrentProcess, TOKEN_DUPLICATE,
      0).RaiseOnError;

  try
    // Duplicate
    NtxDuplicateToken(Result, hToken, TOKEN_ADJUST_DEFAULT or
      TOKEN_ADJUST_SESSIONID or TOKEN_QUERY or TOKEN_DUPLICATE or
      TOKEN_ASSIGN_PRIMARY, TokenPrimary, SecurityImpersonation,
      False).RaiseOnError;

    // Change session
    NtxSetInformationToken(Result, TokenSessionId, @SessionId,
      SizeOf(SessionId)).RaiseOnError;
  finally
    NtxSafeClose(hToken);
  end;
end;

procedure ReSvcRunInSession(ScvParams: TStringArray);
var
  Provider: TDefaultExecProvider;
  Method: IExecMethod;
  ProcessInfo: TProcessInfo;
  BasicInfo: TProcessBasinInformation;
  Session: Integer;
  Status: TNtxStatus;
begin
  Provider := TDefaultExecProvider.Create;

  Provider.UseParams := [ppDesktop, ppToken];
  Provider.strApplication := ParamStr(0);

  if (Length(ScvParams) >= 2) and TryStrToInt(ScvParams[1], Session) then
    Provider.hToken := PrepareToken(Session)
  else
    Provider.hToken := PrepareToken(0);

  // TODO: determine interactive session and active desktop

  if Length(ScvParams) >= 3 then
    Provider.strDesktop := ScvParams[2]
  else
    Provider.strDesktop := 'WinSta0\Default';

  Method := TExecCreateProcessAsUser.Create;
  ProcessInfo := Method.Execute(Provider);

  {$IFDEF DEBUG}
  // Check that the process didn't crash immediately
  Status := NtxWaitForSingleObject(ProcessInfo.hProcess, False, 200);

  case Status.Status of
    STATUS_TIMEOUT: ; // Nothing
    STATUS_SUCCESS:
      begin
        OutputDebugStringW('Abnormal process termination');

        Status := NtxQueryBasicInformationProcess(ProcessInfo.hProcess,
          BasicInfo);

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

  FreeProcessInfo(ProcessInfo);
end;

end.

