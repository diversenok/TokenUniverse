unit TU.RestartSvc;

interface

uses
  NtUtils, NtUiLib.Exceptions, NtUtils.Svc;

const
  RESVC_PARAM = '/service';
  RESVC_SYSPLUS_PARAM = '/plus';
  RESVC_NAME = 'TokenUniverseSvc';
  RESVC_DISPLAY_NAME = 'Token Universe Run-As-System Service';

  DELEGATE_PARAM = '/delegate';
  DELEGATE_PARAM_SYSPLUS = DELEGATE_PARAM + ' ' + RESVC_SYSPLUS_PARAM;

type
  TRestartMethod = (rmElevate, rmDelegateSystem, rmDelegateSystemPlus);

//  Create and invoke TokenUniverse Run-As-System Service.
function ReSvcCreateService(IsSystemPlus: Boolean): TNtxStatus;

// Restart elevated or restart as SYSTEM from a non-elevated process
procedure ReSvcDelegate(RestartMethod: TRestartMethod);

// The payload of TokenUniverse Run-As-System Service.
procedure ReSvcRunInSession(const ScvParams: TArray<String>);

implementation

uses
  Ntapi.WinNt, Ntapi.WinBase, Ntapi.ntstatus, Ntapi.ntseapi, Ntapi.ntpsapi,
  Ntapi.ntpebteb, NtUtils.Objects, System.SysUtils, NtUtils.WinUser,
  NtUtils.Processes.Snapshots, NtUtils.Tokens, NtUtils.Processes.Info,
  NtUtils.Tokens.Info, NtUtils.Processes.Create, NtUtils.Synchronization,
  NtUtils.Processes.Create.Shell, NtUtils.Processes.Create.Win32,
  NtUiLib.Errors;

{ Restart Service client functions }

function ReSvcCreateService;
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
    RESVC_DISPLAY_NAME);

  if not Result.IsSuccess then
    Exit;

  // Prepare the service parameters (session and desktop) so it would know who
  // requested the restart action
  SetLength(Parameters, 2);
  Parameters[0] := IntToStr(RtlGetCurrentPeb.SessionId);
  Parameters[1] := UsrxCurrentDesktopName;

  // Start the service
  Result := ScmxStartService(hxSvc, Parameters);

  ScmxDeleteService(hxSvc);
end;

procedure ReSvcDelegate;
var
  Options: TCreateProcessOptions;
  ProcessInfo: TProcessInfo;
begin
  Options := Default(TCreateProcessOptions);
  Options.Application := ParamStr(0);
  Options.Flags := [poRequireElevation];

  // The parameter states that the execution of the service was delegated
  case RestartMethod of
    rmDelegateSystem:
      Options.Parameters := DELEGATE_PARAM;

    rmDelegateSystemPlus:
      Options.Parameters := DELEGATE_PARAM_SYSPLUS;
  end;

  ShlxExecute(Options, ProcessInfo).RaiseOnError;
end;

{ Restart Service server functions }

function GetCsrssToken: IHandle;
var
  hxProcess: IHandle;
begin
  NtxOpenProcessByName(hxProcess, 'csrss.exe',
    PROCESS_QUERY_LIMITED_INFORMATION, [pnAllowAmbiguousMatch, pnCaseSensitive])
    .RaiseOnError;

  NtxOpenProcessToken(Result, hxProcess, TOKEN_DUPLICATE).RaiseOnError;
end;

function PrepareToken(SessionID: Integer): IHandle;
begin
  // We are already running as SYSTEM, but if we want to obtain a rare
  // `SeCreateTokenPrivilege` (aka SYSTEM+ token) we need to steal it from
  // csrss.exe
  if ParamStr(2) = RESVC_SYSPLUS_PARAM then
    Result := GetCsrssToken
  else
    Result := NtxCurrentProcessToken;

  NtxDuplicateTokenLocal(Result, TokenPrimary).RaiseOnError;
  NtxToken.Set(Result, TokenSessionId, SessionID).RaiseOnError;
end;

procedure ReSvcRunInSession;
var
  Options: TCreateProcessOptions;
  ProcessInfo: TProcessInfo;
  Session: Integer;
  {$IFDEF DEBUG}
  BasicInfo: TProcessBasicInformation;
  Status: TNtxStatus;
  {$ENDIF}
begin
  Options := Default(TCreateProcessOptions);
  Options.Application := ParamStr(0);

  if (Length(ScvParams) >= 2) and TryStrToInt(ScvParams[1], Session) then
    Options.hxToken := PrepareToken(Session)
  else
    Options.hxToken := PrepareToken(USER_SHARED_DATA.ActiveConsoleId);

  // TODO: determine interactive session and active desktop

  if Length(ScvParams) >= 3 then
    Options.Desktop := ScvParams[2]
  else
    Options.Desktop := 'WinSta0\Default';

  AdvxCreateProcess(Options, ProcessInfo).RaiseOnError;

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
            IntToHex(BasicInfo.ExitStatus, 8)));
      end;
  end;

  if not Status.IsSuccess then
    OutputDebugStringW(PWideChar(Status.ToString));
  {$ENDIF}
end;

end.

