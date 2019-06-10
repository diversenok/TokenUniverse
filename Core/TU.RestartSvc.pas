unit TU.RestartSvc;

interface

uses
  Winapi.WinUser;

const
  RESVC_PARAM = '/service';
  RESVC_SYSPLUS_PARAM = '/plus';
  RESVC_NAME = 'TokenUniverseSvc';
  RESVC_DIPLAY_NAME = 'Token Universe Run-As-System Service';

  DELEGATE_PARAM = '/delegate';
  DELEGATE_PARAM_SYSPLUS = DELEGATE_PARAM + ' ' + RESVC_SYSPLUS_PARAM;

/// <summary>
///  A routine to create and invoke TokenUniverse Run-As-System Service.
/// </summary>
/// <param name="IsSystemPlus">
///  Determines whether the service should obtain a SYSTEM+ token.
/// </param>
procedure ReSvcCreateService(IsSystemPlus: Boolean);

/// <summary>
///  This function is elevate TokenUnivese. It is also capable of starting the
///  service for a delegation (when then current instance doesn't have
///  enough rights to create and start it).
/// </summary>
/// <param name="Handle">
///   A window handle to show message boxes that the system might produce while
///   executing this function.
/// </param>
/// <param name="StartService">
///   A boolean that determines whether the elevated copy of should
///   automatically call <see cref="ReSvcCreateService"/>.
/// </param>
/// <param name="IsSystemPlus">
///  Determines whether the service should obtain a SYSTEM+ token.
/// </param>
procedure ReSvcDelegate(Handle: HWND; StartService: Boolean;
  IsSystemPlus: Boolean);

/// <summary>
///  Starts service control dispatcher. This function should be called only if
///  the process was created by SCM.
/// </summary>
function ReSvcMain: Boolean;

implementation

uses
  Winapi.WinNt, Winapi.Svc, Winapi.WinBase, Winapi.ProcessThreadsApi,
  Winapi.Shell, Winapi.WinError, Ntapi.ntstatus, Ntapi.ntseapi, Ntapi.ntdef,
  Ntapi.ntobapi, Ntapi.ntpsapi, NtUtils.Objects, Ntapi.ntrtl, System.SysUtils,
  NtUtils.Snapshots.Processes, NtUtils.Tokens, NtUtils.Exceptions,
  NtUtils.Tokens.Misc;

procedure DebugOut(DebugMessage: String); inline;
begin
  {$IFDEF DEBUG}
  OutputDebugStringW(PChar(DebugMessage));
  {$ENDIF}
end;

{ Restart Service client functions }

procedure ReSvcCreateService(IsSystemPlus: Boolean);
var
  hSCM, hSvc: TScmHandle;
  SI : TStartupInfoW;
  CommandLine: String;
  Params: TServiceDynArgsW;
begin
  // Establish connection to the SCM
  hSCM := OpenSCManagerW(nil, nil, SC_MANAGER_CREATE_SERVICE);

  if hSCM = 0 then
    RaiseLastOsError;

  CommandLine := '"' + ParamStr(0) + '" ' + RESVC_PARAM;
  if IsSystemPlus then
    CommandLine := CommandLine + ' ' + RESVC_SYSPLUS_PARAM;

  // Create Run-as-system service
  hSvc := CreateServiceW(hSCM, RESVC_NAME, RESVC_DIPLAY_NAME,
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_DEMAND_START,
    SERVICE_ERROR_NORMAL, PWideChar(CommandLine), nil, nil, nil, nil, nil);

  CloseServiceHandle(hSCM);

  if hSvc = 0 then
    RaiseLastOSError;

  // Pass the service parameters (session and desktop) so it would know who
  // requested the restart action
  try
    // Query startup info to get desktop. This function does not fail.
    GetStartupInfoW(SI);

    // Allocate memory for parameters
    SetLength(Params, 2);
    Params[0] := PWideChar(IntToStr(RtlGetCurrentPeb.SessionId));
    Params[1] := SI.lpDesktop;

    // Start the service
    if not StartServiceW(hSvc, Length(Params), Params) then
      RaiseLastOSError;
  finally
    CloseServiceHandle(hSvc);
  end;
end;

procedure ReSvcDelegate(Handle: HWND; StartService: Boolean;
  IsSystemPlus: Boolean);
var
  ExecInfo: TShellExecuteInfoW;
begin
  FillChar(ExecInfo, SizeOf(ExecInfo), 0);
  with ExecInfo do
  begin
    cbSize := SizeOf(ExecInfo);
    Wnd := Handle;
    lpVerb := PWideChar('runas');
    lpFile := PWideChar(ParamStr(0));

    // The parameter states that the execution of the service was delegated
    if StartService then
    begin
      if IsSystemPlus then
        lpParameters := DELEGATE_PARAM_SYSPLUS
      else
        lpParameters := DELEGATE_PARAM;
    end;

    fMask := SEE_MASK_NOASYNC or SEE_MASK_UNICODE or SEE_MASK_FLAG_NO_UI;
    nShow := SW_SHOWNORMAL;
  end;
  if not ShellExecuteExW(ExecInfo) then
    RaiseLastOSError;
end;

{ Restart Service server functions }

procedure ReSvcServiceMain(dwNumServicesArgs: Cardinal;
  lpServiceArgVectors: PServiceArgsW) stdcall; forward;

var
  RESVC_SERVICE_TABLE: array [0 .. 1] of TServiceTableEntryW = (
      (lpServiceName: RESVC_NAME; lpServiceProc: ReSvcServiceMain),
      (lpServiceName: nil; lpServiceProc: nil)
    );

  ReSvcStatusHandle: THandle;
  ReSvcStatus: TServiceStatus = (
      dwServiceType:             SERVICE_WIN32_OWN_PROCESS;
      dwCurrentState:            SERVICE_RUNNING;
      dwControlsAccepted:        0;
      dwWin32ExitCode:           0;
      dwServiceSpecificExitCode: 0;
      dwCheckPoint:              0;
      dwWaitHint:                5000
    );

function ReSvcMain: Boolean;
begin
  Result := StartServiceCtrlDispatcherW(PServiceTableEntryW(@RESVC_SERVICE_TABLE));
end;

function ReSvcHandlerEx(dwControl: Cardinal; dwEventType: Cardinal;
  lpEventData: Pointer; lpContext: Pointer): Cardinal; stdcall;
begin
  if dwControl = SERVICE_CONTROL_INTERROGATE then
    Result := ERROR_SUCCESS
  else
    Result := ERROR_CALL_NOT_IMPLEMENTED;
end;

function TryGetCsrssToken: THandle;
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

/// <summary>
///  This routine is called from a service and spawns a new instans of
///  TokenUniverse in the specified session on the specified desktop.
///  If the caller didn't pass us these parameters we use session 0 and
///  WinSta0\Default.
/// </summary>
procedure ReSvcRunInSession(Session: Integer; Desktop: PWideChar);
var
  Token, NewToken: THandle;
  SIEX: TStartupInfoExW;
  PI: TProcessInformation;
  BasicInfo: TProcessBasinInformation;
  Timeout: TLargeInteger;
begin
  Token := 0;
  NewToken := 0;
  try
    // We are already running as SYSTEM, but if we want to obtain a rare
    // `SeCreateTokenPrivilege` (aka SYSTEM+ token) we need to steal it from
    // csrss.exe
    if ParamStr(2) = RESVC_SYSPLUS_PARAM then
      Token := TryGetCsrssToken
    else
      NtxOpenProcessToken(Token, NtCurrentProcess, TOKEN_DUPLICATE,
        0).RaiseOnError;

    // Duplicate
    NtxDuplicateToken(NewToken, Token, TOKEN_ADJUST_DEFAULT or
      TOKEN_ADJUST_SESSIONID or TOKEN_QUERY or TOKEN_DUPLICATE or
      TOKEN_ASSIGN_PRIMARY, TokenPrimary, SecurityImpersonation,
      False).RaiseOnError;

    // Change session
    NtxCheck(NtSetInformationToken(NewToken, TokenSessionId, @Session,
      SizeOf(Session)), NtxFormatTokenSet(TokenSessionId));

    FillChar(PI, SizeOf(PI), 0);
    FillChar(SIEX, SizeOf(SIEX), 0);
    SIEX.StartupInfo.cb := SizeOf(SIEX.StartupInfo);

    // Use the specified desktop
    if Assigned(Desktop) then
      SIEX.StartupInfo.lpDesktop := Desktop
    else
      SIEX.StartupInfo.lpDesktop := 'WinSta0\Default';

    // Launch
    if not CreateProcessAsUserW(NewToken, PWideChar(ParamStr(0)), nil,
      nil, nil, False, 0, nil, nil, SIEX, PI) then
      RaiseLastOSError
    else
    begin
      {$IFDEF DEBUG}

      // Check that the process didn't crash immediately
      Timeout.QuadPart := 200;

      case NtWaitForSingleObject(PI.hProcess, False, Timeout) of
        STATUS_TIMEOUT: ; // Nothing
        STATUS_SUCCESS:
          begin
            DebugOut('Abnormal process termination');

            if not NT_SUCCESS(NtQueryInformationProcess(PI.hProcess,
              ProcessBasicInformation, @BasicInfo, SizeOf(BasicInfo), nil)) then
              DebugOut('Unable to determine the exit code')
            else
              DebugOut(PWideChar('Exit code: 0x' +
                IntToHex(BasicInfo.ExitStatus, 8)));
          end;
        else
          DebugOut('Wait for the new process failed');
      end;
      {$ENDIF}
      NtxSafeClose(PI.hProcess);
      NtxSafeClose(PI.hThread);
    end;
  finally
    if Token <> 0 then
      NtxSafeClose(Token);

    if NewToken <> 0 then
      NtxSafeClose(NewToken);
  end;
end;

/// <summary>
///  The main service routine.
/// </summary>
procedure ReSvcServiceMain(dwNumServicesArgs: Cardinal;
  lpServiceArgVectors: PServiceArgsW) stdcall;
var
  hSCM, hSvc: TScmHandle;
  SeriveArgs: PServiceArgsW;
  Session, i: Integer;
begin
  // Register service control handler
  ReSvcStatusHandle := RegisterServiceCtrlHandlerExW(RESVC_NAME,
    ReSvcHandlerEx, nil);

  // Report running status
  SetServiceStatus(ReSvcStatusHandle, ReSvcStatus);

  // Delete self
  hSCM := OpenSCManagerW(nil, nil, SC_MANAGER_CONNECT);
  if hSCM <> 0 then
  begin
    hSvc := OpenServiceW(hSCM, RESVC_NAME, _DELETE);
    if hSvc <> 0 then
    begin
      DeleteService(hSvc);
      CloseServiceHandle(hSvc);
    end;
    CloseServiceHandle(hSCM);
  end;

  // Start a copy in the specified session
  try
    SeriveArgs := PServiceArgsW(lpServiceArgVectors);

    {$IFDEF DEBUG}
    OutputDebugStringW('Service parameters: ');
    for i := 0 to dwNumServicesArgs - 1 do
      OutputDebugStringW(SeriveArgs[i]);
    {$ENDIF}

    if (dwNumServicesArgs = 3) and TryStrToInt(SeriveArgs[1], Session) then
      ReSvcRunInSession(Session, SeriveArgs[2])
    else
      ReSvcRunInSession(0, nil);
  except
    on E: Exception do
      DebugOut(E.ClassName + ': ' + E.Message);
  end;

  // Report that we have finished
  ReSvcStatus.dwCurrentState := SERVICE_STOPPED;
  SetServiceStatus(ReSvcStatusHandle, ReSvcStatus);
end;

end.

