unit TU.RestartSvc;

interface

const
  RESVC_PARAM = '/service';
  RESVC_NAME = 'TokenUniverseSvc';
  RESVC_DIPLAY_NAME = 'Token Universe Run-As-System Service';

  DELEGATE_PARAM = '/delegate';

procedure ReSvcCreateService;
procedure ReSvcDelegate(Handle: THandle; StartService: Boolean);

function ReSvcServerMain: Boolean;

implementation

uses
  System.SysUtils, Winapi.Windows, Winapi.WinSvc, Winapi.ShellApi,
  TU.Tokens, TU.DebugLog;

{ Restart Service client functions }

procedure ReSvcCreateService;
var
  hSCM, hSvc: SC_HANDLE;
  SessionParam: PWideChar;
begin
  hSCM := OpenSCManagerW(nil, nil, SC_MANAGER_CREATE_SERVICE);

  if hSCM = 0 then
    RaiseLastOsError;

  hSvc := CreateServiceW(hSCM, RESVC_NAME, RESVC_DIPLAY_NAME,
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_DEMAND_START,
    SERVICE_ERROR_NORMAL, PWideChar('"' + ParamStr(0) + '" ' + RESVC_PARAM),
    nil, nil, nil, nil, nil);

  CloseServiceHandle(hSCM);

  if hSvc = 0 then
    RaiseLastOSError;

  try
    with TToken.CreateFromCurrent do
    begin
      SessionParam := PWideChar(TryGetSession.GetValueOrRaise.ToString);
      Free;
    end;

    if not StartServiceW(hSvc, 1, SessionParam) then
      RaiseLastOSError;
  finally
    CloseServiceHandle(hSvc);
  end;
end;

procedure ReSvcDelegate(Handle: THandle; StartService: Boolean);
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
    if StartService then
      lpParameters := DELEGATE_PARAM;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_UNICODE or SEE_MASK_FLAG_NO_UI;
    nShow := SW_SHOWNORMAL;
  end;
  if not ShellExecuteExW(@ExecInfo) then
    RaiseLastOSError;
end;

{ Restart Service server functions }

type
  TServiceArgs = array [Byte] of PWideChar;
  PServiceArgs = ^TServiceArgs;

procedure ReSvcServiceMain(dwNumServicesArgs: DWORD;
  lpServiceArgVectors: PLPWSTR) stdcall; forward;

function StartServiceCtrlDispatcherW(lpServiceStartTable: PServiceTableEntryW):
  BOOL; stdcall; external advapi32;

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

function ReSvcServerMain: Boolean;
begin
  Result := StartServiceCtrlDispatcherW(@RESVC_SERVICE_TABLE);
end;

function ReSvcHandlerEx(dwControl: DWORD; dwEventType: DWORD;
  lpEventData: LPVOID; lpContext: LPVOID): DWORD; stdcall;
begin
  if dwControl = SERVICE_CONTROL_INTERROGATE then
    Result := NO_ERROR
  else
    Result := ERROR_CALL_NOT_IMPLEMENTED;
end;

procedure ReSvcRunInSession(Session: Integer);
var
  Token, NewToken: TToken;
  SI: TStartupInfoW;
  PI: TProcessInformation;
begin
  Token := nil;
  NewToken := nil;

  try
    Token := TToken.CreateFromCurrent;

    NewToken := TToken.CreateDuplicate(Token,
      TOKEN_ADJUST_DEFAULT or TOKEN_ADJUST_SESSIONID or
      TOKEN_QUERY or TOKEN_DUPLICATE or TOKEN_ASSIGN_PRIMARY, ttPrimary);
    NewToken.Session := Session;

    FillChar(PI, SizeOf(PI), 0);
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);

    if not CreateProcessAsUserW(NewToken.Handle, PWideChar(ParamStr(0)), nil,
      nil, nil, False, 0, nil, nil, SI, PI) then
      RaiseLastOSError;
  finally
    Token.Free;
    NewToken.Free;
  end;
end;

procedure ReSvcServiceMain(dwNumServicesArgs: DWORD;
  lpServiceArgVectors: PLPWSTR) stdcall;
var
  hSCM, hSvc: SC_HANDLE;
  Session: Integer;
begin
  ReSvcStatusHandle := RegisterServiceCtrlHandlerExW(RESVC_NAME,
    ReSvcHandlerEx, nil);

  SetServiceStatus(ReSvcStatusHandle, ReSvcStatus); // Report running

  // Delete self
  hSCM := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if hSCM <> 0 then
  begin
    hSvc := OpenService(hSCM, RESVC_NAME, _DELETE);
    if hSvc <> 0 then
    begin
      DeleteService(hSvc);
      CloseServiceHandle(hSvc);
    end;
    CloseServiceHandle(hSCM);
  end;

  // Start a copy in the specified session
  try
    if (dwNumServicesArgs = 2) and TryStrToInt(PServiceArgs(lpServiceArgVectors)
      [1], Session) then
      ReSvcRunInSession(Session);
  except
    on E: Exception do
      TProcmonLogger.SendDbgMessage(E.ClassName + ': ' + E.Message);
  end;

  ReSvcStatus.dwCurrentState := SERVICE_STOPPED;
  SetServiceStatus(ReSvcStatusHandle, ReSvcStatus);
end;

end.

