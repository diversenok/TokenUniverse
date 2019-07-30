unit NtUtils.Svc;

interface

uses
  Winapi.WinNt, NtUtils.Exceptions, Winapi.Svc;

type
  TScmHandle = Winapi.Svc.TScmHandle;

  TServiceConfig = record
    ServiceType: Cardinal;
    StartType: TServiceStartType;
    ErrorControl: TServiceErrorControl;
    TagId: Cardinal;
    BinaryPathName: String;
    LoadOrderGroup: String;
    ServiceStartName: String;
    DisplayName: String;
  end;

// Open a handle to SCM
function ScmxConnect(out hScm: TScmHandle; DesiredAccess: TAccessMask;
  ServerName: String = ''): TNtxStatus;

// Close SCM/service handle
function ScmxClose(var hObject: TScmHandle): Boolean;

// Open a service
function ScmxOpenService(out hSvc: TScmHandle; hScm: TScmHandle;
  ServiceName: String; DesiredAccess: TAccessMask): TNtxStatus;

function ScmxOpenServiceLocal(out hSvc: TScmHandle; ServiceName: String;
  DesiredAccess: TAccessMask): TNtxStatus;

// Create a service
function ScmxCreateService(out hSvc: TScmHandle; hScm: TScmHandle; CommandLine,
  ServiceName, DisplayName: String; StartType: TServiceStartType =
  ServiceDemandStart): TNtxStatus;

function ScmxCreateServiceLocal(out hSvc: TScmHandle; CommandLine, ServiceName,
  DisplayName: String; StartType: TServiceStartType = ServiceDemandStart)
  : TNtxStatus;

// Start a service
function ScmxStartService(hSvc: TScmHandle): TNtxStatus; overload;
function ScmxStartService(hSvc: TScmHandle; Parameters: TArray<String>):
  TNtxStatus; overload;

// Send a control to a service
function ScmxControlService(hSvc: TScmHandle; Control: TServiceControl;
  out ServiceStatus: TServiceStatus): TNtxStatus;

// Delete a service
function ScmxDeleteService(hSvc: TScmHandle): TNtxStatus;

// Query service config
function ScmxQueryConfigService(hSvc: TScmHandle; out Config: TServiceConfig)
  : TNtxStatus;

// Query service status and process information
function ScmxQueryProcessStatusService(hSvc: TScmHandle;
  out Info: TServiceStatusProcess): TNtxStatus;

implementation

uses
  NtUtils.Access.Expected;

function ScmxConnect(out hScm: TScmHandle; DesiredAccess: TAccessMask;
  ServerName: String): TNtxStatus;
var
  pServerName: PWideChar;
begin
  if ServerName <> '' then
    pServerName := PWideChar(ServerName)
  else
    pServerName := nil;

  Result.Location := 'OpenSCManagerW';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objScmManager;

  hScm := OpenSCManagerW(pServerName, nil, DesiredAccess);
  Result.Win32Result := (hScm <> 0);
end;

function ScmxClose(var hObject: TScmHandle): Boolean;
begin
  Result := CloseServiceHandle(hObject);
  hObject := 0;
end;

function ScmxOpenService(out hSvc: TScmHandle; hScm: TScmHandle;
  ServiceName: String; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'OpenServiceW';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objScmService;
  Result.LastCall.Expects(SC_MANAGER_CONNECT, objScmManager);

  hSvc := OpenServiceW(hScm, PWideChar(ServiceName), DesiredAccess);
  Result.Win32Result := (hSvc <> 0);
end;

function ScmxOpenServiceLocal(out hSvc: TScmHandle; ServiceName: String;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hScm: TScmHandle;
begin
  Result := ScmxConnect(hScm, SC_MANAGER_CONNECT);

  if Result.IsSuccess then
  begin
    Result := ScmxOpenService(hSvc, hScm, ServiceName, DesiredAccess);
    ScmxClose(hScm);
  end;
end;

function ScmxCreateService(out hSvc: TScmHandle; hScm: TScmHandle; CommandLine,
  ServiceName, DisplayName: String; StartType: TServiceStartType): TNtxStatus;
begin
  Result.Location := 'CreateServiceW';
  Result.LastCall.Expects(SC_MANAGER_CREATE_SERVICE, objScmManager);

  hSvc := CreateServiceW(hScm, PWideChar(ServiceName), PWideChar(DisplayName),
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, StartType,
    ServiceErrorNormal, PWideChar(CommandLine), nil, nil, nil, nil, nil);
  Result.Win32Result := (hSvc <> 0);
end;

function ScmxCreateServiceLocal(out hSvc: TScmHandle; CommandLine, ServiceName,
  DisplayName: String; StartType: TServiceStartType): TNtxStatus;
var
  hScm: TScmHandle;
begin
  Result := ScmxConnect(hScm, SC_MANAGER_CREATE_SERVICE);

  if Result.IsSuccess then
  begin
    Result := ScmxCreateService(hSvc, hScm, CommandLine, ServiceName,
      DisplayName, StartType);
    ScmxClose(hScm);
  end;
end;

function ScmxStartService(hSvc: TScmHandle): TNtxStatus; overload;
var
  Parameters: TArray<String>;
begin
  SetLength(Parameters, 0);
  Result := ScmxStartService(hSvc, Parameters);
end;

function ScmxStartService(hSvc: TScmHandle; Parameters: TArray<String>):
  TNtxStatus;
var
  i: Integer;
  Params: TArray<PWideChar>;
begin
  SetLength(Params, Length(Parameters));

  for i := 0 to High(Params) do
    Params[i] := PWideChar(Parameters[i]);

  Result.Location := 'StartServiceW';
  Result.LastCall.Expects(SERVICE_START, objScmService);

  Result.Win32Result := StartServiceW(hSvc, Length(Params), Params);
end;

function ScmxControlService(hSvc: TScmHandle; Control: TServiceControl;
  out ServiceStatus: TServiceStatus): TNtxStatus;
begin
  Result.Location := 'ControlService';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(Control);
  Result.LastCall.InfoClassType := TypeInfo(TServiceControl);
  RtlxComputeServiceControlAccess(Result.LastCall, Control);

  Result.Win32Result := ControlService(hSvc, Control, ServiceStatus);
end;

function ScmxDeleteService(hSvc: TScmHandle): TNtxStatus;
begin
  Result.Location := 'DeleteService';
  Result.LastCall.Expects(_DELETE, objScmService);
  Result.Win32Result := DeleteService(hSvc);
end;

function ScmxQueryConfigService(hSvc: TScmHandle; out Config: TServiceConfig)
  : TNtxStatus;
var
  Buffer: PQueryServiceConfigW;
  BufferSize, Required: Cardinal;
begin
  Result.Location := 'QueryServiceConfigW';
  Result.LastCall.Expects(SERVICE_QUERY_CONFIG, objScmService);

  BufferSize := 0;
  repeat
    Buffer := AllocMem(BufferSize);

    Required := 0;
    Result.Win32Result := QueryServiceConfigW(hSvc, Buffer, BufferSize,
      Required);

    if not Result.IsSuccess then
      FreeMem(Buffer);

  until not NtxExpandBuffer(Result, BufferSize, Required);

  if not Result.IsSuccess then
    Exit;

  Config.ServiceType := Buffer.ServiceType;
  Config.StartType := Buffer.StartType;
  Config.ErrorControl := Buffer.ErrorControl;
  Config.TagId := Buffer.TagId;
  Config.BinaryPathName := String(Buffer.BinaryPathName);
  Config.LoadOrderGroup := String(Buffer.LoadOrderGroup);
  Config.ServiceStartName := String(Buffer.ServiceStartName);
  Config.DisplayName := String(Buffer.DisplayName);

  FreeMem(Buffer);
end;

function ScmxQueryProcessStatusService(hSvc: TScmHandle;
  out Info: TServiceStatusProcess): TNtxStatus;
var
  Required: Cardinal;
begin
  Result.Location := 'QueryServiceStatusEx';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(ScStatusProcessInfo);
  Result.LastCall.InfoClassType := TypeInfo(TScStatusType);
  Result.LastCall.Expects(SERVICE_QUERY_STATUS, objScmService);

  Result.Win32Result := QueryServiceStatusEx(hSvc, ScStatusProcessInfo,
    @Info, SizeOf(Info), Required);
end;

end.
