unit NtUtils.Svc;

interface

uses
  Winapi.WinNt, NtUtils.Exceptions, Winapi.Svc;

type
  TScmHandle = Winapi.Svc.TScmHandle;

  TServiceConfig = record
    ServiceType: Cardinal;
    StartType: Cardinal;
    ErrorControl: Cardinal;
    TagId: Cardinal;
    BinaryPathName: String;
    LoadOrderGroup: String;
    ServiceStartName: String;
    DisplayName: String;
  end;

// Open a handle to SCM
function ScmxConnect(out hScm: TScmHandle; DesiredAccess: TAccessMask):
  TNtxStatus;

// Open a service
function ScmxOpenService(out hSvc: TScmHandle; ServiceName: String;
  DesiredAccess: TAccessMask): TNtxStatus;

// Create a service
function ScmxCreateService(out hSvc: TScmHandle; CommandLine, ServiceName,
   DisplayName: String; StartType: Cardinal = SERVICE_DEMAND_START): TNtxStatus;

// Start a service
function ScmxStartService(hSvc: TScmHandle): TNtxStatus; overload;
function ScmxStartService(hSvc: TScmHandle; Parameters: TArray<String>):
  TNtxStatus; overload;

// Delete a service
function ScmxDeleteService(hSvc: TScmHandle): TNtxStatus;

// Close SCM/service handle
function ScmxClose(var hObject: TScmHandle): Boolean;

// Query service config
function ScmxQueryConfigService(hSvc: TScmHandle; out Config: TServiceConfig)
  : TNtxStatus;

// Query service status and process information
function ScmxQueryProcessStatusService(hSvc: TScmHandle;
  out Info: TServiceStatusProcess): TNtxStatus;

implementation

function ScmxConnect(out hScm: TScmHandle; DesiredAccess: TAccessMask):
  TNtxStatus;
begin
  hScm := OpenSCManagerW(nil, nil, DesiredAccess);
  Result.Win32Result := (hScm <> 0);

  Result.Location := 'OpenSCManagerW';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objScmManager;
end;

function ScmxOpenService(out hSvc: TScmHandle; ServiceName: String;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hScm: TScmHandle;
begin
  // Connect to SCM
  Result := ScmxConnect(hScm, SC_MANAGER_CONNECT);

  if not Result.IsSuccess then
    Exit;

  // Create service
  hSvc := OpenServiceW(hScm, PWideChar(ServiceName), DesiredAccess);
  Result.Win32Result := (hSvc <> 0);

  Result.Location := 'OpenServiceW';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objScmService;

  ScmxClose(hScm);
end;

function ScmxCreateService(out hSvc: TScmHandle; CommandLine, ServiceName,
   DisplayName: String; StartType: Cardinal): TNtxStatus;
var
  hScm: TScmHandle;
begin
  // Connect to SCM
  Result := ScmxConnect(hScm, SC_MANAGER_CREATE_SERVICE);

  if not Result.IsSuccess then
    Exit;

  // Create service
  hSvc := CreateServiceW(hScm, PWideChar(ServiceName), PWideChar(DisplayName),
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, StartType,
    SERVICE_ERROR_NORMAL, PWideChar(CommandLine), nil, nil, nil, nil, nil);

  Result.Location := 'CreateServiceW';
  Result.Win32Result := (hSvc <> 0);

  ScmxClose(hScm);
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
  Result.Win32Result := StartServiceW(hSvc, Length(Params), Params);
end;

function ScmxDeleteService(hSvc: TScmHandle): TNtxStatus;
begin
  Result.Location := 'DeleteService';
  Result.Win32Result := DeleteService(hSvc);
end;

function ScmxClose(var hObject: TScmHandle): Boolean;
begin
  Result := CloseServiceHandle(hObject);
  hObject := 0;
end;

function ScmxQueryConfigService(hSvc: TScmHandle; out Config: TServiceConfig)
  : TNtxStatus;
var
  Buffer: PQueryServiceConfigW;
  BufferSize, Required: Cardinal;
begin
  Result.Location := 'QueryServiceConfigW';

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

  Config.ServiceType := Buffer.dwServiceType;
  Config.StartType := Buffer.dwStartType;
  Config.ErrorControl := Buffer.dwErrorControl;
  Config.TagId := Buffer.dwTagId;
  Config.BinaryPathName := String(Buffer.lpBinaryPathName);
  Config.LoadOrderGroup := String(Buffer.lpLoadOrderGroup);
  Config.ServiceStartName := String(Buffer.lpServiceStartName);
  Config.DisplayName := String(Buffer.lpDisplayName);

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

  Result.Win32Result := QueryServiceStatusEx(hSvc, ScStatusProcessInfo,
    @Info, SizeOf(Info), Required);
end;

end.
