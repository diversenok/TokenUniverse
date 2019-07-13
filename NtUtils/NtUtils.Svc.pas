unit NtUtils.Svc;

interface

uses
  Winapi.WinNt, NtUtils.Exceptions, Winapi.Svc;

type
  TScmHandle = Winapi.Svc.TScmHandle;

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

end.
