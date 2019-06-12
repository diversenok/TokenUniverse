unit NtUtils.Svc;

interface

uses
  Winapi.WinNt, NtUtils.Exceptions, Winapi.Svc;

type
  TScmHandle = Winapi.Svc.TScmHandle;
  TStringArray = Winapi.WinNt.TStringArray;

function ScmxCreateService(out hSvc: TScmHandle; CommandLine, ServiceName,
   DisplayName: String; StartType: Cardinal = SERVICE_DEMAND_START): TNtxStatus;

function ScmxStartService(hSvc: TScmHandle): TNtxStatus; overload;
function ScmxStartService(hSvc: TScmHandle; Parameters: TStringArray):
  TNtxStatus; overload;

function ScmxDeleteService(hSvc: TScmHandle): TNtxStatus;

function ScmxClose(var hObject: TScmHandle): Boolean;

implementation

function ScmxCreateService(out hSvc: TScmHandle; CommandLine, ServiceName,
   DisplayName: String; StartType: Cardinal): TNtxStatus;
var
  hScm: TScmHandle;
begin
  // Connect to SCM
  hScm := OpenSCManagerW(nil, nil, SC_MANAGER_CREATE_SERVICE);

  Result.Location := 'OpenSCManagerW';
  Result.Win32Result := (hScm <> 0);

  if not Result.IsSuccess then
    Exit;

  // Create service
  hSvc := CreateServiceW(hScm, PWideChar(ServiceName), PWideChar(DisplayName),
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, StartType,
    SERVICE_ERROR_NORMAL, PWideChar(CommandLine), nil, nil, nil, nil, nil);

  Result.Location := 'CreateServiceW';
  Result.Win32Result := (hSvc <> 0);

  CloseServiceHandle(hScm);
end;

function ScmxStartService(hSvc: TScmHandle): TNtxStatus; overload;
var
  Parameters: TStringArray;
begin
  SetLength(Parameters, 0);
  Result := ScmxStartService(hSvc, Parameters);
end;

function ScmxStartService(hSvc: TScmHandle; Parameters: TStringArray):
  TNtxStatus;
var
  i: Integer;
  Params: TServiceDynArgsW;
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
