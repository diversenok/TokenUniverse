unit NtUtils.Svc.SingleTaskSvc;

interface

uses
  NtUtils.Svc;

type
  TSvcxPayload = procedure(ScvParams: TArray<String>);

// Starts service control dispatcher.
function SvcxMain(ServiceName: String; Payload: TSvcxPayload): Boolean;

implementation

uses
  Winapi.Svc, Winapi.WinError, Winapi.WinBase, System.SysUtils;

var
  SvcxName: String;
  SvcxPayload: TSvcxPayload = nil;

  SvcxStatusHandle: THandle;
  SvcxStatus: TServiceStatus = (
      dwServiceType:             SERVICE_WIN32_OWN_PROCESS;
      dwCurrentState:            SERVICE_RUNNING;
      dwControlsAccepted:        0;
      dwWin32ExitCode:           0;
      dwServiceSpecificExitCode: 0;
      dwCheckPoint:              0;
      dwWaitHint:                5000
    );

function SvcxHandlerEx(dwControl: Cardinal; dwEventType: Cardinal;
  lpEventData: Pointer; lpContext: Pointer): Cardinal; stdcall;
begin
  if dwControl = SERVICE_CONTROL_INTERROGATE then
    Result := ERROR_SUCCESS
  else
    Result := ERROR_CALL_NOT_IMPLEMENTED;
end;

procedure SvcxServiceMain(dwNumServicesArgs: Integer;
  lpServiceArgVectors: PServiceArgsW) stdcall;
var
  i: Integer;
  Parameters: TArray<String>;
begin
  // Register service control handler
  SvcxStatusHandle := RegisterServiceCtrlHandlerExW(PWideChar(SvcxName),
    SvcxHandlerEx, nil);

  // Report running status
  SetServiceStatus(SvcxStatusHandle, SvcxStatus);

  // Prepare passed parameters
  SetLength(Parameters, dwNumServicesArgs);
  for i := 0 to dwNumServicesArgs - 1 do
    Parameters[i] := String(lpServiceArgVectors[i]);

  {$IFDEF DEBUG}
  OutputDebugStringW(PWideChar(ParamStr(0)));
  OutputDebugStringW('Service parameters: ');
  for i := 0 to dwNumServicesArgs - 1 do
    OutputDebugStringW(lpServiceArgVectors[i]);
  {$ENDIF}

  // Call the payload
  try
    if Assigned(SvcxPayload) then
      SvcxPayload(Parameters);
  except
    on E: Exception do
      OutputDebugStringW(PWideChar(E.ClassName + ': ' + E.Message));
  end;

  // Report that we have finished
  SvcxStatus.dwCurrentState := SERVICE_STOPPED;
  SetServiceStatus(SvcxStatusHandle, SvcxStatus);
end;

function SvcxMain(ServiceName: String; Payload: TSvcxPayload): Boolean;
var
  ServiceTable: array [0 .. 1] of TServiceTableEntryW;
begin
  SvcxName := ServiceName;
  SvcxPayload := Payload;

  ServiceTable[0].lpServiceName := PWideChar(SvcxName);
  ServiceTable[0].lpServiceProc := SvcxServiceMain;
  ServiceTable[1].lpServiceName := nil;
  ServiceTable[1].lpServiceProc := nil;

  Result := StartServiceCtrlDispatcherW(PServiceTableEntryW(@ServiceTable));
end;

end.
