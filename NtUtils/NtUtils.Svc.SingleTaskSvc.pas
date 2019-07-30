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
      ServiceType:             SERVICE_WIN32_OWN_PROCESS;
      CurrentState:            ServiceRunning;
      ControlsAccepted:        0;
      Win32ExitCode:           0;
      ServiceSpecificExitCode: 0;
      CheckPoint:              0;
      WaitHint:                5000
    );

function SvcxHandlerEx(Control: TServiceControl; EventType: Cardinal;
  EventData: Pointer; Context: Pointer): Cardinal; stdcall;
begin
  if Control = ServiceControlInterrogate then
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

  for i := 0 to High(Parameters) do
    Parameters[i] := String(lpServiceArgVectors{$R-}[i]{$R+});

  {$IFDEF DEBUG}
  OutputDebugStringW(PWideChar(ParamStr(0)));
  OutputDebugStringW('Service parameters: ');

  for i := 0 to dwNumServicesArgs - 1 do
    OutputDebugStringW(lpServiceArgVectors{$R-}[i]{$R+});
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
  SvcxStatus.CurrentState := ServiceStopped;
  SetServiceStatus(SvcxStatusHandle, SvcxStatus);
end;

function SvcxMain(ServiceName: String; Payload: TSvcxPayload): Boolean;
var
  ServiceTable: array [0 .. 1] of TServiceTableEntryW;
begin
  SvcxName := ServiceName;
  SvcxPayload := Payload;

  ServiceTable[0].ServiceName := PWideChar(SvcxName);
  ServiceTable[0].ServiceProc := SvcxServiceMain;
  ServiceTable[1].ServiceName := nil;
  ServiceTable[1].ServiceProc := nil;

  Result := StartServiceCtrlDispatcherW(PServiceTableEntryW(@ServiceTable));
end;

end.
