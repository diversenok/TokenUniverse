unit TU.DebugLog;

interface

type
  /// <summary>
  ///  A class to send debug messages to Process Monitor.
  /// </summary>
  TProcmonLogger = class
  protected
    class var hDevice: THandle;
    class procedure Init;
  public
    /// <summary> Sends a message to Process Monitor driver. </summary>
    /// <remarks> The message length is limited up to 2048 characters. </remarks>
    /// <returns>
    ///  <para> <c>True</c> if the message was successfully sent; </para>
    ///  <para> <c>False</c> otherwise. </para>
    ///</returns>
    class function SendDbgMessage(Message: String): Boolean;
    class procedure ReleaseDevice;
  end;

implementation

uses
  Winapi.Windows;

{ TProcmonLogger }

class procedure TProcmonLogger.Init;
begin
  // Open Process Monitor driver
  hDevice := CreateFile(PChar('\\.\Global\ProcmonDebugLogger'),
    GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or
    FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
end;

class procedure TProcmonLogger.ReleaseDevice;
begin
  try
    if (hDevice <> 0) and (hDevice <> INVALID_HANDLE_VALUE) then
      CloseHandle(hDevice);

    hDevice := 0;
  except
    ;
  end;
end;

class function TProcmonLogger.SendDbgMessage(Message: String): Boolean;
const
  FILE_DEVICE_PROCMON_LOG = $00009535;
  FUNCTION_ID = $81;
  IOCTL_EXTERNAL_LOG_DEBUGOUT = Cardinal((FILE_DEVICE_PROCMON_LOG shl 16) or
    (FILE_WRITE_ACCESS shl 14) or (FUNCTION_ID shl 2) or METHOD_BUFFERED);
var
  BytesReturned: Cardinal;
begin
  if hDevice = 0 then
    Init;

  Result := False;
  if hDevice <> INVALID_HANDLE_VALUE then
    Result := DeviceIoControl(hDevice, IOCTL_EXTERNAL_LOG_DEBUGOUT,
      PWideChar(Message), Length(Message) * SizeOf(WideChar), nil, 0,
      BytesReturned, nil);
end;

initialization

finalization
  TProcmonLogger.ReleaseDevice;
end.
