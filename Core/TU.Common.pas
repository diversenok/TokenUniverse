unit TU.Common;

interface

uses
  TU.NativeAPI;

function Win32Check(RetVal: LongBool; Where: String): LongBool;
procedure Win32CheckBuffer(BufferSize: Cardinal; Where: String);
function NativeCheck(Status: NTSTATUS; Where: String): Boolean;

implementation

uses
  System.SysUtils, Winapi.WIndows;

resourcestring
  SOSError = 'System Error in %s' + #$D#$A#$D#$A +
    'Code:  0x%x.' + #$D#$A#$D#$A + '%s';

function Win32Check(RetVal: LongBool; Where: String): LongBool;
begin
  if not RetVal then
    raise EOSError.CreateResFmt(@SOSError, [Where, GetLastError,
      SysErrorMessage(GetLastError)]);
  Result := True;
end;

procedure Win32CheckBuffer(BufferSize: Cardinal; Where: String);
begin
  // TODO: What about ERROR_BUFFER_OVERFLOW and ERROR_INVALID_USER_BUFFER?
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BufferSize = 0) then
    raise EOSError.CreateResFmt(@SOSError, [Where, GetLastError,
      SysErrorMessage(GetLastError)]);
end;

function NativeCheck(Status: NTSTATUS; Where: String): Boolean;
begin
  if Status <> STATUS_SUCCESS then
    raise EOSError.CreateResFmt(@SOSError, [Where, Status,
      SysErrorMessage(Status, GetModuleHandle('ntdll.dll'))]);
  Result := Status = STATUS_SUCCESS;
end;

end.
