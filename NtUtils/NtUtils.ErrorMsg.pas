unit NtUtils.ErrorMsg;

interface

uses
  Ntapi.ntdef;

// Converts common error codes to strings, for example:
//      1314 => "ERROR_PRIVILEGE_NOT_HELD"
// $C00000BB =>     "STATUS_NOT_SUPPORTED"
// Returns empty string if the name is not found.

function NtxpWin32ErrorToString(Code: Cardinal): String;
function NtxpStatusToString(Status: NTSTATUS): String;

// The same as above, but on unknown errors returns their decimal/hexadecimal
// representations.
function NtxWin32ErrorToString(Code: Cardinal): String;
function NtxStatusToString(Status: NTSTATUS): String;

// Converts common error codes to their short descriptions, for example:
//      1314 => "Privilege not held"
// $C00000BB =>      "Not supported"
// Returns empty string if the name is not found.
function NtxWin32ErrorDescription(Code: Cardinal): String;
function NtxStatusDescription(Status: NTSTATUS): String;

// Retrieves a full description of a native error.
function NtxFormatErrorMessage(Status: NTSTATUS): String;

implementation

uses
  Ntapi.ntldr, Winapi.WinBase, System.SysUtils, DelphiUtils.Strings;

{$R 'NtUtils.ErrorMsg.res' 'NtUtils.ErrorMsg.rc'}

const
  {
    The resource file contains the names of some common Win32 Errors and
    NTSTATUS values. To fit into the format of the .rc file each category
    (i.e. Win32, NT Success, NT Information, NT Warning, and NT Error) was
    shifted, so it starts from the values listed above. Each group of values
    can contain the maximum count of RC_STATUS_EACH_MAX - 1 items to make sure
    they don't overlap.
  }
  RC_STATUS_SIFT_WIN32 = $8000;
  RC_STATUS_SIFT_NT_SUCCESS = $9000; // See ERROR_SEVERITY_SUCCESS
  RC_STATUS_SIFT_NT_INFO = $A000;    // See ERROR_SEVERITY_INFORMATIONAL
  RC_STATUS_SIFT_NT_WARNING = $B000; // See ERROR_SEVERITY_WARNING
  RC_STATUS_SIFT_NT_ERROR = $C000;   // See ERROR_SEVERITY_ERRORW
  RC_STATUS_EACH_MAX = $1000;

function NtxpWin32ErrorToString(Code: Cardinal): String;
var
  Buf: PWideChar;
begin
  // Make sure the error is within the range
  if Code >= RC_STATUS_EACH_MAX then
    Exit('');

  // Shift it to obtain the resource index
  Code := Code or RC_STATUS_SIFT_WIN32;

  // Extract the string representation
  SetString(Result, Buf, LoadStringW(HInstance, Code, Buf));
end;

function NtxpStatusToString(Status: NTSTATUS): String;
var
  ResIndex: Cardinal;
  Buf: PWideChar;
begin
  // Clear high bits that indicate status category
  ResIndex := Status and $3FFFFFFF;

  // Make sure the substatus is within the range
  if ResIndex >= RC_STATUS_EACH_MAX then
    Exit('');

  // Shift it to obtain the resource index
  case (Status shr 30) of
    0: ResIndex := ResIndex or RC_STATUS_SIFT_NT_SUCCESS;
    1: ResIndex := ResIndex or RC_STATUS_SIFT_NT_INFO;
    2: ResIndex := ResIndex or RC_STATUS_SIFT_NT_WARNING;
    3: ResIndex := ResIndex or RC_STATUS_SIFT_NT_ERROR;
  else
    Assert(False);
  end;

  // Extract the string representation
  SetString(Result, Buf, LoadStringW(HInstance, ResIndex, Buf));
end;

function NtxWin32ErrorToString(Code: Cardinal): String;
begin
  Result := NtxpWin32ErrorToString(Code);
  if Result = '' then
    Result := IntToStr(Code);
end;

function NtxStatusToString(Status: NTSTATUS): String;
begin
  // Check if it's a fake status based on a Win32 error.
  // In this case use "ERROR_SOMETHING_WENT_WRONG" messages.

  if NT_NTWIN32(Status) then
    Result := NtxpWin32ErrorToString(WIN32_FROM_NTSTATUS(Status))
  else
    Result := NtxpStatusToString(Status);

  if Result = '' then
    Result := IntToHexEx(Status, 8);
end;

function NtxWin32ErrorDescription(Code: Cardinal): String;
begin
  // We query the code name which looks like "ERROR_SOMETHING_WENT_WRONG"
  // and prettify it so it appears like "Something went wrong"

  Result := NtxpWin32ErrorToString(Code);

  if Result = '' then
    Exit;

  Result := PrettifyCapsUnderscore('ERROR_', Result);
end;

function NtxStatusDescription(Status: NTSTATUS): String;
begin
  if NT_NTWIN32(Status) then
  begin
    // This status was converted from a Win32 error.
    Result := NtxWin32ErrorDescription(WIN32_FROM_NTSTATUS(Status));
  end
  else
  begin
    // We query the status name which looks like "STATUS_SOMETHING_WENT_WRONG"
    // and prettify it so it appears like "Something went wrong"

    Result := NtxpStatusToString(Status);

    if Result = '' then
      Exit;

    Result := PrettifyCapsUnderscore('STATUS_', Result);
  end;
end;

function NtxFormatErrorMessage(Status: NTSTATUS): String;
var
  StartFrom: Integer;
begin
  if NT_NTWIN32(Status) then
  begin
    // This status was converted from a Win32 errors.
    Result := SysErrorMessage(WIN32_FROM_NTSTATUS(Status));
  end
  else
  begin
    // Get error message from ntdll
    Result := SysErrorMessage(Status, hNtdll);

    // Fix those messages which are formatted like:
    //   {Asdf}
    //   Asdf asdf asdf...
    if (Length(Result) > 0) and (Result[Low(Result)] = '{') then
    begin
      StartFrom := Pos('}'#$D#$A, Result);
      if StartFrom >= Low(Result) then
        Delete(Result, Low(Result), StartFrom + 2);
    end;
  end;

  if Result = '' then
    Result := '<No description available>';
end;

end.
