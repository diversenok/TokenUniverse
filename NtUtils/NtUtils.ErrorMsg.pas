unit NtUtils.ErrorMsg;

interface

uses
  Ntapi.ntdef;

// Converts common error codes to strings, for example:
//      1314 => "ERROR_PRIVILEGE_NOT_HELD"
// $C00000BB =>     "STATUS_NOT_SUPPORTED"
// Returns empty string if the name is not found.

function Win32ErrorNameToString(Code: Cardinal): String;
function StatusNameToString(Status: NTSTATUS): String;

// The same as above, but on unknown errors returns their decimal/hexadecimal
// representations.
function Win32ErrorToString(Code: Cardinal): String;
function StatusToString(Status: NTSTATUS): String;

// Converts common error codes to their short descriptions, for example:
//      1314 => "Privilege not held"
// $C00000BB =>      "Not supported"
// Returns empty string if the name is not found.
function Win32ErrorToDescription(Code: Cardinal): String;
function StatusToDescription(Status: NTSTATUS): String;

// Retrieves a full description of a native error.
function SysNativeErrorMessage(Status: NTSTATUS): String;

implementation

uses
  Winapi.WinBase, System.SysUtils, DelphiUtils.Strings;

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

function Win32ErrorNameToString(Code: Cardinal): String;
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

function StatusNameToString(Status: NTSTATUS): String;
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

function Win32ErrorToString(Code: Cardinal): String;
begin
  Result := Win32ErrorNameToString(Code);
  if Result = '' then
    Result := IntToStr(Code);
end;

function StatusToString(Status: NTSTATUS): String;
begin
  Result := StatusNameToString(Status);
  if Result = '' then
    Result := IntToHexEx(Status, 8);
end;

function PrettifyError(PossiblePrefix: String; Msg: String): String;
var
  i: Integer;
begin
  Result := Msg;

  // Remove the prefix
  if Pos(PossiblePrefix, Result) = Low(Result) then
    Delete(Result, Low(Result), Length(PossiblePrefix));

  // Lower the case and replace underscores with spaces
  for i := Succ(Low(Result)) to High(Result) do
  begin
    case Result[i] of
      'A'..'Z':
        Result[i] := Chr(Ord('a') + Ord(Result[i]) - Ord('A'));
      '_':
          Result[i] := ' ';
    end;
  end;
end;

function Win32ErrorToDescription(Code: Cardinal): String;
begin
  // We query the code name which looks like "ERROR_SOMETHING_WENT_WRONG"
  // and prettify it so it appears like "Something went wrong"

  Result := Win32ErrorNameToString(Code);

  if Result = '' then
    Exit;

  Result := PrettifyError('ERROR_', Result);
end;
function StatusToDescription(Status: NTSTATUS): String;
begin
  // We query the status name which looks like "STATUS_SOMETHING_WENT_WRONG"
  // and prettify it so it appears like "Something went wrong"

  Result := StatusNameToString(Status);

  if Result = '' then
    Exit;

  Result := PrettifyError('STATUS_', Result);
end;

var
  // Optimize queries by caching ntdll handle
  hNtdllInit: Boolean;
  hNtdll: HMODULE;

function SysNativeErrorMessage(Status: NTSTATUS): String;
var
  StartFrom: Integer;
begin
  // Obtain HMODULE for ntdll
  if not hNtdllInit then
  begin
    hNtdll := GetModuleHandleW(ntdll);
    hNtdllInit := True;
  end;

  // Get error message
  Result := SysErrorMessage(Status, hNtdll);

  // Fix those messages which are formatted like:
  //   {Asdf}
  //   Asdf asdf asdf...
  if Result[Low(Result)] = '{' then
  begin
    StartFrom := Pos('}'#$D#$A, Result);
    if StartFrom >= Low(Result) then
      Delete(Result, Low(Result), StartFrom + 2);
  end;
end;

end.
