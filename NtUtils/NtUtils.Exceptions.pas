unit NtUtils.Exceptions;

interface

uses
  System.SysUtils, Ntapi.ntdef;

const
  BUFFER_LIMIT = 1024 * 1024 * 256; // 256 MB

type
  TNtxStatus = record
    Status: NTSTATUS;
    Location: String;
    function Matches(Status: NTSTATUS; Location: String): Boolean;
    function IsSuccess: Boolean;
    procedure RaiseOnError;
    function ToString: String;
    function MessageHint: String;
    class function FromWin32(RetVal: LongBool; Location: String): TNtxStatus;
      static;
  end;

type
  TErrorType = (errWin, errNative);

  ELocatedOSError = class(EOSError)
    ErrorOrigin: String;
    ErrorContext: TObject;
    function Match(Location: String; Code: Cardinal): Boolean;
  end;

  /// <summary> Represents a Win32 error with a known location. </summary>
  EWinError = class(ELocatedOSError)
  public
    constructor Create(Code: Cardinal; Location: String;
      Context: TObject = nil); reintroduce;
    class procedure Report(Code: Cardinal; Location: String);
  end;

  /// <summary> Represents a Native error with a known location. </summary>
  ENtError = class(ELocatedOSError)
  public
    constructor Create(Status: NTSTATUS; Location: String;
      Context: TObject = nil); reintroduce;
    class procedure Report(Status: Cardinal; Location: String);
    function ToWinErrorCode: Cardinal;
  end;

  /// <summary>
  ///   A generic wrapper for the result of a function that can fail but
  ///   is designed not to raise exceptions.
  /// </summary>
  CanFail<ResultType> = record
    Value: ResultType;
    IsValid: Boolean;
    ErrorType: TErrorType;
    ErrorCode: Cardinal;
    ErrorOrigin: String;
    ErrorContext: TObject;

    /// <summary>
    /// Initializes the wrapper. This is necessary since records are created on
    /// the stack and can contain arbitrary data.
    /// </summary>
    procedure Init(Context: TObject = nil);

    /// <returns> The value if it is valid. </returns>
    /// <exception cref="TU.Common.ELocatedOSError">
    ///  Can raise <see cref="TU.Common.ELocatedOSError"/> with the code stored
    ///  in the wrapper if the value is not valid.
    /// </exception>
    function GetValueOrRaise: ResultType;

    /// <summary> Saves the specified data as a valid value. </summary>
    /// <returns> Self. </returns>
    function Succeed(ResultValue: ResultType): CanFail<ResultType>; overload;
    function Succeed: CanFail<ResultType>; overload;
    class function SucceedWith(ResultValue: ResultType): CanFail<ResultType>; static;

    /// <summary> Checks and saves the last Win32 error. </summary>
    /// <returns>
    ///  The same value as <paramref name="Win32Ret"/> parameter.
    /// </returns>
    function CheckError(Win32Ret: LongBool; Where: String): LongBool;

    /// <summary>
    ///  This function is designed to use with API calls that need a probe
    ///  call to obtain the buffer size. It checks the size and the last
    ///  Win32 error. </summary>
    /// <returns>
    ///  <para><c>True</c> if the buffer size is save to use.</para>
    ///  <para><c>False</c> otherwise.</para>
    /// </returns>
    function CheckBuffer(BufferSize: Cardinal; Where: String): Boolean;

    /// <summary> Checks and saves NativeAPI status. </summary>
    /// <returns>
    ///  <para><c>True</c> if the call succeeded.</para>
    ///  <para><c>False</c> otherwise.</para>
    /// </returns>
    function CheckNativeError(Status: NTSTATUS; Where: String): Boolean;

    /// <summary>
    ///  Test the buffer wrapper for validity and copies it's error information.
    /// </summary>
    /// <returns> The buffer wrapper itself (<paramref name="Src"/>). </returns>
    function CopyResult(Src: CanFail<Pointer>): CanFail<Pointer>;
  end;

{ Runtime error-checking functions }
procedure WinCheck(RetVal: LongBool; Where: String; Context: TObject = nil);
function WinTryCheckBuffer(BufferSize: Cardinal): Boolean;
procedure WinCheckBuffer(BufferSize: Cardinal; Where: String; Context: TObject = nil);

procedure NativeCheck(Status: NTSTATUS; Where: String; Context: TObject = nil);
function NativeTryCheckBuffer(var Status: NTSTATUS; BufferSize: Cardinal): Boolean;

implementation

uses
  Ntapi.ntrtl, Ntapi.ntstatus, Winapi.WinBase, Winapi.WinError,
  NtUtils.ErrorMsg, NtUtils.ApiExtension;

{ TNtxStatus }

class function TNtxStatus.FromWin32(RetVal: LongBool;
  Location: String): TNtxStatus;
begin
  Result.Location := Location;

  if not RetVal then
    Result.Status := RtlxGetLastNtStatus
  else
    Result.Status := STATUS_SUCCESS;
end;

function TNtxStatus.IsSuccess: Boolean;
begin
  Result := NT_SUCCESS(Status);
end;

function TNtxStatus.Matches(Status: NTSTATUS; Location: String): Boolean;
begin
  Result := (Self.Status = Status) and (Self.Location = Location);
end;

function TNtxStatus.MessageHint: String;
begin
  Result := SysNativeErrorMessage(Status);
end;

procedure TNtxStatus.RaiseOnError;
begin
  if not NT_SUCCESS(Status) then
    raise ENtError.Create(Status, Location);
end;

function TNtxStatus.ToString: String;
begin
  Result := Location + ': ' + StatusToString(Status);
end;

{ ELocatedOSError }

function ELocatedOSError.Match(Location: String; Code: Cardinal): Boolean;
begin
  Result := (ErrorCode = Code) and (ErrorOrigin = Location);
end;

{ EWinError }

constructor EWinError.Create(Code: Cardinal; Location: String;
  Context: TObject);
begin
  Message := Location + ' returned ' + Win32ErrorToString(Code);
  ErrorOrigin := Location;
  ErrorCode := Code;
  ErrorContext := Context;
end;

class procedure EWinError.Report(Code: Cardinal; Location: String);
begin
  OutputDebugStringW(PWideChar(Location + ': ' + Win32ErrorToString(Code)));
end;

{ ENtError }

constructor ENtError.Create(Status: NTSTATUS; Location: String;
  Context: TObject);
begin
  Message := Location + ' returned ' + StatusToString(Status);
  ErrorOrigin := Location;
  ErrorCode := Status;
  ErrorContext := Context;
end;

class procedure ENtError.Report(Status: Cardinal; Location: String);
begin
  OutputDebugStringW(PWideChar(Location + ': ' + StatusToString(Status)));
end;

function ENtError.ToWinErrorCode: Cardinal;
begin
  if NT_NTWIN32(ErrorCode) then
    Result := WIN32_FROM_NTSTATUS(ErrorCode)
  else
    Result := RtlNtStatusToDosErrorNoTeb(ErrorCode);
end;

{ CanFail<ResultType> }

function CanFail<ResultType>.CheckBuffer(BufferSize: Cardinal;
  Where: String): Boolean;
begin
  IsValid := (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (BufferSize > 0) and
    (BufferSize <= BUFFER_LIMIT);
  if not IsValid then
  begin
    ErrorType := errWin;
    ErrorOrigin := Where;
    if BufferSize > BUFFER_LIMIT then
      ErrorCode := ERROR_IMPLEMENTATION_LIMIT
    else
      ErrorCode := GetLastError;
  end;
  Result := IsValid;
end;

function CanFail<ResultType>.CheckError(Win32Ret: LongBool;
  Where: String): LongBool;
begin
  IsValid := Win32Ret;
  if not Win32Ret then
  begin
    ErrorType := errWin;
    ErrorCode := GetLastError;
    ErrorOrigin := Where;
  end;
  Result := IsValid;
end;

function CanFail<ResultType>.CheckNativeError(Status: NTSTATUS;
  Where: String): Boolean;
begin
  IsValid := NT_SUCCESS(Status);
  if not Result then
  begin
    ErrorType := errNative;
    ErrorCode := Status;
    ErrorOrigin := Where;
  end;
  Result := IsValid;
end;

function CanFail<ResultType>.CopyResult(
  Src: CanFail<Pointer>): CanFail<Pointer>;
begin
  Self.IsValid := Src.IsValid;
  Self.ErrorType := Src.ErrorType;
  Self.ErrorCode := Src.ErrorCode;
  Self.ErrorOrigin := Src.ErrorOrigin;
  Result := Src;
end;

function CanFail<ResultType>.GetValueOrRaise: ResultType;
begin
  if not IsValid then
  begin
    case ErrorType of
      errWin:
        raise EWinError.Create(ErrorCode, ErrorOrigin, ErrorContext);
      errNative:
        raise ENtError.Create(ErrorCode, ErrorOrigin, ErrorContext);
    else
      Assert(False);
    end;
  end;

  Result := Value;
end;

procedure CanFail<ResultType>.Init(Context: TObject = nil);
begin
  // We can't use FillChar(Self, SizeOf(Self), 0) since we may accidentally
  // overwrite a string reference (for example inside Self.Value record)
  // and compiler wouldn't know about it. That leads to memory leaks since
  // strings have reference counting mechanism.
  Self.IsValid := False;
  Self.ErrorCode := 0;
  Self.ErrorOrigin := '';
  Self.ErrorContext := Context;
end;

function CanFail<ResultType>.Succeed: CanFail<ResultType>;
begin
  IsValid := True;
  Result := Self;
end;

class function CanFail<ResultType>.SucceedWith(
  ResultValue: ResultType): CanFail<ResultType>;
begin
  Result.IsValid := True;
  Result.Value := ResultValue;
end;

function CanFail<ResultType>.Succeed(
  ResultValue: ResultType): CanFail<ResultType>;
begin
  IsValid := True;
  Value := ResultValue;
  Result := Self;
end;

{ Functions }

procedure WinCheck(RetVal: LongBool; Where: String; Context: TObject = nil);
begin
  if not RetVal then
    raise EWinError.Create(GetLastError, Where, Context);
end;

// TODO: What about ERROR_BUFFER_OVERFLOW and ERROR_INVALID_USER_BUFFER?

function WinTryCheckBuffer(BufferSize: Cardinal): Boolean;
begin
  Result := (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (BufferSize > 0) and
    (BufferSize <= BUFFER_LIMIT);

  if not Result and (BufferSize > BUFFER_LIMIT) then
    SetLastError(ERROR_IMPLEMENTATION_LIMIT);
end;

procedure WinCheckBuffer(BufferSize: Cardinal; Where: String; Context: TObject);
begin
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BufferSize = 0) then
    raise EWinError.Create(GetLastError, Where, Context);

  if BufferSize > BUFFER_LIMIT then
    raise EWinError.Create(ERROR_IMPLEMENTATION_LIMIT, Where, Context);
end;

procedure NativeCheck(Status: NTSTATUS; Where: String; Context: TObject);
begin
  if not NT_SUCCESS(Status) then
    raise ENtError.Create(Status, Where, Context);
end;

function NativeTryCheckBuffer(var Status: NTSTATUS; BufferSize: Cardinal): Boolean;
begin
  Result := (Status = STATUS_INFO_LENGTH_MISMATCH) or
    (Status = STATUS_BUFFER_TOO_SMALL);

  if BufferSize > BUFFER_LIMIT then
  begin
    Result := False;
    Status := STATUS_IMPLEMENTATION_LIMIT;
  end;
end;

end.
