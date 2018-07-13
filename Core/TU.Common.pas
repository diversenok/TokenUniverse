unit TU.Common;

interface

uses
  System.SysUtils, TU.NativeAPI;

type
  ELocatedOSError = class(EOSError)
  public
   ErrorOrigin: String;
   ErrorContext: TObject;
   constructor CreateLE(Code: Cardinal; Location: String;
     Context: TObject = nil);
  end;

  /// <summary>
  ///   A generic wrapper for the result of a function that can fail but
  ///   is disigned not to raise exceptions.
  /// </summary>
  CanFail<ResultType> = record
    Value: ResultType;
    IsValid: Boolean;
    ErrorCode: Cardinal;
    ErrorOrigin: String;
    ErrorContext: TObject;

    /// <summary> Initializes the wrapper and data with zeros. </summary>
    procedure Init(Context: TObject = nil);

    /// <returns> The value if it is valid. </returns>
    /// <exception cref="EOSError">
    ///  Can raise EOSError with the code stored in the wrapper if the value
    ///  is not valid.
    /// </exception>
    function GetValueOrRaise: ResultType;

    /// <summary> Saves the specified data as a valid value. </summary>
    /// <returns> Self. </returns>
    function Succeed(ResultValue: ResultType): CanFail<ResultType>; overload;
    function Succeed: CanFail<ResultType>; overload;

    /// <summary> Checks and saves the last Win32 error. </summary>
    /// <returns>
    ///  The same value as <paramref name="Win32Ret"/> parameter
    /// </returns>
    function CheckError(Win32Ret: LongBool; Where: String): LongBool;

    /// <summary>
    ///  This function is designed to use with API calls that need a probe
    ///  call to obtain the buffer size. It checks the size and the last
    ///  Win32 error. </summary>
    /// <returns>
    ///  <para>
    ///   <c>True</c> if the buffer size is save to use.
    ///  </para>
    ///  <para><c>False</c> otherwise.</para>
    /// </returns>
    function CheckBuffer(BufferSize: Cardinal; Where: String): Boolean;

    /// <summary> Checks and saves NativeAPI status. </summary>
    /// <returns>
    ///  <para>
    ///   <c>True</c> if <paramref name="Status"/> is equal to
    ///   <c>STATUS_SUCCESS</c>.
    ///  </para>
    ///  <para><c>False</c> otherwise.</para>
    /// </returns>
    function CheckNativeError(Status: NTSTATUS; Where: String): Boolean;

    /// <summary> Saves the last Win32 error. </summary>
    procedure SetLastError(Where: String);

    /// <summary>
    ///  Test the buffer wrapper for validity and copies it's error information.
    /// </summary>
    /// <returns> The buffer wrapper itself (<paramref name="Src"/>). </returns>
    function CopyResult(Src: CanFail<Pointer>): CanFail<Pointer>;
  end;

const
  BUFFER_LIMIT = 1024 * 1024 * 64; // 64 MB

// TODO: What about ERROR_BUFFER_OVERFLOW and ERROR_INVALID_USER_BUFFER?

function Win32Check(RetVal: LongBool; Where: String; Context: TObject = nil):
  LongBool; inline;
procedure Win32CheckBuffer(BufferSize: Cardinal; Where: String;
  Context: TObject = nil); inline;
function NativeCheck(Status: NTSTATUS; Where: String;
  Context: TObject = nil): Boolean; inline;

implementation

uses
  Winapi.Windows;

resourcestring
  OSError = '%s failed.' + #$D#$A#$D#$A +
    'Code 0x%x' + #$D#$A#$D#$A + '%s';

function Win32Check(RetVal: LongBool; Where: String; Context: TObject = nil):
  LongBool;
begin
  if not RetVal then
    raise ELocatedOSError.CreateLE(GetLastError, Where, Context);
  Result := True;
end;

procedure Win32CheckBuffer(BufferSize: Cardinal; Where: String;
  Context: TObject = nil);
begin
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BufferSize = 0) then
    raise ELocatedOSError.CreateLE(GetLastError, Where, Context);

  if BufferSize > BUFFER_LIMIT then
    raise ELocatedOSError.CreateLE(STATUS_IMPLEMENTATION_LIMIT, Where, Context);
end;

function NativeCheck(Status: NTSTATUS; Where: String; Context: TObject = nil):
  Boolean;
begin
  if Status <> STATUS_SUCCESS then
    raise ELocatedOSError.CreateLE(Status, Where, Context);
  Result := True;
end;

{ CanFail<ResultType> }

function CanFail<ResultType>.CheckBuffer(BufferSize: Cardinal;
  Where: String): Boolean;
begin
  IsValid := (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (BufferSize > 0) and
    (BufferSize <= BUFFER_LIMIT);
  if not IsValid then
  begin
    ErrorOrigin := Where;
    if BufferSize > BUFFER_LIMIT then
      ErrorCode := STATUS_IMPLEMENTATION_LIMIT
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
    ErrorCode := GetLastError;
    ErrorOrigin := Where;
  end;
  Result := IsValid;
end;

function CanFail<ResultType>.CheckNativeError(Status: NTSTATUS;
  Where: String): Boolean;
begin
  IsValid := Status = STATUS_SUCCESS;
  if not Result then
  begin
    ErrorCode := Status;
    ErrorOrigin := Where;
  end;
  Result := IsValid;
end;

function CanFail<ResultType>.CopyResult(
  Src: CanFail<Pointer>): CanFail<Pointer>;
begin
  Self.IsValid := Src.IsValid;
  Self.ErrorCode := Src.ErrorCode;
  Self.ErrorOrigin := Src.ErrorOrigin;
  Result := Src;
end;

function CanFail<ResultType>.GetValueOrRaise: ResultType;
begin
  if not IsValid then
    raise ELocatedOSError.CreateLE(ErrorCode, ErrorOrigin, ErrorContext);

  Result := Value;
end;

procedure CanFail<ResultType>.Init(Context: TObject = nil);
begin
  FillChar(Self, SizeOf(Self), 0);
  Self.ErrorContext := Context;
end;

procedure CanFail<ResultType>.SetLastError(Where: String);
begin
  IsValid := False;
  ErrorCode := GetLastError;
  ErrorOrigin := Where;
end;

function CanFail<ResultType>.Succeed: CanFail<ResultType>;
begin
  IsValid := True;
  Result := Self;
end;

function CanFail<ResultType>.Succeed(
  ResultValue: ResultType): CanFail<ResultType>;
begin
  IsValid := True;
  Value := ResultValue;
  Result := Self;
end;

{ ELocatedOSError }

constructor ELocatedOSError.CreateLE(Code: Cardinal;
  Location: String; Context: TObject = nil);
begin
  if Code < STATUS_UNSUCCESSFUL then
    CreateResFmt(@OSError, [Location, Code, SysErrorMessage(Code)])
  else
    CreateResFmt(@OSError, [Location, Code, SysErrorMessage(Code,
      GetModuleHandle('ntdll.dll'))]);

  ErrorCode := Code;
  ErrorOrigin := Location;
  ErrorContext := Context;
end;

end.
