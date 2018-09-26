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
   class function FormatErrorMessage(Location: String; Code: Cardinal): String;
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

    /// <summary>
    /// Initializes the wrapper. This is necessary since records are created on
    /// the stack and can contain arbitrary data.
    /// </summary>
    procedure Init(Context: TObject = nil);
    class function Fail: CanFail<ResultType>; static;

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
    class function SucceedWith(ResultValue: ResultType): CanFail<ResultType>; static;

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

    function GetErrorMessage: String;
  end;

  TEventListener<T> = procedure(Value: T) of object;
  TEventListenerArray<T> = array of TEventListener<T>;

  /// <summary> Multiple source event handler. </summary>
  TEventHandler<T> = record
  strict private
    Listeners: TEventListenerArray<T>;
  public
    /// <summary>
    ///  Adds an event listener. The event listener can add and remove other
    ///  event listeners, but all these changes will take effect only on the
    ///  next call.
    /// </summary>
    /// <remarks>
    ///  Be careful with exceptions since they break the loop of <c>Invoke</c>
    ///  method.
    /// </remarks>
    procedure Add(EventListener: TEventListener<T>);
    function Delete(EventListener: TEventListener<T>): Boolean;
    function Count: Integer;

    /// <summary>
    ///  Tries to call all event listeners. If an exception occures some of the
    ///  listeners may not be notified.
    /// </summary>
    procedure Invoke(Value: T);
    procedure InvokeIfValid(Value: CanFail<T>); inline;

    /// <summary>
    ///  Calls all event listeners regardless of any exceptions. This method
    ///  always succeeds since it ignores and looses all raised exceptions.
    /// </summary>
    procedure InvokeIgnoringErrors(Value: T);
  end;

  /// <summary>
  ///  Multiple source event handler compatible with TNotifyEvent from
  ///  System.Classes
  /// </summary>
  TNotifyEventHandler = TEventHandler<TObject>;

  TEqualityCheckFunc<T> = function(Value1, Value2: T): Boolean;

  TValuedEventHandler<T> = record
  strict private
    Event: TEventHandler<T>;
  public
    ComparisonFunction: TEqualityCheckFunc<T>;
    LastValuePresent: Boolean;
    LastValue: T;

    /// <summary>
    ///  Adds an event listener and calls it with the last known value.
    /// </summary>
    /// <remarks>
    ///  Be careful with exceptions since they break the loop of <c>Invoke</c>
    ///  method.
    /// </remarks>
    procedure Add(EventListener: TEventListener<T>;
      CallWithLastValue: Boolean = True);
    function Delete(EventListener: TEventListener<T>): Boolean;
    function Count: Integer; inline;

    /// <summary>
    ///  Notifies event listeners if the value differs from the previous one.
    /// </summary>
    /// <returns>
    ///  <para><c>True</c> if the value has actually changed </para>
    ///  <para><c>False</c> otherwise </para>
    /// </returns>
    function Invoke(Value: T): Boolean;
    function InvokeIfValid(Value: CanFail<T>): Boolean; inline;
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

/// <symmary>
///  Converts a string that contains a decimal or a hexadecimal number to an
///  integer.
/// </summary>
/// <exception cref="EConvertError"> Can raise EConvertError. </exception>
function StrToIntEx(S: String; Comment: String): Cardinal;
function StrToInt64Ex(S: String; Comment: String): UInt64;

/// <symmary>
///  Converts a number of 100ns intervals from 01.01.1601 to Delphi's
///  <c>TDateTime</c> type.
/// </summary>
function NativeTimeToLocalDateTime(NativeTime: Int64): TDateTime;

implementation

uses
  Winapi.Windows, System.DateUtils;

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

class function CanFail<ResultType>.Fail: CanFail<ResultType>;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function CanFail<ResultType>.GetErrorMessage: String;
begin
  Result := ELocatedOSError.FormatErrorMessage(ErrorOrigin, ErrorCode);
end;

function CanFail<ResultType>.GetValueOrRaise: ResultType;
begin
  if not IsValid then
    raise ELocatedOSError.CreateLE(ErrorCode, ErrorOrigin, ErrorContext);

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

{ ELocatedOSError }

constructor ELocatedOSError.CreateLE(Code: Cardinal;
  Location: String; Context: TObject = nil);
begin
  inherited Create(FormatErrorMessage(Location, Code));
  ErrorCode := Code;
  ErrorOrigin := Location;
  ErrorContext := Context;
end;

class function ELocatedOSError.FormatErrorMessage(Location: String;
  Code: Cardinal): String;
begin
  if Code < STATUS_UNSUCCESSFUL then
    Result := Format(OSError, [Location, Code, SysErrorMessage(Code)])
  else
    Result := Format(OSError, [Location, Code, SysErrorMessage(Code,
      GetModuleHandle('ntdll.dll'))]);
end;

{ TEventHandler<T> }

procedure TEventHandler<T>.Add(EventListener: TEventListener<T>);
begin
  SetLength(Listeners, Length(Listeners) + 1);
  Listeners[High(Listeners)] := EventListener;
end;

function TEventHandler<T>.Count: Integer;
begin
  Result := Length(Listeners);
end;

function TEventHandler<T>.Delete(EventListener: TEventListener<T>): Boolean;
var
  i, position: integer;
begin
  position := -1;

  // Note: we can't simply use @A = @B for `procedure of object` since we should
  // distinguish methods linked to different object instances.
  // Luckily, System.TMethod overrides equality operator just as we need.
  for i := 0 to High(Listeners) do
    if System.PMethod(@@Listeners[i])^ = System.PMethod(@@EventListener)^ then
    begin
      position := i;
      Break;
    end;

  Result := position <> -1;

  if Result then
  begin
    for i := position + 1 to High(Listeners) do
      Listeners[i - 1] := Listeners[i];

    SetLength(Listeners, Length(Listeners) - 1);
  end;

  if not Result then
    OutputDebugString('Cannot delete event listener');
end;

procedure TEventHandler<T>.Invoke(Value: T);
var
  i: integer;
  ListenersCopy: TEventListenerArray<T>;
begin
  // Event listeners can modify the list while we process it, so we should make
  // a copy. All these modifications would take effect only on the next call.
  ListenersCopy := Copy(Listeners, 0, Length(Listeners));

  for i := 0 to High(ListenersCopy) do
    ListenersCopy[i](Value);
end;

procedure TEventHandler<T>.InvokeIfValid(Value: CanFail<T>);
begin
  if Value.IsValid then
    Invoke(Value.Value);
end;

procedure TEventHandler<T>.InvokeIgnoringErrors(Value: T);
var
  i: integer;
  ListenersCopy: TEventListenerArray<T>;
begin
  ListenersCopy := Copy(Listeners, 0, Length(Listeners));

  for i := 0 to High(ListenersCopy) do
    try
      ListenersCopy[i](Value);
    except
      on E: Exception do
        OutputDebugString(PWideChar('InvokeIgnoringErrors:: ' + E.ToString));
    end;
end;

{ TValuedEventHandler<T> }

procedure TValuedEventHandler<T>.Add(EventListener: TEventListener<T>;
  CallWithLastValue: Boolean);
begin
  Event.Add(EventListener);

  if CallWithLastValue and LastValuePresent then
    EventListener(LastValue);
end;

function TValuedEventHandler<T>.Count: Integer;
begin
  Result := Event.Count;
end;

function TValuedEventHandler<T>.Delete(
  EventListener: TEventListener<T>): Boolean;
begin
  Result := Event.Delete(EventListener);
end;

function TValuedEventHandler<T>.Invoke(Value: T): Boolean;
begin
  // Do not invoke on the same value twise
  if LastValuePresent and Assigned(ComparisonFunction) and
    ComparisonFunction(LastValue, Value) then
    Exit(False);

  Result := LastValuePresent;
  LastValuePresent := True;
  LastValue := Value;

  Event.Invoke(Value);
end;

function TValuedEventHandler<T>.InvokeIfValid(Value: CanFail<T>): Boolean;
begin
  if Value.IsValid then
    Result := Invoke(Value.Value)
  else
    Result := False;
end;

{ Conversion functions }

function StrToInt64Ex(S: String; Comment: String): UInt64;
const
  E_CONV_DECHEX = 'Invalid %s. Please specify a decimal or a hexadecimal value.';
var
  E: Integer;
begin
  if S.StartsWith('0x') then
    S := S.Replace('0x', '$', []);

  Val(S, Result, E);
  if E <> 0 then
    raise EConvertError.Create(Format(E_CONV_DECHEX, [Comment]));
end;

function StrToIntEx(S: String; Comment: String): Cardinal;
begin
  Result := StrToInt64Ex(S, Comment);
end;

function NativeTimeToLocalDateTime(NativeTime: Int64): TDateTime;
const
  DAYS_FROM_1601 = 109205;
  SCALE = 864000000000; // 100ns in 1 day
begin
  Result := NativeTime / SCALE - DAYS_FROM_1601;
  Result := TTimeZone.Local.ToLocalTime(Result);
end;

end.
