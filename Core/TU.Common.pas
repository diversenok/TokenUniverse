unit TU.Common;

interface

uses
  System.SysUtils, NtUtils.Exceptions;

type
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

    /// <summary> Calls all event listeners. </summary>
    /// <remarks>
    ///  If an exception occurs some of the listeners may not be notified.
    /// </remarks>
    procedure Invoke(Value: T);

    /// <summary> Calls all event listeners if the value is valid. </summary>
    /// <remarks>
    ///  If an exception occurs some of the listeners may not be notified.
    /// </remarks>
    procedure InvokeIfValid(Value: CanFail<T>); inline;
  end;

  /// <summary>
  ///  Multiple source event handler compatible with TNotifyEvent from
  ///  <see cref="System.Classes"/>.
  /// </summary>
  TNotifyEventHandler = TEventHandler<TObject>;

  TEqualityCheckFunc<T> = function(Value1, Value2: T): Boolean;

  /// <summary> Multiple source event handler with cache support. </summary>
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
    ///  Be careful with exceptions since they break the loop of
    ///  <see cref="Invoke"/> method.
    /// </remarks>
    procedure Add(EventListener: TEventListener<T>;
      CallWithLastValue: Boolean = True);

    /// <summary> Deletes the specified event listener. </summary>
    /// <returns>
    ///  <para><c>True</c> if the event listener was found and deleted; </para>
    ///  <para><c>False</c> if there was no such event listener.</para>
    /// </returns>
    function Delete(EventListener: TEventListener<T>): Boolean;
    function Count: Integer; inline;

    /// <summary>
    ///  Notifies event listeners if the value differs from the previous one.
    /// </summary>
    /// <returns>
    ///  <para><c>True</c> if the value has actually changed; </para>
    ///  <para><c>False</c> otherwise. </para>
    /// </returns>
    function Invoke(Value: T): Boolean;

    /// <summary>
    ///  Notifies event listeners if the value is valid and differs from the
    ///  previous one.
    /// </summary>
    /// <returns>
    ///  <para><c>True</c> if the value has actually changed; </para>
    ///  <para><c>False</c> otherwise. </para>
    /// </returns>
    function InvokeIfValid(Value: CanFail<T>): Boolean; inline;
  end;

/// <symmary>
///  Converts a string that contains a decimal or a hexadecimal number to an
///  integer.
/// </summary>
/// <exception cref="EConvertError"> Can raise EConvertError. </exception>
function TryStrToUInt64Ex(S: String; out Value: UInt64): Boolean;
function StrToUIntEx(S: String; Comment: String): Cardinal; inline;
function StrToUInt64Ex(S: String; Comment: String): UInt64; inline;

/// <symmary>
///  Converts a number of 100ns intervals from 01.01.1601 to Delphi's
///  <c>TDateTime</c> type.
/// </summary>
function NativeTimeToLocalDateTime(NativeTime: Int64): TDateTime;

/// <symmary>
///  Converts Delphi's <c>TDateTime</c> to a number of 100ns intervals from
///  01.01.1601.
/// </summary>
function DateTimeToNative(LocalTime: TDateTime): Int64;

implementation

uses
  Winapi.Windows, System.DateUtils;

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
  // Do not invoke on the same value twice
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

function TryStrToUInt64Ex(S: String; out Value: UInt64): Boolean;
var
  E: Integer;
begin
  if S.StartsWith('0x') then
    S := S.Replace('0x', '$', []);

  Val(S, Value, E);
  Result := (E = 0);
end;

function StrToUInt64Ex(S: String; Comment: String): UInt64;
const
  E_DECHEX = 'Invalid %s. Please specify a decimal or a hexadecimal value.';
begin
  if not TryStrToUInt64Ex(S, Result) then
    raise EConvertError.Create(Format(E_DECHEX, [Comment]));
end;

function StrToUIntEx(S: String; Comment: String): Cardinal;
begin
  {$R-}
  Result := StrToUInt64Ex(S, Comment);
  {$R+}
end;

const
  DAYS_FROM_1601 = 109205;
  NATIVE_TIME_SCALE = 864000000000; // 100ns in 1 day

function NativeTimeToLocalDateTime(NativeTime: Int64): TDateTime;
begin
  Result := NativeTime / NATIVE_TIME_SCALE - DAYS_FROM_1601;
  Result := TTimeZone.Local.ToLocalTime(Result);
end;

function DateTimeToNative(LocalTime: TDateTime): Int64;
begin
  Result := Trunc(NATIVE_TIME_SCALE * (DAYS_FROM_1601 +
    TTimeZone.Local.ToUniversalTime(LocalTime)));
end;

end.
