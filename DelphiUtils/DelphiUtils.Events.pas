unit DelphiUtils.Events;

interface

type
  // Single parameter events
  TEventListener<T> = procedure(Value: T) of object;
  TEventListenerArray<T> = array of TEventListener<T>;

  TEvent<T> = record
  strict private
    Listeners: TEventListenerArray<T>;
  public
    function Count: Integer;
    procedure Subscribe(EventListener: TEventListener<T>);
    function Unsubscribe(EventListener: TEventListener<T>): Boolean;
    procedure Invoke(Value: T);
  end;

  // The one compatible with VCL events
  TNotifyEventHandler = TEvent<TObject>;

  // Single parameter events with cahing
  TEqualityCheckFunc<T> = function(Value1, Value2: T): Boolean;

  TCachingEvent<T> = record
  strict private
    Event: TEvent<T>;
  public
    ComparisonFunction: TEqualityCheckFunc<T>;
    LastValuePresent: Boolean;
    LastValue: T;
    function Count: Integer; inline;
    procedure Subscribe(EventListener: TEventListener<T>;
      CallWithLastValue: Boolean = True);
    function Unsubscribe(EventListener: TEventListener<T>): Boolean;
    function Invoke(Value: T): Boolean;
  end;

  // Double parameter events
  TEventListener2<T1, T2> = procedure(Param1: T1; Param2: T2) of object;
  TEventListenerArray2<T1, T2> = array of TEventListener2<T1, T2>;

  TEvent2<T1, T2> = record
  strict private
    Listeners: TEventListenerArray2<T1, T2>;
  public
    function Count: Integer;
    procedure Subscribe(EventListener: TEventListener2<T1, T2>);
    function Unsubscribe(EventListener: TEventListener2<T1, T2>): Boolean;
    procedure Invoke(Param1: T1; Param2: T2);
  end;

implementation

uses
  NtUtils.Exceptions, Ntapi.ntstatus;

{ TEvent<T> }

function TEvent<T>.Count: Integer;
begin
  Result := Length(Listeners);
end;

procedure TEvent<T>.Invoke(Value: T);
var
  i: Integer;
begin
  // Event listeners must not raise any exceptions
  // unless the caller is aware of it.

  for i := 0 to High(Listeners) do
    Listeners[i](Value);
end;

procedure TEvent<T>.Subscribe(EventListener: TEventListener<T>);
begin
  SetLength(Listeners, Length(Listeners) + 1);
  Listeners[High(Listeners)] := EventListener;
end;

function TEvent<T>.Unsubscribe(
  EventListener: TEventListener<T>): Boolean;
var
  i: Integer;
begin
  // Note: we can't simply compare procedures of object by using @A = @B
  // since we should distinguish methods linked to different object instances.
  // Luckily, System.TMethod overrides equality operator just as we need.

  for i := 0 to High(Listeners) do
    if System.PMethod(@@Listeners[i])^ = System.PMethod(@@EventListener)^ then
    begin
      Delete(Listeners, i, 1);
      Exit(True);
    end;

  Result := False;
  ENtError.Report(STATUS_NOT_FOUND, 'TEvent<T>.Unsubscribe');
end;

{ TCachingEvent<T> }

function TCachingEvent<T>.Count: Integer;
begin
  Result := Event.Count;
end;

function TCachingEvent<T>.Invoke(Value: T): Boolean;
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

procedure TCachingEvent<T>.Subscribe(EventListener: TEventListener<T>;
  CallWithLastValue: Boolean);
begin
  Event.Subscribe(EventListener);

  if CallWithLastValue and LastValuePresent then
    EventListener(LastValue);
end;

function TCachingEvent<T>.Unsubscribe(
  EventListener: TEventListener<T>): Boolean;
begin
  Event.Unsubscribe(EventListener);
end;

{ TEvent2<T1, T2> }

function TEvent2<T1, T2>.Count: Integer;
begin
  Result := Length(Listeners);
end;

procedure TEvent2<T1, T2>.Invoke(Param1: T1; Param2: T2);
var
  i: Integer;
begin
  // Event listeners must not raise any exceptions
  // unless the caller is aware of it.

  for i := 0 to High(Listeners) do
    Listeners[i](Param1, Param2);
end;

procedure TEvent2<T1, T2>.Subscribe(EventListener: TEventListener2<T1, T2>);
begin
  SetLength(Listeners, Length(Listeners) + 1);
  Listeners[High(Listeners)] := EventListener;
end;

function TEvent2<T1, T2>.Unsubscribe(
  EventListener: TEventListener2<T1, T2>): Boolean;
var
  i: Integer;
begin
  // Note: we can't simply compare procedures of object by using @A = @B
  // since we should distinguish methods linked to different object instances.
  // Luckily, System.TMethod overrides equality operator just as we need.

  for i := 0 to High(Listeners) do
    if System.PMethod(@@Listeners[i])^ = System.PMethod(@@EventListener)^ then
    begin
      Delete(Listeners, i, 1);
      Exit(True);
    end;

  Result := False;
  ENtError.Report(STATUS_NOT_FOUND, 'TEvent<T>.Unsubscribe');
end;

end.
