unit TU.Observers;

{
  This module introduces automatic caching events for observing value changes.
}

interface

uses
  NtUtils, DelphiUtils.AutoEvents;

type
  TEqualityCheck<T> = function (const A, B: T): Boolean;

  TAutoObservers<T> = record
  private
    FEvents: TAutoEvent<TNtxStatus, T>;
    FEqualityCheck: TEqualityCheck<T>;
    FHasLastValue: Boolean;
    FLastValue: T;
  public
    procedure Initialize(EqualityCheck: TEqualityCheck<T>);
    function HasObservers: Boolean;
    procedure Notify(const Status: TNtxStatus; const Value: T);
    function Subscribe(const Callback: TEventCallback<TNtxStatus, T>): IAutoReleasable;
  end;

implementation

uses
  System.SysUtils, UI.Exceptions;

{ TAutoObservers<T> }

function TAutoObservers<T>.HasObservers;
begin
  Result := FEvents.HasSubscribers;
end;

procedure TAutoObservers<T>.Initialize;
begin
  FEvents := Default(TAutoEvent<TNtxStatus, T>);
  FEqualityCheck := EqualityCheck;
  FHasLastValue := False;
  FLastValue := Default(T);
end;

procedure TAutoObservers<T>.Notify;
begin
  // Skip consecutive successful invocations that supply the same value
  if Status.IsSuccess and FHasLastValue and Assigned(FEqualityCheck) and
    FEqualityCheck(FLastValue, Value) then
    Exit;

  // Save the new state
  FHasLastValue := Status.IsSuccess;

  if FHasLastValue then
    FLastValue := Value
  else
    FLastValue := Default(T);

  // Notify all subscribers of the new state
  FEvents.Invoke(Status, FLastValue);
end;

function TAutoObservers<T>.Subscribe;
begin
  Result := FEvents.Subscribe(Callback);
end;

end.
