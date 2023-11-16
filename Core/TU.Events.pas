unit TU.Events;

{
  This module allows subscribing to querying system-wide information.
}

interface

uses
  DelphiUtils.AutoEvents, NtUtils, NtUtils.Objects.Snapshots;

type
  TGlobalEvents = record
  private
    class var FOnHandleSnapshot: TAutoEvent<TArray<TSystemHandleEntry>>;
    class var FOnObjectSnapshot: TAutoEvent<TArray<TObjectTypeEntry>>;
    class var FOnLinkLogonSessions: TAutoEvent;
    class constructor Create;
  public
    // Querying
    class function QueryHandles(out Handles: TArray<TSystemHandleEntry>): TNtxStatus; static;
    class function QueryObjects(out KernelObjects: TArray<TObjectTypeEntry>): TNtxStatus; static;

    // Subscribing for future events (without invoking)
    class function SubscribeHandles(Callback: TEventCallback<TArray<TSystemHandleEntry>>): IAutoReleasable; static;
    class function SubscribeObjects(Callback: TEventCallback<TArray<TObjectTypeEntry>>): IAutoReleasable; static;

    // Refreshing
    class function RefreshHandles: TNtxStatus; static;
    class function RefreshObjects: TNtxStatus; static;

    // Other events
    class property OnLinkLogonSessions: TAutoEvent read FOnLinkLogonSessions;
  end;

implementation

uses
  System.SysUtils, UI.Exceptions;

// Workaround internal compiler error by re-declaring exception-safe invokers
// instead of using generic methods of TExceptionSafeInvoker
procedure SafeHandleInvoker(
  Callback: TEventCallback<TArray<TSystemHandleEntry>>;
  const Parameter: TArray<TSystemHandleEntry>
);
begin
  try
    Callback(Parameter);
  except
    on E: Exception do
      ReportException(E);
  end;
end;

procedure SafeObjectInvoker(
  Callback: TEventCallback<TArray<TObjectTypeEntry>>;
  const Parameter: TArray<TObjectTypeEntry>
);
begin
  try
    Callback(Parameter);
  except
    on E: Exception do
      ReportException(E);
  end;
end;

{ TGlobalEvents }

class constructor TGlobalEvents.Create;
begin
  TGlobalEvents.FOnHandleSnapshot.SetCustomInvoker(SafeHandleInvoker);
  TGlobalEvents.FOnObjectSnapshot.SetCustomInvoker(SafeObjectInvoker);
  TGlobalEvents.FOnLinkLogonSessions.SetCustomInvoker(
    TExceptionSafeInvoker.NoParameters);
end;

class function TGlobalEvents.QueryHandles;
begin
  Result := NtxEnumerateHandles(Handles);

  if Result.IsSuccess then
    TGlobalEvents.FOnHandleSnapshot.Invoke(Handles);
end;

class function TGlobalEvents.QueryObjects;
begin
  Result := NtxEnumerateObjects(KernelObjects);

  if Result.IsSuccess then
    TGlobalEvents.FOnObjectSnapshot.Invoke(KernelObjects);
end;

class function TGlobalEvents.RefreshHandles;
var
  Info: TArray<TSystemHandleEntry>;
begin
  // There is no way to observe the result without subscribers
  if not TGlobalEvents.FOnHandleSnapshot.HasSubscribers then
    Exit(Default(TNtxStatus));

  Result := TGlobalEvents.QueryHandles(Info);
end;

class function TGlobalEvents.RefreshObjects;
var
  Info: TArray<TObjectTypeEntry>;
begin
  // There is no way to observe the result without subscribers
  if not TGlobalEvents.FOnObjectSnapshot.HasSubscribers then
    Exit(Default(TNtxStatus));

  Result := TGlobalEvents.QueryObjects(Info);
end;

class function TGlobalEvents.SubscribeHandles;
begin
  Result := TGlobalEvents.FOnHandleSnapshot.Subscribe(Callback);
end;

class function TGlobalEvents.SubscribeObjects;
begin
  Result := TGlobalEvents.FOnObjectSnapshot.Subscribe(Callback);
end;

end.

