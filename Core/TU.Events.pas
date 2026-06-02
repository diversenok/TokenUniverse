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
    class var FOnHandleSnapshot: TAutoEvent<TArray<TNtxSystemHandleEntry>>;
    class var FOnObjectSnapshot: TAutoEvent<TArray<TNtxObjectTypeEntry>>;
    class var FOnLinkLogonSessions: TAutoEvent;
  public
    // Querying
    class function QueryHandles(out Handles: TArray<TNtxSystemHandleEntry>): TNtxStatus; static;
    class function QueryObjects(out KernelObjects: TArray<TNtxObjectTypeEntry>): TNtxStatus; static;

    // Subscribing for future events (without invoking)
    class function SubscribeHandles(Callback: TEventCallback<TArray<TNtxSystemHandleEntry>>): IAutoReleasable; static;
    class function SubscribeObjects(Callback: TEventCallback<TArray<TNtxObjectTypeEntry>>): IAutoReleasable; static;

    // Refreshing
    class function RefreshHandles: TNtxStatus; static;
    class function RefreshObjects: TNtxStatus; static;

    // Other events
    class property OnLinkLogonSessions: TAutoEvent read FOnLinkLogonSessions;
  end;

implementation

uses
  System.SysUtils;

{ TGlobalEvents }

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
  Info: TArray<TNtxSystemHandleEntry>;
begin
  // There is no way to observe the result without subscribers
  if not TGlobalEvents.FOnHandleSnapshot.HasSubscribers then
    Exit(Default(TNtxStatus));

  Result := TGlobalEvents.QueryHandles(Info);
end;

class function TGlobalEvents.RefreshObjects;
var
  Info: TArray<TNtxObjectTypeEntry>;
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

