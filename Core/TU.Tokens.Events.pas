unit TU.Tokens.Events;

{
  This internal module powers the new infrastructure for sharing observers
  between token handles that point to the same kernel object.
}

interface

uses
  Ntapi.WinNt, Ntapi.ntobapi, Ntapi.ntseapi, NtUtils, NtUtils.Objects.Snapshots,
  NtUtils.Tokens.Info, NtUtils.Profiles, DelphiUtils.AutoObjects,
  DelphiUtils.AutoEvents, TU.Observers, TU.Tokens;

type
  // Shared observers between tokens that point to the same kernel object
  TTokenEvents = class
  public
    CreatorPIDIsKnown: Boolean;
    CreatorPID: TProcessId;
    OnBasicInfo: TAutoObservers<TObjectBasicInformation>;
    OnHandles: TAutoObservers<TArray<TSystemHandleEntry>>;
    OnKernelAddress: TAutoObservers<Pointer>;
    OnCreatorPID: TAutoObservers<TProcessId>;
    OnUser: TAutoObservers<TGroup>;
    OnGroups: TAutoObservers<TArray<TGroup>>;
    OnPrivileges: TAutoObservers<TArray<TPrivilege>>;
    OnOwner: TAutoObservers<ISid>;
    OnPrimaryGroup: TAutoObservers<ISid>;
    OnDefaultDacl: TAutoObservers<IAcl>;
    OnSource: TAutoObservers<TTokenSource>;
    OnType: TAutoObservers<TTokenType>;
    OnImpersonation: TAutoObservers<TSecurityImpersonationLevel>;
    OnStatistics: TAutoObservers<TTokenStatistics>;
    OnRestrictedSids: TAutoObservers<TArray<TGroup>>;
    OnSessionId: TAutoObservers<TSessionId>;
    OnSandboxInert: TAutoObservers<LongBool>;
    OnAuditPolicy: TAutoObservers<ITokenAuditPolicy>;
    OnOrigin: TAutoObservers<TLogonId>;
    OnElevation: TAutoObservers<TTokenElevationInfo>;
    OnHasRestrictions: TAutoObservers<LongBool>;
    OnFlags: TAutoObservers<TTokenFlags>;
    OnVirtualizationAllowed: TAutoObservers<LongBool>;
    OnVirtualizationEnabled: TAutoObservers<LongBool>;
    OnIntegrity: TAutoObservers<TGroup>;
    OnUIAccess: TAutoObservers<LongBool>;
    OnMandatoryPolicy: TAutoObservers<TTokenMandatoryPolicy>;
    OnLogonSids: TAutoObservers<TArray<TGroup>>;
    OnIsAppContainer: TAutoObservers<LongBool>;
    OnCapabilities: TAutoObservers<TArray<TGroup>>;
    OnAppContainerSid: TAutoObservers<ISid>;
    OnAppContainerInfo: TAutoObservers<TAppContainerInfo>;
    OnAppContainerNumber: TAutoObservers<Cardinal>;
    OnUserClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnDeviceClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnRestrictedUserClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnRestrictedDeviceClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnDeviceGroups: TAutoObservers<TArray<TGroup>>;
    OnRestrictedDeviceGroups: TAutoObservers<TArray<TGroup>>;
    OnSecurityAttributes: TAutoObservers<TArray<TSecurityAttribute>>;
    OnIsLPAC: TAutoObservers<Boolean>;
    OnIsRestricted: TAutoObservers<LongBool>;
    OnTrustLevel: TAutoObservers<ISid>;
    OnPrivateNamespace: TAutoObservers<LongBool>;
    OnSingletonAttributes: TAutoObservers<TArray<TSecurityAttribute>>;
    OnBnoIsolation: TAutoObservers<TBnoIsolation>;
    OnIsSandboxed: TAutoObservers<LongBool>;
    OnOriginatingTrustLevel: TAutoObservers<ISid>;
    FStrings: array [TTokenStringClass] of String;
    OnStringChange: array [TTokenStringClass] of TAutoEvent<TTokenStringClass, String>;
    function GetString(InfoClass: TTokenStringClass): String;
    procedure SetString(InfoClass: TTokenStringClass; const Value: String);
    property StringCache[InfoClass: TTokenStringClass]: String read GetString write SetString;
    constructor Create;
  end;
  IAutoTokenEvents = IAutoObject<TTokenEvents>;

// Retrieve shared token events or construct new private ones
function RetrieveTokenEvents(const Identity: TLuid): IAutoTokenEvents;

// Exception-safe string invoker
procedure SafeStringInvoker(
  const Callback: TEventCallback<TTokenStringClass, String>;
  const InfoClass: TTokenStringClass;
  const Value: String
);

implementation

uses
  Ntapi.crt, NtUtils.Security.Sid,
  DelphiUtils.Arrays, System.SysUtils, UI.Exceptions;

{$BOOLEVAL OFF}
{$IFOPT R+}{$DEFINE R+}{$ENDIF}
{$IFOPT Q+}{$DEFINE Q+}{$ENDIF}

type
  TEventStorage = record
    Identity: TLuid;
    Events: Weak<IAutoTokenEvents>;
    class var Storage: TArray<TEventStorage>;
  end;

function RetrieveTokenEvents;
var
  i, j, Index: Integer;
  NewEntry: TEventStorage;
begin
  // Do not store events without an identity in the global list
  if Identity = 0 then
    Exit(Auto.From(TTokenEvents.Create));

  // Remove already deleted entries
  j := 0;
  for i := 0 to High(TEventStorage.Storage) do
    if TEventStorage.Storage[i].Events.Upgrade(Result) then
    begin
      if i <> j then
        TEventStorage.Storage[j] := TEventStorage.Storage[i];

      Inc(j);
    end;
  SetLength(TEventStorage.Storage, j);

  // Locate an existing entry or where to put a new one
  Index := TArray.BinarySearchEx<TEventStorage>(
    TEventStorage.Storage,
    function (const Entry: TEventStorage): Integer
    begin
      {$R-}{$Q-}
      Result := Entry.Identity - Identity;
      {$IFDEF R+}{$R+}{$ENDIF}{$IFDEF Q+}{$Q+}{$ENDIF}
    end
  );

  if Index < 0 then
  begin
    // Create and insert the new events
    Result := Auto.From(TTokenEvents.Create);
    NewEntry.Identity := Identity;
    NewEntry.Events := Result;
    Insert(NewEntry, TEventStorage.Storage, -(Index + 1));
    Exit;
  end;

  // Retrieve events from an existing bucket
  if not TEventStorage.Storage[Index].Events.Upgrade(Result) then
  begin
    // The object is already gone, but we have a bucket; recreate it
    Result := Auto.From(TTokenEvents.Create);
    TEventStorage.Storage[Index].Events := Result;
  end;
end;

{ Custom exceotion-safe invokers }

procedure SafeStringInvoker;
begin
  // Workaround internal compiler error by re-implementing the function instread
  // of using TExceptionSafeInvoker.TwoParameters<TTokenStringClass, String>
  try
    Callback(InfoClass, Value);
  except
    on E: Exception do
      ReportException(E);
  end;
end;

{ Comparison functions }

function CompareBasicInfo(const A, B: TObjectBasicInformation): Boolean;
begin
  Result := memcmp(@A, @B, SizeOf(A)) = 0;
end;

function CompareHandles(const A, B: TArray<TSystemHandleEntry>): Boolean;
var
  i: Integer;
begin
  if  Length(A) <> Length(B) then
    Exit(False);

  for i := 0 to High(A) do
    if memcmp(@A[i], @B[i], SizeOf(A[i])) <> 0  then
      Exit(False);

  Result := True;
end;

function CompareKernelAddress(const A, B: Pointer): Boolean;
begin
  Result := A = B;
end;

function ComparePIDs(const A, B: TProcessId): Boolean;
begin
  Result := A = B;
end;

function CompareSid(const A, B: ISid): Boolean;
begin
  Result := (not Assigned(A) and not Assigned(B)) or
    (Assigned(A) and Assigned(B) and RtlxEqualSids(A, B));
end;

function CompareGroup(const A, B: TGroup): Boolean;
begin
  Result := (A.Attributes = B.Attributes) and CompareSid(A.Sid, B.Sid);
end;

function CompareGroups(const A, B: TArray<TGroup>): Boolean;
var
  i: Integer;
begin
  if  Length(A) <> Length(B) then
    Exit(False);

  for i := 0 to High(A) do
    if not CompareGroup(A[i], B[i]) then
      Exit(False);

  Result := True;
end;

function ComparePrivileges(const A, B: TArray<TPrivilege>): Boolean;
var
  i: Integer;
begin
  if  Length(A) <> Length(B) then
    Exit(False);

  for i := 0 to High(A) do
    if (A[i].Luid <> B[i].Luid) or (A[i].Attributes <> B[i].Attributes) then
      Exit(False);

  Result := True;
end;

function CompareSource(const A, B: TTokenSource): Boolean;
begin
  Result := (A.SourceIdentifier = B.SourceIdentifier) and (A.Name = B.Name);
end;

function CompareType(const A, B: TTokenType): Boolean;
begin
  Result := A = B;
end;

function CompareImpersonation(const A, B: TSecurityImpersonationLevel): Boolean;
begin
  Result := A = B;
end;

function CompareStatistics(const A, B: TTokenStatistics): Boolean;
begin
  Result := memcmp(@A, @B, SizeOf(A)) = 0;
end;

function CompareSessionId(const A, B: TSessionId): Boolean;
begin
  Result := A = B;
end;

function CompareLongBool(const A, B: LongBool): Boolean;
begin
  Result := A = B;
end;

function CompareOrigin(const A, B: TLogonId): Boolean;
begin
  Result := A = B;
end;

function CompareElevation(const A, B: TTokenElevationInfo): Boolean;
begin
  Result := (A.Elevated = B.Elevated) and (A.ElevationType = B.ElevationType);
end;

function CompareFlags(const A, B: TTokenFlags): Boolean;
begin
  Result := A = B;
end;

function CompareMandatoryPolicy(const A, B: TTokenMandatoryPolicy): Boolean;
begin
  Result := A = B;
end;

function CompareAppContainerInfo(const A, B: TAppContainerInfo): Boolean;
begin
  Result := CompareSid(A.User, B.User) and CompareSid(A.Package, B.Package) and
    CompareSid(A.ParentPackage, B.ParentPackage) and (A.Name = B.Name) and
    (A.DisplayName = B.DisplayName) and (A.ParentName = B.ParentName);
end;

function CompareBoolean(const A, B: Boolean): Boolean;
begin
  Result := A = B;
end;

function CompareBnoIsolation(const A, B: TBnoIsolation): Boolean;
begin
  Result := (A.Enabled = B.Enabled) and (A.Prefix = B.Prefix);
end;

{ TTokenEvents }

constructor TTokenEvents.Create;
var
  i: TTokenStringClass;
begin
  OnBasicInfo.Initialize(CompareBasicInfo);
  OnHandles.Initialize(CompareHandles);
  OnKernelAddress.Initialize(CompareKernelAddress);
  OnCreatorPID.Initialize(ComparePIDs);
  OnUser.Initialize(CompareGroup);
  OnGroups.Initialize(CompareGroups);
  OnPrivileges.Initialize(ComparePrivileges);
  OnOwner.Initialize(CompareSid);
  OnPrimaryGroup.Initialize(CompareSid);
  OnDefaultDacl.Initialize(nil); // TODO: ACL comparison
  OnSource.Initialize(CompareSource);
  OnType.Initialize(CompareType);
  OnImpersonation.Initialize(CompareImpersonation);
  OnStatistics.Initialize(CompareStatistics);
  OnRestrictedSids.Initialize(CompareGroups);
  OnSessionId.Initialize(CompareSessionId);
  OnSandboxInert.Initialize(CompareLongBool);
  OnAuditPolicy.Initialize(nil); // TODO: Audit comparison
  OnOrigin.Initialize(CompareOrigin);
  OnElevation.Initialize(CompareElevation);
  OnHasRestrictions.Initialize(CompareLongBool);
  OnFlags.Initialize(CompareFlags);
  OnVirtualizationAllowed.Initialize(CompareLongBool);
  OnVirtualizationEnabled.Initialize(CompareLongBool);
  OnIntegrity.Initialize(CompareGroup);
  OnUIAccess.Initialize(CompareLongBool);
  OnMandatoryPolicy.Initialize(CompareMandatoryPolicy);
  OnIsAppContainer.Initialize(CompareLongBool);
  OnCapabilities.Initialize(CompareGroups);
  OnAppContainerSid.Initialize(CompareSid);
  OnAppContainerInfo.Initialize(CompareAppContainerInfo);
  OnUserClaims.Initialize(nil); // TODO: Security attribute comparison
  OnDeviceClaims.Initialize(nil);
  OnRestrictedUserClaims.Initialize(nil);
  OnRestrictedDeviceClaims.Initialize(nil);
  OnDeviceGroups.Initialize(CompareGroups);
  OnRestrictedDeviceGroups.Initialize(CompareGroups);
  OnSecurityAttributes.Initialize(nil);
  OnIsLPAC.Initialize(CompareBoolean);
  OnIsRestricted.Initialize(CompareLongBool);
  OnTrustLevel.Initialize(CompareSid);
  OnPrivateNamespace.Initialize(CompareLongBool);
  OnSingletonAttributes.Initialize(nil);
  OnBnoIsolation.Initialize(CompareBnoIsolation);
  OnIsSandboxed.Initialize(CompareLongBool);
  OnOriginatingTrustLevel.Initialize(CompareSid);

  for i := Low(TTokenStringClass) to High(TTokenStringClass) do
    OnStringChange[i].SetCustomInvoker(SafeStringInvoker);
end;

function TTokenEvents.GetString;
begin
  Result := FStrings[InfoClass];
end;

procedure TTokenEvents.SetString;
begin
  if FStrings[InfoClass] <> Value then
  begin
    FStrings[InfoClass] := Value;
    OnStringChange[InfoClass].Invoke(InfoClass, Value);
  end;
end;

end.
