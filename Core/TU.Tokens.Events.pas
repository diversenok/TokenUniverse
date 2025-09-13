unit TU.Tokens.Events;

{
  This internal module powers the new infrastructure for sharing observers
  between token handles that point to the same kernel object.
}

interface

uses
  Ntapi.WinNt, Ntapi.ntobapi, Ntapi.ntseapi, Ntapi.winsta, Ntapi.appmodel,
  NtUtils, NtUtils.Objects.Snapshots, NtUtils.Tokens.Info, NtUtils.Profiles,
  NtUtils.Security.AppContainer, NtUtils.Lsa.Logon, DelphiUtils.AutoObjects,
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
    OnLogonInfo: TAutoObservers<ILogonSession>;
    OnRestrictedSids: TAutoObservers<TArray<TGroup>>;
    OnSessionId: TAutoObservers<TSessionId>;
    OnSessionInfo: TAutoObservers<TWinStationInformation>;
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
    OnAppContainerInfo: TAutoObservers<TRtlxAppContainerInfo>;
    OnAppContainerNumber: TAutoObservers<Cardinal>;
    OnUserClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnDeviceClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnRestrictedUserClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnRestrictedDeviceClaims: TAutoObservers<TArray<TSecurityAttribute>>;
    OnDeviceGroups: TAutoObservers<TArray<TGroup>>;
    OnRestrictedDeviceGroups: TAutoObservers<TArray<TGroup>>;
    OnSecurityAttributes: TAutoObservers<TArray<TSecurityAttribute>>;
    OnIsLPAC: TAutoObservers<Boolean>;
    OnPackageClaims: TAutoObservers<TPsPkgClaim>;
    OnIsRestricted: TAutoObservers<LongBool>;
    OnTrustLevel: TAutoObservers<ISid>;
    OnPrivateNamespace: TAutoObservers<LongBool>;
    OnSingletonAttributes: TAutoObservers<TArray<TSecurityAttribute>>;
    OnBnoIsolation: TAutoObservers<TBnoIsolation>;
    OnIsSandboxed: TAutoObservers<LongBool>;
    OnIsAppSilo: TAutoObservers<LongBool>;
    FStrings: array [TTokenStringClass] of String;
    OnStringChange: array [TTokenStringClass] of TAutoEvent<TTokenStringClass, String>;
    function GetString(InfoClass: TTokenStringClass): String;
    procedure SetString(InfoClass: TTokenStringClass; const Value: String);
    property StringCache[InfoClass: TTokenStringClass]: String read GetString write SetString;
    constructor Create;
  end;
  IAutoTokenEvents = IObject<TTokenEvents>;

// Retrieve shared token events or construct new private ones
function RetrieveTokenEvents(const Identity: TLuid): IAutoTokenEvents;

implementation

uses
  Ntapi.crt, Ntapi.ntrtl, NtUtils.Security.Sid,
  DelphiUtils.Arrays, System.SysUtils, NtUiCommon.Exceptions;

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
    Exit(Auto.CaptureObject(TTokenEvents.Create));

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
    function (const Entry: TEventStorage): NativeInt
    begin
      {$R-}{$Q-}
      Result := Entry.Identity - Identity;
      {$IFDEF R+}{$R+}{$ENDIF}{$IFDEF Q+}{$Q+}{$ENDIF}
    end
  );

  if Index < 0 then
  begin
    // Create and insert the new events
    Result := Auto.CaptureObject(TTokenEvents.Create);
    NewEntry.Identity := Identity;
    NewEntry.Events := Result;
    Insert(NewEntry, TEventStorage.Storage, -(Index + 1));
    Exit;
  end;

  // Retrieve events from an existing bucket
  if not TEventStorage.Storage[Index].Events.Upgrade(Result) then
  begin
    // The object is already gone, but we have a bucket; recreate it
    Result := Auto.CaptureObject(TTokenEvents.Create);
    TEventStorage.Storage[Index].Events := Result;
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

function CompareSidRaw(const A, B: PSid): Boolean;
begin
  Result := (not Assigned(A) and not Assigned(B)) or
    (Assigned(A) and Assigned(B) and RtlEqualSid(A, B));
end;

function CompareLogonInfo(const A, B: ILogonSession): Boolean;
begin
  Result := (A.Size = B.Size) and
    (A.Data.LogonID = B.Data.LogonID) and
    (A.Data.UserName.ToString = B.Data.UserName.ToString) and
    (A.Data.LogonDomain.ToString = B.Data.LogonDomain.ToString) and
    (A.Data.AuthenticationPackage.ToString = B.Data.AuthenticationPackage.ToString) and
    (A.Data.LogonType = B.Data.LogonType) and
    (A.Data.Session = B.Data.Session) and
    CompareSidRaw(A.Data.SID, B.Data.SID) and
    (A.Data.LogonTime = B.Data.LogonTime) and
    (A.Data.LogonServer.ToString = B.Data.LogonServer.ToString) and
    (A.Data.DNSDomainName.ToString = B.Data.DNSDomainName.ToString) and
    (A.Data.UPN.ToString = B.Data.UPN.ToString) and
    (A.Data.UserFlags = B.Data.UserFlags) and
    (memcmp(@A.Data.LastLogonInfo, @B.Data.LastLogonInfo, SizeOf(A.Data.LastLogonInfo)) = 0) and
    (A.Data.LogonScript.ToString = B.Data.LogonScript.ToString) and
    (A.Data.ProfilePath.ToString = B.Data.ProfilePath.ToString) and
    (A.Data.HomeDirectory.ToString = B.Data.HomeDirectory.ToString) and
    (A.Data.HomeDirectoryDrive.ToString = B.Data.HomeDirectoryDrive.ToString) and
    (A.Data.LogoffTime = B.Data.LogoffTime) and
    (A.Data.KickoffTime = B.Data.KickoffTime) and
    (A.Data.PasswordLastSet = B.Data.PasswordLastSet) and
    (A.Data.PasswordCanChange = B.Data.PasswordCanChange) and
    (A.Data.PasswordMustChange = B.Data.PasswordMustChange);
end;

function CompareSessionInfo(const A, B: TWinStationInformation): Boolean;
begin
  Result := memcmp(@A, @B, SizeOf(A)) = 0;
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

function CompareAppContainerInfo(const A, B: TRtlxAppContainerInfo): Boolean;
begin
  Result := CompareSid(A.Sid, B.Sid) and (A.Moniker = B.Moniker) and
    (A.DisplayName = B.DisplayName) and (A.ParentMoniker = B.ParentMoniker);
end;

function CompareBoolean(const A, B: Boolean): Boolean;
begin
  Result := A = B;
end;

function ComparePackageClaims(const A, B: TPsPkgClaim): Boolean;
begin
  Result := (A.Flags = B.Flags) and (A.Origin = B.Origin)
end;

function CompareBnoIsolation(const A, B: TBnoIsolation): Boolean;
begin
  Result := (A.Enabled = B.Enabled) and (A.Prefix = B.Prefix);
end;

{ TTokenEvents }

constructor TTokenEvents.Create;
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
  OnLogonInfo.Initialize(CompareLogonInfo);
  OnRestrictedSids.Initialize(CompareGroups);
  OnSessionId.Initialize(CompareSessionId);
  OnSessionInfo.Initialize(CompareSessionInfo);
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
  OnPackageClaims.Initialize(ComparePackageClaims);
  OnIsRestricted.Initialize(CompareLongBool);
  OnTrustLevel.Initialize(CompareSid);
  OnPrivateNamespace.Initialize(CompareLongBool);
  OnSingletonAttributes.Initialize(nil);
  OnBnoIsolation.Initialize(CompareBnoIsolation);
  OnIsSandboxed.Initialize(CompareLongBool);
  OnIsAppSilo.Initialize(CompareLongBool);
end;

function TTokenEvents.GetString;
begin
  {$IFOPT R+}
  if (InfoClass < Low(TTokenStringClass)) or
    (InfoClass > High(TTokenStringClass)) then
    raise ERangeError.Create('Invalid token string class');
  {$ENDIF}

  Result := FStrings[InfoClass];
end;

procedure TTokenEvents.SetString;
begin
  {$IFOPT R+}
  if (InfoClass < Low(TTokenStringClass)) or
    (InfoClass > High(TTokenStringClass)) then
    raise ERangeError.Create('Invalid token string class');
  {$ENDIF}

  if FStrings[InfoClass] <> Value then
  begin
    FStrings[InfoClass] := Value;
    OnStringChange[InfoClass].Invoke(InfoClass, Value);
  end;
end;

end.
