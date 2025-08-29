unit TU.Tokens;

{
  This module defines the improved IToken interface.
}

interface

uses
  Ntapi.WinNt, Ntapi.ntseapi, Ntapi.ntobapi, Ntapi.winsta, Ntapi.appmodel,
  NtUtils, NtUtils.Tokens.Info, NtUtils.Objects.Snapshots, NtUtils.Profiles,
  NtUtils.Security.AppContainer, NtUtils.Lsa.Logon, DelphiUtils.AutoObjects,
  DelphiUtils.AutoEvents;

type
  ITokenAuditPolicy = IMemory<PTokenAuditPolicy>;

  TTokenElevationInfo = record
    Elevated: LongBool;
    ElevationType: TTokenElevationType;
    function ToString: String;
  end;

  TTokenStringClass = (
    { Per-handle }
    tsCaption,                 // User-supplied label
    tsHandle,                  // Handle value in hex
    tsHandleDetailed,          // Handle value in dec and hex
    tsAccess,                  // Granted access mask as text
    tsAccessNumeric,           // Granted access mask as value

    { Per kernel object}
    tsHandleCount,             // Number of handles pointing to the object
    tsPagedPoolCharge,         // Number of bytes charged in paged pool
    tsNonPagedPoolCharge,      // Number of bytes charged in non-paged pool
    tsAddress,                 // Kernel object address
    tsCreator,                 // PID of the creator of the object
    tsUser,                    // User SID
    tsGroups,                  // Number of groups
    tsGroupsEnabled,           // Number of enabled groups
    tsPrivileges,              // Number of privileges
    tsPrivilegesEnabled,       // Number of enabled privileges
    tsOwner,                   // Owner SID
    tsPrimaryGroup,            // Primary group SID
    tsSourceName,              // Token source string
    tsSourceId,                // Token source LUID
    tsType,                    // Token type and impersonation level
    tsTokenId,                 // Unique token ID
    tsLogonId,                 // Logon session ID
    tsLogonAuthPackage,        // The name of the authentication packed of the logon session
    tsLogonType,               // The type of logon from the logon session
    tsLogonTime,               // The time of logon from the logon session
    tsModifiedId,              // Unique modified ID value
    tsExpires,                 // Token expiration time
    tsDynamicCharged,          // Number of bytes changed for the dynamic part of the object
    tsDynamicAvailable,        // Number of bytes available for the dynamic part of the object
    tsRestrictedSids,          // Number of restricting SIDs
    tsSessionId,               // Terminal session ID
    tsSessionInfo,             // Terminal session information
    tsSandBoxInert,            // Whether bypassing SRP and App Locker is enabled
    tsOrigin,                  // Originating logon session ID
    tsElevation,               // Token and logon session elevation state
    tsFlags,                   // A bit mask of various flags
    tsRestricted,              // Policy for applying restricting SIDs
    tsSessionReference,        // Whether the token references a logon session
    tsVirtualization,          // Filesystem/registry virtualization policy
    tsFiltered,                // Whether the token was filtered after creation
    tsUIAccess,                // Whether UIPI bypassing flag is set
    tsLowBox,                  // Whether the token is LowBox (aka. AppContainer)
    tsPrivateNamespace,        // Whether private namespace is enabled
    tsChildFlags,              // Child process creation policy
    tsPermissiveLearning,      // Whether permissive learning mode is enabled
    tsRedirectionTrust,        // Redirection trust policy
    tsIntegrity,               // Integrity level
    tsMandatoryPolicy,         // Mandatory policy settings
    tsLogonSid,                // Logon SID for accessing desktop and window station
    tsCapabilities,            // Number of capabilities
    tsAppContainerNumber,      // AppContainer number value
    tsAppContainerName,        // Moniker of the AppContainer profile
    tsAppContainerDisplayName, // DisplayName of the AppContainer profile
    tsUserClaims,              // Number of user claim attributes
    tsDeviceClaims,            // Number of device claim attributes
    tsRestrictedUserClaims,    // Number of restricted user claim attributes
    tsRestrictedDeviceClaims,  // Number of restricted device claim attributes
    tsDeviceGroups,            // Number of device groups
    tsRestrictedDeviceGroups,  // Number of restricted device groups
    tsSecAttributes,           // Number of security attributes
    tsSecAttributesNames,      // The names of all security attributes
    tsLPAC,                    // Whether Low Privilege AppContainer attribute set
    tsPackageFlags,            // Flags that define package attribute behavior
    tsPackageOrigin,           // The origin of the application package
    tsIsRestricted,            // Whether token is restricted or write-only restricted
    tsTrustLevel,              // Process protection level
    tsSingletonAttributes,     // Number of singleton security attributes
    tsBnoIsolation,            // Whether Base Named Objects isolation is enabled
    tsBnoPrefix,               // Prefix for Base Named Objects isolation
    tsIsSandboxed,             // Whether RtlIsSandboxedToken is TRUE
    tsIsAppSilo                // Whether the token has an AppSilo capability
  );

  // A subset of strings that are unique for each handle rather than shared
  // via the kernel object
  TTokenPerHandleStringClass = tsCaption..tsAccessNumeric;

  IToken = interface
    ['{7CF47C07-F5D1-4891-A2B8-83ED0C7419CF}']
    function GetHandle: IHandle;
    function GetCaption: String;
    function GetCachedKernelAddress: Pointer;
    procedure SetCaption(const Value: String);

    property Handle: IHandle read GetHandle;
    property Caption: String read GetCaption write SetCaption;
    property CachedKernelAddress: Pointer read GetCachedKernelAddress;

    // Querying
    function QueryString(InfoClass: TTokenStringClass; ForceRefresh: Boolean = False): String;
    function QueryBasicInfo(out Info: TObjectBasicInformation): TNtxStatus;
    function QueryHandles(out Handles: TArray<TSystemHandleEntry>): TNtxStatus;
    function QueryKernelAddress(out ObjectAddress: Pointer; ForceRefresh: Boolean = False): TNtxStatus;
    function QueryCreatorPID(out CreatorPID: TProcessId; ForceRefresh: Boolean = False): TNtxStatus;
    function QueryUser(out User: TGroup): TNtxStatus;
    function QueryGroups(out Groups: TArray<TGroup>): TNtxStatus;
    function QueryPrivileges(out Privileges: TArray<TPrivilege>): TNtxStatus;
    function QueryOwner(out Owner: ISid): TNtxStatus;
    function QueryPrimaryGroup(out PrimaryGroup: ISid): TNtxStatus;
    function QueryDefaultDacl(out DefaultDacl: IAcl): TNtxStatus;
    function QuerySource(out Source: TTokenSource): TNtxStatus;
    function QueryType(out TokenType: TTokenType): TNtxStatus;
    function QueryImpersonation(out Level: TSecurityImpersonationLevel): TNtxStatus;
    function QueryStatistics(out Statistics: TTokenStatistics): TNtxStatus;
    function QueryLogonInfo(out Info: ILogonSession): TNtxStatus;
    function QueryRestrictedSids(out Sids: TArray<TGroup>): TNtxStatus;
    function QuerySessionId(out SessionId: TSessionId): TNtxStatus;
    function QuerySessionInfo(out Info: TWinStationInformation): TNtxStatus;
    function QuerySandboxInert(out SandboxInert: LongBool): TNtxStatus;
    function QueryAuditPolicy(out AuditPolicy: ITokenAuditPolicy): TNtxStatus;
    function QueryOrigin(out Origin: TLogonId): TNtxStatus;
    function QueryElevation(out Elevation: TTokenElevationInfo): TNtxStatus;
    function QueryLinkedToken(out Token: IToken): TNtxStatus;
    function QueryHasRestrictions(out HasRestrictions: LongBool): TNtxStatus;
    function QueryFlags(out Flags: TTokenFlags): TNtxStatus;
    function QueryVirtualizationAllowed(out Allowed: LongBool): TNtxStatus;
    function QueryVirtualizationEnabled(out Enabled: LongBool): TNtxStatus;
    function QueryIntegrity(out Integrity: TGroup): TNtxStatus;
    function QueryUIAccess(out UIAccess: LongBool): TNtxStatus;
    function QueryMandatoryPolicy(out Policy: TTokenMandatoryPolicy): TNtxStatus;
    function QueryLogonSids(out LogonSids: TArray<TGroup>): TNtxStatus;
    function QueryIsAppContainer(out IsAppContainer: LongBool): TNtxStatus;
    function QueryCapabilities(out Capabilities: TArray<TGroup>): TNtxStatus;
    function QueryAppContainerSid(out Package: ISid): TNtxStatus;
    function QueryAppContainerInfo(out AppContainer: TRtlxAppContainerInfo): TNtxStatus;
    function QueryAppContainerNumber(out Number: Cardinal): TNtxStatus;
    function QueryUserClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryDeviceClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryRestrictedUserClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryRestrictedDeviceClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryDeviceGroups(out Groups: TArray<TGroup>): TNtxStatus;
    function QueryRestrictedDeviceGroups(out Groups: TArray<TGroup>): TNtxStatus;
    function QuerySecurityAttributes(out Attributes: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryIsLPAC(out IsLPAC: Boolean): TNtxStatus;
    function QueryPackageClaims(out PkgClaim: TPsPkgClaim): TNtxStatus;
    function QueryIsRestricted(out IsRestricted: LongBool): TNtxStatus;
    function QueryTrustLevel(out TrustLevel: ISid): TNtxStatus;
    function QueryPrivateNamespace(out PrivateNamespace: LongBool): TNtxStatus;
    function QuerySingletonAttributes(out Attributes: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryBnoIsolation(out Isolation: TBnoIsolation): TNtxStatus;
    function QueryIsSandboxed(out IsSandboxed: LongBool): TNtxStatus;
    function QueryIsAppSilo(out IsAppSilo: LongBool): TNtxStatus;

    // Observing changes
    function ObserveString(InfoClass: TTokenStringClass; Callback: TEventCallback<TTokenStringClass, String>; ForceRefresh: Boolean = False): IAutoReleasable;
    function ObserveBasicInfo(Callback: TEventCallback<TNtxStatus, TObjectBasicInformation>): IAutoReleasable;
    function ObserveHandles(Callback: TEventCallback<TNtxStatus, TArray<TSystemHandleEntry>>): IAutoReleasable;
    function ObserveKernelAddress(Callback: TEventCallback<TNtxStatus, Pointer>): IAutoReleasable;
    function ObserveCreatorPID(Callback: TEventCallback<TNtxStatus, TProcessId>): IAutoReleasable;
    function ObserveUser(Callback: TEventCallback<TNtxStatus, TGroup>): IAutoReleasable;
    function ObserveGroups(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObservePrivileges(Callback: TEventCallback<TNtxStatus, TArray<TPrivilege>>): IAutoReleasable;
    function ObserveOwner(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObservePrimaryGroup(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObserveDefaultDacl(Callback: TEventCallback<TNtxStatus, IAcl>): IAutoReleasable;
    function ObserveSource(Callback: TEventCallback<TNtxStatus, TTokenSource>): IAutoReleasable;
    function ObserveType(Callback: TEventCallback<TNtxStatus, TTokenType>): IAutoReleasable;
    function ObserveImpersonation(Callback: TEventCallback<TNtxStatus, TSecurityImpersonationLevel>): IAutoReleasable;
    function ObserveStatistics(Callback: TEventCallback<TNtxStatus, TTokenStatistics>): IAutoReleasable;
    function ObserveLogonInfo(Callback: TEventCallback<TNtxStatus, ILogonSession>): IAutoReleasable;
    function ObserveRestrictedSids(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveSessionId(Callback: TEventCallback<TNtxStatus, TSessionId>): IAutoReleasable;
    function ObserveSessionInfo(Callback: TEventCallback<TNtxStatus, TWinStationInformation>): IAutoReleasable;
    function ObserveSandboxInert(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveAuditPolicy(Callback: TEventCallback<TNtxStatus, ITokenAuditPolicy>): IAutoReleasable;
    function ObserveOrigin(Callback: TEventCallback<TNtxStatus, TLogonId>): IAutoReleasable;
    function ObserveElevation(Callback: TEventCallback<TNtxStatus, TTokenElevationInfo>): IAutoReleasable;
    function ObserveHasRestrictions(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveFlags(Callback: TEventCallback<TNtxStatus, TTokenFlags>): IAutoReleasable;
    function ObserveVirtualizationAllowed(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveVirtualizationEnabled(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveIntegrity(Callback: TEventCallback<TNtxStatus, TGroup>): IAutoReleasable;
    function ObserveUIAccess(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveMandatoryPolicy(Callback: TEventCallback<TNtxStatus, TTokenMandatoryPolicy>): IAutoReleasable;
    function ObserveLogonSids(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveIsAppContainer(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveCapabilities(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveAppContainerSid(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObserveAppContainerInfo(Callback: TEventCallback<TNtxStatus, TRtlxAppContainerInfo>): IAutoReleasable;
    function ObserveAppContainerNumber(Callback: TEventCallback<TNtxStatus, Cardinal>): IAutoReleasable;
    function ObserveUserClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveDeviceClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveRestrictedUserClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveRestrictedDeviceClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveDeviceGroups(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveRestrictedDeviceGroups(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveSecurityAttributes(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveIsLPAC(Callback: TEventCallback<TNtxStatus, Boolean>): IAutoReleasable;
    function ObservePackageClaims(Callback: TEventCallback<TNtxStatus, TPsPkgClaim>): IAutoReleasable;
    function ObserveIsRestricted(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveTrustLevel(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObservePrivateNamespace(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveSingletonAttributes(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveBnoIsolation(Callback: TEventCallback<TNtxStatus, TBnoIsolation>): IAutoReleasable;
    function ObserveIsSandboxed(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveIsAppSilo(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;

    // Refreshing
    procedure SmartRefresh;
    procedure RefreshString(InfoClass: TTokenStringClass);
    function RefreshBasicInfo: TNtxStatus;
    function RefreshHandles: TNtxStatus;
    function RefreshKernelAddress(ForceRefresh: Boolean = True): TNtxStatus;
    function RefreshCreatorPID(ForceRefresh: Boolean = True): TNtxStatus;
    function RefreshUser: TNtxStatus;
    function RefreshGroups: TNtxStatus;
    function RefreshPrivileges: TNtxStatus;
    function RefreshOwner: TNtxStatus;
    function RefreshPrimaryGroup: TNtxStatus;
    function RefreshDefaultDacl: TNtxStatus;
    function RefreshSource: TNtxStatus;
    function RefreshType: TNtxStatus;
    function RefreshImpersonation: TNtxStatus;
    function RefreshStatistics: TNtxStatus;
    function RefreshLogonInfo: TNtxStatus;
    function RefreshRestrictedSids: TNtxStatus;
    function RefreshSessionId: TNtxStatus;
    function RefreshSessionInfo: TNtxStatus;
    function RefreshSandboxInert: TNtxStatus;
    function RefreshAuditPolicy: TNtxStatus;
    function RefreshOrigin: TNtxStatus;
    function RefreshElevation: TNtxStatus;
    function RefreshHasRestrictions: TNtxStatus;
    function RefreshFlags: TNtxStatus;
    function RefreshVirtualizationAllowed: TNtxStatus;
    function RefreshVirtualizationEnabled: TNtxStatus;
    function RefreshIntegrity: TNtxStatus;
    function RefreshUIAccess: TNtxStatus;
    function RefreshMandatoryPolicy: TNtxStatus;
    function RefreshLogonSids: TNtxStatus;
    function RefreshIsAppContainer: TNtxStatus;
    function RefreshCapabilities: TNtxStatus;
    function RefreshAppContainerSid: TNtxStatus;
    function RefreshAppContainerInfo: TNtxStatus;
    function RefreshAppContainerNumber: TNtxStatus;
    function RefreshUserClaims: TNtxStatus;
    function RefreshDeviceClaims: TNtxStatus;
    function RefreshRestrictedUserClaims: TNtxStatus;
    function RefreshRestrictedDeviceClaims: TNtxStatus;
    function RefreshDeviceGroups: TNtxStatus;
    function RefreshRestrictedDeviceGroups: TNtxStatus;
    function RefreshSecurityAttributes: TNtxStatus;
    function RefreshIsLPAC: TNtxStatus;
    function RefreshPackageClaims: TNtxStatus;
    function RefreshIsRestricted: TNtxStatus;
    function RefreshTrustLevel: TNtxStatus;
    function RefreshPrivateNamespace: TNtxStatus;
    function RefreshSingletonAttributes: TNtxStatus;
    function RefreshBnoIsolation: TNtxStatus;
    function RefreshIsSandboxed: TNtxStatus;
    function RefreshIsAppSilo: TNtxStatus;

    // Setting
    function SetOwner(const Owner: ISid): TNtxStatus;
    function SetPrimaryGroup(const PrimaryGroup: ISid): TNtxStatus;
    function SetDefaultDacl(const DefaultDacl: IAcl): TNtxStatus;
    function SetSessionId(const SessionId: TSessionId): TNtxStatus;
    function SetSessionReference(const Reference: LongBool): TNtxStatus;
    function SetAuditPolicy(const AuditPolicy: ITokenAuditPolicy): TNtxStatus;
    function SetOrigin(const Origin: TLogonId): TNtxStatus;
    function SetLinkedToken(const Token: IToken): TNtxStatus;
    function SetVirtualizationAllowed(const Allowed: LongBool): TNtxStatus;
    function SetVirtualizationEnabled(const Enabled: LongBool): TNtxStatus;
    function SetIntegrity(const Level: TIntegrityRid): TNtxStatus;
    function SetUIAccess(const UIAccess: LongBool): TNtxStatus;
    function SetMandatoryPolicy(const Policy: TTokenMandatoryPolicy): TNtxStatus;
    function SetPrivateNamespace(const PrivateNamespace: LongBool): TNtxStatus;
    function SetChildProcessFlags(const Blocked: LongBool): TNtxStatus;

    // Operations
    function AssignToProcess(const hxProcess: IHandle): TNtxStatus;
    function AssignToProcessById(const PID: TProcessId): TNtxStatus;
    function AssignToThread(const hxThread: IHandle): TNtxStatus;
    function AssignToThreadById(const TID: TThreadId): TNtxStatus;
    function AssignToThreadSafe(const hxThread: IHandle): TNtxStatus;
    function AssignToThreadSafeById(const TID: TThreadId): TNtxStatus;
    function AdjustGroupsReset: TNtxStatus;
    function AdjustGroups(const SIDs: TArray<ISid>; const NewState: TGroupAttributes): TNtxStatus;
    function AdjustPrivileges(const Privileges: TArray<TSeWellKnownPrivilege>; const NewState: TPrivilegeAttributes; const IgnoreMissing: Boolean = False): TNtxStatus;
  end;

// Make an IToken instance from a handle
function CaptureTokenHandle(
  const Handle: IHandle;
  const Caption: String;
  KernelAddress: Pointer = nil
): IToken;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntpsapi, DelphiApi.Reflection, NtUtils.Security.Sid,
  NtUtils.Lsa.Sid, NtUtils.Objects, NtUtils.Tokens, NtUtils.Tokens.Impersonate,
  NtUtils.WinStation, NtUtils.SysUtils, DelphiUiLib.Strings, DelphiUtils.Arrays,
  DelphiUiLib.Reflection, DelphiUiLib.Reflection.Strings,
  NtUiLib.Reflection.Types, System.SysUtils, TU.Tokens.Events, TU.Events;

{ Helper functions }

function TrustLevelToString([opt] const TrustSid: ISid): String;
var
  SubAuthorities: TArray<Cardinal>;
begin
  if not Assigned(TrustSid) then
    Exit('None');

  SubAuthorities := RtlxSubAuthoritiesSid(TrustSid);

  if (RtlxIdentifierAuthoritySid(TrustSid) <> SECURITY_PROCESS_TRUST_AUTHORITY)
    or (Length(SubAuthorities) <> SECURITY_PROCESS_TRUST_AUTHORITY_RID_COUNT) then
    Exit('Invalid');

  Result := TType.Represent<TSecurityTrustType>(SubAuthorities[0]).Text +
    ' (' + TType.Represent<TSecurityTrustLevel>(SubAuthorities[1]).Text + ')';
end;

function SessionInfoToString(const Info: TWinStationInformation): String;
begin
  Result := UiLibUIntToDec(Info.LogonID);

  if Info.WinStationName <> '' then
    Result := Format('%s: %s', [Result, Info.WinStationName]);

  Result := Format('%s (%s)', [Result, Info.FullUserName]);
end;

function TTokenElevationInfo.ToString;
begin
  Result  := BooleanToString(Elevated, bkYesNo) + ' (' + TType.Represent(
    ElevationType).Text + ')';
end;

type
  TToken = class (TInterfacedObject, IToken)
    hxToken: IHandle;
    FKernelObjectAddress: Pointer;
    FPerHandleStrings: array [TTokenPerHandleStringClass] of String;
    FPerHandleEvents: array [TTokenPerHandleStringClass] of TAutoEvent<TTokenStringClass, String>;
    FSharedEvents: IAutoTokenEvents;

    // Global subscriptions
    SystemHandlesSubscription: IAutoReleasable;
    SystemObjectsSubscription: IAutoReleasable;
    LinkedLogonSessionsSubscription: IAutoReleasable;
    procedure ChangedSystemHandles(const AllHandles: TArray<TSystemHandleEntry>);
    procedure ChangedSystemObjects(const KernelObjects: TArray<TObjectTypeEntry>);
    procedure LinkedLogonSessions;

    constructor Create(const Handle: IHandle; const Caption: String; KernelObjectAddress: Pointer = nil);
    function GetHandle: IHandle;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    function GetCachedKernelAddress: Pointer;
    function GetEvents: TTokenEvents;
    property Events: TTokenEvents read GetEvents;
    function GetPerHandleString(InfoClass: TTokenPerHandleStringClass): String;
    procedure SetPerHandleString(InfoClass: TTokenPerHandleStringClass; const Value: String);
    property PerHandleString[InfoClass: TTokenPerHandleStringClass]: String read GetPerHandleString write SetPerHandleString;
    function QueryString(InfoClass: TTokenStringClass; ForceRefresh: Boolean = False): String;
    function QueryBasicInfo(out Info: TObjectBasicInformation): TNtxStatus;
    function QueryHandles(out Handles: TArray<TSystemHandleEntry>): TNtxStatus;
    function QueryKernelAddress(out ObjectAddress: Pointer; ForceRefresh: Boolean = False): TNtxStatus;
    function QueryCreatorPID(out CreatorPID: TProcessId; ForceRefresh: Boolean = False): TNtxStatus;
    function QueryUser(out User: TGroup): TNtxStatus;
    function QueryGroups(out Groups: TArray<TGroup>): TNtxStatus;
    function QueryPrivileges(out Privileges: TArray<TPrivilege>): TNtxStatus;
    function QueryOwner(out Owner: ISid): TNtxStatus;
    function QueryPrimaryGroup(out PrimaryGroup: ISid): TNtxStatus;
    function QueryDefaultDacl(out DefaultDacl: IAcl): TNtxStatus;
    function QuerySource(out Source: TTokenSource): TNtxStatus;
    function QueryType(out &Type: TTokenType): TNtxStatus;
    function QueryImpersonation(out Level: TSecurityImpersonationLevel): TNtxStatus;
    function QueryStatistics(out Statistics: TTokenStatistics): TNtxStatus;
    function QueryLogonInfo(out Info: ILogonSession): TNtxStatus;
    function QueryRestrictedSids(out Sids: TArray<TGroup>): TNtxStatus;
    function QuerySessionId(out SessionId: TSessionId): TNtxStatus;
    function QuerySessionInfo(out Info: TWinStationInformation): TNtxStatus;
    function QuerySandboxInert(out SandboxInert: LongBool): TNtxStatus;
    function QueryAuditPolicy(out AuditPolicy: ITokenAuditPolicy): TNtxStatus;
    function QueryOrigin(out Origin: TLogonId): TNtxStatus;
    function QueryElevation(out Elevation: TTokenElevationInfo): TNtxStatus;
    function QueryLinkedToken(out Token: IToken): TNtxStatus;
    function QueryHasRestrictions(out HasRestrictions: LongBool): TNtxStatus;
    function QueryFlags(out Flags: TTokenFlags): TNtxStatus;
    function QueryVirtualizationAllowed(out Allowed: LongBool): TNtxStatus;
    function QueryVirtualizationEnabled(out Enabled: LongBool): TNtxStatus;
    function QueryIntegrity(out Integrity: TGroup): TNtxStatus;
    function QueryUIAccess(out UIAccess: LongBool): TNtxStatus;
    function QueryMandatoryPolicy(out Policy: TTokenMandatoryPolicy): TNtxStatus;
    function QueryLogonSids(out LogonSids: TArray<TGroup>): TNtxStatus;
    function QueryIsAppContainer(out IsAppContainer: LongBool): TNtxStatus;
    function QueryCapabilities(out Capabilities: TArray<TGroup>): TNtxStatus;
    function QueryAppContainerSid(out Package: ISid): TNtxStatus;
    function QueryAppContainerInfo(out AppContainer: TRtlxAppContainerInfo): TNtxStatus;
    function QueryAppContainerNumber(out Number: Cardinal): TNtxStatus;
    function QueryUserClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryDeviceClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryRestrictedUserClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryRestrictedDeviceClaims(out Claims: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryDeviceGroups(out Groups: TArray<TGroup>): TNtxStatus;
    function QueryRestrictedDeviceGroups(out Groups: TArray<TGroup>): TNtxStatus;
    function QuerySecurityAttributes(out Attributes: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryIsLPAC(out IsLPAC: Boolean): TNtxStatus;
    function QueryPackageClaims(out PkgClaim: TPsPkgClaim): TNtxStatus;
    function QueryIsRestricted(out IsRestricted: LongBool): TNtxStatus;
    function QueryTrustLevel(out TrustLevel: ISid): TNtxStatus;
    function QueryPrivateNamespace(out PrivateNamespace: LongBool): TNtxStatus;
    function QuerySingletonAttributes(out Attributes: TArray<TSecurityAttribute>): TNtxStatus;
    function QueryBnoIsolation(out Isolation: TBnoIsolation): TNtxStatus;
    function QueryIsSandboxed(out IsSandboxed: LongBool): TNtxStatus;
    function QueryIsAppSilo(out IsAppSilo: LongBool): TNtxStatus;
    function ObserveString(InfoClass: TTokenStringClass; Callback: TEventCallback<TTokenStringClass, String>; ForceRefresh: Boolean = False): IAutoReleasable;
    function ObserveBasicInfo(Callback: TEventCallback<TNtxStatus, TObjectBasicInformation>): IAutoReleasable;
    function ObserveHandles(Callback: TEventCallback<TNtxStatus, TArray<TSystemHandleEntry>>): IAutoReleasable;
    function ObserveKernelAddress(Callback: TEventCallback<TNtxStatus, Pointer>): IAutoReleasable;
    function ObserveCreatorPID(Callback: TEventCallback<TNtxStatus, TProcessId>): IAutoReleasable;
    function ObserveUser(Callback: TEventCallback<TNtxStatus, TGroup>): IAutoReleasable;
    function ObserveGroups(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObservePrivileges(Callback: TEventCallback<TNtxStatus, TArray<TPrivilege>>): IAutoReleasable;
    function ObserveOwner(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObservePrimaryGroup(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObserveDefaultDacl(Callback: TEventCallback<TNtxStatus, IAcl>): IAutoReleasable;
    function ObserveSource(Callback: TEventCallback<TNtxStatus, TTokenSource>): IAutoReleasable;
    function ObserveType(Callback: TEventCallback<TNtxStatus, TTokenType>): IAutoReleasable;
    function ObserveImpersonation(Callback: TEventCallback<TNtxStatus, TSecurityImpersonationLevel>): IAutoReleasable;
    function ObserveStatistics(Callback: TEventCallback<TNtxStatus, TTokenStatistics>): IAutoReleasable;
    function ObserveLogonInfo(Callback: TEventCallback<TNtxStatus, ILogonSession>): IAutoReleasable;
    function ObserveRestrictedSids(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveSessionId(Callback: TEventCallback<TNtxStatus, TSessionId>): IAutoReleasable;
    function ObserveSessionInfo(Callback: TEventCallback<TNtxStatus, TWinStationInformation>): IAutoReleasable;
    function ObserveSandboxInert(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveAuditPolicy(Callback: TEventCallback<TNtxStatus, ITokenAuditPolicy>): IAutoReleasable;
    function ObserveOrigin(Callback: TEventCallback<TNtxStatus, TLogonId>): IAutoReleasable;
    function ObserveElevation(Callback: TEventCallback<TNtxStatus, TTokenElevationInfo>): IAutoReleasable;
    function ObserveHasRestrictions(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveFlags(Callback: TEventCallback<TNtxStatus, TTokenFlags>): IAutoReleasable;
    function ObserveVirtualizationAllowed(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveVirtualizationEnabled(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveIntegrity(Callback: TEventCallback<TNtxStatus, TGroup>): IAutoReleasable;
    function ObserveUIAccess(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveMandatoryPolicy(Callback: TEventCallback<TNtxStatus, TTokenMandatoryPolicy>): IAutoReleasable;
    function ObserveLogonSids(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveIsAppContainer(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveCapabilities(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveAppContainerSid(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObserveAppContainerInfo(Callback: TEventCallback<TNtxStatus, TRtlxAppContainerInfo>): IAutoReleasable;
    function ObserveAppContainerNumber(Callback: TEventCallback<TNtxStatus, Cardinal>): IAutoReleasable;
    function ObserveUserClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveDeviceClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveRestrictedUserClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveRestrictedDeviceClaims(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveDeviceGroups(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveRestrictedDeviceGroups(Callback: TEventCallback<TNtxStatus, TArray<TGroup>>): IAutoReleasable;
    function ObserveSecurityAttributes(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveIsLPAC(Callback: TEventCallback<TNtxStatus, Boolean>): IAutoReleasable;
    function ObservePackageClaims(Callback: TEventCallback<TNtxStatus, TPsPkgClaim>): IAutoReleasable;
    function ObserveIsRestricted(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveTrustLevel(Callback: TEventCallback<TNtxStatus, ISid>): IAutoReleasable;
    function ObservePrivateNamespace(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveSingletonAttributes(Callback: TEventCallback<TNtxStatus, TArray<TSecurityAttribute>>): IAutoReleasable;
    function ObserveBnoIsolation(Callback: TEventCallback<TNtxStatus, TBnoIsolation>): IAutoReleasable;
    function ObserveIsSandboxed(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    function ObserveIsAppSilo(Callback: TEventCallback<TNtxStatus, LongBool>): IAutoReleasable;
    procedure SmartRefresh;
    procedure RefreshString(InfoClass: TTokenStringClass);
    function RefreshBasicInfo: TNtxStatus;
    function RefreshHandles: TNtxStatus;
    function RefreshKernelAddress(ForceRefresh: Boolean = True): TNtxStatus;
    function RefreshCreatorPID(ForceRefresh: Boolean = True): TNtxStatus;
    function RefreshUser: TNtxStatus;
    function RefreshGroups: TNtxStatus;
    function RefreshPrivileges: TNtxStatus;
    function RefreshOwner: TNtxStatus;
    function RefreshPrimaryGroup: TNtxStatus;
    function RefreshDefaultDacl: TNtxStatus;
    function RefreshSource: TNtxStatus;
    function RefreshType: TNtxStatus;
    function RefreshImpersonation: TNtxStatus;
    function RefreshStatistics: TNtxStatus;
    function RefreshLogonInfo: TNtxStatus;
    function RefreshRestrictedSids: TNtxStatus;
    function RefreshSessionId: TNtxStatus;
    function RefreshSessionInfo: TNtxStatus;
    function RefreshSandboxInert: TNtxStatus;
    function RefreshAuditPolicy: TNtxStatus;
    function RefreshOrigin: TNtxStatus;
    function RefreshElevation: TNtxStatus;
    function RefreshHasRestrictions: TNtxStatus;
    function RefreshFlags: TNtxStatus;
    function RefreshVirtualizationAllowed: TNtxStatus;
    function RefreshVirtualizationEnabled: TNtxStatus;
    function RefreshIntegrity: TNtxStatus;
    function RefreshUIAccess: TNtxStatus;
    function RefreshMandatoryPolicy: TNtxStatus;
    function RefreshLogonSids: TNtxStatus;
    function RefreshIsAppContainer: TNtxStatus;
    function RefreshCapabilities: TNtxStatus;
    function RefreshAppContainerSid: TNtxStatus;
    function RefreshAppContainerInfo: TNtxStatus;
    function RefreshAppContainerNumber: TNtxStatus;
    function RefreshUserClaims: TNtxStatus;
    function RefreshDeviceClaims: TNtxStatus;
    function RefreshRestrictedUserClaims: TNtxStatus;
    function RefreshRestrictedDeviceClaims: TNtxStatus;
    function RefreshDeviceGroups: TNtxStatus;
    function RefreshRestrictedDeviceGroups: TNtxStatus;
    function RefreshSecurityAttributes: TNtxStatus;
    function RefreshIsLPAC: TNtxStatus;
    function RefreshPackageClaims: TNtxStatus;
    function RefreshIsRestricted: TNtxStatus;
    function RefreshTrustLevel: TNtxStatus;
    function RefreshPrivateNamespace: TNtxStatus;
    function RefreshSingletonAttributes: TNtxStatus;
    function RefreshBnoIsolation: TNtxStatus;
    function RefreshIsSandboxed: TNtxStatus;
    function RefreshIsAppSilo: TNtxStatus;
    function SetOwner(const Owner: ISid): TNtxStatus;
    function SetPrimaryGroup(const PrimaryGroup: ISid): TNtxStatus;
    function SetDefaultDacl(const DefaultDacl: IAcl): TNtxStatus;
    function SetSessionId(const SessionId: TSessionId): TNtxStatus;
    function SetSessionReference(const Reference: LongBool): TNtxStatus;
    function SetAuditPolicy(const AuditPolicy: ITokenAuditPolicy): TNtxStatus;
    function SetOrigin(const Origin: TLogonId): TNtxStatus;
    function SetLinkedToken(const Token: IToken): TNtxStatus;
    function SetVirtualizationAllowed(const Allowed: LongBool): TNtxStatus;
    function SetVirtualizationEnabled(const Enabled: LongBool): TNtxStatus;
    function SetIntegrity(const Level: TIntegrityRid): TNtxStatus;
    function SetUIAccess(const UIAccess: LongBool): TNtxStatus;
    function SetMandatoryPolicy(const Policy: TTokenMandatoryPolicy): TNtxStatus;
    function SetPrivateNamespace(const PrivateNamespace: LongBool): TNtxStatus;
    function SetChildProcessFlags(const Blocked: LongBool): TNtxStatus;
    function AssignToProcess(const hxProcess: IHandle): TNtxStatus;
    function AssignToProcessById(const PID: TProcessId): TNtxStatus;
    function AssignToThread(const hxThread: IHandle): TNtxStatus;
    function AssignToThreadById(const TID: TThreadId): TNtxStatus;
    function AssignToThreadSafe(const hxThread: IHandle): TNtxStatus;
    function AssignToThreadSafeById(const TID: TThreadId): TNtxStatus;
    function AdjustGroupsReset: TNtxStatus;
    function AdjustGroups(const SIDs: TArray<ISid>; const NewState: TGroupAttributes): TNtxStatus;
    function AdjustPrivileges(const Privileges: TArray<TSeWellKnownPrivilege>; const NewState: TPrivilegeAttributes; const IgnoreMissing: Boolean): TNtxStatus;
  end;

function CaptureTokenHandle;
begin
  Result := TToken.Create(Handle, Caption, KernelAddress);
end;

{ TToken }

function TToken.AdjustGroups;
begin
  Result := NtxAdjustGroups(hxToken, Sids, NewState, False);
  SmartRefresh;
end;

function TToken.AdjustGroupsReset;
begin
  Result := NtxAdjustGroups(hxToken, nil, 0, True);
  SmartRefresh;
end;

function TToken.AdjustPrivileges;
begin
  Result := NtxAdjustPrivileges(hxToken, Privileges, NewState, IgnoreMissing);
  SmartRefresh;
end;

function TToken.AssignToProcess;
begin
  Result := NtxAssignPrimaryToken(hxProcess, hxToken);
  SmartRefresh;
end;

function TToken.AssignToProcessById;
begin
  Result := NtxAssignPrimaryTokenById(PID, hxToken);
  SmartRefresh;
end;

function TToken.AssignToThread;
begin
  Result := NtxSetThreadToken(hxThread, hxToken);
  SmartRefresh;
end;

function TToken.AssignToThreadById;
begin
  Result := NtxSetThreadTokenById(TID, hxToken);
  SmartRefresh;
end;

function TToken.AssignToThreadSafe;
begin
  Result := NtxSafeSetThreadToken(hxThread, hxToken);
  SmartRefresh;
end;

function TToken.AssignToThreadSafeById;
begin
  Result := NtxSafeSetThreadTokenById(TID, hxToken);
  SmartRefresh;
end;

procedure TToken.ChangedSystemHandles;
var
  Handles: TArray<TSystemHandleEntry>;
  Status: TNtxStatus;
  Entry: TSystemHandleEntry;
begin
  if Assigned(FKernelObjectAddress) and not Events.OnHandles.HasObservers then
    Exit;

  // Find the current handle info
  Status := RtlxFindHandleEntry(AllHandles, NtCurrentProcessId,
    hxToken.Handle, Entry);

  if not Status.IsSuccess then
    Exit;

  // Save kernel address when available
  if not Assigned(FKernelObjectAddress) and Assigned(Entry.PObject)  then
  begin
    FKernelObjectAddress := Entry.PObject;
    Events.StringCache[tsAddress] := UiLibUIntToHex(
      UIntPtr(FKernelObjectAddress));
    Events.OnKernelAddress.Notify(Default(TNtxStatus), FKernelObjectAddress);
  end;

  // Find handles pointing to the same object
  if Events.OnHandles.HasObservers then
  begin
    Status := RtlxFilterHandlesByHandle(Handles, hxToken);

    if not Status.IsSuccess then
      Handles := nil;

    Events.OnHandles.Notify(Status, Handles);
  end;
end;

procedure TToken.ChangedSystemObjects;
var
  Entry: PObjectEntry;
begin
  if not Assigned(FKernelObjectAddress) or Events.CreatorPIDIsKnown then
    Exit;

  Entry := RtlxFindObjectByAddress(KernelObjects, FKernelObjectAddress);

  if Assigned(Entry) then
  begin
    Events.CreatorPID := Entry.Other.CreatorUniqueProcess;

    if Events.CreatorPID = NtCurrentProcessId then
      Events.StringCache[tsCreator] := 'Current Process'
    else
      Events.StringCache[tsCreator] := TType.Represent(
        Events.CreatorPID).Text;
  end
  else
  begin
    Events.CreatorPID := 0;
    Events.StringCache[tsCreator] := 'Kernel';
  end;

  Events.CreatorPIDIsKnown := True;
  Events.OnCreatorPID.Notify(Default(TNtxStatus), Events.CreatorPID);
end;

constructor TToken.Create;
begin
  hxToken := Handle;
  FKernelObjectAddress := KernelObjectAddress;
  FPerHandleStrings[tsCaption] := Caption;
  FPerHandleStrings[tsHandle] := UiLibUIntToHex(hxToken.Handle, 4 or
    NUMERIC_WIDTH_ROUND_TO_BYTE);
  FPerHandleStrings[tsHandleDetailed] := Format('%s (%s)',
    [UiLibUIntToDec(hxToken.Handle), FPerHandleStrings[tsHandle]]);

  SystemHandlesSubscription := TGlobalEvents.SubscribeHandles(ChangedSystemHandles);
  SystemObjectsSubscription := TGlobalEvents.SubscribeObjects(ChangedSystemObjects);
  LinkedLogonSessionsSubscription := TGlobalEvents.OnLinkLogonSessions.Subscribe(LinkedLogonSessions);
end;

function TToken.GetCachedKernelAddress;
begin
  Result := FKernelObjectAddress;
end;

function TToken.GetCaption;
begin
  Result := FPerHandleStrings[tsCaption];
end;

function TToken.GetEvents;
var
  Statistics: TTokenStatistics;
begin
  if not Assigned(FSharedEvents) then
  begin
    // Establish identity of the token.
    // NOTE: do not use IToken's method to avoid infinite recursion
    if not NtxToken.Query(hxToken, TokenStatistics, Statistics).IsSuccess then
      Statistics.TokenId := 0;

    FSharedEvents := RetrieveTokenEvents(Statistics.TokenId);
  end;

  Result := FSharedEvents.Self;
end;

function TToken.GetHandle;
begin
  Result := hxToken;
end;

function TToken.GetPerHandleString;
begin
  {$IFOPT R+}
  if (InfoClass < Low(TTokenPerHandleStringClass)) or
    (InfoClass > High(TTokenPerHandleStringClass)) then
    raise ERangeError.Create('Invalid per-handle token string class');
  {$ENDIF}

  Result := FPerHandleStrings[InfoClass];
end;

procedure TToken.LinkedLogonSessions;
begin
  SmartRefresh;
end;

function TToken.ObserveAppContainerInfo;
var
  Info: TRtlxAppContainerInfo;
begin
  Callback(QueryAppContainerInfo(Info), Info);
  Result := Events.OnAppContainerInfo.Subscribe(Callback);
end;

function TToken.ObserveAppContainerNumber;
var
  Info: Cardinal;
begin
  Callback(QueryAppContainerNumber(Info), Info);
  Result := Events.OnAppContainerNumber.Subscribe(Callback);
end;

function TToken.ObserveAppContainerSid;
var
  Info: ISid;
begin
  Callback(QueryAppContainerSid(Info), Info);
  Result := Events.OnAppContainerSid.Subscribe(Callback);
end;

function TToken.ObserveAuditPolicy;
var
  Info: ITokenAuditPolicy;
begin
  Callback(QueryAuditPolicy(Info), Info);
  Result := Events.OnAuditPolicy.Subscribe(Callback);
end;

function TToken.ObserveBasicInfo;
var
  Info: TObjectBasicInformation;
begin
  Callback(QueryBasicInfo(Info), Info);
  Result := Events.OnBasicInfo.Subscribe(Callback);
end;

function TToken.ObserveBnoIsolation;
var
  Info: TBnoIsolation;
begin
  Callback(QueryBnoIsolation(Info), Info);
  Result := Events.OnBnoIsolation.Subscribe(Callback);
end;

function TToken.ObserveCapabilities;
var
  Info: TArray<TGroup>;
begin
  Callback(QueryCapabilities(Info), Info);
  Result := Events.OnCapabilities.Subscribe(Callback);
end;

function TToken.ObserveCreatorPID;
var
  Info: TProcessId;
begin
  Callback(QueryCreatorPID(Info), Info);
  Result := Events.OnCreatorPID.Subscribe(Callback);
end;

function TToken.ObserveDefaultDacl;
var
  Info: IAcl;
begin
  Callback(QueryDefaultDacl(Info), Info);
  Result := Events.OnDefaultDacl.Subscribe(Callback);
end;

function TToken.ObserveDeviceClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Callback(QueryDeviceClaims(Info), Info);
  Result := Events.OnDeviceClaims.Subscribe(Callback);
end;

function TToken.ObserveDeviceGroups;
var
  Info: TArray<TGroup>;
begin
  Callback(QueryDeviceGroups(Info), Info);
  Result := Events.OnDeviceGroups.Subscribe(Callback);
end;

function TToken.ObserveElevation;
var
  Info: TTokenElevationInfo;
begin
  Callback(QueryElevation(Info), Info);
  Result := Events.OnElevation.Subscribe(Callback);
end;

function TToken.ObserveFlags;
var
  Info: TTokenFlags;
begin
  Callback(QueryFlags(Info), Info);
  Result := Events.OnFlags.Subscribe(Callback);
end;

function TToken.ObserveGroups;
var
  Info: TArray<TGroup>;
begin
  Callback(QueryGroups(Info), Info);
  Result := Events.OnGroups.Subscribe(Callback);
end;

function TToken.ObserveHandles;
var
  Info: TArray<TSystemHandleEntry>;
begin
  Callback(QueryHandles(Info), Info);
  Result := Events.OnHandles.Subscribe(Callback);
end;

function TToken.ObserveHasRestrictions;
var
  Info: LongBool;
begin
  Callback(QueryHasRestrictions(Info), Info);
  Result := Events.OnHasRestrictions.Subscribe(Callback);
end;

function TToken.ObserveImpersonation;
var
  Info: TSecurityImpersonationLevel;
begin
  Callback(QueryImpersonation(Info), Info);
  Result := Events.OnImpersonation.Subscribe(Callback);
end;

function TToken.ObserveIntegrity;
var
  Info: TGroup;
begin
  Callback(QueryIntegrity(Info), Info);
  Result := Events.OnIntegrity.Subscribe(Callback);
end;

function TToken.ObserveIsAppContainer;
var
  Info: LongBool;
begin
  Callback(QueryIsAppContainer(Info), Info);
  Result := Events.OnIsAppContainer.Subscribe(Callback);
end;

function TToken.ObserveIsAppSilo;
var
  Info: LongBool;
begin
  Callback(QueryIsAppSilo(Info), Info);
  Result := Events.OnIsAppSilo.Subscribe(Callback);
end;

function TToken.ObserveIsLPAC;
var
  Info: Boolean;
begin
  Callback(QueryIsLPAC(Info), Info);
  Result := Events.OnIsLPAC.Subscribe(Callback);
end;

function TToken.ObserveIsRestricted;
var
  Info: LongBool;
begin
  Callback(QueryIsRestricted(Info), Info);
  Result := Events.OnIsRestricted.Subscribe(Callback);
end;

function TToken.ObserveIsSandboxed;
var
  Info: LongBool;
begin
  Callback(QueryIsSandboxed(Info), Info);
  Result := Events.OnIsSandboxed.Subscribe(Callback);
end;

function TToken.ObserveKernelAddress;
var
  Info: Pointer;
begin
  Callback(QueryKernelAddress(Info), Info);
  Result := Events.OnKernelAddress.Subscribe(Callback);
end;

function TToken.ObserveLogonInfo;
var
  Info: ILogonSession;
begin
  Callback(QueryLogonInfo(Info), Info);
  Result := Events.OnLogonInfo.Subscribe(Callback);
end;

function TToken.ObserveLogonSids;
var
  Info: TArray<TGroup>;
begin
  Callback(QueryLogonSids(Info), Info);
  Result := Events.OnLogonSids.Subscribe(Callback);
end;

function TToken.ObserveMandatoryPolicy;
var
  Info: TTokenMandatoryPolicy;
begin
  Callback(QueryMandatoryPolicy(Info), Info);
  Result := Events.OnMandatoryPolicy.Subscribe(Callback);
end;

function TToken.ObserveOrigin;
var
  Info: TLogonId;
begin
  Callback(QueryOrigin(Info), Info);
  Result := Events.OnOrigin.Subscribe(Callback);
end;

function TToken.ObserveOwner;
var
  Info: ISid;
begin
  Callback(QueryOwner(Info), Info);
  Result := Events.OnOwner.Subscribe(Callback);
end;

function TToken.ObservePackageClaims;
var
  Info: TPsPkgClaim;
begin
  Callback(QueryPackageClaims(Info), Info);
  Result := Events.OnPackageClaims.Subscribe(Callback);
end;

function TToken.ObservePrimaryGroup;
var
  Info: ISid;
begin
  Callback(QueryPrimaryGroup(Info), Info);
  Result := Events.OnPrimaryGroup.Subscribe(Callback);
end;

function TToken.ObservePrivateNamespace;
var
  Info: LongBool;
begin
  Callback(QueryPrivateNamespace(Info), Info);
  Result := Events.OnPrivateNamespace.Subscribe(Callback);
end;

function TToken.ObservePrivileges;
var
  Info: TArray<TPrivilege>;
begin
  Callback(QueryPrivileges(Info), Info);
  Result := Events.OnPrivileges.Subscribe(Callback);
end;

function TToken.ObserveRestrictedDeviceClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Callback(QueryRestrictedDeviceClaims(Info), Info);
  Result := Events.OnRestrictedDeviceClaims.Subscribe(Callback);
end;

function TToken.ObserveRestrictedDeviceGroups;
var
  Info: TArray<TGroup>;
begin
  Callback(QueryRestrictedDeviceGroups(Info), Info);
  Result := Events.OnRestrictedDeviceGroups.Subscribe(Callback);
end;

function TToken.ObserveRestrictedSids;
var
  Info: TArray<TGroup>;
begin
  Callback(QueryRestrictedSids(Info), Info);
  Result := Events.OnRestrictedSids.Subscribe(Callback);
end;

function TToken.ObserveRestrictedUserClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Callback(QueryRestrictedUserClaims(Info), Info);
  Result := Events.OnRestrictedUserClaims.Subscribe(Callback);
end;

function TToken.ObserveSandboxInert;
var
  Info: LongBool;
begin
  Callback(QuerySandboxInert(Info), Info);
  Result := Events.OnSandboxInert.Subscribe(Callback);
end;

function TToken.ObserveSecurityAttributes;
var
  Info: TArray<TSecurityAttribute>;
begin
  Callback(QuerySecurityAttributes(Info), Info);
  Result := Events.OnSecurityAttributes.Subscribe(Callback);
end;

function TToken.ObserveSessionId;
var
  Info: TSessionId;
begin
  Callback(QuerySessionId(Info), Info);
  Result := Events.OnSessionId.Subscribe(Callback);
end;

function TToken.ObserveSessionInfo;
var
  Info: TWinStationInformation;
begin
  Callback(QuerySessionInfo(Info), Info);
  Result := Events.OnSessionInfo.Subscribe(Callback);
end;

function TToken.ObserveSingletonAttributes;
var
  Info: TArray<TSecurityAttribute>;
begin
  Callback(QuerySingletonAttributes(Info), Info);
  Result := Events.OnSingletonAttributes.Subscribe(Callback);
end;

function TToken.ObserveSource;
var
  Info: TTokenSource;
begin
  Callback(QuerySource(Info), Info);
  Result := Events.OnSource.Subscribe(Callback);
end;

function TToken.ObserveStatistics;
var
  Info: TTokenStatistics;
begin
  Callback(QueryStatistics(Info), Info);
  Result := Events.OnStatistics.Subscribe(Callback);
end;

function TToken.ObserveString;
begin
  Callback(InfoClass, QueryString(InfoClass, ForceRefresh));

  if (InfoClass >= Low(TTokenPerHandleStringClass)) and
    (InfoClass <= High(TTokenPerHandleStringClass)) then
    // Subscribe a per-handle event
    Result := FPerHandleEvents[InfoClass].Subscribe(Callback)
  else
    // Subscribe a per-kernel-object events
    Result := Events.OnStringChange[InfoClass].Subscribe(Callback);
end;

function TToken.ObserveTrustLevel;
var
  Info: ISid;
begin
  Callback(QueryTrustLevel(Info), Info);
  Result := Events.OnTrustLevel.Subscribe(Callback);
end;

function TToken.ObserveType;
var
  Info: TTokenType;
begin
  Callback(QueryType(Info), Info);
  Result := Events.OnType.Subscribe(Callback);
end;

function TToken.ObserveUIAccess;
var
  Info: LongBool;
begin
  Callback(QueryUIAccess(Info), Info);
  Result := Events.OnUIAccess.Subscribe(Callback);
end;

function TToken.ObserveUser;
var
  Info: TGroup;
begin
  Callback(QueryUser(Info), Info);
  Result := Events.OnUser.Subscribe(Callback);
end;

function TToken.ObserveUserClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Callback(QueryUserClaims(Info), Info);
  Result := Events.OnUserClaims.Subscribe(Callback);
end;

function TToken.ObserveVirtualizationAllowed;
var
  Info: LongBool;
begin
  Callback(QueryVirtualizationAllowed(Info), Info);
  Result := Events.OnVirtualizationAllowed.Subscribe(Callback);
end;

function TToken.ObserveVirtualizationEnabled;
var
  Info: LongBool;
begin
  Callback(QueryVirtualizationEnabled(Info), Info);
  Result := Events.OnVirtualizationEnabled.Subscribe(Callback);
end;

function TToken.QueryAppContainerInfo;
var
  User: TGroup;
  Package: ISid;
begin
  // App container profiles are per-user
  Result := QueryUser(User);

  // Determine package SID
  if Result.IsSuccess then
    Result := QueryAppContainerSid(Package);

  if Result.IsSuccess and not Assigned(Package) then
  begin
    Result.Location := 'TToken.QueryAppContainerInfo';
    Result.Status := STATUS_NO_APPLICATION_PACKAGE;
  end;

  // Read profile info from the registry
  if Result.IsSuccess then
    Result := RtlxQueryAppContainer(AppContainer, Package, User.Sid);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsAppContainerName] := AppContainer.FullMoniker;
    Events.StringCache[tsAppContainerDisplayName] := AppContainer.DisplayName;
  end;

  Events.OnAppContainerInfo.Notify(Result, AppContainer);
end;

function TToken.QueryAppContainerNumber;
begin
  Result := NtxToken.Query(hxToken, TokenAppContainerNumber, Number);

  if Result.IsSuccess then
    Events.StringCache[tsAppContainerNumber] := UiLibUIntToDec(Number);

  Events.OnAppContainerNumber.Notify(Result, Number);
end;

function TToken.QueryAppContainerSid;
begin
  Result := NtxQuerySidToken(hxToken, TokenAppContainerSid, Package);

  if Result.IsSuccess and not Assigned(Package) then
  begin
    Events.StringCache[tsAppContainerName] := '(None)';
    Events.StringCache[tsAppContainerDisplayName] := '(None)';
  end;

  Events.OnAppContainerSid.Notify(Result, Package);
end;

function TToken.QueryAuditPolicy;
begin
  Result := NtxQueryToken(hxToken, TokenAuditPolicy, IMemory(AuditPolicy));
  Events.OnAuditPolicy.Notify(Result, AuditPolicy);
end;

function TToken.QueryBasicInfo;
begin
  Result := NtxObject.Query(hxToken, ObjectBasicInformation, Info);

  if Result.IsSuccess then
  begin
    PerHandleString[tsAccess] := TType.Represent<TTokenAccessMask>(
      Info.GrantedAccess).Text;
    PerHandleString[tsAccessNumeric] := UiLibUIntToHex(Info.GrantedAccess,
      NUMERIC_WIDTH_ROUND_TO_BYTE);
    Events.StringCache[tsHandleCount] := UiLibUIntToDec(
      Info.HandleCount);
    Events.StringCache[tsPagedPoolCharge] := BytesToString(
      Info.PagedPoolCharge);
    Events.StringCache[tsNonPagedPoolCharge] := BytesToString(
      Info.NonPagedPoolCharge);
  end;

  Events.OnBasicInfo.Notify(Result, Info);
end;

function TToken.QueryBnoIsolation;
begin
  Result := NtxQueryBnoIsolationToken(hxToken, Isolation);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsBnoIsolation] := BooleanToString(
      Isolation.Enabled, bkEnabledDisabled);

    if Isolation.Enabled then
      Events.StringCache[tsBnoPrefix] := Isolation.Prefix
    else
      Events.StringCache[tsBnoPrefix] := '(None)';
  end;

  Events.OnBnoIsolation.Notify(Result, Isolation);
end;

function TToken.QueryCapabilities;
begin
  Result := NtxQueryGroupsToken(hxToken, TokenCapabilities, Capabilities);

  if Result.IsSuccess then
    Events.StringCache[tsCapabilities] := UiLibUIntToDec(Length(Capabilities));

  Events.OnCapabilities.Notify(Result, Capabilities);
end;

function TToken.QueryCreatorPID;
begin
  if Events.CreatorPIDIsKnown and not ForceRefresh then
  begin
    Result.Status := STATUS_SUCCESS;
    CreatorPID := Events.CreatorPID;
    Exit;
  end;

  // Make sure kernel address is know since we use it to find the object in
  // the snapshot
  Result := RefreshKernelAddress(ForceRefresh);

  if not Result.IsSuccess then
    Exit;

  // Initiating object snapshotting will indirectly invoke our callback that
  // will cache the result and notify our subscribers
  Result := TGlobalEvents.RefreshObjects;

  if Result.IsSuccess then
    CreatorPID := Events.CreatorPID;
end;

function TToken.QueryDefaultDacl;
begin
  Result := NtxQueryDefaultDaclToken(hxToken, DefaultDacl);
  Events.OnDefaultDacl.Notify(Result, DefaultDacl);
end;

function TToken.QueryDeviceClaims;
begin
  Result := NtxQueryClaimsToken(hxToken, TokenDeviceClaimAttributes, Claims);

  if Result.IsSuccess then
    Events.StringCache[tsDeviceClaims] := UiLibUIntToDec(Length(Claims));

  Events.OnDeviceClaims.Notify(Result, Claims);
end;

function TToken.QueryDeviceGroups;
begin
  Result := NtxQueryGroupsToken(hxToken, TokenDeviceGroups, Groups);

  if Result.IsSuccess then
    Events.StringCache[tsDeviceGroups] := UiLibUIntToDec(Length(Groups));

  Events.OnDeviceGroups.Notify(Result, Groups);
end;

function TToken.QueryElevation;
begin
  Result := NtxToken.Query(hxToken, TokenElevation, Elevation.Elevated);

  if Result.IsSuccess then
    Result := NtxToken.Query(hxToken, TokenElevationType,
      Elevation.ElevationType);

  if Result.IsSuccess then
    Events.StringCache[tsElevation] := Elevation.ToString;

  Events.OnElevation.Notify(Result, Elevation);
end;

function TToken.QueryFlags;
begin
  Result := NtxQueryFlagsToken(hxToken, Flags);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsFlags] := TType.Represent(Flags).Text;

    if BitTest(Flags and TOKEN_WRITE_RESTRICTED) then
      Events.StringCache[tsRestricted] := 'Write-only'
    else if BitTest(Flags and TOKEN_IS_RESTRICTED) then
      Events.StringCache[tsRestricted] := 'Fully'
    else
      Events.StringCache[tsRestricted] := 'No';

    Events.StringCache[tsSessionReference] := BooleanToString(
      not BitTest(Flags and TOKEN_SESSION_NOT_REFERENCED), bkYesNo);

    Events.StringCache[tsSandBoxInert] := BooleanToString(
      BitTest(Flags and TOKEN_SANDBOX_INERT), bkEnabledDisabled);

    if BitTest(Flags and TOKEN_VIRTUALIZE_ALLOWED) then
      Events.StringCache[tsVirtualization] := BooleanToString(
        BitTest(Flags and TOKEN_VIRTUALIZE_ENABLED), bkEnabledDisabled)
    else if not BitTest(Flags and TOKEN_VIRTUALIZE_ENABLED) then
      Events.StringCache[tsVirtualization] := 'Not allowed'
    else
      Events.StringCache[tsVirtualization] := 'Disallowed & Enabled';

    Events.StringCache[tsFiltered] := BooleanToString(
      BitTest(Flags and TOKEN_IS_FILTERED), bkYesNo);

    Events.StringCache[tsUIAccess] := BooleanToString(
      BitTest(Flags and TOKEN_UIACCESS), bkEnabledDisabled);

    Events.StringCache[tsLowBox] := BooleanToString(
      BitTest(Flags and TOKEN_LOWBOX), bkYesNo);

    Events.StringCache[tsPrivateNamespace] := BooleanToString(
      BitTest(Flags and TOKEN_PRIVATE_NAMESPACE), bkYesNo);

    if BitTest(Flags and TOKEN_NO_CHILD_PROCESS) then
      Events.StringCache[tsChildFlags] := 'Blocked'
    else if BitTest(Flags and TOKEN_NO_CHILD_PROCESS_UNLESS_SECURE) then
      Events.StringCache[tsChildFlags] := 'Blocked Unless Secure'
    else if BitTest(Flags and TOKEN_NO_CHILD_PROCESS) then
      Events.StringCache[tsChildFlags] := 'Audit'
    else
      Events.StringCache[tsChildFlags] := 'Allowed';

    Events.StringCache[tsPermissiveLearning] := BooleanToString(
      BitTest(Flags and TOKEN_PERMISSIVE_LEARNING_MODE), bkEnabledDisabled);

    if BitTest(Flags and TOKEN_ENFORCE_REDIRECTION_TRUST) then
      Events.StringCache[tsRedirectionTrust] := 'Enforced'
    else if BitTest(Flags and TOKEN_AUDIT_REDIRECTION_TRUST) then
      Events.StringCache[tsRedirectionTrust] := 'Audit'
    else
      Events.StringCache[tsRedirectionTrust] := 'Disabled';
  end;

  Events.OnFlags.Notify(Result, Flags);
end;

function TToken.QueryGroups;
var
  i: Integer;
  EnabledCount: Cardinal;
begin
  Result := NtxQueryGroupsToken(hxToken, TokenGroups, Groups);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsGroups] := UiLibUIntToDec(Length(Groups));

    EnabledCount := 0;
    for i := 0 to High(Groups) do
      if BitTest(Groups[i].Attributes and SE_GROUP_ENABLED) then
        Inc(EnabledCount);

    Events.StringCache[tsGroupsEnabled] := Format('%d/%d',
      [EnabledCount, Length(Groups)]);
  end;

  Events.OnGroups.Notify(Result, Groups);
end;

function TToken.QueryHandles;
begin
  // Since we are also subscribed to this event, our callback will notify
  // relevant per-token subscribers
  Result := TGlobalEvents.QueryHandles(Handles);

  if Result.IsSuccess then
    Result := RtlxFilterHandlesByHandle(Handles, hxToken);
end;

function TToken.QueryHasRestrictions;
begin
  Result := NtxToken.Query(hxToken, TokenHasRestrictions, HasRestrictions);
  Events.OnHasRestrictions.Notify(Result, HasRestrictions);
end;

function TToken.QueryImpersonation;
begin
  Result := NtxToken.Query(hxToken, TokenImpersonationLevel, Level);
  Events.OnImpersonation.Notify(Result, Level);
end;

function TToken.QueryIntegrity;
begin
  Result := NtxQueryGroupToken(hxToken, TokenIntegrityLevel, Integrity);

  if Result.IsSuccess then
    Events.StringCache[tsIntegrity] := TType.Represent<TIntegrityRid>(
      RtlxRidSid(Integrity.Sid, SECURITY_MANDATORY_UNTRUSTED_RID)).Text;

  Events.OnIntegrity.Notify(Result, Integrity);
end;

function TToken.QueryIsAppContainer;
begin
  Result := NtxToken.Query(hxToken, TokenIsAppContainer, IsAppContainer);
  Events.OnIsAppContainer.Notify(Result, IsAppContainer);
end;

function TToken.QueryIsAppSilo;
begin
  Result := NtxToken.Query(hxToken, TokenIsAppSilo, IsAppSilo);

  if Result.IsSuccess then
    Events.StringCache[tsIsAppSilo] := BooleanToString(IsAppSilo, bkYesNo);

  Events.OnIsAppSilo.Notify(Result, IsAppSilo);
end;

function TToken.QueryIsLPAC;
begin
  Result := NtxQueryLpacToken(hxToken, IsLPAC);

  if Result.IsSuccess then
    Events.StringCache[tsLPAC] := BooleanToString(IsLPAC, bkYesNo);

  Events.OnIsLPAC.Notify(Result, IsLPAC);
end;

function TToken.QueryIsRestricted;
begin
  Result := NtxToken.Query(hxToken, TokenIsRestricted, IsRestricted);

  if Result.IsSuccess then
    Events.StringCache[tsIsRestricted] := BooleanToString(IsRestricted, bkYesNo);

  Events.OnIsRestricted.Notify(Result, IsRestricted);
end;

function TToken.QueryIsSandboxed;
begin
  Result := NtxToken.Query(hxToken, TokenIsSandboxed, IsSandboxed);

  if Result.IsSuccess then
    Events.StringCache[tsIsSandboxed] := BooleanToString(IsSandboxed, bkYesNo);

  Events.OnIsSandboxed.Notify(Result, IsSandboxed);
end;

function TToken.QueryKernelAddress;
var
  Handles: TArray<TSystemHandleEntry>;
begin
  if Assigned(FKernelObjectAddress) and not ForceRefresh then
  begin
    Result.Status := STATUS_SUCCESS;
    ObjectAddress := FKernelObjectAddress;
    Exit;
  end;

  // Invoke handle snapshotting to update the cached address value;
  // This will also indirectly trigger our callback that will notify
  // our subscribers (including kernel address observers)
  Result := QueryHandles(Handles);

  // Kernel address leak prevention can blocks us
  if Result.IsSuccess and not Assigned(FKernelObjectAddress) then
  begin
    Result.Location := 'TToken.QueryKernelAddress';
    Result.LastCall.ExpectedPrivilege := SE_DEBUG_PRIVILEGE;
    Result.Status := STATUS_PRIVILEGE_NOT_HELD;
  end;

  if Result.IsSuccess then
    ObjectAddress := FKernelObjectAddress;
end;

function TToken.QueryLinkedToken;
var
  hToken: THandle;
begin
  Result := NtxToken.Query(hxToken, TokenLinkedToken, hToken);

  if Result.IsSuccess then
    Token := CaptureTokenHandle(Auto.CaptureHandle(hToken),
      'Linked token for ' + GetCaption);
end;

function TToken.QueryLogonInfo;
var
  Statistics: TTokenStatistics;
begin
  Result := QueryStatistics(Statistics);

  if Result.IsSuccess then
    Result := LsaxQueryLogonSession(Statistics.AuthenticationId, Info);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsLogonAuthPackage] := RtlxStringOrDefault(
      Info.Data.AuthenticationPackage.ToString, '(None)');
    Events.StringCache[tsLogonType] := TType.Represent(Info.Data.LogonType).Text;
    Events.StringCache[tsLogonTime] := TType.Represent(Info.Data.LogonTime).Text;
  end;

  Events.OnLogonInfo.Notify(Result, Info);
end;

function TToken.QueryLogonSids;
begin
  Result := NtxQueryGroupsToken(hxToken, TokenLogonSid, LogonSids);

  if Result.IsSuccess then
  begin
    if Length(LogonSids) > 0 then
      Events.StringCache[tsLogonSid] := LsaxSidToString(LogonSids[0].Sid)
    else
      Events.StringCache[tsLogonSid] := '(None)';
  end
  else if Result.Status = STATUS_NOT_FOUND then
    Events.StringCache[tsLogonSid] := '(Not found)';

  Events.OnLogonSids.Notify(Result, LogonSids);
end;

function TToken.QueryMandatoryPolicy;
begin
  Result := NtxToken.Query(hxToken, TokenMandatoryPolicy, Policy);

  if Result.IsSuccess then
    Events.StringCache[tsMandatoryPolicy] := TType.Represent(Policy).Text;

  Events.OnMandatoryPolicy.Notify(Result, Policy);
end;

function TToken.QueryOrigin;
begin
  Result := NtxToken.Query(hxToken, TokenOrigin, Origin);

  if Result.IsSuccess then
    Events.StringCache[tsOrigin] := UiLibUIntToHex(Origin);

  Events.OnOrigin.Notify(Result, Origin);
end;

function TToken.QueryOwner;
begin
  Result := NtxQuerySidToken(hxToken, TokenOwner, Owner);

  if Result.IsSuccess then
    Events.StringCache[tsOwner] := LsaxSidToString(Owner);

  Events.OnOwner.Notify(Result, Owner);
end;

function TToken.QueryPackageClaims;
begin
  Result := NtxQueryPackageClaimsToken(hxToken, PkgClaim);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsPackageFlags] := TType.Represent(PkgClaim.Flags).Text;
    Events.StringCache[tsPackageOrigin] := TType.Represent(PkgClaim.Origin).Text;
  end
  else if Result.Status = STATUS_NOT_FOUND then
  begin
    Events.StringCache[tsPackageFlags] := '(Not found)';
    Events.StringCache[tsPackageOrigin] := '(Not found)';
  end;

  Events.OnPackageClaims.Notify(Result, PkgClaim);
end;

function TToken.QueryPrimaryGroup;
begin
  Result := NtxQuerySidToken(hxToken, TokenPrimaryGroup, PrimaryGroup);

  if Result.IsSuccess then
    Events.StringCache[tsPrimaryGroup] := LsaxSidToString(PrimaryGroup);

  Events.OnPrimaryGroup.Notify(Result, PrimaryGroup);
end;

function TToken.QueryPrivateNamespace;
begin
  Result := NtxToken.Query(hxToken, TokenPrivateNameSpace, PrivateNamespace);

  if Result.IsSuccess then
    Events.StringCache[tsPrivateNamespace] := BooleanToString(PrivateNamespace,
      bkYesNo);

  Events.OnPrivateNamespace.Notify(Result, PrivateNamespace);
end;

function TToken.QueryPrivileges;
var
  i: Integer;
  EnabledCount: Cardinal;
begin
  Result := NtxQueryPrivilegesToken(hxToken, Privileges);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsPrivileges] := UiLibUIntToDec(Length(Privileges));

    EnabledCount := 0;
    for i := 0 to High(Privileges) do
      if BitTest(Privileges[i].Attributes and SE_PRIVILEGE_ENABLED) then
        Inc(EnabledCount);

    Events.StringCache[tsPrivilegesEnabled] := Format('%d/%d',
      [EnabledCount, Length(Privileges)]);
  end;

  Events.OnPrivileges.Notify(Result, Privileges);
end;

function TToken.QueryRestrictedDeviceClaims;
begin
  Result := NtxQueryClaimsToken(hxToken, TokenRestrictedUserClaimAttributes,
    Claims);

  if Result.IsSuccess then
    Events.StringCache[tsRestrictedDeviceClaims] := UiLibUIntToDec(
      Length(Claims));

  Events.OnRestrictedDeviceClaims.Notify(Result, Claims);
end;

function TToken.QueryRestrictedDeviceGroups;
begin
  Result := NtxQueryGroupsToken(hxToken, TokenRestrictedDeviceGroups, Groups);

  if Result.IsSuccess then
    Events.StringCache[tsRestrictedDeviceGroups] := UiLibUIntToDec(
      Length(Groups));

  Events.OnRestrictedDeviceGroups.Notify(Result, Groups);
end;

function TToken.QueryRestrictedSids;
begin
  Result := NtxQueryGroupsToken(hxToken, TokenRestrictedSids, Sids);

  if Result.IsSuccess then
    Events.StringCache[tsRestrictedSids] := UiLibUIntToDec(Length(Sids));

  Events.OnRestrictedSids.Notify(Result, Sids);
end;

function TToken.QueryRestrictedUserClaims;
begin
  Result := NtxQueryClaimsToken(hxToken, TokenRestrictedDeviceClaimAttributes,
    Claims);

  if Result.IsSuccess then
    Events.StringCache[tsRestrictedUserClaims] := UiLibUIntToDec(Length(Claims));

  Events.OnRestrictedUserClaims.Notify(Result, Claims);
end;

function TToken.QuerySandboxInert;
begin
  Result := NtxToken.Query(hxToken, TokenSandBoxInert, SandboxInert);

  if Result.IsSuccess then
    Events.StringCache[tsSandBoxInert] := BooleanToString(SandboxInert,
      bkEnabledDisabled);

  Events.OnSandboxInert.Notify(Result, SandboxInert);
end;

function TToken.QuerySecurityAttributes;
var
  Names: TArray<String>;
  i: Integer;
begin
  Result := NtxQueryAttributesToken(hxToken, TokenSecurityAttributes, Attributes);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsSecAttributes] := UiLibUIntToDec(Length(Attributes));

    if Length(Attributes) > 0 then
    begin
      SetLength(Names, Length(Attributes));

      for i := 0 to High(Attributes) do
        Names[i] := Attributes[i].Name;

      Events.StringCache[tsSecAttributesNames] := String.Join(', ', Names);
    end
    else
      Events.StringCache[tsSecAttributesNames] := '(None)';
  end;

  Events.OnSecurityAttributes.Notify(Result, Attributes);
end;

function TToken.QuerySessionId;
begin
  Result := NtxToken.Query(hxToken, TokenSessionId, SessionId);

  if Result.IsSuccess then
    Events.StringCache[tsSessionId] := UiLibUIntToDec(SessionId);

  Events.OnSessionId.Notify(Result, SessionId);
end;

function TToken.QuerySessionInfo;
var
  SessionId: TSessionId;
begin
  Result := QuerySessionId(SessionId);

  if Result.IsSuccess then
    Result := WsxWinStation.Query(SessionId, WinStationInformation, Info);

  if Result.IsSuccess then
    Events.StringCache[tsSessionInfo] := SessionInfoToString(Info);

  Events.OnSessionInfo.Notify(Result, Info);
end;

function TToken.QuerySingletonAttributes;
begin
  Result := NtxQueryAttributesToken(hxToken, TokenSingletonAttributes,
    Attributes);

  if Result.IsSuccess then
    Events.StringCache[tsSingletonAttributes] := UiLibUIntToDec(
      Length(Attributes));

  Events.OnSingletonAttributes.Notify(Result, Attributes);
end;

function TToken.QuerySource;
begin
  Result := NtxToken.Query(hxToken, TokenSource, Source);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsSourceName] := Source.Name;
    Events.StringCache[tsSourceId] := UiLibUIntToHex(Source.SourceIdentifier);
  end;

  Events.OnSource.Notify(Result, Source);
end;

function TToken.QueryStatistics;
begin
  Result := NtxToken.Query(hxToken, TokenStatistics, Statistics);

  if Result.IsSuccess then
  begin
    Events.StringCache[tsTokenId] := UiLibUIntToHex(Statistics.TokenId);
    Events.StringCache[tsLogonId] := UiLibUIntToHex(Statistics.AuthenticationId);
    Events.StringCache[tsExpires] := TType.Represent(
      Statistics.ExpirationTime).Text;

    if Statistics.TokenType = TokenPrimary then
      Events.StringCache[tsType] := 'Primary'
    else
      Events.StringCache[tsType] := TType.Represent(
        Statistics.ImpersonationLevel).Text;

    Events.StringCache[tsDynamicCharged] := BytesToString(
      Statistics.DynamicCharged);
    Events.StringCache[tsDynamicAvailable] := BytesToString(
      Statistics.DynamicAvailable);
    Events.StringCache[tsGroups] := UiLibUIntToDec(Statistics.GroupCount);
    Events.StringCache[tsPrivileges] := UiLibUIntToDec(Statistics.PrivilegeCount);
    Events.StringCache[tsModifiedId] := UiLibUIntToHex(Statistics.ModifiedId);
  end;

  Events.OnStatistics.Notify(Result, Statistics);
end;

function TToken.QueryString;
var
  Success: Boolean;
begin
  {$IFOPT R+}
  if (InfoClass < Low(TTokenStringClass)) or
    (InfoClass > High(TTokenStringClass)) then
    raise ERangeError.Create('Invalid token string class');
  {$ENDIF}

  case InfoClass of
    // Some per-handle properties don't require querying
    tsCaption, tsHandle, tsHandleDetailed:
      Exit(PerHandleString[InfoClass]);

    // Others can be refreshed if forced
    tsAccess, tsAccessNumeric:
      begin
        // Reset the cache on forced refresh without triggering the events
        if ForceRefresh then
          FPerHandleStrings[InfoClass] := '';

        if (FPerHandleStrings[InfoClass] <> '') or
          RefreshBasicInfo.IsSuccess then
          Exit(FPerHandleStrings[InfoClass]);
      end;
  else
    // Reset the cache on forced refresh without triggering the events
    if ForceRefresh then
      Events.FStrings[InfoClass] := '';

    // Try to retrieve cached per-object properties
    if Events.StringCache[InfoClass] <> '' then
      Exit(Events.StringCache[InfoClass]);
  end;

  Success := False;

  // Query the information and populate the cached strings
  case InfoClass of
    tsHandleCount,
    tsPagedPoolCharge,
    tsNonPagedPoolCharge:      Success := RefreshBasicInfo.IsSuccess;
    tsAddress:                 Success := RefreshKernelAddress(ForceRefresh).IsSuccess;
    tsCreator:                 Success := RefreshCreatorPID(ForceRefresh).IsSuccess;
    tsUser:                    Success := RefreshUser.IsSuccess;
    tsGroupsEnabled:           Success := RefreshGroups.IsSuccess;
    tsPrivilegesEnabled:       Success := RefreshPrivileges.IsSuccess;
    tsOwner:                   Success := RefreshOwner.IsSuccess;
    tsPrimaryGroup:            Success := RefreshPrimaryGroup.IsSuccess;
    tsSourceName,
    tsSourceId:                Success := RefreshSource.IsSuccess;
    tsGroups,
    tsPrivileges,
    tsType,
    tsTokenId,
    tsLogonId,
    tsModifiedId,
    tsExpires,
    tsDynamicCharged,
    tsDynamicAvailable:        Success := RefreshStatistics.IsSuccess;
    tsLogonAuthPackage,
    tsLogonType,
    tsLogonTime:               Success := RefreshLogonInfo.IsSuccess;
    tsRestrictedSids:          Success := RefreshRestrictedSids.IsSuccess;
    tsSessionId:               Success := RefreshSessionId.IsSuccess;
    tsSessionInfo:             Success := RefreshSessionInfo.IsSuccess;
    tsOrigin:                  Success := RefreshOrigin.IsSuccess;
    tsElevation:               Success := RefreshElevation.IsSuccess;
    tsSandBoxInert,
    tsFlags,
    tsRestricted,
    tsSessionReference,
    tsVirtualization,
    tsFiltered,
    tsUIAccess,
    tsLowBox,
    tsPrivateNamespace,
    tsChildFlags,
    tsPermissiveLearning,
    tsRedirectionTrust:        Success := RefreshFlags.IsSuccess;
    tsIntegrity:               Success := RefreshIntegrity.IsSuccess;
    tsMandatoryPolicy:         Success := RefreshMandatoryPolicy.IsSuccess;
    tsLogonSid:                Success := RefreshLogonSids.IsSuccess;
    tsCapabilities:            Success := RefreshCapabilities.IsSuccess;
    tsAppContainerNumber:      Success := RefreshAppContainerNumber.IsSuccess;
    tsAppContainerName,
    tsAppContainerDisplayName: Success := RefreshAppContainerInfo.IsSuccess;
    tsUserClaims:              Success := RefreshUserClaims.IsSuccess;
    tsDeviceClaims:            Success := RefreshDeviceClaims.IsSuccess;
    tsRestrictedUserClaims:    Success := RefreshRestrictedUserClaims.IsSuccess;
    tsRestrictedDeviceClaims:  Success := RefreshRestrictedDeviceClaims.IsSuccess;
    tsDeviceGroups:            Success := RefreshDeviceGroups.IsSuccess;
    tsRestrictedDeviceGroups:  Success := RefreshRestrictedDeviceGroups.IsSuccess;
    tsSecAttributes,
    tsSecAttributesNames:      Success := RefreshSecurityAttributes.IsSuccess;
    tsLPAC:                    Success := RefreshIsLPAC.IsSuccess;
    tsPackageFlags,
    tsPackageOrigin:           Success := RefreshPackageClaims.IsSuccess;
    tsIsRestricted:            Success := RefreshIsRestricted.IsSuccess;
    tsTrustLevel:              Success := RefreshTrustLevel.IsSuccess;
    tsSingletonAttributes:     Success := RefreshSingletonAttributes.IsSuccess;
    tsBnoIsolation,
    tsBnoPrefix:               Success := RefreshBnoIsolation.IsSuccess;
    tsIsSandboxed:             Success := RefreshIsSandboxed.IsSuccess;
    tsIsAppSilo:               Success := RefreshIsAppSilo.IsSuccess;
  end;

  if Success then
    Result := Events.StringCache[InfoClass]
  else
    Result := 'Unknown';
end;

function TToken.QueryTrustLevel;
begin
  Result := NtxQuerySidToken(hxToken, TokenProcessTrustLevel, TrustLevel);

  if Result.IsSuccess then
    Events.StringCache[tsTrustLevel] := TrustLevelToString(TrustLevel);

  Events.OnTrustLevel.Notify(Result, TrustLevel);
end;

function TToken.QueryType;
begin
  Result := NtxToken.Query(hxToken, TokenType, &Type);
  Events.OnType.Notify(Result, &Type);
end;

function TToken.QueryUIAccess;
begin
  Result := NtxToken.Query(hxToken, TokenUIAccess, UIAccess);

  if Result.IsSuccess then
    Events.StringCache[tsUIAccess] := BooleanToString(UIAccess,
      bkEnabledDisabled);

  Events.OnUIAccess.Notify(Result, UIAccess);
end;

function TToken.QueryUser;
begin
  Result := NtxQueryGroupToken(hxToken, TokenUser, User);

  if Result.IsSuccess then
    Events.StringCache[tsUser] := LsaxSidToString(User.Sid);

  Events.OnUser.Notify(Result, User);
end;

function TToken.QueryUserClaims;
begin
  Result := NtxQueryClaimsToken(hxToken, TokenUserClaimAttributes, Claims);

  if Result.IsSuccess then
    Events.StringCache[tsUserClaims] := UiLibUIntToDec(Length(Claims));

  Events.OnUserClaims.Notify(Result, Claims);
end;

function TToken.QueryVirtualizationAllowed;
begin
  Result := NtxToken.Query(hxToken, TokenVirtualizationAllowed, Allowed);
  Events.OnVirtualizationAllowed.Notify(Result, Allowed);
end;

function TToken.QueryVirtualizationEnabled;
begin
  Result := NtxToken.Query(hxToken, TokenVirtualizationEnabled, Enabled);
  Events.OnVirtualizationEnabled.Notify(Result, Enabled);
end;

function TToken.RefreshAppContainerInfo;
var
  Info: TRtlxAppContainerInfo;
begin
  Result := QueryAppContainerInfo(Info);
end;

function TToken.RefreshAppContainerNumber;
var
  Info: Cardinal;
begin
  Result := QueryAppContainerNumber(Info);
end;

function TToken.RefreshAppContainerSid;
var
  Info: ISid;
begin
  Result := QueryAppContainerSid(Info);
end;

function TToken.RefreshAuditPolicy;
var
  Info: ITokenAuditPolicy;
begin
  Result := QueryAuditPolicy(Info);
end;

function TToken.RefreshBasicInfo;
var
  Info: TObjectBasicInformation;
begin
  Result := QueryBasicInfo(Info);
end;

function TToken.RefreshBnoIsolation;
var
  Info: TBnoIsolation;
begin
  Result := QueryBnoIsolation(Info);
end;

function TToken.RefreshCapabilities;
var
  Info: TArray<TGroup>;
begin
  Result := QueryCapabilities(Info);
end;

function TToken.RefreshCreatorPID;
var
  Info: TProcessId;
begin
  Result := QueryCreatorPID(Info, ForceRefresh);
end;

function TToken.RefreshDefaultDacl;
var
  Info: IAcl;
begin
  Result := QueryDefaultDacl(Info);
end;

function TToken.RefreshDeviceClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Result := QueryDeviceClaims(Info);
end;

function TToken.RefreshDeviceGroups;
var
  Info: TArray<TGroup>;
begin
  Result := QueryDeviceGroups(Info);
end;

function TToken.RefreshElevation;
var
  Info: TTokenElevationInfo;
begin
  Result := QueryElevation(Info);
end;

function TToken.RefreshFlags;
var
  Info: TTokenFlags;
begin
  Result := QueryFlags(Info);
end;

function TToken.RefreshGroups;
var
  Info: TArray<TGroup>;
begin
  Result := QueryGroups(Info);
end;

function TToken.RefreshHandles;
begin
  // Our callback for the global event will invoke local subscribers
  Result := TGlobalEvents.RefreshHandles;
end;

function TToken.RefreshHasRestrictions;
var
  Info: LongBool;
begin
  Result := QueryHasRestrictions(Info);
end;

function TToken.RefreshImpersonation;
var
  Info: TSecurityImpersonationLevel;
begin
  Result := QueryImpersonation(Info);
end;

function TToken.RefreshIntegrity;
var
  Info: TGroup;
begin
  Result := QueryIntegrity(Info);
end;

function TToken.RefreshIsAppContainer;
var
  Info: LongBool;
begin
  Result := QueryIsAppContainer(Info);
end;

function TToken.RefreshIsAppSilo: TNtxStatus;
var
  Info: LongBool;
begin
  Result := QueryIsAppSilo(Info);
end;

function TToken.RefreshIsLPAC;
var
  Info: Boolean;
begin
  Result := QueryIsLPAC(Info);
end;

function TToken.RefreshIsRestricted;
var
  Info: LongBool;
begin
  Result := QueryIsRestricted(Info);
end;

function TToken.RefreshIsSandboxed;
var
  Info: LongBool;
begin
  Result := QueryIsSandboxed(Info);
end;

function TToken.RefreshKernelAddress;
begin
  if ForceRefresh or not Assigned(FKernelObjectAddress) then
    Result := TGlobalEvents.RefreshHandles;
end;

function TToken.RefreshLogonInfo;
var
  Info: ILogonSession;
begin
  Result := QueryLogonInfo(Info);
end;

function TToken.RefreshLogonSids;
var
  Info: TArray<TGroup>;
begin
  Result := QueryLogonSids(Info);
end;

function TToken.RefreshMandatoryPolicy;
var
  Info: TTokenMandatoryPolicy;
begin
  Result := QueryMandatoryPolicy(Info);
end;

function TToken.RefreshOrigin;
var
  Info: TLogonId;
begin
  Result := QueryOrigin(Info);
end;

function TToken.RefreshOwner;
var
  Info: ISid;
begin
  Result := QueryOwner(Info);
end;

function TToken.RefreshPackageClaims;
var
  Info: TPsPkgClaim;
begin
  Result := QueryPackageClaims(Info);
end;

function TToken.RefreshPrimaryGroup;
var
  Info: ISid;
begin
  Result := QueryPrimaryGroup(Info);
end;

function TToken.RefreshPrivateNamespace;
var
  Info: LongBool;
begin
  Result := QueryPrivateNamespace(Info);
end;

function TToken.RefreshPrivileges;
var
  Info: TArray<TPrivilege>;
begin
  Result := QueryPrivileges(Info);
end;

function TToken.RefreshRestrictedDeviceClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Result := QueryRestrictedDeviceClaims(Info);
end;

function TToken.RefreshRestrictedDeviceGroups;
var
  Info: TArray<TGroup>;
begin
  Result := QueryRestrictedDeviceGroups(Info);
end;

function TToken.RefreshRestrictedSids;
var
  Info: TArray<TGroup>;
begin
  Result := QueryRestrictedSids(Info);
end;

function TToken.RefreshRestrictedUserClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Result := QueryRestrictedUserClaims(Info);
end;

function TToken.RefreshSandboxInert;
var
  Info: LongBool;
begin
  Result := QuerySandboxInert(Info);
end;

function TToken.RefreshSecurityAttributes;
var
  Info: TArray<TSecurityAttribute>;
begin
  Result := QuerySecurityAttributes(Info);
end;

function TToken.RefreshSessionId;
var
  Info: TSessionId;
begin
  Result := QuerySessionId(Info);
end;

function TToken.RefreshSessionInfo;
var
  Info: TWinStationInformation;
begin
  Result := QuerySessionInfo(Info);
end;

function TToken.RefreshSingletonAttributes;
var
  Info: TArray<TSecurityAttribute>;
begin
  Result := QuerySingletonAttributes(Info);
end;

function TToken.RefreshSource;
var
  Info: TTokenSource;
begin
  Result := QuerySource(Info);
end;

function TToken.RefreshStatistics;
var
  Info: TTokenStatistics;
begin
  Result := QueryStatistics(Info);
end;

procedure TToken.RefreshString;
begin
  // Skip values that don't change externally
  if not (InfoClass in [tsCaption, tsHandle, tsHandleDetailed]) then
    QueryString(InfoClass, True);
end;

function TToken.RefreshTrustLevel;
var
  Info: ISid;
begin
  Result := QueryTrustLevel(Info);
end;

function TToken.RefreshType;
var
  Info: TTokenType;
begin
  Result := QueryType(Info);
end;

function TToken.RefreshUIAccess;
var
  Info: LongBool;
begin
  Result := QueryUIAccess(Info);
end;

function TToken.RefreshUser;
var
  Info: TGroup;
begin
  Result := QueryUser(Info);
end;

function TToken.RefreshUserClaims;
var
  Info: TArray<TSecurityAttribute>;
begin
  Result := QueryUserClaims(Info);
end;

function TToken.RefreshVirtualizationAllowed;
var
  Info: LongBool;
begin
  Result := QueryVirtualizationAllowed(Info);
end;

function TToken.RefreshVirtualizationEnabled;
var
  Info: LongBool;
begin
  Result := QueryVirtualizationEnabled(Info);
end;

function TToken.SetAuditPolicy;
begin
  Result := NtxSetToken(hxToken, TokenAuditPolicy, AuditPolicy.Data,
    AuditPolicy.Size);
  SmartRefresh;
end;

procedure TToken.SetCaption;
begin
  SetPerHandleString(tsCaption, Value);
end;

function TToken.SetChildProcessFlags;
begin
  Result := NtxToken.Set(hxToken, TokenChildProcessFlags, Blocked);
  SmartRefresh;
end;

function TToken.SetDefaultDacl;
begin
  Result := NtxSetDefaultDaclToken(hxToken, DefaultDacl);
  SmartRefresh;
end;

function TToken.SetIntegrity;
begin
  Result := NtxSetIntegrityToken(hxToken, Level);
  SmartRefresh;
end;

function TToken.SetLinkedToken;
var
  hToken: THandle;
begin
  hToken := Token.Handle.Handle;
  Result := NtxToken.Set(hxToken, TokenLinkedToken, hToken);

  // Linking logon session changes elevation type for tokens from both sessions
  TGlobalEvents.OnLinkLogonSessions.Invoke;
end;

function TToken.SetMandatoryPolicy;
begin
  Result := NtxToken.Set(hxToken, TokenMandatoryPolicy, Policy);
  SmartRefresh;
end;

function TToken.SetOrigin;
begin
  Result := NtxToken.Set(hxToken, TokenOrigin, Origin);
  SmartRefresh;
end;

function TToken.SetOwner;
var
  Buffer: TTokenSidInformation;
begin
  Buffer.Sid := Owner.Data;
  Result := NtxToken.Set(hxToken, TokenOwner, Buffer);
  SmartRefresh;
end;

procedure TToken.SetPerHandleString;
begin
  {$IFOPT R+}
  if (InfoClass < Low(TTokenPerHandleStringClass)) or
    (InfoClass > High(TTokenPerHandleStringClass)) then
    raise ERangeError.Create('Invalid per-handle token string class');
  {$ENDIF}

  if FPerHandleStrings[InfoClass] <> Value then
  begin
    FPerHandleStrings[InfoClass] := Value;
    FPerHandleEvents[InfoClass].Invoke(InfoClass, Value);
  end;
end;

function TToken.SetPrimaryGroup;
var
  Buffer: TTokenSidInformation;
begin
  Buffer.Sid := PrimaryGroup.Data;
  Result := NtxToken.Set(hxToken, TokenPrimaryGroup, Buffer);
  SmartRefresh;
end;

function TToken.SetPrivateNamespace;
begin
  Result := NtxToken.Set(hxToken, TokenPrivateNameSpace, PrivateNamespace);
  SmartRefresh;
end;

function TToken.SetSessionId;
begin
  Result := NtxToken.Set(hxToken, TokenSessionId, SessionId);
  SmartRefresh;
end;

function TToken.SetSessionReference;
begin
  Result := NtxToken.Set(hxToken, TokenSessionReference, Reference);
  SmartRefresh;
end;

function TToken.SetUIAccess;
begin
  Result := NtxToken.Set(hxToken, TokenUIAccess, UIAccess);
  SmartRefresh;
end;

function TToken.SetVirtualizationAllowed;
begin
  Result := NtxToken.Set(hxToken, TokenVirtualizationAllowed, Allowed);
  SmartRefresh;
end;

function TToken.SetVirtualizationEnabled;
begin
  Result := NtxToken.Set(hxToken, TokenVirtualizationEnabled, Enabled);
  SmartRefresh;
end;

procedure TToken.SmartRefresh;
begin
  if Events.OnBasicInfo.HasObservers or
    Events.OnStringChange[tsHandleCount].HasSubscribers or
    Events.OnStringChange[tsPagedPoolCharge].HasSubscribers or
    Events.OnStringChange[tsNonPagedPoolCharge].HasSubscribers then
    RefreshBasicInfo;

  // Skip refreshing kernel address and creator PID since these are heavy
  if Events.OnHandles.HasObservers then
    RefreshHandles;

  // See the check for the user below (as part of AppContainer info)

  if Events.OnGroups.HasObservers or
    Events.OnStringChange[tsGroupsEnabled].HasSubscribers then
    RefreshGroups;

  if Events.OnPrivileges.HasObservers or
    Events.OnStringChange[tsPrivilegesEnabled].HasSubscribers then
    RefreshPrivileges;

  if Events.OnOwner.HasObservers or
    Events.OnStringChange[tsOwner].HasSubscribers then
    RefreshOwner;

  if Events.OnPrimaryGroup.HasObservers or
    Events.OnStringChange[tsPrimaryGroup].HasSubscribers then
    RefreshPrimaryGroup;

  if Events.OnDefaultDacl.HasObservers then
    RefreshDefaultDacl;

  if Events.OnSource.HasObservers or
    Events.OnStringChange[tsSourceName].HasSubscribers or
    Events.OnStringChange[tsSourceId].HasSubscribers then
    RefreshSource;

  if Events.OnType.HasObservers then
    RefreshType;

  if Events.OnImpersonation.HasObservers then
    RefreshImpersonation;

  if (Events.OnLogonInfo.HasObservers or
    Events.OnStringChange[tsLogonAuthPackage].HasSubscribers or
    Events.OnStringChange[tsLogonType].HasSubscribers or
    Events.OnStringChange[tsLogonTime].HasSubscribers) and
    RefreshLogonInfo.IsSuccess then
    // Querying logon session info also queries statistics. No need to do it
    // again if the operation succeeded.
  else
  begin
    if Events.OnStatistics.HasObservers or
      Events.OnStringChange[tsGroups].HasSubscribers or
      Events.OnStringChange[tsPrivileges].HasSubscribers or
      Events.OnStringChange[tsType].HasSubscribers or
      Events.OnStringChange[tsTokenId].HasSubscribers or
      Events.OnStringChange[tsLogonId].HasSubscribers or
      Events.OnStringChange[tsModifiedId].HasSubscribers or
      Events.OnStringChange[tsExpires].HasSubscribers or
      Events.OnStringChange[tsDynamicCharged].HasSubscribers or
      Events.OnStringChange[tsDynamicAvailable].HasSubscribers then
      RefreshStatistics;
  end;

  if Events.OnRestrictedSids.HasObservers or
    Events.OnStringChange[tsRestrictedSids].HasSubscribers then
    RefreshRestrictedSids;

  if (Events.OnSessionInfo.HasObservers or
    Events.OnStringChange[tsSessionInfo].HasSubscribers) and
    RefreshSessionInfo.IsSuccess then
    // Querying session info also queries session ID. If the operation
    // succeeded, no need to query it again
  else
  begin
    if Events.OnSessionId.HasObservers or
      Events.OnStringChange[tsSessionId].HasSubscribers then
      RefreshSessionId;
  end;

  if Events.OnSandboxInert.HasObservers then
    RefreshSandboxInert;

  if Events.OnAuditPolicy.HasObservers then
    RefreshAuditPolicy;

  if Events.OnOrigin.HasObservers or
    Events.OnStringChange[tsOrigin].HasSubscribers then
    RefreshOrigin;

  if Events.OnElevation.HasObservers or
    Events.OnStringChange[tsElevation].HasSubscribers then
    RefreshElevation;

  if Events.OnHasRestrictions.HasObservers then
    RefreshHasRestrictions;

  if Events.OnFlags.HasObservers or
    Events.OnStringChange[tsSandBoxInert].HasSubscribers or
    Events.OnStringChange[tsFlags].HasSubscribers or
    Events.OnStringChange[tsRestricted].HasSubscribers or
    Events.OnStringChange[tsSessionReference].HasSubscribers or
    Events.OnStringChange[tsVirtualization].HasSubscribers or
    Events.OnStringChange[tsFiltered].HasSubscribers or
    Events.OnStringChange[tsUIAccess].HasSubscribers or
    Events.OnStringChange[tsLowBox].HasSubscribers or
    Events.OnStringChange[tsPrivateNamespace].HasSubscribers or
    Events.OnStringChange[tsChildFlags].HasSubscribers or
    Events.OnStringChange[tsPermissiveLearning].HasSubscribers or
    Events.OnStringChange[tsRedirectionTrust].HasSubscribers then
    RefreshFlags;

  if Events.OnVirtualizationAllowed.HasObservers then
    RefreshVirtualizationAllowed;

  if Events.OnVirtualizationEnabled.HasObservers then
    RefreshVirtualizationEnabled;

  if Events.OnIntegrity.HasObservers or
    Events.OnStringChange[tsIntegrity].HasSubscribers then
    RefreshIntegrity;

  if Events.OnMandatoryPolicy.HasObservers or
    Events.OnStringChange[tsMandatoryPolicy].HasSubscribers then
    RefreshMandatoryPolicy;

  if Events.OnLogonSids.HasObservers or
    Events.OnStringChange[tsLogonSid].HasSubscribers then
    RefreshLogonSids;

  if Events.OnIsAppContainer.HasObservers then
    RefreshIsAppContainer;

  if Events.OnCapabilities.HasObservers or
    Events.OnStringChange[tsCapabilities].HasSubscribers then
    RefreshCapabilities;

  if (Events.OnAppContainerInfo.HasObservers or
    Events.OnStringChange[tsAppContainerName].HasSubscribers or
    Events.OnStringChange[tsAppContainerDisplayName].HasSubscribers)
    and RefreshAppContainerInfo.IsSuccess then
    // Querying AppContainer info requires querying the user and package SID;
    // if the operation succeeds, no need to query them again
  else
  begin
    if Events.OnUser.HasObservers or
      Events.OnStringChange[tsUser].HasSubscribers then
      RefreshUser;

    if Events.OnAppContainerSid.HasObservers then
      RefreshAppContainerSid;
  end;

  if Events.OnAppContainerNumber.HasObservers or
    Events.OnStringChange[tsAppContainerNumber].HasSubscribers then
    RefreshAppContainerNumber;

  if Events.OnUserClaims.HasObservers or
    Events.OnStringChange[tsUserClaims].HasSubscribers then
    RefreshUserClaims;

  if Events.OnDeviceClaims.HasObservers or
    Events.OnStringChange[tsDeviceClaims].HasSubscribers then
    RefreshDeviceClaims;

  if Events.OnRestrictedUserClaims.HasObservers or
    Events.OnStringChange[tsRestrictedUserClaims].HasSubscribers then
    RefreshRestrictedUserClaims;

  if Events.OnRestrictedDeviceClaims.HasObservers or
    Events.OnStringChange[tsRestrictedDeviceClaims].HasSubscribers then
    RefreshRestrictedDeviceClaims;

  if Events.OnDeviceGroups.HasObservers or
    Events.OnStringChange[tsDeviceGroups].HasSubscribers then
    RefreshDeviceGroups;

  if Events.OnRestrictedDeviceGroups.HasObservers or
    Events.OnStringChange[tsRestrictedDeviceGroups].HasSubscribers then
    RefreshRestrictedDeviceGroups;

  if Events.OnSecurityAttributes.HasObservers or
    Events.OnStringChange[tsSecAttributes].HasSubscribers or
    Events.OnStringChange[tsSecAttributesNames].HasSubscribers then
    RefreshSecurityAttributes;

  if Events.OnIsLPAC.HasObservers or
    Events.OnStringChange[tsLPAC].HasSubscribers then
    RefreshIsLPAC;

  if Events.OnPackageClaims.HasObservers or
    Events.OnStringChange[tsPackageFlags].HasSubscribers or
    Events.OnStringChange[tsPackageOrigin].HasSubscribers then
    RefreshPackageClaims;

  if Events.OnIsRestricted.HasObservers or
    Events.OnStringChange[tsIsRestricted].HasSubscribers then
    RefreshIsRestricted;

  if Events.OnTrustLevel.HasObservers or
    Events.OnStringChange[tsTrustLevel].HasSubscribers then
    RefreshTrustLevel;

  if Events.OnPrivateNamespace.HasObservers then
    RefreshPrivateNamespace;

  if Events.OnSingletonAttributes.HasObservers or
    Events.OnStringChange[tsSingletonAttributes].HasSubscribers then
    RefreshSingletonAttributes;

  if Events.OnBnoIsolation.HasObservers or
    Events.OnStringChange[tsBnoIsolation].HasSubscribers or
    Events.OnStringChange[tsBnoPrefix].HasSubscribers then
    RefreshBnoIsolation;

  if Events.OnIsSandboxed.HasObservers or
    Events.OnStringChange[tsIsSandboxed].HasSubscribers then
    RefreshIsSandboxed;

  if Events.OnIsAppSilo.HasObservers or
    Events.OnStringChange[tsIsAppSilo].HasSubscribers then
    RefreshIsAppSilo;
end;

end.
