unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
uses
  System.SysUtils, System.Generics.Collections,
  TU.Winapi, Winapi.WinNt, Winapi.WinBase, Winapi.WinSafer,
  TU.Tokens.Types, NtUtils.Handles, TU.Common, TU.LsaApi,
  Ntapi.ntdef, Ntapi.ntobapi, NtUtils.Exceptions;

type
  /// <summary>
  ///  A class of information for tokens that can be queried and cached.
  /// </summary>
  TTokenDataClass = (tdNone, tdTokenUser, tdTokenGroups, tdTokenPrivileges,
    tdTokenOwner, tdTokenPrimaryGroup, tdTokenDefaultDacl, tdTokenSource,
    tdTokenType, tdTokenStatistics, tdTokenRestrictedSids, tdTokenSessionId,
    tdTokenAuditPolicy, tdTokenSandBoxInert, tdTokenOrigin, tdTokenElevation,
    tdTokenHasRestrictions, tdTokenVirtualizationAllowed,
    tdTokenVirtualizationEnabled, tdTokenIntegrity, tdTokenUIAccess,
    tdTokenMandatoryPolicy, tdTokenIsRestricted, tdLogonInfo, tdObjectInfo);

  /// <summary> A class of string information for tokens. </summary>
  TTokenStringClass = (tsTokenType, tsAccess, tsUserName,
    tsUserState, tsSession, tsElevation, tsIntegrity, tsObjectAddress, tsHandle,
    tsNoWriteUpPolicy, tsNewProcessMinPolicy, tsUIAccess, tsOwner,
    tsPrimaryGroup, tsSandboxInert, tsHasRestrictions, tsIsRestricted,
    tsVirtualization, tsTokenID, tsExprires, tsDynamicCharged,
    tsDynamicAvailable, tsGroupCount, tsPrivilegeCount, tsModifiedID, tsLogonID,
    tsSourceLUID, tsSourceName, tsOrigin);

  TToken = class;

  /// <summary>
  ///  A class that internally holds cache and publicly holds events. Suitable
  ///  for all tokens pointing the same kernel object.
  /// </summary>
  TTokenCacheAndEvents = class
  private
    IsCached: array [TTokenDataClass] of Boolean;
    User: TGroup;
    Groups: TGroupArray;
    Privileges: TPrivilegeArray;
    Owner: TSecurityIdentifier;
    PrimaryGroup: TSecurityIdentifier;
    Source: TTokenSource;
    TokenType: TTokenTypeEx;
    Statistics: TTokenStatistics;
    RestrictedSids: TGroupArray;
    Session: Cardinal;
    AuditPolicy: TTokenPerUserAudit; // Owned object
    SandboxInert: LongBool;
    Origin: TLuid;
    Elevation: TTokenElevationType;
    HasRestrictions: LongBool;
    VirtualizationAllowed: LongBool;
    VirtualizationEnabled: LongBool;
    Integrity: TTokenIntegrity;
    UIAccess: LongBool;
    MandatoryPolicy: TMandatoryPolicy;
    IsRestricted: LongBool;
    LogonSessionInfo: TLogonSessionInfo;
    ObjectInformation: TObjectBasicInformaion;

    FOnOwnerChange, FOnPrimaryChange: TValuedEventHandler<TSecurityIdentifier>;
    FOnSessionChange: TValuedEventHandler<Cardinal>;
    FOnAuditChange: TValuedEventHandler<TTokenPerUserAudit>;
    FOnOriginChange: TValuedEventHandler<TLuid>;
    FOnUIAccessChange: TValuedEventHandler<LongBool>;
    FOnIntegrityChange: TValuedEventHandler<TTokenIntegrity>;
    FOnVirtualizationAllowedChange: TValuedEventHandler<LongBool>;
    FOnVirtualizationEnabledChange: TValuedEventHandler<LongBool>;
    FOnPolicyChange: TValuedEventHandler<TMandatoryPolicy>;
    FOnPrivilegesChange: TValuedEventHandler<TPrivilegeArray>;
    FOnGroupsChange: TValuedEventHandler<TGroupArray>;
    FOnStatisticsChange: TValuedEventHandler<TTokenStatistics>;
    OnStringDataChange: array [TTokenStringClass] of TEventHandler<String>;

    ObjectAddress: NativeUInt;
    ReferenceCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property OnOwnerChange: TValuedEventHandler<TSecurityIdentifier> read FOnOwnerChange;
    property OnPrimaryChange: TValuedEventHandler<TSecurityIdentifier> read FOnPrimaryChange;
    property OnSessionChange: TValuedEventHandler<Cardinal> read FOnSessionChange;
    property OnAuditChange: TValuedEventHandler<TTokenPerUserAudit> read FOnAuditChange;
    property OnOriginChange: TValuedEventHandler<TLuid> read FOnOriginChange;
    property OnUIAccessChange: TValuedEventHandler<LongBool> read FOnUIAccessChange;
    property OnIntegrityChange: TValuedEventHandler<TTokenIntegrity> read FOnIntegrityChange;
    property OnVirtualizationAllowedChange: TValuedEventHandler<LongBool> read FOnVirtualizationAllowedChange;
    property OnVirtualizationEnabledChange: TValuedEventHandler<LongBool> read FOnVirtualizationEnabledChange;
    property OnPolicyChange: TValuedEventHandler<TMandatoryPolicy> read FOnPolicyChange;
    property OnPrivilegesChange: TValuedEventHandler<TPrivilegeArray> read FOnPrivilegesChange;
    property OnGroupsChange: TValuedEventHandler<TGroupArray> read FOnGroupsChange;
    property OnStatisticsChange: TValuedEventHandler<TTokenStatistics> read FOnStatisticsChange;

    /// <summary>
    ///  Calls the event listener with the newly obtained string and subscribes
    ///  for future events.
    /// </summary>
    procedure SubscribeString(StringClass: TTokenStringClass;
      Listener: TEventListener<String>; TokenToQuery: TToken);
    procedure UnSubscribeString(StringClass: TTokenStringClass;
      Listener: TEventListener<String>);
  end;

  /// <summary>
  ///  A structure that implements an interface of quering and setting token
  ///  information classes.
  /// </summary>
  TTokenData = record
  private
    Token: TToken; // Owner
    procedure SetIntegrityLevel(const Value: TTokenIntegrityLevel);
    procedure SetMandatoryPolicy(const Value: TMandatoryPolicy);
    procedure SetSession(const Value: Cardinal);
    procedure SetAuditPolicy(const Value: TTokenPerUserAudit);
    procedure SetOrigin(const Value: TLuid);
    procedure SetUIAccess(const Value: LongBool);
    procedure SetOwner(const Value: TSecurityIdentifier);
    procedure SetPrimaryGroup(const Value: TSecurityIdentifier);
    function GetVirtualizationAllowed: LongBool;
    function GetVirtualizationEnabled: LongBool;
    function GetElevation: TTokenElevationType;
    function GetGroups: TGroupArray;
    function GetHasRestrictions: LongBool;
    function GetIntegrity: TTokenIntegrity;
    function GetMandatoryPolicy: TMandatoryPolicy;
    function GetOrigin: TLuid;
    function GetOwner: TSecurityIdentifier;
    function GetPrimaryGroup: TSecurityIdentifier;
    function GetPrivileges: TPrivilegeArray;
    function GetRestrictedSids: TGroupArray;
    function GetSandboxInert: LongBool;
    function GetSession: Cardinal;
    function GetAuditPolicy: TTokenPerUserAudit;
    function GetSource: TTokenSource;
    function GetStatistics: TTokenStatistics;
    function GetTokenType: TTokenTypeEx;
    function GetUIAccess: LongBool;
    function GetUser: TGroup;
    function GetLogonSessionInfo: TLogonSessionInfo;
    function GetIsRestricted: LongBool;
    procedure InvokeStringEvent(StringClass: TTokenStringClass);
    procedure SetVirtualizationAllowed(const Value: LongBool);
    procedure SetVirtualizationEnabled(const Value: LongBool);
    function GetObjectInfo: TObjectBasicInformaion;
  public
    property User: TGroup read GetUser;                                         // class 1
    property Groups: TGroupArray read GetGroups;                                // class 2
    property Privileges: TPrivilegeArray read GetPrivileges;                    // class 3
    property Owner: TSecurityIdentifier read GetOwner write SetOwner;           // class 4 #settable
    property PrimaryGroup: TSecurityIdentifier read GetPrimaryGroup write SetPrimaryGroup; // class 5 #settable
    // TODO: class 6: DefaultDacl #settable
    property Source: TTokenSource read GetSource;                               // classes 7 & 8
    property TokenTypeInfo: TTokenTypeEx read GetTokenType;                     // class 9
    property Statistics: TTokenStatistics read GetStatistics;                   // class 10
    property RestrictedSids: TGroupArray read GetRestrictedSids;                // class 11
    property Session: Cardinal read GetSession write SetSession;                // class 12 #settable
    // TODO: class 13 TokenGroupsAndPrivileges (maybe use for optimization)
    // TODO: class 14 SessionReference #settable (and not gettable?)
    property SandboxInert: LongBool read GetSandboxInert;                       // class 15
    property AuditPolicy: TTokenPerUserAudit read GetAuditPolicy write SetAuditPolicy; // class 16 #settable
    property Origin: TLuid read GetOrigin write SetOrigin;                      // class 17 #settable
    property Elevation: TTokenElevationType read GetElevation;                  // classes 18 & 20
    // LinkedToken (class 19 #settable) is exported directly by TToken
    property HasRestrictions: LongBool read GetHasRestrictions;                 // class 21
    // TODO: class 22 AccessInformation (depends on OS version, duplicates most of the info)
    property VirtualizationAllowed: LongBool read GetVirtualizationAllowed write SetVirtualizationAllowed; // class 23 #settable
    property VirtualizationEnabled: LongBool read GetVirtualizationEnabled write SetVirtualizationEnabled; // class 24 #settable
    property Integrity: TTokenIntegrity read GetIntegrity;                      // class 25 #settable
    property IntegrityLevel: TTokenIntegrityLevel write SetIntegrityLevel;
    property UIAccess: LongBool read GetUIAccess write SetUIAccess;             // class 26 #settable
    property MandatoryPolicy: TMandatoryPolicy read GetMandatoryPolicy write SetMandatoryPolicy;    // class 27 #settable
    // class 28 TokenLogonSid returns 0 or 1 logon sids (even if there are more)
    property IsRestricted: LongBool read GetIsRestricted;                       // class 40
    property LogonSessionInfo: TLogonSessionInfo read GetLogonSessionInfo;
    property ObjectInformation: TObjectBasicInformaion read GetObjectInfo;

    /// <summary>
    ///  Ensure that the requested value is in the cache and retrieve it if
    ///  necessary.
    /// </summary>
    function Query(DataClass: TTokenDataClass): Boolean;

    /// <summary>
    ///  Forcibly update the cache by retrieving the required data class.
    /// </summary>
    function ReQuery(DataClass: TTokenDataClass): Boolean;

    /// <summary>
    ///  Make sure that a cached value (if present) is up-to-date.
    /// </summary>
    procedure ValidateCache(DataClass: TTokenDataClass);

    /// <summary> Get a string representation of an info class. </summary>
    function QueryString(StringClass: TTokenStringClass;
      Detailed: Boolean = False): String;
  end;

  /// <summary>
  ///  A class to track all the tokens and to manage their cache and events.
  /// </summary>
  TTokenFactory = class
    /// <summaty>
    ///  We need to track all the handles to find out when new ones are sent to
    ///  our process by other instances of Token Universe.
    /// </summary>
    class var HandleMapping: TDictionary<THandle,TToken>;

    /// <summary>
    ///  Different handles pointing to the same kernel objects must be linked to
    ///  the same cache/event system. This value maps each opened kernel object
    ///  address to a cache/event system (that maitains reference counting).
    /// </summary>
    class var CacheMapping: TDictionary<NativeUInt, TTokenCacheAndEvents>;

    class constructor CreateTokenFactory;
    class destructor DestroyTokenFactory;

    /// <remarks>
    ///  All tokens must register themselves at creation. The handle must be
    ///  already valid at that point.
    /// </remarks>
    class procedure RegisterToken(Token: TToken); static;

    /// <remarks>
    ///  All tokens must call it on destruction.
    /// </remarks>
    class procedure UnRegisterToken(Token: TToken); static;
  end;

  {-------------------  TToken object definition  ---------------------------}

  /// <summary>
  ///  Token Universe representation of an opend token handle.
  /// </summary>
  TToken = class
  private
    procedure SetCaption(const Value: String);
  protected
    hToken: THandle;
    FHandleInformation: THandleInfo;
    FInfoClassData: TTokenData;
    Cache: TTokenCacheAndEvents;

    FCaption: String;
    FOnCaptionChange: TValuedEventHandler<String>;
    FOnCanClose: TEventHandler<TToken>;
    FOnClose: TEventHandler<TToken>;
  protected

    {---  TToken routines to query / set data for token info classes  ---- }

    /// <summary> Queries a fixed-size info class. </summary>
    /// <remarks>
    ///  The function doesn't write to <paramref name="Data"/> parameter
    ///  if it fails.
    /// </remarks>
    function QueryFixedSize<ResultType>(InfoClass: TTokenInformationClass;
      out Data: ResultType): Boolean;

    /// <summary> Sets a fixed-size info class. </summary>
    /// <exception cref="TU.Common.ELocatedOSError">
    ///  Can raise <see cref="TU.Common.ELocatedOSError"/>.
    /// </exception>
    procedure SetFixedSize<ResultType>(InfoClass: TTokenInformationClass;
      const Value: ResultType);

    /// <summary> Queries a variable-size info class. </summary>
    /// <param name="Status">
    ///   Boolean that saves the result of the operation.
    /// </param>
    /// <remarks>
    ///   The buffer that us returned as a function value must be freed after
    ///   usege by calling <see cref="System.FreeMem"/>.
    /// </remarks>
    function QueryVariableSize(InfoClass: TTokenInformationClass;
      out Status: Boolean; ReturnedSize: PCardinal = nil): Pointer;

    /// <summary> Queries a security identifier. </summary>
    /// <remarks>
    ///  The function doesn't write to <paramref name="Sid"/> parameter
    ///  if it fails.
    /// </remarks>
    function QuerySid(InfoClass: TTokenInformationClass;
      out Sid: TSecurityIdentifier): Boolean;

    /// <summary> Queries a security identifier and attributes. </summary>
    /// <remarks>
    ///  The function doesn't write to <paramref name="Group"/> parameter
    ///  if it fails.
    /// </remarks>
    function QuerySidAndAttributes(InfoClass: TTokenInformationClass;
      out Group: TGroup): Boolean;

    /// <summary> Queries a security identifier and attributes array. </summary>
    /// <remarks>
    ///  The function doesn't write to <paramref name="GroupArray"/> parameter
    ///  if it fails.
    /// </remarks>
    function QueryGroups(InfoClass: TTokenInformationClass;
      out GroupArray: TGroupArray): Boolean;

    /// <summary>
    ///  Allocates and fills <see cref="Winapi.PTokenGroups"/>.
    /// </summary>
    /// <remarks> Call <see cref="FreeGroups"/> after use. </remarks>
    class function AllocGroups(Groups: TGroupArray;
      ResetAttributes: Boolean = False): PTokenGroups; static;

    /// <summary>
    ///  Frees memory previously allocated by <see cref="AllocGroups"/>.
    /// </summary>
    class procedure FreeGroups(Groups: PTokenGroups); static;

    /// <summary> Allocates <see cref="Winapi.PTokenPrivileges"/>. </summary>
    /// <remarks> Call <see cref="System.FreeMem"/> after use. </remarks>
    class function AllocPrivileges(Privileges: TPrivilegeArray):
      PTokenPrivileges; static;

    /// <summary> Sets thread token to the specified handle. </summary>
    class procedure AssignTokenToThread(hToken: THandle; TID: NativeUInt);
  public

    {--------------------  TToken public section ---------------------------}

    property Handle: THandle read hToken;
    property HandleInformation: THandleInfo read FHandleInformation;

    property InfoClass: TTokenData read FInfoClassData;
    property Events: TTokenCacheAndEvents read Cache;

    property Caption: String read FCaption write SetCaption;
    property OnCaptionChange: TValuedEventHandler<String> read FOnCaptionChange;

    /// <summary>
    ///  The event is called to test whether the token can be destroyed.
    ///  The listener can deny object destruction by calling
    ///  <see cref="System.SysUtils.EAbort"/>.
    /// </summary>
    property OnCanClose: TEventHandler<TToken> read FOnCanClose;

    /// <summary>
    ///  Asks all subscribed event listeners if the token can be freed.
    /// </summary>
    /// <exception cref="System.SysUtils.EAbort">
    ///  Can raise <see cref="System.SysUtils.EAbort"/>.
    /// </exception>
    function CanBeFreed: Boolean;

    /// <summary> The event is called on token destruction. </summary>
    /// <remarks> Be aware of exceptions at this point. </remarks>
    property OnClose: TEventHandler<TToken> read FOnClose;
    destructor Destroy; override;

    procedure PrivilegeAdjust(Privileges: TPrivilegeArray;
      Action: TPrivilegeAdjustAction);
    procedure GroupAdjust(Groups: TGroupArray; Action: TGroupAdjustAction);
    function SendHandleToProcess(PID: NativeUInt): NativeUInt;

    /// <summary> Assignes primary token to a process. </summary>
    procedure AssignToProcess(PID: NativeUInt);

    /// <summary> Assigned impersonation token to a thread. </summary>
    procedure AssignToThread(TID: NativeUInt);

    /// <summary> Removes the thread impersonation token. </summary>
    class procedure RevertThreadToken(TID: NativeUInt);
  public

    {--------------------  TToken constructors  ----------------------------}

    /// All the constructors can raise <see cref="TU.Common.ELocatedOSError"/>.

    /// <summary>
    ///  Registers in the factory and initializes cache.
    /// </summary>
    procedure AfterConstruction; override;

    /// <summary> General purpuse constructor. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create(Handle: THandle; Caption: String);

    /// <summary>
    ///  Create a TToken object using inherited handle.
    /// </summary>
    constructor CreateByHandle(HandleInfo: THandleInfo);

    /// <summary> Opens a token of current process. </summary>
    constructor CreateOpenCurrent(Access: TAccessMask = MAXIMUM_ALLOWED);

    /// <summary> Opens a token of a process. </summary>
    constructor CreateOpenProcess(PID: NativeUInt; ImageName: String;
      Access: TAccessMask = MAXIMUM_ALLOWED; Attributes: Cardinal = 0);

      /// <summary> Opens a token of a thread. </summary>
    constructor CreateOpenThread(TID: NativeUInt; ImageName: String;
      OpenAsSelf: Boolean; Access: TAccessMask = MAXIMUM_ALLOWED;
      Attributes: Cardinal = 0);

    /// <summary> Duplicates a token. </summary>
    constructor CreateDuplicateToken(SrcToken: TToken; Access: TAccessMask;
      TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);

    /// <summary>
    ///  Duplicates a handle. The result references for the same kernel object.
    /// </summary>
    constructor CreateDuplicateHandle(SrcToken: TToken; Access: TAccessMask;
      SameAccess: Boolean; HandleAttributes: Cardinal = 0);

    /// <summary>
    ///  Queries a token of the specified Windows Terminal Session.
    /// </summary>
    /// <remarks> Requires SeTcbPrivilege. </remarks>
    constructor CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);

    /// <summary> Creates a restricted version of the token. </summary>
    constructor CreateRestricted(SrcToken: TToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TGroupArray;
      PrivilegesToDelete: TPrivilegeArray);

    /// <summary> Logons a user with the specified credentials. </summary>
    constructor CreateWithLogon(LogonType: TSecurityLogonType;
      LogonProvider: TLogonProvider; Domain, User: String; Password: PWideChar;
      AddGroups: TGroupArray);

    /// <summary> Logon a user using Services 4 Users. </summary>
    constructor CreateS4ULogon(LogonType: TSecurityLogonType; Domain,
      User: String; const Source: TTokenSource; AddGroups: TGroupArray);

    /// <summary> Creates a new token from the scratch. </summary>
    /// <remarks> This action requires SeCreateTokenPrivilege. </remarks>
    constructor CreateNtCreateToken(User: TSecurityIdentifier;
      DisableUser: Boolean; Groups: TGroupArray; Privileges: TPrivilegeArray;
      LogonID: TLuid; Owner: TSecurityIdentifier;
      PrimaryGroup: TSecurityIdentifier; Source: TTokenSource; Expires: Int64);

    /// <summary>
    ///  Create a token using <see cref="NtImpersonateAnonymousToken">.
    /// </summary>
    constructor CreateAnonymous(Access: TAccessMask = MAXIMUM_ALLOWED;
      HandleAttributes: Cardinal = 0);

    /// <summary> Create a restricted token using Safer API. </summary>
    constructor CreateSaferToken(SrcToken: TToken; ScopeId: TSaferScopeId;
      LevelId: TSaferLevelId; MakeInert: Boolean = False);

    /// <summary>
    ///  Opens a linked token for the current token.
    ///  Requires SeTcbPrivilege to open a primary token.
    /// </summary>
    function OpenLinkedToken: CanFail<TToken>;
  end;

{----------------------  End of interface section  ----------------------------}

implementation

uses
  System.TypInfo, TU.WtsApi,
  NtUtils.Processes, Winapi.WinError, Winapi.NtSecApi,
  Ntapi.ntstatus, Ntapi.ntpsapi, Ntapi.ntseapi, Ntapi.ntrtl;

const
  /// <summary> Stores which data class a string class depends on. </summary>
  StringClassToDataClass: array [TTokenStringClass] of TTokenDataClass =
    (tdTokenType, tdNone, tdTokenUser, tdTokenUser, tdTokenSessionId,
    tdTokenElevation, tdTokenIntegrity, tdNone, tdNone, tdTokenMandatoryPolicy,
    tdTokenMandatoryPolicy, tdTokenUIAccess, tdTokenOwner, tdTokenPrimaryGroup,
    tdTokenSandBoxInert, tdTokenHasRestrictions, tdTokenIsRestricted,
    tdTokenVirtualizationAllowed, tdTokenStatistics, tdTokenStatistics,
    tdTokenStatistics, tdTokenStatistics, tdTokenStatistics, tdTokenStatistics,
    tdTokenStatistics, tdTokenStatistics, tdTokenSource,
    tdTokenSource, tdTokenOrigin);

{ TTokenCacheAndEvents }

procedure CheckAbandoned(Value: Integer; Name: String);
begin
  if Value > 0 then
    ENtError.Report(STATUS_ABANDONED, Name + ' cleanup');
end;

constructor TTokenCacheAndEvents.Create;
begin
  inherited;
  FOnOwnerChange.ComparisonFunction := CompareSIDs;
  FOnPrimaryChange.ComparisonFunction := CompareSIDs;
  FOnSessionChange.ComparisonFunction := CompareCardinals;
  FOnOriginChange.ComparisonFunction := CompareLUIDs;
  FOnUIAccessChange.ComparisonFunction := CompareLongBools;
  FOnIntegrityChange.ComparisonFunction := CompareIntegrities;
  FOnPolicyChange.ComparisonFunction := ComparePolicies;
  FOnPrivilegesChange.ComparisonFunction := ComparePrivileges;
  FOnGroupsChange.ComparisonFunction := CompareGroups;
  FOnStatisticsChange.ComparisonFunction := CompareStatistics;
  FOnVirtualizationAllowedChange.ComparisonFunction := CompareLongBools;
  FOnVirtualizationEnabledChange.ComparisonFunction := CompareLongBools;
end;

destructor TTokenCacheAndEvents.Destroy;
var
  i: TTokenStringClass;
begin
  AuditPolicy.Free;
  LogonSessionInfo.Free;

  CheckAbandoned(FOnOwnerChange.Count, 'OnOwnerChange');
  CheckAbandoned(FOnPrimaryChange.Count, 'OnPrimaryChange');
  CheckAbandoned(FOnSessionChange.Count, 'OnSessionChange');
  CheckAbandoned(FOnIntegrityChange.Count, 'OnIntegrityChange');
  CheckAbandoned(FOnUIAccessChange.Count, 'OnUIAccessChange');
  CheckAbandoned(FOnPolicyChange.Count, 'OnPolicyChange');
  CheckAbandoned(FOnPrivilegesChange.Count, 'OnPrivilegesChange');
  CheckAbandoned(FOnGroupsChange.Count, 'OnGroupsChange');
  CheckAbandoned(FOnStatisticsChange.Count, 'OnStatisticsChange');

  for i := Low(TTokenStringClass) to High(TTokenStringClass) do
    CheckAbandoned(OnStringDataChange[i].Count,
      GetEnumName(TypeInfo(TTokenStringClass), Integer(i)));

  inherited;
end;

procedure TTokenCacheAndEvents.SubscribeString(StringClass: TTokenStringClass;
  Listener: TEventListener<String>; TokenToQuery: TToken);
begin
  // Query the string and call the new event listener with it
  Listener(TokenToQuery.InfoClass.QueryString(StringClass));

  // Note: tsHandle and tsAccess should be per-handle events and should be
  // stored inside TTokenData. They are currently not supported for invocation.

  // Subscribe for future events
  OnStringDataChange[StringClass].Add(Listener);
end;

procedure TTokenCacheAndEvents.UnSubscribeString(StringClass: TTokenStringClass;
  Listener: TEventListener<String>);
begin
  // Note: tsHandle and tsAccess should be per-handle events and should be
  // stored inside TTokenData. They are currently not supported for invocation.

  OnStringDataChange[StringClass].Delete(Listener);
end;

{ TTokenFactory }

class constructor TTokenFactory.CreateTokenFactory;
begin
  HandleMapping := TDictionary<THandle, TToken>.Create;
  CacheMapping := TDictionary<NativeUInt, TTokenCacheAndEvents>.Create;
end;

class destructor TTokenFactory.DestroyTokenFactory;
begin
  CheckAbandoned(CacheMapping.Count, 'CacheMapping');
  CheckAbandoned(HandleMapping.Count, 'HandleMapping');

  CacheMapping.Destroy;
  HandleMapping.Destroy;
end;

class procedure TTokenFactory.RegisterToken(Token: TToken);
begin
  // Save TToken object for the handle
  HandleMapping.Add(Token.hToken, Token);

  // Each token needs an event handler (which is also a cahce) to be assigned
  // to it. If several TToken objects point to the same kernel object
  // then the same instance of event handler will be assigned to all of them.

  // Try to assign an existing TTokenCacheAndEvents to the token and
  // maintain reference counter for it.
  if CacheMapping.TryGetValue(NativeUInt(Token.HandleInformation.PObject),
      {out} Token.Cache) then
    Inc(Token.Cache.ReferenceCount)
  else
  begin
    // Or create a new instance
    Token.Cache := TTokenCacheAndEvents.Create;
    Token.Cache.ObjectAddress := NativeUInt(Token.HandleInformation.PObject);
    Token.Cache.ReferenceCount := 1;
    CacheMapping.Add(NativeUInt(Token.HandleInformation.PObject), Token.Cache);
  end;
end;

class procedure TTokenFactory.UnRegisterToken(Token: TToken);
begin
  // If the token initialization was not finished because of an exception in a
  // constuctor then the token was not registered and the cleanup is not needed
  if not Assigned(Token.Cache) then
    Exit;  

  // Dereference an event handler
  Dec(Token.Cache.ReferenceCount);

  // Delete it if no references are left
  if Token.Cache.ReferenceCount = 0 then
  begin
    CacheMapping.Remove(Token.Cache.ObjectAddress);
    Token.Cache.Free;
    Token.Cache := nil;
  end;

  // The handle is going to be closed
  HandleMapping.Remove(Token.hToken);
end;

{ TToken }

procedure TToken.AfterConstruction;
var
  OurHandles: THandleInfoArray;
  i: integer;
begin
  inherited;

  // Init ower of InfoClass field
  FInfoClassData.Token := Self;

  // Firstly we need to obtain a kernel object address to be able to link token
  // cache. The only way I know to do so is to make a snapshot of all
  // system/process handles and iterate through them.

  // This information might be already known (from a constructor)
  if HandleInformation.PObject = nil then
  begin
    OurHandles := THandleSnapshot.OfProcess(GetCurrentProcessId);

    for i := 0 to High(OurHandles) do
      if OurHandles[i].HandleValue = hToken then
      begin
        FHandleInformation := OurHandles[i];
        Break;
      end;
  end;

  // This should not happen and I do not know what to do here
  if FHandleInformation.PObject = nil then
    raise EAssertionFailed.Create('Can not obtain kernel object address of a ' +
      'token.');

  // Register in the factory and initialize token Cache
  TTokenFactory.RegisterToken(Self);
end;

class function TToken.AllocGroups(Groups: TGroupArray;
  ResetAttributes: Boolean): PTokenGroups;
var
  i: integer;
begin
  // The caller is responsible to free the result by calling FreeGroups.
  Result := AllocMem(SizeOf(Integer) +
    Length(Groups) * SizeOf(TSIDAndAttributes));

  Result.GroupCount := Length(Groups);

  for i := 0 to High(Groups) do
  begin
    // This function calls ConvertStringSidToSid to allocates memory that
    // we need to clean up by calling LocalFree inside FreeGroups routine.
    Result.Groups[i].Sid := Groups[i].SecurityIdentifier.AllocSid;

    if not ResetAttributes then
      Result.Groups[i].Attributes := Cardinal(Groups[i].Attributes);
  end;
end;

class function TToken.AllocPrivileges(Privileges: TPrivilegeArray):
  PTokenPrivileges;
var
  i: Integer;
  BufferSize: Cardinal;
begin
  BufferSize := SizeOf(Integer) +
    Length(Privileges) * SizeOf(TLUIDAndAttributes);

  // Allocate memory for PTokenPrivileges
  Result := AllocMem(BufferSize);

  Result.PrivilegeCount := Length(Privileges);
  for i := 0 to High(Privileges) do
    Result.Privileges[i] := Privileges[i];
end;

class procedure TToken.AssignTokenToThread(hToken: THandle; TID: NativeUInt);
var
  hThread: THandle;
  ClientId: TClientId;
  ObjAttr: TObjectAttributes;
begin
  InitializeObjectAttributes(ObjAttr);
  ClientId.Create(0, TID);

  // Open the target thread. Specially handle current thread since we don't
  // want to end up with impersonation that we can't even revert
  if TID = GetCurrentThreadId then
    hThread := NtCurrentThread
  else
    NativeCheck(NtOpenThread(hThread, THREAD_SET_THREAD_TOKEN, ObjAttr, ClientId),
    'NtOpenThread with THREAD_SET_THREAD_TOKEN');

  try
    // Set the impersonation token
    NativeCheck(NtSetInformationThread(hThread, ThreadImpersonationToken,
      @hToken, SizeOf(hToken)),
      'NtSetInformationThread#ThreadImpersonationToken');
  finally
    if hToken <> NtCurrentThread then
      NtClose(hThread);
  end;
end;

procedure TToken.AssignToProcess(PID: NativeUInt);
var
  hProcess: THandle;
  ClientId: TClientId;
  ObjAttr: TObjectAttributes;
  AccessToken: TProcessAccessToken;
  Status: NTSTATUS;
begin
  InitializeObjectAttributes(ObjAttr);
  ClientId.Create(PID, 0);

  // Open the target process
  NativeCheck(NtOpenProcess(hProcess, PROCESS_QUERY_INFORMATION or
    PROCESS_SET_INFORMATION, ObjAttr, ClientId), 'NtOpenProcess with ' +
    'PROCESS_QUERY_INFORMATION | PROCESS_SET_INFORMATION');

  // Open its first thread and store the handle inside AccessToken
  Status := NtGetNextThread(hProcess, 0, THREAD_QUERY_LIMITED_INFORMATION, 0, 0,
    AccessToken.Thread);

  if not NT_SUCCESS(Status) then
  begin
    NtClose(hProcess);
    raise ENtError.Create(Status,
      'NtGetNextThread with THREAD_QUERY_LIMITED_INFORMATION');
  end;

  // Prepare the token handle. The thread handle is already in here.
  AccessToken.Token := hToken;

  // Assign the token for the process
  Status := NtSetInformationProcess(hProcess, ProcessAccessToken,
    @AccessToken, SizeOf(AccessToken));

  // Close the process and the thread but not the token
  NtClose(hProcess);
  NtClose(AccessToken.Thread);

  NativeCheck(Status, 'NtSetInformationProcess#ProcessAccessToken', Self);

  // Assigning primary token to a process migh change token's Session ID
  InfoClass.ValidateCache(tdTokenSessionId);

  // Although changing session does not usually change Modified ID it is good to
  // update it
  InfoClass.ValidateCache(tdTokenStatistics);
end;

procedure TToken.AssignToThread(TID: NativeUInt);
begin
  AssignTokenToThread(hToken, TID);
  { TODO: Query and compare the result token to the current one since it
    could've been duplicated to Identification level. }
end;

function TToken.CanBeFreed: Boolean;
begin
  // Check whether someone wants to raise EAbort to deny token destruction
  OnCanClose.Invoke(Self);
  Result := True;
end;

constructor TToken.Create(Handle: THandle; Caption: String);
begin
  hToken := Handle;
  FCaption := Caption;;
end;

constructor TToken.CreateAnonymous(Access: TAccessMask;
  HandleAttributes: Cardinal);
var
  hTokenToRevert: THandle;
  WereImpersonating: Boolean;
begin
  // Save current impersonation token if we have it
  WereImpersonating := NT_SUCCESS(NtOpenThreadTokenEx(NtCurrentThread,
    TOKEN_IMPERSONATE, True, 0, hTokenToRevert));

  try
    NativeCheck(NtImpersonateAnonymousToken(NtCurrentThread),
      'NtImpersonateAnonymousToken');

    NativeCheck(NtOpenThreadTokenEx(NtCurrentThread, Access, True,
      HandleAttributes, hToken), 'NtOpenThreadTokenEx');
  finally
    // We need to undo current impersonation to the old one or to a state
    // with no thread token (aka to zero token)
    if not WereImpersonating then
      hTokenToRevert := 0;

    NtSetInformationThread(NtCurrentThread, ThreadImpersonationToken,
      @hTokenToRevert, SizeOf(hTokenToRevert));

    if WereImpersonating then
      NtClose(hTokenToRevert);
  end;

  FCaption := 'Anonymous token';
end;

constructor TToken.CreateByHandle(HandleInfo: THandleInfo);
begin
  if HandleInfo.UniqueProcessId <> GetCurrentProcessId then
    raise ENotImplemented.Create('TODO');

  hToken := HandleInfo.HandleValue;
  FHandleInformation := HandleInfo;
  FCaption := Format('Inherited %d [0x%x]', [hToken, hToken]);
end;

constructor TToken.CreateDuplicateHandle(SrcToken: TToken; Access: TAccessMask;
  SameAccess: Boolean; HandleAttributes: Cardinal = 0);
const
  Options: array [Boolean] of Cardinal = (0, DUPLICATE_SAME_ACCESS);
var
  Status: NTSTATUS;
  i: Integer;
label
  Done;
begin
  // DuplicateHandle does not support MAXIMUM_ALLOWED access and returns zero
  // access instead. We should implement it on our own by probing additional
  // access masks.

  // Make a lucky guess for MAXIMUM_ALLOWED: try full access first
  if (Access = MAXIMUM_ALLOWED) and not SameAccess then
  begin
    Status := NtDuplicateObject(NtCurrentProcess, SrcToken.hToken,
      NtCurrentProcess, hToken, TOKEN_ALL_ACCESS, HandleAttributes,
      Options[SameAccess]);

    // Check for errors different than access problems
    if Status <> STATUS_ACCESS_DENIED then
      NativeCheck(Status, 'NtDuplicateObject', SrcToken);

    // If the guess was correct no further processing required
    if NT_SUCCESS(Status) then
      goto Done;

    // Full access didn't work. Collect the access that is already granted
    Access := SrcToken.HandleInformation.GrantedAccess;

    // Try each one that is not granted yet
    for i := 0 to ACCESS_COUNT - 1 do
      if (Access and AccessValues[i]) = 0 then
        if NT_SUCCESS(NtDuplicateObject(NtCurrentProcess, SrcToken.hToken,
          NtCurrentProcess, hToken, AccessValues[i], 0, 0)) then
        begin
          // Yes, this access can be granted, add it
          Access := Access or AccessValues[i];
          NtClose(hToken);
        end;

    // At this point Access variable contains expanded MAXIMUM_ALLOWED
  end;

  // Finally, duplicate the handle
  NativeCheck(NtDuplicateObject(NtCurrentProcess, SrcToken.hToken,
    NtCurrentProcess, hToken, Access, HandleAttributes,
    Options[SameAccess]), 'NtDuplicateObject', SrcToken);

  Done: FCaption := SrcToken.Caption + ' (ref)'
  // TODO: No need to snapshot handles, object address is already known
end;

constructor TToken.CreateDuplicateToken(SrcToken: TToken; Access: TAccessMask;
  TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);
var
  ObjAttr: TObjectAttributes;
  SecQos: TSecurityQualityOfService;
  TokenType: TTokenType;
begin
  // Prepare Security QoS to store the impersonation level
  FillChar(SecQos, SizeOf(SecQos), 0);
  SecQos.Length := SizeOf(SecQos);
  SecQos.EffectiveOnly := EffectiveOnly;

  if TokenTypeEx = ttPrimary then
    TokenType := TokenPrimary
  else
  begin
    TokenType := TokenImpersonation;
    SecQos.ImpersonationLevel := TSecurityImpersonationLevel(TokenTypeEx);
  end;

  InitializeObjectAttributes(ObjAttr, nil, 0, 0, @SecQos);

  NativeCheck(NtDuplicateToken(SrcToken.hToken, Access, @ObjAttr, EffectiveOnly,
    TokenType, hToken), 'NtDuplicateToken', SrcToken);

  if EffectiveOnly then
    FCaption := SrcToken.Caption + ' (eff. copy)'
  else
    FCaption := SrcToken.Caption + ' (copy)'
end;

constructor TToken.CreateNtCreateToken(User: TSecurityIdentifier;
  DisableUser: Boolean; Groups: TGroupArray; Privileges: TPrivilegeArray;
  LogonID: TLuid; Owner: TSecurityIdentifier; PrimaryGroup: TSecurityIdentifier;
  Source: TTokenSource; Expires: Int64);
var
  TokenUser: TTokenUser;
  TokenGroups: PTokenGroups;
  TokenPrivileges: PTokenPrivileges;
  TokenOwner: TTokenOwner;
  TokenPrimaryGroup: TTokenPrimaryGroup;
begin
  // Fill user attributes. Zero value is default here and means "Enabled"
  if DisableUser then
    TokenUser.User.Attributes := SE_GROUP_USE_FOR_DENY_ONLY
  else
    TokenUser.User.Attributes := 0;

  TokenGroups := nil;
  TokenPrivileges := nil;
  TokenUser.User.Sid := nil;
  TokenUser.User.Sid := nil;
  TokenOwner.Owner := nil;
  TokenPrimaryGroup.PrimaryGroup := nil;

  try
    // Allocate groups. This memory should be freed with FreeGroups
    TokenGroups := AllocGroups(Groups);

    // Allocate privileges. This memory should be freed with FreeMem
    TokenPrivileges := AllocPrivileges(Privileges);

    // Allocate user, owner, and primary group SIDs.
    // This memory should be freed with LocalFree.
    TokenUser.User.Sid := User.AllocSid;
    TokenOwner.Owner := Owner.AllocSid;
    TokenPrimaryGroup.PrimaryGroup := PrimaryGroup.AllocSid;
  except
    // Free memory in case of abnormal termination
    if Assigned(TokenGroups) then
      FreeGroups(TokenGroups);
    if Assigned(TokenPrivileges) then
      FreeMem(TokenPrivileges);
    if Assigned(TokenUser.User.Sid) then
      LocalFree(TokenUser.User.Sid);
    if Assigned(TokenOwner.Owner) then
      LocalFree(TokenOwner.Owner);
    if Assigned(TokenPrimaryGroup.PrimaryGroup) then
      LocalFree(TokenPrimaryGroup.PrimaryGroup);
    raise;
  end;

  // Call NtCreateToken.
  try
    NativeCheck(NtCreateToken(hToken, TOKEN_ALL_ACCESS, nil, TokenPrimary,
      @LogonID, @Expires, @TokenUser, TokenGroups, TokenPrivileges,
      @TokenOwner, @TokenPrimaryGroup, nil, @Source), 'NtCreateToken');
  finally
    LocalFree(TokenPrimaryGroup.PrimaryGroup);
    LocalFree(TokenOwner.Owner);
    LocalFree(TokenUser.User.Sid);
    FreeMem(TokenPrivileges);
    FreeGroups(TokenGroups);
  end;

  FCaption := 'New token: ';
  if User.HasPrettyName then
    FCaption := FCaption + User.User
  else
    FCaption := FCaption + User.SID;
end;

constructor TToken.CreateOpenCurrent(Access: TAccessMask);
begin
  CreateOpenProcess(GetCurrentProcessId, 'Current process');
end;

constructor TToken.CreateOpenProcess(PID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal);
var
  hProcess: THandle;
  ClientId: TClientId;
  ObjAttr: TObjectAttributes;
begin
  if PID = GetCurrentProcessId then
    hProcess := NtCurrentProcess
  else
  begin
    InitializeObjectAttributes(ObjAttr);
    ClientId.Create(PID, 0);

    NativeCheck(NtOpenProcess(hProcess, PROCESS_QUERY_LIMITED_INFORMATION,
      ObjAttr, ClientId),
      'NtOpenProcess with PROCESS_QUERY_LIMITED_INFORMATION');
  end;

  try
    NativeCheck(NtOpenProcessTokenEx(hProcess, Access, Attributes, hToken),
      'NtOpenProcessTokenEx');
  finally
    NtClose(hProcess);
  end;

  FCaption := Format('%s [%d]', [ImageName, PID]);
end;

constructor TToken.CreateOpenThread(TID: NativeUInt; ImageName: String;
  OpenAsSelf: Boolean; Access: TAccessMask; Attributes: Cardinal);
var
  ObjAttr: TObjectAttributes;
  ClientId: TClientId;
  hThread: THandle;
begin
  InitializeObjectAttributes(ObjAttr);
  ClientId.Create(0, TID);

  // Open the target thread. Note that we always can access our own.
  if TID = GetCurrentThreadId then
    hThread := NtCurrentThread
  else
    NativeCheck(NtOpenThread(hThread, THREAD_QUERY_LIMITED_INFORMATION, ObjAttr,
      ClientId), 'NtOpenThread with THREAD_QUERY_LIMITED_INFORMATION');

  try
    NativeCheck(NtOpenThreadTokenEx(hThread, Access, OpenAsSelf, Attributes,
      hToken), 'NtOpenThreadTokenEx');
  finally
    if hThread <> NtCurrentThread then
      NtClose(hThread);
  end;

  FCaption := Format('Thread %d of %s', [TID, ImageName]);
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);
begin
  WinCheck(WTSQueryUserToken(SessionID, hToken), 'WTSQueryUserToken');
  FCaption := Format('Session %d token', [SessionID]);
end;

constructor TToken.CreateRestricted(SrcToken: TToken; Flags: Cardinal;
  SIDsToDisabe, SIDsToRestrict: TGroupArray;
  PrivilegesToDelete: TPrivilegeArray);
var
  DisableGroups, RestrictGroups: PTokenGroups;
  DeletePrivileges: PTokenPrivileges;
begin
  // Prepare SIDs and LUIDs
  DisableGroups := AllocGroups(SIDsToDisabe);
  DeletePrivileges := AllocPrivileges(PrivilegesToDelete);

  // Attributes for Restricting SIDs must be set to zero
  RestrictGroups := AllocGroups(SIDsToRestrict, True);
  try
    // aka CreateRestrictedToken API
    NativeCheck(NtFilterToken(SrcToken.hToken, Flags, DisableGroups,
      DeletePrivileges, RestrictGroups, hToken), 'NtFilterToken', SrcToken);

    FCaption := 'Restricted ' + SrcToken.Caption;
  finally
    FreeGroups(RestrictGroups);
    FreeMem(DeletePrivileges);
    FreeGroups(DisableGroups);
  end;
end;

constructor TToken.CreateS4ULogon(LogonType: TSecurityLogonType; Domain,
  User: String; const Source: TTokenSource; AddGroups: TGroupArray);
var
  IsWow64: NativeUInt;
  Status, SubStatus: NTSTATUS;
  LsaHandle: TLsaHandle;
  PkgName: ANSI_STRING;
  AuthPkg: Cardinal;
  Buffer: PKERB_S4U_LOGON;
  BufferSize: Cardinal;
  OriginName: ANSI_STRING;
  GroupArray: PTokenGroups;
  ProfileBuffer: Pointer;
  ProfileSize: Cardinal;
  LogonId: TLuid;
  Quotas: TQuotaLimits;
begin
  // TODO -c WoW64: LsaLogonUser overwrites our memory
  if not NT_SUCCESS(NtQueryInformationProcess(NtCurrentProcess,
    ProcessWow64Information, @IsWow64, SizeOf(IsWow64), nil)) or
    (IsWow64 <> 0) then
    raise ENotSupportedException.Create('S4U is not supported under WoW64');

  // Connect to the LSA
  NativeCheck(LsaConnectUntrusted(LsaHandle), 'LsaConnectUntrusted');

  // Lookup for Negotiate package
  PkgName.FromString(NEGOSSP_NAME_A);
  Status := LsaLookupAuthenticationPackage(LsaHandle, PkgName, AuthPkg);

  if not NT_SUCCESS(Status) then
  begin
    LsaDeregisterLogonProcess(LsaHandle);
    raise ENtError.Create(Status, 'LsaLookupAuthenticationPackage');
  end;

  // We need to prepare a blob where KERB_S4U_LOGON is followed by the username
  // and the domain.
  BufferSize := SizeOf(KERB_S4U_LOGON) + Length(User) * SizeOf(WideChar) +
    Length(Domain) * SizeOf(WideChar);
  Buffer := AllocMem(BufferSize);

  Buffer.MessageType := KerbS4ULogon;

  Buffer.ClientUpn.Length := Length(User) * SizeOf(WideChar);
  Buffer.ClientUpn.MaximumLength := Buffer.ClientUpn.Length;

  // Place the username just after the structure
  Buffer.ClientUpn.Buffer := Pointer(NativeUInt(Buffer) +
    SizeOf(KERB_S4U_LOGON));
  Move(PWideChar(User)^, Buffer.ClientUpn.Buffer^, Buffer.ClientUpn.Length);

  Buffer.ClientRealm.Length := Length(Domain) * SizeOf(WideChar);
  Buffer.ClientRealm.MaximumLength := Buffer.ClientRealm.Length;

  // Place the domain after the username
  Buffer.ClientRealm.Buffer := Pointer(NativeUInt(Buffer) +
    SizeOf(KERB_S4U_LOGON) + Buffer.ClientUpn.Length);
  Move(PWideChar(Domain)^, Buffer.ClientRealm.Buffer^,
    Buffer.ClientRealm.Length);

  OriginName.FromString('S4U');

  if Length(AddGroups) > 0 then
    GroupArray := AllocGroups(AddGroups)
  else
    GroupArray := nil;

  // Perform the logon
  SubStatus := STATUS_SUCCESS;
  Status := LsaLogonUser(LsaHandle, OriginName, LogonType, AuthPkg, Buffer,
    BufferSize, GroupArray, Source, ProfileBuffer, ProfileSize, LogonId, hToken,
    Quotas, SubStatus);

  // Clean up
  LsaFreeReturnBuffer(ProfileBuffer);

  if Assigned(GroupArray) then
      FreeGroups(GroupArray);

  FreeMem(Buffer);
  LsaDeregisterLogonProcess(LsaHandle);

  // Prefer more detailed error information
  if not NT_SUCCESS(SubStatus) then
    Status := SubStatus;

  if not NT_SUCCESS(Status) then
    raise ENtError.Create(Status, 'LsaLogonUser');

  FCaption := 'S4U logon of ' + User;
end;

constructor TToken.CreateSaferToken(SrcToken: TToken; ScopeId: TSaferScopeId;
  LevelId: TSaferLevelId; MakeInert: Boolean = False);
var
  hLevel: TSaferLevelHandle;
  Flags: Cardinal;
  LevelName: String;
begin
  WinCheck(SaferCreateLevel(ScopeId, LevelId, SAFER_LEVEL_OPEN, hLevel),
    'SaferCreateLevel');

  Flags := 0;
  if MakeInert then
    Flags := Flags or SAFER_TOKEN_MAKE_INERT;

  try
    WinCheck(SaferComputeTokenFromLevel(hLevel, SrcToken.hToken, hToken,
      Flags, nil), 'SaferComputeTokenFromLevel', SrcToken);
  finally
    SaferCloseLevel(hLevel);
  end;

  case LevelId of
    SAFER_LEVELID_FULLYTRUSTED:
      LevelName := 'Unrestricted';

    SAFER_LEVELID_NORMALUSER:
      LevelName := 'Normal';

    SAFER_LEVELID_CONSTRAINED:
      LevelName := 'Constrained';

    SAFER_LEVELID_UNTRUSTED:
      LevelName := 'Untrusted';

    SAFER_LEVELID_DISALLOWED:
      LevelName := 'Disallowed'
  end;

  FCaption := LevelName + ' Safer for ' + SrcToken.FCaption;
end;

constructor TToken.CreateWithLogon(LogonType: TSecurityLogonType;
  LogonProvider: TLogonProvider; Domain, User: String; Password: PWideChar;
  AddGroups: TGroupArray);
var
  GroupArray: PTokenGroups;
begin
  // If the user doesn't ask us to add some groups to the token we can use
  // simplier LogonUserW routine. Otherwise we use LogonUserExExW (that
  // requires SeTcbPrivilege to add group membership)

  if Length(AddGroups) = 0 then
    WinCheck(LogonUserW(PWideChar(User), PWideChar(Domain), Password, LogonType,
      LogonProvider, hToken), 'LogonUserW')
  else
  begin
    // Allocate SIDs for groups
    GroupArray := AllocGroups(AddGroups);
    try
      WinCheck(LogonUserExExW(PWideChar(User), PWideChar(Domain), Password,
        LogonType, LogonProvider, GroupArray, hToken, nil, nil, nil, nil),
        'LogonUserExExW');
    finally
      FreeGroups(GroupArray);
    end;
  end;

  FCaption := 'Logon of ' + User;
end;

destructor TToken.Destroy;
begin
  // Inform event listeners that we are closing the handle
  try
    OnClose.Invoke(Self);
  except
    on E: Exception do
    begin
      // This is really bad. At least inform the debugger...
      ENtError.Report(STATUS_ASSERTION_FAILURE, 'Token.OnClose: ' + E.Message);
      raise;
    end;
  end;

  // Unregister from the factory before we close the handle
  TTokenFactory.UnRegisterToken(Self);

  if hToken <> 0 then
  try
    NtClose(hToken); // A protected handle may cause an exception
    hToken := 0;
  except
    ; // but destructor should always succeed
  end;

  CheckAbandoned(FOnCanClose.Count, 'OnCanClose');
  CheckAbandoned(FOnClose.Count, 'FOnClose');
  CheckAbandoned(FOnCaptionChange.Count, 'FOnCaptionChange');

  inherited;
end;

class procedure TToken.FreeGroups(Groups: PTokenGroups);
var
  i: Integer;
begin
  Assert(Assigned(Groups));

  // The memory of each item was previously allocated by ConvertStringSidToSid.
  for i := 0 to Groups.GroupCount - 1 do
    if Assigned(Groups.Groups[i].Sid) then
      LocalFree(Groups.Groups[i].Sid);

  FreeMem(Groups);
end;

procedure TToken.GroupAdjust(Groups: TGroupArray; Action:
  TGroupAdjustAction);
const
  IsResetFlag: array [TGroupAdjustAction] of LongBool = (True, False, False);
var
  i: integer;
  GroupArray: PTokenGroups;
begin
  // Allocate group SIDs
  GroupArray := AllocGroups(Groups);

  // Set approriate attribes depending on the action
  for i := 0 to GroupArray.GroupCount - 1 do
    if Action = gaEnable then
      GroupArray.Groups[i].Attributes := Cardinal(GroupEnabled)
    else
      GroupArray.Groups[i].Attributes := 0;

  try
    NativeCheck(NtAdjustGroupsToken(hToken, IsResetFlag[Action], GroupArray, 0,
      nil, nil), 'NtAdjustGroupsToken', Self);

    // Update the cache and notify event listeners
    InfoClass.ValidateCache(tdTokenGroups);
    InfoClass.ValidateCache(tdTokenStatistics);

    // Adjusting groups may change integrity attributes
    InfoClass.ValidateCache(tdTokenIntegrity);
  finally
    FreeGroups(GroupArray);
  end;
end;

function TToken.OpenLinkedToken: CanFail<TToken>;
var
  Handle: THandle;
begin
  Result.Init(Self);

  if Result.CheckError(QueryFixedSize<THandle>(TokenLinkedToken, Handle),
    GetterMessage(TokenLinkedToken)) then
    Result.Value := TToken.Create(Handle, 'Linked token for ' + Caption);
end;

procedure TToken.PrivilegeAdjust(Privileges: TPrivilegeArray;
  Action: TPrivilegeAdjustAction);
const
  ActionToAttribute: array [TPrivilegeAdjustAction] of Cardinal =
    (SE_PRIVILEGE_ENABLED, 0, SE_PRIVILEGE_REMOVED);
var
  PrivArray: PTokenPrivileges;
  i: integer;
  Status: NTSTATUS;
begin
  // Allocate privileges
  PrivArray := AllocPrivileges(Privileges);
  try
    for i := 0 to PrivArray.PrivilegeCount - 1 do
      PrivArray.Privileges[i].Attributes := ActionToAttribute[Action];

    // Perform adjustment
    Status := NtAdjustPrivilegesToken(hToken, False, PrivArray, 0, nil, nil);

    // Note: the system call might return STATUS_NOT_ALL_ASSIGNED which is
    // not considered as an error. Such behavior does not fit into our
    // model so we should overwrite it.
    if not NT_SUCCESS(Status) then
      raise ENtError.Create(Status, 'NtAdjustPrivilegesToken', Self)
    else if Status = STATUS_NOT_ALL_ASSIGNED then
      raise EWinError.Create(ERROR_NOT_ALL_ASSIGNED, 'AdjustTokenPrivileges',
        Self);
  finally
    FreeMem(PrivArray);

    // The function could modify privileges even without succeeding.
    // Update the cache and notify event listeners.
    InfoClass.ValidateCache(tdTokenPrivileges);
    InfoClass.ValidateCache(tdTokenStatistics);
  end;
end;

function TToken.QueryFixedSize<ResultType>(InfoClass: TTokenInformationClass;
  out Data: ResultType): Boolean;
var
  BufferData: ResultType;
  ReturnLength: Cardinal;
begin
  // TODO: Save error code and error location
  Result := GetTokenInformation(hToken, InfoClass, @BufferData,
    SizeOf(ResultType), ReturnLength);

  // Modify the specified Data parameter only on success
  if Result then
    Data := BufferData;
end;

function TToken.QueryGroups(InfoClass: TTokenInformationClass;
  out GroupArray: TGroupArray): Boolean;
var
  Buffer: PTokenGroups;
  i: integer;
begin
  // PTokenGroups can point to a variable-sized memory
  Buffer := QueryVariableSize(InfoClass, Result);

  if Result then
  try
    SetLength(GroupArray, Buffer.GroupCount);

    for i := 0 to Buffer.GroupCount - 1 do
    begin
      // Each SID should be converted to a TSecurityIdentifier
      GroupArray[i].SecurityIdentifier.CreateFromSid(Buffer.Groups[i].Sid);
      GroupArray[i].Attributes := TGroupAttributes(Buffer.Groups[i].Attributes);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.QuerySid(InfoClass: TTokenInformationClass;
  out Sid: TSecurityIdentifier): Boolean;
var
  Buffer: PTokenOwner; // aka TTokenPrimaryGroup aka PPSID
begin
  Buffer := QueryVariableSize(InfoClass, Result);
  if Result then
  try
    Sid := TSecurityIdentifier.CreateFromSid(Buffer.Owner);
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.QuerySidAndAttributes(InfoClass: TTokenInformationClass;
  out Group: TGroup): Boolean;
var
  Buffer: PSIDAndAttributes;
begin
  Buffer := QueryVariableSize(InfoClass, Result);
  if Result then
  try
    Group.SecurityIdentifier.CreateFromSid(Buffer.Sid);
    Group.Attributes := TGroupAttributes(Buffer.Attributes);
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.QueryVariableSize(InfoClass: TTokenInformationClass;
  out Status: Boolean; ReturnedSize: PCardinal): Pointer;
var
  BufferSize, ReturnValue: Cardinal;
begin
  Status := False;
  // TODO: fix potenrial race condition

  // Make a probe call to estimate a requied buffer size
  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);

  // Check for errors and for too big buffers
  if not WinTryCheckBuffer(BufferSize) then
    Exit(nil);

  // Allocate memory
  Result := AllocMem(BufferSize);

  // Query the info class again with a buffer enough to hold the data
  Status := GetTokenInformation(hToken, InfoClass, Result, BufferSize,
    ReturnValue);

  // Clean up on failure
  if not Status then
  begin
    FreeMem(Result);
    Result := nil;
  end
  else if Assigned(ReturnedSize) then
    ReturnedSize^ := BufferSize;

  // Do not free the buffer on success. The caller must do it after use.
end;

class procedure TToken.RevertThreadToken(TID: NativeUInt);
begin
  // Set the handle to zero to revoke the impersonation token
  AssignTokenToThread(0, TID);
end;

function TToken.SendHandleToProcess(PID: NativeUInt): NativeUInt;
var
  hTargetProcess: THandle;
  ClientId: TClientId;
  ObjAttr: TObjectAttributes;
begin
  InitializeObjectAttributes(ObjAttr);
  ClientId.Create(PID, 0);

  // Open the target process
  NativeCheck(NtOpenProcess(hTargetProcess, PROCESS_DUP_HANDLE, ObjAttr,
    ClientId), 'NtOpenProcess with PROCESS_DUP_HANDLE', Self);

  try
    // Send the handle
    NativeCheck(NtDuplicateObject(NtCurrentProcess, hToken, hTargetProcess,
      Result, 0, 0, DUPLICATE_SAME_ACCESS or DUPLICATE_SAME_ATTRIBUTES),
      'NtDuplicateObject', Self);
  finally
    NtClose(hTargetProcess);
  end;
end;

procedure TToken.SetCaption(const Value: String);
begin
  FCaption := Value;
  OnCaptionChange.Invoke(FCaption);
end;

procedure TToken.SetFixedSize<ResultType>(InfoClass: TTokenInformationClass;
  const Value: ResultType);
begin
  if not SetTokenInformation(hToken, InfoClass, @Value, SizeOf(Value))
    then raise EWinError.Create(GetLastError, SetterMessage(InfoClass), Self);
end;

{ TTokenData }

function TTokenData.GetAuditPolicy: TTokenPerUserAudit;
begin
  Assert(Token.Cache.IsCached[tdTokenAuditPolicy]);
  Result := Token.Cache.AuditPolicy;
end;

function TTokenData.GetElevation: TTokenElevationType;
begin
  Assert(Token.Cache.IsCached[tdTokenElevation]);
  Result := Token.Cache.Elevation;
end;

function TTokenData.GetGroups: TGroupArray;
begin
  Assert(Token.Cache.IsCached[tdTokenGroups]);
  Result := Token.Cache.Groups;
end;

function TTokenData.GetHasRestrictions: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenHasRestrictions]);
  Result := Token.Cache.HasRestrictions;
end;

function TTokenData.GetIntegrity: TTokenIntegrity;
begin
  Assert(Token.Cache.IsCached[tdTokenIntegrity]);
  Result := Token.Cache.Integrity;
end;

function TTokenData.GetIsRestricted: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenIsRestricted]);
  Result := Token.Cache.IsRestricted;
end;

function TTokenData.GetLogonSessionInfo: TLogonSessionInfo;
begin
  Assert(Token.Cache.IsCached[tdLogonInfo]);
  Result := Token.Cache.LogonSessionInfo;
end;

function TTokenData.GetMandatoryPolicy: TMandatoryPolicy;
begin
  Assert(Token.Cache.IsCached[tdTokenMandatoryPolicy]);
  Result := Token.Cache.MandatoryPolicy;
end;

function TTokenData.GetObjectInfo: TObjectBasicInformaion;
begin
  Assert(Token.Cache.IsCached[tdObjectInfo]);
  Result := Token.Cache.ObjectInformation;
end;

function TTokenData.GetOrigin: TLuid;
begin
  Assert(Token.Cache.IsCached[tdTokenOrigin]);
  Result := Token.Cache.Origin;
end;

function TTokenData.GetOwner: TSecurityIdentifier;
begin
  Assert(Token.Cache.IsCached[tdTokenOwner]);
  Result := Token.Cache.Owner;
end;

function TTokenData.GetPrimaryGroup: TSecurityIdentifier;
begin
  Assert(Token.Cache.IsCached[tdTokenPrimaryGroup]);
  Result := Token.Cache.PrimaryGroup;
end;

function TTokenData.GetPrivileges: TPrivilegeArray;
begin
  Assert(Token.Cache.IsCached[tdTokenPrivileges]);
  Result := Token.Cache.Privileges;
end;

function TTokenData.GetRestrictedSids: TGroupArray;
begin
  Assert(Token.Cache.IsCached[tdTokenRestrictedSids]);
  Result := Token.Cache.RestrictedSids;
end;

function TTokenData.GetSandboxInert: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenSandBoxInert]);
  Result := Token.Cache.SandboxInert;
end;

function TTokenData.GetSession: Cardinal;
begin
  Assert(Token.Cache.IsCached[tdTokenSessionId]);
  Result := Token.Cache.Session;
end;

function TTokenData.GetSource: TTokenSource;
begin
  Assert(Token.Cache.IsCached[tdTokenSource]);
  Result := Token.Cache.Source;
end;

function TTokenData.GetStatistics: TTokenStatistics;
begin
  Assert(Token.Cache.IsCached[tdTokenStatistics]);
  Result := Token.Cache.Statistics;
end;

function TTokenData.GetTokenType: TTokenTypeEx;
begin
  Assert(Token.Cache.IsCached[tdTokenType]);
  Result := Token.Cache.TokenType;
end;

function TTokenData.GetUIAccess: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenUIAccess]);
  Result := Token.Cache.UIAccess;
end;

function TTokenData.GetUser: TGroup;
begin
  Assert(Token.Cache.IsCached[tdTokenUser]);
  Result := Token.Cache.User;
end;

function TTokenData.GetVirtualizationAllowed: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenVirtualizationAllowed]);
  Result := Token.Cache.VirtualizationAllowed;
end;

function TTokenData.GetVirtualizationEnabled: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenVirtualizationEnabled]);
  Result := Token.Cache.VirtualizationEnabled;
end;

procedure TTokenData.InvokeStringEvent(StringClass: TTokenStringClass);
begin
  // Note that tsHandle and tsAccess are per-handle events
  case StringClass of
    tsHandle: ; // Not supported yet
    tsAccess: ; // Not supported yet
  else
    Token.Events.OnStringDataChange[StringClass].Invoke(
      QueryString(StringClass));
  end;
end;

function TTokenData.Query(DataClass: TTokenDataClass): Boolean;
begin
  if Token.Cache.IsCached[DataClass] or ReQuery(DataClass) then
    Result := True
  else
    Result := False;
end;

function TTokenData.QueryString(StringClass: TTokenStringClass;
  Detailed: Boolean): String;
begin
  if not Query(StringClassToDataClass[StringClass]) then
    Exit('Unknown');

  {$REGION 'Converting cached data to string'}
  case StringClass of
    tsTokenType:
      Result := Token.Cache.TokenType.ToString;

    // Note: this is a per-handle value. Beware of per-kernel-object events.
    tsAccess:
      if Detailed then
        Result := AccessToDetailedString(Token.HandleInformation.GrantedAccess)
      else
        Result := AccessToString(Token.HandleInformation.GrantedAccess);

    tsUserName:
      Result := Token.Cache.User.SecurityIdentifier.ToString;

    tsUserState:
      Result := Token.Cache.User.Attributes.ToString;

    tsSession: // Detailed?
      Result := Token.Cache.Session.ToString;

    tsElevation:
      Result := Token.Cache.Elevation.ToString;

    tsIntegrity:
      Result := Token.Cache.Integrity.ToString;

    tsObjectAddress:
      Result := Format('0x%0.8x',
        [NativeUInt(Token.HandleInformation.PObject)]);

    // Note: this is a per-handle value. Beware of per-kernel-object events.
    tsHandle:
      if Detailed then
        Result := Format('0x%x (%d)', [Token.Handle, Token.Handle])
      else
        Result := Format('0x%x', [Token.Handle]);

    tsNoWriteUpPolicy:
      Result := EnabledDisabledToString(Token.Cache.MandatoryPolicy.Contains(
        MandatoryPolicyNoWriteUp));

    tsNewProcessMinPolicy:
      Result := EnabledDisabledToString(Token.Cache.MandatoryPolicy.Contains(
        MandatoryPolicyNewProcessMin));

    tsUIAccess:
      Result := EnabledDisabledToString(Token.Cache.UIAccess);

    tsOwner:
      Result := Token.Cache.Owner.ToString;

    tsPrimaryGroup:
      Result := Token.Cache.PrimaryGroup.ToString;

    tsSandboxInert:
      Result := YesNoToString(Token.Cache.SandboxInert);

    tsHasRestrictions:
      Result := YesNoToString(Token.Cache.HasRestrictions);

    tsIsRestricted:
      Result := YesNoToString(Token.Cache.IsRestricted);

    tsVirtualization:
      if Query(tdTokenVirtualizationEnabled) then
      begin
        if VirtualizationAllowed  then
          Result := EnabledDisabledToString(VirtualizationEnabled)
        else if not VirtualizationEnabled then
          Result := 'Not allowed'
        else
          Result := 'Disallowed & Enabled';
      end;

    tsTokenID:
      Result := LuidToString(Token.Cache.Statistics.TokenId);

    tsExprires:
      Result := NativeTimeToString(Token.Cache.Statistics.ExpirationTime);

    tsDynamicCharged:
      Result := BytesToString(Token.InfoClass.Statistics.DynamicCharged);

    tsDynamicAvailable:
      Result := BytesToString(Token.InfoClass.Statistics.DynamicAvailable);

    tsGroupCount:
      Result := Token.Cache.Statistics.GroupCount.ToString;

    tsPrivilegeCount:
      Result := Token.Cache.Statistics.PrivilegeCount.ToString;

    tsModifiedID:
      Result := LuidToString(Token.Cache.Statistics.ModifiedId);

    tsLogonID:
      Result := LuidToString(Token.Cache.Statistics.AuthenticationId);

    tsSourceLUID:
      Result := LuidToString(Token.Cache.Source.SourceIdentifier);

    tsSourceName:
      Result := TokeSourceNameToString(Token.Cache.Source);

    tsOrigin:
      Result := LuidToString(Token.Cache.Origin);
  end;
  {$ENDREGION}
end;

function TTokenData.ReQuery(DataClass: TTokenDataClass): Boolean;
var
  pIntegrity: PSIDAndAttributes;
  pPrivBuf: PTokenPrivileges;
  pAudit: PTokenAuditPolicy;
  lType: TTokenType;
  lImpersonation: TSecurityImpersonationLevel;
  lLogonInfo: TLogonSessionInfo;
  lObjInfo: TObjectBasicInformaion;
  i, subAuthCount: Integer;
  bufferSize: Cardinal;
begin
  Result := False;

  // TokenSource can't be queried without TOKEN_QUERY_SOURCE access
  if (Token.HandleInformation.GrantedAccess and TOKEN_QUERY_SOURCE = 0) and
    (DataClass = tdTokenSource) then
    Exit;

  // And almost nothing can be queried without TOKEN_QUERY access
  if (Token.HandleInformation.GrantedAccess and TOKEN_QUERY = 0) and
    not (DataClass in [tdNone, tdTokenSource, tdObjectInfo]) then
      Exit;

  case DataClass of
    tdNone:
       Result := True;

    tdTokenUser:
    begin
      Result := Token.QuerySidAndAttributes(TokenUser, Token.Cache.User);

      // The default value of attributes for user is 0 and means "Enabled".
      // In this case we replace it with this flag. However, it can also be
      // "Use for deny only" and we shouldn't replace it in this case.

      if Result and (Token.Cache.User.Attributes = TGroupAttributes(0)) then
        Token.Cache.User.Attributes := GroupExUser;
    end;

    tdTokenGroups:
    begin
      Result := Token.QueryGroups(TokenGroups, Token.Cache.Groups);
      if Result then
        Token.Events.OnGroupsChange.Invoke(Token.Cache.Groups);
    end;

    tdTokenPrivileges:
    begin
      pPrivBuf := Token.QueryVariableSize(TokenPrivileges, Result);
      if Result then
      try
        SetLength(Token.Cache.Privileges, pPrivBuf.PrivilegeCount);
        for i := 0 to pPrivBuf.PrivilegeCount - 1 do
          Token.Cache.Privileges[i] := pPrivBuf.Privileges[i];

        Token.Events.OnPrivilegesChange.Invoke(Token.Cache.Privileges)
      finally
        FreeMem(pPrivBuf);
      end;
    end;

    tdTokenOwner:
    begin
      Result := Token.QuerySid(TokenOwner, Token.Cache.Owner);
      if Result then
        if Token.Events.OnOwnerChange.Invoke(Token.Cache.Owner) then
          InvokeStringEvent(tsOwner);
    end;

    tdTokenPrimaryGroup:
    begin
     Result := Token.QuerySid(TokenPrimaryGroup, Token.Cache.PrimaryGroup);
     if Result then
       if Token.Events.OnPrimaryChange.Invoke(Token.Cache.PrimaryGroup) then
         InvokeStringEvent(tsPrimaryGroup);
    end;

    tdTokenDefaultDacl: ; // Not implemented

    tdTokenSource:
      Result := Token.QueryFixedSize<TTokenSource>(TokenSource,
        Token.Cache.Source);

    tdTokenType:
    begin
      Result := Token.QueryFixedSize<TTokenType>(TokenType, lType);
      if Result then
      begin
        if lType = TokenPrimary then
          Token.Cache.TokenType := ttPrimary
        else
        begin
          Result := Token.QueryFixedSize<TSecurityImpersonationLevel>(
            TokenImpersonationLevel, lImpersonation);
          if Result then
            Token.Cache.TokenType := TTokenTypeEx(lImpersonation);
        end;
      end;
    end;

    tdTokenStatistics:
    begin
      Result := Token.QueryFixedSize<TTokenStatistics>(TokenStatistics,
        Token.Cache.Statistics);

      if Result then
        if Token.Events.OnStatisticsChange.Invoke(Token.Cache.Statistics) then
        begin
          InvokeStringEvent(tsTokenID);
          InvokeStringEvent(tsExprires);
          InvokeStringEvent(tsDynamicCharged);
          InvokeStringEvent(tsDynamicAvailable);
          InvokeStringEvent(tsGroupCount);
          InvokeStringEvent(tsPrivilegeCount);
          InvokeStringEvent(tsModifiedID);
          InvokeStringEvent(tsLogonID);
        end;
    end;

    tdTokenRestrictedSids:
      Result :=  Token.QueryGroups(TokenRestrictedSids,
        Token.Cache.RestrictedSids);

    tdTokenSessionId:
    begin
      Result := Token.QueryFixedSize<Cardinal>(TokenSessionId,
        Token.Cache.Session);
      if Result then
        if Token.Events.OnSessionChange.Invoke(Token.Cache.Session) then
          InvokeStringEvent(tsSession);
    end;

    tdTokenAuditPolicy:
    begin
      pAudit := Token.QueryVariableSize(TokenAuditPolicy, Result, @bufferSize);
      if Result then
      begin
        Token.Cache.AuditPolicy.Free;

        Token.Cache.AuditPolicy := TTokenPerUserAudit.Create;
        Token.Cache.AuditPolicy.Data := pAudit;
        Token.Cache.AuditPolicy.AuditPolicySize := bufferSize;

        Token.Events.OnAuditChange.Invoke(Token.Cache.AuditPolicy);
      end;
    end;

    tdTokenSandBoxInert:
      Result := Token.QueryFixedSize<LongBool>(TokenSandBoxInert,
        Token.Cache.SandboxInert);

    tdTokenOrigin:
    begin
      Result := Token.QueryFixedSize<TLuid>(TokenOrigin,
        Token.Cache.Origin);
      if Result then
        if Token.Events.OnOriginChange.Invoke(Token.Cache.Origin) then
          InvokeStringEvent(tsOrigin);
    end;

    tdTokenElevation:
      Result := Token.QueryFixedSize<TTokenElevationType>(TokenElevationType,
        Token.Cache.Elevation);

    tdTokenHasRestrictions:
      Result := Token.QueryFixedSize<LongBool>(TokenHasRestrictions,
        Token.Cache.HasRestrictions);

    tdTokenVirtualizationAllowed:
    begin
      Result := Token.QueryFixedSize<LongBool>(TokenVirtualizationAllowed,
        Token.Cache.VirtualizationAllowed);
      if Result then
        if Token.Events.OnVirtualizationAllowedChange.Invoke(
          Token.Cache.VirtualizationAllowed) then
          InvokeStringEvent(tsVirtualization);
    end;

    tdTokenVirtualizationEnabled:
    begin
      Result := Token.QueryFixedSize<LongBool>(TokenVirtualizationEnabled,
        Token.Cache.VirtualizationEnabled);
      if Result then
        if Token.Events.OnVirtualizationEnabledChange.Invoke(
          Token.Cache.VirtualizationEnabled) then
          InvokeStringEvent(tsVirtualization);
    end;

    tdTokenIntegrity:
    begin
      pIntegrity := Token.QueryVariableSize(TokenIntegrityLevel, Result);
      if Result then
        with Token.Cache.Integrity do
        try
          Group.SecurityIdentifier.CreateFromSid(pIntegrity.Sid);
          Group.Attributes := TGroupAttributes(pIntegrity.Attributes);

          // Get level value from the last sub-authority
          subAuthCount := RtlSubAuthorityCountSid(pIntegrity.Sid)^;
          if subAuthCount > 0 then
            Level := TTokenIntegrityLevel(RtlSubAuthoritySid(pIntegrity.Sid,
              subAuthCount - 1)^)
          else
            Level := ilUntrusted;

          if Token.Events.OnIntegrityChange.Invoke(Token.Cache.Integrity) then
            InvokeStringEvent(tsIntegrity);
        finally
         FreeMem(pIntegrity);
        end;
    end;

    tdTokenUIAccess:
    begin
      Result := Token.QueryFixedSize<LongBool>(TokenUIAccess,
        Token.Cache.UIAccess);
      if Result then
        if Token.Cache.FOnUIAccessChange.Invoke(Token.Cache.UIAccess) then
          InvokeStringEvent(tsUIAccess);
    end;

    tdTokenMandatoryPolicy:
    begin
      Result := Token.QueryFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy,
        Token.Cache.MandatoryPolicy);
      if Result then
        if Token.Cache.FOnPolicyChange.Invoke(Token.Cache.MandatoryPolicy) then
        begin
          InvokeStringEvent(tsNoWriteUpPolicy);
          InvokeStringEvent(tsNewProcessMinPolicy);
        end;
    end;

    tdTokenIsRestricted:
      Result := Token.QueryFixedSize<LongBool>(TokenIsRestricted,
        Token.Cache.IsRestricted);

    tdLogonInfo:
    if Query(tdTokenStatistics) then
      begin
        lLogonInfo := TLogonSessionInfo.Query(Token.Cache.Statistics.
          AuthenticationId);

        if Assigned(lLogonInfo) then
        begin
          Token.Cache.LogonSessionInfo.Free;
          Token.Cache.LogonSessionInfo := lLogonInfo;
          Result := True;
        end;
      end;

    tdObjectInfo:
    begin
      Result := NT_SUCCESS(NtQueryObject(Token.hToken, ObjectBasicInformation,
        @lObjInfo, SizeOf(lObjInfo), nil));

      if Result then
        Token.Cache.ObjectInformation := lObjInfo;
    end;
  end;

  Token.Cache.IsCached[DataClass] := Token.Cache.IsCached[DataClass] or Result;
end;

procedure TTokenData.SetAuditPolicy(const Value: TTokenPerUserAudit);
begin
  if not SetTokenInformation(Token.hToken, TokenAuditPolicy, Value.Data,
    Value.AuditPolicySize) then
    raise EWinError.Create(GetLastError, SetterMessage(TokenAuditPolicy), Token);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenAuditPolicy);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetIntegrityLevel(const Value: TTokenIntegrityLevel);
const
  SECURITY_MANDATORY_LABEL_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 16));
var
  mandatoryLabel: TSIDAndAttributes;
begin
  // Wee need to prepare the SID for the integrity level.
  // It contains 1 sub authority and looks like S-1-16-X.
  mandatoryLabel.Sid := AllocMem(RtlLengthRequiredSid(1));
  try
    NativeCheck(RtlInitializeSid(mandatoryLabel.Sid,
      SECURITY_MANDATORY_LABEL_AUTHORITY, 1), 'RtlInitializeSid');

    RtlSubAuthoritySid(mandatoryLabel.Sid, 0)^ := Cardinal(Value);
    mandatoryLabel.Attributes := SE_GROUP_INTEGRITY_ENABLED;

    Token.SetFixedSize<TSIDAndAttributes>(TokenIntegrityLevel, mandatoryLabel);
  finally
    FreeMem(mandatoryLabel.Sid);
  end;

  // Update the cache and notify event listeners.
  ValidateCache(tdTokenIntegrity);
  ValidateCache(tdTokenStatistics);

  // Lowering integrity might disable sensitive privileges
  ValidateCache(tdTokenPrivileges);

  // Integrity SID is also stored in the group list. So, update groups too.
  ValidateCache(tdTokenGroups);

  // Sometimes the integrity level SID might be assigned as the token owner.
  // Since the owner is internally stored as an index in the group table,
  // changing it, in this case, also changes the owner.
  if Token.Cache.IsCached[tdTokenOwner] and
    (Token.Cache.Owner.SIDType = SidTypeLabel) then
    ValidateCache(tdTokenOwner);

  // Note: this logic does not apply to the primary group since it is stored
  // as a separate SID, not as a reference.
end;

procedure TTokenData.SetMandatoryPolicy(const Value: TMandatoryPolicy);
begin
  Token.SetFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy, Value);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenMandatoryPolicy);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetOrigin(const Value: TLuid);
begin
  Token.SetFixedSize<TLuid>(TokenOrigin, Value);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenOrigin);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetOwner(const Value: TSecurityIdentifier);
var
  NewOwner: TTokenOwner;
begin
  NewOwner.Owner := Value.AllocSid;
  try
    Token.SetFixedSize<TTokenOwner>(TokenOwner, NewOwner);

    // Update the cache and notify event listeners
    ValidateCache(tdTokenOwner);
    ValidateCache(tdTokenStatistics);
  finally
    LocalFree(NewOwner.Owner);
  end;
end;

procedure TTokenData.SetPrimaryGroup(const Value: TSecurityIdentifier);
var
  NewPrimaryGroup: TTokenPrimaryGroup;
begin
  NewPrimaryGroup.PrimaryGroup := Value.AllocSid;
  try
    Token.SetFixedSize<TTokenPrimaryGroup>(TokenPrimaryGroup, NewPrimaryGroup);

    // Update the cache and notify event listeners
    ValidateCache(tdTokenPrimaryGroup);
    ValidateCache(tdTokenStatistics);
  finally
    LocalFree(NewPrimaryGroup.PrimaryGroup);
  end;
end;

procedure TTokenData.SetSession(const Value: Cardinal);
begin
  Token.SetFixedSize<Cardinal>(TokenSessionId, Value);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenSessionId);

  // Although changing session does not usually change Modified ID it is good to
  // update it
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetUIAccess(const Value: LongBool);
begin
  Token.SetFixedSize<LongBool>(TokenUIAccess, Value);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenUIAccess);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetVirtualizationAllowed(const Value: LongBool);
begin
  Token.SetFixedSize<LongBool>(TokenVirtualizationAllowed, Value);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenVirtualizationAllowed);
  ValidateCache(tdTokenVirtualizationEnabled); // Just to be sure
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetVirtualizationEnabled(const Value: LongBool);
begin
  Token.SetFixedSize<LongBool>(TokenVirtualizationEnabled, Value);

  // Update the cache and notify event listeners
  ValidateCache(tdTokenVirtualizationEnabled);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.ValidateCache(DataClass: TTokenDataClass);
begin
  if Token.Cache.IsCached[DataClass] then
    ReQuery(DataClass);
end;

end.
