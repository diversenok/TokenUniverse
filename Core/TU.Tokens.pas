unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
uses
  Winapi.WinNt, Ntapi.ntobapi, Winapi.WinBase, Winapi.WinSafer,
  TU.Tokens.Types, NtUtils.Objects.Snapshots, DelphiUtils.Events,
  NtUtils.Security.Sid, Ntapi.ntseapi, Winapi.NtSecApi, NtUtils.Lsa.Audit,
  System.Generics.Collections, NtUtils.Exceptions, NtUtils.Lsa.Logon,
  NtUtils.Security.Acl;

type
  /// <summary>
  ///  A class of information for tokens that can be queried and cached.
  /// </summary>
  TTokenDataClass = (tdNone, tdTokenUser, tdTokenGroups, tdTokenPrivileges,
    tdTokenOwner, tdTokenPrimaryGroup, tdTokenDefaultDacl, tdTokenSource,
    tdTokenType, tdTokenStatistics, tdTokenRestrictedSids, tdTokenSessionId,
    tdTokenAuditPolicy, tdTokenSandBoxInert, tdTokenOrigin, tdTokenElevation,
    tdTokenHasRestrictions, tdTokenFlags, tdTokenVirtualizationAllowed,
    tdTokenVirtualizationEnabled, tdTokenIntegrity, tdTokenUIAccess,
    tdTokenMandatoryPolicy, tdTokenIsRestricted, tdLogonInfo, tdObjectInfo);

  /// <summary> A class of string information for tokens. </summary>
  TTokenStringClass = (tsTokenType, tsAccess, tsUserName,
    tsUserState, tsSession, tsElevation, tsIntegrity, tsObjectAddress, tsHandle,
    tsNoWriteUpPolicy, tsNewProcessMinPolicy, tsUIAccess, tsOwner,
    tsPrimaryGroup, tsSandboxInert, tsHasRestrictions, tsFlags, tsIsRestricted,
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
    Groups: TArray<TGroup>;
    Privileges: TArray<TPrivilege>;
    Owner: ISid;
    PrimaryGroup: ISid;
    DefaultDacl: IAcl;
    Source: TTokenSource;
    TokenType: TTokenTypeEx;
    Statistics: TTokenStatistics;
    RestrictedSids: TArray<TGroup>;
    Session: Cardinal;
    AuditPolicy: IPerUserAudit;
    SandboxInert: LongBool;
    Origin: TLuid;
    Elevation: TTokenElevationType;
    HasRestrictions: LongBool;
    Flags: Cardinal;
    VirtualizationAllowed: LongBool;
    VirtualizationEnabled: LongBool;
    Integrity: TGroup;
    UIAccess: LongBool;
    MandatoryPolicy: Cardinal;
    IsRestricted: LongBool;
    LogonSessionInfo: ILogonSession;
    ObjectInformation: TObjectBasicInformaion;

    FOnOwnerChange, FOnPrimaryChange: TCachingEvent<ISid>;
    FOnSessionChange: TCachingEvent<Cardinal>;
    FOnAuditChange: TCachingEvent<IPerUserAudit>;
    FOnOriginChange: TCachingEvent<TLuid>;
    FOnUIAccessChange: TCachingEvent<LongBool>;
    FOnIntegrityChange: TCachingEvent<TGroup>;
    FOnVirtualizationAllowedChange: TCachingEvent<LongBool>;
    FOnVirtualizationEnabledChange: TCachingEvent<LongBool>;
    FOnPolicyChange: TCachingEvent<Cardinal>;
    FOnPrivilegesChange: TCachingEvent<TArray<TPrivilege>>;
    FOnGroupsChange: TCachingEvent<TArray<TGroup>>;
    FOnStatisticsChange: TCachingEvent<TTokenStatistics>;
    FOnDefaultDaclChange: TEvent<IAcl>;
    FOnFlagsChange: TCachingEvent<Cardinal>;
    OnStringDataChange: array [TTokenStringClass] of TEvent<String>;

    ObjectAddress: NativeUInt;
    ReferenceCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property OnOwnerChange: TCachingEvent<ISid> read FOnOwnerChange;
    property OnPrimaryChange: TCachingEvent<ISid> read FOnPrimaryChange;
    property OnSessionChange: TCachingEvent<Cardinal> read FOnSessionChange;
    property OnAuditChange: TCachingEvent<IPerUserAudit> read FOnAuditChange;
    property OnOriginChange: TCachingEvent<TLuid> read FOnOriginChange;
    property OnUIAccessChange: TCachingEvent<LongBool> read FOnUIAccessChange;
    property OnIntegrityChange: TCachingEvent<TGroup> read FOnIntegrityChange;
    property OnVirtualizationAllowedChange: TCachingEvent<LongBool> read FOnVirtualizationAllowedChange;
    property OnVirtualizationEnabledChange: TCachingEvent<LongBool> read FOnVirtualizationEnabledChange;
    property OnPolicyChange: TCachingEvent<Cardinal> read FOnPolicyChange;
    property OnPrivilegesChange: TCachingEvent<TArray<TPrivilege>> read FOnPrivilegesChange;
    property OnGroupsChange: TCachingEvent<TArray<TGroup>> read FOnGroupsChange;
    property OnStatisticsChange: TCachingEvent<TTokenStatistics> read FOnStatisticsChange;
    property OnDefaultDaclChange: TEvent<IAcl> read FOnDefaultDaclChange;
    property OnFlagsChange: TCachingEvent<Cardinal> read FOnFlagsChange;

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
    procedure SetIntegrityLevel(const Value: Cardinal);
    procedure SetMandatoryPolicy(const Value: Cardinal);
    procedure SetSession(const Value: Cardinal);
    procedure SetAuditPolicy(Value: IPerUserAudit);
    procedure SetOrigin(const Value: TLuid);
    procedure SetUIAccess(const Value: LongBool);
    procedure SetOwner(Value: ISid);
    procedure SetPrimaryGroup(Value: ISid);
    function GetVirtualizationAllowed: LongBool;
    function GetVirtualizationEnabled: LongBool;
    function GetElevation: TTokenElevationType;
    function GetGroups: TArray<TGroup>;
    function GetHasRestrictions: LongBool;
    function GetIntegrity: TGroup;
    function GetMandatoryPolicy: Cardinal;
    function GetOrigin: TLuid;
    function GetOwner: ISid;
    function GetPrimaryGroup: ISid;
    function GetPrivileges: TArray<TPrivilege>;
    function GetRestrictedSids: TArray<TGroup>;
    function GetSandboxInert: LongBool;
    function GetSession: Cardinal;
    function GetAuditPolicy: IPerUserAudit;
    function GetSource: TTokenSource;
    function GetStatistics: TTokenStatistics;
    function GetTokenType: TTokenTypeEx;
    function GetUIAccess: LongBool;
    function GetUser: TGroup;
    function GetLogonSessionInfo: ILogonSession;
    function GetIsRestricted: LongBool;
    function GetDefaultDacl: IAcl;
    function GetFlags: Cardinal;
    procedure InvokeStringEvent(StringClass: TTokenStringClass);
    procedure SetVirtualizationAllowed(const Value: LongBool);
    procedure SetVirtualizationEnabled(const Value: LongBool);
    procedure SetDefaultDacl(Value: IAcl);
    function GetObjectInfo: TObjectBasicInformaion;
  public
    property User: TGroup read GetUser;                                         // class 1
    property Groups: TArray<TGroup> read GetGroups;                             // class 2
    property Privileges: TArray<TPrivilege> read GetPrivileges;                 // class 3
    property Owner: ISid read GetOwner write SetOwner;                          // class 4 #settable
    property PrimaryGroup: ISid read GetPrimaryGroup write SetPrimaryGroup;     // class 5 #settable
    property DefaultDacl: IAcl read GetDefaultDacl write SetDefaultDacl;        // class 6 #settable
    property Source: TTokenSource read GetSource;                               // classes 7 & 8
    property TokenTypeInfo: TTokenTypeEx read GetTokenType;                     // class 9
    property Statistics: TTokenStatistics read GetStatistics;                   // class 10
    property RestrictedSids: TArray<TGroup> read GetRestrictedSids;             // class 11
    property Session: Cardinal read GetSession write SetSession;                // class 12 #settable
    // TODO: class 13 TokenGroupsAndPrivileges (maybe use for optimization)
    // TODO: class 14 SessionReference #settable (and not gettable?)
    property SandboxInert: LongBool read GetSandboxInert;                       // class 15
    property AuditPolicy: IPerUserAudit read GetAuditPolicy write SetAuditPolicy;// class 16 #settable
    property Origin: TLuid read GetOrigin write SetOrigin;                      // class 17 #settable
    property Elevation: TTokenElevationType read GetElevation;                  // classes 18 & 20
    // LinkedToken (class 19 #settable) is exported directly by TToken
    property HasRestrictions: LongBool read GetHasRestrictions;                 // class 21
    property Flags: Cardinal read GetFlags;                                     // class 22 AccessInformation
    property VirtualizationAllowed: LongBool read GetVirtualizationAllowed write SetVirtualizationAllowed; // class 23 #settable
    property VirtualizationEnabled: LongBool read GetVirtualizationEnabled write SetVirtualizationEnabled; // class 24 #settable
    property Integrity: TGroup read GetIntegrity;                               // class 25 #settable
    property IntegrityLevel: Cardinal write SetIntegrityLevel;
    property UIAccess: LongBool read GetUIAccess write SetUIAccess;             // class 26 #settable
    property MandatoryPolicy: Cardinal read GetMandatoryPolicy write SetMandatoryPolicy; // class 27 #settable
    // class 28 TokenLogonSid returns 0 or 1 logon sids (even if there are more)
    property IsRestricted: LongBool read GetIsRestricted;                       // class 40
    property LogonSessionInfo: ILogonSession read GetLogonSessionInfo;
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
    FHandleInformation: THandleEntry;
    FInfoClassData: TTokenData;
    Cache: TTokenCacheAndEvents;

    FCaption: String;
    FOnCaptionChange: TCachingEvent<String>;
    FOnCanClose: TEvent<TToken>;
    FOnClose: TEvent<TToken>;
  public

    {--------------------  TToken public section ---------------------------}

    property Handle: THandle read hToken;
    property HandleInformation: THandleEntry read FHandleInformation;

    property InfoClass: TTokenData read FInfoClassData;
    property Events: TTokenCacheAndEvents read Cache;

    property Caption: String read FCaption write SetCaption;
    property OnCaptionChange: TCachingEvent<String> read FOnCaptionChange;

    /// <summary>
    ///  The event is called to test whether the token can be destroyed.
    ///  The listener can deny object destruction by calling
    ///  <see cref="System.SysUtils.EAbort"/>.
    /// </summary>
    property OnCanClose: TEvent<TToken> read FOnCanClose;

    /// <summary>
    ///  Asks all subscribed event listeners if the token can be freed.
    /// </summary>
    /// <exception cref="System.SysUtils.EAbort">
    ///  Can raise <see cref="System.SysUtils.EAbort"/>.
    /// </exception>
    function CanBeFreed: Boolean;

    /// <summary> The event is called on token destruction. </summary>
    /// <remarks> Be aware of exceptions at this point. </remarks>
    property OnClose: TEvent<TToken> read FOnClose;
    destructor Destroy; override;

    procedure PrivilegeAdjust(Privileges: TArray<TPrivilege>;
      Action: TPrivilegeAdjustAction);
    procedure GroupAdjust(Groups: TArray<TGroup>; Action: TGroupAdjustAction);
    function SendHandleToProcess(PID: NativeUInt): NativeUInt;

    /// <summary> Assignes primary token to a process. </summary>
    procedure AssignToProcess(PID: NativeUInt);

    /// <summary> Assigned impersonation token to a thread. </summary>
    procedure AssignToThread(TID: NativeUInt);

    /// <summary>
    ///  Assigned a token to a thread and make sure that the
    ///  privileged part of the impersonation succeeds.
    /// </summary>
    procedure AssignToThreadSafe(TID: NativeUInt);

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
    constructor CreateByHandle(HandleInfo: THandleEntry);

    /// <summary> Opens a token of current process. </summary>
    constructor CreateOpenCurrent(Access: TAccessMask = MAXIMUM_ALLOWED);

    /// <summary> Opens a token of a process. </summary>
    constructor CreateOpenProcess(PID: NativeUInt; ImageName: String;
      Access: TAccessMask = MAXIMUM_ALLOWED; Attributes: Cardinal = 0);

      /// <summary> Opens a token of a thread. </summary>
    constructor CreateOpenThread(TID: NativeUInt; ImageName: String;
      Access: TAccessMask = MAXIMUM_ALLOWED; Attributes: Cardinal = 0;
      Dummy: Integer = 0);

    constructor CreateOpenEffective(TID: NativeUInt; ImageName: String;
      ImpersonationLevel: TSecurityImpersonationLevel = SecurityImpersonation;
      Access: TAccessMask = MAXIMUM_ALLOWED;
      Attributes: Cardinal = 0; EffectiveOnly: Boolean = False);

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
    constructor CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);

    /// <summary> Creates a restricted version of the token. </summary>
    constructor CreateRestricted(SrcToken: TToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TArray<TGroup>;
      PrivilegesToDelete: TArray<TPrivilege>);

    /// <summary> Logons a user with the specified credentials. </summary>
    constructor CreateWithLogon(LogonType: TSecurityLogonType;
      LogonProvider: TLogonProvider; Domain, User: String; Password: PWideChar;
      AddGroups: TArray<TGroup>);

    /// <summary> Logon a user using Services 4 Users. </summary>
    constructor CreateS4ULogon(LogonType: TSecurityLogonType; Domain,
      User: String; const Source: TTokenSource; AddGroups: TArray<TGroup>);

    /// <summary> Creates a new token from the scratch. </summary>
    /// <remarks> This action requires SeCreateTokenPrivilege. </remarks>
    constructor CreateNtCreateToken(User: ISid; DisableUser: Boolean;
      Groups: TArray<TGroup>; Privileges: TArray<TPrivilege>;
      LogonID: TLuid; Owner: ISid; PrimaryGroup: ISid;
      Source: TTokenSource; Expires: TLargeInteger);

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
    function OpenLinkedToken: TNtxStatusWithValue<TToken>;
  end;

{----------------------  End of interface section  ----------------------------}

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntpsapi, NtUtils.Objects,
  NtUtils.Processes, NtUtils.WinStation, NtUtils.Strings, NtUtils.Tokens,
  NtUtils.Tokens.Impersonate, NtUtils.Access, DelphiUtils.Strings,
  System.SysUtils, System.TypInfo, NtUtils.Tokens.Logon, NtUtils.Tokens.Misc,
  NtUtils.WinSafer;

const
  /// <summary> Stores which data class a string class depends on. </summary>
  StringClassToDataClass: array [TTokenStringClass] of TTokenDataClass =
    (tdTokenType, tdNone, tdTokenUser, tdTokenUser, tdTokenSessionId,
    tdTokenElevation, tdTokenIntegrity, tdNone, tdNone, tdTokenMandatoryPolicy,
    tdTokenMandatoryPolicy, tdTokenUIAccess, tdTokenOwner, tdTokenPrimaryGroup,
    tdTokenSandBoxInert, tdTokenHasRestrictions, tdTokenFlags,
    tdTokenIsRestricted, tdTokenVirtualizationAllowed, tdTokenStatistics,
    tdTokenStatistics, tdTokenStatistics, tdTokenStatistics, tdTokenStatistics,
    tdTokenStatistics, tdTokenStatistics, tdTokenStatistics, tdTokenSource,
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
  FOnIntegrityChange.ComparisonFunction := CompareGroups;
  FOnPolicyChange.ComparisonFunction := CompareCardinals;
  FOnPrivilegesChange.ComparisonFunction := ComparePrivileges;
  FOnGroupsChange.ComparisonFunction := CompareGroupArrays;
  FOnStatisticsChange.ComparisonFunction := CompareStatistics;
  FOnVirtualizationAllowedChange.ComparisonFunction := CompareLongBools;
  FOnVirtualizationEnabledChange.ComparisonFunction := CompareLongBools;
  FOnFlagsChange.ComparisonFunction := CompareCardinals;
end;

destructor TTokenCacheAndEvents.Destroy;
var
  i: TTokenStringClass;
begin
  CheckAbandoned(FOnOwnerChange.Count, 'OnOwnerChange');
  CheckAbandoned(FOnPrimaryChange.Count, 'OnPrimaryChange');
  CheckAbandoned(FOnSessionChange.Count, 'OnSessionChange');
  CheckAbandoned(FOnIntegrityChange.Count, 'OnIntegrityChange');
  CheckAbandoned(FOnUIAccessChange.Count, 'OnUIAccessChange');
  CheckAbandoned(FOnPolicyChange.Count, 'OnPolicyChange');
  CheckAbandoned(FOnPrivilegesChange.Count, 'OnPrivilegesChange');
  CheckAbandoned(FOnGroupsChange.Count, 'OnGroupsChange');
  CheckAbandoned(FOnStatisticsChange.Count, 'OnStatisticsChange');
  CheckAbandoned(FOnDefaultDaclChange.Count, 'OnDefaultDaclChange');
  CheckAbandoned(FOnFlagsChange.Count, 'OnFlagsChange');

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
  OnStringDataChange[StringClass].Subscribe(Listener);
end;

procedure TTokenCacheAndEvents.UnSubscribeString(StringClass: TTokenStringClass;
  Listener: TEventListener<String>);
begin
  // Note: tsHandle and tsAccess should be per-handle events and should be
  // stored inside TTokenData. They are currently not supported for invocation.

  OnStringDataChange[StringClass].Unsubscribe(Listener);
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
  Handles: TArray<THandleEntry>;
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
    NtxEnumerateSystemHandles(Handles).RaiseOnError;

    for i := 0 to High(Handles) do
      if (Handles[i].UniqueProcessId = NtCurrentProcessId) and
        (Handles[i].HandleValue = hToken) then
      begin
        FHandleInformation := Handles[i];
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

procedure TToken.AssignToProcess(PID: NativeUInt);
begin
  NtxAssignPrimaryTokenById(PID, hToken).RaiseOnError;

  // Assigning primary token to a process might change token's Session ID
  InfoClass.ValidateCache(tdTokenSessionId);

  // Although changing session does not usually change Modified ID it is good to
  // update it
  InfoClass.ValidateCache(tdTokenStatistics);
end;

procedure TToken.AssignToThread(TID: NativeUInt);
begin
  NtxSetThreadTokenById(TID, hToken).RaiseOnError;
end;

procedure TToken.AssignToThreadSafe(TID: NativeUInt);
begin
  NtxSafeSetThreadTokenById(TID, hToken).RaiseOnError;
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
begin
  NtxOpenAnonymousToken(hToken, Access, HandleAttributes);
  FCaption := 'Anonymous token';
end;

constructor TToken.CreateByHandle(HandleInfo: THandleEntry);
begin
  if HandleInfo.UniqueProcessId <> NtCurrentProcessId then
    raise ENotImplemented.Create('TODO');

  hToken := HandleInfo.HandleValue;
  FHandleInformation := HandleInfo;
  FCaption := Format('Inherited %d [0x%x]', [hToken, hToken]);
end;

constructor TToken.CreateDuplicateHandle(SrcToken: TToken; Access: TAccessMask;
  SameAccess: Boolean; HandleAttributes: Cardinal = 0);
begin
  NtxDuplicateObject(NtCurrentProcess, SrcToken.hToken, NtCurrentProcess,
    hToken, Access, HandleAttributes, 0).RaiseOnError;

  FCaption := SrcToken.Caption + ' (ref)'
  // TODO: No need to snapshot handles, object address is already known
end;

constructor TToken.CreateDuplicateToken(SrcToken: TToken; Access: TAccessMask;
  TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);
var
  TokenType: TTokenType;
  ImpersonationLvl: TSecurityImpersonationLevel;
begin
  if TokenTypeEx = ttPrimary then
  begin
    TokenType := TokenPrimary;
    ImpersonationLvl := SecurityImpersonation;
  end
  else
  begin
    TokenType := TokenImpersonation;
    ImpersonationLvl := TSecurityImpersonationLevel(TokenTypeEx);
  end;

  NtxDuplicateToken(hToken, SrcToken.hToken, Access, TokenType,
    ImpersonationLvl, 0, EffectiveOnly).RaiseOnError;

  if EffectiveOnly then
    FCaption := SrcToken.Caption + ' (eff. copy)'
  else
    FCaption := SrcToken.Caption + ' (copy)'
end;

constructor TToken.CreateNtCreateToken(User: ISid; DisableUser: Boolean;
  Groups: TArray<TGroup>; Privileges: TArray<TPrivilege>; LogonID: TLuid;
  Owner: ISid; PrimaryGroup: ISid; Source: TTokenSource;
  Expires: TLargeInteger);
var
  TokenUser: TGroup;
begin
  TokenUser.SecurityIdentifier := User;

  // Fill user attributes. Zero value is default here and means "Enabled"
  if DisableUser then
    TokenUser.Attributes := SE_GROUP_USE_FOR_DENY_ONLY
  else
    TokenUser.Attributes := 0;

  NtxCreateToken(hToken, TokenPrimary, SecurityImpersonation, LogonID,
    Expires, TokenUser, Groups, Privileges, Owner, PrimaryGroup, nil,
    Source).RaiseOnError;

  FCaption := 'New token: ';
  if User.UserName <> '' then
    FCaption := FCaption + User.UserName
  else if User.DomainName <> '' then
    FCaption := FCaption + User.DomainName
  else
    FCaption := FCaption + User.SDDL;
end;

constructor TToken.CreateOpenCurrent(Access: TAccessMask);
begin
  CreateOpenProcess(NtCurrentProcessId, 'Current process');
end;

constructor TToken.CreateOpenEffective(TID: NativeUInt; ImageName: String;
  ImpersonationLevel: TSecurityImpersonationLevel; Access: TAccessMask;
  Attributes: Cardinal; EffectiveOnly: Boolean);
begin
  NtxOpenEffectiveTokenById(hToken, TID, ImpersonationLevel, Access,
    Attributes, EffectiveOnly).RaiseOnError;

  FCaption := Format('Eff. thread %d of %s', [TID, ImageName]);
  if EffectiveOnly then
    FCaption := FCaption + ' (eff.)';
end;

constructor TToken.CreateOpenProcess(PID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal);
begin
  NtxOpenProcessTokenById(hToken, PID, Access, Attributes).RaiseOnError;
  FCaption := Format('%s [%d]', [ImageName, PID]);
end;

constructor TToken.CreateOpenThread(TID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal; Dummy: Integer);
begin
  NtxOpenThreadTokenById(hToken, TID, Access, Attributes).RaiseOnError;
  FCaption := Format('Thread %d of %s', [TID, ImageName]);
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);
begin
  WsxQueryToken(hToken, SessionID).RaiseOnError;
  FCaption := Format('Session %d token', [SessionID]);
end;

constructor TToken.CreateRestricted(SrcToken: TToken; Flags: Cardinal;
  SIDsToDisabe, SIDsToRestrict: TArray<TGroup>;
  PrivilegesToDelete: TArray<TPrivilege>);
var
  Disable, Restrict: TArray<ISid>;
  Remove: TArray<TLuid>;
  i: Integer;
begin
  SetLength(Disable, Length(SIDsToDisabe));
  for i := 0 to High(SIDsToDisabe) do
    Disable[i] := SIDsToDisabe[i].SecurityIdentifier;

  SetLength(Restrict, Length(SIDsToRestrict));
  for i := 0 to High(SIDsToRestrict) do
    Restrict[i] := SIDsToRestrict[i].SecurityIdentifier;

  SetLength(Remove, Length(PrivilegesToDelete));
  for i := 0 to High(PrivilegesToDelete) do
    Remove[i] := PrivilegesToDelete[i].Luid;

  NtxFilterToken(hToken, SrcToken.hToken, Flags, Disable, Remove,
    Restrict).RaiseOnError;

  FCaption := 'Restricted ' + SrcToken.Caption;
end;

constructor TToken.CreateS4ULogon(LogonType: TSecurityLogonType; Domain,
  User: String; const Source: TTokenSource; AddGroups: TArray<TGroup>);
begin
  NtxLogonS4U(hToken, Domain, User, LogonType, Source, AddGroups).RaiseOnError;

  FCaption := 'S4U logon of ' + User;
end;

constructor TToken.CreateSaferToken(SrcToken: TToken; ScopeId: TSaferScopeId;
  LevelId: TSaferLevelId; MakeInert: Boolean = False);
var
  LevelName: String;
begin
  SafexComputeSaferTokenById(hToken, SrcToken.hToken, ScopeId, LevelId,
    MakeInert).RaiseOnError;

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

  FCaption := LevelName + ' Safer for ' + SrcToken.FCaption
end;

constructor TToken.CreateWithLogon(LogonType: TSecurityLogonType;
  LogonProvider: TLogonProvider; Domain, User: String; Password: PWideChar;
  AddGroups: TArray<TGroup>);
begin
  NtxLogonUser(hToken, Domain, User, Password, LogonType, LogonProvider,
    AddGroups).RaiseOnError;

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
    NtxSafeClose(hToken);

  CheckAbandoned(FOnCanClose.Count, 'OnCanClose');
  CheckAbandoned(FOnClose.Count, 'OnClose');
  CheckAbandoned(FOnCaptionChange.Count, 'OnCaptionChange');

  inherited;
end;

procedure TToken.GroupAdjust(Groups: TArray<TGroup>; Action:
  TGroupAdjustAction);
const
  ActionToAttribute: array [TGroupAdjustAction] of Cardinal = (0,
    SE_GROUP_ENABLED, 0);
var
  i: integer;
  Sids: TArray<ISid>;
begin
  SetLength(Sids, Length(Groups));
  for i := 0 to High(Groups) do
    Sids[i] := Groups[i].SecurityIdentifier;

  NtxAdjustGroups(hToken, Sids, ActionToAttribute[Action],
    Action = gaResetDefault).RaiseOnError;

  // Update the cache and notify event listeners
  InfoClass.ValidateCache(tdTokenGroups);
  InfoClass.ValidateCache(tdTokenStatistics);

  // Adjusting groups may change integrity attributes
  InfoClass.ValidateCache(tdTokenIntegrity);
end;

function TToken.OpenLinkedToken: TNtxStatusWithValue<TToken>;
var
  Handle: THandle;
begin
  Result.Status := NtxToken.Query<THandle>(hToken, TokenLinkedToken, Handle);

  if Result.Status.IsSuccess then
    Result.Value := TToken.Create(Handle, 'Linked token for ' + Caption);
end;

procedure TToken.PrivilegeAdjust(Privileges: TArray<TPrivilege>;
  Action: TPrivilegeAdjustAction);
const
  ActionToAttribute: array [TPrivilegeAdjustAction] of Cardinal =
    (SE_PRIVILEGE_ENABLED, 0, SE_PRIVILEGE_REMOVED);
var
  Status: TNtxStatus;
  LuidArray: TArray<TLuid>;
  i: Integer;
begin
  SetLength(LuidArray, Length(Privileges));
  for i := 0 to High(Privileges) do
    LuidArray[i] := Privileges[i].Luid;

  Status := NtxAdjustPrivileges(hToken, LuidArray, ActionToAttribute[Action]);

  // The function could modify privileges even without succeeding.
  // Update the cache and notify event listeners.
  InfoClass.ValidateCache(tdTokenPrivileges);
  InfoClass.ValidateCache(tdTokenStatistics);
  InfoClass.ValidateCache(tdTokenFlags);

  // Note: the system call might return STATUS_NOT_ALL_ASSIGNED which is
  // not considered as an error. Such behavior does not fit into our
  // model so we should overwrite it.
  if Status.Status = STATUS_NOT_ALL_ASSIGNED then
    raise ENtError.Create(Status.Status, Status.Location)
  else
    Status.RaiseOnError;
end;

class procedure TToken.RevertThreadToken(TID: NativeUInt);
begin
  NtxSetThreadTokenById(TID, 0).RaiseOnError;
end;

function TToken.SendHandleToProcess(PID: NativeUInt): NativeUInt;
var
  hTargetProcess: THandle;
begin
  NtxOpenProcess(hTargetProcess, PID, PROCESS_DUP_HANDLE).RaiseOnError;

  try
    // Send the handle
    NtxCheck(NtDuplicateObject(NtCurrentProcess, hToken, hTargetProcess,
      Result, 0, 0, DUPLICATE_SAME_ACCESS or DUPLICATE_SAME_ATTRIBUTES),
      'NtDuplicateObject');
  finally
    NtxSafeClose(hTargetProcess);
  end;
end;

procedure TToken.SetCaption(const Value: String);
begin
  FCaption := Value;
  OnCaptionChange.Invoke(FCaption);
end;

{ TTokenData }

function TTokenData.GetAuditPolicy: IPerUserAudit;
begin
  Assert(Token.Cache.IsCached[tdTokenAuditPolicy]);
  Result := Token.Cache.AuditPolicy;
end;

function TTokenData.GetDefaultDacl: IAcl;
begin
  Assert(Token.Cache.IsCached[tdTokenDefaultDacl]);
  Result := Token.Cache.DefaultDacl;
end;

function TTokenData.GetElevation: TTokenElevationType;
begin
  Assert(Token.Cache.IsCached[tdTokenElevation]);
  Result := Token.Cache.Elevation;
end;

function TTokenData.GetFlags: Cardinal;
begin
  Assert(Token.Cache.IsCached[tdTokenFlags]);
  Result := Token.Cache.Flags;
end;

function TTokenData.GetGroups: TArray<TGroup>;
begin
  Assert(Token.Cache.IsCached[tdTokenGroups]);
  Result := Token.Cache.Groups;
end;

function TTokenData.GetHasRestrictions: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenHasRestrictions]);
  Result := Token.Cache.HasRestrictions;
end;

function TTokenData.GetIntegrity: TGroup;
begin
  Assert(Token.Cache.IsCached[tdTokenIntegrity]);
  Result := Token.Cache.Integrity;
end;

function TTokenData.GetIsRestricted: LongBool;
begin
  Assert(Token.Cache.IsCached[tdTokenIsRestricted]);
  Result := Token.Cache.IsRestricted;
end;

function TTokenData.GetLogonSessionInfo: ILogonSession;
begin
  Assert(Token.Cache.IsCached[tdLogonInfo]);
  Result := Token.Cache.LogonSessionInfo;
end;

function TTokenData.GetMandatoryPolicy: Cardinal;
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

function TTokenData.GetOwner: ISid;
begin
  Assert(Token.Cache.IsCached[tdTokenOwner]);
  Result := Token.Cache.Owner;
end;

function TTokenData.GetPrimaryGroup: ISid;
begin
  Assert(Token.Cache.IsCached[tdTokenPrimaryGroup]);
  Result := Token.Cache.PrimaryGroup;
end;

function TTokenData.GetPrivileges: TArray<TPrivilege>;
begin
  Assert(Token.Cache.IsCached[tdTokenPrivileges]);
  Result := Token.Cache.Privileges;
end;

function TTokenData.GetRestrictedSids: TArray<TGroup>;
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
        Result := FormatAccessPrefixed(Token.HandleInformation.GrantedAccess,
          objNtToken)
      else
        Result := FormatAccess(Token.HandleInformation.GrantedAccess,
          objNtToken);

    tsUserName:
      Result := Token.Cache.User.SecurityIdentifier.AsString;

    tsUserState:
      Result := Token.Cache.User.Attributes.ToString;

    tsSession: // Detailed?
      Result := Token.Cache.Session.ToString;

    tsElevation:
      Result := ElevationToString(Token.Cache.Elevation);

    tsIntegrity:
      Result := IntegrityToString(Token.Cache.Integrity.SecurityIdentifier.Rid);

    tsObjectAddress:
      Result := IntToHexEx(UInt64(Token.HandleInformation.PObject));

    // Note: this is a per-handle value. Beware of per-kernel-object events.
    tsHandle:
      if Detailed then
        Result := Format('0x%x (%d)', [Token.Handle, Token.Handle])
      else
        Result := IntToHexEx(Token.Handle);

    tsNoWriteUpPolicy:
      Result := EnabledDisabledToString(Contains(Token.Cache.MandatoryPolicy,
        TOKEN_MANDATORY_POLICY_NO_WRITE_UP));

    tsNewProcessMinPolicy:
      Result := EnabledDisabledToString(Contains(Token.Cache.MandatoryPolicy,
        TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN));

    tsUIAccess:
      Result := EnabledDisabledToString(Token.Cache.UIAccess);

    tsOwner:
      Result := Token.Cache.Owner.AsString;

    tsPrimaryGroup:
      Result := Token.Cache.PrimaryGroup.AsString;

    tsSandboxInert:
      Result := YesNoToString(Token.Cache.SandboxInert);

    tsHasRestrictions:
      Result := YesNoToString(Token.Cache.HasRestrictions);

    tsFlags:
      Result := MapFlags(Token.Cache.Flags, TokenFlagsNames);

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
      Result := IntToHexEx(Token.Cache.Statistics.TokenId);

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
      Result := IntToHexEx(Token.Cache.Statistics.ModifiedId);

    tsLogonID:
      Result := IntToHexEx(Token.Cache.Statistics.AuthenticationId);

    tsSourceLUID:
      Result := IntToHexEx(Token.Cache.Source.SourceIdentifier);

    tsSourceName:
      Result := Token.Cache.Source.ToString;

    tsOrigin:
      Result := IntToHexEx(Token.Cache.Origin);
  end;
  {$ENDREGION}
end;

function TTokenData.ReQuery(DataClass: TTokenDataClass): Boolean;
var
  pAudit: PTokenAuditPolicy;
  lType: TTokenType;
  lImpersonation: TSecurityImpersonationLevel;
  bufferSize: Cardinal;
  Status: TNtxStatus;
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
      Result := NtxQueryGroupToken(Token.hToken, TokenUser,
        Token.Cache.User).IsSuccess;

      // The default value of attributes for user is 0 and means "Enabled".
      // In this case we replace it with this flag. However, it can also be
      // "Use for deny only" and we shouldn't replace it in this case.

      if Result and (Token.Cache.User.Attributes = 0) then
        Token.Cache.User.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or
          SE_GROUP_ENABLED;
    end;

    tdTokenGroups:
    begin
      Result := NtxQueryGroupsToken(Token.hToken, TokenGroups,
        Token.Cache.Groups).IsSuccess;

      if Result then
        Token.Events.OnGroupsChange.Invoke(Token.Cache.Groups);
    end;

    tdTokenPrivileges:
    begin
      Result := NtxQueryPrivilegesToken(Token.hToken,
        Token.Cache.Privileges).IsSuccess;

      if Result then
        Token.Events.OnPrivilegesChange.Invoke(Token.Cache.Privileges);
    end;

    tdTokenOwner:
    begin
      Result := NtxQuerySidToken(Token.hToken, TokenOwner,
        Token.Cache.Owner).IsSuccess;

      if Result then
        if Token.Events.OnOwnerChange.Invoke(Token.Cache.Owner) then
          InvokeStringEvent(tsOwner);
    end;

    tdTokenPrimaryGroup:
    begin
     Result := NtxQuerySidToken(Token.hToken, TokenPrimaryGroup,
        Token.Cache.PrimaryGroup).IsSuccess;

     if Result then
       if Token.Events.OnPrimaryChange.Invoke(Token.Cache.PrimaryGroup) then
         InvokeStringEvent(tsPrimaryGroup);
    end;

    tdTokenDefaultDacl:
    begin
      Result := NtxQueryDefaultDaclToken(Token.hToken,
        Token.Cache.DefaultDacl).IsSuccess;

      if Result then
        Token.Events.OnDefaultDaclChange.Invoke(Token.Cache.DefaultDacl);
    end;

    tdTokenSource:
      Result := NtxToken.Query<TTokenSource>(Token.hToken, TokenSource,
        Token.Cache.Source).IsSuccess;

    tdTokenType:
    begin
      Result := NtxToken.Query<TTokenType>(Token.hToken, TokenType,
        lType).IsSuccess;
      if Result then
      begin
        if lType = TokenPrimary then
          Token.Cache.TokenType := ttPrimary
        else
        begin
          Result := NtxToken.Query<TSecurityImpersonationLevel>(Token.hToken,
            TokenImpersonationLevel, lImpersonation).IsSuccess;
          if Result then
            Token.Cache.TokenType := TTokenTypeEx(lImpersonation);
        end;
      end;
    end;

    tdTokenStatistics:
    begin
      Result := NtxToken.Query<TTokenStatistics>(Token.hToken, TokenStatistics,
        Token.Cache.Statistics).IsSuccess;

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
      Result := NtxQueryGroupsToken(Token.hToken, TokenRestrictedSids,
        Token.Cache.RestrictedSids).IsSuccess;

    tdTokenSessionId:
    begin
      Result := NtxToken.Query<Cardinal>(Token.hToken, TokenSessionId,
        Token.Cache.Session).IsSuccess;
      if Result then
        if Token.Events.OnSessionChange.Invoke(Token.Cache.Session) then
          InvokeStringEvent(tsSession);
    end;

    tdTokenAuditPolicy:
    begin
      pAudit := NtxQueryBufferToken(Token.hToken, TokenAuditPolicy, Status,
        @bufferSize);
      Result := Status.IsSuccess;
      if Result then
      begin
        Token.Cache.AuditPolicy := TTokenPerUserAudit.CreateCopy(pAudit,
          bufferSize);

        FreeMem(pAudit);
        Token.Events.OnAuditChange.Invoke(Token.Cache.AuditPolicy);
      end;
    end;

    tdTokenSandBoxInert:
      Result := NtxToken.Query<LongBool>(Token.hToken, TokenSandBoxInert,
        Token.Cache.SandboxInert).IsSuccess;

    tdTokenOrigin:
    begin
      Result := NtxToken.Query<TLuid>(Token.hToken, TokenOrigin,
        Token.Cache.Origin).IsSuccess;
      if Result then
        if Token.Events.OnOriginChange.Invoke(Token.Cache.Origin) then
          InvokeStringEvent(tsOrigin);
    end;

    tdTokenElevation:
      Result := NtxToken.Query<TTokenElevationType>(Token.hToken,
        TokenElevationType, Token.Cache.Elevation).IsSuccess;

    tdTokenHasRestrictions:
      Result := NtxToken.Query<LongBool>(Token.hToken, TokenHasRestrictions,
        Token.Cache.HasRestrictions).IsSuccess;

    tdTokenFlags:
    begin
      Result := NtxQueryFlagsToken(Token.hToken, Token.Cache.Flags).IsSuccess;

      if Result then
        if Token.Events.OnFlagsChange.Invoke(Token.Cache.Flags) then
          InvokeStringEvent(tsFlags);
    end;

    tdTokenVirtualizationAllowed:
    begin
      Result := NtxToken.Query<LongBool>(Token.hToken,
        TokenVirtualizationAllowed, Token.Cache.VirtualizationAllowed).IsSuccess;
      if Result then
        if Token.Events.OnVirtualizationAllowedChange.Invoke(
          Token.Cache.VirtualizationAllowed) then
          InvokeStringEvent(tsVirtualization);
    end;

    tdTokenVirtualizationEnabled:
    begin
      Result := NtxToken.Query<LongBool>(Token.hToken,
        TokenVirtualizationEnabled, Token.Cache.VirtualizationEnabled).IsSuccess;
      if Result then
        if Token.Events.OnVirtualizationEnabledChange.Invoke(
          Token.Cache.VirtualizationEnabled) then
          InvokeStringEvent(tsVirtualization);
    end;

    tdTokenIntegrity:
    begin
      Result := NtxQueryGroupToken(Token.hToken, TokenIntegrityLevel,
        Token.Cache.Integrity).IsSuccess;

      if Result and
        Token.Events.OnIntegrityChange.Invoke(Token.Cache.Integrity) then
          InvokeStringEvent(tsIntegrity);
    end;

    tdTokenUIAccess:
    begin
      Result := NtxToken.Query<LongBool>(Token.hToken, TokenUIAccess,
        Token.Cache.UIAccess).IsSuccess;
      if Result then
        if Token.Cache.FOnUIAccessChange.Invoke(Token.Cache.UIAccess) then
          InvokeStringEvent(tsUIAccess);
    end;

    tdTokenMandatoryPolicy:
    begin
      Result := NtxToken.Query<Cardinal>(Token.hToken, TokenMandatoryPolicy,
        Token.Cache.MandatoryPolicy).IsSuccess;
      if Result then
        if Token.Cache.FOnPolicyChange.Invoke(Token.Cache.MandatoryPolicy) then
        begin
          InvokeStringEvent(tsNoWriteUpPolicy);
          InvokeStringEvent(tsNewProcessMinPolicy);
        end;
    end;

    tdTokenIsRestricted:
      Result := NtxToken.Query<LongBool>(Token.hToken, TokenIsRestricted,
        Token.Cache.IsRestricted).IsSuccess;

    tdLogonInfo:
    if Query(tdTokenStatistics) then
    begin
      Result := True;
      // This function always returns data
      LsaxQueryLogonSession(Token.Cache.Statistics.AuthenticationId,
        Token.Cache.LogonSessionInfo);
    end;

    tdObjectInfo:
      Result := NtxQueryBasicInfoObject(Token.hToken,
        Token.Cache.ObjectInformation).IsSuccess;
  end;

  Token.Cache.IsCached[DataClass] := Token.Cache.IsCached[DataClass] or Result;
end;

procedure TTokenData.SetAuditPolicy(Value: IPerUserAudit);
var
  pAuditPolicy: PTokenAuditPolicy;
begin
  pAuditPolicy := Value.RawBuffer;

  try
    NtxSetInformationToken(Token.hToken, TokenAuditPolicy, pAuditPolicy,
      Value.RawBufferSize).RaiseOnError;
  finally
    Value.FreeRawBuffer(pAuditPolicy);
  end;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenAuditPolicy);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetDefaultDacl(Value: IAcl);
begin
  NtxSetDefaultDaclToken(Token.hToken, Value).RaiseOnError;
  ValidateCache(tdTokenDefaultDacl);
end;

procedure TTokenData.SetIntegrityLevel(const Value: Cardinal);
begin
  NtxSetIntegrityToken(Token.hToken, Value).RaiseOnError;

  // Update the cache and notify event listeners.
  ValidateCache(tdTokenIntegrity);
  ValidateCache(tdTokenStatistics);
  ValidateCache(tdTokenFlags);

  // Lowering integrity might disable sensitive privileges
  ValidateCache(tdTokenPrivileges);

  // Integrity SID is also stored in the group list. So, update groups too.
  ValidateCache(tdTokenGroups);

  // Sometimes the integrity level SID might be assigned as the token owner.
  // Since the owner is internally stored as an index in the group table,
  // changing it, in this case, also changes the owner.
  if Token.Cache.IsCached[tdTokenOwner] and
    (Token.Cache.Owner.IdentifyerAuthority.ToInt64 =
      SECURITY_MANDATORY_LABEL_AUTHORITY_ID) then
    ValidateCache(tdTokenOwner);

  // Note: this logic does not apply to the primary group since it is stored
  // as a separate SID, not as a reference.
end;

procedure TTokenData.SetMandatoryPolicy(const Value: Cardinal);
begin
  NtxToken.SetInfo<Cardinal>(Token.hToken, TokenMandatoryPolicy,
    Value).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenMandatoryPolicy);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetOrigin(const Value: TLuid);
begin
  NtxToken.SetInfo<TLuid>(Token.hToken, TokenOrigin, Value).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenOrigin);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetOwner(Value: ISid);
var
  NewOwner: TTokenOwner;
begin
  NewOwner.Owner := Value.Sid;
  NtxToken.SetInfo<TTokenOwner>(Token.hToken, TokenOwner,NewOwner).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenOwner);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetPrimaryGroup(Value: ISid);
var
  NewPrimaryGroup: TTokenPrimaryGroup;
begin
  NewPrimaryGroup.PrimaryGroup := Value.Sid;
  NtxToken.SetInfo<TTokenPrimaryGroup>(Token.hToken, TokenPrimaryGroup,
    NewPrimaryGroup).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenPrimaryGroup);
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetSession(const Value: Cardinal);
begin
  NtxToken.SetInfo<Cardinal>(Token.hToken, TokenSessionId, Value).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenSessionId);

  // Although changing session does not usually change Modified ID it is good to
  // update it
  ValidateCache(tdTokenStatistics);
end;

procedure TTokenData.SetUIAccess(const Value: LongBool);
begin
  NtxToken.SetInfo<LongBool>(Token.hToken, TokenUIAccess, Value).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenUIAccess);
  ValidateCache(tdTokenStatistics);
  ValidateCache(tdTokenFlags);
end;

procedure TTokenData.SetVirtualizationAllowed(const Value: LongBool);
begin
  NtxToken.SetInfo<LongBool>(Token.hToken, TokenVirtualizationAllowed,
    Value).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenVirtualizationAllowed);
  ValidateCache(tdTokenVirtualizationEnabled); // Just to be sure
  ValidateCache(tdTokenStatistics);
  ValidateCache(tdTokenFlags);
end;

procedure TTokenData.SetVirtualizationEnabled(const Value: LongBool);
begin
  NtxToken.SetInfo<LongBool>(Token.hToken, TokenVirtualizationEnabled,
    Value).RaiseOnError;

  // Update the cache and notify event listeners
  ValidateCache(tdTokenVirtualizationEnabled);
  ValidateCache(tdTokenStatistics);
  ValidateCache(tdTokenFlags);
end;

procedure TTokenData.ValidateCache(DataClass: TTokenDataClass);
begin
  if Token.Cache.IsCached[DataClass] then
    ReQuery(DataClass);
end;

end.
