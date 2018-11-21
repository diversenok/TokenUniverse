unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
uses
  System.SysUtils, Winapi.Windows, System.Generics.Collections,
  TU.Tokens.Winapi, TU.Tokens.Types, TU.Handles, TU.Common, TU.LsaApi;

type
  /// <summary>
  ///  A class of information for tokens that can be queried and cached.
  /// </summary>
  TTokenDataClass = (tdNone, tdTokenUser, tdTokenGroups, tdTokenPrivileges,
    tdTokenOwner, tdTokenPrimaryGroup, tdTokenDefaultDacl, tdTokenSource,
    tdTokenType, tdTokenStatistics, tdTokenRestrictedSids, tdTokenSessionId,
    tdTokenSandBoxInert, tdTokenOrigin, tdTokenElevation,
    tdTokenHasRestrictions, tdTokenVirtualization, tdTokenIntegrity,
    tdTokenUIAccess, tdTokenMandatoryPolicy, tdLogonInfo);

  /// <summary> A class of string information for tokens. </summary>
  TTokenStringClass = (tsTokenType, tsAccess, tsUserName,
    tsUserState, tsSession, tsElevation, tsIntegrity, tsObjectAddress, tsHandle,
    tsNoWriteUpPolicy, tsNewProcessMinPolicy, tsUIAccess, tsOwner,
    tsPrimaryGroup, tsSandboxInert, tsHasRestrictions, tsTokenID, tsExprires,
    tsDynamicCharged, tsDynamicAvailable, tsGroupCount, tsPrivilegeCount,
    tsModifiedID, tsLogonID, tsLogonAuthPackage, tsLogonServer,
    tsLogonWtsSession, tsLogonTime, tsLogonType, tsLogonUserName, tsSourceLUID,
    tsSourceName, tsOrigin);

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
    SandboxInert: LongBool;
    Origin: LUID;
    Elevation: TTokenElevationType;
    HasRestrictions: LongBool;
    Integrity: TTokenIntegrity;
    UIAccess: LongBool;
    MandatoryPolicy: TMandatoryPolicy;
    LogonSessionInfo: TLogonSessionInfo;

    FOnSessionChange: TValuedEventHandler<Cardinal>;
    FOnUIAccessChange: TValuedEventHandler<LongBool>;
    FOnIntegrityChange: TValuedEventHandler<TTokenIntegrity>;
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

    property OnSessionChange: TValuedEventHandler<Cardinal> read FOnSessionChange;
    property OnUIAccessChange: TValuedEventHandler<LongBool> read FOnUIAccessChange;
    property OnIntegrityChange: TValuedEventHandler<TTokenIntegrity> read FOnIntegrityChange;
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
    procedure SetUIAccess(const Value: LongBool);
    function GetElevation: TTokenElevationType;
    function GetGroups: TGroupArray;
    function GetHasRestrictions: LongBool;
    function GetIntegrity: TTokenIntegrity;
    function GetMandatoryPolicy: TMandatoryPolicy;
    function GetOrigin: LUID;
    function GetOwner: TSecurityIdentifier;
    function GetPrimaryGroup: TSecurityIdentifier;
    function GetPrivileges: TPrivilegeArray;
    function GetRestrictedSids: TGroupArray;
    function GetSandboxInert: LongBool;
    function GetSession: Cardinal;
    function GetSource: TTokenSource;
    function GetStatistics: TTokenStatistics;
    function GetTokenType: TTokenTypeEx;
    function GetUIAccess: LongBool;
    function GetUser: TGroup;
    function GetLogonSessionInfo: TLogonSessionInfo;
    procedure InvokeStringEvent(StringClass: TTokenStringClass);
  public
    property User: TGroup read GetUser;                                         // class 1
    property Groups: TGroupArray read GetGroups;                                // class 2
    property Privileges: TPrivilegeArray read GetPrivileges;                    // class 3
    property Owner: TSecurityIdentifier read GetOwner;                          // class 4 #settable
    property PrimaryGroup: TSecurityIdentifier read GetPrimaryGroup;            // class 5 #settable
    // TODO: class 6: DefaultDacl #settable
    property Source: TTokenSource read GetSource;                               // classes 7 & 8
    property TokenTypeInfo: TTokenTypeEx read GetTokenType;                     // class 9
    property Statistics: TTokenStatistics read GetStatistics;                   // class 10
    property RestrictedSids: TGroupArray read GetRestrictedSids;                // class 11
    property Session: Cardinal read GetSession write SetSession;                // class 12 #settable
    // TODO: class 13 TokenGroupsAndPrivileges (maybe use for optimization)
    // TODO: class 14 SessionReference #settable (and not gettable?)
    property SandboxInert: LongBool read GetSandboxInert;                       // class 15
    // TODO -cEnhancement: class 16 TokenAuditPolicy #settable
    property Origin: LUID read GetOrigin;                                       // class 17 #settable
    property Elevation: TTokenElevationType read GetElevation;                  // classes 18 & 20
    // LinkedToken (class 19 #settable) is exported directly by TToken
    property HasRestrictions: LongBool read GetHasRestrictions;                 // class 21
    // TODO: class 22 AccessInformation (depends on OS version, duplicates most of the info)
    // TODO: class 23 & 24 Virtualization #settable (both)
    property Integrity: TTokenIntegrity read GetIntegrity;                      // class 25 #settable
    property IntegrityLevel: TTokenIntegrityLevel write SetIntegrityLevel;
    property UIAccess: LongBool read GetUIAccess write SetUIAccess;             // class 26 #settable
    property MandatoryPolicy: TMandatoryPolicy read GetMandatoryPolicy write SetMandatoryPolicy;    // class 27 #settable
    property LogonSessionInfo: TLogonSessionInfo read GetLogonSessionInfo;

    /// <summary>
    ///  Ensure that the required value is in the cache and retrieves it if
    ///  necessary.
    /// </summary>
    function Query(DataClass: TTokenDataClass): Boolean;

    /// <summary>
    ///  Forcibly update the cache by retrieving it regardless of the cache
    ///  state.
    /// </summary>
    function ReQuery(DataClass: TTokenDataClass): Boolean;

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
    FHandleInformation: THandleInformation;
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
      out Status: Boolean): Pointer;

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
    ///  Converts an array of groups from our
    ///  <see cref="TU.Tokens.Types.TSecurityIdentifier"/> format to Winapi's
    ///  <see cref="Winapi.PSID"/> array.
    /// </summary>
    /// <remarks>
    ///  The memory of each item should be freed by calling
    ///  <see cref="FreeSidArray"/>.
    /// </remarks>
    class function ConvertGroupArrayToSIDs(Groups: TGroupArray):
      TSIDAndAttributesArray; static;

    /// <summary>
    ///  Frees memory previously allocated by
    ///  <see cref="ConvertGroupArrayToSIDs"/>.
    /// </summary>
    class procedure FreeSidArray(SIDs: TSIDAndAttributesArray); static;
  public

    {--------------------  TToken public section ---------------------------}

    property Handle: THandle read hToken;
    property HandleInformation: THandleInformation read FHandleInformation;

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

    procedure PrivilegeAdjust(PrivilegeArray: TPrivilegeLUIDArray;
      Action: TPrivilegeAdjustAction);
    procedure GroupAdjust(GroupArray: TGroupArray; Action: TGroupAdjustAction);
    function SendHandleToProcess(PID: Cardinal): NativeUInt;
    procedure AssignToProcess(PID: Cardinal);
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
    constructor CreateByHandle(HandleInfo: THandleInformation);

    /// <summary> Opens a token of current process. </summary>
    constructor CreateOpenCurrent;

    /// <summary> Opens a token of another process. </summary>
    constructor CreateOpenProcess(PID: Cardinal);

    /// <summary> Duplicates a token. </summary>
    constructor CreateDuplicateToken(SrcToken: TToken; Access: ACCESS_MASK;
      TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);

    /// <summary>
    ///  Duplicates a handle. The result references for the same kernel object.
    /// </summary>
    constructor CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
      SameAccess: Boolean);

    /// <summary>
    ///  Queries a token of the specified Windows Terminal Session.
    /// </summary>
    /// <remarks> Requires SeTcbPrivilege. </remarks>
    constructor CreateQueryWts(SessionID: Cardinal);

    /// <summary> Creates a restricted version of the token. </summary>
    constructor CreateRestricted(SrcToken: TToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TGroupArray;
      PrivilegesToDelete: TPrivilegeArray);

    /// <summary> Logons the user with the specified credentials. </summary>
    constructor CreateWithLogon(LogonType: TLogonType;
      LogonProvider: TLogonProvider; Domain, User: String; Password: PWideChar;
      AddGroups: TGroupArray);

    /// <summary>
    ///  Opens a linked token for the current token.
    ///  Requires SeTcbPrivilege to open a primary token.
    /// </summary>
    function OpenLinkedToken: CanFail<TToken>;
  end;

{----------------------  End of interface section  ----------------------------}

implementation

uses
  System.TypInfo, TU.NativeAPI, TU.WtsApi, TU.Processes;

const
  /// <summary> Stores which data class a string class depends on. </summary>
  StringClassToDataClass: array [TTokenStringClass] of TTokenDataClass =
    (tdTokenType, tdNone, tdTokenUser, tdTokenUser, tdTokenSessionId,
    tdTokenElevation, tdTokenIntegrity, tdNone, tdNone, tdTokenMandatoryPolicy,
    tdTokenMandatoryPolicy, tdTokenUIAccess, tdTokenOwner, tdTokenPrimaryGroup,
    tdTokenSandBoxInert,tdTokenHasRestrictions, tdTokenStatistics,
    tdTokenStatistics, tdTokenStatistics, tdTokenStatistics, tdTokenStatistics,
    tdTokenStatistics, tdTokenStatistics, tdTokenStatistics, tdLogonInfo,
    tdLogonInfo, tdLogonInfo, tdLogonInfo, tdLogonInfo, tdLogonInfo,
    tdTokenSource, tdTokenSource, tdTokenOrigin);

{ TTokenCacheAndEvents }

procedure CheckAbandoned(Value: Integer; Name: String);
begin
  if Value > 0 then
    OutputDebugString(PChar('Abandoned ' + Name));
end;

constructor TTokenCacheAndEvents.Create;
begin
  inherited;
  FOnSessionChange.ComparisonFunction := CompareCardinals;
  FOnUIAccessChange.ComparisonFunction := CompareLongBools;
  FOnIntegrityChange.ComparisonFunction := CompareIntegrities;
  FOnPolicyChange.ComparisonFunction := ComparePolicies;
  FOnPrivilegesChange.ComparisonFunction := ComparePrivileges;
  FOnGroupsChange.ComparisonFunction := CompareGroups;
  FOnStatisticsChange.ComparisonFunction := CompareStatistics;
end;

destructor TTokenCacheAndEvents.Destroy;
var
  i: TTokenStringClass;
begin
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

  // Subscribe for future events
  OnStringDataChange[StringClass].Add(Listener);
end;

procedure TTokenCacheAndEvents.UnSubscribeString(StringClass: TTokenStringClass;
  Listener: TEventListener<String>);
begin
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

  // Each token need an event handler to be assigned to it. These event handlers
  // might be the same objects for different TToken instances. It happens so
  // when we have several handles pointing the same kernel object.

  // Assign an existing TTokenCacheAndEvents to the token and maintain reference
  // counter
  if CacheMapping.TryGetValue(Token.HandleInformation.KernelObjectAddress,
      {out} Token.Cache) then
    Inc(Token.Cache.ReferenceCount)
  else
  begin
    // Or create a new one
    Token.Cache := TTokenCacheAndEvents.Create;
    Token.Cache.ObjectAddress := Token.HandleInformation.KernelObjectAddress;
    Token.Cache.ReferenceCount := 1;
    CacheMapping.Add(Token.HandleInformation.KernelObjectAddress, Token.Cache);
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
  end;

  // The handle is going to be closed
  HandleMapping.Remove(Token.hToken);
end;

{ TToken }

procedure TToken.AfterConstruction;
var
  HandleList: THandleList;
  i: integer;
begin
  inherited;

  // Init ower of InfoClass field
  FInfoClassData.Token := Self;

  // Firstly we need to obtain a kernel object address to be able to link token
  // cache. The only way I know to do so is to make a snapshot of all
  // system/process handles and iterate through them.

  // This information might be already known (from a constructor)
  if HandleInformation.KernelObjectAddress = 0 then
  begin
    HandleList := THandleList.CreateOnly(GetCurrentProcessId);

    for i := 0 to HandleList.Count - 1 do
      if HandleList[i].Handle = hToken then
      begin
        FHandleInformation := HandleList[i];
        Break;
      end;

    HandleList.Free;
  end;
  
  // This should not happen and I do not know what to do here
  if FHandleInformation.KernelObjectAddress = 0 then
    raise Exception.Create('Can not obtain kernel object address of a token');

  // Register in the factory and initialize token Cache
  TTokenFactory.RegisterToken(Self);
end;

procedure TToken.AssignToProcess(PID: Cardinal);
var
  hProcess: THandle;
  AccessToken: TProcessAccessToken;
  Status: NTSTATUS;
begin
  // Open the target process
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or
    PROCESS_SET_INFORMATION, False, PID);
  WinCheck(hProcess <> 0, 'OpenProcess with PROCESS_QUERY_INFORMATION | ' +
    'PROCESS_SET_INFORMATION', Self);

  // Open it's first thread and store the handle inside AccessToken
  Status := NtGetNextThread(hProcess, 0, THREAD_QUERY_INFORMATION, 0, 0,
    AccessToken.Thread);

  if not NT_SUCCESS(Status) then
  begin
    CloseHandle(hProcess);
    NativeCheck(Status, 'NtGetNextThread with THREAD_QUERY_INFORMATION', Self);
  end;

  // Prepare the token handle. The thread handle is already in here.
  AccessToken.Token := hToken;

  // Assign the token for the process
  Status := NtSetInformationProcess(hProcess, ProcessAccessToken,
    @AccessToken, SizeOf(AccessToken));

  // Close the process and the thread but not the token
  CloseHandle(hProcess);
  CloseHandle(AccessToken.Thread);

  NativeCheck(Status, 'NtSetInformationProcess#ProcessAccessToken', Self);
end;

function TToken.CanBeFreed: Boolean;
begin
  // Check whether someone wants to raise EAbort to deny token destruction
  OnCanClose.Invoke(Self);
  Result := True;
end;

class function TToken.ConvertGroupArrayToSIDs(
  Groups: TGroupArray): TSIDAndAttributesArray;
var
  i: integer;
begin
  // Note that ConvertStringSidToSid allocates memory that we need to clean up
  // by calling LocalFree. It is done inside FreeSidArray routine.
  SetLength(Result, Length(Groups));
  for i := 0 to High(Result) do
    ConvertStringSidToSid(PWideChar(Groups[i].SecurityIdentifier.SID),
      Result[i].Sid);
end;

constructor TToken.Create(Handle: THandle; Caption: String);
begin
  hToken := Handle;
  FCaption := Caption;;
end;

constructor TToken.CreateByHandle(HandleInfo: THandleInformation);
begin
  if HandleInfo.ContextPID <> GetCurrentProcessId then
    raise ENotImplemented.Create('TODO');

  hToken := HandleInfo.Handle;
  FHandleInformation := HandleInfo;
  FCaption := Format('Inherited %d [0x%x]', [hToken, hToken]);
end;

constructor TToken.CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
  SameAccess: Boolean);
const
  Options: array [Boolean] of Cardinal = (0, DUPLICATE_SAME_ACCESS);
begin
  { TODO: DuplicateHandle doesn't work well with MAXIMUM_ALLOWED access.
    We need to implement it manually. }

  WinCheck(DuplicateHandle(GetCurrentProcess, SrcToken.hToken,
    GetCurrentProcess, @hToken, Access, False, Options[SameAccess]),
    'DuplicateHandle');

  FCaption := SrcToken.Caption + ' (ref)'
end;

constructor TToken.CreateDuplicateToken(SrcToken: TToken; Access: ACCESS_MASK;
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

  // Prepare Object Attributes to store Security QoS
  FillChar(ObjAttr, SizeOf(ObjAttr), 0);
  ObjAttr.Length := SizeOf(ObjAttr);
  ObjAttr.SecurityQualityOfService := @SecQos;

  NativeCheck(NtDuplicateToken(SrcToken.hToken, Access, @ObjAttr, EffectiveOnly,
    TokenType, hToken), 'NtDuplicateToken', SrcToken);

  if EffectiveOnly then
    FCaption := SrcToken.Caption + ' (eff. copy)'
  else
    FCaption := SrcToken.Caption + ' (copy)'
end;

constructor TToken.CreateOpenCurrent;
begin
  WinCheck(OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED, hToken),
    'OpenProcessToken');

  FCaption := 'Current process';
end;

constructor TToken.CreateOpenProcess(PID: Cardinal);
var
  hProcess: THandle;
  ProcessList: TProcessList;
begin
  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
  if hProcess = 0 then
    WinCheck(False, 'OpenProcess');

  try
    WinCheck(OpenProcessToken(hProcess, MAXIMUM_ALLOWED, hToken),
      'OpenProcessToken');
  finally
    CloseHandle(hProcess);
  end;

  // To obtain the executable's name we snapshot process list on the system
  ProcessList := TProcessList.Create;
  FCaption := Format('%s [%d]', [ProcessList.FindName(PID), PID]);
  ProcessList.Free;
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal);
begin
  WinCheck(WTSQueryUserToken(SessionID, hToken), 'WTSQueryUserToken');
  FCaption := Format('Session %d token', [SessionID]);
end;

constructor TToken.CreateRestricted(SrcToken: TToken; Flags: Cardinal;
  SIDsToDisabe, SIDsToRestrict: TGroupArray;
  PrivilegesToDelete: TPrivilegeArray);
var
  Disable, Restrict: TSIDAndAttributesArray;
begin
  // Prepare SID arrays of the suitable format
  Disable := ConvertGroupArrayToSIDs(SIDsToDisabe);
  Restrict := ConvertGroupArrayToSIDs(SIDsToRestrict);
  try
    WinCheck(CreateRestrictedToken(SrcToken.hToken, Flags,
      Length(Disable), Disable,
      Length(PrivilegesToDelete), PLUIDAndAttributes(PrivilegesToDelete),
      Length(Restrict), Restrict,
      hToken), 'CreateRestricted', SrcToken);

    FCaption := 'Restricted ' + SrcToken.Caption;
  finally
    FreeSidArray(Disable);
    FreeSidArray(Restrict);
  end;
end;

constructor TToken.CreateWithLogon(LogonType: TLogonType;
  LogonProvider: TLogonProvider; Domain, User: String; Password: PWideChar;
  AddGroups: TGroupArray);
var
  SIDs: TSIDAndAttributesArray;
  pGroups: PTokenGroups;
  i: integer;
begin
  // If the user doesn't ask us to add some groups to the token we can use
  // simplier LogonUserW routine. Otherwise we use LogonUserExExW (that
  // requires SeTcbPrivilege to add group membership)

  if Length(AddGroups) = 0 then
    WinCheck(LogonUserW(PWideChar(User), PWideChar(Domain), Password,
      Cardinal(LogonType), Cardinal(LogonProvider), hToken), 'LogonUserW')
  else
  begin
    // Convert the specified groups to an array of PSIDs
    SIDs := ConvertGroupArrayToSIDs(AddGroups);

    // Allocate the memory for PTokenGroups buffer
    pGroups := AllocMem(SizeOf(Integer) + SizeOf(TSIDAndAttributes) *
      Length(AddGroups));
    try
      // Copy SIDs along with their attributes
      pGroups.GroupCount := Length(SIDs);
      for i := 0 to High(SIDs) do
        pGroups.Groups[i] := SIDs[i];

      WinCheck(LogonUserExExW(PWideChar(User), PWideChar(Domain), Password,
        Cardinal(LogonType), Cardinal(LogonProvider), pGroups, hToken, nil,
        nil, nil, nil), 'LogonUserExExW');
    finally
      FreeMem(pGroups);
      FreeSidArray(SIDs);
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
      OutputDebugString(PChar('Token.OnClose: ' + E.Message));
      raise;
    end;
  end;

  // Unregister from the factory before we close the handle
  TTokenFactory.UnRegisterToken(Self);

  if hToken <> 0 then
  try
    CloseHandle(hToken); // A protected handle may cause an exception
    hToken := 0;
  except
    ; // but destructor should always succeed
  end;

  if FOnCanClose.Count > 0 then
    OutputDebugString('Abandoned OnCanClose');
  if FOnClose.Count > 0 then
    OutputDebugString('Abandoned OnClose');
  if FOnCaptionChange.Count > 0 then
    OutputDebugString('Abandoned OnCaptionChange');

  inherited;
end;

class procedure TToken.FreeSidArray(SIDs: TSIDAndAttributesArray);
var
  i: integer;
begin
  // The memory was previously allocated by ConvertStringSidToSid.
  for i := 0 to High(SIDs) do
    if Assigned(SIDs[i].Sid) then
      LocalFree(NativeUInt(SIDs[i].Sid));
end;

procedure TToken.GroupAdjust(GroupArray: TGroupArray; Action:
  TGroupAdjustAction);
const
  IsResetFlag: array [TGroupAdjustAction] of LongBool = (True, False, False);
var
  i: integer;
  GroupsArray: TSIDAndAttributesArray;
  Buffer: PTokenGroups;
begin
  // Allocate enought memory to hold all the groups
  Buffer := AllocMem(SizeOf(Integer) + SizeOf(TSIDAndAttributes) *
    Length(GroupArray));
  Buffer.GroupCount := Length(GroupArray);

  // Allocate all PSIDs and copy them to the buffer
  GroupsArray := ConvertGroupArrayToSIDs(GroupArray);
  for i := 0 to High(GroupsArray) do
    Buffer.Groups[i] := GroupsArray[i];

  // Set approriate attribes depending on action
  if Action = gaEnable then
    for i := 0 to Buffer.GroupCount - 1 do
      Buffer.Groups[i].Attributes := Cardinal(GroupEnabled);

  try
    WinCheck(AdjustTokenGroups(hToken, IsResetFlag[Action], Buffer, 0, nil,
      nil), 'AdjustTokenGroups', Self);

    // Update the cache and notify event listeners
    InfoClass.ReQuery(tdTokenGroups);
    InfoClass.ReQuery(tdTokenStatistics);
  finally
    FreeMem(Buffer);
    FreeSidArray(GroupsArray);
  end;
end;

function TToken.OpenLinkedToken: CanFail<TToken>;
var
  Handle: THandle;
begin
  Result.Init(Self);

  if Result.CheckError(QueryFixedSize<THandle>(TokenLinkedToken, Handle),
    GetterMessage(TokenLinkedToken)) then
    Result.Value := TToken.Create(Handle, 'Linked token for' + Caption);
end;

procedure TToken.PrivilegeAdjust(PrivilegeArray: TPrivilegeLUIDArray;
  Action: TPrivilegeAdjustAction);
const
  ActionToAttribute: array [TPrivilegeAdjustAction] of Cardinal =
    (SE_PRIVILEGE_ENABLED, 0, SE_PRIVILEGE_REMOVED);
var
  Buffer: PTokenPrivileges;
  BufferSize: Cardinal;
  i: integer;
begin
  // Allocate enought memory to hold all the privileges
  BufferSize := SizeOf(Cardinal) + SizeOf(TLUIDAndAttributes) *
    Length(PrivilegeArray);

  Buffer := AllocMem(BufferSize);
  try
    Buffer.PrivilegeCount := Length(PrivilegeArray);
    for i := 0 to High(PrivilegeArray) do
    begin
      Buffer.Privileges[i].Luid := PrivilegeArray[i];
      Buffer.Privileges[i].Attributes := ActionToAttribute[Action];
    end;

    WinCheck(AdjustTokenPrivileges(hToken, False, Buffer, BufferSize, nil,
      nil) and (GetLastError = ERROR_SUCCESS), 'AdjustTokenPrivileges', Self);
  finally
    FreeMem(Buffer);

    // The function could modify privileges even without succeeding.
    // Update the cache and notify event listeners.
    InfoClass.ReQuery(tdTokenPrivileges);
    InfoClass.ReQuery(tdTokenStatistics);
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
  out Status: Boolean): Pointer;
var
  BufferSize, ReturnValue: Cardinal;
begin
  Status := False;

  // Make a probe call to estimate a requied buffer size
  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);

  // Check for errors and for too big buffers
  if not WinTryCheckBuffer(BufferSize) then
    Exit;

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
  end;

  // Do not free the buffer on success. The caller must do it after use.
end;

function TToken.SendHandleToProcess(PID: Cardinal): NativeUInt;
var
  hTargetProcess: THandle;
begin
  // Open target process
  hTargetProcess := OpenProcess(PROCESS_DUP_HANDLE, False, PID);
  WinCheck(LongBool(hTargetProcess), 'OpenProcess#PROCESS_DUP_HANDLE');

  try
    // Send then handle
    WinCheck(DuplicateHandle(GetCurrentProcess, hToken, hTargetProcess, @Result,
      0, False, DUPLICATE_SAME_ACCESS), 'DuplicateHandle')
  finally
    CloseHandle(hTargetProcess);
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
    then raise ELocatedOSError.CreateLE(GetLastError, SetterMessage(InfoClass),
      Self);
end;

{ TTokenData }

function TTokenData.GetElevation: TTokenElevationType;
begin
  Result := Token.Cache.Elevation;
end;

function TTokenData.GetGroups: TGroupArray;
begin
  Result := Token.Cache.Groups;
end;

function TTokenData.GetHasRestrictions: LongBool;
begin
  Result := Token.Cache.HasRestrictions;
end;

function TTokenData.GetIntegrity: TTokenIntegrity;
begin
  Result := Token.Cache.Integrity;
end;

function TTokenData.GetLogonSessionInfo: TLogonSessionInfo;
begin
  Result := Token.Cache.LogonSessionInfo;
end;

function TTokenData.GetMandatoryPolicy: TMandatoryPolicy;
begin
  Result := Token.Cache.MandatoryPolicy;
end;

function TTokenData.GetOrigin: LUID;
begin
  Result := Token.Cache.Origin;
end;

function TTokenData.GetOwner: TSecurityIdentifier;
begin
  Result := Token.Cache.Owner;
end;

function TTokenData.GetPrimaryGroup: TSecurityIdentifier;
begin
  Result := Token.Cache.PrimaryGroup;
end;

function TTokenData.GetPrivileges: TPrivilegeArray;
begin
  Result := Token.Cache.Privileges;
end;

function TTokenData.GetRestrictedSids: TGroupArray;
begin
  Result := Token.Cache.RestrictedSids;
end;

function TTokenData.GetSandboxInert: LongBool;
begin
  Result := Token.Cache.SandboxInert;
end;

function TTokenData.GetSession: Cardinal;
begin
  Result := Token.Cache.Session;
end;

function TTokenData.GetSource: TTokenSource;
begin
  Result := Token.Cache.Source;
end;

function TTokenData.GetStatistics: TTokenStatistics;
begin
  Result := Token.Cache.Statistics;
end;

function TTokenData.GetTokenType: TTokenTypeEx;
begin
  Result := Token.Cache.TokenType;
end;

function TTokenData.GetUIAccess: LongBool;
begin
  Result := Token.Cache.UIAccess;
end;

function TTokenData.GetUser: TGroup;
begin
  Result := Token.Cache.User;
end;

procedure TTokenData.InvokeStringEvent(StringClass: TTokenStringClass);
begin
  Token.Events.OnStringDataChange[StringClass].Invoke(QueryString(StringClass));
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

    tsAccess:
      if Detailed then
        Result := AccessToDetailedString(Token.HandleInformation.Access)
      else
        Result := AccessToString(Token.HandleInformation.Access);

    tsUserName:
      Result := Token.Cache.User.SecurityIdentifier.ToString;

    tsUserState:
      Result := Token.Cache.User.Attributes.ToString;

    tsSession: // Detailed?
      Result := Token.Cache.Session.ToString;

    tsElevation:
      Result := Token.Cache.Elevation.ToString;

    tsIntegrity:
      if Detailed then
        Result := Token.Cache.Integrity.ToDetailedString
      else
        Result := Token.Cache.Integrity.ToString;

    tsObjectAddress:
      Result := Format('0x%0.8x',
        [Token.HandleInformation.KernelObjectAddress]);

    tsHandle:
      if Detailed then
        Result := Format('0x%x (%d)', [Token.Handle, Token.Handle])
      else
        Result := Format('0x%x', [Token.Handle]);

    tsNoWriteUpPolicy:
      Result := EnabledDisabledToString(Cardinal(Token.Cache.MandatoryPolicy)
        and Cardinal(TokenMandatoryPolicyNoWriteUp) <> 0);

    tsNewProcessMinPolicy:
      Result := EnabledDisabledToString(Cardinal(Token.Cache.MandatoryPolicy)
        and Cardinal(TokenMandatoryPolicyNewProcessMin) <> 0);

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

    tsTokenID:
      Result := Token.Cache.Statistics.TokenId.ToString;

    tsExprires:
      Result := NativeTimeToString(Token.Cache.Statistics.ExpirationTime.QuadPart);

    tsDynamicCharged:
      Result := BytesToString(Token.InfoClass.Statistics.DynamicCharged);

    tsDynamicAvailable:
      Result := BytesToString(Token.InfoClass.Statistics.DynamicAvailable);

    tsGroupCount:
      Result := Token.Cache.Statistics.GroupCount.ToString;

    tsPrivilegeCount:
      Result := Token.Cache.Statistics.PrivilegeCount.ToString;

    tsModifiedID:
      Result := Token.Cache.Statistics.ModifiedId.ToString;

    tsLogonID:
      Result := Token.Cache.Statistics.AuthenticationId.ToString;

    tsLogonAuthPackage:
      Result := Token.Cache.LogonSessionInfo.AuthPackage;

    tsLogonServer:
      Result := Token.Cache.LogonSessionInfo.LogonServer;

    tsLogonWtsSession: // Detailed?
      Result := Token.Cache.LogonSessionInfo.Session.ToString;

    tsLogonTime:
      Result := DateTimeToStr(Token.Cache.LogonSessionInfo.LogonTime);

    tsLogonType:
      Result := LogonTypeToString(Token.Cache.LogonSessionInfo.LogonType);

    tsLogonUserName:
      if Token.Cache.LogonSessionInfo.UserPresent then
        Result := Token.Cache.LogonSessionInfo.User.ToString
      else
        Result := 'No user';

    tsSourceLUID:
      Result := Token.Cache.Source.SourceIdentifier.ToString;

    tsSourceName:
      Result := TokeSourceNameToString(Token.Cache.Source);

    tsOrigin:
      Result := Token.Cache.Origin.ToString;
  end;
  {$ENDREGION}
end;

function TTokenData.ReQuery(DataClass: TTokenDataClass): Boolean;
var
  pIntegrity: PSIDAndAttributes;
  pPrivBuf: PTokenPrivileges;
  lType: TTokenType;
  lImpersonation: TSecurityImpersonationLevel;
  i: Integer;
begin
  Result := False;

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
      Result := Token.QuerySid(TokenOwner, Token.Cache.Owner);

    tdTokenPrimaryGroup:
     Result := Token.QuerySid(TokenPrimaryGroup, Token.Cache.PrimaryGroup);

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

    tdTokenSandBoxInert:
      Result := Token.QueryFixedSize<LongBool>(TokenSandBoxInert,
        Token.Cache.SandboxInert);

    tdTokenOrigin:
      Result := Token.QueryFixedSize<LUID>(TokenOrigin,
        Token.Cache.Origin);

    tdTokenElevation:
      Result := Token.QueryFixedSize<TTokenElevationType>(TokenElevationType,
        Token.Cache.Elevation);

    tdTokenHasRestrictions:
      Result := Token.QueryFixedSize<LongBool>(TokenHasRestrictions,
        Token.Cache.HasRestrictions);

    tdTokenVirtualization: ; // Not implemented

    tdTokenIntegrity:
    begin
      pIntegrity := Token.QueryVariableSize(TokenIntegrityLevel, Result);
      if Result then
      try
        Token.Cache.Integrity.SID.CreateFromSid(pIntegrity.Sid);

        // Get level value from the SID sub-authority
        if GetSidSubAuthorityCount(pIntegrity.Sid)^ = 1 then
          Token.Cache.Integrity.Level := TTokenIntegrityLevel(
            GetSidSubAuthority(pIntegrity.Sid, 0)^);

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

    tdLogonInfo:
    if Query(tdTokenStatistics) then
      with QueryLogonSession(Token.Cache.Statistics.AuthenticationId) do
        if IsValid then
        begin
          Result := True;
          Token.Cache.LogonSessionInfo := Value;
        end;
  end;

  Token.Cache.IsCached[DataClass] := Token.Cache.IsCached[DataClass] or Result;
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
  mandatoryLabel.Sid := AllocMem(GetSidLengthRequired(1));
  try
    InitializeSid(mandatoryLabel.Sid, SECURITY_MANDATORY_LABEL_AUTHORITY, 1);
    GetSidSubAuthority(mandatoryLabel.Sid, 0)^ := DWORD(Value);
    mandatoryLabel.Attributes := SE_GROUP_INTEGRITY;

    Token.SetFixedSize<TSIDAndAttributes>(TokenIntegrityLevel, mandatoryLabel);
  finally
    FreeMem(mandatoryLabel.Sid);
  end;

  // Update the cache and notify event listeners.
  // Integrity is also stored in the group list, so update them too
  ReQuery(tdTokenIntegrity);
  ReQuery(tdTokenGroups);
  ReQuery(tdTokenStatistics);
end;

procedure TTokenData.SetMandatoryPolicy(const Value: TMandatoryPolicy);
begin
  Token.SetFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy, Value);

  // Update the cache and notify event listeners
  ReQuery(tdTokenMandatoryPolicy);
  ReQuery(tdTokenStatistics);
end;

procedure TTokenData.SetSession(const Value: Cardinal);
begin
  Token.SetFixedSize<Cardinal>(TokenSessionId, Value);

  // Update the cache and notify event listeners
  ReQuery(tdTokenSessionId);
  ReQuery(tdTokenStatistics);
end;

procedure TTokenData.SetUIAccess(const Value: LongBool);
begin
  Token.SetFixedSize<LongBool>(TokenUIAccess, Value);

  // Update the cache and notify event listeners
  ReQuery(tdTokenUIAccess);
  ReQuery(tdTokenStatistics);
end;

end.
