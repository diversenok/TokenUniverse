unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
uses
  Ntapi.WinNt, Ntapi.ntobapi, Ntapi.WinSafer,
  TU.Tokens.Types, NtUtils.Objects.Snapshots, DelphiUtils.Events,
  NtUtils.Security.Sid, Ntapi.ntseapi, Ntapi.NtSecApi, NtUtils.Lsa.Audit,
  System.Generics.Collections, NtUtils.Lsa.Logon, DelphiUtils.AutoObjects,
  NtUtils.Security.Acl, NtUtils.Objects, NtUtils, TU.Tokens3;

type
  TTokenElevationInfo = TU.Tokens3.TTokenElevationInfo;

  /// <summary>
  ///  A class of information for tokens that can be queried and cached.
  /// </summary>
  TTokenDataClass = (tdNone, tdTokenUser, tdTokenGroups, tdTokenPrivileges,
    tdTokenOwner, tdTokenPrimaryGroup, tdTokenDefaultDacl, tdTokenSource,
    tdTokenType, tdTokenStatistics, tdTokenRestrictedSids, tdTokenSessionId,
    tdTokenAuditPolicy, tdTokenSandBoxInert, tdTokenOrigin,
    tdTokenElevationInfo, tdTokenHasRestrictions, tdTokenFlags,
    tdTokenVirtualizationAllowed, tdTokenVirtualizationEnabled,
    tdTokenIntegrity, tdTokenUIAccess, tdTokenMandatoryPolicy,
    tdTokenIsRestricted, tdTokenAppContainer, tdLogonInfo, tdObjectInfo,
    tdHandleInfo);

  IToken = interface;
  TToken = class;

  TLogonSessionCache = record
    LogonId: TLogonId;
    WellKnownSid: ISid;
    Detailed: ILogonSession;
  end;

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
    Session: TSessionId;
    AuditPolicy: IMemory<PTokenAuditPolicy>;
    SandboxInert: LongBool;
    Origin: TLogonId;
    ElevationInfo: TTokenElevationInfo;
    HasRestrictions: LongBool;
    Flags: TTokenFlags;
    VirtualizationAllowed: LongBool;
    VirtualizationEnabled: LongBool;
    Integrity: TGroup;
    UIAccess: LongBool;
    MandatoryPolicy: TTokenMandatoryPolicy;
    IsRestricted: LongBool;
    AppContainer: ISid;
    LogonSessionInfo: TLogonSessionCache;
    ObjectInformation: TObjectBasicInformation;
    HandleInformation: TSystemHandleEntry;
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
    procedure SetAuditPolicy(const Value: IMemory<PTokenAuditPolicy>);
    procedure SetOrigin(const Value: TLuid);
    procedure SetUIAccess(const Value: LongBool);
    procedure SetOwner(Value: ISid);
    procedure SetPrimaryGroup(Value: ISid);
    function GetVirtualizationAllowed: LongBool;
    function GetVirtualizationEnabled: LongBool;
    function GetElevationInfo: TTokenElevationInfo;
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
    function GetAuditPolicy: IMemory<PTokenAuditPolicy>;
    function GetSource: TTokenSource;
    function GetStatistics: TTokenStatistics;
    function GetTokenType: TTokenTypeEx;
    function GetUIAccess: LongBool;
    function GetUser: TGroup;
    function GetLogonSessionInfo: TLogonSessionCache;
    function GetIsRestricted: LongBool;
    function GetDefaultDacl: IAcl;
    function GetFlags: Cardinal;
    procedure SetVirtualizationAllowed(const Value: LongBool);
    procedure SetVirtualizationEnabled(const Value: LongBool);
    procedure SetDefaultDacl(Value: IAcl);
    function GetObjectInfo: TObjectBasicInformation;
    procedure SetSessionReference(const Value: LongBool);
    function GetHandleInfo: TSystemHandleEntry;
    function GetAppContainer: ISid;
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
    property SessionReference: LongBool write SetSessionReference;              // class 14 #settable + not gettable
    property SandboxInert: LongBool read GetSandboxInert;                       // class 15
    property AuditPolicy: IMemory<PTokenAuditPolicy> read GetAuditPolicy write SetAuditPolicy;// class 16 #settable
    property Origin: TLuid read GetOrigin write SetOrigin;                      // class 17 #settable
    property ElevationInfo: TTokenElevationInfo read GetElevationInfo;          // class 18 & 20
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
    property AppContainer: ISid read GetAppContainer;                           // class 31
    property IsRestricted: LongBool read GetIsRestricted;                       // class 40
    property LogonSessionInfo: TLogonSessionCache read GetLogonSessionInfo;
    property ObjectInformation: TObjectBasicInformation read GetObjectInfo;
    property HandleInformation: TSystemHandleEntry read GetHandleInfo;

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
  end;
  PTokenData = ^TTokenData;

  {-------------------  TToken object definition  ---------------------------}

  TTokenEvent = TEvent<IToken>;
  PTokenEvent = ^TTokenEvent;

  IToken = interface
    ['{A5F087C7-53FE-4C6F-B4E9-2AF5CD270117}']
    procedure SetCaption(const Value: String);
    function GetCaption: String;
    function GetHandle: IHandle;
    function GetInfoClassData: PTokenData;
    function GetCache: TTokenCacheAndEvents;
    property Handle: IHandle read GetHandle;
    function ConvertToReal: IToken;
    property InfoClass: PTokenData read GetInfoClassData;
    property Events: TTokenCacheAndEvents read GetCache;
    property Caption: String read GetCaption write SetCaption;
    function SendHandleToProcess(PID: NativeUInt): NativeUInt;
    function OpenLinkedToken(out Token: IToken): TNtxStatus;
  end;

  /// <summary>
  ///  Token Universe representation of an opend token handle.
  /// </summary>
  TToken = class(TInterfacedObject, IToken, IToken3)
  private
    procedure SetCaption(const Value: String);
    function GetCaption: String;
    function GetHandle: IHandle;
    function GetInfoClassData: PTokenData;
    function GetCache: TTokenCacheAndEvents;
    function GetOnClose: PTokenEvent;
    function GetOnCanClose: PTokenEvent;
  protected
    hxToken: IHandle;
    FInfoClassData: TTokenData;
    Cache: TTokenCacheAndEvents;

    FCaption: String;
    FOnCanClose: TTokenEvent;
    FOnClose: TTokenEvent;

    // Migration to the new IToken interface
    FTokenV3: IToken3;
    property TokenV3: IToken3 read FTokenV3 implements IToken3;
  public
    {--------------------  TToken public section ---------------------------}

    property Handle: IHandle read GetHandle;
    function ConvertToReal: IToken;

    property InfoClass: PTokenData read GetInfoClassData;
    property Caption: String read GetCaption write SetCaption;

    /// <summary>
    ///  The event is called to test whether the token can be destroyed.
    ///  The listener can deny object destruction by calling
    ///  <see cref="System.SysUtils.EAbort"/>.
    /// </summary>
    property OnCanClose: PTokenEvent read GetOnCanClose;

    /// <summary> The event is called on token destruction. </summary>
    /// <remarks> Be aware of exceptions at this point. </remarks>
    property OnClose: PTokenEvent read GetOnClose;
    destructor Destroy; override;

    function SendHandleToProcess(PID: NativeUInt): NativeUInt;

    /// <summary> Removes the thread impersonation token. </summary>
    class procedure RevertThreadToken(TID: NativeUInt);
  public

    {--------------------  TToken constructors  ----------------------------}

    /// <summary>
    ///  Registers in the factory and initializes cache.
    /// </summary>
    procedure AfterConstruction; override;

    /// <summary> General purpuse constructor. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create(Handle: IHandle; Caption: String);

    /// <summary>
    ///  Create a TToken object using inherited handle.
    /// </summary>
    constructor CreateByHandle(Handle: THandle);

    /// <summary>
    ///  Create a pseudo TToken object that represent a entry from a system
    ///  handle snapshot.
    /// </summary>
    constructor CreatePseudo(HandleInfo: TSystemHandleEntry; ImageName: String;
      LocalCopy: IHandle = nil);

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
    constructor CreateDuplicateToken(SrcToken: IToken; Access: TAccessMask;
      TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);

    /// <summary>
    ///  Duplicates a handle. The result references for the same kernel object.
    /// </summary>
    constructor CreateDuplicateHandle(SrcToken: IToken; Access: TAccessMask;
      SameAccess: Boolean; HandleAttributes: Cardinal = 0);

    /// <summary>
    ///  Queries a token of the specified Windows Terminal Session.
    /// </summary>
    constructor CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);

    /// <summary> Creates a restricted version of the token. </summary>
    constructor CreateRestricted(SrcToken: IToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TArray<TGroup>;
      PrivilegesToDelete: TArray<TPrivilege>);

    /// <summary> Logons a user with the specified credentials. </summary>
    constructor CreateWithLogon(LogonType: TSecurityLogonType;
      Domain, User, Password: String; AddGroups: TArray<TGroup>);

    /// <summary> Logon a user using Services 4 Users. </summary>
    constructor CreateS4ULogon(Domain, User: String; const Source: TTokenSource;
      AddGroups: TArray<TGroup>);

    /// <summary> Creates a new token from the scratch. </summary>
    /// <remarks> This action requires SeCreateTokenPrivilege. </remarks>
    constructor CreateNtCreateToken(User: ISid; DisableUser: Boolean;
      Groups: TArray<TGroup>; Privileges: TArray<TPrivilege>;
      LogonID: TLuid; Owner: ISid; PrimaryGroup: ISid;
      const Source: TTokenSource; Expires: TLargeInteger);

    /// <summary>
    ///  Create a token using <see cref="NtImpersonateAnonymousToken">.
    /// </summary>
    constructor CreateAnonymous(Access: TAccessMask = MAXIMUM_ALLOWED;
      HandleAttributes: Cardinal = 0);

    /// <summary> Create a restricted token using Safer API. </summary>
    constructor CreateSaferToken(SrcToken: IToken; ScopeId: TSaferScopeId;
      LevelId: TSaferLevelId; MakeInert: Boolean = False);

    /// <summary>
    ///  Opens a linked token for the current token.
    ///  Requires SeTcbPrivilege to open a primary token.
    /// </summary>
    function OpenLinkedToken(out Token: IToken): TNtxStatus;
  end;

{----------------------  End of interface section  ----------------------------}

implementation

uses
  Ntapi.ntstatus, Ntapi.ntpsapi, NtUtils.Tokens.Info,
  NtUtils.Processes, NtUtils.WinStation, NtUtils.Tokens, NtUiLib.Errors,
  NtUtils.Tokens.Impersonate, System.SysUtils, NtUtils.Tokens.Logon,
  NtUtils.WinSafer, DelphiUtils.Arrays, NtUtils.Lsa.Sid, NtUiLib.Exceptions;

{ TToken }

procedure TToken.AfterConstruction;
begin
  inherited;
  FInfoClassData.Token := Self;

  if not Assigned(Cache) then
    Cache := TTokenCacheAndEvents.Create;

  FTokenV3 := CaptureTokenHandle(hxToken, FCaption);
end;

function TToken.ConvertToReal: IToken;
var
  hxProcess: IHandle;
begin
  if not Assigned(hxToken) then
  begin
    NtxOpenProcess(hxProcess, Cache.HandleInformation.UniqueProcessId,
      PROCESS_DUP_HANDLE).RaiseOnError;

    NtxDuplicateHandleFrom(hxProcess.Handle,
      Cache.HandleInformation.HandleValue, hxToken).RaiseOnError;
  end;

  Result := Self;
end;

constructor TToken.Create(Handle: IHandle; Caption: String);
begin
  hxToken := Handle;
  FCaption := Caption;;
end;

constructor TToken.CreateAnonymous(Access: TAccessMask;
  HandleAttributes: Cardinal);
begin
  NtxOpenAnonymousToken(hxToken, Access, HandleAttributes).RaiseOnError;
  FCaption := 'Anonymous token';
end;

constructor TToken.CreateByHandle(Handle: THandle);
begin
  hxToken := NtxObject.Capture(Handle);
  FCaption := Format('Inherited %d [0x%x]', [hxToken.Handle, hxToken.Handle]);
end;

constructor TToken.CreateDuplicateHandle(SrcToken: IToken; Access: TAccessMask;
  SameAccess: Boolean; HandleAttributes: Cardinal = 0);
begin
  NtxDuplicateHandleLocal(SrcToken.Handle.Handle, hxToken, Access,
    HandleAttributes).RaiseOnError;

  FCaption := SrcToken.Caption + ' (ref)'
  // TODO: No need to snapshot handles, object address is already known
end;

constructor TToken.CreateDuplicateToken(SrcToken: IToken; Access: TAccessMask;
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

  NtxDuplicateToken(hxToken, SrcToken.Handle, TokenType,
    ImpersonationLvl, AttributeBuilder.UseEffectiveOnly(EffectiveOnly).
    UseDesiredAccess(Access)).RaiseOnError;

  if EffectiveOnly then
    FCaption := SrcToken.Caption + ' (eff. copy)'
  else
    FCaption := SrcToken.Caption + ' (copy)'
end;

constructor TToken.CreateNtCreateToken(User: ISid; DisableUser: Boolean;
  Groups: TArray<TGroup>; Privileges: TArray<TPrivilege>; LogonID: TLuid;
  Owner: ISid; PrimaryGroup: ISid; const Source: TTokenSource;
  Expires: TLargeInteger);
var
  TokenUser: TGroup;
begin
  TokenUser.Sid := User;

  // Fill user attributes. Zero value is default here and means "Enabled"
  if DisableUser then
    TokenUser.Attributes := SE_GROUP_USE_FOR_DENY_ONLY
  else
    TokenUser.Attributes := 0;

  NtxCreateToken(hxToken, TokenPrimary, SecurityImpersonation,
    Source, LogonID, TokenUser, PrimaryGroup, Groups, Privileges, Owner, nil,
    Expires).RaiseOnError;

  FCaption := 'New token: ' + LsaxSidToString(User);
end;

constructor TToken.CreateOpenCurrent(Access: TAccessMask);
begin
  CreateOpenProcess(NtCurrentProcessId, 'Current process');
end;

constructor TToken.CreateOpenEffective(TID: NativeUInt; ImageName: String;
  ImpersonationLevel: TSecurityImpersonationLevel; Access: TAccessMask;
  Attributes: Cardinal; EffectiveOnly: Boolean);
begin
  NtxCopyEffectiveTokenById(hxToken, TID, ImpersonationLevel, Access,
    Attributes, EffectiveOnly).RaiseOnError;

  FCaption := Format('Eff. thread %d of %s', [TID, ImageName]);
  if EffectiveOnly then
    FCaption := FCaption + ' (eff.)';
end;

constructor TToken.CreateOpenProcess(PID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal);
begin
  NtxOpenProcessTokenById(hxToken, PID, Access, Attributes).RaiseOnError;
  FCaption := Format('%s [%d]', [ImageName, PID]);
end;

constructor TToken.CreateOpenThread(TID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal; Dummy: Integer);
begin
  NtxOpenThreadTokenById(hxToken, TID, Access, Attributes).RaiseOnError;
  FCaption := Format('Thread %d of %s', [TID, ImageName]);
end;

constructor TToken.CreatePseudo(HandleInfo: TSystemHandleEntry;
  ImageName: String; LocalCopy: IHandle);
begin
  hxToken := LocalCopy;

  // Add object info to the cache
  FInfoClassData.Token := Self;
  Cache := TTokenCacheAndEvents.Create;
  Cache.HandleInformation := HandleInfo;
  Cache.ObjectInformation.GrantedAccess := HandleInfo.GrantedAccess;
  Cache.IsCached[tdHandleInfo] := True;
  Cache.IsCached[tdObjectInfo] := True;

  FCaption := Format('Handle %d @ %s', [HandleInfo.HandleValue, ImageName]);
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);
begin
  WsxQueryToken(hxToken, SessionID).RaiseOnError;
  FCaption := Format('Session %d token', [SessionID]);
end;

constructor TToken.CreateRestricted(SrcToken: IToken; Flags: Cardinal;
  SIDsToDisabe, SIDsToRestrict: TArray<TGroup>;
  PrivilegesToDelete: TArray<TPrivilege>);
var
  Disable, Restrict: TArray<ISid>;
  Remove: TArray<TLuid>;
  i: Integer;
begin
  SetLength(Disable, Length(SIDsToDisabe));
  for i := 0 to High(SIDsToDisabe) do
    Disable[i] := SIDsToDisabe[i].Sid;

  SetLength(Restrict, Length(SIDsToRestrict));
  for i := 0 to High(SIDsToRestrict) do
    Restrict[i] := SIDsToRestrict[i].Sid;

  SetLength(Remove, Length(PrivilegesToDelete));
  for i := 0 to High(PrivilegesToDelete) do
    Remove[i] := PrivilegesToDelete[i].Luid;

  NtxFilterToken(hxToken, SrcToken.Handle, Flags, Disable, Remove,
    Restrict).RaiseOnError;

  FCaption := 'Restricted ' + SrcToken.Caption;
end;

constructor TToken.CreateS4ULogon(Domain, User: String;
  const Source: TTokenSource; AddGroups: TArray<TGroup>);
begin
  LsaxLogonS4U(hxToken, Domain, User, Source, AddGroups).RaiseOnError;

  FCaption := 'S4U logon of ' + User;
end;

constructor TToken.CreateSaferToken(SrcToken: IToken; ScopeId: TSaferScopeId;
  LevelId: TSaferLevelId; MakeInert: Boolean = False);
var
  LevelName: String;
begin
  SafexComputeSaferTokenById(hxToken, SrcToken.Handle, ScopeId, LevelId,
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

  FCaption := LevelName + ' Safer for ' + SrcToken.Caption
end;

constructor TToken.CreateWithLogon(LogonType: TSecurityLogonType;
  Domain, User, Password: String; AddGroups: TArray<TGroup>);
begin
  LsaxLogonUser(hxToken, Domain, User, PWideChar(Password), LogonType,
    AddGroups).RaiseOnError;

  FCaption := 'Logon of ' + User;
end;

destructor TToken.Destroy;
begin
  // Inform event listeners that we are closing the handle
  OnClose.Invoke(Self);

  // Unregister from the factory before we close the handle
  Cache.Free;

  inherited;
end;

function TToken.GetCache: TTokenCacheAndEvents;
begin
  Result := Cache;
end;

function TToken.GetCaption: String;
begin
  Result := FCaption;
end;

function TToken.GetHandle: IHandle;
begin
  Result := hxToken;
end;

function TToken.GetInfoClassData: PTokenData;
begin
  Result := @FInfoClassData;
end;

function TToken.GetOnCanClose: PTokenEvent;
begin
  Result := @FOnCanClose;
end;

function TToken.GetOnClose: PTokenEvent;
begin
  Result := @FOnClose;
end;

function TToken.OpenLinkedToken(out Token: IToken): TNtxStatus;
var
  Handle: THandle;
begin
  Result := NtxToken.Query(hxToken, TokenLinkedToken,
    Handle);

  if Result.IsSuccess then
    Token := TToken.Create(NtxObject.Capture(Handle),
      'Linked token for ' + Caption);
end;

class procedure TToken.RevertThreadToken(TID: NativeUInt);
begin
  NtxSetThreadTokenById(TID, nil).RaiseOnError;
end;

function TToken.SendHandleToProcess(PID: NativeUInt): NativeUInt;
var
  hxTargetProcess: IHandle;
begin
  NtxOpenProcess(hxTargetProcess, PID, PROCESS_DUP_HANDLE).RaiseOnError;

  // Send the handle
  NtxDuplicateHandleTo(hxTargetProcess.Handle, hxToken.Handle,
    Result).RaiseOnError;
end;

procedure TToken.SetCaption(const Value: String);
begin
  FCaption := Value;

  if Assigned(TokenV3) then
    TokenV3.Caption := Value;
end;

{ TTokenData }

function TTokenData.GetAppContainer: ISid;
begin
  Assert(Token.Cache.IsCached[tdTokenAppContainer]);
  Result := Token.Cache.AppContainer;
end;

function TTokenData.GetAuditPolicy: IMemory<PTokenAuditPolicy>;
begin
  Assert(Token.Cache.IsCached[tdTokenAuditPolicy]);
  Result := Token.Cache.AuditPolicy;
end;

function TTokenData.GetDefaultDacl: IAcl;
begin
  Assert(Token.Cache.IsCached[tdTokenDefaultDacl]);
  Result := Token.Cache.DefaultDacl;
end;

function TTokenData.GetElevationInfo: TTokenElevationInfo;
begin
  Assert(Token.Cache.IsCached[tdTokenElevationInfo]);
  Result := Token.Cache.ElevationInfo;
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

function TTokenData.GetHandleInfo: TSystemHandleEntry;
begin
  Assert(Token.Cache.IsCached[tdHandleInfo]);
  Result := Token.Cache.HandleInformation;
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

function TTokenData.GetLogonSessionInfo: TLogonSessionCache;
begin
  Assert(Token.Cache.IsCached[tdLogonInfo]);
  Result := Token.Cache.LogonSessionInfo;
end;

function TTokenData.GetMandatoryPolicy: Cardinal;
begin
  Assert(Token.Cache.IsCached[tdTokenMandatoryPolicy]);
  Result := Token.Cache.MandatoryPolicy;
end;

function TTokenData.GetObjectInfo: TObjectBasicInformation;
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

function TTokenData.Query(DataClass: TTokenDataClass): Boolean;
begin
  Result := Token.Cache.IsCached[DataClass] or
    (Assigned(Token.hxToken) and ReQuery(DataClass));
end;

function TTokenData.ReQuery(DataClass: TTokenDataClass): Boolean;
var
  lType: TTokenType;
  lImpersonation: TSecurityImpersonationLevel;
  Handles: TArray<TSystemHandleEntry>;
begin
  Result := False;

  case DataClass of
    tdNone:
       Result := True;

    tdTokenUser:
    begin
      Result := Token.TokenV3.QueryUser(Token.Cache.User).IsSuccess;

      // The default value of attributes for user is 0 and means "Enabled".
      // In this case we replace it with this flag. However, it can also be
      // "Use for deny only" and we shouldn't replace it in this case.

      if Result and (Token.Cache.User.Attributes = 0) then
        Token.Cache.User.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or
          SE_GROUP_ENABLED;
    end;

    tdTokenGroups:
      Result := Token.TokenV3.QueryGroups(Token.Cache.Groups).IsSuccess;

    tdTokenPrivileges:
      Result := Token.TokenV3.QueryPrivileges(Token.Cache.Privileges).IsSuccess;

    tdTokenOwner:
      Result := Token.TokenV3.QueryOwner(Token.Cache.Owner).IsSuccess;

    tdTokenPrimaryGroup:
     Result := Token.TokenV3.QueryPrimaryGroup(Token.Cache.PrimaryGroup).IsSuccess;

    tdTokenDefaultDacl:
      Result := Token.TokenV3.QueryDefaultDacl(Token.Cache.DefaultDacl).IsSuccess;

    tdTokenSource:
      Result := Token.TokenV3.QuerySource(Token.Cache.Source).IsSuccess;

    tdTokenType:
    begin
      Result := Token.TokenV3.QueryType(lType).IsSuccess;

      if Result then
      begin
        if lType = TokenPrimary then
          Token.Cache.TokenType := ttPrimary
        else
        begin
          Result := Token.TokenV3.QueryImpersonation(lImpersonation).IsSuccess;

          if Result then
            Token.Cache.TokenType := TTokenTypeEx(lImpersonation);
        end;
      end;
    end;

    tdTokenStatistics:
      Result := Token.TokenV3.QueryStatistics(Token.Cache.Statistics).IsSuccess;

    tdTokenRestrictedSids:
      Result := Token.TokenV3.QueryRestrictedSids(Token.Cache.RestrictedSids).IsSuccess;

    tdTokenSessionId:
      Result := Token.TokenV3.QuerySessionId(Token.Cache.Session).IsSuccess;

    tdTokenAuditPolicy:
      Result := Token.TokenV3.QueryAuditPolicy(Token.Cache.AuditPolicy).IsSuccess;

    tdTokenSandBoxInert:
      Result := Token.TokenV3.QuerySandboxInert(Token.Cache.SandboxInert).IsSuccess;

    tdTokenOrigin:
      Result := Token.TokenV3.QueryOrigin(Token.Cache.Origin).IsSuccess;

    tdTokenElevationInfo:
      Result := Token.TokenV3.QueryElevation(Token.Cache.ElevationInfo).IsSuccess;

    tdTokenHasRestrictions:
      Result := Token.TokenV3.QueryHasRestrictions(Token.Cache.HasRestrictions).IsSuccess;

    tdTokenFlags:
      Result := Token.TokenV3.QueryFlags(Token.Cache.Flags).IsSuccess;

    tdTokenVirtualizationAllowed:
      Result := Token.TokenV3.QueryVirtualizationAllowed(Token.Cache.VirtualizationAllowed).IsSuccess;

    tdTokenVirtualizationEnabled:
      Result := Token.TokenV3.QueryVirtualizationEnabled(Token.Cache.VirtualizationEnabled).IsSuccess;

    tdTokenIntegrity:
      Result := Token.TokenV3.QueryIntegrity(Token.Cache.Integrity).IsSuccess;

    tdTokenUIAccess:
      Result := Token.TokenV3.QueryUIAccess(Token.Cache.UIAccess).IsSuccess;

    tdTokenMandatoryPolicy:
      Result := Token.TokenV3.QueryMandatoryPolicy(Token.Cache.MandatoryPolicy).IsSuccess;

    tdTokenIsRestricted:
      Result := Token.TokenV3.QueryIsRestricted(Token.Cache.IsRestricted).IsSuccess;

    tdTokenAppContainer:
      Result := Token.TokenV3.QueryAppContainerSid(Token.Cache.AppContainer).IsSuccess;

    tdLogonInfo:
    if Query(tdTokenStatistics) then
    with Token.Cache.LogonSessionInfo do
    begin
      Result := True;
      LogonId :=  Token.Cache.Statistics.AuthenticationId;
      WellKnownSid := LsaxLookupKnownLogonSessionSid(LogonId);
      LsaxQueryLogonSession(LogonId, Detailed);
    end;

    tdObjectInfo:
      Result := Token.TokenV3.QueryBasicInfo(Token.Cache.ObjectInformation).IsSuccess;

    tdHandleInfo:
    begin
      Result := NtxEnumerateHandles(Handles).IsSuccess;

      if Result then
      begin
        TArray.FilterInline<TSystemHandleEntry>(Handles,
          ByProcess(NtCurrentProcessId));

        Result := NtxFindHandleEntry(Handles, NtCurrentProcessId,
          Token.hxToken.Handle, Token.Cache.HandleInformation).IsSuccess;
      end;
    end;
  end;

  Token.Cache.IsCached[DataClass] := Token.Cache.IsCached[DataClass] or Result;
end;

procedure TTokenData.SetAuditPolicy;
begin
  Token.TokenV3.SetAuditPolicy(Value).RaiseOnError;
end;

procedure TTokenData.SetDefaultDacl;
begin
  Token.TokenV3.SetDefaultDacl(Value).RaiseOnError;
end;

procedure TTokenData.SetIntegrityLevel;
begin
  Token.TokenV3.SetIntegrity(Value).RaiseOnError;
end;

procedure TTokenData.SetMandatoryPolicy;
begin
  Token.TokenV3.SetMandatoryPolicy(Value).RaiseOnError;
end;

procedure TTokenData.SetOrigin;
begin
  Token.TokenV3.SetOrigin(Value).RaiseOnError;
end;

procedure TTokenData.SetOwner;
begin
  Token.TokenV3.SetOwner(Value).RaiseOnError;
end;

procedure TTokenData.SetPrimaryGroup;
begin
  Token.TokenV3.SetPrimaryGroup(Value).RaiseOnError;
end;

procedure TTokenData.SetSession;
begin
  Token.TokenV3.SetSessionId(Value).RaiseOnError;
end;

procedure TTokenData.SetSessionReference;
begin
  Token.TokenV3.SetSessionReference(Value).RaiseOnError;
end;

procedure TTokenData.SetUIAccess;
begin
  Token.TokenV3.SetUIAccess(Value).RaiseOnError;
end;

procedure TTokenData.SetVirtualizationAllowed;
begin
  Token.TokenV3.SetVirtualizationAllowed(Value).RaiseOnError;
end;

procedure TTokenData.SetVirtualizationEnabled;
begin
  Token.TokenV3.SetVirtualizationEnabled(Value).RaiseOnError;
end;

procedure TTokenData.ValidateCache(DataClass: TTokenDataClass);
begin
  if Token.Cache.IsCached[DataClass] then
    ReQuery(DataClass);
end;

end.

