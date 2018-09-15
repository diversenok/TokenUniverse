unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  System.SysUtils, Winapi.Windows, System.Generics.Collections,
  TU.Tokens.Winapi, TU.Handles, TU.Common;

type
  TSIDNameUseHelper = record helper for TSIDNameUse
    function ToString: string;
  end;

  TSecurityIdentifier = record
  private
    procedure GetDomainAndUser(SrcSid: PSID);
    procedure GetStringSid(SrcSid: PSID);
  public
    SID, Domain, User: String;
    SIDType: TSIDNameUse;
    constructor CreateFromSid(SrcSid: PSID);
    constructor CreateFromStringSid(StringSID: string);
    constructor CreateFromUserName(Name: string);
    class function CreateWellKnown(WellKnownSidType: TWellKnownSidType):
      CanFail<TSecurityIdentifier>; static;
    function ToString: String;
    function HasPrettyName: Boolean;
  end;

  TGroupAttributes = (
    GroupMandatory = $00000001,
    GroupEnabledByDefault = $00000002,
    GroupEnabled = $00000004,
    GroupOwner = $00000008,
    GroupUforDenyOnly = $00000010,
    GroupIntegrity = $00000020,
    GroupIntegrityEnabled = $00000040,
    GroupResource = $20000000,
    GroupLogonId = Integer($C0000000)
  );

  TGroupAttributesHelper = record helper for TGroupAttributes
    function StateToString: String;
    function FlagsToString: String;
    function ContainAnyFlags: Boolean;
    function Contain(Flag: TGroupAttributes): Boolean;
  end;

  TGroup = record
    SecurityIdentifier: TSecurityIdentifier;
    Attributes: TGroupAttributes;
  end;

  TGroupArray = array of TGroup;
  TGroupAdjustAction = (gaResetDefault, gaEnable, gaDisable);

  TPrivilege = TLUIDAndAttributes;

  TPrivilegeHelper = record helper for TPrivilege
    function Name: String;
    function Description: String;
    function AttributesToString: String;
    function AttributesToDetailedString: String;
    function AttributesContain(Flag: Cardinal): Boolean;
  end;

  TPrivilegeArray = array of TPrivilege;
  TPrivilegeLUIDArray = array of TLargeInteger;

  TPrivilegeAdjustAction = (paEnable, paDisable, paRemove);

  TLuidHelper = record helper for LUID
    function ToString: String;
  end;

  TTokenTypeInfo = record
    TokenType: TTokenType;
    Impersonation: TSecurityImpersonationLevel;
    function ToString: String;
  end;

  TTokenElevationTypeHelper = record helper for TTokenElevationType
    function ToString: string;
  end;

  TTokenIntegrityLevel = (
    ilUntrusted = $0000,
    ilLow = $1000,
    ilMedium = $2000,
    ilMediumPlus = $2100,
    ilHigh = $3000,
    ilSystem = $4000
  );

  TTokenIntegrityLevelHelper = record helper for TTokenIntegrityLevel
    function IsWellKnown: Boolean;
  end;

  TTokenIntegrity = record
    SID: TSecurityIdentifier;
    Level: TTokenIntegrityLevel;
    function ToString: String;
    function ToDetailedString: String;
  end;

  TMandatoryPolicy = (
    TokenMandatoryPolicyOff,
    TokenMandatoryPolicyNoWriteUp,
    TokenMandatoryPolicyNewProcessMin,
    TokenMandatoryPolicyValidMask
  );

  TToken = class;
  TTokenEvents = class
  protected
    FTokenObjectAddress: NativeUInt;
    FReferenceCount: Integer;
    FOnSessionChange, FOnUIAccessChange: TEventHandler<CanFail<Cardinal>>;
    FOnIntegrityChange: TEventHandler<CanFail<TTokenIntegrity>>;
    FOnPolicyChange: TEventHandler<CanFail<TMandatoryPolicy>>;
    FOnPrivilegesChange: TEventHandler<CanFail<TPrivilegeArray>>;
    FOnGroupsChange: TEventHandler<CanFail<TGroupArray>>;
  public
    destructor Destroy; override;
    property TokenObjectAddress: NativeUInt read FTokenObjectAddress;
    property OnSessionChange: TEventHandler<CanFail<Cardinal>> read FOnSessionChange;
    property OnIntegrityChange: TEventHandler<CanFail<TTokenIntegrity>> read FOnIntegrityChange;
    property OnUIAccessChange: TEventHandler<CanFail<Cardinal>> read FOnUIAccessChange;
    property OnPolicyChange: TEventHandler<CanFail<TMandatoryPolicy>> read FOnPolicyChange;
    property OnPrivilegesChange: TEventHandler<CanFail<TPrivilegeArray>> read FOnPrivilegesChange;
    property OnGroupsChange: TEventHandler<CanFail<TGroupArray>> read FOnGroupsChange;
  end;

  TToken = class
  private
    procedure SetCaption(const Value: String);
    function GetAccess: CanFail<ACCESS_MASK>;
    function GetObjAddress: NativeUInt;
    function GetUser: CanFail<TSecurityIdentifier>;
    function GetGroups: CanFail<TGroupArray>;
    function GetPrivileges: CanFail<TPrivilegeArray>;
    function GetOwner: CanFail<TSecurityIdentifier>;
    function GetPrimaryGroup: CanFail<TSecurityIdentifier>;
    function GetTokenType: CanFail<TTokenTypeInfo>;
    function GetStatistics: CanFail<TTokenStatistics>;
    function GetSource: CanFail<TTokenSource>;
    function GetRestrictedSids: CanFail<TGroupArray>;
    function GetSandboxInert: CanFail<LongBool>;
    function GetTokenOrigin: CanFail<Int64>;
    function GetElevation: CanFail<TTokenElevationType>;
    function GetLinkedToken: CanFail<TToken>;
    function GetHasRestrictions: CanFail<LongBool>;
    function GetSession: Cardinal;
    procedure SetSession(const Value: Cardinal);
    function GetIntegrity: TTokenIntegrityLevel;
    procedure SetIntegrity(const Value: TTokenIntegrityLevel);
    function GetUIAccess: Cardinal;
    procedure SetUIAccess(const Value: Cardinal);
    function GetMandatoryPolicy: TMandatoryPolicy;
    procedure SetMandatoryPolicy(const Value: TMandatoryPolicy);
  protected
    function GetFixedSize<ResultType>(InfoClass: TTokenInformationClass):
      CanFail<ResultType>;
    procedure SetFixedSize<ResultType>(InfoClass: TTokenInformationClass;
      Value: ResultType);

    /// <remarks> The buffer should be freed using FreeMem. </remarks>
    function QueryVariableBuffer(InfoClass: TTokenInformationClass):
      CanFail<Pointer>;
    function QuerySid(InfoClass: TTokenInformationClass):
      CanFail<TSecurityIdentifier>;
    function QueryGroups(InfoClass: TTokenInformationClass):
      CanFail<TGroupArray>;

    /// <remarks>
    ///  The memory of each item should be freed with <c>LocalFree</c>.
    /// </remarks>
    class function ConverGroupArrayToSIDs(Groups: TGroupArray):
      TSIDAndAttributesArray; static;
    class procedure FreeSidArray(SIDs: TSIDAndAttributesArray); static;
  protected
    var hToken: THandle;
    var FOrigin: THandleItem;
    var FCaption: String;
    var FOnCanClose: TEventHandler<TToken>;
    var FOnClose: TEventHandler<TToken>;
    var FOnCaptionChange: TEventHandler<String>;

    /// <summary>
    ///  Different handles pointing to the same kernel objects must be linked to
    ///  the same event handlers. This value is a mapping of each opened kernel
    ///  object address to an event associated with it.
    /// </summary>
    class var EventMapping: TDictionary<NativeUInt, TTokenEvents>;
    class constructor CreateEventMapping;
    class destructor DestroyEventMapping;
    var FEvents: TTokenEvents;
    /// <remarks>
    ///  This procedure should be called after setting <c>hToken</c> field but
    ///  before invoking any events.
    /// </remarks>
    procedure InitializeEvents;
    procedure FinalizeEvents;
  public
    /// <summary> Opens a token of current process. </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateFromCurrent;

    /// <summary> Opens a token of another process. </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateFromProcess(PID: Cardinal);

    /// <summary> Creates a duplicate of a token. </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateDuplicate(SrcToken: TToken; Access: ACCESS_MASK;
      TokenImpersonation: TSecurityImpersonationLevel; TokenType: TTokenType);

    /// <summary> Duplicates a handle. The result handle references for the same
    ///   token object as the source handle. </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
      SameAccess: Boolean);

    /// <summary>
    ///   Tries to duplicate a token handle from another process.
    /// </summary>
    /// <remarks>
    ///   If <paramref name="hProcess"/> is zero then the object represents
    ///   a pseudo-token. In this case only access and kernel object pointer can
    ///   be successfully queried.
    /// </remarks>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor CreateFromHandleItem(Item: THandleItem; hProcess: THandle);

    /// <summary>
    ///  Uses <c>WTSQueryUserToken</c> to obtain a token of the specified
    ///  session.
    /// </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateQueryWts(SessionID: Cardinal);

    /// <summary> Uses <c>CreateRestrictedToken</c>. </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateRestricted(SrcToken: TToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TGroupArray;
      PrivilegesToDelete: TPrivilegeArray);

    /// <summary> Logons the user with the specified credentials. </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateWithLogon(LogonType, LogonProvider: Cardinal;
      Domain, User: String; Password: PWideChar);

    /// <summary>
    ///  Asks all subscribed event listeners if the token can be freed.
    /// </summary>
    /// <exception cref="EAbort"> Can raise EAbort. </exception>
    function CanBeFreed: Boolean;
    destructor Destroy; override;

    function IsValidToken: Boolean;
    property Handle: THandle read hToken;
    property Access: CanFail<ACCESS_MASK> read GetAccess;
    property ObjAddress: NativeUInt read GetObjAddress;
    property Caption: String read FCaption write SetCaption;

    /// <summary>
    ///  Asks all event listeners for confirmation before closing the token.
    ///  Any event listener can call <c>Abort;</c> to prevent further actions.
    /// </summary>
    property OnCanClose: TEventHandler<TToken> read FOnCanClose;
    property OnClose: TEventHandler<TToken> read FOnClose;
    property OnCaptionChange: TEventHandler<String> read FOnCaptionChange;
    property Events: TTokenEvents read FEvents write FEvents;

    { Token Information classes }

    function TryGetSession: CanFail<Cardinal>;
    function TryGetIntegrity: CanFail<TTokenIntegrity>;
    function TryGetUIAccess: CanFail<Cardinal>;
    function TryGetMandatoryPolicy: CanFail<TMandatoryPolicy>;

    property User: CanFail<TSecurityIdentifier> read GetUser;                   // class 1
    property Groups: CanFail<TGroupArray> read GetGroups;                       // class 2
    property Privileges: CanFail<TPrivilegeArray> read GetPrivileges;           // class 3
    property Owner: CanFail<TSecurityIdentifier> read GetOwner;                 // class 4 #settable
    property PrimaryGroup: CanFail<TSecurityIdentifier> read GetPrimaryGroup;   // class 5 #settable
    // TODO: class 6: DefaultDacl #settable
    property Source: CanFail<TTokenSource> read GetSource;                      // classes 7 & 8
    property TokenTypeInfo: CanFail<TTokenTypeInfo> read GetTokenType;          // class 9
    property Statistics: CanFail<TTokenStatistics> read GetStatistics;          // class 10
    property RestrictedSids: CanFail<TGroupArray> read GetRestrictedSids;       // class 11
    property Session: Cardinal read GetSession write SetSession;                // class 12 #settable
    // TODO: class 13 TokenGroupsAndPrivileges (maybe use for optimization)
    // TODO: class 14 SessionReference #settable (and not gettable?)
    property SandboxInert: CanFail<LongBool> read GetSandboxInert;              // class 15
    // TODO -cEnhancement: class 16 TokenAuditPolicy #settable
    property TokenOrigin: CanFail<Int64> read GetTokenOrigin;                   // class 17 #settable
    property Elevation: CanFail<TTokenElevationType> read GetElevation;         // classes 18 & 20
    property LinkedToken: CanFail<TToken> read GetLinkedToken;                  // class 19 #settable
    property HasRestrictions: CanFail<LongBool> read GetHasRestrictions;        // class 21
    // TODO: class 22 AccessInformation (depends on OS version, duplicates most of the info)
    // TODO: class 23 & 24 Virtualization #settable (both)
    property Integrity: TTokenIntegrityLevel read GetIntegrity write SetIntegrity; // class 25 #settable
    property UIAccess: Cardinal read GetUIAccess write SetUIAccess;             // class 26 #settable
    property MandatoryPolicy: TMandatoryPolicy read GetMandatoryPolicy write SetMandatoryPolicy; // class 27 #settable

    { Actions }

    function SendHandleToProcess(PID: Cardinal): NativeUInt;
    procedure PrivilegeAdjust(PrivilegeArray: TPrivilegeLUIDArray;
      Action: TPrivilegeAdjustAction);
    procedure GroupAdjust(GroupArray: TGroupArray; Action: TGroupAdjustAction);
  end;

const
  ACCESS_COUNT = 13;
  AccessValues: array [0 .. ACCESS_COUNT - 1] of Cardinal = (
    TOKEN_ASSIGN_PRIMARY, TOKEN_DUPLICATE, TOKEN_IMPERSONATE, TOKEN_QUERY,
    TOKEN_QUERY_SOURCE, TOKEN_ADJUST_PRIVILEGES, TOKEN_ADJUST_GROUPS,
    TOKEN_ADJUST_DEFAULT, TOKEN_ADJUST_SESSIONID, _DELETE, READ_CONTROL,
    WRITE_DAC, WRITE_OWNER);
  AccessStrings: array [0 .. ACCESS_COUNT - 1] of String = ('Assign primary',
    'Duplicate', 'Impersonate', 'Query', 'Query source', 'Adjust privileges',
    'Adjust groups', 'Adjust default', 'Adjust SessionId', 'Delete',
    'Read control', 'Write DAC', 'Write owner');

function AccessToString(Access: Cardinal): String;
function AccessToDetailedString(Access: Cardinal): String;
function TokeSourceNameToString(TokenSource: TTokenSource): String;
function NativeTimeToString(NativeTime: Int64): String;
function BytesToString(Size: Cardinal): String;
function YesNoToString(Value: LongBool): String;

implementation

uses
  TU.NativeAPI, TU.WtsApi;

{ TToken }

function TToken.CanBeFreed: Boolean;
begin
  OnCanClose.Invoke(Self);
  Result := True;
end;

class function TToken.ConverGroupArrayToSIDs(
  Groups: TGroupArray): TSIDAndAttributesArray;
var
  i: integer;
begin
  SetLength(Result, Length(Groups));
  for i := 0 to High(Result) do
    ConvertStringSidToSid(PWideChar(Groups[i].SecurityIdentifier.SID),
      Result[i].Sid);
end;

constructor TToken.CreateDuplicate(SrcToken: TToken; Access: ACCESS_MASK;
  TokenImpersonation: TSecurityImpersonationLevel; TokenType: TTokenType);
begin
  Win32Check(DuplicateTokenEx(SrcToken.hToken, Cardinal(Access), nil,
    TokenImpersonation, TokenType, hToken),
    'DuplicateTokenEx', SrcToken);
  FCaption := SrcToken.Caption + ' (copy)';
  InitializeEvents;
end;

constructor TToken.CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
  SameAccess: Boolean);
const
  Options: array [Boolean] of Cardinal = (0, DUPLICATE_SAME_ACCESS);
begin
  Win32Check(DuplicateHandle(GetCurrentProcess, SrcToken.hToken,
    GetCurrentProcess, @hToken, Access, False, Options[SameAccess]),
    'DuplicateHandle');

  if (SrcToken.FOrigin.OwnerPID = GetCurrentProcessId) or
    (SrcToken.FOrigin.OwnerPID = 0) then
    FCaption := SrcToken.Caption + ' (reference)'
  else
    FCaption := Format('Referenced 0x%x from PID %d', [SrcToken.FOrigin.hToken,
      SrcToken.FOrigin.OwnerPID]);

  InitializeEvents;
end;

class constructor TToken.CreateEventMapping;
begin
  EventMapping := TDictionary<NativeUInt, TTokenEvents>.Create;
end;

constructor TToken.CreateFromCurrent;
begin
  Win32Check(OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED, hToken),
    'OpenProcessToken');
  FCaption := 'Current process';
  InitializeEvents;
end;

constructor TToken.CreateFromHandleItem(Item: THandleItem; hProcess: THandle);
begin
  hToken := 0;
  FOrigin := Item;
  FCaption := Format('0x%x', [Item.hToken]);

  if hProcess <> 0 then
    DuplicateHandle(hProcess, Item.hToken, GetCurrentProcess, @hToken, 0, False,
      DUPLICATE_SAME_ACCESS);

  InitializeEvents;
end;

constructor TToken.CreateFromProcess(PID: Cardinal);
var
  hProcess: THandle;
begin
  hProcess := OpenProcess(MAXIMUM_ALLOWED, False, PID);
  if hProcess = 0 then
    Win32Check(False, 'OpenProcess');

  Win32Check(OpenProcessToken(hProcess, MAXIMUM_ALLOWED, hToken),
    'OpenProcessToken');
  FCaption := 'PID ' + IntToStr(PID);

  InitializeEvents;
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal);
begin
  Win32Check(WTSQueryUserToken(SessionID, hToken), 'WTSQueryUserToken');
  FCaption := Format('Token of session %d', [SessionID]);
  InitializeEvents;
end;

constructor TToken.CreateRestricted(SrcToken: TToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TGroupArray;
      PrivilegesToDelete: TPrivilegeArray);
var
  Disable, Restrict: TSIDAndAttributesArray;
begin
  Disable := ConverGroupArrayToSIDs(SIDsToDisabe);
  Restrict := ConverGroupArrayToSIDs(SIDsToRestrict);
  try
    Win32Check(CreateRestrictedToken(SrcToken.hToken, Flags,
      Length(Disable), Disable,
      Length(PrivilegesToDelete), PLUIDAndAttributes(PrivilegesToDelete),
      Length(Restrict), Restrict,
      hToken), 'CreateRestricted', SrcToken);

    FCaption := 'Restricted ' + SrcToken.Caption;
  finally
    FreeSidArray(Disable);
    FreeSidArray(Restrict);
  end;
  InitializeEvents;
end;

constructor TToken.CreateWithLogon(LogonType, LogonProvider: Cardinal; Domain,
  User: String; Password: PWideChar);
begin
  // TODO: switch to LogonUserExExW, it can add groups to a new token
  Win32Check(LogonUserW(PWideChar(User), PWideChar(Domain), Password, LogonType,
    LogonProvider, hToken), 'LogonUserW');
  FCaption := 'Logon of ' + User;
  InitializeEvents;
end;

destructor TToken.Destroy;
begin
  OnClose.Invoke(Self);
  if hToken <> 0 then
  try
    CloseHandle(hToken);
    hToken := 0;
  except
    ; // destructor should always succeed
  end;
  if Assigned(FEvents) then
    FinalizeEvents;

  if Length(FOnCanClose.Listeners) > 0 then
    OutputDebugString('Abandoned OnCanClose');
  if Length(FOnClose.Listeners) > 0 then
    OutputDebugString('Abandoned OnClose');
  if Length(FOnCaptionChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnCaptionChange');

  inherited;
end;

class destructor TToken.DestroyEventMapping;
begin
  if EventMapping.Count > 0 then
    OutputDebugString('DestroyEventMapping:: some element are still in the list');

  EventMapping.Free;
end;

procedure TToken.FinalizeEvents;
begin
  Dec(FEvents.FReferenceCount);
  if FEvents.FReferenceCount = 0 then
  begin
    EventMapping.Remove(FEvents.FTokenObjectAddress);
    FEvents.Free;
  end;
end;

class procedure TToken.FreeSidArray(SIDs: TSIDAndAttributesArray);
var
  i: integer;
begin
  for i := 0 to High(SIDs) do
    if Assigned(SIDs[i].Sid) then
      LocalFree(NativeUInt(SIDs[i].Sid));
end;

function TToken.GetAccess: CanFail<ACCESS_MASK>;
var
  info: TObjectBasicInformaion;
begin
  Result.Init(Self);

  // Pseudo-token mode
  if hToken = 0 then
    Exit(Result.Succeed(FOrigin.Access));

  if Result.CheckNativeError(NtQueryObject(hToken, ObjectBasicInformation,
    @info, SizeOf(info), nil), 'NtQueryObject') then
    Result.Succeed(info.GrantedAccess);
end;

function TToken.GetElevation: CanFail<TTokenElevationType>;
begin
  Result := GetFixedSize<TTokenElevationType>(TokenElevationType);
end;

function TToken.GetFixedSize<ResultType>(
  InfoClass: TTokenInformationClass): CanFail<ResultType>;
var
  ReturnLength: Cardinal;
begin
  Result.Init(Self);
  Result.CheckError(GetTokenInformation(hToken, InfoClass, @Result.Value,
    SizeOf(Result.Value), ReturnLength), GetterMessage(InfoClass));
end;

function TToken.GetGroups: CanFail<TGroupArray>;
begin
  Result := QueryGroups(TokenGroups);
end;

function TToken.GetHasRestrictions: CanFail<LongBool>;
begin
  Result := GetFixedSize<LongBool>(TokenHasRestrictions);
end;

function TToken.GetIntegrity: TTokenIntegrityLevel;
begin
  Result := TryGetIntegrity.GetValueOrRaise.Level;
end;

function TToken.GetLinkedToken: CanFail<TToken>;
begin
  Result.Init(Self);

  with GetFixedSize<THandle>(TokenLinkedToken) do
    if IsValid then
    begin
      Result.Value := TToken.Create;
      Result.Value.hToken := Value;
      Result.Value.FCaption := 'Linked token for ' + Caption;
      Result.Value.InitializeEvents;
      Result.Succeed;
    end
    else
    begin
      Result.IsValid := False;
      Result.ErrorCode := ErrorCode;
      Result.ErrorOrigin := ErrorOrigin;
    end;
end;

function TToken.GetMandatoryPolicy: TMandatoryPolicy;
begin
  Result := TryGetMandatoryPolicy.GetValueOrRaise;
end;

function TToken.GetObjAddress: NativeUInt;
var
  HandleList: THandleList;
  i: integer;
begin
  if FOrigin.KernelObjectAddress = 0 then // not yet obtained
  begin
    HandleList := THandleList.CreateOnly(GetCurrentProcessId);

    for i := 0 to HandleList.Count do
      if HandleList[i].hToken = hToken then
      begin
        FOrigin := HandleList[i];
        Break;
      end;

    HandleList.Free;
  end;

  Result := FOrigin.KernelObjectAddress
end;

function TToken.GetOwner: CanFail<TSecurityIdentifier>;
begin
  Result := QuerySid(TokenOwner);
end;

function TToken.GetPrimaryGroup: CanFail<TSecurityIdentifier>;
begin
  Result := QuerySid(TokenPrimaryGroup);
end;

function TToken.GetPrivileges: CanFail<TPrivilegeArray>;
var
  Buffer: PTokenPrivileges;
  i: integer;
begin
  with Result.CopyResult(QueryVariableBuffer(TokenPrivileges)) do
    if IsValid then
    begin
      Buffer := Value;
      try
        SetLength(Result.Value, Buffer.PrivilegeCount);
        for i := 0 to Buffer.PrivilegeCount - 1 do
          Result.Value[i] := Buffer.Privileges[i];
      finally
        FreeMem(Buffer);
      end;
    end;
end;

function TToken.GetRestrictedSids: CanFail<TGroupArray>;
begin
  Result := QueryGroups(TokenRestrictedSids);
end;

function TToken.GetSandboxInert: CanFail<LongBool>;
begin
  Result := GetFixedSize<LongBool>(TokenSandBoxInert);
end;

function TToken.GetSession: Cardinal;
begin
  Result := TryGetSession.GetValueOrRaise;
end;

function TToken.GetSource: CanFail<TTokenSource>;
begin
  Result := GetFixedSize<TTokenSource>(TokenSource);
end;

function TToken.GetStatistics: CanFail<TTokenStatistics>;
begin
  Result := GetFixedSize<TTokenStatistics>(TokenStatistics);
end;

function TToken.GetTokenOrigin: CanFail<Int64>;
begin
  Result := GetFixedSize<Int64>(TTokenInformationClass.TokenOrigin);
end;

function TToken.GetTokenType: CanFail<TTokenTypeInfo>;
var
 ReturnValue: Cardinal;
begin
  Result.Init(Self);

  if not Result.CheckError(GetTokenInformation(hToken, TokenType,
    @Result.Value.TokenType, SizeOf(Result.Value.TokenType), ReturnValue),
    GetterMessage(TokenType)) then
    Exit;

  if Result.Value.TokenType = TokenImpersonation then
    Result.CheckError(GetTokenInformation(hToken, TokenImpersonationLevel,
    @Result.Value.Impersonation, SizeOf(Result.Value.Impersonation),
    ReturnValue), GetterMessage(TokenImpersonationLevel));
end;

function TToken.GetUIAccess: Cardinal;
begin
  Result := TryGetUIAccess.GetValueOrRaise;
end;

function TToken.GetUser: CanFail<TSecurityIdentifier>;
var
  Buffer: PSIDAndAttributes;
begin
  with Result.CopyResult(QueryVariableBuffer(TokenUser)) do
    if IsValid then
    begin
      Buffer := Value;
      try
        Result.Succeed(TSecurityIdentifier.CreateFromSid(Buffer.Sid));
      finally
        FreeMem(Buffer);
      end;
    end;
end;

procedure TToken.GroupAdjust(GroupArray: TGroupArray; Action:
  TGroupAdjustAction);
const
  IsResetFlag: array [TGroupAdjustAction] of LongBool = (True, False, False);
var
  i: integer;
  GroupsArray: TSIDAndAttributesArray;
  NewState: PTokenGroups;
begin
  NewState := AllocMem(SizeOf(Integer) + SizeOf(TSIDAndAttributes) *
    Length(GroupArray));
  NewState.GroupCount := Length(GroupArray);

  GroupsArray := ConverGroupArrayToSIDs(GroupArray);
  for i := 0 to High(GroupsArray) do
    NewState.Groups[i] := GroupsArray[i];

  if Action = gaEnable then
    for i := 0 to NewState.GroupCount - 1 do
      NewState.Groups[i].Attributes := Cardinal(GroupEnabled);
  try
    Win32Check(AdjustTokenGroups(hToken, IsResetFlag[Action], NewState, 0, nil,
      nil), 'AdjustTokenGroups', Self);
    Events.OnGroupsChange.Invoke(Groups);
  finally
    FreeMem(NewState);
    FreeSidArray(GroupsArray);
  end;
end;

procedure TToken.InitializeEvents;
begin
  if EventMapping.TryGetValue(ObjAddress, FEvents) then
    Inc(FEvents.FReferenceCount)
  else
  begin
    FEvents := TTokenEvents.Create;
    FEvents.FTokenObjectAddress := ObjAddress;
    FEvents.FReferenceCount := 1;
    EventMapping.Add(ObjAddress, FEvents);
  end;
end;

function TToken.IsValidToken: Boolean;
begin
  Result := hToken <> 0;
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

    Win32Check(AdjustTokenPrivileges(hToken, False, Buffer, BufferSize, nil,
      nil) and (GetLastError = ERROR_SUCCESS), 'AdjustTokenPrivileges', Self);
  finally
    FreeMem(Buffer);
    Events.OnPrivilegesChange.Invoke(GetPrivileges);
  end;
end;

function TToken.QueryGroups(InfoClass: TTokenInformationClass):
  CanFail<TGroupArray>;
var
  Buffer: PTokenGroups;
  i: integer;
begin
  with Result.CopyResult(QueryVariableBuffer(InfoClass)) do
    if IsValid then
    begin
      Buffer := Value;
      try
        SetLength(Result.Value, Buffer.GroupCount);
        for i := 0 to Buffer.GroupCount - 1 do
        with Result.Value[i] do
        begin
          SecurityIdentifier.CreateFromSid(Buffer.Groups[i].Sid);
          Attributes := TGroupAttributes(Buffer.Groups[i].Attributes);
        end;
      finally
        FreeMem(Buffer);
      end;
    end;
end;

function TToken.QuerySid(InfoClass: TTokenInformationClass):
  CanFail<TSecurityIdentifier>;
var
  Buffer: PTokenOwner; // aka TTokenPrimaryGroup aka PPSID
begin
  with Result.CopyResult(QueryVariableBuffer(InfoClass)) do
    if IsValid then
    begin
      Buffer := Value;
      try
        Result.Value := TSecurityIdentifier.CreateFromSid(Buffer.Owner);
      finally
        FreeMem(Buffer);
      end;
    end;
end;

function TToken.QueryVariableBuffer(
  InfoClass: TTokenInformationClass): CanFail<Pointer>;
var
  BufferSize, ReturnValue: Cardinal;
begin
  Result.Init(Self);

  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);
  if not Result.CheckBuffer(BufferSize, GetterMessage(InfoClass)) then
    Exit;

  Result.Value := AllocMem(BufferSize);
  if not Result.CheckError(GetTokenInformation(hToken, InfoClass, Result.Value,
    BufferSize, ReturnValue), GetterMessage(InfoClass)) then
  begin
    FreeMem(Result.Value);
    Result.Value := nil;
  end;
end;

function TToken.SendHandleToProcess(PID: Cardinal): NativeUInt;
var
  hTargetProcess: THandle;
begin
  hTargetProcess := OpenProcess(PROCESS_DUP_HANDLE, False, PID);
  Win32Check(LongBool(hTargetProcess), 'OpenProcess#PROCESS_DUP_HANDLE');

  if not DuplicateHandle(GetCurrentProcess, hToken, hTargetProcess, @Result, 0,
    False, DUPLICATE_SAME_ACCESS) then
    Win32Check(False, 'DuplicateHandle')
  else
    CloseHandle(hTargetProcess);
end;

procedure TToken.SetCaption(const Value: String);
begin
  FCaption := Value;
  OnCaptionChange.Invoke(FCaption);
end;

procedure TToken.SetFixedSize<ResultType>(InfoClass: TTokenInformationClass;
  Value: ResultType);
begin
  if not SetTokenInformation(hToken, InfoClass, @Value, SizeOf(Value))
    then
    raise ELocatedOSError.CreateLE(GetLastError, SetterMessage(InfoClass));
end;

procedure TToken.SetIntegrity(const Value: TTokenIntegrityLevel);
const
  SECURITY_MANDATORY_LABEL_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 16));
var
  mandatoryLabel: TSIDAndAttributes;
begin
  mandatoryLabel.Sid := AllocMem(GetSidLengthRequired(1));
  try
    InitializeSid(mandatoryLabel.Sid, SECURITY_MANDATORY_LABEL_AUTHORITY, 1);
    GetSidSubAuthority(mandatoryLabel.Sid, 0)^ := DWORD(Value);
    mandatoryLabel.Attributes := SE_GROUP_INTEGRITY;
    Win32Check(SetTokenInformation(hToken, TokenIntegrityLevel, @mandatoryLabel,
      SizeOf(TSIDAndAttributes)), SetterMessage(TokenIntegrityLevel));
  finally
    FreeMem(mandatoryLabel.Sid);
  end;
  Events.OnIntegrityChange.Invoke(TryGetIntegrity);

  // Integrity can disable privileges
  Events.OnPrivilegesChange.Invoke(GetPrivileges);

  // And has it's own record in group list
  Events.OnGroupsChange.Invoke(Groups);
end;

procedure TToken.SetMandatoryPolicy(const Value: TMandatoryPolicy);
begin
  SetFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy, Value);
  Events.OnPolicyChange.Invoke(TryGetMandatoryPolicy);
end;

procedure TToken.SetSession(const Value: Cardinal);
begin
  SetFixedSize<Cardinal>(TokenSessionId, Value);
  Events.OnSessionChange.Invoke(TryGetSession);
end;

procedure TToken.SetUIAccess(const Value: Cardinal);
begin
  SetFixedSize<Cardinal>(TokenUIAccess, Value);
  Events.OnUIAccessChange.Invoke(TryGetUIAccess);
end;

function TToken.TryGetIntegrity: CanFail<TTokenIntegrity>;
var
  Buffer: PSIDAndAttributes;
begin
  with Result.CopyResult(QueryVariableBuffer(TokenIntegrityLevel)) do
    if IsValid then
    begin
      Buffer := Value;
      try
        Result.Value.SID := TSecurityIdentifier.CreateFromSid(Buffer.Sid);
        Result.Value.Level := TTokenIntegrityLevel(GetSidSubAuthority(
          Buffer.Sid, 0)^);
      finally
        FreeMem(Buffer);
      end;
    end;
end;

function TToken.TryGetMandatoryPolicy: CanFail<TMandatoryPolicy>;
begin
  Result := GetFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy);
end;

function TToken.TryGetSession: CanFail<Cardinal>;
begin
  Result := GetFixedSize<Cardinal>(TokenSessionId);
end;

function TToken.TryGetUIAccess: CanFail<Cardinal>;
begin
  Result := GetFixedSize<Cardinal>(TokenUIAccess);
end;

{ TTokenAccess }

function AccessToDetailedString(Access: Cardinal): String;
begin
  Result := Format('%s (0x%0.6x)', [AccessToString(Access), Access]);
end;

function AccessToString(Access: Cardinal): String;
var
  Granted: array of string;
  Right, StrInd: integer;
begin
  if Access = TOKEN_ALL_ACCESS then
    Exit('Full access');

  if Access = 0 then
    Exit('No access');

  SetLength(Granted, ACCESS_COUNT);
  StrInd := 0;
  for Right := 0 to ACCESS_COUNT - 1 do
  if Access and AccessValues[Right] = AccessValues[Right] then
    begin
      Granted[StrInd] := AccessStrings[Right];
      Inc(StrInd);
    end;
  SetLength(Granted, StrInd);
  Result := String.Join(', ', Granted);
end;

{ TSIDNameUseHelper }

function TSIDNameUseHelper.ToString: string;
begin
  case Self of
    SidTypeZero: Result := 'Undefined';
    SidTypeUser: Result := 'User';
    SidTypeGroup: Result := 'Group';
    SidTypeDomain: Result := 'Domain';
    SidTypeAlias: Result := 'Alias';
    SidTypeWellKnownGroup:  Result := 'Well-known Group';
    SidTypeDeletedAccount: Result := 'Deleted Account';
    SidTypeInvalid: Result := 'Invalid';
    SidTypeUnknown: Result := 'Unknown';
    SidTypeComputer: Result := 'Computer';
    SidTypeLabel: Result := 'Label';
  else
    Result := Format('%d (out of bound)', [Cardinal(Self)]);
  end;
end;

{ TSecurityIdentifier }

constructor TSecurityIdentifier.CreateFromSid(SrcSid: PSID);
begin
  GetStringSid(SrcSid);
  GetDomainAndUser(SrcSid);
end;

constructor TSecurityIdentifier.CreateFromStringSid(StringSID: string);
var
  Buffer: PSID;
begin
  SID := StringSID;
  if Win32Check(ConvertStringSidToSid(PWideChar(SID), Buffer),
    'ConvertStringSidToSid') then
  try
    GetDomainAndUser(Buffer);
  finally
    LocalFree(NativeUInt(Buffer));
  end;
end;

constructor TSecurityIdentifier.CreateFromUserName(Name: string);
var
  SidBuffer, DomainBuffer: Pointer;
  SidSize, DomainChars, Reserved2: Cardinal;
begin
  SidSize := 0;
  DomainChars := 0;
  LookupAccountNameW(nil, PWideChar(Name), nil, SidSize, nil,
    DomainChars, Reserved2);
  Win32CheckBuffer(SidSize, 'LookupAccountNameW');

  SidBuffer := AllocMem(SidSize);
  DomainBuffer := AllocMem((DomainChars + 1) * SizeOf(WideChar));
  try
    Win32Check(LookupAccountNameW(nil, PWideChar(Name), SidBuffer, SidSize,
      DomainBuffer, DomainChars, Reserved2), 'LookupAccountNameW');

    CreateFromSid(SidBuffer);
  finally
    FreeMem(SidBuffer);
    FreeMem(DomainBuffer);
  end;
end;

class function TSecurityIdentifier.CreateWellKnown(
  WellKnownSidType: TWellKnownSidType): CanFail<TSecurityIdentifier>;
var
  Buffer: PSID;
  BufferSize: Cardinal;
begin
  Result.Init;

  BufferSize := 0;
  CreateWellKnownSid(WellKnownSidType, nil, nil, BufferSize);
  if not Result.CheckBuffer(BufferSize, 'CreateWellKnownSid') then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    if Result.CheckError(CreateWellKnownSid(WellKnownSidType, nil, Buffer,
      BufferSize), 'CreateWellKnownSid') then
      Result.Value.CreateFromSid(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TSecurityIdentifier.GetDomainAndUser(SrcSid: PSID);
var
  BufUser, BufDomain: PWideChar;
  UserChars, DomainChars, peUse: Cardinal;
begin
  Domain := '';
  User := '';
  SIDType := SidTypeZero;

  UserChars := 0;
  DomainChars := 0;
  LookupAccountSidW(nil, SrcSid, nil, UserChars, nil, DomainChars, peUse);
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or
    ((UserChars = 0) and (DomainChars = 0)) then
    Exit;

  BufUser := AllocMem((UserChars + 1) * SizeOf(WideChar));
  BufDomain := AllocMem((DomainChars + 1) * SizeOf(WideChar));
  try
    if LookupAccountSidW(nil, SrcSid, BufUser, UserChars, BufDomain,
      DomainChars, peUse) then // We don't need exceptions
    begin
      SIDType := TSIDNameUse(peUse);
      if UserChars <> 0 then
        SetString(User, BufUser, UserChars);
      if DomainChars <> 0 then
        SetString(Domain, BufDomain, DomainChars);
    end;
  finally
    FreeMem(BufUser);
    FreeMem(BufDomain);
  end;
end;

procedure TSecurityIdentifier.GetStringSid(SrcSid: PSID);
var
  Buffer: PWideChar;
begin
  SID := '';
  if Win32Check(ConvertSidToStringSidW(SrcSid, Buffer),
    'ConvertSidToStringSidW') then
  begin
    SID := String(Buffer);
    LocalFree(NativeUInt(Buffer));
  end;
end;

function TSecurityIdentifier.HasPrettyName: Boolean;
begin
  Result := (Domain <> '') or (User <> '');
end;

function TSecurityIdentifier.ToString: String;
begin
 if (User <> '') and (Domain <> '') then
    Result := Domain + '\' + User
  else if User <> '' then
    Result := User
  else if Domain <> '' then
    Result := Domain
  else if SID <> '' then
    Result := SID
  else
    Result := 'Invalid SID';
  // TODO: Convert unknown IL and Logon session
end;

{ TGroupAttributesHelper }

function TGroupAttributesHelper.Contain(Flag: TGroupAttributes): Boolean;
begin
  Result := Cardinal(Self) and Cardinal(Flag) = Cardinal(Flag);
end;

function TGroupAttributesHelper.ContainAnyFlags: Boolean;
const
  AllFlags = Cardinal(GroupMandatory) or Cardinal(GroupOwner) or
    Cardinal(GroupIntegrity) or Cardinal(GroupResource) or
    Cardinal(GroupLogonId) or Cardinal(GroupUforDenyOnly);
begin
  Result := Cardinal(Self) and AllFlags <> 0;
end;

function TGroupAttributesHelper.FlagsToString: String;
const
  GROUP_FLAGS_COUNT = 6;
  FlagValues: array [1 .. GROUP_FLAGS_COUNT] of TGroupAttributes = (
    GroupMandatory, GroupOwner, GroupIntegrity, GroupResource, GroupLogonId,
    GroupUforDenyOnly);
  FlagStrings: array [1 .. GROUP_FLAGS_COUNT] of String = (
    'Mandatory', 'Owner', 'Integrity', 'Resource', 'Logon Id',
    'Use for deny only');
var
  Strings: array of string;
  FlagInd, StrInd: Integer;
begin
  if not ContainAnyFlags then
    Exit('');

  SetLength(Strings, GROUP_FLAGS_COUNT);
  StrInd := 0;
  for FlagInd := 1 to GROUP_FLAGS_COUNT do
    if Cardinal(Self) and Cardinal(FlagValues[FlagInd]) =
      Cardinal(FlagValues[FlagInd]) then
    begin
      Strings[StrInd] := FlagStrings[FlagInd];
      Inc(StrInd);
    end;
  SetLength(Strings, StrInd);
  Result := String.Join(', ', Strings);
end;

function TGroupAttributesHelper.StateToString: String;
begin
  if Self.Contain(GroupEnabled) then
  begin
    if Self.Contain(GroupEnabledByDefault) then
      Result := 'Enabled'
    else
      Result := 'Enabled (modified)';
  end
  else
  begin
    if Self.Contain(GroupEnabledByDefault) then
      Result := 'Disabled (modified)'
    else
      Result := 'Disabled';
  end;

  if Self.Contain(GroupIntegrityEnabled) then
  begin
    if Self.Contain(GroupEnabled) or Self.Contain(GroupEnabledByDefault) then
      Result := 'Integrity Enabled, Group ' + Result
    else
      Exit('Integrity Enabled');
  end;
end;

{ TPrivilegeHelper }

function TPrivilegeHelper.AttributesContain(Flag: Cardinal): Boolean;
begin
  Result := Self.Attributes and Flag = Flag;
end;

function TPrivilegeHelper.AttributesToDetailedString: String;
begin
  Result := Format('0x%0.4x: %s', [Self.Attributes, Self.AttributesToString]);
end;

function TPrivilegeHelper.AttributesToString: String;
begin
  if Self.AttributesContain(SE_PRIVILEGE_ENABLED) then
  begin
    if Self.AttributesContain(SE_PRIVILEGE_ENABLED_BY_DEFAULT) then
      Result := 'Enabled'
    else
      Result := 'Enabled (modified)';
  end
  else
  begin
    if Self.AttributesContain(SE_PRIVILEGE_ENABLED_BY_DEFAULT) then
      Result := 'Disabled (modified)'
    else
      Result := 'Disabled';
  end;

  if Self.AttributesContain(SE_PRIVILEGE_REMOVED) then
    Result := 'Removed, ' + Result;

  if Self.AttributesContain(SE_PRIVILEGE_USED_FOR_ACCESS) then
    Result := 'Used for access, ' + Result;
end;

function TPrivilegeHelper.Description: String;
var
  Buffer: PWideChar;
  BufferChars, LangId: Cardinal;
begin
  BufferChars := 0;
  LookupPrivilegeDisplayNameW(nil, PWideChar(Name), nil, BufferChars, LangId);
  Win32CheckBuffer(BufferChars, 'LookupPrivilegeDisplayNameW');

  Buffer := AllocMem((BufferChars + 1) * SizeOf(WideChar));
  try
    Win32Check(LookupPrivilegeDisplayNameW(nil, PWideChar(Name), Buffer,
      BufferChars, LangId), 'LookupPrivilegeDisplayNameW');

    SetString(Result, Buffer, BufferChars);
  finally
    FreeMem(Buffer);
  end;
end;

function TPrivilegeHelper.Name: String;
var
  Buffer: PWideChar;
  BufferChars: Cardinal;
begin
  BufferChars := 0;
  LookupPrivilegeNameW(nil, Self.Luid, nil, BufferChars);

  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BufferChars = 0) then
    Exit(Format('Unknown privilege %d', [Self.Luid]));

  Buffer := AllocMem((BufferChars + 1) * SizeOf(WideChar));
  try
    if LookupPrivilegeNameW(nil, Self.Luid, Buffer, BufferChars) then
      SetString(Result, Buffer, BufferChars);
  finally
    FreeMem(Buffer);
  end;
end;

{ TLuidHelper }

function TLuidHelper.ToString: String;
begin
  Result := Format('0x%x', [PInt64(@Self)^]);
end;

{ TTokenTypeInfo }

function TTokenTypeInfo.ToString: String;
begin
  if TokenType = TokenPrimary then
    Result := 'Primary token'
  else
    case Impersonation of
      SecurityAnonymous: Result :=  'Anonymous';
      SecurityIdentification: Result := 'Identification';
      SecurityImpersonation: Result := 'Impersonation';
      SecurityDelegation: Result := 'Delegation';
    end;
end;

{ TTokenElevationTypeHelper }

function TTokenElevationTypeHelper.ToString: string;
begin
  case Self of
    TokenElevationTypeDefault: Result := 'N/A';
    TokenElevationTypeFull: Result := 'Yes';
    TokenElevationTypeLimited: Result := 'No';
  end;
end;

{ TTokenIntegrityLevelHelper }

function TTokenIntegrityLevelHelper.IsWellKnown: Boolean;
begin
  case Self of
    ilUntrusted, ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem:
      Result := True;
  else
    Result := False;
  end;
end;

{ TTokenIntegrity }

function TTokenIntegrity.ToDetailedString: String;
begin
  if SID.User <> '' then
    Result := SID.ToString
  else
    Result := Format('Intermediate Mandatory Level: 0x%0.4x', [Cardinal(Level)]);
end;

function TTokenIntegrity.ToString: String;
begin
  case Self.Level of
    ilUntrusted: Result := 'Untrusted';
    ilLow: Result := 'Low';
    ilMedium: Result := 'Medium';
    ilMediumPlus: Result := 'Medium +';
    ilHigh: Result := 'High';
    ilSystem: Result := 'System';
  else
    Result := 'Intermediate';
  end;
end;

{ TTokenEvents }

destructor TTokenEvents.Destroy;
begin
  if Length(FOnSessionChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnSessionChange');
  if Length(FOnSessionChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnSessionChange');
  if Length(FOnIntegrityChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnIntegrityChange');
  if Length(FOnUIAccessChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnUIAccessChange');
  if Length(FOnPolicyChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnPolicyChange');
  if Length(FOnPrivilegesChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnPrivilegesChange');
  if Length(FOnGroupsChange.Listeners) > 0 then
    OutputDebugString('Abandoned OnGroupsChange');
  inherited;
end;

{ Conversion functions }

function TokeSourceNameToString(TokenSource: TTokenSource): String;
begin
  // sourcename field may or may not contain zero-termination byte
  Result := String(PAnsiChar(AnsiString(TokenSource.sourcename)));
end;

function NativeTimeToString(NativeTime: Int64): String;
begin
  if NativeTime = Int64.MaxValue then
    Result := 'Infinite'
  else
    Result := DateTimeToStr(NativeTimeToLocalDateTime(NativeTime));
end;

function BytesToString(Size: Cardinal): String;
begin
  if Size mod 1024 = 0 then
    Result := (Size div 1024).ToString + ' kB'
  else
    Result := Size.ToString + ' B';
end;

function YesNoToString(Value: LongBool): String;
begin
  if Value then
    Result := 'Yes'
  else
    Result := 'No';
end;

end.
