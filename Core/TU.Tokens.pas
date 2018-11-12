unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
uses
  System.SysUtils, Winapi.Windows, System.Generics.Collections,
  TU.Tokens.Winapi, TU.Tokens.Types,
  TU.Handles, TU.Common;

type
  TToken = class;
  TTokenEvents = class
  strict private
    FOnSessionChange, FOnUIAccessChange: TValuedEventHandler<Cardinal>;
    FOnIntegrityChange: TValuedEventHandler<TTokenIntegrity>;
    FOnPolicyChange: TValuedEventHandler<TMandatoryPolicy>;
    FOnPrivilegesChange: TValuedEventHandler<TPrivilegeArray>;
    FOnGroupsChange: TValuedEventHandler<TGroupArray>;
    FOnStatisticsChange: TValuedEventHandler<TTokenStatistics>;
    class function CompareCardinals(Value1, Value2: Cardinal): Boolean; static;
    class function CompareIntegrities(Value1, Value2: TTokenIntegrity): Boolean; static;
    class function ComparePolicies(Value1, Value2: TMandatoryPolicy): Boolean; static;
    class function ComparePrivileges(Value1, Value2: TPrivilegeArray): Boolean; static;
    class function CompareGroups(Value1, Value2: TGroupArray): Boolean; static;
    class function CompareStatistics(Value1, Value2: TTokenStatistics): Boolean; static;
  private
    FTokenObjectAddress: NativeUInt;
    FReferenceCount: Integer;
    FUpdateFlag: Integer;
    procedure UpdateStatistics(Token: TToken);
    procedure UpdatePrivileges(Token: TToken);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property TokenObjectAddress: NativeUInt read FTokenObjectAddress;
    property OnSessionChange: TValuedEventHandler<Cardinal> read FOnSessionChange;
    property OnIntegrityChange: TValuedEventHandler<TTokenIntegrity> read FOnIntegrityChange;
    property OnUIAccessChange: TValuedEventHandler<Cardinal> read FOnUIAccessChange;
    property OnPolicyChange: TValuedEventHandler<TMandatoryPolicy> read FOnPolicyChange;
    property OnPrivilegesChange: TValuedEventHandler<TPrivilegeArray> read FOnPrivilegesChange;
    property OnGroupsChange: TValuedEventHandler<TGroupArray> read FOnGroupsChange;
    property OnStatisticsChange: TValuedEventHandler<TTokenStatistics> read FOnStatisticsChange;
  end;

  TToken = class
  strict private
    procedure SetCaption(const Value: String);
    function GetAccess: CanFail<ACCESS_MASK>;
    function GetObjAddress: NativeUInt;
    function GetUser: CanFail<TGroup>;
    function GetGroups: CanFail<TGroupArray>;
    function GetPrivileges: CanFail<TPrivilegeArray>;
    function GetOwner: CanFail<TSecurityIdentifier>;
    function GetPrimaryGroup: CanFail<TSecurityIdentifier>;
    function GetTokenType: CanFail<TTokenTypeEx>;
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
  strict protected
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
    class function ConvertGroupArrayToSIDs(Groups: TGroupArray):
      TSIDAndAttributesArray; static;
    class procedure FreeSidArray(SIDs: TSIDAndAttributesArray); static;
  protected
    var hToken: THandle;
    var FOrigin: THandleInformation;
    var FCaption: String;
    var FOnCanClose: TEventHandler<TToken>;
    var FOnClose: TEventHandler<TToken>;
    var FOnCaptionChange: TValuedEventHandler<String>;

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
      TokenTypeEx: TTokenTypeEx);

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
    constructor CreateFromHandleItem(Item: THandleInformation; hProcess: THandle);

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
      Domain, User: String; Password: PWideChar; AddGroups: TGroupArray);

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
    property OnCaptionChange: TValuedEventHandler<String> read FOnCaptionChange;
    property Events: TTokenEvents read FEvents write FEvents;

    { Token Information classes }

    function TryGetSession: CanFail<Cardinal>;
    function TryGetIntegrity: CanFail<TTokenIntegrity>;
    function TryGetUIAccess: CanFail<Cardinal>;
    function TryGetMandatoryPolicy: CanFail<TMandatoryPolicy>;

    property User: CanFail<TGroup> read GetUser;                                // class 1
    property Groups: CanFail<TGroupArray> read GetGroups;                       // class 2
    property Privileges: CanFail<TPrivilegeArray> read GetPrivileges;           // class 3
    property Owner: CanFail<TSecurityIdentifier> read GetOwner;                 // class 4 #settable
    property PrimaryGroup: CanFail<TSecurityIdentifier> read GetPrimaryGroup;   // class 5 #settable
    // TODO: class 6: DefaultDacl #settable
    property Source: CanFail<TTokenSource> read GetSource;                      // classes 7 & 8
    property TokenTypeInfo: CanFail<TTokenTypeEx> read GetTokenType;            // class 9
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

implementation

uses
  TU.NativeAPI, TU.WtsApi;

{ TToken }

function TToken.CanBeFreed: Boolean;
begin
  OnCanClose.Invoke(Self);
  Result := True;
end;

class function TToken.ConvertGroupArrayToSIDs(
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
  TokenTypeEx: TTokenTypeEx);
begin
  WinCheck(DuplicateTokenEx(SrcToken.hToken, Cardinal(Access), nil,
    TokenTypeEx.SecurityImpersonationLevel, TokenTypeEx.TokenTypeValue, hToken),
    'DuplicateTokenEx', SrcToken);
  FCaption := SrcToken.Caption + ' (copy)';
  InitializeEvents;
end;

constructor TToken.CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
  SameAccess: Boolean);
const
  Options: array [Boolean] of Cardinal = (0, DUPLICATE_SAME_ACCESS);
begin
  WinCheck(DuplicateHandle(GetCurrentProcess, SrcToken.hToken,
    GetCurrentProcess, @hToken, Access, False, Options[SameAccess]),
    'DuplicateHandle');

  if (SrcToken.FOrigin.ContextPID = GetCurrentProcessId) or
    (SrcToken.FOrigin.ContextPID = 0) then
    FCaption := SrcToken.Caption + ' (reference)'
  else
    FCaption := Format('Referenced 0x%x from PID %d', [SrcToken.FOrigin.Handle,
      SrcToken.FOrigin.ContextPID]);

  InitializeEvents;
end;

class constructor TToken.CreateEventMapping;
begin
  EventMapping := TDictionary<NativeUInt, TTokenEvents>.Create;
end;

constructor TToken.CreateFromCurrent;
begin
  WinCheck(OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED, hToken),
    'OpenProcessToken');
  FCaption := 'Current process';
  InitializeEvents;
end;

constructor TToken.CreateFromHandleItem(Item: THandleInformation; hProcess: THandle);
begin
  hToken := 0;
  FOrigin := Item;
  FCaption := Format('Handle 0x%x (%d)', [Item.Handle, Item.Handle]);

  if hProcess <> 0 then
    DuplicateHandle(hProcess, Item.Handle, GetCurrentProcess, @hToken, 0, False,
      DUPLICATE_SAME_ACCESS)
  else
    hToken := Item.Handle;

  InitializeEvents;
end;

constructor TToken.CreateFromProcess(PID: Cardinal);
var
  hProcess: THandle;
begin
  hProcess := OpenProcess(MAXIMUM_ALLOWED, False, PID);
  if hProcess = 0 then
    WinCheck(False, 'OpenProcess');

  WinCheck(OpenProcessToken(hProcess, MAXIMUM_ALLOWED, hToken),
    'OpenProcessToken');
  FCaption := 'PID ' + IntToStr(PID);

  InitializeEvents;
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal);
begin
  WinCheck(WTSQueryUserToken(SessionID, hToken), 'WTSQueryUserToken');
  FCaption := Format('Token of session %d', [SessionID]);
  InitializeEvents;
end;

constructor TToken.CreateRestricted(SrcToken: TToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TGroupArray;
      PrivilegesToDelete: TPrivilegeArray);
var
  Disable, Restrict: TSIDAndAttributesArray;
begin
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
  InitializeEvents;
end;

constructor TToken.CreateWithLogon(LogonType, LogonProvider: Cardinal; Domain,
  User: String; Password: PWideChar; AddGroups: TGroupArray);
var
  SIDs: TSIDAndAttributesArray;
  pGroups: PTokenGroups;
  i: integer;
begin
  if Length(AddGroups) = 0 then
    WinCheck(LogonUserW(PWideChar(User), PWideChar(Domain), Password,
      LogonType, LogonProvider, hToken), 'LogonUserW')
  else
  begin
    SIDs := ConvertGroupArrayToSIDs(AddGroups);
    pGroups := AllocMem(SizeOf(Integer) + SizeOf(TSIDAndAttributes) *
      Length(AddGroups));
    try
      pGroups.GroupCount := Length(SIDs);
      for i := 0 to High(SIDs) do
        pGroups.Groups[i] := SIDs[i];

      WinCheck(LogonUserExExW(PWideChar(User), PWideChar(Domain), Password,
        LogonType, LogonProvider, pGroups, hToken, nil, nil, nil, nil),
        'LogonUserExExW');
    finally
      FreeMem(pGroups);
      FreeSidArray(SIDs);
    end;
  end;

  FCaption := 'Logon of ' + User;
  InitializeEvents;
end;

destructor TToken.Destroy;
begin
  OnClose.Invoke(Self);
  if hToken <> 0 then
  try
    CloseHandle(hToken); // A protected handle may cause an exception
    hToken := 0;
  except
    ; // but destructor should always succeed
  end;
  if Assigned(FEvents) then
    FinalizeEvents;

  if FOnCanClose.Count > 0 then
    OutputDebugString('Abandoned OnCanClose');
  if FOnClose.Count > 0 then
    OutputDebugString('Abandoned OnClose');
  if FOnCaptionChange.Count > 0 then
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
  if Events.OnGroupsChange.InvokeIfValid(Result) then
    Events.UpdateStatistics(Self);
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

    for i := 0 to HandleList.Count - 1 do
      if HandleList[i].Handle = hToken then
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
  if Events.OnPrivilegesChange.InvokeIfValid(Result) then
    Events.UpdateStatistics(Self);
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
  Events.OnStatisticsChange.InvokeIfValid(Result);
end;

function TToken.GetTokenOrigin: CanFail<Int64>;
begin
  Result := GetFixedSize<Int64>(TTokenInformationClass.TokenOrigin);
end;

function TToken.GetTokenType: CanFail<TTokenTypeEx>;
var
  ReturnValue: Cardinal;
  TokenTypeValue: TTokenType;
  Impersonation: TSecurityImpersonationLevel;
begin
  Result.Init(Self);

  if not Result.CheckError(GetTokenInformation(hToken, TokenType,
    @TokenTypeValue, SizeOf(TokenTypeValue), ReturnValue),
    GetterMessage(TokenType)) then
    Exit;

  if TokenTypeValue = TokenImpersonation then
  begin
    if Result.CheckError(GetTokenInformation(hToken, TokenImpersonationLevel,
    @Impersonation, SizeOf(Impersonation), ReturnValue),
    GetterMessage(TokenImpersonationLevel)) then
      Result.Value := TTokenTypeEx(Impersonation);
  end
  else
    Result.Value := ttPrimary;
end;

function TToken.GetUIAccess: Cardinal;
begin
  Result := TryGetUIAccess.GetValueOrRaise;
end;

function TToken.GetUser: CanFail<TGroup>;
var
  Buffer: PSIDAndAttributes;
begin
  with Result.CopyResult(QueryVariableBuffer(TokenUser)) do
    if IsValid then
    begin
      Buffer := Value;
      try
        Result.Value.SecurityIdentifier := TSecurityIdentifier.CreateFromSid(
          Buffer.Sid);

        if Buffer.Attributes = 0 then // 0 is default here and means "Enabled"
          Result.Value.Attributes := GroupExUser
        else // But it can also be "Use for deny only"
          Result.Value.Attributes := TGroupAttributes(Buffer.Attributes);
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

  GroupsArray := ConvertGroupArrayToSIDs(GroupArray);
  for i := 0 to High(GroupsArray) do
    NewState.Groups[i] := GroupsArray[i];

  if Action = gaEnable then
    for i := 0 to NewState.GroupCount - 1 do
      NewState.Groups[i].Attributes := Cardinal(GroupEnabled);
  try
    WinCheck(AdjustTokenGroups(hToken, IsResetFlag[Action], NewState, 0, nil,
      nil), 'AdjustTokenGroups', Self);
    GetGroups; // query and notify event listeners
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

    WinCheck(AdjustTokenPrivileges(hToken, False, Buffer, BufferSize, nil,
      nil) and (GetLastError = ERROR_SUCCESS), 'AdjustTokenPrivileges', Self);
  finally
    FreeMem(Buffer);
    GetPrivileges; // query and notify event listeners
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
  WinCheck(LongBool(hTargetProcess), 'OpenProcess#PROCESS_DUP_HANDLE');

  if not DuplicateHandle(GetCurrentProcess, hToken, hTargetProcess, @Result, 0,
    False, DUPLICATE_SAME_ACCESS) then
    WinCheck(False, 'DuplicateHandle')
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
    WinCheck(SetTokenInformation(hToken, TokenIntegrityLevel, @mandatoryLabel,
      SizeOf(TSIDAndAttributes)), SetterMessage(TokenIntegrityLevel));
  finally
    FreeMem(mandatoryLabel.Sid);
  end;
  TryGetIntegrity; // query and notify event listeners
end;

procedure TToken.SetMandatoryPolicy(const Value: TMandatoryPolicy);
begin
  SetFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy, Value);
  TryGetMandatoryPolicy; // query and notify event listeners
end;

procedure TToken.SetSession(const Value: Cardinal);
begin
  SetFixedSize<Cardinal>(TokenSessionId, Value);
  TryGetSession; // query and notify event listeners
end;

procedure TToken.SetUIAccess(const Value: Cardinal);
begin
  SetFixedSize<Cardinal>(TokenUIAccess, Value);
  TryGetUIAccess; // query and notify event listeners
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

  // Lowering integrity can disable privileges
  if Events.OnIntegrityChange.InvokeIfValid(Result) then
    Events.UpdatePrivileges(Self);
end;

function TToken.TryGetMandatoryPolicy: CanFail<TMandatoryPolicy>;
begin
  Result := GetFixedSize<TMandatoryPolicy>(TokenMandatoryPolicy);

  if Events.OnPolicyChange.InvokeIfValid(Result) then
    Events.UpdateStatistics(Self);
end;

function TToken.TryGetSession: CanFail<Cardinal>;
begin
  Result := GetFixedSize<Cardinal>(TokenSessionId);
  if Events.OnSessionChange.InvokeIfValid(Result) then
    Events.UpdateStatistics(Self);
end;

function TToken.TryGetUIAccess: CanFail<Cardinal>;
begin
  Result := GetFixedSize<Cardinal>(TokenUIAccess);
  if Events.OnUIAccessChange.InvokeIfValid(Result) then
    Events.UpdateStatistics(Self);
end;


{ TTokenEvents }

procedure TTokenEvents.BeginUpdate;
begin
  Inc(FUpdateFlag);
end;

class function TTokenEvents.CompareCardinals(Value1, Value2: Cardinal): Boolean;
begin
  Result := Value1 = Value2;
end;

class function TTokenEvents.CompareGroups(Value1, Value2: TGroupArray): Boolean;
var
  i: integer;
begin
  Result := Length(Value1) = Length(Value2);
  if Result then
    for i := 0 to High(Value1) do
      if (Value1[i].SecurityIdentifier.SID <> Value2[i].SecurityIdentifier.SID)
        or (Value1[i].Attributes <> Value2[i].Attributes) then
          Exit(False);
end;

class function TTokenEvents.CompareIntegrities(Value1,
  Value2: TTokenIntegrity): Boolean;
begin
  Result := Value1.Level = Value2.Level;
end;

class function TTokenEvents.ComparePolicies(Value1,
  Value2: TMandatoryPolicy): Boolean;
begin
  Result := Value1 = Value2;
end;

class function TTokenEvents.ComparePrivileges(Value1,
  Value2: TPrivilegeArray): Boolean;
var
  i: integer;
begin
  Result := Length(Value1) = Length(Value2);
  if Result then
    for i := 0 to High(Value1) do
      if (Value1[i].Attributes <> Value2[i].Attributes) or
        (Value1[i].Luid <> Value2[i].Luid) then
        Exit(False);
end;

class function TTokenEvents.CompareStatistics(Value1,
  Value2: TTokenStatistics): Boolean;
begin
  Result := CompareMem(@Value1, @Value2, SizeOf(TTokenStatistics));
end;

constructor TTokenEvents.Create;
begin
  inherited;
  FOnSessionChange.ComparisonFunction := CompareCardinals;
  FOnUIAccessChange.ComparisonFunction := CompareCardinals;
  FOnIntegrityChange.ComparisonFunction := CompareIntegrities;
  FOnPolicyChange.ComparisonFunction := ComparePolicies;
  FOnPrivilegesChange.ComparisonFunction := ComparePrivileges;
  FOnGroupsChange.ComparisonFunction := CompareGroups;
  FOnStatisticsChange.ComparisonFunction := CompareStatistics;
end;

destructor TTokenEvents.Destroy;
begin
  if FOnSessionChange.Count > 0 then
    OutputDebugString('Abandoned OnSessionChange');
  if FOnSessionChange.Count > 0 then
    OutputDebugString('Abandoned OnSessionChange');
  if FOnIntegrityChange.Count > 0 then
    OutputDebugString('Abandoned OnIntegrityChange');
  if FOnUIAccessChange.Count > 0 then
    OutputDebugString('Abandoned OnUIAccessChange');
  if FOnPolicyChange.Count > 0 then
    OutputDebugString('Abandoned OnPolicyChange');
  if FOnPrivilegesChange.Count > 0 then
    OutputDebugString('Abandoned OnPrivilegesChange');
  if FOnGroupsChange.Count > 0 then
    OutputDebugString('Abandoned OnGroupsChange');
  if FOnStatisticsChange.Count > 0 then
    OutputDebugString('Abandoned OnStatisticsChange');
  inherited;
end;

procedure TTokenEvents.EndUpdate;
begin
  Dec(FUpdateFlag);
end;

procedure TTokenEvents.UpdatePrivileges(Token: TToken);
begin
  if FUpdateFlag = 0 then
  begin
    BeginUpdate;
    // Query privileges and share them with event listeners, but without
    // update of dependent events
    Token.Privileges;
    Token.Statistics; // Query and notify event listeners separetly
    EndUpdate;
  end;
end;

procedure TTokenEvents.UpdateStatistics(Token: TToken);
begin
  if FUpdateFlag = 0 then
    Token.Statistics; // Query and share with event listeners
end;


end.
