unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  System.SysUtils, Winapi.Windows, TU.Tokens.Winapi, TU.Handles, TU.Common;

type
  TSecurityIdentifier = record
  private
    procedure GetDomainAndUser(SrcSid: PSID);
    procedure GetStringSid(SrcSid: PSID);
  public
    SID, Domain, User: String;
    constructor CreateFromSid(SrcSid: PSID);
    constructor CreateFromStringSid(StringSID: string);
    constructor CreateFromUserName(Name: string);
    function ToString: String;
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
    function Contain(Flag: TGroupAttributes): Boolean;
  end;

  TGroup = record
    SecurityIdentifier: TSecurityIdentifier;
    Attributes: TGroupAttributes;
  end;

  TGroupArray = array of TGroup;

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

  TTokenSourceHelper = record helper for TTokenSource
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
    // TODO: Does TOKEN_MANDATORY_LABEL contain attributes?
    Level: TTokenIntegrityLevel;
    function ToString: String;
    function ToDetailedString: String;
  end;

  TToken = class
  protected
  type
    TTokenHelper<ResultType> = class
      class function GetFixedSize(Token: TToken;
        InfoClass: TTokenInformationClass): CanFail<ResultType>; static;
      class procedure SetFixedSize(Token: TToken;
        InfoClass: TTokenInformationClass; Value: ResultType); static;
    end;
  var
    hToken: THandle;
    Origin: THandleItem;
    FCaption: String;

    procedure SetCaption(const Value: String);

    /// <remarks> The bufer should be freed using FreeMem. </remarks>
    function QueryVariableBuffer(InfoClass: TTokenInformationClass):
      CanFail<Pointer>;
    function QuerySid(InfoClass: TTokenInformationClass):
      CanFail<TSecurityIdentifier>;
    function QueryGroups(InfoClass: TTokenInformationClass):
      CanFail<TGroupArray>;

    function GetAccess: CanFail<ACCESS_MASK>;
    function GetObjAddress: NativeUInt;
    function GetUser: CanFail<TSecurityIdentifier>;           // class 1
    function GetGroups: CanFail<TGroupArray>;                 // class 2
    function GetPrivileges: CanFail<TPrivilegeArray>;         // class 3
    function GetOwner: CanFail<TSecurityIdentifier>;          // class 4
    function GetPrimaryGroup: CanFail<TSecurityIdentifier>;   // class 5
    function GetTokenType: CanFail<TTokenTypeInfo>;           // classes 7 & 8
    function GetStatistics: CanFail<TTokenStatistics>;        // class 9
    function GetSource: CanFail<TTokenSource>;                // class 10
    function GetRestrictedSids: CanFail<TGroupArray>;         // class 11
    function GetSandboxInert: CanFail<LongBool>;              // class 15
    function GetTokenOrigin: CanFail<Int64>;                  // class 17
    function GetElevation: CanFail<TTokenElevationType>;      // classes 18 & 20
    function GetLinkedToken: CanFail<TToken>;                 // class 19
    function GetHasRestrictions: CanFail<LongBool>;           // class 20
    function GetSessionEx: Cardinal;
    procedure SetSession(const Value: Cardinal);
    function GetIntegrity: TTokenIntegrityLevel;
    procedure SetIntegrity(const Value: TTokenIntegrityLevel);
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
    //// session.
    /// </summary>
    /// <exception cref="EOSError"> Can raise EOSError. </exception>
    constructor CreateQueryWts(SessionID: Cardinal);

    destructor Destroy; override;
    function IsValidToken: Boolean;
    property Handle: THandle read hToken;
    property Access: CanFail<ACCESS_MASK> read GetAccess;
    property ObjAddress: NativeUInt read GetObjAddress;
    property Caption: String read FCaption write SetCaption;

    { Token Information classes }

    function TryGetSession: CanFail<Cardinal>;
    function TryGetIntegrity: CanFail<TTokenIntegrity>;

    property User: CanFail<TSecurityIdentifier> read GetUser;                   // class 1
    property Groups: CanFail<TGroupArray> read GetGroups;                       // class 2
    property Privileges: CanFail<TPrivilegeArray> read GetPrivileges;           // class 3
    property Owner: CanFail<TSecurityIdentifier> read GetOwner;                 // class 4
    property PrimaryGroup: CanFail<TSecurityIdentifier> read GetPrimaryGroup;   // class 5
    // TODO: class 6: DefaultDacl
    property Source: CanFail<TTokenSource> read GetSource;                      // classes 7 & 8
    property TokenTypeInfo: CanFail<TTokenTypeInfo> read GetTokenType;          // class 9
    property Statistics: CanFail<TTokenStatistics> read GetStatistics;          // class 10
    property RestrictedSids: CanFail<TGroupArray> read GetRestrictedSids;       // class 11
    property Session: Cardinal read GetSessionEx write SetSession;              // class 12
    // TODO: class 13 TokenGroupsAndPrivileges
    // TODO: class 14 SessionReference
    property SandboxInert: CanFail<LongBool> read GetSandboxInert;              // class 15
    // TODO -cEnhancement: class 16 TokenAuditPolicy
    property TokenOrigin: CanFail<Int64> read GetTokenOrigin;                   // class 17
    property Elevation: CanFail<TTokenElevationType> read GetElevation;         // classes 18 & 20
    property LinkedToken: CanFail<TToken> read GetLinkedToken;                  // class 19
    property HasRestrictions: CanFail<LongBool> read GetHasRestrictions;        // class 21
    // TODO: class 22 AccessInformation
    // TODO: class 23 & 24 Virtualization
    property Integrity: TTokenIntegrityLevel read GetIntegrity write SetIntegrity; // class 25

    /// <summary>
    ///  Asks all event listeners for confirmation before closing the token.
    ///  Any event listener can call <c>Abort;</c> to prevent further actions.
    /// </summary>
    var OnCanClose: TEventHandler<TToken>;
    var OnClose: TEventHandler<TToken>;
    var OnCaptionChange: TEventHandler<String>;
    var OnSessionChange: TEventHandler<CanFail<Cardinal>>;
    var OnIntegrityChange: TEventHandler<CanFail<TTokenIntegrity>>;
    var OnPrivilegesChange: TEventHandler<CanFail<TPrivilegeArray>>;

    { Actions }

    function SendHandleToProcess(PID: Cardinal): NativeUInt;
    procedure PrivilegeAdjust(PrivilegeArray: TPrivilegeLUIDArray;
      Action: TPrivilegeAdjustAction); overload;
    procedure PrivilegeAdjust(Privilege: TLargeInteger;
      Action: TPrivilegeAdjustAction); overload;
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

implementation

uses
  TU.NativeAPI, TU.WtsApi;

{ TToken }

constructor TToken.CreateDuplicate(SrcToken: TToken; Access: ACCESS_MASK;
  TokenImpersonation: TSecurityImpersonationLevel; TokenType: TTokenType);
begin
  Win32Check(DuplicateTokenEx(SrcToken.hToken, Cardinal(Access), nil,
    TokenImpersonation, TokenType, hToken),
    'DuplicateTokenEx', SrcToken);
  Caption := SrcToken.Caption + ' (copy)';
end;

constructor TToken.CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
  SameAccess: Boolean);
var
  Options: Cardinal;
begin
  if SameAccess then
    Options := DUPLICATE_SAME_ACCESS
  else
    Options := 0;

  Win32Check(DuplicateHandle(GetCurrentProcess, SrcToken.hToken,
    GetCurrentProcess, @hToken, Access, False, Options),
    'DuplicateHandle');

  if (SrcToken.Origin.OwnerPID = GetCurrentProcessId) or
    (SrcToken.Origin.OwnerPID = 0) then
    Caption := SrcToken.Caption + ' (reference)'
  else
    Caption := Format('Referenced 0x%x from PID %d', [SrcToken.Origin.hToken,
      SrcToken.Origin.OwnerPID]);
end;

constructor TToken.CreateFromCurrent;
begin
  Win32Check(OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED, hToken),
    'OpenProcessToken');
  Caption := 'Current process';
end;

constructor TToken.CreateFromHandleItem(Item: THandleItem; hProcess: THandle);
begin
  hToken := 0;
  Origin := Item;
  Caption := Format('0x%x', [Item.hToken]);

  if hProcess <> 0 then
    DuplicateHandle(hProcess, Item.hToken, GetCurrentProcess, @hToken, 0, False,
      DUPLICATE_SAME_ACCESS);
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
  Caption := 'PID ' + IntToStr(PID);
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal);
begin
  Win32Check(WTSQueryUserToken(SessionID, hToken), 'WTSQueryUserToken');
  FCaption := Format('Token of session %d', [SessionID]);
end;

destructor TToken.Destroy;
begin
  // The event listener can abort this operation by raising EAbort
  OnCanClose.Involve(Self);

  OnClose.Involve(Self);
  if hToken <> 0 then
  try
    CloseHandle(hToken);
    hToken := 0;
  except
    ; // destructors should always succeed
  end;
  inherited;
end;

function TToken.GetAccess: CanFail<ACCESS_MASK>;
var
  info: TObjectBasicInformaion;
begin
  Result.Init(Self);

  // Pseudo-token mode
  if hToken = 0 then
    Exit(Result.Succeed(Origin.Access));

  if Result.CheckNativeError(NtQueryObject(hToken, ObjectBasicInformation,
    @info, SizeOf(info), nil), 'NtQueryObject') then
    Result.Succeed(info.GrantedAccess);
end;

function TToken.GetElevation: CanFail<TTokenElevationType>;
begin
  Result := TTokenHelper<TTokenElevationType>.GetFixedSize(Self,
    TokenElevationType);
end;

function TToken.GetGroups: CanFail<TGroupArray>;
begin
  Result := QueryGroups(TokenGroups);
end;

function TToken.GetHasRestrictions: CanFail<LongBool>;
begin
  Result := TTokenHelper<LongBool>.GetFixedSize(Self, TokenHasRestrictions);
end;

function TToken.TryGetIntegrity: CanFail<TTokenIntegrity>;
var
  Buffer: PSIDAndAttributes;
begin
  with Result.CopyResult(QueryVariableBuffer(TokenIntegrityLevel)) do
    if IsValid then
    begin
      Buffer := PSIDAndAttributes(Value);

      Result.Value.SID := TSecurityIdentifier.CreateFromSid(Buffer.Sid);
      Result.Value.Level := TTokenIntegrityLevel(GetSidSubAuthority(Buffer.Sid,
        0)^);

      FreeMem(Buffer);
    end;
end;

function TToken.GetIntegrity: TTokenIntegrityLevel;
begin
  Result := TryGetIntegrity.GetValueOrRaise.Level;
end;

function TToken.GetLinkedToken: CanFail<TToken>;
begin
  Result.Init(Self);

  with TTokenHelper<THandle>.GetFixedSize(Self, TokenLinkedToken) do
    if IsValid then
    begin
      Result.Value := TToken.Create;
      Result.Value.hToken := Value;
      Result.Value.Caption := 'Linked token for ' + Caption;
      Result.Succeed;
    end
    else
    begin
      Result.IsValid := False;
      Result.ErrorCode := ErrorCode;
      Result.ErrorOrigin := ErrorOrigin;
    end;
end;

function TToken.GetObjAddress: NativeUInt;
var
  HandleList: THandleList;
  i: integer;
begin
  if Origin.KernelObjectAddress = 0 then // not yet obtained
  begin
    HandleList := THandleList.CreateOnly(GetCurrentProcessId);

    for i := 0 to HandleList.Count do
      if HandleList[i].hToken = hToken then
      begin
        Origin := HandleList[i];
        Break;
      end;

    HandleList.Free;
  end;

  Result := Origin.KernelObjectAddress
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
        {$R-}
        SetLength(Result.Value, Buffer.PrivilegeCount);
        for i := 0 to Buffer.PrivilegeCount - 1 do
          Result.Value[i] := Buffer.Privileges[i];
        {$R+}
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
  Result := TTokenHelper<LongBool>.GetFixedSize(Self, TokenSandBoxInert);
end;

function TToken.TryGetSession: CanFail<Cardinal>;
begin
  Result := TTokenHelper<Cardinal>.GetFixedSize(Self, TokenSessionId);
end;

function TToken.GetSource: CanFail<TTokenSource>;
begin
  Result := TTokenHelper<TTokenSource>.GetFixedSize(Self, TokenSource);
end;

function TToken.GetStatistics: CanFail<TTokenStatistics>;
begin
  Result := TTokenHelper<TTokenStatistics>.GetFixedSize(Self, TokenStatistics);
end;

function TToken.GetTokenOrigin: CanFail<Int64>;
begin
  Result := TTokenHelper<Int64>.GetFixedSize(Self,
    TTokenInformationClass.TokenOrigin);
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

function TToken.GetUser: CanFail<TSecurityIdentifier>;
var
  Buffer: PSIDAndAttributes;
begin
  with Result.CopyResult(QueryVariableBuffer(TokenUser)) do
    if IsValid then
    begin
      Buffer := Value;
      Result.Succeed(TSecurityIdentifier.CreateFromSid(Buffer.Sid));
      FreeMem(Buffer);
    end;
end;

function TToken.GetSessionEx: Cardinal;
begin
  Result := TryGetSession.GetValueOrRaise;
end;

function TToken.IsValidToken: Boolean;
begin
  Result := hToken <> 0;
end;

procedure TToken.PrivilegeAdjust(Privilege: TLargeInteger;
  Action: TPrivilegeAdjustAction);
var
  PrivArray: TPrivilegeLUIDArray;
begin
  SetLength(PrivArray, 1);
  PrivArray[0] := Privilege;
  PrivilegeAdjust(PrivArray, Action);
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
    OnPrivilegesChange.Involve(GetPrivileges);
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
        {$R-}
        SetLength(Result.Value, Buffer.GroupCount);
        for i := 0 to Buffer.GroupCount - 1 do
        with Result.Value[i] do
        begin
          SecurityIdentifier.CreateFromSid(Buffer.Groups[i].Sid);
          Attributes := TGroupAttributes(Buffer.Groups[i].Attributes);
        end;
        {$R+}
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
      Result.Value := TSecurityIdentifier.CreateFromSid(Buffer.Owner);
      FreeMem(Buffer);
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
    Result.Value := nil
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
  OnCaptionChange.Involve(FCaption);
end;

procedure TToken.SetIntegrity(const Value: TTokenIntegrityLevel);
var
  mandatoryLabelAuthority: SID_IDENTIFIER_AUTHORITY;
  mandatoryLabel: TSIDAndAttributes;
begin
  FillChar(mandatoryLabelAuthority, SizeOf(mandatoryLabelAuthority), 0);
  mandatoryLabelAuthority.Value[5] := 16; // SECURITY_MANDATORY_LABEL_AUTHORITY

  mandatoryLabel.Sid := AllocMem(12);
  try
    InitializeSid(mandatoryLabel.Sid, mandatoryLabelAuthority, 1);
    GetSidSubAuthority(mandatoryLabel.Sid, 0)^ := DWORD(Value);
    mandatoryLabel.Attributes := SE_GROUP_INTEGRITY;
    Win32Check(SetTokenInformation(hToken, TokenIntegrityLevel, @mandatoryLabel,
      SizeOf(TSIDAndAttributes)), SetterMessage(TokenIntegrityLevel));
  finally
    FreeMem(mandatoryLabel.Sid);
  end;
  OnIntegrityChange.Involve(TryGetIntegrity);
  OnPrivilegesChange.Involve(GetPrivileges); // Integrity can disable privileges
end;

procedure TToken.SetSession(const Value: Cardinal);
begin
  TTokenHelper<Cardinal>.SetFixedSize(Self, TokenSessionId, Value);
  OnSessionChange.Involve(TryGetSession);
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

procedure TSecurityIdentifier.GetDomainAndUser(SrcSid: PSID);
var
  BufUser, BufDomain: PWideChar;
  UserChars, DomainChars, peUse: Cardinal;
begin
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
  if ConvertSidToStringSid(SrcSid, Buffer) then
  begin
    SID := String(Buffer);
    LocalFree(NativeUInt(Buffer));
  end;
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

function TGroupAttributesHelper.FlagsToString: String;
const
  GROUP_FLAGS_COUNT = 5;
  FlagValues: array [1 .. GROUP_FLAGS_COUNT] of TGroupAttributes = (
    GroupMandatory, GroupOwner, GroupIntegrity, GroupResource, GroupLogonId);
  FlagStrings: array [1 .. GROUP_FLAGS_COUNT] of String = (
    'Mandatory', 'Owner', 'Integrity', 'Resource', 'Logon Id');
var
  Strings: array of string;
  FlagInd, StrInd: Integer;
begin
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
const
  GROUP_STATE_COUNT = 4;
  StateValues: array [1 .. GROUP_STATE_COUNT] of TGroupAttributes = (
    GroupEnabledByDefault, GroupEnabled, GroupUforDenyOnly,
    GroupIntegrityEnabled);
  StateStrings: array [1 .. GROUP_STATE_COUNT] of String = (
    'Enabled by default', 'Enabled', 'Use for deny only',
    'Integrity enabled');
var
  Strings: array of string;
  StateInd, StrInd: Integer;
  FilteredAttributes: Cardinal;
begin
  // We don't need "Enabled by default" and "Enabled" at the same time
  if Cardinal(Self) and Cardinal(GroupEnabledByDefault) <> 0 then
    FilteredAttributes := Cardinal(Self) and not Cardinal(GroupEnabled)
  else
    FilteredAttributes := Cardinal(Self);

  SetLength(Strings, GROUP_STATE_COUNT);
  StrInd := 0;
  for StateInd := 1 to GROUP_STATE_COUNT do
    if FilteredAttributes and Cardinal(StateValues[StateInd]) =
      Cardinal(StateValues[StateInd]) then
    begin
      Strings[StrInd] := StateStrings[StateInd];
      Inc(StrInd);
    end;
  SetLength(Strings, StrInd);
  Result := String.Join(', ', Strings);
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
const
  PRIV_FLAGS_COUNT = 4;
  FlagValues: array [1 .. PRIV_FLAGS_COUNT] of Cardinal = (
    SE_PRIVILEGE_ENABLED_BY_DEFAULT, SE_PRIVILEGE_ENABLED,
    SE_PRIVILEGE_REMOVED, SE_PRIVILEGE_USED_FOR_ACCESS);
  FlagStrings: array [1 .. PRIV_FLAGS_COUNT] of String = (
    'Enabled by default', 'Enabled', 'Removed', 'Used for access');
var
  Strings: array of string;
  FlagInd, StrInd: Integer;
  FilteredAttributes: Cardinal;
begin
  if Self.Attributes = 0 then
    Exit('Disabled');

  // We don't need "Enabled by default" and "Enabled" at the same time
  if Self.Attributes and SE_PRIVILEGE_ENABLED_BY_DEFAULT <> 0 then
    FilteredAttributes := Self.Attributes and not SE_PRIVILEGE_ENABLED
  else
    FilteredAttributes := Self.Attributes;

  SetLength(Strings, PRIV_FLAGS_COUNT);
  StrInd := 0;
  for FlagInd := 1 to PRIV_FLAGS_COUNT do
    if FilteredAttributes and FlagValues[FlagInd] = FlagValues[FlagInd] then
    begin
      Strings[StrInd] := FlagStrings[FlagInd];
      Inc(StrInd);
    end;
  SetLength(Strings, StrInd);
  Result := String.Join(', ', Strings);
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

{ TTokenSourceHelper }

function TTokenSourceHelper.ToString: String;
begin
  Result := String(Self.sourcename);
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

{ TToken.TTokenHelper<ResultType> }

class function TToken.TTokenHelper<ResultType>.GetFixedSize(
  Token: TToken; InfoClass: TTokenInformationClass): CanFail<ResultType>;
var
  ReturnLength: Cardinal;
begin
  Result.Init(Token);

  Result.CheckError(GetTokenInformation(Token.hToken, InfoClass, @Result.Value,
    SizeOf(Result.Value), ReturnLength), GetterMessage(InfoClass));
end;

class procedure TToken.TTokenHelper<ResultType>.SetFixedSize(Token: TToken;
  InfoClass: TTokenInformationClass; Value: ResultType);
begin
  if not SetTokenInformation(Token.hToken, InfoClass, @Value, SizeOf(Value))
    then
    raise ELocatedOSError.CreateLE(GetLastError, SetterMessage(InfoClass));
end;

end. // CreateWellKnownSid
