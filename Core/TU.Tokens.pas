unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  System.SysUtils, Winapi.Windows, TU.Handles, TU.Common;

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
    function ToString: String;
  end;

  TGroup = record
    SecurityIdentifier: TSecurityIdentifier;
    Attributes: TGroupAttributes;
  end;

  TGroupArray = array of TGroup;

  TPrivilege = TLUIDAndAttributes;

  TPrivilegeHelper = record helper for TPrivilege
    function Name: String;
    function AttributesToString: String;
    function AttributesToDetailedString: String;
  end;

  TPrivilegeArray = array of TPrivilege;

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

  TTokenIntegrity = record
    SID: TSecurityIdentifier;
    // TODO: Does TOKEN_MANDATORY_LABEL contain attributes?
    Level: TTokenIntegrityLevel;
    function IsWellKnown: Boolean;
    function ToString: String;
    function ToDetailedString: String;
  end;

  TToken = class
  protected
    hToken: THandle;
    Origin: THandleItem;
    function QuerySid(InfoClass: TTokenInformationClass;
      ErrorComment: String): CanFail<TSecurityIdentifier>;
    function QueryGroups(InfoClass: TTokenInformationClass;
      ErrorComment: String): CanFail<TGroupArray>;
    function GetAccess: CanFail<ACCESS_MASK>;
    function GetUser: CanFail<TSecurityIdentifier>;           // class 1
    function GetGroups: CanFail<TGroupArray>;                 // class 2
    function GetPrivileges: CanFail<TPrivilegeArray>;         // class 3
    function GetOwner: CanFail<TSecurityIdentifier>;          // class 4
    function GetPrimaryGroup: CanFail<TSecurityIdentifier>;   // class 5
    function GetTokenType: CanFail<TTokenTypeInfo>;           // classes 7 & 8
    function GetStatistics: CanFail<TTokenStatistics>;        // class 9
    function GetSource: CanFail<TTokenSource>;                // class 10
    function GetRestrictedSids: CanFail<TGroupArray>;         // class 11
    function GetSession: CanFail<Cardinal>;                   // class 12
    function GetSandboxInert: CanFail<LongBool>;              // class 15
    function GetTokenOrigin: CanFail<Int64>;                  // class 17
    function GetElevation: CanFail<TTokenElevationType>;      // classes 18 & 20
    function GetLinkedToken: TToken;                          // class 19
    function GetHasRestrictions: CanFail<LongBool>;           // class 20
    function GetIntegrity: CanFail<TTokenIntegrity>;
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
      TokenTypeInfo: TTokenTypeInfo);

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
    destructor Destroy; override;
    function IsValidToken: Boolean;
    var Caption: String;
    property Handle: THandle read hToken;
    property Access: CanFail<ACCESS_MASK> read GetAccess;

    { Token Information classes }

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
    property Session: CanFail<Cardinal> read GetSession;                        // class 12
    // TODO: class 13 TokenGroupsAndPrivileges
    // TODO: class 14 SessionReference
    property SandboxInert: CanFail<LongBool> read GetSandboxInert;              // class 15
    // TODO -cEnhancement: class 16 TokenAuditPolicy
    property TokenOrigin: CanFail<Int64> read GetTokenOrigin;                   // class 17
    property Elevation: CanFail<TTokenElevationType> read GetElevation;         // classes 18 & 20
    property LinkedToken: TToken read GetLinkedToken;                           // class 19
    property HasRestrictions: CanFail<LongBool> read GetHasRestrictions;        // class 21
    // TODO: class 22 AccessInformation
    // TODO: class 23 & 24 Virtualization
    property Integrity: CanFail<TTokenIntegrity> read GetIntegrity;             // class 25

    { Actions }

    function SendHandleToProcess(PID: Cardinal): NativeUInt;
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

type
  TObjectInformationClass = (ObjectBasicInformation);

  TObjectBasicInformaion = record
    Attributes: Cardinal;
    GrantedAccess: ACCESS_MASK;
    HandleCount: Cardinal;
    PointerCount: Cardinal;
    Reserved: array [0..9] of Cardinal;
  end;

  TSIDAndAttributesHash = record
    const SID_HASH_SIZE = 32;
  var
    SidCount: Cardinal;
    SidAttr: PSIDAndAttributes;
    Hash: array [0 .. SID_HASH_SIZE-1] of NativeUInt;
  end;
  PSIDAndAttributesHash = ^TSIDAndAttributesHash;

  TTokenAccessInformation = record
    SidHash: PSIDAndAttributesHash;
    RestrictedSidHash: PSIDAndAttributesHash;
    Privileges: PTokenPrivileges;
    AuthenticationId: Int64;
    TokenType: TTokenType;
    ImpersonationLevel: TSecurityImpersonationLevel;
    MandatoryPolicy: TOKEN_MANDATORY_POLICY;
    Flags: DWORD;
    AppContainerNumber: DWORD;
    PackageSid: PSID;
    CapabilitiesHash: PSIDAndAttributesHash;
    TrustLevelSid: PSID;
    SecurityAttributes: Pointer;
  end;
  PTokenAccessInformation = ^TTokenAccessInformation;

function NtQueryObject(ObjectHandle: THandle; ObjectInformationClass:
  TObjectInformationClass; ObjectInformation: Pointer; ObjectInformationLength:
  Cardinal; ReturnLength: PCardinal): LongWord; stdcall; external 'ntdll.dll';

{ TToken }

constructor TToken.CreateDuplicate(SrcToken: TToken; Access: ACCESS_MASK;
      TokenTypeInfo: TTokenTypeInfo);
begin
  Win32Check(DuplicateTokenEx(SrcToken.hToken, Cardinal(Access), nil,
    TokenTypeInfo.Impersonation, TokenTypeInfo.TokenType, hToken),
    'DuplicateTokenEx');
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

  if SrcToken.Origin.OwnerPID = GetCurrentProcessId then
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
  Origin := Item;
  Caption := Format('0x%x', [Item.hToken]);

  if not DuplicateHandle(hProcess, Item.hToken, GetCurrentProcess, @hToken, 0,
    False, DUPLICATE_SAME_ACCESS) then
    hToken := 0;
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

destructor TToken.Destroy;
begin
  if hToken <> 0 then
    CloseHandle(hToken);
  inherited;
end;

function TToken.GetAccess: CanFail<ACCESS_MASK>;
var
  info: TObjectBasicInformaion;
begin
  Result.Init;

  // Pseudo-token mode
  if hToken = 0 then
    Exit(Result.Succeed(Origin.Access));

  if Result.CheckNativeError(NtQueryObject(hToken, ObjectBasicInformation, @info,
    SizeOf(info), nil), 'NtQueryObject') then
    Result.Succeed(info.GrantedAccess);
end;

function TToken.GetElevation: CanFail<TTokenElevationType>;
var
  ReturnValue: DWORD;
begin
  Result.Init;

  Result.CheckError(GetTokenInformation(hToken, TokenElevationType,
    @Result.Value, SizeOf(Result.Value), ReturnValue),
    'GetTokenInformation:TokenElevationType');
end;

function TToken.GetGroups: CanFail<TGroupArray>;
begin
  Result := QueryGroups(TokenGroups, 'GetTokenInformation:TokenGroups');
end;

function TToken.GetHasRestrictions: CanFail<LongBool>;
var
  ReturnLength: Cardinal;
begin
  Result.Init;

  Result.CheckError(GetTokenInformation(hToken, TokenHasRestrictions,
    @Result.Value, SizeOf(Result.Value), ReturnLength),
    'GetTokenInformation:TokenHasRestrictions');
end;

function TToken.GetIntegrity: CanFail<TTokenIntegrity>;
var
  Buffer: PSIDAndAttributes;
  BufferSize, ReturnValue: Cardinal;
begin
  Result.Init;

  BufferSize := 0;
  GetTokenInformation(hToken, TokenIntegrityLevel, nil, 0, BufferSize);
  if not Result.CheckBuffer(BufferSize,
    'GetTokenInformation:TokenIntegrityLevel') then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    if not Result.CheckError(GetTokenInformation(hToken, TokenIntegrityLevel,
      Buffer, BufferSize, ReturnValue),
      'GetTokenInformation:TokenIntegrityLevel') then
      Exit;

    Result.Value.SID := TSecurityIdentifier.CreateFromSid(Buffer.Sid);
    Result.Value.Level := TTokenIntegrityLevel(GetSidSubAuthority(Buffer.Sid, 0)^);
    Result.Succeed;
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.GetLinkedToken: TToken;
var
  hLinkedToken: THandle;
  ReturnValue: DWORD;
begin
  Win32Check(GetTokenInformation(hToken, TokenLinkedToken, @hLinkedToken,
    SizeOf(hLinkedToken), ReturnValue), 'GetTokenInformation:TokenLinkedToken');
  Result := TToken.Create;
  Result.hToken := hLinkedToken;
  Result.Caption := 'Linked token for ' + Caption;
end;

function TToken.GetOwner: CanFail<TSecurityIdentifier>;
begin
  Result := QuerySid(TokenOwner, 'GetTokenInformation:TokenOwner');
end;

function TToken.GetPrimaryGroup: CanFail<TSecurityIdentifier>;
begin
  Result := QuerySid(TokenPrimaryGroup, 'GetTokenInformation:TokenPrimaryGroup');
end;

function TToken.GetPrivileges: CanFail<TPrivilegeArray>;
var
  Buffer: PTokenPrivileges;
  BufferSize, ReturnLength: Cardinal;
  i: integer;
begin
  Result.Init;

  BufferSize := 0;
  GetTokenInformation(hToken, TokenPrivileges, nil, 0, BufferSize);

  if not Result.CheckBuffer(BufferSize, 'GetTokenInformation:TokenPrivileges')
    then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    if not Result.CheckError(GetTokenInformation(hToken, TokenPrivileges,
      Buffer, BufferSize, ReturnLength), 'GetTokenInformation:TokenPrivileges')
      then
      Exit;

    SetLength(Result.Value, Buffer.PrivilegeCount);
    for i := 0 to Buffer.PrivilegeCount - 1 do
      Result.Value[i] := Buffer.Privileges[i];
    Result.Succeed;
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.GetRestrictedSids: CanFail<TGroupArray>;
begin
  Result := QueryGroups(TokenRestrictedSids,
    'GetTokenInformation:TokenRestrictedSids');
end;

function TToken.GetSandboxInert: CanFail<LongBool>;
var
  ReturnLength: Cardinal;
begin
  Result.Init;

  Result.CheckError(GetTokenInformation(hToken, TokenSandBoxInert,
    @Result.Value, SizeOf(Result.Value), ReturnLength),
    'GetTokenInformation:TokenSandBoxInert');
end;

function TToken.GetSession: CanFail<Cardinal>;
var
  ReturnValue: Cardinal;
begin
  Result.Init;

  Result.CheckError(GetTokenInformation(hToken, TokenSessionId, @Result.Value,
    SizeOf(Result.Value), ReturnValue), 'GetTokenInformation:TokenSessionId');
end;

function TToken.GetSource: CanFail<TTokenSource>;
var
  ReturnLength: Cardinal;
begin
  Result.Init;
  Result.CheckError(GetTokenInformation(hToken, TokenSource, @Result.Value,
    SizeOf(Result.Value), ReturnLength), 'GetTokenInformation:TokenSource');
end;

function TToken.GetStatistics: CanFail<TTokenStatistics>;
var
  ReturnLength: Cardinal;
begin
  Result.Init;

  Result.CheckError(GetTokenInformation(hToken, TokenStatistics, @Result.Value,
    SizeOf(Result.Value), ReturnLength), 'GetTokenInformation:TokenStatistics');
end;

function TToken.GetTokenOrigin: CanFail<Int64>;
var
  ReturnLength: Cardinal;
begin
  Result.Init;

  Result.CheckError(GetTokenInformation(hToken,
    TTokenInformationClass.TokenOrigin, @Result.Value, SizeOf(Result.Value),
    ReturnLength), 'GetTokenInformation:TokenOrigin');
end;

function TToken.GetTokenType: CanFail<TTokenTypeInfo>;
var
 ReturnValue: Cardinal;
begin
  Result.Init;

  if not Result.CheckError(GetTokenInformation(hToken, TokenType,
    @Result.Value.TokenType, SizeOf(Result.Value.TokenType), ReturnValue),
    'GetTokenInformation:TokenType') then
    Exit;

  if Result.Value.TokenType = TokenImpersonation then
    Result.CheckError(GetTokenInformation(hToken, TokenImpersonationLevel,
    @Result.Value.Impersonation, SizeOf(Result.Value.Impersonation),
    ReturnValue), 'GetTokenInformation:TokenImpersonationLevel');
end;

function TToken.GetUser: CanFail<TSecurityIdentifier>;
var
  Buffer: PSIDAndAttributes;
  BufferSize, ReturnValue: Cardinal;
begin
  Result.Init;

  BufferSize := 0;
  GetTokenInformation(hToken, TokenUser, nil, 0, BufferSize);

  if not Result.CheckBuffer(BufferSize, 'GetTokenInformation:TokenUser') then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    if not Result.CheckError(GetTokenInformation(hToken, TokenUser, Buffer,
      BufferSize, ReturnValue), 'GetTokenInformation:TokenUser') then
      Exit;

    Result.Succeed(TSecurityIdentifier.CreateFromSid(Buffer.Sid));
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.IsValidToken: Boolean;
begin
  Result := hToken <> 0;
end;

function TToken.QueryGroups(InfoClass: TTokenInformationClass;
  ErrorComment: String): CanFail<TGroupArray>;
var
  Buffer: PTokenGroups;
  BufferSize, ReturnLength: Cardinal;
  i: integer;
begin
  Result.Init;

  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);

  if not Result.CheckBuffer(BufferSize, ErrorComment) then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    if not Result.CheckError(GetTokenInformation(hToken, InfoClass, Buffer,
      BufferSize, ReturnLength), ErrorComment) then
      Exit;

    SetLength(Result.Value, Buffer.GroupCount);
    for i := 0 to Buffer.GroupCount - 1 do
    begin
      Result.Value[i].SecurityIdentifier.CreateFromSid(Buffer.Groups[i].Sid);
      Result.Value[i].Attributes := TGroupAttributes(Buffer.Groups[i].Attributes);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.QuerySid(InfoClass: TTokenInformationClass;
  ErrorComment: String): CanFail<TSecurityIdentifier>;
var
  Buffer: PTokenOwner; // aka TTokenPrimaryGroup
  BufferSize, ReturnValue: Cardinal;
begin
  Result.Init;

  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);

  if not Result.CheckBuffer(BufferSize, ErrorComment) then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    if Result.CheckError(GetTokenInformation(hToken, InfoClass, Buffer,
      BufferSize, ReturnValue), ErrorComment) then
      Result.Succeed(TSecurityIdentifier.CreateFromSid(Buffer.Owner));
  finally
    FreeMem(Buffer);
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

{ TTokenAccess }

function AccessToDetailedString(Access: Cardinal): String;
begin
  Result := Format('0x%0.8x: %s', [Access, AccessToString(Access)]);
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
  LookupAccountName(nil, PWideChar(Name), nil, SidSize, nil,
    DomainChars, Reserved2);
  Win32CheckBuffer(SidSize, 'LookupAccountName');

  SidBuffer := AllocMem(SidSize);
  DomainBuffer := AllocMem(DomainChars * SizeOf(WideChar));
  try
    if Win32Check(LookupAccountName(nil, PWideChar(Name), SidBuffer, SidSize,
      DomainBuffer, DomainChars, Reserved2),
      'LookupAccountName') then
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
  LookupAccountSid(nil, SrcSid, nil, UserChars, nil, DomainChars, peUse);
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or
    ((UserChars = 0) and (DomainChars = 0)) then
    Exit;

  BufUser := AllocMem(UserChars * SizeOf(WideChar));
  BufDomain := AllocMem(DomainChars * SizeOf(WideChar));
  try
    if LookupAccountSid(nil, SrcSid, BufUser, UserChars, BufDomain,
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
end;

{ TGroupAttributesHelper }

function TGroupAttributesHelper.ToString: String;
const
  GROUP_FLAGS_COUNT = 9;
  FlagValues: array [1 .. GROUP_FLAGS_COUNT] of TGroupAttributes = (
    GroupMandatory, GroupEnabledByDefault, GroupEnabled, GroupOwner,
    GroupUforDenyOnly, GroupIntegrity, GroupIntegrityEnabled, GroupResource,
    GroupLogonId);
  FlagStrings: array [1 .. GROUP_FLAGS_COUNT] of String = (
    'Mandatory', 'Enabled by default', 'Enabled', 'Owner', 'Use for deny only',
    'Integrity', 'Integrity enabled', 'Resource', 'Logon Id');
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

{ TPrivilegeHelper }

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
begin
  if Self.Attributes = 0 then
    Exit('Disabled');

  SetLength(Strings, PRIV_FLAGS_COUNT);
  StrInd := 0;
  for FlagInd := 1 to PRIV_FLAGS_COUNT do
    if Self.Attributes and FlagValues[FlagInd] = FlagValues[FlagInd] then
    begin
      Strings[StrInd] := FlagStrings[FlagInd];
      Inc(StrInd);
    end;
  SetLength(Strings, StrInd);
  Result := String.Join(', ', Strings);
end;

function TPrivilegeHelper.Name: String;
var
  Buffer: PWideChar;
  BufferChars: Cardinal;
begin
  BufferChars := 0;
  LookupPrivilegeName(nil, Self.Luid, nil, BufferChars);

  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BufferChars = 0) then
    Exit(Format('Unknown privilege %d', [Self.Luid]));

  Buffer := AllocMem(BufferChars * SizeOf(WideChar));
  try
    if LookupPrivilegeName(nil, Self.Luid, Buffer, BufferChars) then
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

{ TTokenIntegrity }

function TTokenIntegrity.IsWellKnown: Boolean;
begin
  case Self.Level of
    ilUntrusted, ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem:
      Result := True;
  else
    Result := False;
  end;
end;

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

end. // CreateWellKnownSid
