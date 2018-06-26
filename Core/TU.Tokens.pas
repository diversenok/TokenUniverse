unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  System.SysUtils, Winapi.Windows, TU.Handles;

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
      ErrorComment: String): TSecurityIdentifier;
    function QueryGroups(InfoClass: TTokenInformationClass;
      ErrorComment: String): TGroupArray;
    function GetAccess: ACCESS_MASK;
    function GetUser: TSecurityIdentifier;             // class 1
    function GetGroups: TGroupArray;                   // class 2
    function GetPrivileges: TPrivilegeArray;           // class 3
    function GetOwner: TSecurityIdentifier;            // class 4
    function GetPrimaryGroup: TSecurityIdentifier;     // class 5
    function GetDefaultDacl: PACL;                     // class 6
    function GetTokenType: TTokenTypeInfo;             // classes 7 & 8
    function GetStatistics: TTokenStatistics;          // class 9
    function GetSource: TTokenSource;                  // class 10
    function GetRestrictedSids: TGroupArray;           // class 11
    function GetSession: Cardinal;                     // class 12
    function GetSandboxInert: LongBool;                // class 15
    function GetTokenOrigin: Int64;                    // class 17
    function GetElevationType: TTokenElevationType;    // classes 18 & 20
    function GetLinkedToken: TToken;                   // class 19
    function GetHasRestrictions: LongBool;             // class 20
    function GetIntegrity: TTokenIntegrity;
  public
    constructor CreateFromCurrent;
    constructor CreateFromProcess(PID: Cardinal);
    constructor CreateDuplicate(SrcToken: TToken; Access: ACCESS_MASK;
      TokenTypeInfo: TTokenTypeInfo);
    constructor CreateDuplicateHandle(SrcToken: TToken; Access: ACCESS_MASK;
      SameAccess: Boolean);
    constructor CreateFromHandleItem(Item: THandleItem; hProcess: THandle);
    destructor Destroy; override;
    function IsValidToken: Boolean;
    var Caption: String;
    property Handle: THandle read hToken;
    property Access: ACCESS_MASK read GetAccess;
    property User: TSecurityIdentifier read GetUser;                            // class 1
    property Groups: TGroupArray read GetGroups;                                // class 2
    property Privileges: TPrivilegeArray read GetPrivileges;                    // class 3
    property Owner: TSecurityIdentifier read GetOwner;                          // class 4
    property PrimaryGroup: TSecurityIdentifier read GetPrimaryGroup;            // class 5
    property DefaultDacl: PACL read GetDefaultDacl;                             // class 6
    property Source: TTokenSource read GetSource;                               // classes 7 & 8
    property TokenTypeAndImpersonation: TTokenTypeInfo read GetTokenType;       // class 9
    property Statistics: TTokenStatistics read GetStatistics;                   // class 10
    property RestrictedSids: TGroupArray read GetRestrictedSids;                // class 11
    property Session: Cardinal read GetSession;                                 // class 12
    // class 13 TokenGroupsAndPrivileges
    // TODO -cEnhancement: class 14 SessionReference
    property SandboxInert: LongBool read GetSandboxInert;                       // class 15
    // TODO -cEnhancement: class 16 TokenAuditPolicy
    property TokenOrigin: Int64 read GetTokenOrigin;                            // class 17
    property Elevation: TTokenElevationType read GetElevationType;              // classes 18 & 20
    property LinkedToken: TToken read GetLinkedToken;                           // class 19
    property HasRestrictions: LongBool read GetHasRestrictions;
    property Integrity: TTokenIntegrity read GetIntegrity;
    function SendHandleToProcess(PID: Cardinal): THandle;
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
  TU.Common;

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
    'TToken.CreateDuplicate');
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
    'TToken.CreateDuplicateHandle1');

  if SrcToken.Origin.OwnerPID = GetCurrentProcessId then
    Caption := SrcToken.Caption + ' (reference)'
  else
    Caption := Format('Referenced 0x%x from PID %d', [SrcToken.Origin.hToken,
      SrcToken.Origin.OwnerPID]);
end;

constructor TToken.CreateFromCurrent;
begin
  Win32Check(OpenProcessToken(GetCurrentProcess, MAXIMUM_ALLOWED, hToken),
    'TToken.CreateFromCurrent');
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
    Win32Check(False, 'TToken.CreateFromProcess#1');

  Win32Check(OpenProcessToken(hProcess, MAXIMUM_ALLOWED, hToken),
    'TToken.CreateFromProcess#2');
  Caption := 'PID ' + IntToStr(PID);
end;

destructor TToken.Destroy;
begin
  if hToken <> 0 then
    CloseHandle(hToken);
  inherited;
end;

function TToken.GetAccess: ACCESS_MASK;
var
  info: TObjectBasicInformaion;
begin
  // Pseudo-token mode
  if hToken = 0 then
    Exit(Origin.Access);

  NativeCheck(NtQueryObject(hToken, ObjectBasicInformation, @info,
    SizeOf(info), nil), 'TToken.GetAccess');

  Result := info.GrantedAccess;
end;

function TToken.GetDefaultDacl: PACL;
begin
  raise ENotImplemented.Create('TToken.GetDefaultDacl');
end;

function TToken.GetElevationType: TTokenElevationType;
var
  ReturnValue: DWORD;
begin
  Win32Check(GetTokenInformation(hToken, TokenElevationType, @Result,
    SizeOf(Result), ReturnValue), 'TToken.GetElevationType');
end;

function TToken.GetGroups: TGroupArray;
begin
  Result := QueryGroups(TokenGroups, 'TToken.GetGroups');
end;

function TToken.GetHasRestrictions: LongBool;
var
  ReturnLength: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TokenHasRestrictions, @Result,
    SizeOf(Result), ReturnLength), 'TToken.GetHasRestrictions');
end;

function TToken.GetIntegrity: TTokenIntegrity;
var
  Buffer: PSIDAndAttributes;
  BufferSize, ReturnValue: Cardinal;
begin
  BufferSize := 0;
  GetTokenInformation(hToken, TokenIntegrityLevel, nil, 0, BufferSize);
  Win32CheckBuffer(BufferSize, 'TToken.GetIntegrity#1');

  Buffer := AllocMem(BufferSize);
  try
    Win32Check(GetTokenInformation(hToken, TokenIntegrityLevel, Buffer,
      BufferSize, ReturnValue), 'TToken.GetIntegrity#2');

    Result.SID := TSecurityIdentifier.CreateFromSid(Buffer.Sid);
    Result.Level := TTokenIntegrityLevel(GetSidSubAuthority(Buffer.Sid, 0)^);
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
    SizeOf(hLinkedToken), ReturnValue), 'TToken.GetLinkedToken');
  Result := TToken.Create;
  Result.hToken := hLinkedToken;
  Result.Caption := 'Linked token for ' + Caption;
end;

function TToken.GetOwner: TSecurityIdentifier;
begin
  Result := QuerySid(TokenOwner, 'TToken.GetOwner');
end;

function TToken.GetPrimaryGroup: TSecurityIdentifier;
begin
  Result := QuerySid(TokenPrimaryGroup, 'TToken.GetPrimaryGroup');
end;

function TToken.GetPrivileges: TPrivilegeArray;
var
  Buffer: PTokenPrivileges;
  BufferSize, ReturnLength: Cardinal;
  i: integer;
begin
  BufferSize := 0;
  GetTokenInformation(hToken, TokenPrivileges, nil, 0, BufferSize);
  Win32CheckBuffer(BufferSize, 'TToken.GetPrivileges#1');

  Buffer := AllocMem(BufferSize);
  try
    Win32Check(GetTokenInformation(hToken, TokenPrivileges, Buffer, BufferSize,
      ReturnLength), 'TToken.GetPrivileges#2');

    SetLength(Result, Buffer.PrivilegeCount);
    for i := 0 to Buffer.PrivilegeCount - 1 do
      Result[i] := Buffer.Privileges[i];
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.GetRestrictedSids: TGroupArray;
begin
  Result := QueryGroups(TokenRestrictedSids, 'TToken.GetRestrictedSids');
end;

function TToken.GetSandboxInert: LongBool;
var
  ReturnLength: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TokenSandBoxInert, @Result,
    SizeOf(Result), ReturnLength), 'TToken.GetSandboxInert');
end;

function TToken.GetSession: Cardinal;
var
  ReturnValue: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TokenSessionId, @Result,
    SizeOf(Result), ReturnValue), 'TToken.GetSession');
end;

function TToken.GetSource: TTokenSource;
var
  ReturnLength: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TokenSource, @Result, SizeOf(Result),
    ReturnLength), 'TToken.GetSource');
end;

function TToken.GetStatistics: TTokenStatistics;
var
  ReturnLength: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TokenStatistics, @Result,
    SizeOf(Result), ReturnLength), 'TToken.GetStatistics');
end;

function TToken.GetTokenOrigin: Int64;
var
  ReturnLength: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TTokenInformationClass.TokenOrigin,
    @Result, SizeOf(Result), ReturnLength), 'TToken.GetTokenOrigin');
end;

function TToken.GetTokenType: TTokenTypeInfo;
var
 tokType: TTokenType;
 tokImp: TSecurityImpersonationLevel;
 ReturnValue: Cardinal;
begin
  Win32Check(GetTokenInformation(hToken, TokenType, @tokType, SizeOf(tokType),
    ReturnValue), 'TToken.GetTokenType#1');

  Result.TokenType := tokType;

  if tokType = TokenImpersonation then
  begin
    Win32Check(GetTokenInformation(hToken, TokenImpersonationLevel, @tokImp,
      SizeOf(tokImp), ReturnValue), 'TToken.GetTokenType#2');
    Result.Impersonation := tokImp;
  end;
end;

function TToken.GetUser: TSecurityIdentifier;
var
  Buffer: PSIDAndAttributes;
  BufferSize, ReturnValue: Cardinal;
begin
  BufferSize := 0;
  GetTokenInformation(hToken, TokenUser, nil, 0, BufferSize);
  Win32CheckBuffer(BufferSize, 'TToken.GetUser#1');

  Buffer := AllocMem(BufferSize);
  try
    Win32Check(GetTokenInformation(hToken, TokenUser, Buffer, BufferSize,
      ReturnValue), 'TToken.GetUser#2');
    Result := TSecurityIdentifier.CreateFromSid(Buffer.Sid);
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.IsValidToken: Boolean;
begin
  Result := hToken <> 0;
end;

function TToken.QueryGroups(InfoClass: TTokenInformationClass;
  ErrorComment: String): TGroupArray;
var
  Buffer: PTokenGroups;
  BufferSize, ReturnLength: Cardinal;
  i: integer;
begin
  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);
  Win32CheckBuffer(BufferSize, ErrorComment + '#1');

  Buffer := AllocMem(BufferSize);
  try
    Win32Check(GetTokenInformation(hToken, InfoClass, Buffer, BufferSize,
      ReturnLength), ErrorComment + '#2');

    SetLength(Result, Buffer.GroupCount);
    for i := 0 to Buffer.GroupCount - 1 do
    begin
      Result[i].SecurityIdentifier.CreateFromSid(Buffer.Groups[i].Sid);
      Result[i].Attributes := TGroupAttributes(Buffer.Groups[i].Attributes);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.QuerySid(InfoClass: TTokenInformationClass;
  ErrorComment: String): TSecurityIdentifier;
var
  Buffer: PTokenOwner; // aka TTokenPrimaryGroup
  BufferSize, ReturnValue: Cardinal;
begin
  BufferSize := 0;
  GetTokenInformation(hToken, InfoClass, nil, 0, BufferSize);
  Win32CheckBuffer(BufferSize, ErrorComment + '#1');

  Buffer := AllocMem(BufferSize);
  try
    Win32Check(GetTokenInformation(hToken, InfoClass, Buffer,
      BufferSize, ReturnValue), ErrorComment + '#2');
    Result := TSecurityIdentifier.CreateFromSid(Buffer.Owner);
  finally
    FreeMem(Buffer);
  end;
end;

function TToken.SendHandleToProcess(PID: Cardinal): THandle;
var
  hTargetProcess: THandle;
begin
  hTargetProcess := OpenProcess(PROCESS_DUP_HANDLE, False, PID);
  Win32Check(LongBool(hTargetProcess), 'TToken.SendHandleToProcess#1');

  if not DuplicateHandle(GetCurrentProcess, hToken, hTargetProcess, @Result, 0,
    False, DUPLICATE_SAME_ACCESS) then
    Win32Check(False, 'TToken.SendHandleToProcess#2')
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
    'TSecurityIdentifier.CreateFromStringSid') then
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
  Win32CheckBuffer(SidSize, 'TSecurityIdentifier.CreateFromUserName#1');

  SidBuffer := AllocMem(SidSize);
  DomainBuffer := AllocMem(DomainChars * SizeOf(WideChar));
  try
    if Win32Check(LookupAccountName(nil, PWideChar(Name), SidBuffer, SidSize,
      DomainBuffer, DomainChars, Reserved2),
      'TSecurityIdentifier.CreateFromUserName#2') then
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
  if Win32Check(ConvertSidToStringSid(SrcSid, Buffer),
    'TSecurityIdentifier.GetStringSid') then
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
  else
    Result := SID
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
