unit TU.Tokens.Types;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

{ DONE: Staring from some point of complexity the compiler starts to confuse
  record helpers for the same types even if they are declared as not alises.
  So don't use the same names for methods in helpers for types that are based
  on the same simple type. }

uses
  TU.Winapi, Winapi.WinNt, Winapi.NtSecApi, NtUtils.Exceptions;

type
  TSecurityIdentifier = record
  strict private
    procedure GetDomainAndUser(SrcSid: PSid);
    procedure GetStringSid(SrcSid: PSid);
    procedure CreateFromStringSid(StringSID: string);
    constructor CreateFromUserName(Name: string; Dummy: Integer = 0);
  public
    SID, Domain, User: String;
    SIDType: TSIDNameUse;
    constructor CreateFromSid(SrcSid: PSid);
    constructor CreateFromString(UserOrSID: String);
    class function CreateWellKnown(WellKnownSidType: TWellKnownSidType):
      CanFail<TSecurityIdentifier>; static;
    function ToString: String;
    function SIDTypeToString: String;
    function HasPrettyName: Boolean;
    function AllocSid: PSid;
  end;

  TGroupAttributes = (
    GroupMandatory = SE_GROUP_MANDATORY,
    GroupEnabledByDefault = SE_GROUP_ENABLED_BY_DEFAULT,
    GroupEnabled = SE_GROUP_ENABLED,
    GroupOwner = SE_GROUP_OWNER,
    GroupUforDenyOnly = SE_GROUP_USE_FOR_DENY_ONLY,
    GroupIntegrity = SE_GROUP_INTEGRITY,
    GroupIntegrityEnabled = SE_GROUP_INTEGRITY_ENABLED,
    GroupResource = SE_GROUP_RESOURCE,
    GroupLogonId = Integer(SE_GROUP_LOGON_ID),
    GroupExUser = SE_GROUP_ENABLED or SE_GROUP_ENABLED_BY_DEFAULT
    // Used to display TOKEN_USER's attributes correctly
  );

  TGroupAttributesHelper = record helper for TGroupAttributes
    function StateToString: String;
    function FlagsToString: String;
    function ToString: String;
    function ContainAnyFlags: Boolean;
    function Contain(Flag: TGroupAttributes): Boolean;
  end;

  TGroup = record
    SecurityIdentifier: TSecurityIdentifier;
    Attributes: TGroupAttributes;
  end;

  TGroupArray = array of TGroup;
  TGroupAdjustAction = (gaResetDefault, gaEnable, gaDisable);

  TPrivilege = TLuidAndAttributes;

  TPrivilegeHelper = record helper for TPrivilege
    function Name: String;
    function Description: String;
    function AttributesToString: String;
    function AttributesContain(Flag: Cardinal): Boolean;
  end;

  TPrivilegeArray = array of TPrivilege;

  TPrivilegeAdjustAction = (paEnable, paDisable, paRemove);

  TTokenTypeEx = (ttAnonymous, ttIdentification, ttImpersonation, ttDelegation,
   ttPrimary);

  TTokenTypeExHelper = record helper for TTokenTypeEx
    function ToString: String;
    function TokenTypeValue: TTokenType;
    function SecurityImpersonationLevel: TSecurityImpersonationLevel;
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
    ilSystem = $4000,
    ilProtected = $5000
  );

  TTokenIntegrity = record
    Group: TGroup;
    Level: TTokenIntegrityLevel;
    function IsWellKnown: Boolean;
    function ToString: String;
  end;

  TMandatoryPolicy = (
    MandatoryPolicyOff = TOKEN_MANDATORY_POLICY_OFF,
    MandatoryPolicyNoWriteUp = TOKEN_MANDATORY_POLICY_NO_WRITE_UP,
    MandatoryPolicyNewProcessMin = TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN,
    MandatoryPolicyAll = TOKEN_MANDATORY_POLICY_VALID_MASK
  );

  TMandatoryPolicyHelper = record helper for TMandatoryPolicy
    function Contains(Flag: TMandatoryPolicy): Boolean;
    procedure Create(NoWriteUp, NewProcessMin: Boolean);
  end;

  TAuditState = (
    asIncludeSuccess = PER_USER_AUDIT_SUCCESS_INCLUDE,
    asExcludeSuccess = PER_USER_AUDIT_SUCCESS_EXCLUDE,
    asIncludeFailure = PER_USER_AUDIT_FAILURE_INCLUDE,
    asExcludeFailure = PER_USER_AUDIT_FAILURE_EXCLUDE
  );

  TAuditStateHelper = record helper for TAuditState
    function Contains(State: TAuditState): Boolean;
    procedure Include(State: TAuditState);
    procedure Exclude(State: TAuditState);
  end;

  TTokenPerUserAudit = class
  private
    function GetState(Index: Integer): TAuditState;
    procedure SetState(Index: Integer; Value: TAuditState);
  public
    AuditPolicySize: Integer;
    Data: PTokenAuditPolicy;
    property SubCategory[Index: Integer]: TAuditState read GetState write SetState;
    constructor CreateCopy(Src: TTokenPerUserAudit);
    destructor Destroy; override;
  end;

function CreateTokenSource(SourceName: String; SourceLuid: TLuid):
  TTokenSource;

{ Comparison function used by cached event handling system }
function CompareSIDs(Value1, Value2: TSecurityIdentifier): Boolean;
function CompareCardinals(Value1, Value2: Cardinal): Boolean;
function CompareLUIDs(Value1, Value2: TLuid): Boolean;
function CompareIntegrities(Value1, Value2: TTokenIntegrity): Boolean;
function CompareLongBools(Value1, Value2: LongBool): Boolean;
function ComparePolicies(Value1, Value2: TMandatoryPolicy): Boolean;
function ComparePrivileges(Value1, Value2: TPrivilegeArray): Boolean;
function CompareGroups(Value1, Value2: TGroupArray): Boolean;
function CompareStatistics(Value1, Value2: TTokenStatistics): Boolean;

{ Conversion functions }
function AccessToString(Access: Cardinal): String;
function AccessToDetailedString(Access: Cardinal): String;
function TokeSourceNameToString(TokenSource: TTokenSource): String;
function ObjectAttributesToString(ObjAttributes: Cardinal): String;
function NativeTimeToString(NativeTime: TLargeInteger): String;

/// <summary>
///   Formats a string to use as a location of an error that might occur while
///   quering token info class.
/// </summary>
function GetterMessage(InfoClass: TTokenInformationClass): String;

/// <summary>
///   Formats a string to use as a location of an error that might occur while
///   setting token info class.
/// </summary>
function SetterMessage(InfoClass: TTokenInformationClass): String;

implementation

uses
  System.SysUtils, System.TypInfo, TU.LsaApi, DelphiUtils.Strings,
  Winapi.WinBase, Winapi.WinError, Winapi.Sddl,
  Ntapi.ntseapi, Ntapi.ntdef, Ntapi.ntstatus;

function GetterMessage(InfoClass: TTokenInformationClass): String;
begin
  // We use a name of info class from the enumeration definition
  Result := 'GetTokenInformation:' +
    GetEnumName(TypeInfo(TTokenInformationClass), Integer(InfoClass))
end;

function SetterMessage(InfoClass: TTokenInformationClass): String;
begin
  // We use a name of info class from the enumeration definition
  Result := 'SetTokenInformation:' +
    GetEnumName(TypeInfo(TTokenInformationClass), Integer(InfoClass))
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

function TSecurityIdentifier.AllocSid: PSid;
begin
  WinCheck(ConvertStringSidToSidW(PWideChar(SID), Result),
    'ConvertStringSidToSid');
end;

constructor TSecurityIdentifier.CreateFromSid(SrcSid: PSid);
begin
  GetStringSid(SrcSid);
  GetDomainAndUser(SrcSid);
end;

constructor TSecurityIdentifier.CreateFromString(UserOrSID: String);
begin
  if UserOrSID.StartsWith('S-1-') then
    CreateFromStringSid(UserOrSID)
  else if UserOrSID.StartsWith('s-1-') then
    CreateFromStringSid(UpperCase(UserOrSID))
  else
    CreateFromUserName(UserOrSID);
end;

procedure TSecurityIdentifier.CreateFromStringSid(StringSID: string);
var
  Buffer: PSid;
begin
  SID := StringSID;
  WinCheck(ConvertStringSidToSidW(PWideChar(SID), Buffer),
    'ConvertStringSidToSid');

  try
    GetDomainAndUser(Buffer);
  finally
    LocalFree(Buffer);
  end;
end;

constructor TSecurityIdentifier.CreateFromUserName(Name: string;
  Dummy: Integer = 0);
var
  SidBuffer, DomainBuffer: Pointer;
  SidSize, DomainChars: Cardinal;
begin
  SidSize := 0;
  DomainChars := 0;
  LookupAccountNameW(nil, PWideChar(Name), nil, SidSize, nil,
    DomainChars, SIDType);
  WinCheckBuffer(SidSize, 'LookupAccountNameW');

  SidBuffer := AllocMem(SidSize);
  DomainBuffer := AllocMem((DomainChars + 1) * SizeOf(WideChar));
  try
    WinCheck(LookupAccountNameW(nil, PWideChar(Name), SidBuffer, SidSize,
      DomainBuffer, DomainChars, SIDType), 'LookupAccountNameW');

    CreateFromSid(SidBuffer);
  finally
    FreeMem(SidBuffer);
    FreeMem(DomainBuffer);
  end;
end;

class function TSecurityIdentifier.CreateWellKnown(
  WellKnownSidType: TWellKnownSidType): CanFail<TSecurityIdentifier>;
var
  Buffer: PSid;
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

procedure TSecurityIdentifier.GetDomainAndUser(SrcSid: PSid);
var
  BufUser, BufDomain: PWideChar;
  UserChars, DomainChars: Cardinal;
begin
  Domain := '';
  User := '';
  SIDType := SidTypeZero;

  // TODO: Optimize multiple queries with LsaLookupSids / LsaLookupNames

  UserChars := 0;
  DomainChars := 0;
  LookupAccountSidW(nil, SrcSid, nil, UserChars, nil, DomainChars, SIDType);
  if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or
    ((UserChars = 0) and (DomainChars = 0)) then
    Exit;

  BufUser := AllocMem((UserChars + 1) * SizeOf(WideChar));
  BufDomain := AllocMem((DomainChars + 1) * SizeOf(WideChar));
  try
    if LookupAccountSidW(nil, SrcSid, BufUser, UserChars, BufDomain,
      DomainChars, SIDType) then // We don't need exceptions
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

procedure TSecurityIdentifier.GetStringSid(SrcSid: PSid);
var
  Buffer: PWideChar;
begin
  // TODO: assert for valid SIDs
  SID := '';
  WinCheck(ConvertSidToStringSidW(SrcSid, Buffer), 'ConvertSidToStringSidW');

  SID := String(Buffer);
  LocalFree(Buffer);
end;

function TSecurityIdentifier.HasPrettyName: Boolean;
begin
  Result := (Domain <> '') or (User <> '');
end;

function TSecurityIdentifier.SIDTypeToString: String;
begin
  case SIDType of
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
    SidTypeLogonSession: Result := 'Logon Session';
  else
    Result := Format('%d (out of bound)', [Cardinal(SIDType)]);
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

function TGroupAttributesHelper.ToString: String;
begin
  Result := FlagsToString;
  if Result = '' then
    Result := StateToString
  else
    Result := StateToString + ', ' + Result;
end;

{ TPrivilegeHelper }

function TPrivilegeHelper.AttributesContain(Flag: Cardinal): Boolean;
begin
  Result := Self.Attributes and Flag = Flag;
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
begin
  Result := TPrivilegeCache.QueryDisplayName(Luid);
end;

function TPrivilegeHelper.Name: String;
begin
  Result := TPrivilegeCache.QueryName(Luid);
end;

{ TTokenTypeExHelper }

function TTokenTypeExHelper.SecurityImpersonationLevel:
  TSecurityImpersonationLevel;
begin
  if Self = ttPrimary then
    Result := SecurityImpersonation
  else
    Result := TSecurityImpersonationLevel(Self);
end;

function TTokenTypeExHelper.TokenTypeValue: TTokenType;
begin
  if Self = ttPrimary then
    Result := TokenPrimary
  else
    Result := TokenImpersonation;
end;

function TTokenTypeExHelper.ToString: String;
begin
  case Self of
    ttAnonymous: Result :=  'Anonymous';
    ttIdentification: Result := 'Identification';
    ttImpersonation: Result := 'Impersonation';
    ttDelegation: Result := 'Delegation';
    ttPrimary: Result := 'Primary token';
  end
end;

{ TTokenElevationTypeHelper }

function TTokenElevationTypeHelper.ToString: string;
begin
  case Self of
    TokenElevationTypeDefault: Result := 'N/A';
    TokenElevationTypeFull: Result := 'Full';
    TokenElevationTypeLimited: Result := 'Limited';
  else
    Result := '(out of bound)';
  end;
end;

{ TTokenIntegrity }

function TTokenIntegrity.IsWellKnown: Boolean;
begin
  case Level of
    ilUntrusted, ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem, ilProtected:
      Result := True;
  else
    Result := False;
  end;
end;

function TTokenIntegrity.ToString: String;
begin
  case Level of
    ilUntrusted: Result := 'Untrusted';
    ilLow: Result := 'Low';
    ilMedium: Result := 'Medium';
    ilMediumPlus: Result := 'Medium +';
    ilHigh: Result := 'High';
    ilSystem: Result := 'System';
    ilProtected: Result := 'Protected';
  else
    Result := IntToHexEx(UInt64(Level));
  end;
end;

{ TMandatoryPolicyHelper }

function TMandatoryPolicyHelper.Contains(Flag: TMandatoryPolicy): Boolean;
begin
  Result := Cardinal(Self) and Cardinal(Flag) = Cardinal(Flag);
end;

procedure TMandatoryPolicyHelper.Create(NoWriteUp, NewProcessMin: Boolean);
begin
  if NoWriteUp and NewProcessMin then
    Self := MandatoryPolicyAll
  else if NoWriteUp then
    Self := MandatoryPolicyNoWriteUp
  else if NewProcessMin then
    Self := MandatoryPolicyNewProcessMin
  else
    Self := MandatoryPolicyOff;
end;

{ TAuditStateHelper }

function TAuditStateHelper.Contains(State: TAuditState): Boolean;
begin
  Result := Cardinal(Self) and Cardinal(State) = Cardinal(State);
end;

procedure TAuditStateHelper.Exclude(State: TAuditState);
begin
  Self := TAuditState(Cardinal(Self) and not Cardinal(State));
end;

procedure TAuditStateHelper.Include(State: TAuditState);
begin
  Self := TAuditState(Cardinal(Self) or Cardinal(State));
end;

{ TTokenPerUserAudit }

constructor TTokenPerUserAudit.CreateCopy(Src: TTokenPerUserAudit);
var
  i: Integer;
begin
  Self.AuditPolicySize := Src.AuditPolicySize;

  Self.Data := AllocMem(AuditPolicySize);
  for i := 0 to AuditPolicySize - 1 do
    Self.Data.PerUserPolicy[i] := Src.Data.PerUserPolicy[i];
end;

destructor TTokenPerUserAudit.Destroy;
begin
  FreeMem(Data);
  Data := nil;
end;

function TTokenPerUserAudit.GetState(Index: Integer): TAuditState;
var
  RawResult: Byte;
begin
  if (Index < 0) or (Index > AuditPolicySize * 2) then
    Exit(TAuditState(0));

  // Each bytes stores policies for two subcategories
  RawResult := Data.PerUserPolicy[Index div 2];

  // We need only half of the byte
  if Index mod 2 = 0 then
    RawResult := RawResult mod $10
  else
    RawResult := RawResult div $10;

  Result := TAuditState(RawResult);
end;

procedure TTokenPerUserAudit.SetState(Index: Integer; Value: TAuditState);
var
  PolicyByte, RawValue: Byte;
begin
  if (Index < 0) or (Index > AuditPolicySize * 2) then
    Exit;

  {$R-}
  // We need half a byte
  RawValue := Byte(Value) mod $10;
  {$R+}

  // Preserve another half of the target byte since it might already store
  // some policies.

  PolicyByte := Data.PerUserPolicy[Index div 2];

  if Index mod 2 = 0 then
  begin
    PolicyByte := PolicyByte and $F0;
    PolicyByte := PolicyByte or RawValue;
  end
  else
  begin    
    RawValue := RawValue shl 4;
    PolicyByte := PolicyByte and $0F;
    PolicyByte := PolicyByte or RawValue;
  end;  

  Data.PerUserPolicy[Index div 2] := PolicyByte;
end;

{ TTokenSource }

function CreateTokenSource(SourceName: String; SourceLuid: TLuid):
  TTokenSource;
var
  i, Count: integer;
begin
  FillChar(Result, SizeOf(Result), 0);

  Count := Length(SourceName);
  if Count > 8 then
    Count := 8;

  for i := 1 to Count do
    Result.sourcename[i] := AnsiChar(SourceName[Low(SourceName) + i - 1]);

  Result.SourceIdentifier := SourceLuid;
end;

{ Comparison functions }

function CompareSIDs(Value1, Value2: TSecurityIdentifier): Boolean;
begin
  Result := Value1.SID = Value2.SID;
end;

function CompareCardinals(Value1, Value2: Cardinal): Boolean;
begin
  Result := Value1 = Value2;
end;

function CompareLUIDs(Value1, Value2: TLuid): Boolean;
begin
  Result := Value1 = Value2;
end;

function CompareGroups(Value1, Value2: TGroupArray): Boolean;
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

function CompareIntegrities(Value1, Value2: TTokenIntegrity): Boolean;
begin
  Result := (Value1.Group.SecurityIdentifier.SID =
    Value2.Group.SecurityIdentifier.SID) and
    (Value1.Group.Attributes = Value2.Group.Attributes);
end;

function CompareLongBools(Value1, Value2: LongBool): Boolean;
begin
  Result := Value1 = Value2;
end;

function ComparePolicies(Value1, Value2: TMandatoryPolicy): Boolean;
begin
  Result := Value1 = Value2;
end;

function ComparePrivileges(Value1, Value2: TPrivilegeArray): Boolean;
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

function CompareStatistics(Value1, Value2: TTokenStatistics): Boolean;
begin
  Result := (Value1.ModifiedId = Value2.ModifiedId);
end;

{ Conversion functions }

function TokeSourceNameToString(TokenSource: TTokenSource): String;
begin
  // sourcename field may or may not contain zero-termination byte
  Result := String(PAnsiChar(AnsiString(TokenSource.sourcename)));
end;

function ObjectAttributesToString(ObjAttributes: Cardinal): String;
begin
  if ObjAttributes and (OBJ_PERMANENT or OBJ_EXCLUSIVE) =
    (OBJ_PERMANENT or OBJ_EXCLUSIVE) then
    Result := 'Permanent, Exclusive'
  else if ObjAttributes and OBJ_PERMANENT <> 0 then
    Result := 'Permanent'
  else if ObjAttributes and OBJ_EXCLUSIVE <> 0 then
    Result := 'Exclusive'
  else
    Result := 'None';
end;

function NativeTimeToString(NativeTime: TLargeInteger): String;
begin
  if NativeTime.QuadPart = 0 then
    Result := 'Never'
  else if NativeTime.QuadPart = Int64.MaxValue then
    Result := 'Infinite'
  else
    Result := DateTimeToStr(NativeTime.ToDateTime);
end;

end.
