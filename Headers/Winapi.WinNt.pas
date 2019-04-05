unit Winapi.WinNt;
{$MINENUMSIZE 4}

interface

// Note: line numbers are valid for SDK 10.0.17134

const
  kernelbase = 'kernelbase.dll';
  kernel32  = 'kernel32.dll';
  advapi32  = 'advapi32.dll';
  secur32 = 'secur32.dll';

  // 8881
  _DELETE = $00010000;
  READ_CONTROL = $00020000;
  WRITE_DAC = $00040000;
  WRITE_OWNER = $00080000;
  SYNCHRONIZE = $00100000;

  STANDARD_RIGHTS_REQUIRED = _DELETE or READ_CONTROL or WRITE_DAC or WRITE_OWNER;
  STANDARD_RIGHTS_READ = READ_CONTROL;
  STANDARD_RIGHTS_WRITE = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE = READ_CONTROL;
  STANDARD_RIGHTS_ALL = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE;
  SPECIFIC_RIGHTS_ALL = $0000FFFF;

  ACCESS_SYSTEM_SECURITY = $01000000;
  MAXIMUM_ALLOWED = $02000000;

  GENERIC_READ = Cardinal($80000000);
  GENERIC_WRITE = $40000000;
  GENERIC_EXECUTE = $20000000;
  GENERIC_ALL = $10000000;

  // 9007
  SID_MAX_SUB_AUTHORITIES = 15;
  SECURITY_MAX_SID_STRING_CHARACTERS = 2 + 4 + 15 +
    (11 * SID_MAX_SUB_AUTHORITIES) + 1;

  // 9623
  SE_GROUP_MANDATORY = $00000001;
  SE_GROUP_ENABLED_BY_DEFAULT = $00000002;
  SE_GROUP_ENABLED = $00000004;
  SE_GROUP_OWNER = $00000008;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
  SE_GROUP_INTEGRITY = $00000020;
  SE_GROUP_INTEGRITY_ENABLED = $00000040;
  SE_GROUP_RESOURCE = $20000000;
  SE_GROUP_LOGON_ID = $C0000000;

  // 9682
  ACL_REVISION = 2;

  // 9738
  ACCESS_ALLOWED_ACE_TYPE = $0;
  ACCESS_DENIED_ACE_TYPE = $1;
  SYSTEM_AUDIT_ACE_TYPE = $2;
  SYSTEM_ALARM_ACE_TYPE = $3;
  ACCESS_ALLOWED_CALLBACK_ACE_TYPE = $9;
  ACCESS_DENIED_CALLBACK_ACE_TYPE = $A;
  SYSTEM_AUDIT_CALLBACK_ACE_TYPE = $D;
  SYSTEM_ALARM_CALLBACK_ACE_TYPE = $E;
  SYSTEM_MANDATORY_LABEL_ACE_TYPE = $11;
  SYSTEM_RESOURCE_ATTRIBUTE_ACE_TYPE = $12;
  SYSTEM_SCOPED_POLICY_ID_ACE_TYPE = $13;
  SYSTEM_PROCESS_TRUST_LABEL_ACE_TYPE = $14;
  SYSTEM_ACCESS_FILTER_ACE_TYPE = $15;

  // 9779
  OBJECT_INHERIT_ACE = $1;
  CONTAINER_INHERIT_ACE = $2;
  NO_PROPAGATE_INHERIT_ACE = $4;
  INHERIT_ONLY_ACE = $8;
  INHERITED_ACE = $10;

  // 9805
  SUCCESSFUL_ACCESS_ACE_FLAG = $40;
  FAILED_ACCESS_ACE_FLAG = $80;

  // 9915
  SYSTEM_MANDATORY_LABEL_NO_WRITE_UP = $1;
  SYSTEM_MANDATORY_LABEL_NO_READ_UP = $2;
  SYSTEM_MANDATORY_LABEL_NO_EXECUTE_UP = $4;

  // 10096
  SECURITY_DESCRIPTOR_REVISION = 1;

  // 10320
  SE_PRIVILEGE_ENABLED_BY_DEFAULT = $00000001;
  SE_PRIVILEGE_ENABLED = $00000002;
  SE_PRIVILEGE_REMOVED = $00000004;
  SE_PRIVILEGE_USED_FOR_ACCESS = Cardinal($80000000);

  // 10803
  TOKEN_MANDATORY_POLICY_OFF = $0;
  TOKEN_MANDATORY_POLICY_NO_WRITE_UP = $1;
  TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN = $2;
  TOKEN_MANDATORY_POLICY_VALID_MASK = TOKEN_MANDATORY_POLICY_NO_WRITE_UP or
    TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  // 10846
  TOKEN_SOURCE_LENGTH = 8;

  // 11202
  OWNER_SECURITY_INFORMATION = $00000001;
  GROUP_SECURITY_INFORMATION = $00000002;
  DACL_SECURITY_INFORMATION = $00000004;
  SACL_SECURITY_INFORMATION = $00000008;
  LABEL_SECURITY_INFORMATION = $00000010;
  ATTRIBUTE_SECURITY_INFORMATION = $00000020;
  SCOPE_SECURITY_INFORMATION = $00000040;
  PROCESS_TRUST_LABEL_SECURITY_INFORMATION = $00000080;
  ACCESS_FILTER_SECURITY_INFORMATION = $00000100;
  BACKUP_SECURITY_INFORMATION = $00010000;

  PROTECTED_DACL_SECURITY_INFORMATION = $80000000;
  PROTECTED_SACL_SECURITY_INFORMATION = $40000000;
  UNPROTECTED_DACL_SECURITY_INFORMATION = $20000000;
  UNPROTECTED_SACL_SECURITY_INFORMATION = $10000000;

type
  // 818
  TLargeInteger = record
    QuadPart: Int64;
    function ToDateTime: TDateTime;
    procedure FromDateTime(DateTime: TDateTime);
  end;
  PLargeInteger = ^TLargeInteger;

  // 838
  TULargeInteger = UInt64;
  PULargeInteger = ^TULargeInteger;

  // 873
  TLuid = Int64;
  PLuid = ^TLuid;

  TLuidArray = array [Word] of TLuid;
  PLuidArray = ^TLuidArray;

  // 1119
  PListEntry = ^TListEntry;
  TListEntry = record
    Flink: PListEntry;
    Blink: PListEntry;
  end;

  // 8864
  TAccessMask = Cardinal;

  // 8923
  TGenericMapping = record
    GenericRead: TAccessMask;
    GenericWrite: TAccessMask;
    GenericExecute: TAccessMask;
    GenericAll: TAccessMask;
  end;
  PGenericMapping = ^TGenericMapping;

  // 8944
  TLuidAndAttributes = packed record // weird alignment...
    Luid: TLuid;
    Attributes: Cardinal;
  end;
  PLuidAndAttributes = ^TLuidAndAttributes;

  // 8984
  TSidIdentifierAuthority = record
    Value: array [0..5] of Byte;
  end;
  PSidIdentifierAuthority = ^TSidIdentifierAuthority;

  // 8994
  TSid = record
   Revision: Byte;
   SubAuthorityCount: Byte;
   IdentifierAuthority: TSidIdentifierAuthority;
   SubAuthority: array [0 .. SID_MAX_SUB_AUTHORITIES - 1] of Cardinal;
  end;
  PSid = ^TSid;

  TSidArray = array [Word] of PSid;
  PSidArray = ^TSidArray;

  TSidDynArray = array of PSid;

  // 9042
  TSidNameUse = (SidTypeZero, SidTypeUser, SidTypeGroup, SidTypeDomain,
    SidTypeAlias, SidTypeWellKnownGroup, SidTypeDeletedAccount, SidTypeInvalid,
    SidTypeUnknown, SidTypeComputer, SidTypeLabel, SidTypeLogonSession);

  // 9056
  TSidAndAttributes = record
    Sid: PSid;
    Attributes: Cardinal;
  end;
  PSidAndAttributes = ^TSidAndAttributes;

  TSIDAndAttributesDynArray = array of TSIDAndAttributes;

  // 9084
  TSIDAndAttributesHash = record
    const SID_HASH_SIZE = 32;
  var
    SidCount: Cardinal;
    SidAttr: PSIDAndAttributes;
    Hash: array [0 .. SID_HASH_SIZE - 1] of NativeUInt;
  end;
  PSIDAndAttributesHash = ^TSIDAndAttributesHash;

  // 9695
  TAcl = record
    AclRevision: Byte;
    Sbz1: Byte;
    AclSize: Word;
    AceCount: Word;
    Sbz2: Word;
  end;
  PAcl = ^TAcl;

  // 9725
  TAceHeader = record
    AceType: Byte;
    AceFlags: Byte;
    AceSize: Word;
  end;
  PAceHeader = ^TAceHeader;

  // This structure covers:
  //  ACCESS_ALLOWED_ACE & ACCESS_DENIED_ACE
  //  SYSTEM_AUDIT_ACE & SYSTEM_ALARM_ACE
  //  SYSTEM_RESOURCE_ATTRIBUTE_ACE
  //  SYSTEM_SCOPED_POLICY_ID_ACE
  //  SYSTEM_MANDATORY_LABEL_ACE
  //  SYSTEM_PROCESS_TRUST_LABEL_ACE
  //  SYSTEM_ACCESS_FILTER_ACE
  //  ACCESS_ALLOWED_CALLBACK_ACE & ACCESS_DENIED_CALLBACK_ACE
  //  SYSTEM_AUDIT_CALLBACK_ACE & SYSTEM_ALARM_CALLBACK_ACE
  // i.e. everything except OBJECT ACEs.
  TAce = record
    Header: TAceHeader;
    Mask: TAccessMask;
    SidStart: Cardinal;
    function Sid: PSid;
  end;
  PAce = ^TAce;

  // 10054
  TAclInformationClass = (
    AclRevisionInformation = 1,
    AclSizeInformation = 2
  );

  // 10064
  TAclRevisionInformation = record
    AclRevision: Cardinal;
  end;
  PAclRevisionInformation = ^TAclRevisionInformation;

  // 10073
  TAclSizeInformation = record
    AceCount: Cardinal;
    AclBytesInUse: Cardinal;
    AclBytesFree: Cardinal;
  end;
  PAclSizeInformation = ^TAclSizeInformation;

  // 10105
  TSecurityDescriptorControl = Word;
  PSecurityDescriptorControl = ^TSecurityDescriptorControl;

  // 10195
  TSecurityDescriptor = record
    Revision: Byte;
    Sbz1: Byte;
    Control: TSecurityDescriptorControl;
    Owner: PSid;
    Group: PSid;
    Sacl: PAcl;
    Dacl: PAcl;
  end;
  PSecurityDescriptor = ^TSecurityDescriptor;

  // 10346
  TPrivilegeSet = record
    PrivilegeCount: Cardinal;
    Control: Cardinal;
    Privilege: array [Byte] of TLuidAndAttributes;
  end;
  PPrivilegeSet = ^TPrivilegeSet;

  // 10556
  TSecurityImpersonationLevel = (SecurityAnonymous,
    SecurityIdentification, SecurityImpersonation, SecurityDelegation);

  // 10638
  TTokenType = (TokenTPad, TokenPrimary, TokenImpersonation);

  // 10650
  TTokenElevationType = (TokenElevationTPad, TokenElevationTypeDefault,
    TokenElevationTypeFull, TokenElevationTypeLimited);

  // 10661
  TTokenInformationClass = (
    TokenInfoTPad, // The compiler wouldn't generate TypeInfo without it
    TokenUser,
    TokenGroups,
    TokenPrivileges,
    TokenOwner,
    TokenPrimaryGroup,
    TokenDefaultDacl,
    TokenSource,
    TokenType,
    TokenImpersonationLevel,
    TokenStatistics,
    TokenRestrictedSids,
    TokenSessionId,
    TokenGroupsAndPrivileges,
    TokenSessionReference,
    TokenSandBoxInert,
    TokenAuditPolicy,
    TokenOrigin,
    TokenElevationType,
    TokenLinkedToken,
    TokenElevation,
    TokenHasRestrictions,
    TokenAccessInformation,
    TokenVirtualizationAllowed,
    TokenVirtualizationEnabled,
    TokenIntegrityLevel,
    TokenUIAccess,
    TokenMandatoryPolicy,
    TokenLogonSid,
    TokenIsAppContainer,
    TokenCapabilities,
    TokenAppContainerSid,
    TokenAppContainerNumber,
    TokenUserClaimAttributes,
    TokenDeviceClaimAttributes,
    TokenRestrictedUserClaimAttributes,
    TokenRestrictedDeviceClaimAttributes,
    TokenDeviceGroups,
    TokenRestrictedDeviceGroups,
    TokenSecurityAttributes,
    TokenIsRestricted,
    TokenProcessTrustLevel,
    TokenPrivateNameSpace,
    TokenSingletonAttributes,
    TokenBnoIsolation,
    TokenChildProcessFlags,
    MaxTokenInfoClass
  );

  // 10714
  TTokenUser = record
    User: TSidAndAttributes;
  end;
  PTokenUser = ^TTokenUser;

  // 10738
  TTokenGroups = record
    GroupCount: Integer;
    Groups: array[Word] of TSIDAndAttributes;
  end;
  PTokenGroups = ^TTokenGroups;

  // 10747
  TTokenPrivileges = record
    PrivilegeCount: Integer;
    Privileges: array[Byte] of TLUIDAndAttributes;
  end;
  PTokenPrivileges = ^TTokenPrivileges;

  // 10754
  TTokenOwner = record
    Owner: PSid;
  end;
  PTokenOwner = ^TTokenOwner;

  // 10761
  TTokenPrimaryGroup = record
    PrimaryGroup: PSid;
  end;
  PTokenPrimaryGroup = ^TTokenPrimaryGroup;

  // 10766
  TTokenDefaultDacl = record
    DefaultDacl :PAcl;
  end;
  PTokenDefaultDacl = ^TTokenDefaultDacl;

  // 10814
  TTokenMandatoryPolicy = record
    Policy: Cardinal;
  end;
  PTokenMandatoryPolicy = ^TTokenMandatoryPolicy;

  // 10820
  TTokenAccessInformation = record
    SidHash: PSIDAndAttributesHash;
    RestrictedSidHash: PSIDAndAttributesHash;
    Privileges: PTokenPrivileges;
    AuthenticationId: Int64;
    TokenType: TTokenType;
    ImpersonationLevel: TSecurityImpersonationLevel;
    MandatoryPolicy: TTokenMandatoryPolicy;
    Flags: Cardinal;
    AppContainerNumber: Cardinal;
    PackageSid: PSid;
    CapabilitiesHash: PSIDAndAttributesHash;
    TrustLevelSid: PSid;
    SecurityAttributes: Pointer;
  end;
  PTokenAccessInformation = ^TTokenAccessInformation;

  // 10842
  TTokenAuditPolicy = record
    // The actual length depends on the count of SubCategories of auditing.
    // Each half of a byte is a set of Winapi.NtSecApi.PER_USER_AUDIT_* flags.
    PerUserPolicy: array [Byte] of Byte;
  end;
  PTokenAuditPolicy = ^TTokenAuditPolicy;

  // 10848
  TTokenSource = record
    sourcename: array[1 .. TOKEN_SOURCE_LENGTH] of AnsiChar;
    SourceIdentifier: TLuid;
  end;
  PTokenSource = ^TTokenSource;

  // 10854
  TTokenStatistics = record
    TokenId: TLuid;
    AuthenticationId: TLuid;
    ExpirationTime: TLargeInteger;
    TokenType: TTokenType;
    ImpersonationLevel: TSecurityImpersonationLevel;
    DynamicCharged: Cardinal;
    DynamicAvailable: Cardinal;
    GroupCount: Cardinal;
    PrivilegeCount: Cardinal;
    ModifiedId: TLuid;
  end;
  PTokenStatistics = ^TTokenStatistics;

  // 11176
  TSecurityQualityOfService = record
    Length: Cardinal;
    ImpersonationLevel: TSecurityImpersonationLevel;
    ContextTrackingMode: Boolean;
    EffectiveOnly: Boolean;
  end;
  PSecurityQualityOfService = ^TSecurityQualityOfService;

  // 11200
  TSecurityInformation = Cardinal;
  PSecurityInformation = ^TSecurityInformation;

  // 11450
  TQuotaLimits = record
    PagedPoolLimit: NativeUInt;
    NonPagedPoolLimit: NativeUInt;
    MinimumWorkingSetSize: NativeUInt;
    MaximumWorkingSetSize: NativeUInt;
    PagefileLimit: NativeUInt;
    TimeLimit: TLargeInteger;
  end;
  PQuotaLimits = ^TQuotaLimits;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Ntapi.ntexapi;

{ TAce }

function TAce.Sid: PSid;
begin
  Result := PSid(@Self.SidStart);
end;

{ TLargeInteger }

const
  DAYS_FROM_1601 = 109205; // difference with Delphi's zero time in days
  DAY_TO_NATIVE_TIME = 864000000000; // 100ns in 1 day
  MINUTE_TO_NATIVE_TIME = 600000000; // 100ns in 1 minute

function GetTimeZoneBias: Int64;
var
  TimeZoneInfo: TRtlTimeZoneInformation;
begin
  if NT_SUCCESS(NtQuerySystemInformation(SystemCurrentTimeZoneInformation,
    @TimeZoneInfo, SizeOf(TimeZoneInfo), nil)) then
    Result := Int64(TimeZoneInfo.Bias) * MINUTE_TO_NATIVE_TIME
  else
    Result := 0;
end;

procedure TLargeInteger.FromDateTime(DateTime: TDateTime);
begin
  QuadPart := Trunc(DAY_TO_NATIVE_TIME * (DAYS_FROM_1601 + DateTime))
    + GetTimeZoneBias;
end;

function TLargeInteger.ToDateTime: TDateTime;
begin
  Result := (QuadPart - GetTimeZoneBias) / DAY_TO_NATIVE_TIME - DAYS_FROM_1601;
end;

end.
