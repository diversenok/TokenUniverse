unit Winapi.WinNt;

{$MINENUMSIZE 4}

interface

// Note: line numbers are valid for SDK 10.0.17134

const
  kernelbase = 'kernelbase.dll';
  kernel32  = 'kernel32.dll';
  advapi32  = 'advapi32.dll';
  secur32 = 'secur32.dll';

  // 8894
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

  // 9020
  SID_MAX_SUB_AUTHORITIES = 15;
  SECURITY_MAX_SID_STRING_CHARACTERS = 2 + 4 + 15 +
    (11 * SID_MAX_SUB_AUTHORITIES) + 1;

  // 9425
  SECURITY_MANDATORY_UNTRUSTED_RID = $0000;
  SECURITY_MANDATORY_LOW_RID = $1000;
  SECURITY_MANDATORY_MEDIUM_RID = $2000;
  SECURITY_MANDATORY_MEDIUM_PLUS_RID = SECURITY_MANDATORY_MEDIUM_RID + $0100;
  SECURITY_MANDATORY_HIGH_RID = $3000;
  SECURITY_MANDATORY_SYSTEM_RID = $4000;
  SECURITY_MANDATORY_PROTECTED_PROCESS_RID = $5000;

  // 9641
  SE_GROUP_MANDATORY = $00000001;
  SE_GROUP_ENABLED_BY_DEFAULT = $00000002;
  SE_GROUP_ENABLED = $00000004;
  SE_GROUP_OWNER = $00000008;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
  SE_GROUP_INTEGRITY = $00000020;
  SE_GROUP_INTEGRITY_ENABLED = $00000040;
  SE_GROUP_RESOURCE = $20000000;
  SE_GROUP_LOGON_ID = $C0000000;

  // 9700
  ACL_REVISION = 2;

  // TODO: reversed; does this value present in headers?
  MAX_ACL_SIZE = $FFFC;

  // 9797
  OBJECT_INHERIT_ACE = $1;
  CONTAINER_INHERIT_ACE = $2;
  NO_PROPAGATE_INHERIT_ACE = $4;
  INHERIT_ONLY_ACE = $8;
  INHERITED_ACE = $10;
  CRITICAL_ACE_FLAG = $20;               // for access allowed ace
  SUCCESSFUL_ACCESS_ACE_FLAG = $40;      // for audit and alarm aces
  FAILED_ACCESS_ACE_FLAG = $80;          // for audit and alarm aces
  TRUST_PROTECTED_FILTER_ACE_FLAG = $40; // for access filter ace

  // 9944
  SYSTEM_MANDATORY_LABEL_NO_WRITE_UP = $1;
  SYSTEM_MANDATORY_LABEL_NO_READ_UP = $2;
  SYSTEM_MANDATORY_LABEL_NO_EXECUTE_UP = $4;

  // 10125
  SECURITY_DESCRIPTOR_REVISION = 1;

  // 10349
  SE_PRIVILEGE_ENABLED_BY_DEFAULT = $00000001;
  SE_PRIVILEGE_ENABLED = $00000002;
  SE_PRIVILEGE_REMOVED = $00000004;
  SE_PRIVILEGE_USED_FOR_ACCESS = Cardinal($80000000);

  // 10833
  TOKEN_MANDATORY_POLICY_OFF = $0;
  TOKEN_MANDATORY_POLICY_NO_WRITE_UP = $1;
  TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN = $2;
  TOKEN_MANDATORY_POLICY_VALID_MASK = TOKEN_MANDATORY_POLICY_NO_WRITE_UP or
    TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  // 10950
  CLAIM_SECURITY_ATTRIBUTE_TYPE_INVALID = $00;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_INT64 = $01;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_UINT64 = $02;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_STRING = $03;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_FQBN = $04;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_SID = $05;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_BOOLEAN = $06;
  CLAIM_SECURITY_ATTRIBUTE_TYPE_OCTET_STRING = $10;

  // 10995
  CLAIM_SECURITY_ATTRIBUTE_NON_INHERITABLE = $0001;
  CLAIM_SECURITY_ATTRIBUTE_VALUE_CASE_SENSITIVE = $0002;
  CLAIM_SECURITY_ATTRIBUTE_USE_FOR_DENY_ONLY = $0004;
  CLAIM_SECURITY_ATTRIBUTE_DISABLED_BY_DEFAULT = $0008;
  CLAIM_SECURITY_ATTRIBUTE_DISABLED = $0010;
  CLAIM_SECURITY_ATTRIBUTE_MANDATORY = $0020;
  CLAIM_SECURITY_ATTRIBUTE_CUSTOM_FLAGS = $FFFF0000;

  // 11232
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
  TStringArray = array of String;

  // 823
  TLargeInteger = record
    QuadPart: Int64;
    function ToDateTime: TDateTime;
    procedure FromDateTime(DateTime: TDateTime);
  end;
  PLargeInteger = ^TLargeInteger;

  // 843
  TULargeInteger = UInt64;
  PULargeInteger = ^TULargeInteger;

  // 873
  TLuid = Int64;
  PLuid = ^TLuid;

  TLuidArray = array [Word] of TLuid;
  PLuidArray = ^TLuidArray;

  TLuidDynArray = array of TLuid;

  // 1119
  PListEntry = ^TListEntry;
  TListEntry = record
    Flink: PListEntry;
    Blink: PListEntry;
  end;

  // 8877
  TAccessMask = Cardinal;

  // 8936
  TGenericMapping = record
    GenericRead: TAccessMask;
    GenericWrite: TAccessMask;
    GenericExecute: TAccessMask;
    GenericAll: TAccessMask;
  end;
  PGenericMapping = ^TGenericMapping;

  // 8957
  TLuidAndAttributes = packed record // weird alignment...
    Luid: TLuid;
    Attributes: Cardinal;
  end;
  PLuidAndAttributes = ^TLuidAndAttributes;

  TPrivilege = TLuidAndAttributes;
  PPrivilege = PLuidAndAttributes;
  TPrivilegeArray = array of TPrivilege;

  // 8999
  TSidIdentifierAuthority = record
    Value: array [0..5] of Byte;
  end;
  PSidIdentifierAuthority = ^TSidIdentifierAuthority;

  // 9007
  TSid_Internal = record
   Revision: Byte;
   SubAuthorityCount: Byte;
   IdentifierAuthority: TSidIdentifierAuthority;
   SubAuthority: array [0 .. SID_MAX_SUB_AUTHORITIES - 1] of Cardinal;
  end;
  PSid = ^TSid_Internal;

  TSidArray = array [Word] of PSid;
  PSidArray = ^TSidArray;

  TSidDynArray = array of PSid;

  // 9055
  TSidNameUse = (SidTypeZero, SidTypeUser, SidTypeGroup, SidTypeDomain,
    SidTypeAlias, SidTypeWellKnownGroup, SidTypeDeletedAccount, SidTypeInvalid,
    SidTypeUnknown, SidTypeComputer, SidTypeLabel, SidTypeLogonSession);

  // 9069
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

  // 9713
  TAcl_Internal = record
    AclRevision: Byte;
    Sbz1: Byte;
    AclSize: Word;
    AceCount: Word;
    Sbz2: Word;
  end;
  PAcl = ^TAcl_Internal;

  // 9756
  {$MINENUMSIZE 1}
  TAceType = (
    AceTypeAccessAllowed = 0,
    AceTypeAccessDenied = 1,
    AceTypeSystemAudit = 2,
    AceTypeSystemAlarm = 3,

    AceTypeCompoundAccessAllowed = 4, // Unknown

    AceTypeObjectAccessAllowed = 5, // Object ace
    AceTypeObjectAccessDenied = 6,  // Object ace
    AceTypeObjectSystemAudit = 7,   // Object ace
    AceTypeObjectSystemAlarm = 8,   // Object ace

    AceTypeAccessAllowedCallback = 9,
    AceTypeAccessDeniedCallback = 10,

    AceTypeObjectAccessAllowedCallback = 11, // Object ace
    AceTypeObjectAccessDeniedCallback = 12,  // Object ace

    AceTypeSystemAuditCallback = 13,
    AceTypeSystemAlarmCallback = 14,

    AceTypeObjectSystemAuditCallback = 15, // Object ace
    AceTypeObjectSystemAlarmCallback = 16, // Object ace

    AceTypeSystemMandatoryLabel = 17,
    AceTypeSystemResourceAttribute = 18,
    AceTypeSystemScopedPolicyId = 19,
    AceTypeSystemProcessTrustLabel = 20,
    AceTypeSystemAccessFilter = 21
  );
  {$MINENUMSIZE 4}

  // 9743
  TAceHeader = record
    AceType: TAceType;
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
  TAce_Internal = record
    Header: TAceHeader;
    Mask: TAccessMask;
    SidStart: Cardinal;
    function Sid: PSid;
  end;
  PAce = ^TAce_Internal;

  // This structure covers:
  //  ACCESS_ALLOWED_OBJECT_ACE & ACCESS_DENIED_OBJECT_ACE
  //  SYSTEM_AUDIT_OBJECT_ACE & SYSTEM_ALARM_OBJECT_ACE
  //  ACCESS_ALLOWED_CALLBACK_OBJECT_ACE & ACCESS_DENIED_CALLBACK_OBJECT_ACE
  //  SYSTEM_AUDIT_CALLBACK_OBJECT_ACE & SYSTEM_ALARM_CALLBACK_OBJECT_ACE
  TObjectAce_Internal = record
    Header: TAceHeader;
    Mask: TAccessMask;
    Flags: Cardinal;
    ObjectType: TGuid;
    InheritedObjectType: TGuid;
    SidStart: Cardinal;
    function Sid: PSid;
  end;
  PObjectAce = ^TObjectAce_Internal;

  // 10083
  TAclInformationClass = (
    AclRevisionInformation = 1,
    AclSizeInformation = 2
  );

  // 10093
  TAclRevisionInformation = record
    AclRevision: Cardinal;
  end;
  PAclRevisionInformation = ^TAclRevisionInformation;

  // 10102
  TAclSizeInformation = record
    AceCount: Integer;
    AclBytesInUse: Cardinal;
    AclBytesFree: Cardinal;
    function AclBytesTotal: Cardinal;
  end;
  PAclSizeInformation = ^TAclSizeInformation;

  // 10134
  TSecurityDescriptorControl = Word;
  PSecurityDescriptorControl = ^TSecurityDescriptorControl;

  // 10234
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

  // 10375
  TPrivilegeSet = record
    PrivilegeCount: Cardinal;
    Control: Cardinal;
    Privilege: array [Byte] of TLuidAndAttributes;
  end;
  PPrivilegeSet = ^TPrivilegeSet;

  // 10585
  TSecurityImpersonationLevel = (SecurityAnonymous,
    SecurityIdentification, SecurityImpersonation, SecurityDelegation);

  // 10667
  TTokenType = (TokenTPad, TokenPrimary, TokenImpersonation);

  // 10679
  TTokenElevationType = (TokenElevationTPad, TokenElevationTypeDefault,
    TokenElevationTypeFull, TokenElevationTypeLimited);

  // 10744
  TTokenUser = record
    User: TSidAndAttributes;
  end;
  PTokenUser = ^TTokenUser;

  // 10768
  TTokenGroups = record
    GroupCount: Integer;
    Groups: array[Word] of TSIDAndAttributes;
  end;
  PTokenGroups = ^TTokenGroups;

  // 10777
  TTokenPrivileges = record
    PrivilegeCount: Integer;
    Privileges: array[Byte] of TLUIDAndAttributes;
  end;
  PTokenPrivileges = ^TTokenPrivileges;

  // 10783
  TTokenOwner = record
    Owner: PSid;
  end;
  PTokenOwner = ^TTokenOwner;

  // 10791
  TTokenPrimaryGroup = record
    PrimaryGroup: PSid;
  end;
  PTokenPrimaryGroup = ^TTokenPrimaryGroup;

  // 10796
  TTokenDefaultDacl = record
    DefaultDacl :PAcl;
  end;
  PTokenDefaultDacl = ^TTokenDefaultDacl;

  // 10808
  TTokenGroupsAndPrivileges = record
    SidCount: Cardinal;
    SidLength: Cardinal;
    Sids: PSidAndAttributes;
    RestrictedSidCount: Cardinal;
    RestrictedSidLength: Cardinal;
    RestrictedSids: PSidAndAttributes;
    PrivilegeCount: Cardinal;
    PrivilegeLength: Cardinal;
    Privileges: PLuidAndAttributes;
    AuthenticationId: TLuid;
  end;
  PTokenGroupsAndPrivileges = ^TTokenGroupsAndPrivileges;

  // 10821
  TTokenLinkedToken = record
    LinkedToken: THandle;
  end;
  PTokenLinkedToken = ^TTokenLinkedToken;

  // 10825
  TTokenElevation = record
    TokenIsElevated: LongBool;
  end;
  PTokenElevation = ^TTokenElevation;

  // 10829
  TTokenMandatoryLabel = record
    MandatoryLabel: TSidAndAttributes;
  end;
  PTokenMandatoryLabel = ^TTokenMandatoryLabel;

  // 10844
  TTokenMandatoryPolicy = record
    Policy: Cardinal;
  end;
  PTokenMandatoryPolicy = ^TTokenMandatoryPolicy;

  // 10850
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

  // 10872
  TTokenAuditPolicy = record
    // The actual length depends on the count of SubCategories of auditing.
    // Each half of a byte is a set of Winapi.NtSecApi.PER_USER_AUDIT_* flags.
    PerUserPolicy: array [Byte] of Byte;
  end;
  PTokenAuditPolicy = ^TTokenAuditPolicy;

  // 10878
  TTokenSource = record
    const TOKEN_SOURCE_LENGTH = 8;
  var
    sourcename: array[1 .. TOKEN_SOURCE_LENGTH] of AnsiChar;
    SourceIdentifier: TLuid;
  end;
  PTokenSource = ^TTokenSource;

  // 10884
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

  // 10906
  TTokenOrigin = record
    OriginatingLogonSession: TLuid;
  end;
  PTokenOrigin = ^TTokenOrigin;

  // 10929
  TTokenAppContainer = record
    TokenAppContainer: PSid;
  end;
  PTokenAppContainer = ^TTokenAppContainer;

  // 11051
  TClaimSecurityAttributeV1 = record
    Name: PWideChar;
    ValueType: Word;
    Reserved: Word;
    Flags: Cardinal;
    ValueCount: Integer;
    Values: Pointer;
  end;
  PClaimSecurityAttributeV1 = ^TClaimSecurityAttributeV1;

  // 11170
  TClaimSecurityAttributes = record
    Version: Word;
    Reserved: Word;
    AttributeCount: Cardinal;
    Attribute: PClaimSecurityAttributeV1;
  end;
  PClaimSecurityAttributes = ^TClaimSecurityAttributes;

  // 11206
  TSecurityQualityOfService = record
    Length: Cardinal;
    ImpersonationLevel: TSecurityImpersonationLevel;
    ContextTrackingMode: Boolean;
    EffectiveOnly: Boolean;
  end;
  PSecurityQualityOfService = ^TSecurityQualityOfService;

  // 11230
  TSecurityInformation = Cardinal;
  PSecurityInformation = ^TSecurityInformation;

  // 11481
  TQuotaLimits = record
    PagedPoolLimit: NativeUInt;
    NonPagedPoolLimit: NativeUInt;
    MinimumWorkingSetSize: NativeUInt;
    MaximumWorkingSetSize: NativeUInt;
    PagefileLimit: NativeUInt;
    TimeLimit: TLargeInteger;
  end;
  PQuotaLimits = ^TQuotaLimits;

  // 11519
  TIoCounters = record
    ReadOperationCount: UInt64;
    WriteOperationCount: UInt64;
    OtherOperationCount: UInt64;
    ReadTransferCount: UInt64;
    WriteTransferCount: UInt64;
    OtherTransferCount: UInt64;
  end;
  PIoCounters = ^TIoCounters;

const
  // 9175
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));

  // 9424
  SECURITY_MANDATORY_LABEL_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 16));

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Ntapi.ntexapi;

{ TAce_Internal }

function TAce_Internal.Sid: PSid;
begin
  Result := PSid(@Self.SidStart);
end;

{ TObjectAce_Internal }

function TObjectAce_Internal.Sid: PSid;
begin
  Result := PSid(@Self.SidStart);
end;

{ TAclSizeInformation }

function TAclSizeInformation.AclBytesTotal: Cardinal;
begin
  Result := AclBytesInUse + AclBytesFree;
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
  // After call to NtQuerySystemInformation we get timezone bias in minutes
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
