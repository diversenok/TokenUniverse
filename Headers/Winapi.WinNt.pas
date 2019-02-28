unit Winapi.WinNt;
{$MINENUMSIZE 4}

interface

// Note: line numbers are valid for SDK 10.0.17134

const
  kernelbase = 'kernelbase.dll';
  kernel32  = 'kernel32.dll';
  advapi32  = 'advapi32.dll';

  // 8881
  _DELETE = $00010000;
  READ_CONTROL = $00020000;
  WRITE_DAC = $00040000;
  WRITE_OWNER = $00080000;
  SYNCHRONIZE = $00100000;

  STANDARD_RIGHTS_REQUIRED = $000F0000;
  STANDARD_RIGHTS_READ = READ_CONTROL;
  STANDARD_RIGHTS_WRITE = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE = READ_CONTROL;
  STANDARD_RIGHTS_ALL = $001F0000;
  SPECIFIC_RIGHTS_ALL = $0000FFFF;

  ACCESS_SYSTEM_SECURITY = $01000000;
  MAXIMUM_ALLOWED = $02000000;

  GENERIC_READ = Cardinal($80000000);
  GENERIC_WRITE = $40000000;
  GENERIC_EXECUTE = $20000000;
  GENERIC_ALL = $10000000;

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

type
  // 818
  TLargeInteger = Int64;
  PLargeInteger = ^TLargeInteger;

  // 838
  TULargeInteger = UInt64;
  PULargeInteger = ^TULargeInteger;

  // 873
  TLuid = Int64;
  PLuid = ^TLuid;

  // 1119
  PListEntry = ^TListEntry;
  TListEntry = record
    Flink: PListEntry;
    Blink: PListEntry;
  end;

  // 8822
  PSid = Pointer;

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

  // 9695
  TAcl = record
    AclRevision: Byte;
    Sbz1: Byte;
    AclSize: Word;
    AceCount: Word;
    Sbz2: Word;
  end;
  PAcl = ^TAcl;

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

implementation

end.
