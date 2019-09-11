unit Winapi.WinNt;

{$MINENUMSIZE 4}

interface

// Note: line numbers are valid for SDK 10.0.17134

const
  kernelbase = 'kernelbase.dll';
  kernel32  = 'kernel32.dll';
  advapi32  = 'advapi32.dll';
  secur32 = 'secur32.dll';

  INFINITE = $FFFFFFFF;

  // 7477
  CONTEXT_i386 = $00010000;

  CONTEXT_CONTROL = CONTEXT_i386 or $00000001;  // SS:SP, CS:IP, FLAGS, BP
  CONTEXT_INTEGER = CONTEXT_i386 or $00000002;  // AX, BX, CX, DX, SI, DI
  CONTEXT_SEGMENTS = CONTEXT_i386 or $00000004; // DS, ES, FS, GS
  CONTEXT_FLOATING_POINT = CONTEXT_i386 or $00000008;     // 387 state
  CONTEXT_DEBUG_REGISTERS = CONTEXT_i386 or $00000010;    // DB 0-3,6,7
  CONTEXT_EXTENDED_REGISTERS = CONTEXT_i386 or $00000020; // cpu specific extensions

  CONTEXT_FULL = CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS;
  CONTEXT_ALL = CONTEXT_FULL or CONTEXT_FLOATING_POINT or
    CONTEXT_DEBUG_REGISTERS or CONTEXT_EXTENDED_REGISTERS;

  CONTEXT_XSTATE = CONTEXT_i386 or $00000040;

  CONTEXT_EXCEPTION_ACTIVE = $08000000;
  CONTEXT_SERVICE_ACTIVE = $10000000;
  CONTEXT_EXCEPTION_REQUEST = $40000000;
  CONTEXT_EXCEPTION_REPORTING = $80000000;

  // EFLAGS register bits
  EFLAGS_CF = $0001; // Carry
  EFLAGS_PF = $0004; // Parity
  EFLAGS_AF = $0010; // Auxiliary Carry
  EFLAGS_ZF = $0040; // Zero
  EFLAGS_SF = $0080; // Sign
  EFLAGS_TF = $0100; // Trap
  EFLAGS_DF = $0400; // Direction
  EFLAGS_OF = $0800; // Overflow

  // 8894
  _DELETE = $00010000;      // SDDL: DE
  READ_CONTROL = $00020000; // SDDL: RC
  WRITE_DAC = $00040000;    // SDDL: WD
  WRITE_OWNER = $00080000;  // SDDL: WO
  SYNCHRONIZE = $00100000;  // SDDL: SY

  STANDARD_RIGHTS_REQUIRED = _DELETE or READ_CONTROL or WRITE_DAC or WRITE_OWNER;
  STANDARD_RIGHTS_READ = READ_CONTROL;
  STANDARD_RIGHTS_WRITE = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE = READ_CONTROL;
  STANDARD_RIGHTS_ALL = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE;
  SPECIFIC_RIGHTS_ALL = $0000FFFF;

  ACCESS_SYSTEM_SECURITY = $01000000; // SDDL: AS
  MAXIMUM_ALLOWED = $02000000;        // SDDL: MA

  GENERIC_READ = Cardinal($80000000); // SDDL: GR
  GENERIC_WRITE = $40000000;          // SDDL: GW
  GENERIC_EXECUTE = $20000000;        // SDDL: GX
  GENERIC_ALL = $10000000;            // SDDL: GA

  // 9020
  SID_MAX_SUB_AUTHORITIES = 15;
  SECURITY_MAX_SID_STRING_CHARACTERS = 2 + 4 + 15 +
    (11 * SID_MAX_SUB_AUTHORITIES) + 1;

  SECURITY_ANONYMOUS_LOGON_RID = $00000007;
  SECURITY_LOCAL_SYSTEM_RID    = $00000012;
  SECURITY_LOCAL_SERVICE_RID   = $00000013;
  SECURITY_NETWORK_SERVICE_RID = $00000014;

  // 9425
  SECURITY_MANDATORY_UNTRUSTED_RID = $0000;
  SECURITY_MANDATORY_LOW_RID = $1000;
  SECURITY_MANDATORY_MEDIUM_RID = $2000;
  SECURITY_MANDATORY_MEDIUM_PLUS_RID = SECURITY_MANDATORY_MEDIUM_RID + $0100;
  SECURITY_MANDATORY_HIGH_RID = $3000;
  SECURITY_MANDATORY_SYSTEM_RID = $4000;
  SECURITY_MANDATORY_PROTECTED_PROCESS_RID = $5000;

  // 9622
  SYSTEM_LUID = $3e7;
  ANONYMOUS_LOGON_LUID = $3e6;
  LOCALSERVICE_LUID = $3e5;
  NETWORKSERVICE_LUID = $3e4;

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

  // Token flags
  TOKEN_WRITE_RESTRICTED = $0008;
  TOKEN_IS_RESTRICTED = $0010;
  TOKEN_SESSION_NOT_REFERENCED = $0020;
  TOKEN_SANDBOX_INERT = $0040;
  TOKEN_VIRTUALIZE_ALLOWED = $0200;
  TOKEN_VIRTUALIZE_ENABLED = $0400;
  TOKEN_IS_FILTERED = $0800;
  TOKEN_UIACCESS = $1000;
  TOKEN_NOT_LOW = $2000;

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
  OWNER_SECURITY_INFORMATION = $00000001; // q: RC; s: WO
  GROUP_SECURITY_INFORMATION = $00000002; // q: RC; s: WO
  DACL_SECURITY_INFORMATION = $00000004;  // q: RC; s: WD
  SACL_SECURITY_INFORMATION = $00000008;  // q, s: AS
  LABEL_SECURITY_INFORMATION = $00000010; // q: RC; s: WO
  ATTRIBUTE_SECURITY_INFORMATION = $00000020; // q: RC; s: WD
  SCOPE_SECURITY_INFORMATION = $00000040; // q: RC; s: AS
  PROCESS_TRUST_LABEL_SECURITY_INFORMATION = $00000080;
  ACCESS_FILTER_SECURITY_INFORMATION = $00000100;
  BACKUP_SECURITY_INFORMATION = $00010000; // q, s: RC | AS; s: WD | WO | AS

  PROTECTED_DACL_SECURITY_INFORMATION = $80000000;   // s: WD
  PROTECTED_SACL_SECURITY_INFORMATION = $40000000;   // s: AS
  UNPROTECTED_DACL_SECURITY_INFORMATION = $20000000; // s: WD
  UNPROTECTED_SACL_SECURITY_INFORMATION = $10000000; // s: AS

type
  // If range checks are enabled make sure to wrap all accesses to any-size
  // arrays inside a {$R-}/{$R+} block which temporarily disables them.
  ANYSIZE_ARRAY = 0..0;

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

  TLuidArray = array [ANYSIZE_ARRAY] of TLuid;
  PLuidArray = ^TLuidArray;

  // 1119
  PListEntry = ^TListEntry;
  TListEntry = record
    Flink: PListEntry;
    Blink: PListEntry;
  end;

  // 2529
  {$ALIGN 16}
  M128A = record
    Low: UInt64;
    High: Int64;
  end;
  {$ALIGN 8}

  // 3837
  {$ALIGN 16}
  TContext64 = record
    PnHome: array [1..6] of UInt64;
    ContextFlags: Cardinal; // CONTEXT_*
    MxCsr: Cardinal;
    SegCs: WORD;
    SegDs: WORD;
    SegEs: WORD;
    SegFs: WORD;
    SegGs: WORD;
    SegSs: WORD;
    EFlags: Cardinal;
    Dr0: UInt64;
    Dr1: UInt64;
    Dr2: UInt64;
    Dr3: UInt64;
    Dr6: UInt64;
    Dr7: UInt64;
    Rax: UInt64;
    Rcx: UInt64;
    Rdx: UInt64;
    Rbx: UInt64;
    Rsp: UInt64;
    Rbp: UInt64;
    Rsi: UInt64;
    Rdi: UInt64;
    R8: UInt64;
    R9: UInt64;
    R10: UInt64;
    R11: UInt64;
    R12: UInt64;
    R13: UInt64;
    R14: UInt64;
    R15: UInt64;
    Rip: UInt64;
    FloatingPointState: array [0..31] of M128A;
    VectorRegister: array [0..25] of M128A;
    VectorControl: UInt64;
    DebugControl: UInt64;
    LastBranchToRip: UInt64;
    LastBranchFromRip: UInt64;
    LastExceptionToRip: UInt64;
    LastExceptionFromRip: UInt64
  end;
  PContext64 = ^TContext64;
  {$ALIGN 8}

  // 7507
  TFloatingSaveArea = record
  const
    SIZE_OF_80387_REGISTERS = 80;
  var
    ControlWord: Cardinal;
    StatusWord: Cardinal;
    TagWord: Cardinal;
    ErrorOffset: Cardinal;
    ErrorSelector: Cardinal;
    DataOffset: Cardinal;
    DataSelector: Cardinal;
    RegisterArea: array [0 .. SIZE_OF_80387_REGISTERS - 1] of Byte;
    Cr0NpxState: Cardinal;
  end;

  // 7548
  TContext32 = record
  const
    MAXIMUM_SUPPORTED_EXTENSION = 512;
  var
    ContextFlags: Cardinal; // CONTEXT_*
    Dr0: Cardinal;
    Dr1: Cardinal;
    Dr2: Cardinal;
    Dr3: Cardinal;
    Dr6: Cardinal;
    Dr7: Cardinal;
    FloatSave: TFloatingSaveArea;
    SegGs: Cardinal;
    SegFs: Cardinal;
    SegEs: Cardinal;
    SegDs: Cardinal;
    Edi: Cardinal;
    Esi: Cardinal;
    Ebx: Cardinal;
    Edx: Cardinal;
    Ecx: Cardinal;
    Eax: Cardinal;
    Ebp: Cardinal;
    Eip: Cardinal;
    SegCs: Cardinal;
    EFlags: Cardinal;
    Esp: Cardinal;
    SegSs: Cardinal;
    ExtendedRegisters: array [0 .. MAXIMUM_SUPPORTED_EXTENSION - 1] of Byte;
  end;
  PContext32 = ^TContext32;

  {$IFDEF WIN64}
  TContext = TContext64;
  {$ELSE}
  TContext = TContext32;
  {$ENDIF}
  PContext = ^TContext;

  // 8775
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
  const
    EXCEPTION_MAXIMUM_PARAMETERS = 15;
  var
    ExceptionCode: Cardinal;
    ExceptionFlags: Cardinal;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: Cardinal;
    ExceptionInformation: array [0 .. EXCEPTION_MAXIMUM_PARAMETERS - 1] of
      NativeUInt;
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

  // 8999
  TSidIdentifierAuthority = record
    Value: array [0..5] of Byte;
    function ToInt64: Int64;
    procedure FromInt64(IntValue: Int64);
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

  TSidArray = array [ANYSIZE_ARRAY] of PSid;
  PSidArray = ^TSidArray;

  // 9055
  TSidNameUse = (SidTypeUndefined, SidTypeUser, SidTypeGroup, SidTypeDomain,
    SidTypeAlias, SidTypeWellKnownGroup, SidTypeDeletedAccount, SidTypeInvalid,
    SidTypeUnknown, SidTypeComputer, SidTypeLabel, SidTypeLogonSession);

  // 9069
  TSidAndAttributes = record
    Sid: PSid;
    Attributes: Cardinal;
  end;
  PSidAndAttributes = ^TSidAndAttributes;

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
    Privilege: array [ANYSIZE_ARRAY] of TLuidAndAttributes;
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

  // 10768
  TTokenGroups = record
    GroupCount: Integer;
    Groups: array [ANYSIZE_ARRAY] of TSIDAndAttributes;
  end;
  PTokenGroups = ^TTokenGroups;

  // 10777
  TTokenPrivileges = record
    PrivilegeCount: Integer;
    Privileges: array [ANYSIZE_ARRAY] of TLUIDAndAttributes;
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
    DefaultDacl: PAcl;
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

  // 10850
  TTokenAccessInformation = record
    SidHash: PSIDAndAttributesHash;
    RestrictedSidHash: PSIDAndAttributesHash;
    Privileges: PTokenPrivileges;
    AuthenticationId: Int64;
    TokenType: TTokenType;
    ImpersonationLevel: TSecurityImpersonationLevel;
    MandatoryPolicy: Cardinal;
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
    PerUserPolicy: array [ANYSIZE_ARRAY] of Byte;
  end;
  PTokenAuditPolicy = ^TTokenAuditPolicy;

  // 10878
  TTokenSource = record
    const TOKEN_SOURCE_LENGTH = 8;
  var
    sourcename: array[1 .. TOKEN_SOURCE_LENGTH] of AnsiChar;
    SourceIdentifier: TLuid;
    procedure FromString(Name: String);
    function ToString: String;
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

  // 16684
  TImageFileHeader = record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: Cardinal;
    PointerToSymbolTable: Cardinal;
    NumberOfSymbols: Cardinal;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;
  PImageFileHeader = ^TImageFileHeader;

  // 16750
  TImageDataDirectory = record
    VirtualAddress: Cardinal;
    Size: Cardinal;
  end;
  PImageDataDirectory = ^TImageDataDirectory;

  // 16938 (out of order)
  {$MINENUMSIZE 2}
  TImageDirectoryEntry = (
    ImageDirectoryEntryExport = 0,
    ImageDirectoryEntryImport = 1,
    ImageDirectoryEntryResource = 2,
    ImageDirectoryEntryException = 3,
    ImageDirectoryEntrySecurity = 4,
    ImageDirectoryEntryBaseReloc = 5,
    ImageDirectoryEntryDebug = 6,
    ImageDirectoryEntryArchitecture = 7,
    ImageDirectoryEntryGlobalPtr = 8,
    ImageDirectoryEntryTls = 9,
    ImageDirectoryEntryLoadConfig = 10,
    ImageDirectoryEntryBoundImport = 11,
    ImageDirectoryEntryIat = 12,
    ImageDirectoryEntryDelayImport = 13,
    ImageDirectoryEntryComDescriptor = 14,
    ImageDirectoryEntryReserved = 15
  );
  {$MINENUMSIZE 4}

  // 16761
  TImageOptionalHeaders32 = record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Cardinal;
    SizeOfInitializedData: Cardinal;
    SizeOfUninitializedData: Cardinal;
    AddressOfEntryPoint: Cardinal;
    BaseOfCode: Cardinal;
    BaseOfData: Cardinal;
    ImageBase: Cardinal;
    SectionAlignment: Cardinal;
    FileAlignment: Cardinal;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: Cardinal;
    SizeOfImage: Cardinal;
    SizeOfHeaders: Cardinal;
    CheckSum: Cardinal;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Cardinal;
    SizeOfStackCommit: Cardinal;
    SizeOfHeapReserve: Cardinal;
    SizeOfHeapCommit: Cardinal;
    LoaderFlags: Cardinal;
    NumberOfRvaAndSizes: Cardinal;
    DataDirectory: array [TImageDirectoryEntry] of TImageDataDirectory;
  end;
  PImageOptionalHeaders32 = ^TImageOptionalHeaders32;

  // 16820
  TImageOptionalHeader64 = record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Cardinal;
    SizeOfInitializedData: Cardinal;
    SizeOfUninitializedData: Cardinal;
    AddressOfEntryPoint: Cardinal;
    BaseOfCode: Cardinal;
    ImageBase: UInt64;
    SectionAlignment: Cardinal;
    FileAlignment: Cardinal;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: Cardinal;
    SizeOfImage: Cardinal;
    SizeOfHeaders: Cardinal;
    CheckSum: Cardinal;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: UInt64;
    SizeOfStackCommit: UInt64;
    SizeOfHeapReserve: UInt64;
    SizeOfHeapCommit: UInt64;
    LoaderFlags: Cardinal;
    NumberOfRvaAndSizes: Cardinal;
    DataDirectory: array [TImageDirectoryEntry] of TImageDataDirectory;
  end;
  PImageOptionalHeader64 = ^TImageOptionalHeader64;

  // 16867
  TImageNtHeaders64 = record
    Signature: Cardinal;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;
  PImageNtHeaders64 = ^TImageNtHeaders64;

  // 16873
  TImageNtHeaders32 = record
    Signature: Cardinal;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeaders32;
  end;
  PImageNtHeaders32 = ^TImageNtHeaders32;

  // 17007
  TImageSectionHeader = record
  const
    IMAGE_SIZEOF_SHORT_NAME = 8;
  var
    Name: array [0 .. IMAGE_SIZEOF_SHORT_NAME - 1] of Byte;
    Misc: Cardinal;
    VirtualAddress: Cardinal;
    SizeOfRawData: Cardinal;
    PointerToRawData: Cardinal;
    PointerToRelocations: Cardinal;
    PointerToLinenumbers: Cardinal;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: Cardinal;
  end;
  PImageSectionHeader = ^TImageSectionHeader;
  PPImageSectionHeader = ^PImageSectionHeader;

  // 17867
  TImageExportDirectory = record
    Characteristics: Cardinal;
    TimeDateStamp: Cardinal;
    MajorVersion: Word;
    MinorVersion: Word;
    Name: Cardinal;
    Base: Cardinal;
    NumberOfFunctions: Cardinal;
    NumberOfNames: Cardinal;
    AddressOfFunctions: Cardinal;     // RVA from base of image
    AddressOfNames: Cardinal;         // RVA from base of image
    AddressOfNameOrdinals: Cardinal;  // RVA from base of image
  end;
  PImageExportDirectory = ^TImageExportDirectory;

const
  // 9175
  SECURITY_NT_AUTHORITY_ID = 5;
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));

  SECURITY_LOGON_IDS_RID = 5;
  SECURITY_LOGON_IDS_RID_COUNT = 3;

  // 9424
  SECURITY_MANDATORY_LABEL_AUTHORITY_ID = 16;
  SECURITY_MANDATORY_LABEL_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 16));

function PrivilegesToLuids(Privileges: TArray<TPrivilege>): TArray<TLuid>;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Ntapi.ntexapi;

{ TSidIdentifierAuthority }

procedure TSidIdentifierAuthority.FromInt64(IntValue: Int64);
begin
  Value[0] := Byte(IntValue shr 40);
  Value[1] := Byte(IntValue shr 32);
  Value[2] := Byte(IntValue shr 24);
  Value[3] := Byte(IntValue shr 16);
  Value[4] := Byte(IntValue shr 8);
  Value[5] := Byte(IntValue shr 0);
end;

function TSidIdentifierAuthority.ToInt64: Int64;
begin
  Result := (Int64(Value[5]) shl  0) or
            (Int64(Value[4]) shl  8) or
            (Int64(Value[3]) shl 16) or
            (Int64(Value[2]) shl 24) or
            (Int64(Value[1]) shl 32) or
            (Int64(Value[0]) shl 40);
end;

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

{ TTokenSource }

procedure TTokenSource.FromString(Name: String);
var
  i, Count: integer;
begin
  FillChar(sourcename, SizeOf(sourcename), 0);

  Count := Length(Name);
  if Count > 8 then
    Count := 8;

  for i := 1 to Count do
    sourcename[i] := AnsiChar(Name[Low(String) + i - 1]);
end;

function TTokenSource.ToString: String;
begin
  // sourcename field may or may not contain a zero-termination byte
  Result := String(PAnsiChar(AnsiString(sourcename)));
end;

{ Conversion functions }

function PrivilegesToLuids(Privileges: TArray<TPrivilege>): TArray<TLuid>;
var
  i: Integer;
begin
  SetLength(Result, Length(Privileges));

  for i := 0 to High(Privileges) do
    Result[i] := Privileges[i].Luid;
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
