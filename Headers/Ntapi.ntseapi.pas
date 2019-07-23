unit Ntapi.ntseapi;

{$WARN SYMBOL_PLATFORM OFF}
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntrtl;

const
  TOKEN_ASSIGN_PRIMARY = $0001;
  TOKEN_DUPLICATE = $0002;
  TOKEN_IMPERSONATE = $0004;
  TOKEN_QUERY = $0008;
  TOKEN_QUERY_SOURCE = $0010;
  TOKEN_ADJUST_PRIVILEGES = $0020;
  TOKEN_ADJUST_GROUPS = $0040;
  TOKEN_ADJUST_DEFAULT = $0080;
  TOKEN_ADJUST_SESSIONID = $0100;

  TOKEN_ALL_ACCESS_P = STANDARD_RIGHTS_REQUIRED  or
    TOKEN_ASSIGN_PRIMARY or TOKEN_DUPLICATE or TOKEN_IMPERSONATE or
    TOKEN_QUERY or TOKEN_QUERY_SOURCE or TOKEN_ADJUST_PRIVILEGES or
    TOKEN_ADJUST_GROUPS or TOKEN_ADJUST_DEFAULT;

  TOKEN_ALL_ACCESS = TOKEN_ALL_ACCESS_P or TOKEN_ADJUST_SESSIONID;

  DISABLE_MAX_PRIVILEGE = $1;
  SANDBOX_INERT = $2;
  LUA_TOKEN = $4;
  WRITE_RESTRICTED = $8;

  SE_MIN_WELL_KNOWN_PRIVILEGE = 2;
  SE_MAX_WELL_KNOWN_PRIVILEGE = 36;

type
  {$MINENUMSIZE 1}
  TSeWellKnownPrivilege = (
    SE_RESERVED_LUID_0 = 0,
    SE_RESERVED_LUID_1 = 1,
    SE_CREATE_TOKEN_PRIVILEGE = 2,
    SE_ASSIGN_PRIMARY_TOKEN_PRIVILEGE = 3,
    SE_LOCK_MEMORY_PRIVILEGE = 4,
    SE_INCREASE_QUOTA_PRIVILEGE = 5,
    SE_MACHINE_ACCOUNT_PRIVILEGE = 6,
    SE_TCB_PRIVILEGE = 7,
    SE_SECURITY_PRIVILEGE = 8,
    SE_TAKE_OWNERSHIP_PRIVILEGE = 9,
    SE_LOAD_DRIVER_PRIVILEGE = 10,
    SE_SYSTEM_PROFILE_PRIVILEGE = 11,
    SE_SYSTEMTIME_PRIVILEGE = 12,
    SE_PROFILE_SINGLE_PROCESS_PRIVILEGE = 13,
    SE_INCREASE_BASE_PRIORITY_PRIVILEGE = 14,
    SE_CREATE_PAGEFILE_PRIVILEGE = 15,
    SE_CREATE_PERMANENT_PRIVILEGE = 16,
    SE_BACKUP_PRIVILEGE = 17,
    SE_RESTORE_PRIVILEGE = 18,
    SE_SHUTDOWN_PRIVILEGE = 19,
    SE_DEBUG_PRIVILEGE = 20,
    SE_AUDIT_PRIVILEGE = 21,
    SE_SYSTEM_ENVIRONMENT_PRIVILEGE = 22,
    SE_CHANGE_NOTIFY_PRIVILEGE = 23,
    SE_REMOTE_SHUTDOWN_PRIVILEGE = 24,
    SE_UNDOCK_PRIVILEGE = 25,
    SE_SYNC_AGENT_PRIVILEGE = 26,
    SE_ENABLE_DELEGATION_PRIVILEGE = 27,
    SE_MANAGE_VOLUME_PRIVILEGE = 28,
    SE_IMPERSONATE_PRIVILEGE = 29,
    SE_CREATE_GLOBAL_PRIVILEGE = 30,
    SE_TRUSTED_CREDMAN_ACCESS_PRIVILEGE = 31,
    SE_RELABEL_PRIVILEGE = 32,
    SE_INCREASE_WORKING_SET_PRIVILEGE = 33,
    SE_TIME_ZONE_PRIVILEGE = 34,
    SE_CREATE_SYMBOLIC_LINK_PRIVILEGE = 35,
    SE_DELEGATE_SESSION_USER_IMPERSONATE_PRIVILEGE = 36
  );
  {$MINENUMSIZE 4}

  // WinNt.10661
  TTokenInformationClass = (
    TokenInfoTPad = 0, // [Required to generate TypeInfo]
    TokenUser = 1,                             // q: TSidAndAttributes
    TokenGroups = 2,                           // q: TTokenGroups
    TokenPrivileges = 3,                       // q: TTokenPrivileges
    TokenOwner = 4,                            // q, s: TTokenOwner
    TokenPrimaryGroup = 5,                     // q, s: TTokenPrimaryGroup
    TokenDefaultDacl = 6,                      // q, s: TTokenDefaultDacl
    TokenSource = 7,                           // q: TTokenSource
    TokenType = 8,                             // q: TTokenType
    TokenImpersonationLevel = 9,               // q: TSecurityImpersonationLevel
    TokenStatistics = 10,                      // q: TTokenStatistics
    TokenRestrictedSids = 11,                  // q: TTokenGroups
    TokenSessionId = 12,                       // q, s: Cardinal
    TokenGroupsAndPrivileges = 13,             // q: TTokenGroupsAndPrivileges
    TokenSessionReference = 14,                // s: LongBool
    TokenSandBoxInert = 15,                    // q: LongBool
    TokenAuditPolicy = 16,                     // q, s: TTokenAuditPolicy
    TokenOrigin = 17,                          // q, s: TLuid
    TokenElevationType = 18,                   // q: TTokenElevationType
    TokenLinkedToken = 19,                     // q, s: THandle
    TokenElevation = 20,                       // q: LongBool
    TokenHasRestrictions = 21,                 // q: LongBool
    TokenAccessInformation = 22,               // q: TTokenAccessInformation
    TokenVirtualizationAllowed = 23,           // q, s: LongBool
    TokenVirtualizationEnabled = 24,           // q, s: LongBool
    TokenIntegrityLevel = 25,                  // q, s: TSidAndAttributes
    TokenUIAccess = 26,                        // q, s: LongBool
    TokenMandatoryPolicy = 27,                 // q, s: Cardinal
    TokenLogonSid = 28,                        // q: TTokenGroups
    TokenIsAppContainer = 29,                  // q: LongBool
    TokenCapabilities = 30,                    // q: TTokenGroups
    TokenAppContainerSid = 31,                 // q: TTokenAppContainer
    TokenAppContainerNumber = 32,              // q: Cardinal
    TokenUserClaimAttributes = 33,             // q: TClaimSecurityAttributes
    TokenDeviceClaimAttributes = 34,           // q: TClaimSecurityAttributes
    TokenRestrictedUserClaimAttributes = 35,   // q:
    TokenRestrictedDeviceClaimAttributes = 36, // q:
    TokenDeviceGroups = 37,                    // q: TTokenGroups
    TokenRestrictedDeviceGroups = 38,          // q: TTokenGroups
    TokenSecurityAttributes = 39,              // q, s:
    TokenIsRestricted = 40,                    // q: LongBool
    TokenProcessTrustLevel = 41,               // q:
    TokenPrivateNameSpace = 42,                // q, s: LongBool
    TokenSingletonAttributes = 43,             // q:
    TokenBnoIsolation = 44,                    // q:
    TokenChildProcessFlags = 45,               // q:
    TokenIsLessPrivilegedAppContainer = 46     // q:
  );

function NtCreateToken(out TokenHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; TokenType: TTokenType;
  var AuthenticationId: TLuid; var ExpirationTime: TLargeInteger;
  const User: TSidAndAttributes; Groups: PTokenGroups;
  Privileges: PTokenPrivileges; Owner: PTokenOwner;
  var PrimaryGroup: TTokenPrimaryGroup; DefaultDacl: PTokenDefaultDacl;
  const Source: TTokenSource): NTSTATUS; stdcall; external ntdll;

// Win 8+
function NtCreateLowBoxToken(out TokenHandle: THandle;
  ExistingTokenHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; PackageSid: PSID;
  CapabilityCount: Cardinal; Capabilities: PSIDAndAttributes;
  HandleCount: Cardinal; Handles: TArray<THandle>): NTSTATUS; stdcall;
  external ntdll delayed;

function NtOpenProcessTokenEx(ProcessHandle: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal;
  out TokenHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtOpenThreadTokenEx(ThreadHandle: THandle;
  DesiredAccess: TAccessMask; OpenAsSelf: Boolean; HandleAttributes: Cardinal;
  out TokenHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtDuplicateToken(ExistingTokenHandle: THandle;
  DesiredAccess: TAccessMask; ObjectAttributes: PObjectAttributes;
  EffectiveOnly: LongBool; TokenType: TTokenType; out NewTokenHandle: THandle)
  : NTSTATUS; stdcall; external ntdll;

function NtQueryInformationToken(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal; out ReturnLength: Cardinal): NTSTATUS;
  stdcall; external ntdll;

function NtSetInformationToken(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass;
  TokenInformation: Pointer; TokenInformationLength: Cardinal): NTSTATUS;
  stdcall; external ntdll;

function NtAdjustPrivilegesToken(TokenHandle: THandle;
  DisableAllPrivileges: Boolean; NewState: PTokenPrivileges;
  BufferLength: Cardinal; PreviousState: PTokenPrivileges;
  ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

function NtAdjustGroupsToken(TokenHandle: THandle; ResetToDefault: Boolean;
  NewState: PTokenGroups; BufferLength: Cardinal; PreviousState:
  PTokenPrivileges; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

function NtFilterToken(ExistingTokenHandle: THandle; Flags: Cardinal;
  SidsToDisable: PTokenGroups; PrivilegesToDelete: PTokenPrivileges;
  RestrictedSids: PTokenGroups; out NewTokenHandle: THandle): NTSTATUS;
  stdcall; external ntdll;

function NtCompareTokens(FirstTokenHandle: THandle; SecondTokenHandle: THandle;
  out Equal: LongBool): NTSTATUS; stdcall; external ntdll;

function NtImpersonateAnonymousToken(ThreadHandle: THandle): NTSTATUS;
  stdcall; external ntdll;

implementation

end.
