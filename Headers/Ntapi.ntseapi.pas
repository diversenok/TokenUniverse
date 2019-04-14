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
  // WinNt.10661
  TTokenInformationClass = (
    TokenInfoTPad = 0, // [Required to generate TypeInfo]
    TokenUser = 1,                             // q: TTokenUser
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
    TokenOrigin = 17,                          // q, s: TTokenOrigin
    TokenElevationType = 18,                   // q: TTokenElevationType
    TokenLinkedToken = 19,                     // q, s: TTokenLinkedToken
    TokenElevation = 20,                       // q: TTokenElevation
    TokenHasRestrictions = 21,                 // q: LongBool
    TokenAccessInformation = 22,               // q: TTokenAccessInformation
    TokenVirtualizationAllowed = 23,           // q, s: LongBool
    TokenVirtualizationEnabled = 24,           // q, s: LongBool
    TokenIntegrityLevel = 25,                  // q, s: TTokenMandatoryLabel
    TokenUIAccess = 26,                        // q, s: LongBool
    TokenMandatoryPolicy = 27,                 // q, s: TTokenMandatoryPolicy
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
  const User: TTokenUser; Groups: PTokenGroups; Privileges: PTokenPrivileges;
  Owner: PTokenOwner; PrimaryGroup: PTokenPrimaryGroup;
  DefaultDacl: PTokenDefaultDacl; const Source: TTokenSource): NTSTATUS;
  stdcall; external ntdll;

// Win 8+
function NtCreateLowBoxToken(out TokenHandle: THandle;
  ExistingTokenHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; PackageSid: PSID;
  CapabilityCount: Cardinal; Capabilities: PSIDAndAttributes;
  HandleCount: Cardinal; Handles: array of THandle): NTSTATUS; stdcall;
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
