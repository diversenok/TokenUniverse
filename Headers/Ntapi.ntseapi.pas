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
  SE_MAX_WELL_KNOWN_PRIVILEGE = 35;

function NtCreateToken(out TokenHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; TokenType: TTokenType;
  AuthenticationId: PLuid; ExpirationTime: PInt64; User: PTokenUser;
  Groups: PTokenGroups; Privileges: PTokenPrivileges; Owner: PTokenOwner;
  PrimaryGroup: PTokenPrimaryGroup; DefaultDacl: PTokenDefaultDacl;
  Source: PTokenSource): NTSTATUS; stdcall; external ntdll;

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
  TokenInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
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
