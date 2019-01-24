unit Ntapi.ntseapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, TU.Winapi, Ntapi.ntdef, Ntapi.ntrtl;

const
  SE_MIN_WELL_KNOWN_PRIVILEGE = 2;
  SE_MAX_WELL_KNOWN_PRIVILEGE = 35;

function NtCreateToken(out TokenHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; TokenType: TTokenType;
  AuthenticationId: PLUID; ExpirationTime: PInt64; User: PTokenUser;
  Groups: PTokenGroups; Privileges: PTokenPrivileges; Owner: PTokenOwner;
  PrimaryGroup: PTokenPrimaryGroup; DefaultDacl: PTokenDefaultDacl;
  Source: PTokenSource): NTSTATUS; stdcall; external ntdll;

function NtOpenProcessTokenEx(ProcessHandle: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal;
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

implementation

end.
