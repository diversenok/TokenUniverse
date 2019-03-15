unit Winapi.NtSecApi;

interface
{$MINENUMSIZE 4}

uses
  Ntapi.ntdef, Winapi.WinNt, Winapi.WinBase;

const
  NEGOSSP_NAME_A: AnsiString = 'Negotiate';

  // 2046
  PER_USER_POLICY_UNCHANGED = $00;
  PER_USER_AUDIT_SUCCESS_INCLUDE = $01;
  PER_USER_AUDIT_SUCCESS_EXCLUDE = $02;
  PER_USER_AUDIT_FAILURE_INCLUDE = $04;
  PER_USER_AUDIT_FAILURE_EXCLUDE = $08;
  PER_USER_AUDIT_NONE = $10;

  // 3690, values for UserFlags
  LOGON_GUEST = $01;
  LOGON_NOENCRYPTION = $02;
  LOGON_CACHED_ACCOUNT = $04;
  LOGON_USED_LM_PASSWORD = $08;
  LOGON_EXTRA_SIDS = $20;
  LOGON_SUBAUTH_SESSION_KEY = $40;
  LOGON_SERVER_TRUST_ACCOUNT = $80;
  LOGON_NTLMV2_ENABLED = $100;
  LOGON_RESOURCE_GROUPS = $200;
  LOGON_PROFILE_PATH_RETURNED = $400;
  LOGON_NT_V2 = $800;
  LOGON_LM_V2 = $1000;
  LOGON_NTLM_V2 = $2000;
  LOGON_OPTIMIZED = $4000;
  LOGON_WINLOGON = $8000;
  LOGON_PKINIT = $10000;
  LOGON_NO_OPTIMIZED = $20000;
  LOGON_NO_ELEVATION = $40000;
  LOGON_MANAGED_SERVICE = $80000;

type
  TLsaHandle = THandle;

  TLsaString = ANSI_STRING;
  PLsaString = ^TLsaString;

  TLsaUnicodeString = UNICODE_STRING;
  PLsaUnicodeString = ^TLsaUnicodeString;

  TLsaOperationalMode = Cardinal;

  TGuidArray = array [Byte] of TGUID;
  PGuidArray = ^TGuidArray;
  TGuidDynArray = array of TGUID;

  // 2760
  TLsaLastInterLogonInfo = record
    LastSuccessfulLogon: TLargeInteger;
    LastFailedLogon: TLargeInteger;
    FailedAttemptCountSinceLastSuccessfulLogon: Cardinal;
  end;
  PLsaLastInterLogonInfo = ^TLsaLastInterLogonInfo;

  // 2769
  TSecurityLogonSessionData = record
    Size: Cardinal;
    LogonId: TLuid;
    UserName: TLsaUnicodeString;
    LogonDomain: TLsaUnicodeString;
    AuthenticationPackage: TLsaUnicodeString;
    LogonType: TSecurityLogonType;
    Session: Cardinal;
    Sid: PSID;
    LogonTime: TLargeInteger;
    LogonServer: TLsaUnicodeString;
    DnsDomainName: TLsaUnicodeString;
    Upn: TLsaUnicodeString;
    UserFlags: Cardinal;
    LastLogonInfo: TLsaLastInterLogonInfo;
    LogonScript: TLsaUnicodeString;
    ProfilePath: TLsaUnicodeString;
    HomeDirectory: TLsaUnicodeString;
    HomeDirectoryDrive: TLsaUnicodeString;
    LogoffTime: TLargeInteger;
    KickOffTime: TLargeInteger;
    PasswordLastSet: TLargeInteger;
    PasswordCanChange: TLargeInteger;
    PasswordMustChange: TLargeInteger;
  end;
  PSecurityLogonSessionData = ^TSecurityLogonSessionData;

  // 4335
  TKerbLogonSubmitType = (
    KerbInteractiveLogon = 2,
    KerbSmartCardLogon = 6,
    KerbWorkstationUnlockLogon = 7,
    KerbSmartCardUnlockLogon = 8,
    KerbProxyLogon = 9,
    KerbTicketLogon = 10,
    KerbTicketUnlockLogon = 11,
    KerbS4ULogon = 12,
    KerbCertificateLogon = 13,
    KerbCertificateS4ULogon = 14,
    KerbCertificateUnlockLogon = 15
  );

  // 4469
  KERB_S4U_LOGON = record
    MessageType: TKerbLogonSubmitType;
    Flags: Cardinal;
    ClientUpn: UNICODE_STRING;
    ClientRealm: UNICODE_STRING;
  end;
  PKERB_S4U_LOGON = ^KERB_S4U_LOGON;

  // 5194
  TPolicyAuditSidArray = record
    UsersCount: Cardinal;
    UserSidArray: PSidArray;
  end;
  PPolicyAuditSidArray = ^TPolicyAuditSidArray;

  // 5205
  TAuditPolicyInformation = record
    AuditSubCategoryGuid: TGuid;
    AuditingInformation: Cardinal;
    AuditCategoryGuid: TGuid;
  end;
  PAuditPolicyInformation = ^TAuditPolicyInformation;

  TPAuditPolicyInformationArray = array [Byte] of PAuditPolicyInformation;
  PPAuditPolicyInformationArray = ^TPAuditPolicyInformationArray;

// 1648
function LsaRegisterLogonProcess(const LogonProcessName: TLsaString;
  out LsaHandle: TLsaHandle; out SecurityMode: TLsaOperationalMode): NTSTATUS;
  stdcall; external secur32;

// 1663
function LsaLogonUser(LsaHandle: TLsaHandle; const OriginName: TLsaString;
  LogonType: TSecurityLogonType; AuthenticationPackage: Cardinal;
  AuthenticationInformation: Pointer; AuthenticationInformationLength: Cardinal;
  LocalGroups: PTokenGroups; const SourceContext: TTokenSource;
  out ProfileBuffer: Pointer; out ProfileBufferLength: Cardinal;
  out LogonId: TLuid; out hToken: THandle; out Quotas: TQuotaLimits;
  out SubStatus: NTSTATUS): NTSTATUS; stdcall; external secur32;

// 1686
function LsaLookupAuthenticationPackage(LsaHandle: TLsaHandle;
  const PackageName: TLsaString; out AuthenticationPackage: Cardinal): NTSTATUS;
  stdcall; external secur32;

// 1697
function LsaFreeReturnBuffer(Buffer: Pointer): NTSTATUS; stdcall;
  external secur32;

// 1721
function LsaDeregisterLogonProcess(LsaHandle: TLsaHandle): NTSTATUS; stdcall;
  external secur32;

// 1729
function LsaConnectUntrusted(out LsaHandle: TLsaHandle): NTSTATUS; stdcall;
  external secur32;

// 2813
function LsaEnumerateLogonSessions(out LogonSessionCount: Integer;
  out LogonSessionList: PLuidArray): NTSTATUS; stdcall; external secur32;

// 2820
function LsaGetLogonSessionData(var LogonId: TLuid;
  out pLogonSessionData: PSecurityLogonSessionData): NTSTATUS; stdcall;
  external secur32;

// 5264
function AuditQuerySystemPolicy(pSubCategoryGuids: TGuidDynArray;
  dwPolicyCount: Cardinal; ppAuditPolicy: PPAuditPolicyInformationArray):
  Boolean; stdcall; external advapi32;

// 5274
function AuditQueryPerUserPolicy(pSid: PSid; SubCategoryGuids: TGuidDynArray;
  dwPolicyCount: Cardinal; ppAuditPolicy: PPAuditPolicyInformationArray):
  Boolean; stdcall; external advapi32;

// 5285
function AuditEnumeratePerUserPolicy(out pAuditSidArray: PPolicyAuditSidArray):
  Boolean; stdcall; external advapi32;

// 5314
function AuditEnumerateCategories(out pAuditCategoriesArray: PGuidArray;
  out dwCountReturned: Cardinal): Boolean; stdcall; external advapi32;

// 5323
function AuditEnumerateSubCategories(const AuditCategoryGuid: TGuid;
  bRetrieveAllSubCategories: Boolean; out pAuditSubCategoriesArray: PGuidArray;
  out dwCountReturned: Cardinal): Boolean; stdcall; external advapi32;

// 5334
function AuditLookupCategoryNameW(const AuditCategoryGuid: TGuid;
  out pszCategoryName: PWideChar): Boolean; stdcall; external advapi32;

// 5356
function AuditLookupSubCategoryNameW(const AuditSubCategoryGuid: TGuid;
  out pszSubCategoryName: PWideChar): Boolean; stdcall; external advapi32;

// 5448
procedure AuditFree(Buffer: Pointer); stdcall; external advapi32;

implementation

end.
