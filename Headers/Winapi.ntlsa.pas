unit Winapi.ntlsa;
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Winapi.NtSecApi;

const
  MAX_PREFERRED_LENGTH = MaxInt;

  // 174, values for Lsa[Get/Set]SystemAccessAccount
  SECURITY_ACCESS_INTERACTIVE_LOGON = $00000001;
  SECURITY_ACCESS_NETWORK_LOGON = $00000002;
  SECURITY_ACCESS_BATCH_LOGON = $00000004;
  SECURITY_ACCESS_SERVICE_LOGON = $00000010;
  SECURITY_ACCESS_DENY_INTERACTIVE_LOGON = $00000040;
  SECURITY_ACCESS_DENY_NETWORK_LOGON = $00000080;
  SECURITY_ACCESS_DENY_BATCH_LOGON = $00000100;
  SECURITY_ACCESS_DENY_SERVICE_LOGON = $00000200;
  SECURITY_ACCESS_REMOTE_INTERACTIVE_LOGON = $00000400;
  SECURITY_ACCESS_DENY_REMOTE_INTERACTIVE_LOGON = $00000800;

  // 1757
  POLICY_VIEW_LOCAL_INFORMATION = $00000001;
  POLICY_VIEW_AUDIT_INFORMATION = $00000002;
  POLICY_GET_PRIVATE_INFORMATION = $00000004;
  POLICY_TRUST_ADMIN = $00000008;
  POLICY_CREATE_ACCOUNT = $00000010;
  POLICY_CREATE_SECRET = $00000020;
  POLICY_CREATE_PRIVILEGE = $00000040;
  POLICY_SET_DEFAULT_QUOTA_LIMITS = $00000080;
  POLICY_SET_AUDIT_REQUIREMENTS = $00000100;
  POLICY_AUDIT_LOG_ADMIN = $00000200;
  POLICY_SERVER_ADMIN = $00000400;
  POLICY_LOOKUP_NAMES = $00000800;
  POLICY_NOTIFICATION = $00001000;

  // 2452
  ACCOUNT_VIEW = $00000001;
  ACCOUNT_ADJUST_PRIVILEGES = $00000002;
  ACCOUNT_ADJUST_QUOTAS = $00000004;
  ACCOUNT_ADJUST_SYSTEM_ACCESS = $00000008;

  // 3627, see SECURITY_ACCESS_*
  SE_INTERACTIVE_LOGON_NAME = 'SeInteractiveLogonRight';
  SE_NETWORK_LOGON_NAME = 'SeNetworkLogonRight';
  SE_BATCH_LOGON_NAME = 'SeBatchLogonRight';
  SE_SERVICE_LOGON_NAME = 'SeServiceLogonRight';
  SE_DENY_INTERACTIVE_LOGON_NAME = 'SeDenyInteractiveLogonRight';
  SE_DENY_NETWORK_LOGON_NAME = 'SeDenyNetworkLogonRight';
  SE_DENY_BATCH_LOGON_NAME = 'SeDenyBatchLogonRight';
  SE_DENY_SERVICE_LOGON_NAME = 'SeDenyServiceLogonRight';
  SE_REMOTE_INTERACTIVE_LOGON_NAME = 'SeRemoteInteractiveLogonRight';
  SE_DENY_REMOTE_INTERACTIVE_LOGON_NAME = 'SeDenyRemoteInteractiveLogonRight';

type
  TLsaHandle = Winapi.NtSecApi.TLsaHandle;
  TLsaEnumerationHandle = Cardinal;

  // 1956
  TPolicyPrivilegeDefinition = record
    Name: TLsaUnicodeString;
    LocalValue: TLuid;
  end;
  PPolicyPrivilegeDefinition = ^TPolicyPrivilegeDefinition;

  TPolicyPrivilegeDefinitionArray = array [Byte] of TPolicyPrivilegeDefinition;
  PPolicyPrivilegeDefinitionArray = ^TPolicyPrivilegeDefinitionArray;

  // Winapi.LsaLookup 70
  TLsaTrustInformation = record
    Name: TLsaUnicodeString;
    Sid: PSid;
  end;
  PLsaTrustInformation = ^TLsaTrustInformation;

  TLsaTrustInformationArray = array [Word] of TLsaTrustInformation;
  PLsaTrustInformationArray = ^TLsaTrustInformationArray;

  // Winapi.LsaLookup 89
  TLsaReferencedDomainList = record
    Entries: Integer;
    Domains: PLsaTrustInformationArray;
  end;
  PLsaReferencedDomainList = ^TLsaReferencedDomainList;

  // Winapi.LsaLookup 111
  TLsaTranslatedSid2 = record
    Use: TSidNameUse;
    Sid: PSid;
    DomainIndex: Integer;
    Flags: Cardinal;
  end;
  PLsaTranslatedSid2 = ^TLsaTranslatedSid2;

  TLsaTranslatedSid2Array = array [Word] of TLsaTranslatedSid2;
  PLsaTranslatedSid2Array = ^TLsaTranslatedSid2Array;

  // Winapi.LsaLookup 142
  TLsaTranslatedName = record
    Use: TSidNameUse;
    Name: TLsaUnicodeString;
    DomainIndex: Integer;
  end;
  PLsaTranslatedName = ^TLsaTranslatedName;

  TLsaTranslatedNameArray = array [Word] of TLsaTranslatedName;
  PLsaTranslatedNameArray = ^TLsaTranslatedNameArray;

// 2983
function LsaFreeMemory(Buffer: Pointer): NTSTATUS; stdcall; external advapi32;

// 2989
function LsaClose(ObjectHandle: TLsaHandle): NTSTATUS; stdcall;
  external advapi32;

// 2997
function LsaDelete(ObjectHandle: TLsaHandle): NTSTATUS; stdcall;
  external advapi32;

// 3108
function LsaOpenPolicy(SystemName: PLsaUnicodeString;
  const ObjectAttributes: TObjectAttributes; DesiredAccess: TAccessMask;
  out PolicyHandle: TLsaHandle): NTSTATUS; stdcall; external advapi32;

// 3329
function LsaCreateAccount(PolicyHandle: TLsaHandle;
  AccountSid: PSid; DesiredAccess: TAccessMask; out AccountHandle: TLsaHandle):
  NTSTATUS; stdcall; external advapi32;

// 3338
function LsaEnumerateAccounts(PolicyHandle: TLsaHandle;
  var EnumerationContext: TLsaEnumerationHandle;
  out Buffer: PSidArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external advapi32;

// 3371
function LsaEnumeratePrivileges(PolicyHandle: TLsaHandle;
  var EnumerationContext: TLsaEnumerationHandle;
  out Buffer: PPolicyPrivilegeDefinitionArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external advapi32;

// 3444
function LsaOpenAccount(PolicyHandle: TLsaHandle;
  AccountSid: PSid; DesiredAccess: TAccessMask; out AccountHandle: TLsaHandle):
  NTSTATUS; stdcall; external advapi32;

// 3453
function LsaEnumeratePrivilegesOfAccount(AccountHandle: TLsaHandle;
  out Privileges: PPrivilegeSet): NTSTATUS; stdcall; external advapi32;

// 3460
function LsaAddPrivilegesToAccount(AccountHandle: TLsaHandle;
  Privileges: PPrivilegeSet): NTSTATUS; stdcall; external advapi32;

// 3467
function LsaRemovePrivilegesFromAccount(AccountHandle: TLsaHandle;
  AllPrivileges: Boolean; Privileges: PPrivilegeSet): NTSTATUS; stdcall;
  external advapi32;

// 3475
function LsaGetQuotasForAccount(AccountHandle: TLsaHandle;
  out QuotaLimits: TQuotaLimits): NTSTATUS; stdcall; external advapi32;

// 3482
function LsaSetQuotasForAccount(AccountHandle: TLsaHandle;
  const QuotaLimits: PQuotaLimits): NTSTATUS; stdcall; external advapi32;

// 3489
function LsaGetSystemAccessAccount(AccountHandle: TLsaHandle;
  out SystemAccess: Cardinal): NTSTATUS; stdcall; external advapi32;

// 3496
function LsaSetSystemAccessAccount(AccountHandle: TLsaHandle;
  SystemAccess: Cardinal): NTSTATUS; stdcall; external advapi32;

// 3574
function LsaLookupPrivilegeValue(PolicyHandle: TLsaHandle;
  const Name: TLsaUnicodeString; out Value: TLuid): NTSTATUS; stdcall;
  external advapi32;

// 3582
function LsaLookupPrivilegeName(PolicyHandle: TLsaHandle;
  var Value: TLuid; out Name: PLsaUnicodeString): NTSTATUS; stdcall;
  external advapi32;

// 3590
function LsaLookupPrivilegeDisplayName(PolicyHandle: TLsaHandle;
  const Name: TLsaUnicodeString; out DisplayName: PLsaUnicodeString;
  out LanguageReturned: Smallint): NTSTATUS; stdcall; external advapi32;

// 3605
function LsaGetUserName(out UserName: PLsaUnicodeString;
  out DomainName: PLsaUnicodeString): NTSTATUS; stdcall; external advapi32;

// 3394
function LsaLookupNames2(PolicyHandle: TLsaHandle; Flags: Cardinal;
  Count: Integer; const Name: TLsaUnicodeString;
  out ReferencedDomain: PLsaReferencedDomainList;
  out Sid: PLsaTranslatedSid2): NTSTATUS; stdcall;
  external advapi32; overload;

// 3394
function LsaLookupNames2(PolicyHandle: TLsaHandle; Flags: Cardinal;
  Count: Integer; Names: TLsaUnicodeStringDynArray;
  out ReferencedDomains: PLsaReferencedDomainList;
  out Sids: PLsaTranslatedSid2Array): NTSTATUS; stdcall;
  external advapi32; overload;

// 3406
function LsaLookupSids(PolicyHandle: TLsaHandle; Count: Cardinal;
  Sids: TSidDynArray; out ReferencedDomains: PLsaReferencedDomainList;
  out Names: PLsaTranslatedNameArray): NTSTATUS; stdcall; external advapi32;

implementation

end.
