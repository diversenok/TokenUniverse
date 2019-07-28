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

  POLICY_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $0FFF;

  // 2330
  POLICY_QOS_SCHANNEL_REQUIRED = $00000001;
  POLICY_QOS_OUTBOUND_INTEGRITY = $00000002;
  POLICY_QOS_OUTBOUND_CONFIDENTIALITY = $00000004;
  POLICY_QOS_INBOUND_INTEGRITY = $00000008;
  POLICY_QOS_INBOUND_CONFIDENTIALITY = $00000010;
  POLICY_QOS_ALLOW_LOCAL_ROOT_CERT_STORE = $00000020;
  POLICY_QOS_RAS_SERVER_ALLOWED = $00000040;
  POLICY_QOS_DHCP_SERVER_ALLOWED = $00000080;

  // 2383
  POLICY_KERBEROS_VALIDATE_CLIENT = $00000080;

  // 2452
  ACCOUNT_VIEW = $00000001;
  ACCOUNT_ADJUST_PRIVILEGES = $00000002;
  ACCOUNT_ADJUST_QUOTAS = $00000004;
  ACCOUNT_ADJUST_SYSTEM_ACCESS = $00000008;

  ACCOUNT_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $000F;

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

  // 1900
  TPolicyLsaServerRole = (
    PolicyServerRoleBackup = 2,
    PolicyServerRolePrimary = 3
  );

  // 1956
  TPolicyPrivilegeDefinition = record
    Name: TLsaUnicodeString;
    LocalValue: TLuid;
  end;
  PPolicyPrivilegeDefinition = ^TPolicyPrivilegeDefinition;

  TPolicyPrivilegeDefinitionArray = array [ANYSIZE_ARRAY] of
    TPolicyPrivilegeDefinition;
  PPolicyPrivilegeDefinitionArray = ^TPolicyPrivilegeDefinitionArray;

  // 2024
  TPolicyInformationClass = (
    PolicyReserved = 0,
    PolicyAuditLogInformation = 1,      // q:
    PolicyAuditEventsInformation = 2,   // q, s:
    PolicyPrimaryDomainInformation = 3, // q, s: TPolicyPrimaryDomainInfo
    PolicyPdAccountInformation = 4,     // q: TLsaUnicodeString
    PolicyAccountDomainInformation = 5, // q, s:
    PolicyLsaServerRoleInformation = 6, // q, s: TPolicyLsaServerRole
    PolicyReplicaSourceInformation = 7, // q, s: TPolicyReplicaSourceInfo
    PolicyDefaultQuotaInformation = 8,  // q, s: TQuotaLimits
    PolicyModificationInformation = 9,  // q: TPolicyModificationInfo
    PolicyAuditFullSetInformation = 10,
    PolicyAuditFullQueryInformation = 11,
    PolicyDnsDomainInformation = 12,    // q, s:
    PolicyDnsDomainInformationInt = 13,
    PolicyLocalAccountDomainInformation = 14, // q, s:
    PolicyMachineAccountInformation = 15 // q: TPolicyMachineAcctInfo
  );

  // 2188
  TPolicyPrimaryDomainInfo = record
    Name: TLsaUnicodeString;
    Sid: PSid;
  end;
  PPolicyPrimaryDomainInfo = ^TPolicyPrimaryDomainInfo;

  // 2244
  TPolicyReplicaSourceInfo = record
    ReplicaSource: TLsaUnicodeString;
    ReplicaAccountName: TLsaUnicodeString;
  end;
  PPolicyReplicaSourceInfo = ^TPolicyReplicaSourceInfo;

  // 2269
  TPolicyModificationInfo = record
    ModifiedId: TLargeInteger;
    DatabaseCreationTime: TLargeInteger;
  end;
  PPolicyModificationInfo = ^TPolicyModificationInfo;

  // 2315
  TPolicyDomainInformationClass = (
    PolicyDomainReserved = 0,
    PolicyDomainQualityOfServiceInformation = 1, // Cardinal, POLICY_QOS_*
    PolicyDomainEfsInformation = 2,
    PolicyDomainKerberosTicketInformation = 3 // TPolicyDomainKerberosTicketInfo
  );

  // 2386
  TPolicyDomainKerberosTicketInfo = record
    AuthenticationOptions: Cardinal; // POLICY_KERBEROS_*
    MaxServiceTicketAge: TLargeInteger;
    MaxTicketAge: TLargeInteger;
    MaxRenewAge: TLargeInteger;
    MaxClockSkew: TLargeInteger;
    Reserved: TLargeInteger;
  end;
  PPolicyDomainKerberosTicketInfo = ^TPolicyDomainKerberosTicketInfo;

  // 2420
  TPolicyMachineAcctInfo = record
    Rid: Cardinal;
    Sid: PSid;
  end;
  PPolicyMachineAcctInfo = ^TPolicyMachineAcctInfo;

  // 2432
  TPolicyNotificationInformationClass = (
    PolicyNotifyReserved = 0,
    PolicyNotifyAuditEventsInformation = 1,
    PolicyNotifyAccountDomainInformation = 2,
    PolicyNotifyServerRoleInformation = 3,
    PolicyNotifyDnsDomainInformation = 4,
    PolicyNotifyDomainEfsInformation = 5,
    PolicyNotifyDomainKerberosTicketInformation = 6,
    PolicyNotifyMachineAccountPasswordInformation = 7,
    PolicyNotifyGlobalSaclInformation = 8
  );

  // Winapi.LsaLookup 70
  TLsaTrustInformation = record
    Name: TLsaUnicodeString;
    Sid: PSid;
  end;
  PLsaTrustInformation = ^TLsaTrustInformation;

  TLsaTrustInformationArray = array [ANYSIZE_ARRAY] of TLsaTrustInformation;
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

  TLsaTranslatedSid2Array = array [ANYSIZE_ARRAY] of TLsaTranslatedSid2;
  PLsaTranslatedSid2Array = ^TLsaTranslatedSid2Array;

  // Winapi.LsaLookup 142
  TLsaTranslatedName = record
    Use: TSidNameUse;
    Name: TLsaUnicodeString;
    DomainIndex: Integer;
  end;
  PLsaTranslatedName = ^TLsaTranslatedName;

  TLsaTranslatedNameArray = array [ANYSIZE_ARRAY] of TLsaTranslatedName;
  PLsaTranslatedNameArray = ^TLsaTranslatedNameArray;

// 2983
function LsaFreeMemory(Buffer: Pointer): NTSTATUS; stdcall; external advapi32;

// 2989
function LsaClose(ObjectHandle: TLsaHandle): NTSTATUS; stdcall;
  external advapi32;

// 2997
function LsaDelete(ObjectHandle: TLsaHandle): NTSTATUS; stdcall;
  external advapi32;

// 3003
function LsaQuerySecurityObject(ObjectHandle: TLsaHandle;
  SecurityInformation: TSecurityInformation; out SecurityDescriptor:
  PSecurityDescriptor): NTSTATUS; stdcall; external advapi32;

// 3031
function LsaSetSecurityObject(ObjectHandle: TLsaHandle;
  SecurityInformation: TSecurityInformation; const SecurityDescriptor:
  TSecurityDescriptor): NTSTATUS; stdcall; external advapi32;

// 3108
function LsaOpenPolicy(SystemName: PLsaUnicodeString;
  const ObjectAttributes: TObjectAttributes; DesiredAccess: TAccessMask;
  out PolicyHandle: TLsaHandle): NTSTATUS; stdcall; external advapi32;

// 3237
function LsaQueryInformationPolicy(PolicyHandle: TLsaHandle;
  InformationClass: TPolicyInformationClass; out Buffer: Pointer): NTSTATUS;
  stdcall; external advapi32;

// 3281
function LsaSetInformationPolicy(PolicyHandle: TLsaHandle;
  InformationClass: TPolicyInformationClass; Buffer: Pointer): NTSTATUS;
  stdcall; external advapi32;

// 3289
function LsaQueryDomainInformationPolicy(PolicyHandle: TLsaHandle;
  InformationClass: TPolicyDomainInformationClass; out Buffer: Pointer):
  NTSTATUS; stdcall; external advapi32;

// 3297
function LsaSetDomainInformationPolicy(PolicyHandle: TLsaHandle;
  InformationClass: TPolicyDomainInformationClass; Buffer: Pointer): NTSTATUS;
  stdcall; external advapi32;

// 3306
function LsaRegisterPolicyChangeNotification(InformationClass:
  TPolicyNotificationInformationClass; NotificationEventHandle: THandle):
  NTSTATUS; stdcall; external secur32;

// 3313
function LsaUnregisterPolicyChangeNotification(InformationClass:
  TPolicyNotificationInformationClass; NotificationEventHandle: THandle):
  NTSTATUS; stdcall; external secur32;

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

// 3394
function LsaLookupNames2(PolicyHandle: TLsaHandle; Flags: Cardinal;
  Count: Integer; const Name: TLsaUnicodeString;
  out ReferencedDomain: PLsaReferencedDomainList;
  out Sid: PLsaTranslatedSid2): NTSTATUS; stdcall;
  external advapi32; overload;

function LsaLookupNames2(PolicyHandle: TLsaHandle; Flags: Cardinal;
  Count: Integer; Names: TArray<TLsaUnicodeString>;
  out ReferencedDomains: PLsaReferencedDomainList;
  out Sids: PLsaTranslatedSid2Array): NTSTATUS; stdcall;
  external advapi32; overload;

// 3406
function LsaLookupSids(PolicyHandle: TLsaHandle; Count: Cardinal;
  Sids: TArray<PSid>; out ReferencedDomains: PLsaReferencedDomainList;
  out Names: PLsaTranslatedNameArray): NTSTATUS; stdcall; external advapi32;

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

implementation

end.
