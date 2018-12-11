unit TU.Winapi;

interface

uses
  Winapi.Windows;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

const
  SE_GROUP_INTEGRITY = $20;
  ERROR_CANT_ENABLE_DENY_ONLY = $275;

type
  TTokenInformationClass = (
    TokenTPad, // The compiler wouldn't generate TypeInfo without it
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

  TSIDNameUse = (
    SidTypeZero,
    SidTypeUser,
    SidTypeGroup,
    SidTypeDomain,
    SidTypeAlias,
    SidTypeWellKnownGroup,
    SidTypeDeletedAccount,
    SidTypeInvalid,
    SidTypeUnknown,
    SidTypeComputer,
    SidTypeLabel
  );

  TWellKnownSidType = (
    WinNullSid,
    WinWorldSid,
    WinLocalSid,
    WinCreatorOwnerSid,
    WinCreatorGroupSid,
    WinCreatorOwnerServerSid,
    WinCreatorGroupServerSid,
    WinNtAuthoritySid,
    WinDialupSid,
    WinNetworkSid,
    WinBatchSid,
    WinInteractiveSid,
    WinServiceSid,
    WinAnonymousSid,
    WinProxySid,
    WinEnterpriseControllersSid,
    WinSelfSid,
    WinAuthenticatedUserSid,
    WinRestrictedCodeSid,
    WinTerminalServerSid,
    WinRemoteLogonIdSid,
    WinLogonIdsSid,
    WinLocalSystemSid,
    WinLocalServiceSid,
    WinNetworkServiceSid,
    WinBuiltinDomainSid,
    WinBuiltinAdministratorsSid,
    WinBuiltinUsersSid,
    WinBuiltinGuestsSid,
    WinBuiltinPowerUsersSid,
    WinBuiltinAccountOperatorsSid,
    WinBuiltinSystemOperatorsSid,
    WinBuiltinPrintOperatorsSid,
    WinBuiltinBackupOperatorsSid,
    WinBuiltinReplicatorSid,
    WinBuiltinPreWindows2000CompatibleAccessSid,
    WinBuiltinRemoteDesktopUsersSid,
    WinBuiltinNetworkConfigurationOperatorsSid,
    WinAccountAdministratorSid,
    WinAccountGuestSid,
    WinAccountKrbtgtSid,
    WinAccountDomainAdminsSid,
    WinAccountDomainUsersSid,
    WinAccountDomainGuestsSid,
    WinAccountComputersSid,
    WinAccountControllersSid,
    WinAccountCertAdminsSid,
    WinAccountSchemaAdminsSid,
    WinAccountEnterpriseAdminsSid,
    WinAccountPolicyAdminsSid,
    WinAccountRasAndIasServersSid,
    WinNTLMAuthenticationSid,
    WinDigestAuthenticationSid,
    WinSChannelAuthenticationSid,
    WinThisOrganizationSid,
    WinOtherOrganizationSid,
    WinBuiltinIncomingForestTrustBuildersSid,
    WinBuiltinPerfMonitoringUsersSid,
    WinBuiltinPerfLoggingUsersSid,
    WinBuiltinAuthorizationAccessSid,
    WinBuiltinTerminalServerLicenseServersSid,
    WinBuiltinDCOMUsersSid,
    WinBuiltinIUsersSid,
    WinIUserSid,
    WinBuiltinCryptoOperatorsSid,
    WinUntrustedLabelSid,
    WinLowLabelSid,
    WinMediumLabelSid,
    WinHighLabelSid,
    WinSystemLabelSid,
    WinWriteRestrictedCodeSid,
    WinCreatorOwnerRightsSid,
    WinCacheablePrincipalsGroupSid,
    WinNonCacheablePrincipalsGroupSid,
    WinEnterpriseReadonlyControllersSid,
    WinAccountReadonlyControllersSid,
    WinBuiltinEventLogReadersGroup
  );

  TSIDAndAttributesArray = array of TSIDAndAttributes;

  TTokenGroups = record
    GroupCount: Integer;
    Groups: array[Word] of TSIDAndAttributes;
  end;
  PTokenGroups = ^TTokenGroups;

  TTokenPrivileges = record
    PrivilegeCount: Integer;
    Privileges: array[Byte] of TLUIDAndAttributes;
  end;
  PTokenPrivileges = ^TTokenPrivileges;

  TSIDAndAttributesHash = record
    const SID_HASH_SIZE = 32;
  var
    SidCount: Cardinal;
    SidAttr: PSIDAndAttributes;
    Hash: array [0 .. SID_HASH_SIZE - 1] of NativeUInt;
  end;
  PSIDAndAttributesHash = ^TSIDAndAttributesHash;

  TTokenAccessInformation = record
    SidHash: PSIDAndAttributesHash;
    RestrictedSidHash: PSIDAndAttributesHash;
    Privileges: PTokenPrivileges;
    AuthenticationId: Int64;
    TokenType: TTokenType;
    ImpersonationLevel: TSecurityImpersonationLevel;
    MandatoryPolicy: TOKEN_MANDATORY_POLICY;
    Flags: DWORD;
    AppContainerNumber: DWORD;
    PackageSid: PSID;
    CapabilitiesHash: PSIDAndAttributesHash;
    TrustLevelSid: PSID;
    SecurityAttributes: Pointer;
  end;
  PTokenAccessInformation = ^TTokenAccessInformation;

function GetTokenInformation(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal; var ReturnLength: Cardinal): LongBool;
  stdcall; external advapi32;

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges:
  LongBool; NewState: PTokenPrivileges; BufferLength: Cardinal;
  PreviousState: PTokenPrivileges; ReturnLength: PCardinal): LongBool;
  stdcall; external advapi32;

function AdjustTokenGroups(TokenHandle: THandle; ResetToDefault: LongBool;
  NewState: PTokenGroups; BufferLength: Cardinal;
  PreviousState: PTokenGroups; ReturnLength: PCardinal): LongBool;
  stdcall; external advapi32;

function LogonUserExExW(lpszUsername: PWideChar; lpszDomain: PWideChar;
  lpszPassword: PWideChar; dwLogonType: Cardinal; dwLogonProvider: Cardinal;
  pTokenGroups: PTokenGroups; out hToken: THandle; ppLogonSid: PPointer;
  pProfileBuffer: PPointer; pdwProfileLength: PCardinal;
  QuotaLimits: Pointer): LongBool; stdcall; external advapi32;

function SetTokenInformation(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal): LongBool; stdcall; external advapi32;

function CreateRestrictedToken(ExistingTokenHandle: THandle; Flags: Cardinal;
  DisableSidCount: Cardinal; SidsToDisable: TSIDAndAttributesArray;
  DeletePrivilegeCount: Cardinal; PrivilegesToDelete: PLUIDAndAttributes;
  RestrictedSidCount: Cardinal; SidsToRestrict: TSIDAndAttributesArray;
  out NewTokenHandle: THandle): LongBool; stdcall; external advapi32;

function CreateWellKnownSid(WellKnownSidType: TWellKnownSidType;
  DomainSid: PSID; pSid: PSID; var cbSid: Cardinal): LongBool;
  stdcall; external advapi32;

function AllocateLocallyUniqueId(var Luid: LUID): LongBool; stdcall;
  external advapi32;

function OpenThread(dwDesiredAccess: Cardinal; bInheritHandle: LongBool;
  dwThreadId: Cardinal): THandle; stdcall; external kernel32;

type
  TAccessGroup = (agRead, agWrite, agExecute, agStandard);

const
  ACCESS_COUNT = 13;
  AccessValues: array [0 .. ACCESS_COUNT - 1] of Cardinal = (
    TOKEN_ASSIGN_PRIMARY, TOKEN_DUPLICATE, TOKEN_IMPERSONATE, TOKEN_QUERY,
    TOKEN_QUERY_SOURCE, TOKEN_ADJUST_DEFAULT, TOKEN_ADJUST_PRIVILEGES,
     TOKEN_ADJUST_GROUPS, TOKEN_ADJUST_SESSIONID, _DELETE, READ_CONTROL,
    WRITE_DAC, WRITE_OWNER);
  AccessStrings: array [0 .. ACCESS_COUNT - 1] of String = ('Assign primary',
    'Duplicate', 'Impersonate', 'Query', 'Query source', 'Adjust default',
    'Adjust privileges', 'Adjust groups', 'Adjust session', 'Delete',
    'Read control', 'Write DAC', 'Write owner');

  AccessGroupValues: array [0 .. ACCESS_COUNT - 1] of TAccessGroup = (
    agExecute, agRead, agExecute, agRead, agRead, agWrite, agWrite, agWrite,
    agWrite, agStandard, agStandard, agStandard, agStandard);
  AccessGroupStrings: array [TAccessGroup] of String = ('Generic Read',
    'Generic Write', 'Generic Execute', 'Standard');

implementation

end.
