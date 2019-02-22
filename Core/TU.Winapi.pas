unit TU.Winapi;

interface

uses
  Winapi.WinNt, Ntapi.ntseapi;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

type
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
    WinBuiltinEventLogReadersGroup,
    WinNewEnterpriseReadonlyControllersSid,
    WinBuiltinCertSvcDComAccessGroup,
    WinMediumPlusLabelSid,
    WinLocalLogonSid,
    WinConsoleLogonSid,
    WinThisOrganizationCertificateSid,
    WinApplicationPackageAuthoritySid,
    WinBuiltinAnyPackageSid,
    WinCapabilityInternetClientSid,
    WinCapabilityInternetClientServerSid,
    WinCapabilityPrivateNetworkClientServerSid,
    WinCapabilityPicturesLibrarySid,
    WinCapabilityVideosLibrarySid,
    WinCapabilityMusicLibrarySid,
    WinCapabilityDocumentsLibrarySid,
    WinCapabilitySharedUserCertificatesSid,
    WinCapabilityEnterpriseAuthenticationSid,
    WinCapabilityRemovableStorageSid,
    WinBuiltinRDSRemoteAccessServersSid,
    WinBuiltinRDSEndpointServersSid,
    WinBuiltinRDSManagementServersSid,
    WinUserModeDriversSid,
    WinBuiltinHyperVAdminsSid,
    WinAccountCloneableControllersSid,
    WinBuiltinAccessControlAssistanceOperatorsSid,
    WinBuiltinRemoteManagementUsersSid,
    WinAuthenticationAuthorityAssertedSid,
    WinAuthenticationServiceAssertedSid,
    WinLocalAccountSid,
    WinLocalAccountAndAdministratorSid,
    WinAccountProtectedUsersSid,
    WinCapabilityAppointmentsSid,
    WinCapabilityContactsSid,
    WinAccountDefaultSystemManagedSid,
    WinBuiltinDefaultSystemManagedGroupSid,
    WinBuiltinStorageReplicaAdminsSid,
    WinAccountKeyAdminsSid,
    WinAccountEnterpriseKeyAdminsSid,
    WinAuthenticationKeyTrustSid,
    WinAuthenticationKeyPropertyMFASid,
    WinAuthenticationKeyPropertyAttestationSid,
    WinAuthenticationFreshKeyAuthSid,
    WinBuiltinDeviceOwnersSid
  );

  TSIDAndAttributesArray = array of TSIDAndAttributes;

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
    MandatoryPolicy: TTokenMandatoryPolicy;
    Flags: Cardinal;
    AppContainerNumber: Cardinal;
    PackageSid: PSid;
    CapabilitiesHash: PSIDAndAttributesHash;
    TrustLevelSid: PSid;
    SecurityAttributes: Pointer;
  end;
  PTokenAccessInformation = ^TTokenAccessInformation;

function GetTokenInformation(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal; var ReturnLength: Cardinal): LongBool;
  stdcall; external advapi32;

function SetTokenInformation(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal): LongBool; stdcall; external advapi32;

function CreateWellKnownSid(WellKnownSidType: TWellKnownSidType;
  DomainSid: PSid; PSid: PSid; var cbSid: Cardinal): LongBool;
  stdcall; external advapi32;

function GetCurrentSession: Cardinal;

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

uses
  Ntapi.ntrtl, Ntapi.ntpebteb;

function GetCurrentSession: Cardinal;
begin
  Result := RtlGetCurrentPeb.SessionId;
end;

end.
