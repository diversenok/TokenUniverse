unit TU.Winapi;

interface

uses
  Winapi.Windows;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

const
  SE_GROUP_MANDATORY = $00000001;
  SE_GROUP_ENABLED_BY_DEFAULT = $00000002;
  SE_GROUP_ENABLED = $00000004;
  SE_GROUP_OWNER = $00000008;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
  SE_GROUP_INTEGRITY = $00000020;
  SE_GROUP_INTEGRITY_ENABLED = $00000040;
  SE_GROUP_RESOURCE = $20000000;
  SE_GROUP_LOGON_ID = $C0000000;

  TOKEN_MANDATORY_POLICY_OFF = $0;
  TOKEN_MANDATORY_POLICY_NO_WRITE_UP = $1;
  TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN = $2;

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

function LocalFree(hMem: Pointer): Pointer; stdcall; external kernel32;

function OpenThread(dwDesiredAccess: Cardinal; bInheritHandle: LongBool;
  dwThreadId: Cardinal): THandle; stdcall; external kernel32;

function RtlGetCurrentPeb: Pointer; stdcall; external 'ntdll.dll';
function GetCurrentSession: Cardinal; inline;

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

function GetCurrentSession: Cardinal;
begin
  // Current session ID is always stored in the PEB
  {$POINTERMATH ON}
  try
    // We use hardcoded offsets of SessionID field from PPEB structure
    {$IFDEF WIN64}
      Result := PCardinal(PByte(RtlGetCurrentPeb) + $2C0)^;
    {$ELSE}
      Result := PCardinal(PByte(RtlGetCurrentPeb) + $1D4)^;
    {$ENDIF}
  except
    Result := 0;
    OutputDebugStringW('Exception while reading PEB');
  end;
  {$POINTERMATH OFF}
end;

end.
