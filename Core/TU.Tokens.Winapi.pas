unit TU.Tokens.Winapi;

interface

uses
  Winapi.Windows;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

const
  SE_GROUP_INTEGRITY = $20;

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

function SetTokenInformation(TokenHandle: THandle;
  TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
  TokenInformationLength: Cardinal): LongBool; stdcall; external advapi32;

function GetterMessage(InfoClass: TTokenInformationClass): String;
function SetterMessage(InfoClass: TTokenInformationClass): String;

implementation

uses
  System.TypInfo;

function GetterMessage(InfoClass: TTokenInformationClass): String;
begin
  Result := 'GetTokenInformation:' +
    GetEnumName(TypeInfo(TTokenInformationClass), Integer(InfoClass))
end;

function SetterMessage(InfoClass: TTokenInformationClass): String;
begin
  Result := 'SetTokenInformation:' +
    GetEnumName(TypeInfo(TTokenInformationClass), Integer(InfoClass))
end;

end.
