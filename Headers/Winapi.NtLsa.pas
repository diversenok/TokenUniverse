unit Winapi.NtLsa;
{$MINENUMSIZE 4}

interface

uses
  Ntapi.ntdef, Winapi.WinNt, Winapi.WinBase;

const
  NEGOSSP_NAME_A: AnsiString = 'Negotiate';

type
  TLsaHandle = THandle;

  LSA_STRING = ANSI_STRING;
  PLSA_STRING = ^LSA_STRING;

  TLsaOperationalMode = Cardinal;

  // NtSecApi 3856
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

  // NtSecApi 3982
  KERB_S4U_LOGON = record
    MessageType: TKerbLogonSubmitType;
    Flags: Cardinal;
    ClientUpn: UNICODE_STRING;
    ClientRealm: UNICODE_STRING;
  end;
  PKERB_S4U_LOGON = ^KERB_S4U_LOGON;

// 1049
function LsaRegisterLogonProcess(const LogonProcessName: LSA_STRING;
  out LsaHandle: TLsaHandle; out SecurityMode: TLsaOperationalMode): NTSTATUS;
  stdcall; external secur32;

// 1064
function LsaLogonUser(LsaHandle: TLsaHandle; const OriginName: LSA_STRING;
  LogonType: TSecurityLogonType; AuthenticationPackage: Cardinal;
  AuthenticationInformation: Pointer; AuthenticationInformationLength: Cardinal;
  LocalGroups: PTokenGroups; const SourceContext: TTokenSource;
  out ProfileBuffer: Pointer; out ProfileBufferLength: Cardinal;
  out LogonId: TLuid; out hToken: THandle; out Quotas: TQuotaLimits;
  out SubStatus: NTSTATUS): NTSTATUS; stdcall; external secur32;

// 1087
function LsaLookupAuthenticationPackage(LsaHandle: TLsaHandle;
  const PackageName: LSA_STRING; out AuthenticationPackage: Cardinal): NTSTATUS;
  stdcall; external secur32;

// 1098
function LsaFreeReturnBuffer(Buffer: Pointer): NTSTATUS; stdcall;
  external secur32;

// 1122
function LsaDeregisterLogonProcess(LsaHandle: TLsaHandle): NTSTATUS; stdcall;
  external secur32;

// 1130
function LsaConnectUntrusted(out LsaHandle: TLsaHandle): NTSTATUS; stdcall;
  external secur32;

implementation

end.
