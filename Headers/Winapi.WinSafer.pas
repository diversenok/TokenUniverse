unit Winapi.WinSafer;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt;

const
  // 57
  SAFER_LEVEL_OPEN = 1;

  // 72
  SAFER_TOKEN_NULL_IF_EQUAL = $00000001;
  SAFER_TOKEN_COMPARE_ONLY = $00000002;
  SAFER_TOKEN_MAKE_INERT = $00000004;
  SAFER_TOKEN_WANT_FLAGS = $00000008;

type
  TSaferLevelHandle = NativeUInt;

  TSaferScopeId = (
    SAFER_SCOPEID_MACHINE = 1,
    SAFER_SCOPEID_USER = 2
  );

  TSaferLevelId = (
    SAFER_LEVELID_FULLYTRUSTED = $40000,
    SAFER_LEVELID_NORMALUSER = $20000,
    SAFER_LEVELID_CONSTRAINED = $10000,
    SAFER_LEVELID_UNTRUSTED = $01000,
    SAFER_LEVELID_DISALLOWED = $00000
  );

  TSaferObjectInfoClass = (
    SaferObjectLevelId = 1,      // get: Cardinal
    SaferObjectScopeId = 2,      // get: Cardinal
    SaferObjectFriendlyName = 3, // get/set: PWideChar
    SaferObjectDescription = 4,  // get/set: PWideChar
    SaferObjectBuiltin = 5,      // get: LongBool

    SaferObjectDisallowed = 6,              // get: LongBool
    SaferObjectDisableMaxPrivilege = 7,     // get: LongBool
    SaferObjectInvertDeletedPrivileges = 8, // get: LongBool
    SaferObjectDeletedPrivileges = 9,       // get: TTokenPrivileges
    SaferObjectDefaultOwner = 10,           // get: TTokenOwner
    SaferObjectSidsToDisable = 11,          // get: TTokenGroups
    SaferObjectRestrictedSidsInverted = 12, // get: TTokenGroups
    SaferObjectRestrictedSidsAdded = 13,    // get: TTokenGroups

    SaferObjectAllIdentificationGuids = 14, // get: SAFER_IDENTIFICATION_GUIDS
    SaferObjectSingleIdentification = 15,   // get/set: SAFER_IDENTIFICATION_*

    SaferObjectExtendedError = 16           // get: Cardinal dwError
  );

function SaferCreateLevel(dwScopeId: TSaferScopeId; dwLevelId: TSaferLevelId;
  OpenFlags: Cardinal; out LevelHandle: TSaferLevelHandle;
  lpReserved: Pointer = nil): LongBool; stdcall; external advapi32;

function SaferCloseLevel(hLevelHandle: TSaferLevelHandle): LongBool; stdcall;
  external advapi32;

function SaferComputeTokenFromLevel(LevelHandle: TSaferLevelHandle;
  InAccessToken: THandle; out OutAccessToken: THandle;
  dwFlags: Cardinal; lpReserved: PCardinal): LongBool; stdcall;
  external advapi32;

function SaferGetLevelInformation(LevelHandle: TSaferLevelHandle;
  dwInfoType: TSaferObjectInfoClass; lpQueryBuffer: Pointer;
  dwInBufferSize: Cardinal; out lpdwOutBufferSize: Cardinal): LongBool; stdcall;
  external advapi32;

function SaferSetLevelInformation(LevelHandle: TSaferLevelHandle;
  dwInfoType: TSaferObjectInfoClass; lpQueryBuffer: Pointer;
  dwInBufferSize: Cardinal): LongBool; stdcall; external advapi32;

implementation

end.
