unit Winapi.NtSecApi;

interface

uses
  Winapi.WinNt;

const
  // 1857
  PER_USER_POLICY_UNCHANGED = $00;
  PER_USER_AUDIT_SUCCESS_INCLUDE = $01;
  PER_USER_AUDIT_SUCCESS_EXCLUDE = $02;
  PER_USER_AUDIT_FAILURE_INCLUDE = $04;
  PER_USER_AUDIT_FAILURE_EXCLUDE = $08;
  PER_USER_AUDIT_NONE = $10;

type
  TGuidArray = array [Byte] of TGUID;
  PGuidArray = ^TGuidArray;
  TGuidDynArray = array of TGUID;

  TSidArray = array [Word] of PSid;
  PSidArray = ^TSidArray;

  // 4483
  TPolicyAuditSidArray = record
    UsersCount: Cardinal;
    UserSidArray: PSidArray;
  end;
  PPolicyAuditSidArray = ^TPolicyAuditSidArray;

  // 4494
  TAuditPolicyInformation = record
    AuditSubCategoryGuid: TGuid;
    AuditingInformation: Cardinal;
    AuditCategoryGuid: TGuid;
  end;
  PAuditPolicyInformation = ^TAuditPolicyInformation;

  TPAuditPolicyInformationArray = array [Byte] of PAuditPolicyInformation;
  PPAuditPolicyInformationArray = ^TPAuditPolicyInformationArray;

// 4553
function AuditQuerySystemPolicy(pSubCategoryGuids: TGuidDynArray;
  dwPolicyCount: Cardinal; ppAuditPolicy: PPAuditPolicyInformationArray):
  Boolean; stdcall; external advapi32;

// 4563
function AuditQueryPerUserPolicy(pSid: PSid; SubCategoryGuids: TGuidDynArray;
  dwPolicyCount: Cardinal; ppAuditPolicy: PPAuditPolicyInformationArray):
  Boolean; stdcall; external advapi32;

// 4571
function AuditEnumeratePerUserPolicy(out pAuditSidArray: PPolicyAuditSidArray):
  Boolean; stdcall; external advapi32;

// 4600
function AuditEnumerateCategories(out pAuditCategoriesArray: PGuidArray;
  out dwCountReturned: Cardinal): Boolean; stdcall; external advapi32;

// 4612
function AuditEnumerateSubCategories(const AuditCategoryGuid: TGuid;
  bRetrieveAllSubCategories: Boolean; out pAuditSubCategoriesArray: PGuidArray;
  out dwCountReturned: Cardinal): Boolean; stdcall; external advapi32;

// 4623
function AuditLookupCategoryNameW(const AuditCategoryGuid: TGuid;
  out pszCategoryName: PWideChar): Boolean; stdcall; external advapi32;

// 4645
function AuditLookupSubCategoryNameW(const AuditSubCategoryGuid: TGuid;
  out pszSubCategoryName: PWideChar): Boolean; stdcall; external advapi32;

// 4737
procedure AuditFree(Buffer: Pointer); stdcall; external advapi32;

implementation

end.
