unit NtUtils.Sam.Security;

interface

uses
  Winapi.WinNt, Ntapi.ntsam, NtUtils.Exceptions, NtUtils.Security.Acl,
  NtUtils.Security.Sid;

type
  TAce = NtUtils.Security.Acl.TAce;
  IAcl = NtUtils.Security.Acl.IAcl;
  TAcl = NtUtils.Security.Acl.TAcl;
  ISid = NtUtils.Security.Sid.ISid;

{ Query security }

// Security descriptor (free with SamFreeMemory)
function SamxQuerySecurityObject(SamHandle: TSamHandle; SecurityInformation:
  TSecurityInformation; out SecDesc: PSecurityDescriptor): TNtxStatus;

// Owner
function SamxQueryOwnerObject(SamHandle: TSamHandle; out Owner: ISid):
  TNtxStatus;

// Primary group
function SamxQueryPrimaryGroupObject(SamHandle: TSamHandle;
  out PrimaryGroup: ISid): TNtxStatus;

// DACL
function SamxQueryDaclObject(SamHandle: TSamHandle; out Dacl: IAcl): TNtxStatus;

// SACL
function SamxQuerySaclObject(SamHandle: TSamHandle; out Sacl: IAcl): TNtxStatus;

// Mandatory label
function SamxQueryLabelObject(SamHandle: TSamHandle; out Sacl: IAcl):
  TNtxStatus;

{ Set security }

// Security descriptor
function SamxSetSecurityObject(SamHandle: TSamHandle; SecInfo:
  TSecurityInformation; const SecDesc: TSecurityDescriptor): TNtxStatus;

// Owner
function SamxSetOwnerObject(SamHandle: TSamHandle; Owner: ISid): TNtxStatus;

// Primary group
function SamxSetPrimaryGroupObject(SamHandle: TSamHandle; Group: ISid):
  TNtxStatus;

// DACL
function SamxSetDaclObject(SamHandle: TSamHandle; Dacl: IAcl): TNtxStatus;

// SACL
function SamxSetSaclObject(SamHandle: TSamHandle; Sacl: IAcl): TNtxStatus;

// Mandatory label
function SamxSetLabelObject(SamHandle: TSamHandle; Sacl: IAcl): TNtxStatus;

implementation

{ Query security }

// Security descriptor (free with SamFreeMemory)
function SamxQuerySecurityObject(SamHandle: TSamHandle; SecurityInformation:
  TSecurityInformation; out SecDesc: PSecurityDescriptor): TNtxStatus;
begin
  Result.Location := 'SamQuerySecurityObject';
  Result.LastCall.Expects(RtlxComputeReadAccess(SecurityInformation), objNone);

  Result.Status := SamQuerySecurityObject(SamHandle, SecurityInformation,
    SecDesc);
end;

// Owner
function SamxQueryOwnerObject(SamHandle: TSamHandle; out Owner: ISid):
  TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := SamxQuerySecurityObject(SamHandle, OWNER_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetOwnerSD(pSD, Owner);
  SamFreeMemory(pSD);
end;

// Primary group
function SamxQueryPrimaryGroupObject(SamHandle: TSamHandle;
  out PrimaryGroup: ISid): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := SamxQuerySecurityObject(SamHandle, GROUP_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetPrimaryGroupSD(pSD, PrimaryGroup);
  SamFreeMemory(pSD);
end;

// DACL
function SamxQueryDaclObject(SamHandle: TSamHandle; out Dacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := SamxQuerySecurityObject(SamHandle, DACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetDaclSD(pSD, Dacl);
  SamFreeMemory(pSD);
end;

// SACL
function SamxQuerySaclObject(SamHandle: TSamHandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := SamxQuerySecurityObject(SamHandle, SACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetSaclSD(pSD, Sacl);
  SamFreeMemory(pSD);
end;

// Mandatory label
function SamxQueryLabelObject(SamHandle: TSamHandle; out Sacl: IAcl):
  TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := SamxQuerySecurityObject(SamHandle, LABEL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetSaclSD(pSD, Sacl);
  SamFreeMemory(pSD);
end;

{ Set security }

// Security descriptor
function SamxSetSecurityObject(SamHandle: TSamHandle; SecInfo:
  TSecurityInformation; const SecDesc: TSecurityDescriptor): TNtxStatus;
begin
  Result.Location := 'SamSetSecurityObject';
  Result.LastCall.Expects(RtlxComputeWriteAccess(SecInfo), objNone);

  Result.Status := SamSetSecurityObject(SamHandle, SecInfo, SecDesc);
end;

// Owner
function SamxSetOwnerObject(SamHandle: TSamHandle; Owner: ISid): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareOwnerSD(SD, Owner);

  if Result.IsSuccess then
    Result := SamxSetSecurityObject(SamHandle, OWNER_SECURITY_INFORMATION, SD);
end;

// Primary group
function SamxSetPrimaryGroupObject(SamHandle: TSamHandle; Group: ISid):
  TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPreparePrimaryGroupSD(SD, Group);

  if Result.IsSuccess then
    Result := SamxSetSecurityObject(SamHandle, GROUP_SECURITY_INFORMATION, SD);
end;

// DACL
function SamxSetDaclObject(SamHandle: TSamHandle; Dacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareDaclSD(SD, Dacl);

  if Result.IsSuccess then
    Result := SamxSetSecurityObject(SamHandle, DACL_SECURITY_INFORMATION, SD);
end;

// SACL
function SamxSetSaclObject(SamHandle: TSamHandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareSaclSD(SD, Sacl);

  if Result.IsSuccess then
    Result := SamxSetSecurityObject(SamHandle, SACL_SECURITY_INFORMATION, SD);
end;

// Mandatory label
function SamxSetLabelObject(SamHandle: TSamHandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareSaclSD(SD, Sacl);

  if Result.IsSuccess then
    Result := SamxSetSecurityObject(SamHandle, LABEL_SECURITY_INFORMATION, SD);
end;

end.
