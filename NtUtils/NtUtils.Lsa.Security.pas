unit NtUtils.Lsa.Security;

interface

uses
  Winapi.WinNt, Winapi.ntlsa, NtUtils.Exceptions, NtUtils.Security.Acl,
  NtUtils.Security.Sid;

type
  TAce = NtUtils.Security.Acl.TAce;
  IAcl = NtUtils.Security.Acl.IAcl;
  TAcl = NtUtils.Security.Acl.TAcl;
  ISid = NtUtils.Security.Sid.ISid;

{ Query security }

// Security descriptor (free with LsaFreeMemory)
function LsaxQuerySecurityObject(LsaHandle: TLsaHandle; SecurityInformation:
  TSecurityInformation; out SecDesc: PSecurityDescriptor): TNtxStatus;

// Owner
function LsaxQueryOwnerObject(LsaHandle: TLsaHandle; out Owner: ISid):
  TNtxStatus;

// Primary group
function LsaxQueryPrimaryGroupObject(LsaHandle: TLsaHandle;
  out PrimaryGroup: ISid): TNtxStatus;

// DACL
function LsaxQueryDaclObject(LsaHandle: TLsaHandle; out Dacl: IAcl): TNtxStatus;

// SACL
function LsaxQuerySaclObject(LsaHandle: TLsaHandle; out Sacl: IAcl): TNtxStatus;

// Mandatory label
function LsaxQueryLabelObject(LsaHandle: TLsaHandle; out Sacl: IAcl):
  TNtxStatus;

{ Set security }

// Security descriptor
function LsaxSetSecurityObject(LsaHandle: TLsaHandle; SecInfo:
  TSecurityInformation; const SecDesc: TSecurityDescriptor): TNtxStatus;

// Owner
function LsaxSetOwnerObject(LsaHandle: TLsaHandle; Owner: ISid): TNtxStatus;

// Primary group
function LsaxSetPrimaryGroupObject(LsaHandle: TLsaHandle; Group: ISid):
  TNtxStatus;

// DACL
function LsaxSetDaclObject(LsaHandle: TLsaHandle; Dacl: IAcl): TNtxStatus;

// SACL
function LsaxSetSaclObject(LsaHandle: TLsaHandle; Sacl: IAcl): TNtxStatus;

// Mandatory label
function LsaxSetLabelObject(LsaHandle: TLsaHandle; Sacl: IAcl): TNtxStatus;

implementation

uses
  Ntapi.ntrtl;

{ Query security }

function LsaxQuerySecurityObject(LsaHandle: TLsaHandle; SecurityInformation:
  TSecurityInformation; out SecDesc: PSecurityDescriptor): TNtxStatus;
begin
  Result.Location := 'LsaQuerySecurityObject';
  Result.Status := LsaQuerySecurityObject(LsaHandle, SecurityInformation,
    SecDesc);
end;

// Owner
function LsaxQueryOwnerObject(LsaHandle: TLsaHandle; out Owner: ISid):
  TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, OWNER_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetOwnerSD(pSD, Owner);
  LsaFreeMemory(pSD);
end;

// Primary group
function LsaxQueryPrimaryGroupObject(LsaHandle: TLsaHandle;
  out PrimaryGroup: ISid): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, GROUP_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetPrimaryGroupSD(pSD, PrimaryGroup);
  LsaFreeMemory(pSD);
end;

// DACL
function LsaxQueryDaclObject(LsaHandle: TLsaHandle; out Dacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, DACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetDaclSD(pSD, Dacl);
  LsaFreeMemory(pSD);
end;

// SACL
function LsaxQuerySaclObject(LsaHandle: TLsaHandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, SACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetSaclSD(pSD, Sacl);
  LsaFreeMemory(pSD);
end;

// Mandatory label
function LsaxQueryLabelObject(LsaHandle: TLsaHandle; out Sacl: IAcl):
  TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, LABEL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetSaclSD(pSD, Sacl);
  LsaFreeMemory(pSD);
end;

{ Set security }

function LsaxSetSecurityObject(LsaHandle: TLsaHandle; SecInfo:
  TSecurityInformation; const SecDesc: TSecurityDescriptor): TNtxStatus;
begin
  Result.Location := 'LsaSetSecurityObject';
  Result.Status := LsaSetSecurityObject(LsaHandle, SecInfo, SecDesc);
end;

// Owner
function LsaxSetOwnerObject(LsaHandle: TLsaHandle; Owner: ISid): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareOwnerSD(SD, Owner);

  if Result.IsSuccess then
    Result := LsaxSetSecurityObject(LsaHandle, OWNER_SECURITY_INFORMATION, SD);
end;

// Primary group
function LsaxSetPrimaryGroupObject(LsaHandle: TLsaHandle; Group: ISid):
  TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPreparePrimaryGroupSD(SD, Group);

  if Result.IsSuccess then
    Result := LsaxSetSecurityObject(LsaHandle, GROUP_SECURITY_INFORMATION, SD);
end;

// DACL
function LsaxSetDaclObject(LsaHandle: TLsaHandle; Dacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareDaclSD(SD, Dacl);

  if Result.IsSuccess then
    Result := LsaxSetSecurityObject(LsaHandle, DACL_SECURITY_INFORMATION, SD);
end;

// SACL
function LsaxSetSaclObject(LsaHandle: TLsaHandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareSaclSD(SD, Sacl);

  if Result.IsSuccess then
    Result := LsaxSetSecurityObject(LsaHandle, SACL_SECURITY_INFORMATION, SD);
end;

// Mandatory label
function LsaxSetLabelObject(LsaHandle: TLsaHandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareSaclSD(SD, Sacl);

  if Result.IsSuccess then
    Result := LsaxSetSecurityObject(LsaHandle, LABEL_SECURITY_INFORMATION, SD);
end;

end.
