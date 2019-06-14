unit NtUtils.Lsa.Security;

interface

uses
  Winapi.WinNt, Winapi.ntlsa, NtUtils.Exceptions, NtUtils.Security.Acl,
  NtUtils.Security.Sid;

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
function LsaxQuerySaclObject(LsaHandle: TLsaHandle; out SACL: IAcl): TNtxStatus;

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
  Defaulted: Boolean;
  OwnerSid: PSid;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, OWNER_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetOwnerSecurityDescriptor';
  Result.Status := RtlGetOwnerSecurityDescriptor(pSD, OwnerSid,  Defaulted);

  if Result.IsSuccess then
    Owner := TSid.CreateCopy(OwnerSid);

  LsaFreeMemory(pSD);
end;

// Primary group
function LsaxQueryPrimaryGroupObject(LsaHandle: TLsaHandle;
  out PrimaryGroup: ISid): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  Defaulted: Boolean;
  GroupSid: PSid;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, GROUP_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetGroupSecurityDescriptor';
  Result.Status := RtlGetGroupSecurityDescriptor(pSD, GroupSid,  Defaulted);

  if Result.IsSuccess then
    PrimaryGroup := TSid.CreateCopy(GroupSid);

  LsaFreeMemory(pSD);
end;

// DACL
function LsaxQueryDaclObject(LsaHandle: TLsaHandle; out Dacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  DaclPresent, Defaulted: Boolean;
  pDacl: PAcl;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, DACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetDaclSecurityDescriptor';
  Result.Status := RtlGetDaclSecurityDescriptor(pSD, DaclPresent, pDacl,
    Defaulted);

  if Result.IsSuccess and DaclPresent and Assigned(pDacl) then
    Dacl := TAcl.CreateCopy(pDacl)
  else
    Dacl := nil;

  LsaFreeMemory(pSD);
end;

// SACL
function LsaxQuerySaclObject(LsaHandle: TLsaHandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  SaclPresent, Defaulted: Boolean;
  pSacl: PAcl;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, SACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetSaclSecurityDescriptor';
  Result.Status := RtlGetSaclSecurityDescriptor(pSD, SaclPresent, pSacl,
    Defaulted);

  if Result.IsSuccess and SaclPresent and Assigned(pSacl) then
    Sacl := TAcl.CreateCopy(pSacl)
  else
    Sacl := nil;

  LsaFreeMemory(pSD);
end;

// Mandatory label
function LsaxQueryLabelObject(LsaHandle: TLsaHandle; out Sacl: IAcl):
  TNtxStatus;
var
  pSD: PSecurityDescriptor;
  SaclPresent, Defaulted: Boolean;
  pSacl: PAcl;
begin
  Result := LsaxQuerySecurityObject(LsaHandle, LABEL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetSaclSecurityDescriptor';
  Result.Status := RtlGetSaclSecurityDescriptor(pSD, SaclPresent, pSacl,
    Defaulted);

  if Result.IsSuccess and SaclPresent and Assigned(pSacl) then
    Sacl := TAcl.CreateCopy(pSacl)
  else
    Sacl := nil;

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
  Result := RtlxCreateSecurityDescriptor(SD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetOwnerSecurityDescriptor';

  if Assigned(Owner) then
    Result.Status := RtlSetOwnerSecurityDescriptor(SD, Owner.Sid, False)
  else
    Result.Status := RtlSetOwnerSecurityDescriptor(SD, nil, True);

  if not Result.IsSuccess then
    Exit;

  Result := LsaxSetSecurityObject(LsaHandle, OWNER_SECURITY_INFORMATION, SD);
end;

// Primary group
function LsaxSetPrimaryGroupObject(LsaHandle: TLsaHandle; Group: ISid):
  TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxCreateSecurityDescriptor(SD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetGroupSecurityDescriptor';

  if Assigned(Group) then
    Result.Status := RtlSetGroupSecurityDescriptor(SD, Group.Sid, False)
  else
    Result.Status := RtlSetGroupSecurityDescriptor(SD, nil, True);

  if not Result.IsSuccess then
    Exit;

  Result := LsaxSetSecurityObject(LsaHandle, GROUP_SECURITY_INFORMATION, SD);
end;

// DACL
function LsaxSetDaclObject(LsaHandle: TLsaHandle; Dacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxCreateSecurityDescriptor(SD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetDaclSecurityDescriptor';

  if Assigned(Dacl) then
    Result.Status := RtlSetDaclSecurityDescriptor(SD, True, Dacl.Acl, False)
  else
    Result.Status := RtlSetDaclSecurityDescriptor(SD, True, nil, False);

  if not Result.IsSuccess then
    Exit;

  Result := LsaxSetSecurityObject(LsaHandle, DACL_SECURITY_INFORMATION, SD);
end;

// SACL
function LsaxSetSaclObject(LsaHandle: TLsaHandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxCreateSecurityDescriptor(SD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetSaclSecurityDescriptor';

  if Assigned(Sacl) then
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, Sacl.Acl, False)
  else
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, nil, False);

  if not Result.IsSuccess then
    Exit;

  Result := LsaxSetSecurityObject(LsaHandle, SACL_SECURITY_INFORMATION, SD);
end;

// Mandatory label
function LsaxSetLabelObject(LsaHandle: TLsaHandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxCreateSecurityDescriptor(SD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetSaclSecurityDescriptor';

  if Assigned(Sacl) then
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, Sacl.Acl, False)
  else
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, nil, False);

  if not Result.IsSuccess then
    Exit;

  Result := LsaxSetSecurityObject(LsaHandle, LABEL_SECURITY_INFORMATION, SD);
end;

end.
