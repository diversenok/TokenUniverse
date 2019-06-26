unit NtUtils.Objects.Security;

interface

uses
  Winapi.WinNt, NtUtils.Exceptions, NtUtils.Security.Acl, NtUtils.Security.Sid;

type
  TAce = NtUtils.Security.Acl.TAce;
  IAcl = NtUtils.Security.Acl.IAcl;
  TAcl = NtUtils.Security.Acl.TAcl;
  ISid = NtUtils.Security.Sid.ISid;

  { Query security }

// Query security descriptor of an object. Free it with FreeMem after use.
function NtxQuerySecurityObject(hObject: THandle; SecurityInformation:
  TSecurityInformation; out SecDesc: PSecurityDescriptor): TNtxStatus;

// Query owner of an object
function NtxQueryOwnerObject(hObject: THandle; out Owner: ISid): TNtxStatus;

// Query primary group of an object
function NtxQueryPrimaryGroupObject(hObject: THandle;
  out PrimaryGroup: ISid): TNtxStatus;

// Query DACL of an object
function NtxQueryDaclObject(hObject: THandle; out Dacl: IAcl): TNtxStatus;

// Query SACL of an object
function NtxQuerySaclObject(hObject: THandle; out Sacl: IAcl): TNtxStatus;

// Query mandatory label of an object
function NtxQueryLabelObject(hObject: THandle; out Sacl: IAcl): TNtxStatus;

  { Set security }

// Set security descriptor
function NtxSetSecurityObject(hObject: THandle; SecInfo: TSecurityInformation;
  const SecDesc: TSecurityDescriptor): TNtxStatus;

// Set owner of an object
function NtxSetOwnerObject(hObject: THandle; Owner: ISid): TNtxStatus;

// Set primary group of an object
function NtxSetPrimaryGroupObject(hObject: THandle; Group: ISid): TNtxStatus;

// Set DACL of an object
function NtxSetDaclObject(hObject: THandle; Dacl: IAcl): TNtxStatus;

// Set SACL of an object
function NtxSetSaclObject(hObject: THandle; Sacl: IAcl): TNtxStatus;

// Set mandatory label of an object
function NtxSetLabelObject(hObject: THandle; Sacl: IAcl): TNtxStatus;

implementation

uses
  Ntapi.ntrtl, Ntapi.ntobapi;

{ Query security functions }

// Security sescriptor
function NtxQuerySecurityObject(hObject: THandle; SecurityInformation:
  TSecurityInformation; out SecDesc: PSecurityDescriptor): TNtxStatus;
var
  BufferSize: Cardinal;
begin
  // Make a probe call to estimate buffer size
  Result.Location := 'NtQuerySecurityObject';
  Result.Status := NtQuerySecurityObject(hObject, SecurityInformation, nil, 0,
    BufferSize);

  if not NtxTryCheckBuffer(Result.Status, BufferSize) then
    Exit;

  SecDesc := AllocMem(BufferSize);

  Result.Status := NtQuerySecurityObject(hObject, SecurityInformation, SecDesc,
   BufferSize, BufferSize);

  if not Result.IsSuccess then
  begin
    FreeMem(SecDesc);
    SecDesc := nil;
  end;
end;

// Owner
function NtxQueryOwnerObject(hObject: THandle; out Owner: ISid): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := NtxQuerySecurityObject(hObject, OWNER_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetOwnerSD(pSD, Owner);
  FreeMem(pSD);
end;

// Primary group
function NtxQueryPrimaryGroupObject(hObject: THandle;
  out PrimaryGroup: ISid): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := NtxQuerySecurityObject(hObject, GROUP_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetPrimaryGroupSD(pSD, PrimaryGroup);
  FreeMem(pSD);
end;

// DACL
function NtxQueryDaclObject(hObject: THandle; out Dacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := NtxQuerySecurityObject(hObject, DACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetDaclSD(pSD, Dacl);
  FreeMem(pSD);
end;

// SACL
function NtxQuerySaclObject(hObject: THandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := NtxQuerySecurityObject(hObject, SACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetSaclSD(pSD, Sacl);
  FreeMem(pSD);
end;

// Mandatiry laber
function NtxQueryLabelObject(hObject: THandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
begin
  Result := NtxQuerySecurityObject(hObject, LABEL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxGetSaclSD(pSD, Sacl);
  FreeMem(pSD);
end;

{ Set security funtions }

function NtxSetSecurityObject(hObject: THandle; SecInfo: TSecurityInformation;
  const SecDesc: TSecurityDescriptor): TNtxStatus;
begin
  Result.Location := 'NtSetSecurityObject';
  Result.Status := NtSetSecurityObject(hObject, SecInfo, SecDesc);
end;

// Owner
function NtxSetOwnerObject(hObject: THandle; Owner: ISid): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareOwnerSD(SD, Owner);

  if Result.IsSuccess then
    Result := NtxSetSecurityObject(hObject, OWNER_SECURITY_INFORMATION, SD);
end;

// Primary group
function NtxSetPrimaryGroupObject(hObject: THandle; Group: ISid): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPreparePrimaryGroupSD(SD, Group);

  if Result.IsSuccess then
    Result := NtxSetSecurityObject(hObject, GROUP_SECURITY_INFORMATION, SD);
end;

// DACL
function NtxSetDaclObject(hObject: THandle; Dacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareDaclSD(SD, Dacl);

  if Result.IsSuccess then
    Result := NtxSetSecurityObject(hObject, DACL_SECURITY_INFORMATION, SD);
end;

// SACL
function NtxSetSaclObject(hObject: THandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareSaclSD(SD, Sacl);

  if Result.IsSuccess then
    Result := NtxSetSecurityObject(hObject, SACL_SECURITY_INFORMATION, SD);
end;

// Mandatory label
function NtxSetLabelObject(hObject: THandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  Result := RtlxPrepareSaclSD(SD, Sacl);

  if Result.IsSuccess then
    Result := NtxSetSecurityObject(hObject, LABEL_SECURITY_INFORMATION, SD);
end;

end.
