unit NtUtils.Security.Acl;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, NtUtils.Security.Sid, NtUtils.Exceptions;

type
  TAce = record
    AceType: TAceType;
    AceFlags: Byte;
    Mask: TAccessMask;
    Sid: ISid;
    function Size: Cardinal;
    function Allocate: PAce;
  end;

  IAcl = interface
    function Acl: PAcl;
    function Count: Integer;
    function GetAce(Index: Integer): TAce;
    function Delete(Index: Integer): TNtxStatus;
    function AddAt(const Ace: TAce; Index: Integer): TNtxStatus;
  end;

  TAcl = class(TInterfacedObject, IAcl)
  private
    FAcl: PAcl;
    AclSize: Cardinal;
  public
    constructor CreateEmpy(InitialSize: Cardinal = 1024);
    constructor CreateCopy(SrcAcl: PAcl);
    function Acl: PAcl;
    function Count: Integer;
    function GetAce(Index: Integer): TAce;
    function Delete(Index: Integer): TNtxStatus;
    function AddAt(const Ace: TAce; Index: Integer): TNtxStatus;
  end;

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
  Ntapi.ntrtl, Ntapi.ntstatus, Ntapi.ntobapi;

{ TAce }

function TAce.Allocate: PAce;
begin
  if not Assigned(Sid) then
    raise ENtError.Create(STATUS_INVALID_SID, 'TAce.Allocate');

  // TODO: Object aces?
  Result := AllocMem(Size);
  Result.Header.AceType := AceType;
  Result.Header.AceFlags := AceFlags;
  Result.Header.AceSize := Size;
  Result.Mask := Mask;
  Move(Sid.Sid^, Result.Sid^, RtlLengthSid(Sid.Sid));
end;

function TAce.Size: Cardinal;
begin
  if not Assigned(Sid) then
    raise ENtError.Create(STATUS_INVALID_SID, 'TAce.Size');

  Result := SizeOf(TAce_Internal) - SizeOf(Cardinal) + RtlLengthSid(Sid.Sid);
end;

{ TAcl }

function TAcl.AddAt(const Ace: TAce; Index: Integer): TNtxStatus;
var
  AceBuffer: PAce;
begin
  AceBuffer := Ace.Allocate;

  Result.Location := 'RtlAddAce';
  Result.Status := RtlAddAce(FAcl, ACL_REVISION, Index, AceBuffer, Ace.Size);

  FreeMem(AceBuffer);
end;

function TAcl.Count: Integer;
var
  SizeInfo: TAclSizeInformation;
begin
  NtxCheck(RtlQueryInformationAcl(FAcl, SizeInfo), 'RtlQueryInformationAcl');
  Result := SizeInfo.AceCount;
end;

constructor TAcl.CreateCopy(SrcAcl: PAcl);
var
  i: Integer;
  SizeInfo: TAclSizeInformation;
  Ace: PAce;
begin
  if not Assigned(SrcAcl) or not RtlValidAcl(SrcAcl) then
    raise ENtError.Create(STATUS_INVALID_ACL, 'RtlValidAcl');

  NtxCheck(RtlQueryInformationAcl(SrcAcl, SizeInfo), 'RtlQueryInformationAcl');

  AclSize := SizeInfo.AclBytesInUse;
  AclSize := AclSize + AclSize shr 4 + 256; // + 6% + 256 Bytes
  AclSize := (AclSize shr 10 + 1) shl 10; // Round up to 1024n Bytes

  CreateEmpy(AclSize);

  for i := 0 to SizeInfo.AceCount - 1 do
  begin
    NtxCheck(RtlGetAce(SrcAcl, i, Ace), 'RtlGetAce');
    NtxCheck(RtlAddAce(FAcl, ACL_REVISION, -1, Ace, Ace.Header.AceSize),
      'RtlAddAce');
  end;
end;

constructor TAcl.CreateEmpy(InitialSize: Cardinal);
const
  MAX_ACL_SIZE = $FFFC; // TODO: reversed; does this value present in headers?
var
  Status: NTSTATUS;
begin
  if InitialSize > MAX_ACL_SIZE then
    InitialSize := MAX_ACL_SIZE;

  AclSize := InitialSize;
  FAcl := AllocMem(AclSize);
  Status := RtlCreateAcl(FAcl, AclSize, ACL_REVISION);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(FAcl);
    raise ENtError.Create(Status, 'RtlCreateAcl');
  end;
end;

function TAcl.Delete(Index: Integer): TNtxStatus;
begin
  Result.Location := 'RtlDeleteAce';
  Result.Status := RtlDeleteAce(FAcl, Index);
end;

function TAcl.GetAce(Index: Integer): TAce;
var
  pAceRef: PAce;
begin
  NtxCheck(RtlGetAce(FAcl, Index, pAceRef), 'RtlGetAce');

  Result.AceType := pAceRef.Header.AceType;
  Result.AceFlags := pAceRef.Header.AceFlags;

  case pAceRef.Header.AceType of

    AceTypeAccessAllowed,         AceTypeAccessDenied,
    AceTypeSystemAudit,           AceTypeSystemAlarm,
    AceTypeAccessAllowedCallback, AceTypeAccessDeniedCallback,
    AceTypeSystemAuditCallback,   AceTypeSystemAlarmCallback,
    AceTypeSystemMandatoryLabel,  AceTypeSystemResourceAttribute,
    AceTypeSystemScopedPolicyId,  AceTypeSystemProcessTrustLabel,
    AceTypeSystemAccessFilter:
    begin
      // Non-object aces
      Result.Mask := pAceRef.Mask;
      Result.Sid := TSid.CreateCopy(pAceRef.Sid);
    end;

    AceTypeObjectAccessAllowed,         AceTypeObjectAccessDenied,
    AceTypeObjectSystemAudit,           AceTypeObjectSystemAlarm,
    AceTypeObjectAccessAllowedCallback, AceTypeObjectAccessDeniedCallback,
    AceTypeObjectSystemAuditCallback,   AceTypeObjectSystemAlarmCallback:
    begin
      // Object aces
      Result.Mask := PObjectAce(pAceRef).Mask;
      Result.Sid := TSid.CreateCopy(PObjectAce(pAceRef).Sid);
    end;

  else
    // Unsupported ace type
    Result.Mask := 0;
    Result.Sid := nil;
  end;
end;

function TAcl.Acl: PAcl;
begin
  Result := FAcl;
end;

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
    Exit;
  end;
end;

// Owner
function NtxQueryOwnerObject(hObject: THandle; out Owner: ISid): TNtxStatus;
var
  SecDesc: PSecurityDescriptor;
  Defaulted: Boolean;
  OwnerSid: PSid;
begin
  Result := NtxQuerySecurityObject(hObject, OWNER_SECURITY_INFORMATION,
    SecDesc);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetOwnerSecurityDescriptor';
  Result.Status := RtlGetOwnerSecurityDescriptor(SecDesc, OwnerSid,  Defaulted);

  if Result.IsSuccess then
    Owner := TSid.CreateCopy(OwnerSid);

  FreeMem(SecDesc);
end;

// Primary group
function NtxQueryPrimaryGroupObject(hObject: THandle;
  out PrimaryGroup: ISid): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  Defaulted: Boolean;
  GroupSid: PSid;
begin
  Result := NtxQuerySecurityObject(hObject, GROUP_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetGroupSecurityDescriptor';
  Result.Status := RtlGetGroupSecurityDescriptor(pSD, GroupSid, Defaulted);

  if Result.IsSuccess then
    PrimaryGroup := TSid.CreateCopy(GroupSid);

  FreeMem(pSD);
end;

// DACL
function NtxQueryDaclObject(hObject: THandle; out Dacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  pDaclRef: PAcl;
  DaclPresent, Defaulted: Boolean;
begin
  Result := NtxQuerySecurityObject(hObject, DACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetDaclSecurityDescriptor';
  Result.Status := RtlGetDaclSecurityDescriptor(pSD, DaclPresent, pDaclRef,
    Defaulted);

  if Result.IsSuccess and DaclPresent and Assigned(pDaclRef) then
    Dacl := TAcl.CreateCopy(pDaclRef)
  else
    Dacl := nil;

  FreeMem(pSD);
end;

// SACL
function NtxQuerySaclObject(hObject: THandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  pSaclRef: PAcl;
  SaclPresent, Defaulted: Boolean;
begin
  Result := NtxQuerySecurityObject(hObject, SACL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetSaclSecurityDescriptor';
  Result.Status := RtlGetSaclSecurityDescriptor(pSD, SaclPresent, pSaclRef,
    Defaulted);

  if Result.IsSuccess and SaclPresent and Assigned(pSaclRef) then
    Sacl := TAcl.CreateCopy(pSaclRef)
  else
    Sacl := nil;

  FreeMem(pSD);
end;

// Mandatiry laber
function NtxQueryLabelObject(hObject: THandle; out Sacl: IAcl): TNtxStatus;
var
  pSD: PSecurityDescriptor;
  pSaclRef: PAcl;
  SaclPresent, Defaulted: Boolean;
begin
  Result := NtxQuerySecurityObject(hObject, LABEL_SECURITY_INFORMATION, pSD);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlGetSaclSecurityDescriptor';
  Result.Status := RtlGetSaclSecurityDescriptor(pSD, SaclPresent, pSaclRef,
    Defaulted);

  if Result.IsSuccess and SaclPresent and Assigned(pSaclRef) then
    Sacl := TAcl.CreateCopy(pSaclRef)
  else
    Sacl := nil;

  FreeMem(pSD);
end;

{ Set security funtions }

// Owner
function NtxSetOwnerObject(hObject: THandle; Owner: ISid): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  FillChar(SD, SizeOf(SD), 0);

  Result.Location := 'RtlCreateSecurityDescriptor';
  Result.Status := RtlCreateSecurityDescriptor(SD,
    SECURITY_DESCRIPTOR_REVISION);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetOwnerSecurityDescriptor';

  if Assigned(Owner) then
    Result.Status := RtlSetOwnerSecurityDescriptor(SD, Owner.Sid, False)
  else
    Result.Status := RtlSetOwnerSecurityDescriptor(SD, nil, True);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'NtSetSecurityObject';
  Result.Status := NtSetSecurityObject(hObject, OWNER_SECURITY_INFORMATION, SD);
end;

// Primary group
function NtxSetPrimaryGroupObject(hObject: THandle; Group: ISid): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  FillChar(SD, SizeOf(SD), 0);

  Result.Location := 'RtlCreateSecurityDescriptor';
  Result.Status := RtlCreateSecurityDescriptor(SD,
    SECURITY_DESCRIPTOR_REVISION);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetGroupSecurityDescriptor';

  if Assigned(Group) then
    Result.Status := RtlSetGroupSecurityDescriptor(SD, Group.Sid, False)
  else
    Result.Status := RtlSetGroupSecurityDescriptor(SD, nil, True);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'NtSetSecurityObject';
  Result.Status := NtSetSecurityObject(hObject, GROUP_SECURITY_INFORMATION, SD);
end;

// DACL
function NtxSetDaclObject(hObject: THandle; Dacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  FillChar(SD, SizeOf(SD), 0);

  Result.Location := 'RtlCreateSecurityDescriptor';
  Result.Status := RtlCreateSecurityDescriptor(SD,
    SECURITY_DESCRIPTOR_REVISION);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetDaclSecurityDescriptor';

  if Assigned(Dacl) then
    Result.Status := RtlSetDaclSecurityDescriptor(SD, True, Dacl.Acl, False)
  else
    Result.Status := RtlSetDaclSecurityDescriptor(SD, True, nil, False);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'NtSetSecurityObject';
  Result.Status := NtSetSecurityObject(hObject, DACL_SECURITY_INFORMATION, SD);
end;

// SACL
function NtxSetSaclObject(hObject: THandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  FillChar(SD, SizeOf(SD), 0);

  Result.Location := 'RtlCreateSecurityDescriptor';
  Result.Status := RtlCreateSecurityDescriptor(SD,
    SECURITY_DESCRIPTOR_REVISION);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetSaclSecurityDescriptor';

  if Assigned(Sacl) then
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, Sacl.Acl, False)
  else
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, nil, False);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'NtSetSecurityObject';
  Result.Status := NtSetSecurityObject(hObject, SACL_SECURITY_INFORMATION, SD);
end;

// Mandatory label
function NtxSetLabelObject(hObject: THandle; Sacl: IAcl): TNtxStatus;
var
  SD: TSecurityDescriptor;
begin
  FillChar(SD, SizeOf(SD), 0);

  Result.Location := 'RtlCreateSecurityDescriptor';
  Result.Status := RtlCreateSecurityDescriptor(SD,
    SECURITY_DESCRIPTOR_REVISION);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetSaclSecurityDescriptor';

  if Assigned(Sacl) then
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, Sacl.Acl, False)
  else
    Result.Status := RtlSetSaclSecurityDescriptor(SD, True, nil, False);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'NtSetSecurityObject';
  Result.Status := NtSetSecurityObject(hObject, LABEL_SECURITY_INFORMATION, SD);
end;

end.
