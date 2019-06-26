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

// Prepare security descriptor
function RtlxCreateSecurityDescriptor(var SecDesc: TSecurityDescriptor):
  TNtxStatus;

// Get owner from the security descriptor
function RtlxGetOwnerSD(pSecDesc: PSecurityDescriptor; out Owner: ISid):
  TNtxStatus;

// Get primary group from the security descriptor
function RtlxGetPrimaryGroupSD(pSecDesc: PSecurityDescriptor; out Group: ISid):
  TNtxStatus;

// Get DACL from the security descriptor
function RtlxGetDaclSD(pSecDesc: PSecurityDescriptor; out Dacl: IAcl):
  TNtxStatus;

// Get SACL from the security descriptor
function RtlxGetSaclSD(pSecDesc: PSecurityDescriptor; out Sacl: IAcl):
  TNtxStatus;

// Prepare a security descriptor with an owner
function RtlxPrepareOwnerSD(var SecDesc: TSecurityDescriptor; Owner: ISid):
  TNtxStatus;

// Prepare a security descriptor with a primary group
function RtlxPreparePrimaryGroupSD(var SecDesc: TSecurityDescriptor; Group: ISid):
  TNtxStatus;

// Prepare a security descriptor with a DACL
function RtlxPrepareDaclSD(var SecDesc: TSecurityDescriptor; Dacl: IAcl):
  TNtxStatus;

// Prepare a security descriptor with a SACL
function RtlxPrepareSaclSD(var SecDesc: TSecurityDescriptor; Sacl: IAcl):
  TNtxStatus;

implementation

uses
  Ntapi.ntrtl, Ntapi.ntstatus;

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

{ functions }

function RtlxCreateSecurityDescriptor(var SecDesc: TSecurityDescriptor):
  TNtxStatus;
begin
  FillChar(SecDesc, SizeOf(SecDesc), 0);
  Result.Location := 'RtlCreateSecurityDescriptor';
  Result.Status := RtlCreateSecurityDescriptor(SecDesc,
    SECURITY_DESCRIPTOR_REVISION);
end;

function RtlxGetOwnerSD(pSecDesc: PSecurityDescriptor; out Owner: ISid):
  TNtxStatus;
var
  Defaulted: Boolean;
  OwnerSid: PSid;
begin
  Result.Location := 'RtlGetOwnerSecurityDescriptor';
  Result.Status := RtlGetOwnerSecurityDescriptor(pSecDesc, OwnerSid, Defaulted);

  if Result.IsSuccess then
    Owner := TSid.CreateCopy(OwnerSid);
end;

function RtlxGetPrimaryGroupSD(pSecDesc: PSecurityDescriptor; out Group: ISid):
  TNtxStatus;
var
  Defaulted: Boolean;
  GroupSid: PSid;
begin
  Result.Location := 'RtlGetGroupSecurityDescriptor';
  Result.Status := RtlGetGroupSecurityDescriptor(pSecDesc, GroupSid, Defaulted);

  if Result.IsSuccess then
    Group := TSid.CreateCopy(GroupSid);
end;

function RtlxGetDaclSD(pSecDesc: PSecurityDescriptor; out Dacl: IAcl):
  TNtxStatus;
var
  pDaclRef: PAcl;
  DaclPresent, Defaulted: Boolean;
begin
  Result.Location := 'RtlGetDaclSecurityDescriptor';
  Result.Status := RtlGetDaclSecurityDescriptor(pSecDesc, DaclPresent, pDaclRef,
    Defaulted);

  if Result.IsSuccess and DaclPresent and Assigned(pDaclRef) then
    Dacl := TAcl.CreateCopy(pDaclRef)
  else
    Dacl := nil;
end;

function RtlxGetSaclSD(pSecDesc: PSecurityDescriptor; out Sacl: IAcl):
  TNtxStatus;
var
  pSaclRef: PAcl;
  SaclPresent, Defaulted: Boolean;
begin
  Result.Location := 'RtlGetSaclSecurityDescriptor';
  Result.Status := RtlGetSaclSecurityDescriptor(pSecDesc, SaclPresent, pSaclRef,
    Defaulted);

  if Result.IsSuccess and SaclPresent and Assigned(pSaclRef) then
    Sacl := TAcl.CreateCopy(pSaclRef)
  else
    Sacl := nil;
end;

function RtlxPrepareOwnerSD(var SecDesc: TSecurityDescriptor; Owner: ISid):
  TNtxStatus;
begin
  Result := RtlxCreateSecurityDescriptor(SecDesc);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetOwnerSecurityDescriptor';

  if Assigned(Owner) then
    Result.Status := RtlSetOwnerSecurityDescriptor(SecDesc, Owner.Sid, False)
  else
    Result.Status := RtlSetOwnerSecurityDescriptor(SecDesc, nil, True);
end;

function RtlxPreparePrimaryGroupSD(var SecDesc: TSecurityDescriptor; Group: ISid):
  TNtxStatus;
begin
  Result := RtlxCreateSecurityDescriptor(SecDesc);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetGroupSecurityDescriptor';

  if Assigned(Group) then
    Result.Status := RtlSetGroupSecurityDescriptor(SecDesc, Group.Sid, False)
  else
    Result.Status := RtlSetGroupSecurityDescriptor(SecDesc, nil, True);
end;

function RtlxPrepareDaclSD(var SecDesc: TSecurityDescriptor; Dacl: IAcl):
  TNtxStatus;
begin
  Result := RtlxCreateSecurityDescriptor(SecDesc);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetDaclSecurityDescriptor';

  if Assigned(Dacl) then
    Result.Status := RtlSetDaclSecurityDescriptor(SecDesc, True, Dacl.Acl,
      False)
  else
    Result.Status := RtlSetDaclSecurityDescriptor(SecDesc, True, nil, False);
end;

function RtlxPrepareSaclSD(var SecDesc: TSecurityDescriptor; Sacl: IAcl):
  TNtxStatus;
begin
  Result := RtlxCreateSecurityDescriptor(SecDesc);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'RtlSetSaclSecurityDescriptor';

  if Assigned(Sacl) then
    Result.Status := RtlSetSaclSecurityDescriptor(SecDesc, True, Sacl.Acl,
      False)
  else
    Result.Status := RtlSetSaclSecurityDescriptor(SecDesc, True, nil, False);
end;

end.
