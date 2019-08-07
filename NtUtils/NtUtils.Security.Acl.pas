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
    function SizeInfo: TAclSizeInformation;
    function GetAce(Index: Integer): TAce;
    function Delete(Index: Integer): TNtxStatus;
    function AddAt(const Ace: TAce; Index: Integer): TNtxStatus;
    procedure MapGenericMask(const GenericMapping: TGenericMapping);
  end;

  TAcl = class(TInterfacedObject, IAcl)
  private
    FAcl: PAcl;
    procedure Expand(AtLeast: Cardinal);
  public
    constructor CreateEmpy(InitialSize: Cardinal = 512);
    constructor CreateCopy(SrcAcl: PAcl);
    function Acl: PAcl;
    function SizeInfo: TAclSizeInformation;
    function GetAce(Index: Integer): TAce;
    function Delete(Index: Integer): TNtxStatus;
    function AddAt(const Ace: TAce; Index: Integer): TNtxStatus;
    procedure MapGenericMask(const GenericMapping: TGenericMapping);
  end;

// Query ACL size information
function RtlxQuerySizeInfoAcl(Acl: PAcl; out SizeInfo: TAclSizeInformation):
  TNtxStatus;

// Appen all ACEs from one ACL to another
function RtlxAppendAcl(SourceAcl, TargetAcl: PAcl): TNtxStatus;

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

// Compute required access to read/write security
function RtlxComputeReadAccess(SecurityInformation: TSecurityInformation)
  : TAccessMask;
function RtlxComputeWriteAccess(SecurityInformation: TSecurityInformation)
  : TAccessMask;

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

function ExpandingEq(Value: Cardinal): Cardinal; inline;
begin
  // Exrta capacity: +12.5% + 256 Bytes
  Result := Value shr 3 + 256;
end;

function ExpandSize(SizeInfo: TAclSizeInformation;
  Requires: Cardinal): Cardinal;
begin
  // Satisfy the requirements + some extra capacity
  if SizeInfo.AclBytesFree < Requires then
    SizeInfo.AclBytesFree := Requires + ExpandingEq(Requires);

  // Make sure we have enough extra capacity
  if SizeInfo.AclBytesFree < ExpandingEq(SizeInfo.AclBytesInUse) then
    SizeInfo.AclBytesFree := ExpandingEq(SizeInfo.AclBytesInUse);

  Result := SizeInfo.AclBytesTotal;

  if Result > MAX_ACL_SIZE then
    Result := MAX_ACL_SIZE;
end;

function TAcl.Acl: PAcl;
begin
  Result := FAcl;
end;

function TAcl.AddAt(const Ace: TAce; Index: Integer): TNtxStatus;
var
  AceBuffer: PAce;
begin
  // Expand the ACL if we are going to run out of space
  if SizeInfo.AclBytesFree < Ace.Size then
    Expand(Ace.Size);

  AceBuffer := Ace.Allocate;

  Result.Location := 'RtlAddAce';
  Result.Status := RtlAddAce(FAcl, ACL_REVISION, Index, AceBuffer, Ace.Size);

  FreeMem(AceBuffer);
end;

constructor TAcl.CreateCopy(SrcAcl: PAcl);
var
  SizeInfo: TAclSizeInformation;
begin
  if not Assigned(SrcAcl) or not RtlValidAcl(SrcAcl) then
    NtxAssert(STATUS_INVALID_ACL, 'RtlValidAcl');

  RtlxQuerySizeInfoAcl(SrcAcl, SizeInfo).RaiseOnError;

  // Create an ACL, potentially with some extra capacity
  CreateEmpy(ExpandSize(SizeInfo, 0));

  // Add all aces from the source ACL
  RtlxAppendAcl(SrcAcl, FAcl).RaiseOnError;
end;

constructor TAcl.CreateEmpy(InitialSize: Cardinal);
var
  Status: NTSTATUS;
begin
  if InitialSize > MAX_ACL_SIZE then
    InitialSize := MAX_ACL_SIZE;

  FAcl := AllocMem(InitialSize);
  Status := RtlCreateAcl(FAcl, InitialSize, ACL_REVISION);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(FAcl);
    NtxAssert(Status, 'RtlCreateAcl');
  end;
end;

function TAcl.Delete(Index: Integer): TNtxStatus;
begin
  Result.Location := 'RtlDeleteAce';
  Result.Status := RtlDeleteAce(FAcl, Index);
end;

procedure TAcl.Expand(AtLeast: Cardinal);
var
  NewAcl: PAcl;
  NewAclSize: Cardinal;
  Status: NTSTATUS;
begin
  NewAclSize := ExpandSize(SizeInfo, AtLeast);

  // Check if expanding is possible/needs reallocation
  if NewAclSize = SizeInfo.AclBytesTotal then
    Exit;

  NewAcl := AllocMem(NewAclSize);
  Status := RtlCreateAcl(NewAcl, NewAclSize, ACL_REVISION);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(NewAcl);
    NtxAssert(Status, 'RtlCreateAcl');
  end;

  // Copy all ACEs to the new ACL
  RtlxAppendAcl(FAcl, NewAcl).RaiseOnError;

  // Replace current ACL with the new one
  FreeMem(FAcl);
  FAcl := NewAcl;
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

procedure TAcl.MapGenericMask(const GenericMapping: TGenericMapping);
var
  i: Integer;
  Ace: PAce;
begin
  for i := 0 to SizeInfo.AceCount - 1 do
  begin
    NtxCheck(RtlGetAce(FAcl, i, Ace), 'RtlGetAce');
    RtlMapGenericMask(Ace.Mask, GenericMapping);
  end;
end;

function TAcl.SizeInfo: TAclSizeInformation;
begin
  RtlxQuerySizeInfoAcl(FAcl, Result).RaiseOnError;
end;

{ functions }

function RtlxQuerySizeInfoAcl(Acl: PAcl; out SizeInfo: TAclSizeInformation):
  TNtxStatus;
begin
  Result.Location := 'RtlQueryInformationAcl';
  Result.Status := RtlQueryInformationAcl(Acl, SizeInfo);
end;

function RtlxAppendAcl(SourceAcl, TargetAcl: PAcl): TNtxStatus;
var
  i: Integer;
  Ace: PAce;
  SizeInfo: TAclSizeInformation;
begin
  Result.Location := 'RtlQueryInformationAcl';
  Result.Status := RtlQueryInformationAcl(SourceAcl, SizeInfo);

  if not Result.IsSuccess then
    Exit;

  for i := 0 to SizeInfo.AceCount - 1 do
  begin
    Result.Location := 'RtlGetAce';
    Result.Status := RtlGetAce(SourceAcl, i, Ace);

    if not Result.IsSuccess then
      Exit;

    Result.Location := 'RtlAddAce';
    Result.Status := RtlAddAce(TargetAcl, ACL_REVISION, -1, Ace,
      Ace.Header.AceSize);

    if not Result.IsSuccess then
      Exit;
  end;
end;

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

function RtlxComputeReadAccess(SecurityInformation: TSecurityInformation)
  : TAccessMask;
const
  REQUIRE_READ_CONTROL = OWNER_SECURITY_INFORMATION or
    GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION or
    LABEL_SECURITY_INFORMATION or ATTRIBUTE_SECURITY_INFORMATION or
    SCOPE_SECURITY_INFORMATION or BACKUP_SECURITY_INFORMATION;
  REQUIRE_SYSTEM_SECURITY = SACL_SECURITY_INFORMATION or
    BACKUP_SECURITY_INFORMATION;
begin
  Result := 0;

  if SecurityInformation and REQUIRE_READ_CONTROL <> 0 then
    Result := Result or READ_CONTROL;

  if SecurityInformation and REQUIRE_SYSTEM_SECURITY <> 0 then
    Result := Result or ACCESS_SYSTEM_SECURITY;
end;

function RtlxComputeWriteAccess(SecurityInformation: TSecurityInformation)
  : TAccessMask;
const
  REQUIRE_WRITE_DAC = DACL_SECURITY_INFORMATION or
    ATTRIBUTE_SECURITY_INFORMATION or BACKUP_SECURITY_INFORMATION or
    PROTECTED_DACL_SECURITY_INFORMATION or
    UNPROTECTED_DACL_SECURITY_INFORMATION;
  REQUIRE_WRITE_OWNER = OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION
    or LABEL_SECURITY_INFORMATION or BACKUP_SECURITY_INFORMATION;
  REQUIRE_SYSTEM_SECURITY = SACL_SECURITY_INFORMATION or
    SCOPE_SECURITY_INFORMATION or
    BACKUP_SECURITY_INFORMATION or PROTECTED_SACL_SECURITY_INFORMATION or
    UNPROTECTED_SACL_SECURITY_INFORMATION;
begin
  Result := 0;

  if SecurityInformation and REQUIRE_WRITE_DAC <> 0 then
    Result := Result or WRITE_DAC;

  if SecurityInformation and REQUIRE_WRITE_OWNER <> 0 then
    Result := Result or WRITE_OWNER;

  if SecurityInformation and REQUIRE_SYSTEM_SECURITY <> 0 then
    Result := Result or ACCESS_SYSTEM_SECURITY;
end;

end.
