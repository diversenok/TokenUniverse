unit NtUtils.Sam;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntsam, NtUtils.Exceptions,
  NtUtils.Security.Sid;

type
  TRidAndName = record
    Name: String;
    RelativeId: Cardinal;
  end;

  TRidAndNameArray = array of TRidAndName;

  TGroupMembership = Ntapi.ntsam.TGroupMembership;
  TGroupMemberships = array of TGroupMembership;

// Connect to the local SAM server
function SamxConnect(out hServer: TSamHandle; DesiredAccess: TAccessMask):
  TNtxStatus;

// Connect to a remote SAM server
function SamxConnectRemote(out hServer: TSamHandle; ServerName: String;
  DesiredAccess: TAccessMask): TNtxStatus;

// Close SAM handle
function SamxClose(var SamHandle: TSamHandle): NTSTATUS;

{ --------------------------------- Domains -------------------------------- }

// Open a domain
function SamxOpenDomain(out hDomain: TSamHandle; hServer: TSamHandle;
  DomainId: PSid; DesiredAccess: TAccessMask): TNtxStatus;

function SamxOpenLocalDomain(out hDomain: TSamHandle; DomainId: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Open the parent of the SID as a domain
function SamxOpenParentDomain(out hDomain: TSamHandle; SID: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Lookup a domain
function SamxLookupDomain(hServer: TSamHandle; Name: String;
  out DomainId: ISid): TNtxStatus;

// Enumerate domains
function SamxEnumerateDomains(hServer: TSamHandle; out Names: TStringArray):
  TNtxStatus;

{ --------------------------------- Groups ---------------------------------- }

// Enumerate groups
function SamxEnumerateGroups(hDomain: TSamHandle; out Groups: TRidAndNameArray):
  TNtxStatus;

// Open a group
function SamxOpenGroup(out hGroup: TSamHandle; hDomain: TSamHandle;
  GroupId: Cardinal; DesiredAccess: TAccessMask): TNtxStatus;

// Open a group by SID
function SamxOpenGroupBySid(out hGroup: TSamHandle; Sid: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Get groups members
function SamxGetMembersGroup(hGroup: TSamHandle;
  out Members: TGroupMemberships): TNtxStatus;

{ --------------------------------- Aliases --------------------------------- }

// Enumerate aliases in domain
function SamxEnumerateAliases(hDomain: TSamHandle;
  out Aliases: TRidAndNameArray): TNtxStatus;

// Open an alias
function SamxOpenAlias(out hAlias: TSamHandle; hDomain: TSamHandle;
  AliasId: Cardinal; DesiredAccess: TAccessMask): TNtxStatus;

// Open an alias by SID
function SamxOpenAliasBySid(out hAlias: TSamHandle; Sid: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Get alias members
function SamxGetMembersAlias(hAlias: TSamHandle; out Members: ISidArray):
  TNtxStatus;

{ ---------------------------------- Users ---------------------------------- }

// Enumerate users in domain
function SamxEnumerateUsers(hDomain: TSamHandle; UserType: Cardinal;
  out Users: TRidAndNameArray): TNtxStatus;

// Open a user
function SamxOpenUser(out hUser: TSamHandle; hDomain: TSamHandle;
  UserId: Cardinal; DesiredAccess: TAccessMask): TNtxStatus;

// Open a user by SID
function SamxOpenUserBySid(out hUser: TSamHandle; Sid: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Get groups for a user
function SamxGetGroupsForUser(hUser: TSamHandle; out Groups: TGroupMemberships):
  TNtxStatus;

implementation

uses
  Ntapi.ntstatus;

{ Server }

function SamxConnect(out hServer: TSamHandle; DesiredAccess: TAccessMask):
  TNtxStatus;
var
  ObjAttr: TObjectAttributes;
begin
  InitializeObjectAttributes(ObjAttr);
  Result.Location := 'SamConnect';
  Result.Status := SamConnect(nil, hServer, DesiredAccess, ObjAttr);
end;

function SamxConnectRemote(out hServer: TSamHandle; ServerName: String;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  NameStr: UNICODE_STRING;
begin
  InitializeObjectAttributes(ObjAttr);
  NameStr.FromString(ServerName);

  Result.Location := 'SamConnect';
  Result.Status := SamConnect(@NameStr, hServer, DesiredAccess, ObjAttr);
end;

function SamxClose(var SamHandle: TSamHandle): NTSTATUS;
begin
  Result := SamCloseHandle(SamHandle);
  SamHandle := 0;
end;

{ Domains }

function SamxOpenDomain(out hDomain: TSamHandle; hServer: TSamHandle;
  DomainId: PSid; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'SamOpenDomain';
  Result.Status := SamOpenDomain(hServer, DesiredAccess, DomainId, hDomain);
end;

function SamxOpenLocalDomain(out hDomain: TSamHandle; DomainId: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hServer: TSamHandle;
begin
  Result := SamxConnect(hServer, SAM_SERVER_LOOKUP_DOMAIN);

  if not Result.IsSuccess then
    Exit;

  Result := SamxOpenDomain(hDomain, hServer, DomainId, DesiredAccess);
  SamxClose(hServer);
end;

function SamxOpenParentDomain(out hDomain: TSamHandle; SID: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;
begin
  if Sid.SubAuthorities = 0 then
  begin
    Result.Location := 'ISid.ParentSid';
    Result.Status := STATUS_INVALID_SID;
    Exit;
  end;

  Result := SamxOpenLocalDomain(hDomain, Sid.ParentSid.Sid, DOMAIN_LOOKUP);
end;

function SamxLookupDomain(hServer: TSamHandle; Name: String;
  out DomainId: ISid): TNtxStatus;
var
  NameStr: UNICODE_STRING;
  Buffer: PSid;
begin
  NameStr.FromString(Name);

  Result.Location := 'SamLookupDomainInSamServer';
  Result.Status := SamLookupDomainInSamServer(hServer, NameStr, Buffer);

  if Result.IsSuccess then
    DomainId := TSid.CreateCopy(Buffer);

  SamFreeMemory(Buffer);
end;

function SamxEnumerateDomains(hServer: TSamHandle; out Names: TStringArray):
  TNtxStatus;
var
  EnumContext: TSamEnumerationHandle;
  Buffer: PSamRidEnumerationArray;
  Count, i: Integer;
begin
  EnumContext := 0;

  Result.Location := 'SamEnumerateDomainsInSamServer';
  Result.Status := SamEnumerateDomainsInSamServer(hServer, EnumContext, Buffer,
    MAX_PREFERRED_LENGTH, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Names, Count);

  // RelativeId is always zero for domains, but names are available
  for i := 0 to High(Names) do
    Names[i] := Buffer[i].Name.ToString;

  SamFreeMemory(Buffer);
end;

{ Groups }

function SamxEnumerateGroups(hDomain: TSamHandle; out Groups: TRidAndNameArray):
  TNtxStatus;
var
  EnumContext: TSamEnumerationHandle;
  Buffer: PSamRidEnumerationArray;
  Count, i: Integer;
begin
  EnumContext := 0;

  Result.Location := 'SamEnumerateGroupsInDomain';
  Result.Status := SamEnumerateGroupsInDomain(hDomain, EnumContext, Buffer,
    MAX_PREFERRED_LENGTH, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Groups, Count);

  for i := 0 to High(Groups) do
  begin
    Groups[i].RelativeId := Buffer[i].RelativeId;
    Groups[i].Name := Buffer[i].Name.ToString;
  end;

  SamFreeMemory(Buffer);
end;

function SamxOpenGroup(out hGroup: TSamHandle; hDomain: TSamHandle;
  GroupId: Cardinal; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'SamOpenGroup';
  Result.Status := SamOpenGroup(hDomain, DesiredAccess, GroupId, hGroup);
end;

function SamxOpenGroupBySid(out hGroup: TSamHandle; Sid: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hDomain: TSamHandle;
begin
  Result := SamxOpenParentDomain(hDomain, Sid, DOMAIN_LOOKUP);

  if not Result.IsSuccess then
    Exit;

  Result := SamxOpenGroup(hGroup, hDomain, Sid.Rid, DesiredAccess);
  SamxClose(hDomain);
end;

function SamxGetMembersGroup(hGroup: TSamHandle;
  out Members: TGroupMemberships): TNtxStatus;
var
  BufferIDs, BufferAttributes: PCardinalArray;
  Count, i: Integer;
begin
  Result.Location := 'SamGetMembersInGroup';
  Result.Status := SamGetMembersInGroup(hGroup, BufferIDs, BufferAttributes,
    Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Members, Count);

  for i := 0 to High(Members) do
  begin
    Members[i].RelativeId := BufferIDs[i];
    Members[i].Attributes := BufferAttributes[i];
  end;

  SamFreeMemory(BufferIDs);
  SamFreeMemory(BufferAttributes);
end;

{ Aliases }

function SamxEnumerateAliases(hDomain: TSamHandle;
  out Aliases: TRidAndNameArray): TNtxStatus;
var
  EnumContext: TSamEnumerationHandle;
  Buffer: PSamRidEnumerationArray;
  Count, i: Integer;
begin
  EnumContext := 0;

  Result.Location := 'SamEnumerateAliasesInDomain';
  Result.Status := SamEnumerateAliasesInDomain(hDomain, EnumContext,
    Buffer, MAX_PREFERRED_LENGTH, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Aliases, Count);

  for i := 0 to High(Aliases) do
  begin
    Aliases[i].RelativeId := Buffer[i].RelativeId;
    Aliases[i].Name := Buffer[i].Name.ToString;
  end;

  SamFreeMemory(Buffer);
end;

function SamxOpenAlias(out hAlias: TSamHandle; hDomain: TSamHandle;
  AliasId: Cardinal; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'SamOpenAlias';
  Result.Status := SamOpenAlias(hDomain, DesiredAccess, AliasId, hAlias);
end;

function SamxOpenAliasBySid(out hAlias: TSamHandle; Sid: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hDomain: TSamHandle;
begin
  Result := SamxOpenParentDomain(hDomain, Sid, DOMAIN_LOOKUP);

  if not Result.IsSuccess then
    Exit;

  Result := SamxOpenAlias(hAlias, hDomain, Sid.Rid, DesiredAccess);
  SamxClose(hDomain);
end;

function SamxGetMembersAlias(hAlias: TSamHandle; out Members: ISidArray):
  TNtxStatus;
var
  Buffer: PSidArray;
  Count, i: Integer;
begin
  Result.Location := 'SamGetMembersInAlias';
  Result.Status := SamGetMembersInAlias(hAlias, Buffer, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Members, Count);

  for i := 0 to High(Members) do
    Members[i] := TSid.CreateCopy(Buffer[i]);

  SamFreeMemory(Buffer);
end;

{ Users }

function SamxEnumerateUsers(hDomain: TSamHandle; UserType: Cardinal;
  out Users: TRidAndNameArray): TNtxStatus;
var
  EnumContext: TSamEnumerationHandle;
  Buffer: PSamRidEnumerationArray;
  Count, i: Integer;
begin
  EnumContext := 0;

  Result.Location := 'SamEnumerateUsersInDomain';
  Result.Status := SamEnumerateUsersInDomain(hDomain, EnumContext,
    UserType, Buffer, MAX_PREFERRED_LENGTH, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Users, Count);

  for i := 0 to High(Users) do
  begin
    Users[i].RelativeId := Buffer[i].RelativeId;
    Users[i].Name := Buffer[i].Name.ToString;
  end;

  SamFreeMemory(Buffer);
end;

function SamxOpenUser(out hUser: TSamHandle; hDomain: TSamHandle;
  UserId: Cardinal; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'SamOpenUser';
  Result.Status := SamOpenUser(hDomain, DesiredAccess, UserId, hUser);
end;

// Open a user by SID
function SamxOpenUserBySid(out hUser: TSamHandle; Sid: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hDomain: TSamHandle;
begin
  Result := SamxOpenParentDomain(hDomain, Sid, DOMAIN_LOOKUP);

  if not Result.IsSuccess then
    Exit;

  Result := SamxOpenUser(hUser, hDomain, Sid.Rid, DesiredAccess);
  SamxClose(hDomain);
end;

function SamxGetGroupsForUser(hUser: TSamHandle; out Groups: TGroupMemberships):
  TNtxStatus;
var
  Buffer: PGroupMembershipArray;
  Count, i: Integer;
begin
  Result.Location := 'SamGetGroupsForUser';
  Result.Status := SamGetGroupsForUser(hUser, Buffer, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Groups, Count);

  for i := 0 to High(Groups) do
    Groups[i] := Buffer[i]^;

  SamFreeMemory(Buffer);
end;

end.
