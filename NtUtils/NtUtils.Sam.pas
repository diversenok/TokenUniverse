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

// Free a buffer returned by a SamxQuery* function
function SamxFreeMemory(Buffer: Pointer): NTSTATUS;

{ --------------------------------- Domains -------------------------------- }

// Open a domain
function SamxOpenDomain(out hDomain: TSamHandle; DomainId: PSid;
  DesiredAccess: TAccessMask): TNtxStatus; overload;
function SamxOpenDomain(out hDomain: TSamHandle; hServer: TSamHandle;
  DomainId: PSid; DesiredAccess: TAccessMask): TNtxStatus; overload;

// Open the parent of the SID as a domain
function SamxOpenParentDomain(out hDomain: TSamHandle; SID: ISid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Lookup a domain
function SamxLookupDomain(hServer: TSamHandle; Name: String;
  out DomainId: ISid): TNtxStatus;

// Enumerate domains
function SamxEnumerateDomains(hServer: TSamHandle; out Names: TStringArray):
  TNtxStatus;

// Query domain information; free the result with SamxFreeMemory
function SamxQueryDomain(hDomain: TSamHandle; InfoClass:
  TDomainInformationClass; out Status: TNtxStatus): Pointer;

// Set domain information
function SamxSetDomain(hDomain: TSamHandle; InfoClass: TDomainInformationClass;
  Data: Pointer): TNtxStatus;

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

// Query group information; free the result with SamxFreeMemory
function SamxQueryGroup(hGroup: TSamHandle; InfoClass: TGroupInformationClass;
  out Status: TNtxStatus): Pointer;

// Set group information
function SamxSetGroup(hGroup: TSamHandle; InfoClass: TGroupInformationClass;
  Data: Pointer): TNtxStatus;

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

// Query alias information; free the result with SamxFreeMemory
function SamxQueryAlias(hAlias: TSamHandle; InfoClass: TAliasInformationClass;
  out Status: TNtxStatus): Pointer;

// Set alias information
function SamxSetAlias(hAlias: TSamHandle; InfoClass: TAliasInformationClass;
  Data: Pointer): TNtxStatus;

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

// Query user information; free the result with SamxFreeMemory
function SamxQueryUser(hUser: TSamHandle; InfoClass: TUserInformationClass;
  out Status: TNtxStatus): Pointer;

// Set user information
function SamxSetUser(hUser: TSamHandle; InfoClass: TUserInformationClass;
  Data: Pointer): TNtxStatus;

implementation

uses
  Ntapi.ntstatus, System.TypInfo;

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

function SamxFreeMemory(Buffer: Pointer): NTSTATUS;
begin
  Result := SamFreeMemory(Buffer);
end;

{ Domains }

function SamxOpenDomain(out hDomain: TSamHandle; hServer: TSamHandle;
  DomainId: PSid; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'SamOpenDomain';
  Result.Status := SamOpenDomain(hServer, DesiredAccess, DomainId, hDomain);
end;

function SamxOpenDomain(out hDomain: TSamHandle; DomainId: PSid;
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

  Result := SamxOpenDomain(hDomain, Sid.ParentSid.Sid, DOMAIN_LOOKUP);
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

function SamxQueryDomain(hDomain: TSamHandle; InfoClass:
  TDomainInformationClass; out Status: TNtxStatus): Pointer;
begin
  Status.Location := 'SamQueryInformationDomain [' +
    GetEnumName(TypeInfo(TDomainInformationClass), Integer(InfoClass)) + ']';
  Status.Status := SamQueryInformationDomain(hDomain, InfoClass, Result);
end;

function SamxSetDomain(hDomain: TSamHandle; InfoClass: TDomainInformationClass;
  Data: Pointer): TNtxStatus;
begin
  Result.Location := 'SamSetInformationDomain [' +
    GetEnumName(TypeInfo(TDomainInformationClass), Integer(InfoClass)) + ']';
  Result.Status := SamSetInformationDomain(hDomain, InfoClass, Data);
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

function SamxQueryGroup(hGroup: TSamHandle; InfoClass: TGroupInformationClass;
  out Status: TNtxStatus): Pointer;
begin
  Status.Location := 'SamQueryInformationGroup [' +
    GetEnumName(TypeInfo(TGroupInformationClass), Integer(InfoClass)) + ']';
  Status.Status := SamQueryInformationGroup(hGroup, InfoClass, Result);
end;

function SamxSetGroup(hGroup: TSamHandle; InfoClass: TGroupInformationClass;
  Data: Pointer): TNtxStatus;
begin
  Result.Location := 'SamSetInformationGroup [' +
    GetEnumName(TypeInfo(TGroupInformationClass), Integer(InfoClass)) + ']';
  Result.Status := SamSetInformationGroup(hGroup, InfoClass, Data);
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

function SamxQueryAlias(hAlias: TSamHandle; InfoClass: TAliasInformationClass;
  out Status: TNtxStatus): Pointer;
begin
  Status.Location := 'SamQueryInformationAlias [' +
    GetEnumName(TypeInfo(TAliasInformationClass), Integer(InfoClass)) + ']';
  Status.Status := SamQueryInformationAlias(hAlias, InfoClass, Result);
end;

// Set alias information
function SamxSetAlias(hAlias: TSamHandle; InfoClass: TAliasInformationClass;
  Data: Pointer): TNtxStatus;
begin
  Result.Location := 'SamSetInformationAlias [' +
    GetEnumName(TypeInfo(TAliasInformationClass), Integer(InfoClass)) + ']';
  Result.Status := SamSetInformationAlias(hAlias, InfoClass, Data);
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

function SamxQueryUser(hUser: TSamHandle; InfoClass: TUserInformationClass;
  out Status: TNtxStatus): Pointer;
begin
  Status.Location := 'SamQueryInformationUser [' +
    GetEnumName(TypeInfo(TUserInformationClass), Integer(InfoClass)) + ']';
  Status.Status := SamQueryInformationUser(hUser, InfoClass, Result);
end;

// Set user information
function SamxSetUser(hUser: TSamHandle; InfoClass: TUserInformationClass;
  Data: Pointer): TNtxStatus;
begin
  Result.Location := 'SamSetInformationUser [' +
    GetEnumName(TypeInfo(TUserInformationClass), Integer(InfoClass)) + ']';
  Result.Status := SamSetInformationUser(hUser, InfoClass, Data);
end;

end.
