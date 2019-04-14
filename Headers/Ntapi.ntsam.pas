unit Ntapi.ntsam;

interface
{$MINENUMSIZE 4}

uses
  Winapi.WinNt, Ntapi.ntdef;

const
  samlib = 'samlib.dll';

  MAX_PREFERRED_LENGTH = MaxInt;

  // 158
  SAM_SERVER_CONNECT = $0001;
  SAM_SERVER_SHUTDOWN = $0002;
  SAM_SERVER_INITIALIZE = $0004;
  SAM_SERVER_CREATE_DOMAIN = $0008;
  SAM_SERVER_ENUMERATE_DOMAINS = $0010;
  SAM_SERVER_LOOKUP_DOMAIN = $0020;

  // 202
  DOMAIN_READ_PASSWORD_PARAMETERS = $0001;
  DOMAIN_WRITE_PASSWORD_PARAMS = $0002;
  DOMAIN_READ_OTHER_PARAMETERS = $0004;
  DOMAIN_WRITE_OTHER_PARAMETERS = $0008;
  DOMAIN_CREATE_USER = $0010;
  DOMAIN_CREATE_GROUP = $0020;
  DOMAIN_CREATE_ALIAS = $0040;
  DOMAIN_GET_ALIAS_MEMBERSHIP = $0080;
  DOMAIN_LIST_ACCOUNTS = $0100;
  DOMAIN_LOOKUP = $0200;
  DOMAIN_ADMINISTER_SERVER = $0400;

  // 528
  GROUP_READ_INFORMATION = $0001;
  GROUP_WRITE_ACCOUNT = $0002;
  GROUP_ADD_MEMBER = $0004;
  GROUP_REMOVE_MEMBER = $0008;
  GROUP_LIST_MEMBERS = $0010;

  // 604
  ALIAS_ADD_MEMBER = $0001;
  ALIAS_REMOVE_MEMBER = $0002;
  ALIAS_LIST_MEMBERS = $0004;
  ALIAS_READ_INFORMATION = $0008;
  ALIAS_WRITE_ACCOUNT = $0010;

  // 706
  USER_READ_GENERAL = $0001;
  USER_READ_PREFERENCES = $0002;
  USER_WRITE_PREFERENCES = $0004;
  USER_READ_LOGON = $0008;
  USER_READ_ACCOUNT = $0010;
  USER_WRITE_ACCOUNT = $0020;
  USER_CHANGE_PASSWORD = $0040;
  USER_FORCE_PASSWORD_CHANGE = $0080;
  USER_LIST_GROUPS = $0100;
  USER_READ_GROUP_INFORMATION = $0200;
  USER_WRITE_GROUP_INFORMATION = $0400;

type
  TSamHandle = NativeUInt;

  TSamEnumerationHandle = Cardinal;

  // 77
  TSamRidEnumeration = record
    RelativeId: Cardinal;
    Name: UNICODE_STRING;
  end;
  PSamRidEnumeration = ^TSamRidEnumeration;

  TSamRidEnumerationArray = array [Word] of TSamRidEnumeration;
  PSamRidEnumerationArray = ^TSamRidEnumerationArray;

  // 82
  TSamSidEnumeration = record
    Sid: PSid;
    Name: UNICODE_STRING;
  end;
  PSamSidEnumeration = ^TSamSidEnumeration;

  TSamSidEnumerationArray = array [Word] of TSamSidEnumeration;
  PSamSidEnumerationArray = ^TSamSidEnumerationArray;

  TCardinalArray = array [Word] of Cardinal;
  PCardinalArray = ^TCardinalArray;

// 1777
function SamFreeMemory(Buffer: Pointer): NTSTATUS; stdcall; external samlib;

// 1799
function SamCloseHandle(SamHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 1805
function SamConnect(ServerName: PUNICODE_STRING; out ServerHandle: TSamHandle;
  DesiredAccess: TAccessMask; const ObjectAttributes: TObjectAttributes):
  NTSTATUS; stdcall; external samlib;

// 1820
function SamLookupDomainInSamServer(ServerHandle: TSamHandle;
  const Name: UNICODE_STRING; out DomainId: PSid): NTSTATUS; stdcall;
  external samlib;

// 1828
function SamEnumerateDomainsInSamServer(ServerHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1838
function SamOpenDomain(ServerHandle: TSamHandle; DesiredAccess: TAccessMask;
  DomainId: PSid; out DomainHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 1874
function SamEnumerateGroupsInDomain(DomainHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1906
function SamEnumerateUsersInDomain(DomainHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle; UserAccountControl: Cardinal;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1929
function SamEnumerateAliasesInDomain(DomainHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1967
function SamOpenGroup(DomainHandle: TSamHandle; DesiredAccess: TAccessMask;
  GroupId: Cardinal; out GroupHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2013
function SamGetMembersInGroup(GroupHandle: TSamHandle;
  out MemberIds: PCardinalArray; out Attributes: PCardinalArray;
  out MemberCount: Cardinal): NTSTATUS; stdcall; external samlib;

// 2030
function SamOpenAlias(DomainHandle: TSamHandle; DesiredAccess: TAccessMask;
  AliasId: Cardinal; out AliasHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2098
function SamGetMembersInAlias(AliasHandle: TSamHandle; out MemberIds: PSidArray;
  out MemberCount: Cardinal): NTSTATUS; stdcall; external samlib;

// 2106
function SamOpenUser(DomainHandle: TSamHandle; DesiredAccess: TAccessMask;
  UserId: Cardinal; out UserHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2198
function SamRidToSid(ObjectHandle: TSamHandle; Rid: Cardinal;
  out Sid: PSid): NTSTATUS; stdcall; external samlib;

implementation

end.
