unit Ntapi.ntsam;

{$MINENUMSIZE 4}

interface

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

  SAM_SERVER_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $3F;

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

  DOMAIN_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $7FF;

  // 352, password properties
  DOMAIN_PASSWORD_COMPLEX = $00000001;
  DOMAIN_PASSWORD_NO_ANON_CHANGE = $00000002;
  DOMAIN_PASSWORD_NO_CLEAR_CHANGE = $00000004;
  DOMAIN_LOCKOUT_ADMINS = $00000008;
  DOMAIN_PASSWORD_STORE_CLEARTEXT = $00000010;
  DOMAIN_REFUSE_PASSWORD_CHANGE = $00000020;
  DOMAIN_NO_LM_OWF_CHANGE = $00000040;

  // 528
  GROUP_READ_INFORMATION = $0001;
  GROUP_WRITE_ACCOUNT = $0002;
  GROUP_ADD_MEMBER = $0004;
  GROUP_REMOVE_MEMBER = $0008;
  GROUP_LIST_MEMBERS = $0010;

  GROUP_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1F;

  // 604
  ALIAS_ADD_MEMBER = $0001;
  ALIAS_REMOVE_MEMBER = $0002;
  ALIAS_LIST_MEMBERS = $0004;
  ALIAS_READ_INFORMATION = $0008;
  ALIAS_WRITE_ACCOUNT = $0010;

  ALIAS_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1F;

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

  USER_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $7FF;

  // 761, user control flags
  USER_ACCOUNT_DISABLED = $00000001;
  USER_HOME_DIRECTORY_REQUIRED = $00000002;
  USER_PASSWORD_NOT_REQUIRED = $00000004;
  USER_TEMP_DUPLICATE_ACCOUNT = $00000008;
  USER_NORMAL_ACCOUNT = $00000010;
  USER_MNS_LOGON_ACCOUNT = $00000020;
  USER_INTERDOMAIN_TRUST_ACCOUNT = $00000040;
  USER_WORKSTATION_TRUST_ACCOUNT = $00000080;
  USER_SERVER_TRUST_ACCOUNT = $00000100;
  USER_DONT_EXPIRE_PASSWORD = $00000200;
  USER_ACCOUNT_AUTO_LOCKED = $00000400;
  USER_ENCRYPTED_TEXT_PASSWORD_ALLOWED = $00000800;
  USER_SMARTCARD_REQUIRED = $00001000;
  USER_TRUSTED_FOR_DELEGATION = $00002000;
  USER_NOT_DELEGATED = $00004000;
  USER_USE_DES_KEY_ONLY = $00008000;
  USER_DONT_REQUIRE_PREAUTH = $00010000;
  USER_PASSWORD_EXPIRED = $00020000;
  USER_TRUSTED_TO_AUTHENTICATE_FOR_DELEGATION = $00040000;
  USER_NO_AUTH_DATA_REQUIRED = $00080000;
  USER_PARTIAL_SECRETS_ACCOUNT = $00100000;
  USER_USE_AES_KEYS = $00200000;

type
  TSamHandle = NativeUInt;

  TSamEnumerationHandle = Cardinal;

  // 77
  TSamRidEnumeration = record
    RelativeId: Cardinal;
    Name: UNICODE_STRING;
  end;
  PSamRidEnumeration = ^TSamRidEnumeration;

  TSamRidEnumerationArray = array [ANYSIZE_ARRAY] of TSamRidEnumeration;
  PSamRidEnumerationArray = ^TSamRidEnumerationArray;

  // 82
  TSamSidEnumeration = record
    Sid: PSid;
    Name: UNICODE_STRING;
  end;
  PSamSidEnumeration = ^TSamSidEnumeration;

  TSamSidEnumerationArray = array [ANYSIZE_ARRAY] of TSamSidEnumeration;
  PSamSidEnumerationArray = ^TSamSidEnumerationArray;

  TCardinalArray = array [ANYSIZE_ARRAY] of Cardinal;
  PCardinalArray = ^TCardinalArray;

  // 263
  TDomainInformationClass = (
    DomainEnumPadding = 0,
    DomainPasswordInformation = 1,    // q, s: TDomainPasswordInformation
    DomainGeneralInformation = 2,     // q: TDomainGeneralInformation
    DomainLogoffInformation = 3,      // q, s: TLargeInteger
    DomainOemInformation = 4,         // q, s: UNICODE_STRING
    DomainNameInformation = 5,        // q: UNICODE_STRING
    DomainReplicationInformation = 6, // q, s: UNICODE_STRING
    DomainServerRoleInformation = 7,  // q, s: TDomainServerRole
    DomainModifiedInformation = 8,    // q: TDomainModifiedInformation
    DomainStateInformation = 9,       // q, s: TDomainServerEnableState
    DomainUasInformation = 10,        // q, s: Boolean
    DomainGeneralInformation2 = 11,   // q:
    DomainLockoutInformation = 12,    // q, s:
    DomainModifiedInformation2 = 13   // q:
  );

  // 279
  TDomainServerEnableState = (
    DomainServerEnabled = 1,
    DomainServerDisabled
  );

  // 284
  TDomainServerRole = (
    DomainServerRoleBackup = 2,
    DomainServerRolePrimary
  );

  // 290
  TDomainGeneralInformation = record
    ForceLogoff: TLargeInteger;
    OemInformation: UNICODE_STRING;
    DomainName: UNICODE_STRING;
    ReplicaSourceNodeName: UNICODE_STRING;
    DomainModifiedCount: Int64;
    DomainServerState: TDomainServerEnableState;
    DomainServerRole: TDomainServerRole;
    UasCompatibilityRequired: Boolean;
    UserCount: Cardinal;
    GroupCount: Cardinal;
    AliasCount: Cardinal;
  end;
  PDomainGeneralInformation = ^TDomainGeneralInformation;

  // 333
  TDomainPasswordInformation = record
    MinPasswordLength: Word;
    PasswordHistoryLength: Word;
    PasswordProperties: Cardinal;
    MaxPasswordAge: TLargeInteger;
    MinPasswordAge: TLargeInteger;
  end;
  PDomainPasswordInformation = TDomainPasswordInformation;

  // 394
  TDomainModifiedInformation = record
    DomainModifiedCount: Int64;
    CreationTime: TLargeInteger;
  end;
  PDomainModifiedInformation = ^TDomainModifiedInformation;

  // 559
  TGroupMembership = record
    RelativeId: Cardinal;
    Attributes: Cardinal;
  end;
  PGroupMembership = ^TGroupMembership;

  PGroupMembershipArray = array [ANYSIZE_ARRAY] of PGroupMembership;

  // 565
  TGroupInformationClass = (
    GroupEnumPadding = 0,
    GroupGeneralInformation = 1,     // q: TGroupGeneralInformation
    GroupNameInformation = 2,        // q, s: UNICODE_STRING;
    GroupAttributeInformation = 3,   // q, s: Cardinal
    GroupAdminCommentInformation = 4 // q, s: UNICODE_STRING;
  );

  // 573
  TGroupGeneralInformation = record
    Name: UNICODE_STRING;
    Attributes: Cardinal;
    MemberCount: Cardinal;
    AdminComment: UNICODE_STRING;
  end;
  PGroupGeneralInformation = ^TGroupGeneralInformation;

  // 634
  TAliasInformationClass = (
    AliasEnumPadding = 0,
    AliasGeneralInformation = 1,      // q: TAliasGeneralInformation
    AliasNameInformation = 2,         // q, s: UNICODE_STRING
    AliasAdminCommentInformation = 3, // q, s: UNICODE_STRING
    AliasReplicationInformation = 4,  // q: UNICODE_STRING
    AliasExtendedInformation = 5      // q, s:
  );

  // 642
  TAliasGeneralInformation = record
    Name: UNICODE_STRING;
    MemberCount: Cardinal;
    AdminComment: UNICODE_STRING;
  end;
  PAliasGeneralInformation = ^TAliasGeneralInformation;

  // 829
  TLogonHours = record
    UnitsPerWeek: Word;
    LogonHours: PByte;
  end;

  // 860
  TUserInformationClass = (
    UserEnumPadding = 0,
    UserGeneralInformation = 1,       // q: TUserGeneralInformation
    UserPreferencesInformation = 2,   // q, s: TUserPreferencesInformation
    UserLogonInformation = 3,         // q: TUserLogonInformation
    UserLogonHoursInformation = 4,    // q, s: TLogonHours
    UserAccountInformation = 5,       // q: TUserAccountInformation
    UserNameInformation = 6,          // q, s: {Name + Full name}
    UserAccountNameInformation = 7,   // q, s: UNICODE_STRING
    UserFullNameInformation = 8,      // q, s: UNICODE_STRING
    UserPrimaryGroupInformation = 9,  // q, s: Cardinal
    UserHomeInformation = 10,         // q, s: TUserHomeInformation
    UserScriptInformation = 11,       // q, s: UNICODE_STRING
    UserProfileInformation = 12,      // q, s: UNICODE_STRING
    UserAdminCommentInformation = 13, // q, s: UNICODE_STRING
    UserWorkStationsInformation = 14, // q, s: UNICODE_STRING
    UserSetPasswordInformation = 15,  // s: TUserSetPasswordInformation
    UserControlInformation = 16,      // q, s: Cardinal
    UserExpiresInformation = 17,      // q, s: TLargeInteger
    UserInternal1Information = 18,    // q, s:
    UserInternal2Information = 19,    // q, s:
    UserParametersInformation = 20,   // q, s: UNICODE_STRING
    UserAllInformation = 21,          // q, s:
    UserInternal3Information = 22,    // q, s:
    UserInternal4Information = 23,    // s:
    UserInternal5Information = 24,    // s:
    UserInternal4InformationNew = 25, // s:
    UserInternal5InformationNew = 26, // s:
    UserInternal6Information = 27,    // q, s:
    UserExtendedInformation = 28,     // q, s:
    UserLogonUIInformation = 29       // q: TUserLogonUiInformation
  );

  // 1105
  TUserGeneralInformation = record
    UserName: UNICODE_STRING;
    FullName: UNICODE_STRING;
    PrimaryGroupId: Cardinal;
    AdminComment: UNICODE_STRING;
    UserComment: UNICODE_STRING;
  end;
  PUserGeneralInformation = ^TUserGeneralInformation;

  // 1113
  TUserPreferencesInformation = record
    UserComment: UNICODE_STRING;
    Reserved1: UNICODE_STRING;
    CountryCode: Word;
    CodePage: Word;
  end;
  PUserPreferencesInformation = ^TUserPreferencesInformation;

  // 1125
  TUserLogonInformation = packed record
    UserName: UNICODE_STRING;
    FullName: UNICODE_STRING;
    UserId: Cardinal;
    PrimaryGroupId: Cardinal;
    HomeDirectory: UNICODE_STRING;
    HomeDirectoryDrive: UNICODE_STRING;
    ScriptPath: UNICODE_STRING;
    ProfilePath: UNICODE_STRING;
    WorkStations: UNICODE_STRING;
    LastLogon: TLargeInteger;
    LastLogoff: TLargeInteger;
    PasswordLastSet: TLargeInteger;
    PasswordCanChange: TLargeInteger;
    PasswordMustChange: TLargeInteger;
    LogonHours: TLogonHours;
    BadPasswordCount: Word;
    LogonCount: Word;
    UserAccountControl: Cardinal;
  end;
  PUserLogonInformation = ^TUserLogonInformation;

  // 1148
  TUserAccountInformation = packed record
    UserName: UNICODE_STRING;
    FullName: UNICODE_STRING;
    UserId: Cardinal;
    PrimaryGroupId: Cardinal;
    HomeDirectory: UNICODE_STRING;
    HomeDirectoryDrive: UNICODE_STRING;
    ScriptPath: UNICODE_STRING;
    ProfilePath: UNICODE_STRING;
    AdminComment: UNICODE_STRING;
    WorkStations: UNICODE_STRING;
    LastLogon: TLargeInteger;
    LastLogoff: TLargeInteger;
    LogonHours: TLogonHours;
    BadPasswordCount: Word;
    LogonCount: Word;
    PasswordLastSet: TLargeInteger;
    AccountExpires: TLargeInteger;
    UserAccountControl: Cardinal;
  end;
  PUserAccountInformation = ^TUserAccountInformation;

  // 1187
  TUserHomeInformation = record
    HomeDirectory: UNICODE_STRING;
    HomeDirectoryDrive: UNICODE_STRING;
  end;
  PUserHomeInformation = ^TUserHomeInformation;

  // 1208
  TUserSetPasswordInformation = record
    Password: UNICODE_STRING;
    PasswordExpired: Boolean;
  end;
  PUserSetPasswordInformation = ^TUserSetPasswordInformation;

  // 1249
  TUserLogonUiInformation = record
    PasswordIsBlank: Boolean;
    AccountIsDisabled: Boolean;
  end;
  PUserLogonUiInformation = ^TUserLogonUiInformation;

// 1777
function SamFreeMemory(Buffer: Pointer): NTSTATUS; stdcall;
  external samlib; overload;
function SamFreeMemory(Buffer: PGroupMembershipArray): NTSTATUS; stdcall;
  external samlib; overload;

// 1784
function SamSetSecurityObject(ObjectHandle: TSamHandle;
  SecurityInformation: TSecurityInformation; const SecurityDescriptor:
  TSecurityDescriptor): NTSTATUS; stdcall; external samlib;

// 1792
function SamQuerySecurityObject(ObjectHandle: TSamHandle;
  SecurityInformation: TSamHandle; out SecurityDescriptor: PSecurityDescriptor):
  NTSTATUS; stdcall; external samlib;

// 1799
function SamCloseHandle(SamHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 1805
function SamConnect(ServerName: PUNICODE_STRING; out ServerHandle: TSamHandle;
  DesiredAccess: TAccessMask; const ObjectAttributes: TObjectAttributes):
  NTSTATUS; stdcall; external samlib;

// 1814
function SamShutdownSamServer(ServerHandle: TSamHandle):
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

// 1847
function SamQueryInformationDomain(DomainHandle: TSamHandle;
  DomainInformationClass: TDomainInformationClass; out Buffer: Pointer):
  NTSTATUS; stdcall; external samlib;

// 1855
function SamSetInformationDomain(DomainHandle: TSamHandle;
  DomainInformationClass: TDomainInformationClass; DomainInformation: Pointer):
  NTSTATUS; stdcall; external samlib;

// 1863
function SamCreateGroupInDomain(DomainHandle: TSamHandle; const AccountName:
  UNICODE_STRING; DesiredAccess: TAccessMask; out GroupHandle: TSamHandle;
  out RelativeId: Cardinal): NTSTATUS; stdcall; external samlib;

// 1874
function SamEnumerateGroupsInDomain(DomainHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1884
function SamCreateUser2InDomain(DomainHandle: TSamHandle; const AccountName:
  UNICODE_STRING; AccountType: Cardinal; DesiredAccess: TAccessMask;
  out UserHandle: TSamHandle; out GrantedAccess: TAccessMask;
  out RelativeId: Cardinal): NTSTATUS; stdcall; external samlib;

// 1906
function SamEnumerateUsersInDomain(DomainHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle; UserAccountControl: Cardinal;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1917
function SamCreateAliasInDomain(DomainHandle: TSamHandle; const AccountName:
  UNICODE_STRING; DesiredAccess: TAccessMask; out AliasHandle: TSamHandle;
  out RelativeId: Cardinal): NTSTATUS; stdcall; external samlib;

// 1927
function SamEnumerateAliasesInDomain(DomainHandle: TSamHandle;
  var EnumerationContext: TSamEnumerationHandle;
  out Buffer: PSamRidEnumerationArray; PreferedMaximumLength: Integer;
  out CountReturned: Integer): NTSTATUS; stdcall; external samlib;

// 1967
function SamOpenGroup(DomainHandle: TSamHandle; DesiredAccess: TAccessMask;
  GroupId: Cardinal; out GroupHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 1976
function SamQueryInformationGroup(GroupHandle: TSamHandle;
  GroupInformationClass: TGroupInformationClass;
  out Buffer: Pointer): NTSTATUS; stdcall; external samlib;

// 1984
function SamSetInformationGroup(GroupHandle: TSamHandle;
  GroupInformationClass: TGroupInformationClass; Buffer: Pointer): NTSTATUS;
  stdcall; external samlib;

// 1992
function SamAddMemberToGroup(GroupHandle: TSamHandle; MemberId: Cardinal;
  Attributes: Cardinal): NTSTATUS; stdcall; external samlib;

// 2000
function SamDeleteGroup(GroupHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2006
function SamRemoveMemberFromGroup(GroupHandle: TSamHandle; MemberId: Cardinal):
  NTSTATUS; stdcall; external samlib;

// 2013
function SamGetMembersInGroup(GroupHandle: TSamHandle;
  out MemberIds: PCardinalArray; out Attributes: PCardinalArray;
  out MemberCount: Integer): NTSTATUS; stdcall; external samlib;

// 2022
function SamSetMemberAttributesOfGroup(GroupHandle: TSamHandle;
  MemberId: Cardinal; Attributes: Cardinal): NTSTATUS; stdcall; external samlib;

// 2030
function SamOpenAlias(DomainHandle: TSamHandle; DesiredAccess: TAccessMask;
  AliasId: Cardinal; out AliasHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2039
function SamQueryInformationAlias(AliasHandle: TSamHandle;
  AliasInformationClass: TAliasInformationClass; out Buffer: Pointer): NTSTATUS;
  stdcall; external samlib;

// 2047
function SamSetInformationAlias(AliasHandle: TSamHandle;
  AliasInformationClass: TAliasInformationClass; Buffer: Pointer): NTSTATUS;
  stdcall; external samlib;

// 2055
function SamDeleteAlias(AliasHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2061
function SamAddMemberToAlias(AliasHandle: TSamHandle; MemberId: PSid): NTSTATUS;
  stdcall; external samlib;

// 2068
function SamAddMultipleMembersToAlias(AliasHandle: TSamHandle; MemberIds:
  TArray<PSid>; MemberCount: Cardinal): NTSTATUS; stdcall; external samlib;

// 2076
function SamRemoveMemberFromAlias(AliasHandle: TSamHandle; MemberId: PSid):
  NTSTATUS; stdcall; external samlib;

// 2083
function SamRemoveMultipleMembersFromAlias(AliasHandle: TSamHandle; MemberIds:
  TArray<PSid>; MemberCount: Cardinal): NTSTATUS; stdcall; external samlib;

// 2098
function SamGetMembersInAlias(AliasHandle: TSamHandle; out MemberIds: PSidArray;
  out MemberCount: Integer): NTSTATUS; stdcall; external samlib;

// 2106
function SamOpenUser(DomainHandle: TSamHandle; DesiredAccess: TAccessMask;
  UserId: Cardinal; out UserHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2115
function SamDeleteUser(UserHandle: TSamHandle): NTSTATUS; stdcall;
  external samlib;

// 2121
function SamQueryInformationUser(UserHandle: TSamHandle;
  UserInformationClass: TUserInformationClass; out Buffer: Pointer): NTSTATUS;
  stdcall; external samlib;

// 2129
function SamSetInformationUser(UserHandle: TSamHandle;
  UserInformationClass: TUserInformationClass; Buffer: Pointer): NTSTATUS;
  stdcall; external samlib;

// 2137
function SamChangePasswordUser(UserHandle: TSamHandle; const OldPassword:
  UNICODE_STRING; const NewPassword: UNICODE_STRING): NTSTATUS;
  stdcall; external samlib;

// 2167
function SamGetGroupsForUser(UserHandle: TSamHandle; out Groups:
  PGroupMembershipArray; out MembershipCount: Integer): NTSTATUS;
  stdcall; external samlib;

// 2198
function SamRidToSid(ObjectHandle: TSamHandle; Rid: Cardinal;
  out Sid: PSid): NTSTATUS; stdcall; external samlib;

implementation

end.
