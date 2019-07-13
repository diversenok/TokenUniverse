unit NtUtils.AccessMasks;

interface

uses
  Winapi.WinNt;

type
  TAccessMaskType = (objNone, objNtProcess, objNtThread, objNtJob, objNtToken,
    objUsrDesttop, objUsrWindowStation, objLsaPolicy, objLsaAccount,
    objScmManager, objScmService, objSamServer, objSamDomain, objSamGroup,
    objSamAlias, objSamUser);

function FormatAccess(Access: TAccessMask; MaskType: TAccessMaskType): String;
function FormatAccessPrefixed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;

implementation

uses
  DelphiUtils.Strings, Ntapi.ntpsapi, Ntapi.ntseapi, Winapi.ntlsa, Winapi.Svc,
  Ntapi.ntsam, Winapi.WinUser;

const
  NonSpecificAccess: array [0..10] of TFlagName = (
    (Value: READ_CONTROL;           Name: 'Read permissions'),
    (Value: WRITE_DAC;              Name: 'Write permissions'),
    (Value: WRITE_OWNER;            Name: 'Write owner'),
    (Value: SYNCHRONIZE;            Name: 'Synchronize'),
    (Value: _DELETE;                Name: 'Delete'),
    (Value: ACCESS_SYSTEM_SECURITY; Name: 'System security'),
    (Value: MAXIMUM_ALLOWED;        Name: 'Maximum allowed'),
    (Value: GENERIC_READ;           Name: 'Generic read'),
    (Value: GENERIC_WRITE;          Name: 'Generic write'),
    (Value: GENERIC_EXECUTE;        Name: 'Generic execute'),
    (Value: GENERIC_ALL;            Name: 'Generic all')
  );

  SpecificAccessNtProcess: array [0..13] of TFlagName = (
    (Value: PROCESS_TERMINATE;                 Name: 'Terminate'),
    (Value: PROCESS_CREATE_THREAD;             Name: 'Create threads'),
    (Value: PROCESS_SET_SESSIONID;             Name: 'Set session ID'),
    (Value: PROCESS_VM_OPERATION;              Name: 'Modify memory'),
    (Value: PROCESS_VM_READ;                   Name: 'Read memory'),
    (Value: PROCESS_VM_WRITE;                  Name: 'Write memory'),
    (Value: PROCESS_DUP_HANDLE;                Name: 'Duplicate handles'),
    (Value: PROCESS_CREATE_PROCESS;            Name: 'Create process'),
    (Value: PROCESS_SET_QUOTA;                 Name: 'Set quota'),
    (Value: PROCESS_SET_INFORMATION;           Name: 'Set information'),
    (Value: PROCESS_QUERY_INFORMATION;         Name: 'Query information'),
    (Value: PROCESS_SUSPEND_RESUME;            Name: 'Suspend/resume'),
    (Value: PROCESS_QUERY_LIMITED_INFORMATION; Name: 'Query limited information'),
    (Value: PROCESS_SET_LIMITED_INFORMATION;   Name: 'Set limited information')
  );

  SpecificAccessNtThread: array [0..12] of TFlagName = (
    (Value: THREAD_TERMINATE;                 Name: 'Terminate'),
    (Value: THREAD_SUSPEND_RESUME;            Name: 'Suspend/resume'),
    (Value: THREAD_ALERT;                     Name: 'Alert'),
    (Value: THREAD_GET_CONTEXT;               Name: 'Get context'),
    (Value: THREAD_SET_CONTEXT;               Name: 'Set context'),
    (Value: THREAD_SET_INFORMATION;           Name: 'Set information'),
    (Value: THREAD_QUERY_INFORMATION;         Name: 'Query information'),
    (Value: THREAD_SET_THREAD_TOKEN;          Name: 'Set token'),
    (Value: THREAD_IMPERSONATE;               Name: 'Impersonate'),
    (Value: THREAD_DIRECT_IMPERSONATION;      Name: 'Direct impersonation'),
    (Value: THREAD_SET_LIMITED_INFORMATION;   Name: 'Set limited information'),
    (Value: THREAD_QUERY_LIMITED_INFORMATION; Name: 'Query limited information'),
    (Value: THREAD_RESUME;                    Name: 'Resume')
  );

  SpecificAccessNtJob: array [0..5] of TFlagName = (
    (Value: JOB_OBJECT_ASSIGN_PROCESS;          Name: 'Assign process'),
    (Value: JOB_OBJECT_SET_ATTRIBUTES;          Name: 'Set attributes'),
    (Value: JOB_OBJECT_QUERY;                   Name: 'Query'),
    (Value: JOB_OBJECT_TERMINATE;               Name: 'Terminate'),
    (Value: JOB_OBJECT_SET_SECURITY_ATTRIBUTES; Name: 'Set security attributes'),
    (Value: JOB_OBJECT_IMPERSONATE;             Name: 'Impersonate')
  );

  SpecificAccessNtToken: array [0..8] of TFlagName = (
    (Value: TOKEN_DUPLICATE;         Name: 'Duplicate'),
    (Value: TOKEN_QUERY;             Name: 'Query'),
    (Value: TOKEN_QUERY_SOURCE;      Name: 'Query source'),
    (Value: TOKEN_IMPERSONATE;       Name: 'Impersonate'),
    (Value: TOKEN_ASSIGN_PRIMARY;    Name: 'Assign primary'),
    (Value: TOKEN_ADJUST_DEFAULT;    Name: 'Adjust defaults'),
    (Value: TOKEN_ADJUST_PRIVILEGES; Name: 'Adjust privileges'),
    (Value: TOKEN_ADJUST_GROUPS;     Name: 'Adjust groups'),
    (Value: TOKEN_ADJUST_SESSIONID;  Name: 'Adjust session ID')
  );

  SpecificAccessUsrDesktop: array [0..8] of TFlagName = (
    (Value: DESKTOP_READOBJECTS;     Name: 'Read objects'),
    (Value: DESKTOP_CREATEWINDOW;    Name: 'Create window'),
    (Value: DESKTOP_CREATEMENU;      Name: 'Create menu'),
    (Value: DESKTOP_HOOKCONTROL;     Name: 'Hook control'),
    (Value: DESKTOP_JOURNALRECORD;   Name: 'Journal record'),
    (Value: DESKTOP_JOURNALPLAYBACK; Name: 'Journal playback'),
    (Value: DESKTOP_ENUMERATE;       Name: 'Enumerate'),
    (Value: DESKTOP_WRITEOBJECTS;    Name: 'Write objects'),
    (Value: DESKTOP_SWITCHDESKTOP;   Name: 'Switch desktop')
  );

  SpecificAccessUsrWinsta: array [0..8] of TFlagName = (
    (Value: WINSTA_ENUMDESKTOPS;      Name: 'Enumerate desktops'),
    (Value: WINSTA_READATTRIBUTES;    Name: 'Read attributes'),
    (Value: WINSTA_ACCESSCLIPBOARD;   Name: 'Access clipboard'),
    (Value: WINSTA_CREATEDESKTOP;     Name: 'Create desktop'),
    (Value: WINSTA_WRITEATTRIBUTES;   Name: 'Write attributes'),
    (Value: WINSTA_ACCESSGLOBALATOMS; Name: 'Access global atoms'),
    (Value: WINSTA_EXITWINDOWS;       Name: 'Exit Windows'),
    (Value: WINSTA_ENUMERATE;         Name: 'Enumerate'),
    (Value: WINSTA_READSCREEN;        Name: 'Read screen')
  );

  SpecificAccessLsaPolicy: array [0..12] of TFlagName = (
    (Value: POLICY_VIEW_LOCAL_INFORMATION;   Name: 'View local information'),
    (Value: POLICY_VIEW_AUDIT_INFORMATION;   Name: 'View audit information'),
    (Value: POLICY_GET_PRIVATE_INFORMATION;  Name: 'Get private information'),
    (Value: POLICY_TRUST_ADMIN;              Name: 'Trust admin'),
    (Value: POLICY_CREATE_ACCOUNT;           Name: 'Create account'),
    (Value: POLICY_CREATE_SECRET;            Name: 'Create secret'),
    (Value: POLICY_CREATE_PRIVILEGE;         Name: 'Create privilege'),
    (Value: POLICY_SET_DEFAULT_QUOTA_LIMITS; Name: 'Set default quota'),
    (Value: POLICY_SET_AUDIT_REQUIREMENTS;   Name: 'Set audit requirements'),
    (Value: POLICY_AUDIT_LOG_ADMIN;          Name: 'Audit log admin'),
    (Value: POLICY_SERVER_ADMIN;             Name: 'Server admin'),
    (Value: POLICY_LOOKUP_NAMES;             Name: 'Lookup names'),
    (Value: POLICY_NOTIFICATION;             Name: 'Notification')
  );

  SpecificAccessLsaAccount: array [0..3] of TFlagName = (
    (Value: ACCOUNT_VIEW;                 Name: 'View'),
    (Value: ACCOUNT_ADJUST_PRIVILEGES;    Name: 'Adjust privileges'),
    (Value: ACCOUNT_ADJUST_QUOTAS;        Name: 'Adjust quotas'),
    (Value: ACCOUNT_ADJUST_SYSTEM_ACCESS; Name: 'Adjust system access')
  );

  SpecificAccessScmManager: array [0..5] of TFlagName = (
    (Value: SC_MANAGER_CONNECT;            Name: 'Connect'),
    (Value: SC_MANAGER_CREATE_SERVICE;     Name: 'Create service'),
    (Value: SC_MANAGER_ENUMERATE_SERVICE;  Name: 'Enumerate services'),
    (Value: SC_MANAGER_LOCK;               Name: 'Lock'),
    (Value: SC_MANAGER_QUERY_LOCK_STATUS;  Name: 'Query lock status'),
    (Value: SC_MANAGER_MODIFY_BOOT_CONFIG; Name: 'Modify boot config')
  );

  SpecificAccessScmService: array [0..8] of TFlagName = (
    (Value: SERVICE_QUERY_CONFIG;         Name: 'Query config'),
    (Value: SERVICE_CHANGE_CONFIG;        Name: 'Change config'),
    (Value: SERVICE_QUERY_STATUS;         Name: 'Query status'),
    (Value: SERVICE_ENUMERATE_DEPENDENTS; Name: 'Enumerate dependents'),
    (Value: SERVICE_START;                Name: 'Start'),
    (Value: SERVICE_STOP;                 Name: 'Stop'),
    (Value: SERVICE_PAUSE_CONTINUE;       Name: 'Pause/continue'),
    (Value: SERVICE_INTERROGATE;          Name: 'Interrogate'),
    (Value: SERVICE_USER_DEFINED_CONTROL; Name: 'User-defined control')
  );

  SpecificAccessSamServer: array [0..5] of TFlagName = (
    (Value: SAM_SERVER_CONNECT;           Name: 'Connect'),
    (Value: SAM_SERVER_SHUTDOWN;          Name: 'Shutdown'),
    (Value: SAM_SERVER_INITIALIZE;        Name: 'Initialize'),
    (Value: SAM_SERVER_CREATE_DOMAIN;     Name: 'Create domain'),
    (Value: SAM_SERVER_ENUMERATE_DOMAINS; Name: 'Enumerate domains'),
    (Value: SAM_SERVER_LOOKUP_DOMAIN;     Name: 'Lookup domain')
  );

  SpecificAccessSamDomain: array [0..10] of TFlagName = (
    (Value: DOMAIN_READ_PASSWORD_PARAMETERS; Name: 'Read password parameters'),
    (Value: DOMAIN_WRITE_PASSWORD_PARAMS;    Name: 'Write password parameters'),
    (Value: DOMAIN_READ_OTHER_PARAMETERS;    Name: 'Read other parameters'),
    (Value: DOMAIN_WRITE_OTHER_PARAMETERS;   Name: 'Write other parameters'),
    (Value: DOMAIN_CREATE_USER;              Name: 'Create user'),
    (Value: DOMAIN_CREATE_GROUP;             Name: 'Create group'),
    (Value: DOMAIN_CREATE_ALIAS;             Name: 'Create alias'),
    (Value: DOMAIN_GET_ALIAS_MEMBERSHIP;     Name: 'Get alias membership'),
    (Value: DOMAIN_LIST_ACCOUNTS;            Name: 'List accounts'),
    (Value: DOMAIN_LOOKUP;                   Name: 'Lookup'),
    (Value: DOMAIN_ADMINISTER_SERVER;        Name: 'Administer server')
  );

  SpecificAccessSamGroup: array [0..4] of TFlagName = (
    (Value: GROUP_READ_INFORMATION; Name: 'Read information'),
    (Value: GROUP_WRITE_ACCOUNT;    Name: 'Write account'),
    (Value: GROUP_ADD_MEMBER;       Name: 'Add member'),
    (Value: GROUP_REMOVE_MEMBER;    Name: 'Remove member'),
    (Value: GROUP_LIST_MEMBERS;     Name: 'List members')
  );

  SpecificAccessSamAlias: array [0..4] of TFlagName = (
    (Value: ALIAS_ADD_MEMBER;       Name: 'Add member'),
    (Value: ALIAS_REMOVE_MEMBER;    Name: 'Remove member'),
    (Value: ALIAS_LIST_MEMBERS;     Name: 'List members'),
    (Value: ALIAS_READ_INFORMATION; Name: 'Read information'),
    (Value: ALIAS_WRITE_ACCOUNT;    Name: 'Write account')
  );

  SpecificAccessSamUser: array [0..10] of TFlagName = (
    (Value: USER_READ_GENERAL;            Name: 'Read general'),
    (Value: USER_READ_PREFERENCES;        Name: 'Read preferences'),
    (Value: USER_WRITE_PREFERENCES;       Name: 'Write preferences'),
    (Value: USER_READ_LOGON;              Name: 'Read logon'),
    (Value: USER_READ_ACCOUNT;            Name: 'Read account'),
    (Value: USER_WRITE_ACCOUNT;           Name: 'Write account'),
    (Value: USER_CHANGE_PASSWORD;         Name: 'Change password'),
    (Value: USER_FORCE_PASSWORD_CHANGE;   Name: 'Force password change'),
    (Value: USER_LIST_GROUPS;             Name: 'List groups'),
    (Value: USER_READ_GROUP_INFORMATION;  Name: 'Read group information'),
    (Value: USER_WRITE_GROUP_INFORMATION; Name: 'Write group information')
  );

  FullAccessForType: array [TAccessMaskType] of Cardinal = (SPECIFIC_RIGHTS_ALL,
    PROCESS_ALL_ACCESS, THREAD_ALL_ACCESS, JOB_OBJECT_ALL_ACCESS,
    TOKEN_ALL_ACCESS, DESKTOP_ALL_ACCESS, WINSTA_ALL_ACCESS, POLICY_ALL_ACCESS,
    ACCOUNT_ALL_ACCESS, SC_MANAGER_ALL_ACCESS, SERVICE_ALL_ACCESS,
    SAM_SERVER_ALL_ACCESS, DOMAIN_ALL_ACCESS, GROUP_ALL_ACCESS,
    ALIAS_ALL_ACCESS, USER_ALL_ACCESS
  );

procedure ExcludeFlags(var Value: Cardinal; Mapping: array of TFlagName);
var
  i: Integer;
begin
  for i := Low(Mapping) to High(Mapping) do
    Value := Value and not Mapping[i].Value;
end;

procedure ConcatFlags(var Result: String; NewFlags: String);
begin
  if (Result <> '') and (NewFlags <> '') then
    Result := Result + ', ' + NewFlags
  else if NewFlags <> '' then
    Result := NewFlags;
end;

function FormatAccess(Access: TAccessMask; MaskType: TAccessMaskType): String;
var
  i: Integer;
begin
  if Access = 0 then
    Exit('No access');

  Result := '';

  // Map and exclude full access
  if Contains(Access, FullAccessForType[MaskType]) then
  begin
    Result := 'Full access';
    Access := Access and not FullAccessForType[MaskType];

    if Access = 0 then
      Exit;
  end;

  // Map and exclude type-specific access
  case MaskType of
    objNtProcess:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessNtProcess));
      ExcludeFlags(Access, SpecificAccessNtProcess);
    end;

    objNtThread:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessNtThread));
      ExcludeFlags(Access, SpecificAccessNtThread);
    end;

    objNtJob:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessNtJob));
      ExcludeFlags(Access, SpecificAccessNtJob);
    end;

    objNtToken:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessNtToken));
      ExcludeFlags(Access, SpecificAccessNtToken);
    end;

    objUsrDesttop:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessUsrDesktop));
      ExcludeFlags(Access, SpecificAccessUsrDesktop);
    end;

    objUsrWindowStation:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessUsrWinsta));
      ExcludeFlags(Access, SpecificAccessUsrWinsta);
    end;

    objLsaPolicy:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessLsaPolicy));
      ExcludeFlags(Access, SpecificAccessLsaPolicy);
    end;

    objLsaAccount:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessLsaAccount));
      ExcludeFlags(Access, SpecificAccessLsaAccount);
    end;

    objScmManager:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessScmManager));
      ExcludeFlags(Access, SpecificAccessScmManager);
    end;

    objScmService:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessScmService));
      ExcludeFlags(Access, SpecificAccessScmService);
    end;

    objSamServer:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessSamServer));
      ExcludeFlags(Access, SpecificAccessSamServer);
    end;

    objSamDomain:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessSamDomain));
      ExcludeFlags(Access, SpecificAccessSamDomain);
    end;

    objSamGroup:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessSamGroup));
      ExcludeFlags(Access, SpecificAccessSamGroup);
    end;

    objSamAlias:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessSamAlias));
      ExcludeFlags(Access, SpecificAccessSamAlias);
    end;

    objSamUser:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessSamUser));
      ExcludeFlags(Access, SpecificAccessSamUser);
    end;
  end;

  if Access = 0 then
    Exit;

  // Map and exclude standard, generic, and other access rights
  ConcatFlags(Result, MapFlags(Access, NonSpecificAccess));
  ExcludeFlags(Access, NonSpecificAccess);

  if Access = 0 then
    Exit;

  // Map unknown and reserved bits as hex values
  for i := 0 to 31 do
    if Contains(Access, 1 shl i) then
      ConcatFlags(Result, IntToHexEx(1 shl i, 6));
end;

function FormatAccessPrefixed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;
begin
  Result := IntToHexEx(Access, 6) + ' (' + FormatAccess(Access, MaskType) + ')';
end;

end.
