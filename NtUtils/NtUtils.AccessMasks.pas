unit NtUtils.AccessMasks;

interface

uses
  Winapi.WinNt;

type
  TAccessMaskType = (objProcess, objThread, objToken, objPolicy, objAccount);

function FormatAccess(Access: TAccessMask; MaskType: TAccessMaskType): String;
function FormatAccessPrefixed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;

implementation

uses
  DelphiUtils.Strings, Ntapi.ntpsapi, Ntapi.ntseapi, Winapi.ntlsa;

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

  SpecificAccessProcess: array [0..13] of TFlagName = (
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

  SpecificAccessThread: array [0..12] of TFlagName = (
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

  SpecificAccessToken: array [0..8] of TFlagName = (
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

  SpecificAccessPolicy: array [0..12] of TFlagName = (
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

  SpecificAccessAccount: array [0..3] of TFlagName = (
    (Value: ACCOUNT_VIEW;                 Name: 'View'),
    (Value: ACCOUNT_ADJUST_PRIVILEGES;    Name: 'Adjust privileges'),
    (Value: ACCOUNT_ADJUST_QUOTAS;        Name: 'Adjust quotas'),
    (Value: ACCOUNT_ADJUST_SYSTEM_ACCESS; Name: 'Adjust system access')
  );

  FullAccessForType: array [TAccessMaskType] of Cardinal = (
    PROCESS_ALL_ACCESS, THREAD_ALL_ACCESS, TOKEN_ALL_ACCESS, POLICY_ALL_ACCESS,
    ACCOUNT_ALL_ACCESS
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
    objProcess:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessProcess));
      ExcludeFlags(Access, SpecificAccessProcess);
    end;

    objThread:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessThread));
      ExcludeFlags(Access, SpecificAccessThread);
    end;

    objToken:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessToken));
      ExcludeFlags(Access, SpecificAccessToken);
    end;

    objPolicy:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessPolicy));
      ExcludeFlags(Access, SpecificAccessPolicy);
    end;

    objAccount:
    begin
      ConcatFlags(Result, MapFlags(Access, SpecificAccessAccount));
      ExcludeFlags(Access, SpecificAccessAccount);
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
      ConcatFlags(Result, IntToHexEx(1 shl i, 8));
end;

function FormatAccessPrefixed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;
begin
  Result := IntToHexEx(Access, 6) + ' (' + FormatAccess(Access, MaskType) + ')';
end;

end.
