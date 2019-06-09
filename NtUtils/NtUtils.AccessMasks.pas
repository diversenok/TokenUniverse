unit NtUtils.AccessMasks;

interface

uses
  Winapi.WinNt;

type
  TAccessMaskType = (objProcess, objThread, objToken);

function FormatAccess(Access: TAccessMask; MaskType: TAccessMaskType): String;
function FormatAccessPrefixed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;

implementation

uses
  DelphiUtils.Strings, Ntapi.ntpsapi, Ntapi.ntseapi;

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

  SpecificAccessProcess: array [0..12] of TFlagName = (
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
    (Value: PROCESS_QUERY_LIMITED_INFORMATION; Name: 'Query limited information')
  );

  SpecificAccessThread: array [0..11] of TFlagName = (
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
    (Value: THREAD_QUERY_LIMITED_INFORMATION; Name: 'Query limited information')
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

  FullAccessForType: array [TAccessMaskType] of Cardinal = (
    PROCESS_ALL_ACCESS, THREAD_ALL_ACCESS, TOKEN_ALL_ACCESS
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
  end;

  // Map and exclude standard, generic, and other access rights
  ConcatFlags(Result, MapFlags(Access, NonSpecificAccess));
  ExcludeFlags(Access, NonSpecificAccess);

  // Map unknown and reserved bits as hex values
  if Access <> 0 then
    for i := 0 to 31 do
      if Contains(Access, 1 shl i) then
        ConcatFlags(Result, IntToHexEx(1 shl i, 8));
end;

function FormatAccessPrefixed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;
begin
  Result := IntToHexEx(Access, 8) + ' (' + FormatAccess(Access, MaskType) + ')';
end;

end.
