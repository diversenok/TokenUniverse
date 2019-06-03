unit NtUtils.AccessMasks;

interface

uses
  Winapi.WinNt;

type
  TAccessMaskType = (objProcess, objThread, objToken);

function FormatAccess(Access: TAccessMask; MaskType: TAccessMaskType): String;
function FormatAccessDetailed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;

implementation

uses
  DelphiUtils.Strings, Ntapi.ntpsapi, Ntapi.ntseapi;

const
  AccessStandard: array [0..4] of TFlagName = (
    (Value: READ_CONTROL; Name: 'Read permissions'),
    (Value: WRITE_DAC;    Name: 'Write permissions'),
    (Value: WRITE_OWNER;  Name: 'Write owner'),
    (Value: SYNCHRONIZE;  Name: 'Synchronize'),
    (Value: _DELETE;      Name: 'Delete')
  );

  AccessProcess: array [0..12] of TFlagName = (
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

  AccessThread: array [0..11] of TFlagName = (
    (Value: THREAD_TERMINATE;                 Name: 'Terminate'),
    (Value: THREAD_SUSPEND_RESUME;            Name: 'Suspend/resume'),
    (Value: THREAD_ALERT;                     Name: 'Alert'),
    (Value: THREAD_GET_CONTEXT;               Name: 'Get context'),
    (Value: THREAD_SET_CONTEXT;               Name: 'Set context'),
    (Value: THREAD_SET_INFORMATION;           Name: 'Set information'),
    (Value: THREAD_QUERY_INFORMATION;         Name: 'Query information'),
    (Value: THREAD_SET_THREAD_TOKEN;          Name: 'Set token'),
    (Value: THREAD_IMPERSONATE;               Name: 'Write direct impersonation'),
    (Value: THREAD_DIRECT_IMPERSONATION;      Name: 'Read direct impersonation'),
    (Value: THREAD_SET_LIMITED_INFORMATION;   Name: 'Set limited information'),
    (Value: THREAD_QUERY_LIMITED_INFORMATION; Name: 'Query limited information')
  );

  AccessToken: array [0..8] of TFlagName = (
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

  FullAccessByType: array [TAccessMaskType] of Cardinal = (
    PROCESS_ALL_ACCESS, THREAD_ALL_ACCESS, TOKEN_ALL_ACCESS
  );

function FormatAccess(Access: TAccessMask; MaskType: TAccessMaskType): String;
begin
  if Access = 0 then
    Exit('No access');

  if Access = FullAccessByType[MaskType] then
    Exit('Full access');

  case MaskType of
    objProcess:
      Result := MapFlags(Access, AccessProcess);

    objThread:
      Result := MapFlags(Access, AccessThread);

    objToken:
      Result := MapFlags(Access, AccessToken);
  else
    Result := IntToHexEx(Access);
  end;
end;

function FormatAccessDetailed(Access: TAccessMask;
  MaskType: TAccessMaskType): String;
begin
  Result := FormatAccess(Access, MaskType) + '(' + IntToHexEx(Access, 6) + ')';
end;

end.
