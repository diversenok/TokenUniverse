unit NtUtils.Job;

interface

uses
  Winapi.WinNt, Ntapi.ntpsapi, NtUtils.Exceptions;

// Create new job object
function NtxCreateJob(out hJob: THandle; ObjectName: String = '';
  RootDirectory: THandle = 0; HandleAttributes: Cardinal = 0): TNtxStatus;

// Open job object by name
function NtxOpenJob(out hJob: THandle; DesiredAccess: TAccessMask;
  ObjectName: String; RootDirectory: THandle = 0;
  HandleAttributes: Cardinal = 0): TNtxStatus;

// Enumerate active processes in a job
function NtxEnurateProcessesInJob(hJob: THandle;
  out ProcessIds: TArray<NativeUInt>): TNtxStatus;

type
  NtxJob = class
    // Query fixed-size information
    class function Query<T>(hJob: THandle;
      InfoClass: TJobObjectInfoClass; out Buffer: T): TNtxStatus; static;

    // Set fixed-size information
    class function SetInfo<T>(hJob: THandle;
      InfoClass: TJobObjectInfoClass; const Buffer: T): TNtxStatus; static;
  end;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntseapi;

function NtxCreateJob(out hJob: THandle; ObjectName: String;
  RootDirectory: THandle; HandleAttributes: Cardinal): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  NameStr: UNICODE_STRING;
begin
  if ObjectName <> '' then
  begin
    NameStr.FromString(ObjectName);
    InitializeObjectAttributes(ObjAttr, @NameStr, HandleAttributes,
      RootDirectory);
  end
  else
    InitializeObjectAttributes(ObjAttr, nil, HandleAttributes);

  Result.Location := 'NtCreateJobObject';
  Result.Status := NtCreateJobObject(hJob, JOB_OBJECT_ALL_ACCESS, @ObjAttr);
end;

function NtxOpenJob(out hJob: THandle; DesiredAccess: TAccessMask;
  ObjectName: String; RootDirectory: THandle; HandleAttributes: Cardinal):
  TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  NameStr: UNICODE_STRING;
begin
  NameStr.FromString(ObjectName);
  InitializeObjectAttributes(ObjAttr, @NameStr, HandleAttributes,
    RootDirectory);

  Result.Location := 'NtOpenJobObject';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objNtJob;

  Result.Status := NtOpenJobObject(hJob, DesiredAccess, ObjAttr);
end;

function NtxEnurateProcessesInJob(hJob: THandle;
  out ProcessIds: TArray<NativeUInt>): TNtxStatus;
const
  INITIAL_CAPACITY = 8;
var
  BufferSize, Required: Cardinal;
  Buffer: PJobBasicProcessIdList;
  i: Integer;
begin
  Result.Location := 'NtQueryInformationJobObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(JobObjectBasicProcessIdList);
  Result.LastCall.InfoClassType := TypeInfo(TJobObjectInfoClass);
  Result.LastCall.Expects(JOB_OBJECT_QUERY, objNtJob);

  // Initial buffer capacity should be enough for at least one item.
  BufferSize := SizeOf(Cardinal) * 2 + SizeOf(NativeUInt) * INITIAL_CAPACITY;

  repeat
    // Allocate a buffer for MaxCount items
    Buffer := AllocMem(BufferSize);

    Required := 0;
    Result.Status := NtQueryInformationJobObject(hJob,
      JobObjectBasicProcessIdList, Buffer, BufferSize, nil);

    // If not all processes fit into the list then calculate the required size
    if Result.Status = STATUS_BUFFER_OVERFLOW then
       Required := SizeOf(Cardinal) * 2 +
         SizeOf(NativeUInt) * Buffer.NumberOfAssignedProcesses;

    if not Result.IsSuccess then
      FreeMem(Buffer);

  until not NtxExpandBuffer(Result, BufferSize, Required, True);

  if not Result.IsSuccess then
    Exit;

  SetLength(ProcessIds, Buffer.NumberOfProcessIdsInList);

  for i := 0 to High(ProcessIds) do
    ProcessIds[i] := Buffer.ProcessIdList{$R-}[i]{$R+};

  FreeMem(Buffer);
end;

class function NtxJob.Query<T>(hJob: THandle; InfoClass: TJobObjectInfoClass;
  out Buffer: T): TNtxStatus;
begin
  Result.Location := 'NtQueryInformationJobObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TJobObjectInfoClass);
  Result.LastCall.Expects(JOB_OBJECT_QUERY, objNtJob);

  Result.Status := NtQueryInformationJobObject(hJob, InfoClass, @Buffer,
    SizeOf(Buffer), nil);
end;

class function NtxJob.SetInfo<T>(hJob: THandle; InfoClass: TJobObjectInfoClass;
  const Buffer: T): TNtxStatus;
begin
  Result.Location := 'NtSetInformationJobObject';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TJobObjectInfoClass);

  case InfoClass of
    JobObjectBasicLimitInformation, JobObjectExtendedLimitInformation:
      Result.LastCall.ExpectedPrivilege := SE_INCREASE_BASE_PRIORITY_PRIVILEGE;

    JobObjectSecurityLimitInformation:
      Result.LastCall.ExpectedPrivilege := SE_ASSIGN_PRIMARY_TOKEN_PRIVILEGE;
  end;

  case InfoClass of
    JobObjectSecurityLimitInformation:
      Result.LastCall.Expects(JOB_OBJECT_SET_SECURITY_ATTRIBUTES, objNtJob);
  else
    Result.LastCall.Expects(JOB_OBJECT_SET_ATTRIBUTES, objNtJob);
  end;

  Result.Status := NtSetInformationJobObject(hJob, InfoClass, @Buffer,
    SizeOf(Buffer));
end;

end.
