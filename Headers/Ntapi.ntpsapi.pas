unit Ntapi.ntpsapi;

{$WARN SYMBOL_PLATFORM OFF}
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntpebteb;

const
  PROCESS_TERMINATE = $0001;
  PROCESS_CREATE_THREAD = $0002;
  PROCESS_SET_SESSIONID = $0004;
  PROCESS_VM_OPERATION = $0008;
  PROCESS_VM_READ = $0010;
  PROCESS_VM_WRITE = $0020;
  PROCESS_DUP_HANDLE = $0040;
  PROCESS_CREATE_PROCESS = $0080;
  PROCESS_SET_QUOTA = $0100;
  PROCESS_SET_INFORMATION = $0200;
  PROCESS_QUERY_INFORMATION = $0400;
  PROCESS_SUSPEND_RESUME = $0800;
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
  PROCESS_SET_LIMITED_INFORMATION = $2000;

  PROCESS_ALL_ACCESS = STANDARD_RIGHTS_ALL or SPECIFIC_RIGHTS_ALL;

  THREAD_TERMINATE = $0001;
  THREAD_SUSPEND_RESUME = $0002;
  THREAD_ALERT = $0004;
  THREAD_GET_CONTEXT = $0008;
  THREAD_SET_CONTEXT = $0010;
  THREAD_SET_INFORMATION = $0020;
  THREAD_QUERY_INFORMATION = $0040;
  THREAD_SET_THREAD_TOKEN = $0080;
  THREAD_IMPERSONATE = $0100;
  THREAD_DIRECT_IMPERSONATION = $0200;
  THREAD_SET_LIMITED_INFORMATION = $0400;
  THREAD_QUERY_LIMITED_INFORMATION = $0800;
  THREAD_RESUME = $1000;

  THREAD_ALL_ACCESS = STANDARD_RIGHTS_ALL or SPECIFIC_RIGHTS_ALL;

  JOB_OBJECT_ASSIGN_PROCESS = $0001;
  JOB_OBJECT_SET_ATTRIBUTES = $0002;
  JOB_OBJECT_QUERY = $0004;
  JOB_OBJECT_TERMINATE = $0008;
  JOB_OBJECT_SET_SECURITY_ATTRIBUTES = $0010;
  JOB_OBJECT_IMPERSONATE = $0020;

  JOB_OBJECT_ALL_ACCESS = STANDARD_RIGHTS_ALL or $3F;

  JOB_OBJECT_UILIMIT_HANDLES = $00000001;
  JOB_OBJECT_UILIMIT_READCLIPBOARD = $00000002;
  JOB_OBJECT_UILIMIT_WRITECLIPBOARD = $00000004;
  JOB_OBJECT_UILIMIT_SYSTEMPARAMETERS = $00000008;
  JOB_OBJECT_UILIMIT_DISPLAYSETTINGS = $00000010;
  JOB_OBJECT_UILIMIT_GLOBALATOMS = $00000020;
  JOB_OBJECT_UILIMIT_DESKTOP = $00000040;
  JOB_OBJECT_UILIMIT_EXITWINDOWS = $00000080;

  JOB_OBJECT_TERMINATE_AT_END_OF_JOB = 0;
  JOB_OBJECT_POST_AT_END_OF_JOB = 1;

  NtCurrentProcess: THandle = THandle(-1);
  NtCurrentThread: THandle = THandle(-2);

  // Not NT, but useful
  function NtCurrentProcessId: NativeUInt;
  function NtCurrentThreadId: NativeUInt;

type
  TProcessInfoClass = (
    ProcessBasicInformation = 0,       // q: TProcessBasinInformation
    ProcessQuotaLimits = 1,            // q, s: TQuotaLimits
    ProcessIoCounters = 2,             // q: TIoCounters
    ProcessVmCounters = 3,
    ProcessTimes = 4,
    ProcessBasePriority = 5,           // s: KPRIORITY
    ProcessRaisePriority = 6,
    ProcessDebugPort = 7,
    ProcessExceptionPort = 8,
    ProcessAccessToken = 9,            // s: TProcessAccessToken
    ProcessLdtInformation = 10,
    ProcessLdtSize = 11,
    ProcessDefaultHardErrorMode = 12,
    ProcessIoPortHandlers = 13,
    ProcessPooledUsageAndLimits = 14,
    ProcessWorkingSetWatch = 15,
    ProcessUserModeIOPL = 16,
    ProcessEnableAlignmentFaultFixup = 17,
    ProcessPriorityClass = 18,
    ProcessWx86Information = 19,
    ProcessHandleCount = 20,
    ProcessAffinityMask = 21,
    ProcessPriorityBoost = 22,
    ProcessDeviceMap = 23,
    ProcessSessionInformation = 24,    // q: Cardinal
    ProcessForegroundInformation = 25,
    ProcessWow64Information = 26,      // q: NativeUInt
    ProcessImageFileName = 27,         // q: UNICODE_STRING
    ProcessLUIDDeviceMapsEnabled = 28,
    ProcessBreakOnTermination = 29,
    ProcessDebugObjectHandle = 30,
    ProcessDebugFlags = 31,
    ProcessHandleTracing = 32,
    ProcessIoPriority = 33,
    ProcessExecuteFlags = 34,
    ProcessResourceManagement = 35,
    ProcessCookie = 36,
    ProcessImageInformation = 37,       // q: TSectionImageInformation
    ProcessCycleTime = 38,
    ProcessPagePriority = 39,
    ProcessInstrumentationCallback = 40,
    ProcessThreadStackAllocation = 41,
    ProcessWorkingSetWatchEx = 42,
    ProcessImageFileNameWin32 = 43,     // q: UNICODE_STRING
    ProcessImageFileMapping = 44,
    ProcessAffinityUpdateMode = 45,
    ProcessMemoryAllocationMode = 46,
    ProcessGroupInformation = 47,
    ProcessTokenVirtualizationEnabled = 48,
    ProcessConsoleHostProcess = 49,
    ProcessWindowInformation = 50,
    ProcessHandleInformation = 51,
    ProcessMitigationPolicy = 52,
    ProcessDynamicFunctionTableInformation = 53,
    ProcessHandleCheckingMode = 54,
    ProcessKeepAliveCount = 55,
    ProcessRevokeFileHandles = 56,
    ProcessWorkingSetControl = 57,
    ProcessHandleTable = 58,
    ProcessCheckStackExtentsMode = 59,
    ProcessCommandLineInformation = 60 // q: UNICODE_STRING
  );

  TThreadInfoClass = (
    ThreadBasicInformation = 0,    // q: TThreadBasicInformation
    ThreadTimes = 1,
    ThreadPriority = 2,
    ThreadBasePriority = 3,
    ThreadAffinityMask = 4,
    ThreadImpersonationToken = 5,  // s: THandle
    ThreadDescriptorTableEntry = 6,
    ThreadEnableAlignmentFaultFixup = 7,
    ThreadEventPair = 8,
    ThreadQuerySetWin32StartAddress = 9,
    ThreadZeroTlsCell = 10,
    ThreadPerformanceCount = 11,
    ThreadAmILastThread = 12,
    ThreadIdealProcessor = 13,
    ThreadPriorityBoost = 14,
    ThreadSetTlsArrayAddress = 15,
    ThreadIsIoPending = 16,
    ThreadHideFromDebugger = 17,
    ThreadBreakOnTermination = 18,
    ThreadSwitchLegacyState = 19,
    ThreadIsTerminated = 20,       // q: LongBool
    ThreadLastSystemCall = 21,
    ThreadIoPriority = 22,
    ThreadCycleTime = 23,
    ThreadPagePriority = 24,
    ThreadActualBasePriority = 25,
    ThreadTebInformation = 26,      // q: TThreadTebInformation
    ThreadCSwitchMon = 27,
    ThreadCSwitchPmu = 28,
    ThreadWow64Context = 29,
    ThreadGroupInformation = 30,
    ThreadUmsInformation = 31,
    ThreadCounterProfiling = 32,
    ThreadIdealProcessorEx = 33
  );

  // ProcessBasicInformation
  TProcessBasinInformation = record
    ExitStatus: NTSTATUS;
    PebBaseAddress: PPeb;
    AffinityMask: NativeUInt;
    BasePriority: KPRIORITY;
    UniqueProcessId: NativeUInt;
    InheritedFromUniqueProcessId: NativeUInt;
  end;
  PProcessBasinInformation = ^TProcessBasinInformation;

  // ProcessAccessToken
  TProcessAccessToken = record
    Token: THandle; // needs TOKEN_ASSIGN_PRIMARY
    Thread: THandle; // currently unused, was THREAD_QUERY_INFORMATION
  end;

  TThreadBasicInformation = record
    ExitStatus: NTSTATUS;
    TebBaseAddress: PTeb;
    ClientId: TClientId;
    AffinityMask: NativeUInt;
    Priority: KPRIORITY;
    BasePriority: Integer;
  end;
  PThreadBasicInformation = ^TThreadBasicInformation;

  TThreadTebInformation = record
    TebInformation: Pointer;
    TebOffset: Cardinal;
    BytesToRead: Cardinal;
  end;
  PThreadTebInformation = ^TThreadTebInformation;

  TPsApcRoutine = procedure (ApcArgument1, ApcArgument2, ApcArgument3: Pointer);
    stdcall;

  TJobObjectInfoClass = (
    JobObjectReserved = 0,
    JobObjectBasicAccountingInformation = 1, // q: TJobBasicAccountingInfo
    JobObjectBasicLimitInformation = 2,      // q, s: TJobBasicLimitInformation
    JobObjectBasicProcessIdList = 3,         // q: TJobBasicProcessIdList
    JobObjectBasicUIRestrictions = 4,        // q, s: Cardinal
    JobObjectSecurityLimitInformation = 5,   // not supported
    JobObjectEndOfJobTimeInformation = 6,    // s: Cardinal
    JobObjectAssociateCompletionPortInformation = 7, // s:
    JobObjectBasicAndIoAccountingInformation = 8, // q:
    JobObjectExtendedLimitInformation = 9, // q, s: TJobExtendedLimitInformation
    JobObjectJobSetInformation = 10,       // q: Cardinal
    JobObjectGroupInformation = 11         // q, s: Word
  );

  TJobBasicAccountingInfo = record
    TotalUserTime: TLargeInteger;
    TotalKernelTime: TLargeInteger;
    ThisPeriodTotalUserTime: TLargeInteger;
    ThisPeriodTotalKernelTime: TLargeInteger;
    TotalPageFaultCount: Cardinal;
    TotalProcesses: Cardinal;
    ActiveProcesses: Cardinal;
    TotalTerminatedProcesses: Cardinal;
  end;
  PJobBasicAccountingInfo = ^TJobBasicAccountingInfo;

  TJobBasicLimitInformation = record
    PerProcessUserTimeLimit: TLargeInteger;
    PerJobUserTimeLimit: TLargeInteger;
    LimitFlags: Cardinal;
    MinimumWorkingSetSize: NativeUInt;
    MaximumWorkingSetSize: NativeUInt;
    ActiveProcessLimit: Cardinal;
    Affinity: NativeUInt;
    PriorityClass: Cardinal;
    SchedulingClass: Cardinal;
  end;
  PJobBasicLimitInformation = ^TJobBasicLimitInformation;

  TJobBasicProcessIdList = record
    NumberOfAssignedProcesses: Cardinal;
    NumberOfProcessIdsInList: Cardinal;
    ProcessIdList: array [ANYSIZE_ARRAY] of NativeUInt;
  end;
  PJobBasicProcessIdList = ^TJobBasicProcessIdList;

  TJobExtendedLimitInformation = record
    BasicLimitInformation: TJobBasicLimitInformation;
    IoInfo: TIoCounters;
    ProcessMemoryLimit: NativeUInt;
    JobMemoryLimit: NativeUInt;
    PeakProcessMemoryUsed: NativeUInt;
    PeakJobMemoryUsed: NativeUInt;
  end;
  PJobExtendedLimitInformation = ^TJobExtendedLimitInformation;

// Processes

function NtOpenProcess(out ProcessHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes; const ClientId: TClientId):
  NTSTATUS; stdcall; external ntdll;

function NtTerminateProcess(ProcessHandle: THandle; ExitStatus: NTSTATUS):
  NTSTATUS; stdcall; external ntdll;

function NtSuspendProcess(ProcessHandle: THandle): NTSTATUS; stdcall;
  external ntdll;

function NtResumeProcess(ProcessHandle: THandle): NTSTATUS; stdcall;
  external ntdll;

function NtQueryInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: TProcessInfoClass; ProcessInformation: Pointer;
  ProcessInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

// Absent in ReactOS
function NtGetNextProcess(ProcessHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Flags: Cardinal; out NewProcessHandle: THandle):
  NTSTATUS; stdcall; external ntdll delayed;

// Absent in ReactOS
function NtGetNextThread(ProcessHandle: THandle; ThreadHandle: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal; Flags: Cardinal;
  out NewThreadHandle: THandle): NTSTATUS; stdcall; external ntdll delayed;

function NtSetInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: TProcessInfoClass; ProcessInformation: Pointer;
  ProcessInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

// Threads

function NtOpenThread(out ThreadHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes; const ClientId: TClientId):
  NTSTATUS; stdcall; external ntdll;

function NtTerminateThread(ThreadHandle: THandle; ExitStatus: NTSTATUS):
  NTSTATUS; stdcall; external ntdll;

function NtSuspendThread(ThreadHandle: THandle; PreviousSuspendCount:
  PCardinal): NTSTATUS; stdcall; external ntdll;

function NtResumeThread(ThreadHandle: THandle; PreviousSuspendCount:
  PCardinal): NTSTATUS; stdcall; external ntdll;

function NtGetCurrentProcessorNumber: Cardinal; stdcall; external ntdll;

function NtQueryInformationThread(ThreadHandle: THandle;
  ThreadInformationClass: TThreadInfoClass; ThreadInformation: Pointer;
  ThreadInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

function NtSetInformationThread(ThreadHandle: THandle;
  ThreadInformationClass: TThreadInfoClass; ThreadInformation: Pointer;
  ThreadInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtAlertThread(ThreadHandle: THandle): NTSTATUS; stdcall;
  external ntdll;

function NtAlertResumeThread(ThreadHandle: THandle; PreviousSuspendCount:
  PCardinal): NTSTATUS; stdcall; external ntdll;

function NtImpersonateThread(ServerThreadHandle: THandle;
  ClientThreadHandle: THandle; const SecurityQos: TSecurityQualityOfService):
  NTSTATUS; stdcall; external ntdll;

function NtQueueApcThread(ThreadHandle: THandle; ApcRoutine: TPsApcRoutine;
  ApcArgument1, ApcArgument2, ApcArgument3: Pointer): NTSTATUS; stdcall;
  external ntdll;

// Job objects

function NtCreateJobObject(out JobHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes): NTSTATUS; stdcall; external ntdll;

function NtOpenJobObject(out JobHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall; external ntdll;

function NtAssignProcessToJobObject(JobHandle: THandle; ProcessHandle: THandle):
  NTSTATUS; stdcall; external ntdll;

function NtTerminateJobObject(JobHandle: THandle; ExitStatus: NTSTATUS):
  NTSTATUS; stdcall; external ntdll;

function NtIsProcessInJob(ProcessHandle: THandle;
  JobHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtQueryInformationJobObject(JobHandle: THandle;
  JobObjectInformationClass: TJobObjectInfoClass; JobObjectInformation: Pointer;
  JobObjectInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

function NtSetInformationJobObject(JobHandle: THandle;
  JobObjectInformationClass: TJobObjectInfoClass; JobObjectInformation: Pointer;
  JobObjectInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

implementation

function NtCurrentProcessId: NativeUInt;
begin
  Result := NtCurrentTeb.ClientId.UniqueProcess;
end;

function NtCurrentThreadId: NativeUInt;
begin
  Result := NtCurrentTeb.ClientId.UniqueThread;
end;

end.
