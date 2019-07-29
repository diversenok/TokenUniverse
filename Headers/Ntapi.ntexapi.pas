unit Ntapi.ntexapi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntkeapi;

const
  // Event

  EVENT_QUERY_STATE = $0001;
  EVENT_MODIFY_STATE = $0002;
  EVENT_ALL_ACCESS = STANDARD_RIGHTS_ALL or $0003;

  // Mutant

  MUTANT_QUERY_STATE = $0001;
  MUTANT_ALL_ACCESS = STANDARD_RIGHTS_ALL or MUTANT_QUERY_STATE;

  // Semaphore

  SEMAPHORE_QUERY_STATE = $0001;
  SEMAPHORE_MODIFY_STATE = $0002;
  SEMAPHORE_ALL_ACCESS = STANDARD_RIGHTS_ALL or $0003;

  // Timer

  TIMER_QUERY_STATE = $0001;
  TIMER_MODIFY_STATE = $0002;
  TIMER_ALL_ACCESS = STANDARD_RIGHTS_ALL or $0003;

  // Global flags

  FLG_MAINTAIN_OBJECT_TYPELIST = $4000; // kernel

type
  // Event

  TEventInformationClass = (
    EventBasicInformation = 0 // q: TEventBasicInformation
  );

  TEventBasicInformation = record
    EventType: TEventType;
    EventState: Integer;
  end;
  PEventBasicInformation = ^TEventBasicInformation;

  // Mutant

  TMutantInformationClass = (
    MutantBasicInformation = 0, // q: TMutantBasicInformation
    MutantOwnerInformation = 1  // q: TClientId
  );

  TMutantBasicInformation = record
    CurrentCount: Integer;
    OwnedByCaller: Boolean;
    AbandonedState: Boolean;
  end;
  PMutantBasicInformation = ^TMutantBasicInformation;

  // Semaphore

  TSemaphoreInformationClass = (
    SemaphoreBasicInformation = 0 // q: TSemaphoreBasicInformation
  );

  TSemaphoreBasicInformation = record
    CurrentCount: Integer;
    MaximumCount: Integer;
  end;
  PSemaphoreBasicInformation = ^TSemaphoreBasicInformation;

  // Timer

  TTimerInformationClass = (
    TimerBasicInformation = 0 // q: TTimerBasicInformation
  );

  TTimerBasicInformation = record
    RemainingTime: TLargeInteger;
    TimerState: Boolean;
  end;
  PTimerBasicInformation = ^TTimerBasicInformation;

  TTimerApcRoutine = procedure(TimerContext: Pointer; TimerLowValue: Cardinal;
    TimerHighValue: Integer) stdcall;

  TTimerSetInformationClass = (
    TimerSetCoalescableTimer = 0 // s: TTimerSetCoalescableTimerInfo
  );

  TTimerSetCoalescableTimerInfo = record
    DueTime: TLargeInteger;            // in
    TimerApcRoutine: TTimerApcRoutine; // in opt
    TimerContext: Pointer;             // in opt
    WakeContext: Pointer;              // in opt
    Period: Cardinal;                  // in opt
    TolerableDelay: Cardinal;          // in
    PreviousState: PBoolean;           // out opt
  end;
  PTimerSetCoalescableTimerInfo = ^TTimerSetCoalescableTimerInfo;

  // System Information

  TSystemInformationClass = (
    SystemBasicInformation = 0,
    SystemProcessorInformation = 1,
    SystemPerformanceInformation = 2,
    SystemTimeOfDayInformation = 3,
    SystemPathInformation = 4,
    SystemProcessInformation = 5, // q: TSystemProcessInformation
    SystemCallCountInformation = 6,
    SystemDeviceInformation = 7,
    SystemProcessorPerformanceInformation = 8,
    SystemFlagsInformation = 9,
    SystemCallTimeInformation = 10,
    SystemModuleInformation = 11,
    SystemLocksInformation = 12,
    SystemStackTraceInformation = 13,
    SystemPagedPoolInformation = 14,
    SystemNonPagedPoolInformation = 15,
    SystemHandleInformation = 16,
    SystemObjectInformation = 17, // q: TSystemObjectTypeInformation mixed with TSystemObjectInformation
    SystemPageFileInformation = 18,
    SystemVdmInstemulInformation = 19,
    SystemVdmBopInformation = 20,
    SystemFileCacheInformation = 21,
    SystemPoolTagInformation = 22,
    SystemInterruptInformation = 23,
    SystemDpcBehaviorInformation = 24,
    SystemFullMemoryInformation = 25,
    SystemLoadGdiDriverInformation = 26,
    SystemUnloadGdiDriverInformation = 27,
    SystemTimeAdjustmentInformation = 28,
    SystemSummaryMemoryInformation = 29,
    SystemMirrorMemoryInformation = 30,
    SystemPerformanceTraceInformation = 31,
    SystemObsolete0 = 32,
    SystemExceptionInformation = 33,
    SystemCrashDumpStateInformation = 34,
    SystemKernelDebuggerInformation = 35,
    SystemContextSwitchInformation = 36,
    SystemRegistryQuotaInformation = 37,
    SystemExtendServiceTableInformation = 38,
    SystemPrioritySeperation = 39,
    SystemVerifierAddDriverInformation = 40,
    SystemVerifierRemoveDriverInformation = 41,
    SystemProcessorIdleInformation = 42,
    SystemLegacyDriverInformation = 43,
    SystemCurrentTimeZoneInformation = 44, // q, s: TRtlTimeZoneInformation
    SystemLookasideInformation = 45,
    SystemTimeSlipNotification = 46,
    SystemSessionCreate = 47,
    SystemSessionDetach = 48,
    SystemSessionInformation = 49,
    SystemRangeStartInformation = 50,
    SystemVerifierInformation = 51,
    SystemVerifierThunkExtend = 52,
    SystemSessionProcessInformation = 53,
    SystemLoadGdiDriverInSystemSpace = 54,
    SystemNumaProcessorMap = 55,
    SystemPrefetcherInformation = 56,
    SystemExtendedProcessInformation = 57,
    SystemRecommendedSharedDataAlignment = 58,
    SystemComPlusPackage = 59,
    SystemNumaAvailableMemory = 60,
    SystemProcessorPowerInformation = 61,
    SystemEmulationBasicInformation = 62,
    SystemEmulationProcessorInformation = 63,
    SystemExtendedHandleInformation = 64 // q: TSystemHandleInformationEx
  );

  TSystemThreadInformation = record
    KernelTime: TLargeInteger;
    UserTime: TLargeInteger;
    CreateTime: TLargeInteger;
    WaitTime: Cardinal;
    StartAddress: Pointer;
    ClientId: TClientId;
    Priority: KPRIORITY;
    BasePriority: Integer;
    ContextSwitches: Cardinal;
    ThreadState: KTHREAD_STATE;
    WaitReason: KWAIT_REASON;
  end;
  PSystemThreadInformation = ^TSystemThreadInformation;

  TSystemProcessInformationFixed = record
    NextEntryOffset: Cardinal;
    NumberOfThreads: Cardinal;
    WorkingSetPrivateSize: Int64; // since VISTA
    HardFaultCount: Cardinal; // since WIN7
    NumberOfThreadsHighWatermark: Cardinal; // since WIN7
    CycleTime: UInt64; // since WIN7
    CreateTime: Int64;
    UserTime: Int64;
    KernelTime: Int64;
    ImageName: UNICODE_STRING;
    BasePriority: Cardinal;
    ProcessId: NativeUInt;
    InheritedFromProcessId: NativeUInt;
    HandleCount: Cardinal;
    SessionId: Cardinal;
    UniqueProcessKey: NativeUInt; // since VISTA & SystemExtendedProcessInformation
    PeakVirtualSize: NativeUInt;
    VirtualSize: NativeUInt;
    PageFaultCount: Cardinal;
    PeakWorkingSetSize: NativeUInt;
    WorkingSetSize: NativeUInt;
    QuotaPeakPagedPoolUsage: NativeUInt;
    QuotaPagedPoolUsage: NativeUInt;
    QuotaPeakNonPagedPoolUsage: NativeUInt;
    QuotaNonPagedPoolUsage: NativeUInt;
    PagefileUsage: NativeUInt;
    PeakPagefileUsage: NativeUInt;
    PrivatePageCount: NativeUInt;
    ReadOperationCount: UInt64;
    WriteOperationCount: UInt64;
    OtherOperationCount: UInt64;
    ReadTransferCount: UInt64;
    WriteTransferCount: UInt64;
    OtherTransferCount: UInt64;
    function GetImageName: String;
  end;
  PSystemProcessInformationFixed = ^TSystemProcessInformationFixed;

  TSystemProcessInformation = record
    Process: TSystemProcessInformationFixed;
    Threads: array [ANYSIZE_ARRAY] of TSystemThreadInformation;
  end;
  PSystemProcessInformation = ^TSystemProcessInformation;

  TSystemObjectTypeInformation = record
    NextEntryOffset: Cardinal;
    NumberOfObjects: Cardinal;
    NumberOfHandles: Cardinal;
    TypeIndex: Cardinal;
    InvalidAttributes: Cardinal;
    GenericMapping: TGenericMapping;
    ValidAccessMask: Cardinal;
    PoolType: Cardinal;
    SecurityRequired: Boolean;
    WaitableObject: Boolean;
    TypeName: UNICODE_STRING;
  end;
  PSystemObjectTypeInformation = ^TSystemObjectTypeInformation;

  TSystemObjectInformation = record
    NextEntryOffset: Cardinal;
    ObjectAddress: Pointer;
    CreatorUniqueProcess: THandle;
    CreatorBackTraceIndex: Word;
    Flags: Word;
    PointerCount: Integer;
    HandleCount: Integer;
    PagedPoolCharge: Cardinal;
    NonPagedPoolCharge: Cardinal;
    ExclusiveProcessId: THandle;
    SecurityDescriptor: Pointer;
    NameInfo: UNICODE_STRING;
  end;
  PSystemObjectInformation = ^TSystemObjectInformation;

  TSystemHandleTableEntryInfoEx = record
    PObject: Pointer;
    UniqueProcessId: NativeUInt;
    HandleValue: NativeUInt;
    GrantedAccess: TAccessMask;
    CreatorBackTraceIndex: Word;
    ObjectTypeIndex: Word;
    HandleAttributes: Cardinal;
    Reserved: Cardinal;
  end;
  PSystemHandleTableEntryInfoEx = ^TSystemHandleTableEntryInfoEx;

  TSystemHandleInformationEx = record
    NumberOfHandles: NativeInt;
    Reserved: NativeUInt;
    Handles: array [ANYSIZE_ARRAY] of TSystemHandleTableEntryInfoEx;
  end;
  PSystemHandleInformationEx = ^TSystemHandleInformationEx;

// Thread execution

function NtDelayExecution(Alertable: Boolean; DelayInterval:
  PLargeInteger = nil): NTSTATUS; stdcall; external ntdll; overload;

function NtDelayExecution(Alertable: Boolean; const DelayInterval:
  TLargeInteger): NTSTATUS; stdcall; external ntdll; overload;

// Event

function NtCreateEvent(out EventHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; EventType: TEventType;
  InitialState: Boolean): NTSTATUS; stdcall; external ntdll;

function NtOpenEvent(out EventHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall; external ntdll;

function NtSetEvent(EventHandle: THandle; PreviousState: PCardinal): NTSTATUS;
  stdcall; external ntdll;

function  NtSetEventBoostPriority(EventHandle: THandle): NTSTATUS;
  stdcall; external ntdll;

function NtClearEvent(EventHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtResetEvent(EventHandle: THandle; PreviousState: PCardinal):
  NTSTATUS; stdcall; external ntdll;

function NtPulseEvent(EventHandle: THandle; PreviousState: PCardinal):
  NTSTATUS; stdcall; external ntdll;

function NtQueryEvent(EventHandle: THandle; EventInformationClass:
  TEventInformationClass; EventInformation: Pointer; EventInformationLength:
  Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

// Mutant

function NtCreateMutant(out MutantHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; InitialOwner: Boolean): NTSTATUS;
  stdcall; external ntdll;

function NtOpenMutant(out MutantHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall; external ntdll;

function NtReleaseMutant(MutantHandle: THandle; PreviousCount: PInteger):
  NTSTATUS; stdcall; external ntdll;

function NtQueryMutant(MutantHandle: THandle; MutantInformationClass:
  TMutantInformationClass; MutantInformation: Pointer; MutantInformationLength:
  Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

// Semaphore

function NtCreateSemaphore(out SemaphoreHandle: THandle; DesiredAccess:
  TAccessMask; ObjectAttributes: PObjectAttributes; InitialCount: Integer;
  MaximumCount: Integer): NTSTATUS; stdcall; external ntdll;

function NtOpenSemaphore(out SemaphoreHandle: THandle; DesiredAccess:
  TAccessMask; const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall;
  external ntdll;

function NtReleaseSemaphore(SemaphoreHandle: THandle; ReleaseCount: Integer;
  PreviousCount: PInteger): NTSTATUS; stdcall; external ntdll;

function NtQuerySemaphore(SemaphoreHandle: THandle; SemaphoreInformationClass:
  TSemaphoreInformationClass; SemaphoreInformation: Pointer;
  SemaphoreInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

// Timer

function NtCreateTimer(out TimerHandle: THandle; DesiredAccess: TAccessMask;
  ObjectAttributes: PObjectAttributes; TimerType: TTimerType): NTSTATUS;
  stdcall; external ntdll;

function NtOpenTimer(out TimerHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall; external ntdll;

function NtSetTimer(TimerHandle: THandle; DueTime: PLargeInteger;
  TimerApcRoutine: TTimerApcRoutine; TimerContext: Pointer;
  ResumeTimer: Boolean; Period: Integer; PreviousState: PBoolean): NTSTATUS;
  stdcall; external ntdll;

function NtSetTimerEx(TimerHandle: THandle; TimerSetInformationClass:
  TTimerSetInformationClass; TimerSetInformation: Pointer;
  TimerSetInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtCancelTimer(TimerHandle: THandle;
  CurrentState: PBoolean): NTSTATUS; stdcall; external ntdll;

function NtQueryTimer(TimerHandle: THandle; TimerInformationClass:
  TTimerInformationClass; TimerInformation: Pointer; TimerInformationLength:
  Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

// Time

function NtQueryTimerResolution(out MaximumTime: Cardinal;
  out MinimumTime: Cardinal; out CurrentTime: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtSetTimerResolution(DesiredTime: Cardinal; SetResolution: Boolean;
  out ActualTime: Cardinal): NTSTATUS; stdcall; external ntdll;

// LUIDs

function NtAllocateLocallyUniqueId(out Luid: TLuid): NTSTATUS; stdcall;
  external ntdll;

// System Information

function NtQuerySystemInformation(SystemInformationClass
  : TSystemInformationClass; SystemInformation: Pointer;
  SystemInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

implementation

{ TSystemProcessInformationFixed }

function TSystemProcessInformationFixed.GetImageName: String;
begin
  if not Assigned(@Self) then
    Result := 'Unknown process'
  else
  begin
    Result := ImageName.ToString;
    if Result = '' then
      Result := 'System Idle Process';
  end;
end;

end.
