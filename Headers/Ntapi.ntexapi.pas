unit Ntapi.ntexapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntkeapi;

const
  FLG_MAINTAIN_OBJECT_TYPELIST = $4000;

type
  TSystemInformationClass = (
    SystemProcessInformation = 5, // q: TSystemProcessInformation
    SystemObjectInformation = 17, // q: TSystemObjectTypeInformation mixed with TSystemObjectInformation
    SystemCurrentTimeZoneInformation = 44, // q, s: TRtlTimeZoneInformation
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

  // SystemProcessInformation
  TSystemProcessInformation = record
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
    Threads: array [WORD] of TSystemThreadInformation;
    function GetImageName: String;
  end;
  PSystemProcessInformation = ^TSystemProcessInformation;

  // SystemObjectInformation
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

  // TSystemHandleInformationEx
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

  // SystemExtendedHandleInformation
  TSystemHandleInformationEx = record
    NumberOfHandles: NativeInt;
    Reserved: NativeUInt;
    Handles: array [0..0] of TSystemHandleTableEntryInfoEx;
  end;
  PSystemHandleInformationEx = ^TSystemHandleInformationEx;

function NtQuerySystemInformation(SystemInformationClass
  : TSystemInformationClass; SystemInformation: Pointer;
  SystemInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

function NtAllocateLocallyUniqueId(out Luid: TLuid): NTSTATUS; stdcall;
  external ntdll;

implementation

uses
  Ntapi.ntpsapi, Ntapi.ntobapi;

{ TSystemProcessInformation }

function TSystemProcessInformation.GetImageName: String;
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
