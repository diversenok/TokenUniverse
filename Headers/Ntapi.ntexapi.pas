unit Ntapi.ntexapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, Ntapi.ntdef, Ntapi.ntkeapi;

type
  TSystemInformationClass = (
    SystemProcessInformation = 5, // q: TSystemProcessInformation
    SystemExtendedHandleInformation = 64 // q: TSystemHandleInformationEx
  );

  TSystemThreadInformation = record
    KernelTime: UInt64;
    UserTime: UInt64;
    CreateTime: UInt64;
    WaitTime: Cardinal;
    StartAddress: Pointer;
    ClientId: TClientId;
    Priority: KPRIORITY;
    BasePriority: Integer;
    ContextSwitches: Cardinal;
    ThreadState: Cardinal;
    WaitReason: KWAIT_REASON;
  end;

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
    function QueryFullImageName: String;
  end;
  PSystemProcessInformation = ^TSystemProcessInformation;

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
    NumberOfHandles: NativeUInt;
    Reserved: NativeUInt;
    Handles: array [Word] of TSystemHandleTableEntryInfoEx; // TODO: enlarge
  end;
  PSystemHandleInformationEx = ^TSystemHandleInformationEx;

function NtQuerySystemInformation(SystemInformationClass
  : TSystemInformationClass; SystemInformation: Pointer;
  SystemInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

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

function TSystemProcessInformation.QueryFullImageName: String;
const
  MAX_NAME = $1000E; // value used by QueryFullProcessImageNameW
var
  hProcess: THandle;
  ObjAttr: TObjectAttributes;
  ClientID: TClientId;
  Buffer: PUNICODE_STRING;
begin
  ClientID.Create(ProcessId, 0);
  InitializeObjectAttributes(ObjAttr);
  Result := '';

  if NT_SUCCESS(NtOpenProcess(hProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    ObjAttr, ClientID)) then
  begin
    Buffer := AllocMem(MAX_NAME);

    if NT_SUCCESS(NtQueryInformationProcess(hProcess, ProcessImageFileNameWin32,
      Buffer, MAX_NAME, nil)) then
      Result := Buffer.ToString;

    FreeMem(Buffer);
    NtClose(hProcess);
  end;
end;

end.
