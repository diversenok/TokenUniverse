unit Ntapi.ntpebteb;
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

type
  TPeb = record
    InheritedAddressSpace: Boolean;
    ReadImageFileExecOptions: Boolean;
    BeingDebugged: Boolean;
    BitField: Boolean;
    Mutant: THandle;
    ImageBaseAddress: Pointer;
    Ldr: Pointer; // ntpsapi.PPEB_LDR_DATA
    ProcessParameters: Pointer; // ntrtl.PRTL_USER_PROCESS_PARAMETERS
    SubSystemData: Pointer;
    ProcessHeap: Pointer;
    FastPebLock: Pointer; // WinNt.PRTL_CRITICAL_SECTION
    IFEOKey: Pointer;
    AtlThunkSListPtr: Pointer; // WinNt.PSLIST_HEADER
    CrossProcessFlags: Cardinal;
    UserSharedInfoPtr: Pointer;
    SystemReserved: Cardinal;
    AtlThunkSListPtr32: Cardinal;
    ApiSetMap: Pointer; // ntpebteb.PAPI_SET_NAMESPACE
    TlsExpansionCounter: Cardinal;
    TlsBitmap: Pointer;
    TlsBitmapBits: array [0..1] of Cardinal;

    ReadOnlySharedMemoryBase: Pointer;
    SharedData: Pointer; // HotpatchInformation
    ReadOnlyStaticServerData: PPointer;

    AnsiCodePageData: Pointer; // PCPTABLEINFO
    OemCodePageData: Pointer; // PCPTABLEINFO
    UnicodeCaseTableData: Pointer; // PNLSTABLEINFO

    NumberOfProcessors: Cardinal;
    NtGlobalFlag: Cardinal;

    CriticalSectionTimeout: TULargeInteger;
    HeapSegmentReserve: NativeUInt;
    HeapSegmentCommit: NativeUInt;
    HeapDeCommitTotalFreeThreshold: NativeUInt;
    HeapDeCommitFreeBlockThreshold: NativeUInt;

    NumberOfHeaps: Cardinal;
    MaximumNumberOfHeaps: Cardinal;
    ProcessHeaps: PPointer; // PHEAP

    GdiSharedHandleTable: Pointer;
    ProcessStarterHelper: Pointer;
    GdiDCAttributeList: Cardinal;

    LoaderLock: Pointer; // WinNt.PRTL_CRITICAL_SECTION

    OSMajorVersion: Cardinal;
    OSMinorVersion: Cardinal;
    OSBuildNumber: Word;
    OSCSDVersion: Word;
    OSPlatformId: Cardinal;
    ImageSubsystem: Cardinal;
    ImageSubsystemMajorVersion: Cardinal;
    ImageSubsystemMinorVersion: Cardinal;
    ActiveProcessAffinityMask: NativeUInt;

  {$IFNDEF WIN64}
    GdiHandleBuffer: array [0 .. 33] of Cardinal;
  {$ELSE}
    GdiHandleBuffer: array [0 .. 59] of Cardinal;
  {$ENDIF}

    PostProcessInitRoutine: Pointer;

    TlsExpansionBitmap: Pointer;
    TlsExpansionBitmapBits: array [1..32] of Cardinal;

    SessionId: Cardinal;

    AppCompatFlags: TULargeInteger;
    AppCompatFlagsUser: TULargeInteger;
    pShimData: Pointer;
    AppCompatInfo: Pointer; // APPCOMPAT_EXE_DATA

    CSDVersion: UNICODE_STRING;

    ActivationContextData: Pointer; // ACTIVATION_CONTEXT_DATA
    ProcessAssemblyStorageMap: Pointer; // ASSEMBLY_STORAGE_MAP
    SystemDefaultActivationContextData: Pointer; // ACTIVATION_CONTEXT_DATA
    SystemAssemblyStorageMap: Pointer; // ASSEMBLY_STORAGE_MAP

    MinimumStackCommit: NativeUInt;

    FlsCallback: PPointer;
    FlsListHead: TListEntry;
    FlsBitmap: Pointer;
    FlsBitmapBits: array [1..4] of Cardinal; // TODO: Check
    FlsHighIndex: Cardinal;

    WerRegistrationData: Pointer;
    WerShipAssertPtr: Pointer;
    pUnused: Pointer; // pContextData
    pImageHeaderHash: Pointer;
    TracingFlags: Cardinal;
    CsrServerReadOnlySharedMemoryBase: UInt64;
    TppWorkerpListLock: Pointer; // WinNt.PRTL_CRITICAL_SECTION
    TppWorkerpList: TListEntry;
    WaitOnAddressHashTable: array [1..128] of Pointer;
    TelemetryCoverageHeader: Pointer; // REDSTONE3
    CloudFileFlags: Cardinal;
    CloudFileDiagFlags: Cardinal; // REDSTONE4
    PlaceholderCompatibilityMode: Byte;
    PlaceholderCompatibilityModeReserved: array [1..7] of Byte;
    LeapSecondData: Pointer; // *_LEAP_SECOND_DATA; // REDSTONE5
    LeapSecondFlags: Cardinal;
    NtGlobalFlag2: Cardinal;
  end;
  PPeb = ^TPeb;

  TActivationContextStack = record
    ActiveFrame: Pointer;
    FrameListCache: TListEntry;
    Flags: Cardinal;
    NextCookieSequenceNumber: Cardinal;
    StackId: Cardinal;
  end;
  PActivationContextStack = ^TActivationContextStack;

  TGdiTebBatch = record
    Offset: Cardinal;
    HDC: NativeUInt;
    Buffer: array [0..309] of Cardinal;
  end;

  PNtTib = ^TNtTib;
  TNtTib = record
    ExceptionList: Pointer;
    StackBase: Pointer;
    StackLimit: Pointer;
    SubSystemTib: Pointer;
    FiberData: Pointer;
    ArbitraryUserPointer: Pointer;
    Self: PNtTib;
  end;

  TTeb = record
    NtTib: TNtTib;

    EnvironmentPointer: Pointer;
    ClientId: TClientId;
    ActiveRpcHandle: Pointer;
    ThreadLocalStoragePointer: Pointer;
    ProcessEnvironmentBlock: PPeb;

    LastErrorValue: Cardinal;
    CountOfOwnedCriticalSections: Cardinal;
    CsrClientThread: Pointer;
    Win32ThreadInfo: Pointer;
    User32Reserved: array [1..26] of Cardinal;
    UserReserved: array [1..5] of Cardinal;
    WOW32Reserved: Pointer;
    CurrentLocale: Cardinal;
    FpSoftwareStatusRegister: Cardinal;
    ReservedForDebuggerInstrumentation: array [1..16] of Pointer;

   {$IFDEF WIN64}
     SystemReserved1: array [1..30] of Pointer;
	 {$ELSE}
     SystemReserved1: array [1..26] of Pointer;
	 {$ENDIF}

    PlaceholderCompatibilityMode: CHAR;
    PlaceholderReserved: array [1..11] of CHAR;
    ProxiedProcessId: Cardinal;
    ActivationStack: TActivationContextStack;

    WorkingOnBehalfTicket: array [1..8] of Byte;
    ExceptionCode: NTSTATUS;

    ActivationContextStackPointer: PActivationContextStack;
    InstrumentationCallbackSp: NativeUInt;
    InstrumentationCallbackPreviousPc: NativeUInt;
    InstrumentationCallbackPreviousSp: NativeUInt;

 	{$IFDEF WIN64}
    TxFsContext: Cardinal;
	{$ENDIF}

    InstrumentationCallbackDisabled: BOOLEAN;

	{$IFNDEF WIN64}
    SpareBytes: array [1..23] of Byte;
    TxFsContext: Cardinal;
	{$ENDIF}

    GdiTebBatch: TGdiTebBatch;
    RealClientId: TClientId;
    GdiCachedProcessHandle: THandle;
    GdiClientPID: Cardinal;
    GdiClientTID: Cardinal;
    GdiThreadLocalInfo: Pointer;
    Win32ClientInfo: array [0..61] of NativeUInt;
    glDispatchTable: array [1..233] of Pointer;
    glReserved1: array [1..29] of NativeUInt;
    glReserved2: Pointer;
    glSectionInfo: Pointer;
    glSection: Pointer;
    glTable: Pointer;
    glCurrentRC: Pointer;
    glContext: Pointer;

    LastStatusValue: NTSTATUS;
    StaticUnicodeString: UNICODE_STRING;
    StaticUnicodeBuffer: array [1..261] of WideChar;

    DeallocationStack: Pointer;
    TlsSlots: array [1..64] of Pointer;
    TlsLinks: TListEntry;

    Vdm: Pointer;
    ReservedForNtRpc: Pointer;
    DbgSsReserved: array [1..2] of Pointer;

    HardErrorMode: Cardinal;

	{$IFDEF WIN64}
	Instrumentation: array [1..11] of Pointer;
	{$ELSE}
    Instrumentation: array [1..9] of Pointer;
	{$ENDIF}

    ActivityId: TGuid;

    SubProcessTag: Pointer;
    PerflibData: Pointer;
    EtwTraceData: Pointer;
    WinSockData: Pointer;
    GdiBatchCount: Cardinal;

    IdealProcessorValue: Cardinal;

    GuaranteedStackBytes: Cardinal;
    ReservedForPerf: Pointer;
    ReservedForOle: Pointer;
    WaitingOnLoaderLock: Cardinal;
    SavedPriorityState: Pointer;
    ReservedForCodeCoverage: NativeUInt;
    ThreadPoolData: Pointer;
    TlsExpansionSlots: PPointer;

	{$IFDEF WIN64}
    DeallocationBStore: Pointer;
    BStoreLimit: Pointer;
	{$ENDIF}

    MuiGeneration: Cardinal;
    IsImpersonating: Cardinal;
    NlsCache: Pointer;
    pShimData: Pointer;
    HeapVirtualAffinity: Word;
    LowFragHeapDataSlot: Word;
    CurrentTransactionHandle: THandle;
    ActiveFrame: Pointer;
    FlsData: Pointer;

    PreferredLanguages: Pointer;
    UserPrefLanguages: Pointer;
    MergedPrefLanguages: Pointer;
    MuiImpersonation: Cardinal;
    CrossTebFlags: Word;
    SameTebFlags: Word;

    TxnScopeEnterCallback: Pointer;
    TxnScopeExitCallback: Pointer;
    TxnScopeContext: Pointer;
    LockCount: Cardinal;
    WowTebOffset: Integer;
    ResourceRetValue: Pointer;
    ReservedForWdf: Pointer;
    ReservedForCrt: Int64;
    EffectiveContainerId: TGuid;
  end;
  PTeb = ^TTeb;

function NtCurrentTeb: PTeb;

implementation

{$IFDEF WIN64}
function NtCurrentTeb: PTeb;
asm
  mov rax, gs:[$0030]
end;
{$ENDIF}

{$IFDEF WIN32}
function NtCurrentTeb: PTeb;
asm
  mov eax, fs:[$0018]
end;
{$ENDIF}

end.
