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

implementation

end.
