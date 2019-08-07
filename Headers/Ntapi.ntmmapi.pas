unit Ntapi.ntmmapi;

{$MINENUMSIZE 4}

interface

uses
  Ntapi.ntdef;

const
  // WinNt.12784
  PAGE_NOACCESS = $01;
  PAGE_READONLY = $02;
  PAGE_READWRITE = $04;
  PAGE_WRITECOPY = $08;
  PAGE_EXECUTE = $10;
  PAGE_EXECUTE_READ = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_GUARD = $100;
  PAGE_NOCACHE = $200;
  PAGE_WRITECOMBINE = $400;

  MEM_COMMIT = $00001000;
  MEM_RESERVE = $00002000;
  MEM_DECOMMIT = $00004000;
  MEM_RELEASE = $00008000;
  MEM_FREE = $00010000;
  MEM_RESET = $00080000;
  MEM_TOP_DOWN = $00100000;
  MEM_WRITE_WATCH = $00200000;
  MEM_PHYSICAL = $00400000;
  MEM_ROTATE = $00800000;
  MEM_LARGE_PAGES = $20000000;

  // reactos.mmtypes; lock options
  MAP_PROCESS = 1;
  MAP_SYSTEM = 2;

type
  TMemoryInformationClass = (
    MemoryBasicInformation = 0,          // q: TMemoryBasicInformation
    MemoryWorkingSetInformation = 1,
    MemoryMappedFilenameInformation = 2, // q: UNICODE_STRING
    MemoryRegionInformation = 3,
    MemoryWorkingSetExInformation = 4,   // q: TMemoryWorkingSetExInformation
    MemorySharedCommitInformation = 5,
    MemoryImageInformation = 6           // q: TMemoryImageInformation
  );

  // WinNt.12692
  TMemoryBasicInformation = record
    BaseAddress: Pointer;
    AllocationBase: Pointer;
    AllocationProtect: Cardinal;
    RegionSize: NativeUInt;
    State: Cardinal;
    Protect: Cardinal;
    MemoryType: Cardinal;
  end;
  PMemoryBasicInformation = ^TMemoryBasicInformation;

  TMemoryWorkingSetExInformation = record
    VirtualAddress: Pointer;
    VirtualAttributes: NativeUInt;
  end;
  PMemoryWorkingSetExInformation = ^TMemoryWorkingSetExInformation;

  TMemoryImageInformation = record
    ImageBase: Pointer;
    SizeOfImage: NativeUInt;
    ImageFlags: Cardinal;
  end;
  PMemoryImageInformation = ^TMemoryImageInformation;

  TSectionImageInformation = record
    TransferAddress: Pointer;
    ZeroBits: Cardinal;
    MaximumStackSize: NativeUInt;
    CommittedStackSize: NativeUInt;
    SubSystemType: Cardinal;
    SubSystemVersion: Cardinal;
    OperatingSystemVersion: Cardinal;
    ImageCharacteristics: Word;
    DllCharacteristics: Word;
    Machine: Word;
    ImageContainsCode: Boolean;
    ImageFlags: Byte;
    LoaderFlags: Cardinal;
    ImageFileSize: Cardinal;
    CheckSum: Cardinal;
  end;
  PSectionImageInformation = ^TSectionImageInformation;

function NtAllocateVirtualMemory(ProcessHandle: THandle; var BaseAddress:
  Pointer; ZeroBits: NativeUInt; var RegionSize: NativeUInt; AllocationType:
  Cardinal; Protect: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtFreeVirtualMemory(ProcessHandle: THandle; var BaseAddress: Pointer;
  var RegionSize: NativeUInt; FreeType: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtReadVirtualMemory(ProcessHandle: THandle; BaseAddress: Pointer;
  Buffer: Pointer; BufferSize: NativeUInt; NumberOfBytesRead: PNativeUInt):
  NTSTATUS; stdcall; external ntdll;

function NtWriteVirtualMemory(ProcessHandle: THandle; BaseAddress: Pointer;
  Buffer: Pointer; BufferSize: NativeUInt; NumberOfBytesWritten: PNativeUInt):
  NTSTATUS; stdcall; external ntdll;

function NtProtectVirtualMemory(ProcessHandle: THandle; var BaseAddress:
  Pointer; var RegionSize: NativeUInt; NewProtect: Cardinal;
  out OldProtect: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtQueryVirtualMemory(ProcessHandle: THandle; BaseAddress: Pointer;
  MemoryInformationClass: TMemoryInformationClass; MemoryInformation: Pointer;
  MemoryInformationLength: NativeUInt; ReturnLength: PNativeUInt): NTSTATUS;
  stdcall; external ntdll;

function NtLockVirtualMemory(ProcessHandle: THandle; var BaseAddress: Pointer;
  var RegionSize: NativeUInt; MapType: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtUnlockVirtualMemory(ProcessHandle: THandle; var BaseAddress: Pointer;
  var RegionSize: NativeUInt; MapType: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtFlushInstructionCache(ProcessHandle: THandle; BaseAddress: Pointer;
  Length: NativeUInt): NTSTATUS; stdcall; external ntdll;

function NtFlushWriteBuffer: NTSTATUS; stdcall; external ntdll;

implementation

end.
