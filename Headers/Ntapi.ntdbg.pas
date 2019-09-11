unit Ntapi.ntdbg;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

const
  DEBUG_READ_EVENT = $0001;
  DEBUG_PROCESS_ASSIGN = $0002;
  DEBUG_SET_INFORMATION = $0004;
  DEBUG_QUERY_INFORMATION = $0008;
  DEBUG_ALL_ACCESS = STANDARD_RIGHTS_ALL or $000F;

  // Creation flag
  DEBUG_KILL_ON_CLOSE = $1;

type
  TDbgKmException = record
    ExceptionRecord: TExceptionRecord;
    FirstChance: Cardinal;
  end;
  PDbgKmException = ^TDbgKmException;

  TDbgKmCreateThread = record
    SubSystemKey: Cardinal;
    StartAddress: Pointer;
  end;
  PDbgKmCreateThread = ^TDbgKmCreateThread;

  TDbgKmCreateProcess = record
    SubSystemKey: Cardinal;
    FileHandle: THandle;
    BaseOfImage: Pointer;
    DebugInfoFileOffset: Cardinal;
    DebugInfoSize: Cardinal;
    InitialThread: TDbgKmCreateThread;
  end;
  PDbgKmCreateProcess = ^TDbgKmCreateProcess;

  TDbgKmLoadDll = record
    FileHandle: THandle;
    BaseOfDll: Pointer;
    DebugInfoFileOffset: Cardinal;
    DebugInfoSize: Cardinal;
    NamePointer: Pointer;
  end;
  PDbgKmLoadDll = ^TDbgKmLoadDll;

  TDbgState = (
    DbgIdle = 0,
    DbgReplyPending = 1,
    DbgCreateThreadStateChange = 2,
    DbgCreateProcessStateChange = 3,
    DbgExitThreadStateChange = 4,
    DbgExitProcessStateChange = 5,
    DbgExceptionStateChange = 6,
    DbgBreakpointStateChange = 7,
    DbgSingleStepStateChange = 8,
    DbgLoadDllStateChange = 9,
    DbgUnloadDllStateChange = 10
  );

  TDbgUiCreateThread = record
    HandleToThread: THandle;
    NewThread: TDbgKmCreateThread;
  end;
  PDbgUiCreateThread = ^TDbgUiCreateThread;

  TDbgUiCreateProcess = record
    HandleToProcess: THandle;
    HandleToThread: THandle;
    NewProcess: TDbgKmCreateProcess;
  end;
  PDbgUiCreateProcess = ^TDbgUiCreateProcess;

  TDbgUiWaitStateChange = record
    NewState: TDbgState;
    AppClientId: TClientId;
  case Integer of
    0: (Exception: TDbgKmException);
    1: (CreateThread: TDbgUiCreateThread);
    2: (CreateProcessInfo: TDbgUiCreateProcess);
    3: (ExitThread: NTSTATUS);
    4: (ExitProcess: NTSTATUS);
    5: (LoadDll: TDbgKmLoadDll);
    6: (UnloadDll: Pointer);
  end;
  PDbgUiWaitStateChange = ^TDbgUiWaitStateChange;

  TDebugObjectInfoClass = (
    DebugObjectUnusedInformation = 0,
    DebugObjectKillProcessOnExitInformation = 1
  );

function NtCreateDebugObject(out DebugObjectHandle: THandle;
  DesiredAccess: TAccessMask; const ObjectAttributes: TObjectAttributes;
  Flags: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtDebugActiveProcess(ProcessHandle: THandle;
  DebugObjectHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtDebugContinue(DebugObjectHandle: THandle; const ClientId: TClientId;
  ContinueStatus: NTSTATUS): NTSTATUS; stdcall; external ntdll;

function NtRemoveProcessDebug(ProcessHandle: THandle;
  DebugObjectHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtSetInformationDebugObject(DebugObjectHandle: THandle;
  DebugObjectInformationClass: TDebugObjectInfoClass; DebugInformation: Pointer;
  DebugInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall;
  external ntdll;

// Debug UI

function DbgUiConnectToDbg: NTSTATUS; stdcall; external ntdll;

function NtWaitForDebugEvent(DebugObjectHandle: THandle; Alertable: Boolean;
  Timeout: PLargeInteger; out WaitStateChange: TDbgUiWaitStateChange): NTSTATUS;
  stdcall; external ntdll;

function DbgUiGetThreadDebugObject: THandle; stdcall; external ntdll;

procedure DbgUiSetThreadDebugObject(DebugObject: THandle); stdcall;
  external ntdll;

function DbgUiDebugActiveProcess(Process: THandle): NTSTATUS; stdcall;
  external ntdll;

procedure DbgUiRemoteBreakin(Context: Pointer); stdcall; external ntdll;

function DbgUiIssueRemoteBreakin(Process: THandle): NTSTATUS; stdcall;
  external ntdll;

implementation

end.
