unit NtUtils.Shellcode;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntrtl, NtUtils.Exceptions;

// Write a portion of data to a process' memory
function NtxWriteDataProcess(hProcess: THandle; Buffer: Pointer;
  BufferSize: NativeUInt; out Status: TNtxStatus): Pointer;

// Write executable assembly code to a process
function NtxWriteAssemblyProcess(hProcess: THandle; Buffer: Pointer;
  BufferSize: NativeUInt; out Status: TNtxStatus): Pointer;

// Copy data to a process and invoke a function on a remote thread
function RtlxInvokeFunctionProcess(out hThread: THandle; hProcess: THandle;
  Routine: TUserThreadStartRoutine; ParamBuffer: Pointer; ParamBufferSize:
  NativeUInt; Timeout: Int64 = INFINITE): TNtxStatus;

// Copy assembly code and data and invoke it in a remote thread
function RtlxInvokeAssemblyProcess(out hThread: THandle; hProcess: THandle;
  AssemblyBuffer: Pointer; AssemblyBufferSize: NativeUInt; ParamBuffer: Pointer;
  ParamBufferSize: NativeUInt; Timeout: Int64 = INFINITE): TNtxStatus;

// Synchronously invoke assembly code in a remote thread
function RtlxInvokeAssemblySyncProcess(hProcess: THandle; AssemblyBuffer:
  Pointer; AssemblyBufferSize: NativeUInt; ParamBuffer: Pointer;
  ParamBufferSize: NativeUInt; StatusComment: String): TNtxStatus;

// Inject a dll into a process
function RtlxInjectDllProcess(out hThread: THandle; hProcess: THandle;
  DllName: String; Timeout: Int64): TNtxStatus;

implementation

uses
  Ntapi.ntmmapi, Ntapi.ntstatus,
  NtUtils.Processes.Memory, NtUtils.Threads, NtUtils.Objects, NtUtils.Ldr;

function NtxWriteDataProcess(hProcess: THandle; Buffer: Pointer;
  BufferSize: NativeUInt; out Status: TNtxStatus): Pointer;
begin
  // Allocate writable memory
  Status := NtxAllocateMemoryProcess(hProcess, BufferSize, Result);

  if not Status.IsSuccess then
    Exit;

  Status := NtxWriteMemoryProcess(hProcess, Result, Buffer, BufferSize);

  // Undo allocation on failure
  if not Status.IsSuccess then
    NtxFreeMemoryProcess(hProcess, Result, BufferSize);
end;

function NtxWriteAssemblyProcess(hProcess: THandle; Buffer: Pointer;
  BufferSize: NativeUInt; out Status: TNtxStatus): Pointer;
begin
  // Allocate and write the code to memory
  Result := NtxWriteDataProcess(hProcess, Buffer, BufferSize, Status);

  if not Status.IsSuccess then
    Exit;

  // Make the memory executable
  Status := NtxProtectMemoryProcess(hProcess, Result, BufferSize,
    PAGE_EXECUTE_READ);

  // Flush instruction cache to make sure the processor executes the code
  // from memory, not from its cache
  if Status.IsSuccess then
    Status := NtxFlushInstructionCache(hProcess, Result, BufferSize);

  // Undo everything on error
  if not Status.IsSuccess then
    NtxFreeMemoryProcess(hProcess, Result, BufferSize);
end;

function RtlxInvokeFunctionProcess(out hThread: THandle; hProcess: THandle;
  Routine: TUserThreadStartRoutine; ParamBuffer: Pointer; ParamBufferSize:
  NativeUInt; Timeout: Int64): TNtxStatus;
var
  Parameter: Pointer;
begin
  // Write data
  Parameter := NtxWriteDataProcess(hProcess, ParamBuffer, ParamBufferSize,
    Result);

  if not Result.IsSuccess then
    Exit;

  // Create remote thread
  Result := RtlxCreateThread(hThread, hProcess, Routine, Parameter);

  if not Result.IsSuccess then
  begin
    // Free allocation on failure
    NtxFreeMemoryProcess(hProcess, Parameter, ParamBufferSize);
    Exit;
  end;

  if Timeout <> 0 then
  begin
    Result := NtxWaitForSingleObject(hThread, False, Timeout);

    // If the thread terminated we can clean up the memory
    if Result.Status = STATUS_WAIT_0 then
      NtxFreeMemoryProcess(hProcess, Parameter, ParamBufferSize);
  end;
end;

function RtlxInvokeAssemblyProcess(out hThread: THandle; hProcess: THandle;
  AssemblyBuffer: Pointer; AssemblyBufferSize: NativeUInt; ParamBuffer: Pointer;
  ParamBufferSize: NativeUInt; Timeout: Int64 = INFINITE): TNtxStatus;
var
  pCode: Pointer;
begin
  // Write assembly code
  pCode := NtxWriteAssemblyProcess(hProcess, AssemblyBuffer, AssemblyBufferSize,
    Result);

  if not Result.IsSuccess then
    Exit;

  // Invoke this code passing the parameter buffer
  Result := RtlxInvokeFunctionProcess(hThread, hProcess, pCode, ParamBuffer,
    ParamBufferSize, Timeout);

  // Free the assembly allocation if the thread exited or anything else happen
  if Result.Matches(STATUS_WAIT_0, 'NtWaitForSingleObject')
    or not Result.IsSuccess then
    NtxFreeMemoryProcess(hProcess, pCode, AssemblyBufferSize);
end;

function RtlxInvokeAssemblySyncProcess(hProcess: THandle; AssemblyBuffer:
  Pointer; AssemblyBufferSize: NativeUInt; ParamBuffer: Pointer;
  ParamBufferSize: NativeUInt; StatusComment: String): TNtxStatus;
var
  ResultCode: NTSTATUS;
  hThread: THandle;
begin
  // Invoke the assembly code and wait for the result
  Result := RtlxInvokeAssemblyProcess(hThread, hProcess, AssemblyBuffer,
    AssemblyBufferSize, ParamBuffer, ParamBufferSize, INFINITE);

  if not Result.IsSuccess then
    Exit;

  Result := NtxQueryExitStatusThread(hThread, ResultCode);
  NtxSafeClose(hThread);

  if Result.IsSuccess then
  begin
    // Pass the result of assembly code execution to the caller
    Result.Location := StatusComment;
    Result.Status := ResultCode;
  end;
end;

function RtlxInjectDllProcess(out hThread: THandle; hProcess: THandle;
  DllName: String; Timeout: Int64): TNtxStatus;
var
  hKernel32: HMODULE;
  pLoadLibrary: Pointer;
begin
  // TODO: WoW64 support
  Result := LdrxGetDllHandle(kernel32, hKernel32);

  if not Result.IsSuccess then
    Exit;

  Result := LdrxGetProcedureAddress(hKernel32, 'LoadLibraryW', pLoadLibrary);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxInvokeFunctionProcess(hThread, hProcess, pLoadLibrary,
    PWideChar(DllName), (Length(DllName) + 1) * SizeOf(WideChar), Timeout);
end;

end.
