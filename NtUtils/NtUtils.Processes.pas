unit NtUtils.Processes;

interface

uses
  Winapi.WinNt, Ntapi.ntpsapi, NtUtils.Exceptions;

const
  // Ntapi.ntpsapi
  NtCurrentProcess: THandle = THandle(-1);

type
  TProcessHandleEntry = Ntapi.ntpsapi.TProcessHandleTableEntryInfo;

// Open a process (always succeeds for the current PID)
function NtxOpenProcess(out hProcess: THandle; PID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;

// Reopen a handle to the current process with the specific access
function NtxOpenCurrentProcess(out hProcess: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;

// Query variable-size information
function NtxQueryProcess(hProcess: THandle; InfoClass: TProcessInfoClass;
  out Status: TNtxStatus): Pointer;

// Set variable-size information
function NtxSetProcess(hProcess: THandle; InfoClass: TProcessInfoClass;
  Data: Pointer; DataSize: Cardinal): TNtxStatus;

type
  NtxProcess = class
    // Query fixed-size information
    class function Query<T>(hProcess: THandle;
      InfoClass: TProcessInfoClass; out Buffer: T): TNtxStatus; static;

    // Set fixed-size information
    class function SetInfo<T>(hProcess: THandle;
      InfoClass: TProcessInfoClass; const Buffer: T): TNtxStatus; static;
  end;

// Enumerate handles of a process
function NtxEnumerateHandlesProcess(hProcess: THandle; out Handles:
  TArray<TProcessHandleEntry>): TNtxStatus;

// Query a string
function NtxQueryStringProcess(hProcess: THandle; InfoClass: TProcessInfoClass;
  out Str: String): TNtxStatus;

// Try to query image name in Win32 format
function NtxTryQueryImageProcessById(PID: NativeUInt): String;

// Fail if the current process is running under WoW64
function NtxAssertNotWoW64: TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Ntapi.ntobapi, Ntapi.ntseapi, NtUtils.Objects,
  NtUtils.Access.Expected;

function NtxOpenProcess(out hProcess: THandle; PID: NativeUInt;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal = 0): TNtxStatus;
var
  ClientId: TClientId;
  ObjAttr: TObjectAttributes;
begin
  if PID = NtCurrentProcessId then
  begin
    hProcess := NtCurrentProcess;
    Result.Status := STATUS_SUCCESS;
  end
  else
  begin
    InitializeObjectAttributes(ObjAttr, nil, HandleAttributes);
    ClientId.Create(PID, 0);

    Result.Location := 'NtOpenProcess';
    Result.LastCall.CallType := lcOpenCall;
    Result.LastCall.AccessMask := DesiredAccess;
    Result.LastCall.AccessMaskType := TAccessMaskType.objNtProcess;

    Result.Status := NtOpenProcess(hProcess, DesiredAccess, ObjAttr, ClientId);
  end;
end;

function NtxOpenCurrentProcess(out hProcess: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal): TNtxStatus;
var
  Flags: Cardinal;
begin
  // Duplicating the pseudo-handle is more reliable then opening process by PID

  if DesiredAccess = MAXIMUM_ALLOWED then
  begin
    Flags := DUPLICATE_SAME_ACCESS;
    DesiredAccess := 0;
  end
  else
    Flags := 0;

  Result.Location := 'NtDuplicateObject';
  Result.Status := NtDuplicateObject(NtCurrentProcess, NtCurrentProcess,
    NtCurrentProcess, hProcess, DesiredAccess, HandleAttributes, Flags);
end;

function NtxQueryProcess(hProcess: THandle; InfoClass: TProcessInfoClass;
  out Status: TNtxStatus): Pointer;
var
  BufferSize, Required: Cardinal;
begin
  Status.Location := 'NtQueryInformationProcess';
  Status.LastCall.CallType := lcQuerySetCall;
  Status.LastCall.InfoClass := Cardinal(InfoClass);
  Status.LastCall.InfoClassType := TypeInfo(TProcessInfoClass);
  RtlxComputeProcessQueryAccess(Status.LastCall, InfoClass);

  BufferSize := 0;
  repeat
    Result := AllocMem(BufferSize);

    Required := 0;
    Status.Status := NtQueryInformationProcess(hProcess, InfoClass, Result,
      BufferSize, @Required);

    if not Status.IsSuccess then
    begin
      FreeMem(Result);
      Result := nil;
    end;
  until not NtxExpandBuffer(Status, BufferSize, Required);
end;

function NtxSetProcess(hProcess: THandle; InfoClass: TProcessInfoClass;
  Data: Pointer; DataSize: Cardinal): TNtxStatus;
begin
  Result.Location := 'NtSetInformationProcess';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TProcessInfoClass);
  RtlxComputeProcessSetAccess(Result.LastCall, InfoClass);

  Result.Status := NtSetInformationProcess(hProcess, InfoClass, Data, DataSize);
end;

class function NtxProcess.Query<T>(hProcess: THandle;
  InfoClass: TProcessInfoClass; out Buffer: T): TNtxStatus;
begin
  Result.Location := 'NtQueryInformationProcess';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TProcessInfoClass);
  RtlxComputeProcessQueryAccess(Result.LastCall, InfoClass);

  Result.Status := NtQueryInformationProcess(hProcess, InfoClass, @Buffer,
    SizeOf(Buffer), nil);
end;

class function NtxProcess.SetInfo<T>(hProcess: THandle;
  InfoClass: TProcessInfoClass; const Buffer: T): TNtxStatus;
begin
  Result := NtxSetProcess(hProcess, InfoClass, @Buffer, SizeOf(Buffer));
end;

function NtxEnumerateHandlesProcess(hProcess: THandle; out Handles:
  TArray<TProcessHandleEntry>): TNtxStatus;
var
  Buffer: PProcessHandleSnapshotInformation;
  i: Integer;
begin
  Buffer := NtxQueryProcess(hProcess, ProcessHandleInformation, Result);

  if Result.IsSuccess then
  begin
    SetLength(Handles, Buffer.NumberOfHandles);

    for i := 0 to High(Handles) do
      Handles[i] := Buffer.Handles{$R-}[i]{$R+};

    FreeMem(Buffer);
  end;
end;

function NtxQueryStringProcess(hProcess: THandle; InfoClass: TProcessInfoClass;
  out Str: String): TNtxStatus;
var
  Buffer: PUNICODE_STRING;
begin
  case InfoClass of
    ProcessImageFileName, ProcessImageFileNameWin32,
    ProcessCommandLineInformation:
    begin
      Buffer := NtxQueryProcess(hProcess, InfoClass, Result);

      if Result.IsSuccess then
      begin
        Str := Buffer.ToString;
        FreeMem(Buffer);
      end;
    end;
  else
    Result.Location := 'NtxQueryStringProcess';
    Result.Status := STATUS_INVALID_INFO_CLASS;
    Exit;
  end;
end;

function NtxTryQueryImageProcessById(PID: NativeUInt): String;
var
  hProcess: THandle;
begin
  Result := '';

  if not NtxOpenProcess(hProcess, PID, PROCESS_QUERY_LIMITED_INFORMATION
    ).IsSuccess then
    Exit;

  NtxQueryStringProcess(hProcess, ProcessImageFileNameWin32, Result);
  NtxSafeClose(hProcess);
end;

function NtxAssertNotWoW64: TNtxStatus;
var
  IsWoW64: NativeUInt;
begin
  Result := NtxProcess.Query<NativeUInt>(NtCurrentProcess,
    ProcessWow64Information, IsWoW64);

  if Result.IsSuccess and (IsWoW64 <> 0) then
  begin
    Result.Location := '[WoW64 assertion]';
    Result.Status := STATUS_ASSERTION_FAILURE;
  end;
end;

end.
