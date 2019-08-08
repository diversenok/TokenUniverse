unit NtUtils.WinUser.WinstaLock;

interface

uses
  NtUtils.Exceptions;

// Lock/unlock current session's window station
function UsrxLockWindowStation(Lock: Boolean): TNtxStatus;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntdef, Winapi.WinUser, Ntapi.ntldr, Ntapi.ntpebteb,
  NtUtils.Ldr, NtUtils.Processes.Snapshots, NtUtils.Processes, NtUtils.Objects,
  NtUtils.Shellcode;

// User32.dll has a pair of functions called LockWindowStation and
// UnlockWindowStation. Although any application can call them, only calls
// issued by a registered instance of winlogon.exe will succeed.
// So, we inject a thread to winlogon to execute this call in its context.

type
  TGetProcessWindowStation = function: HWINSTA; stdcall;
  TLockWindowStation = function (hWinStation: HWINSTA): LongBool; stdcall;
  TRtlGetLastWin32Error = function: Cardinal; stdcall;

  TUsrxLockerParam = record
    GetProcessWindowStation: TGetProcessWindowStation;
    LockWindowStation: TLockWindowStation;
    RtlGetLastWin32Error: TRtlGetLastWin32Error;
  end;
  PWinStaPayload = ^TUsrxLockerParam;

// We are going to execute the following function inside winlogon, so make sure
// to use only functions and variables referenced through the Data parameter.
// Note: be consistent with the raw assembly below (the one we actually use).
function UsrxLockerPayload(Data: PWinStaPayload): NTSTATUS; stdcall;
begin
  if Data.LockWindowStation(Data.GetProcessWindowStation) then
    Result := STATUS_SUCCESS
  else
    Result := NTSTATUS_FROM_WIN32(Data.RtlGetLastWin32Error);
end;

const
  // Be consistent with function code above
  {$IFDEF WIN64}
  UsrxLockerAsm: array [0..77] of Byte = ($55, $48, $83, $EC, $30, $48, $8B,
    $EC, $48, $89, $4D, $40, $48, $8B, $45, $40, $FF, $10, $48, $89, $C1, $48,
    $8B, $45, $40, $FF, $50, $08, $85, $C0, $74, $09, $C7, $45, $2C, $00, $00,
    $00, $00, $EB, $1C, $48, $8B, $45, $40, $FF, $50, $10, $89, $45, $28, $8B,
    $45, $28, $81, $E0, $FF, $FF, $00, $00, $81, $C8, $00, $00, $07, $C0, $89,
    $45, $2C, $8B, $45, $2C, $48, $8D, $65, $30, $5D, $C3);
  {$ENDIF}
  {$IFDEF WIN32}
  UsrxLockerAsm: array [0..62] of Byte = ($55, $8B, $EC, $83, $C4, $F8, $8B,
    $45, $08, $FF, $10, $50, $8B, $45, $08, $FF, $50, $04, $85, $C0, $74, $07,
    $33, $C0, $89, $45, $FC, $EB, $19, $8B, $45, $08, $FF, $50, $08, $89, $45,
    $F8, $8B, $45, $F8, $25, $FF, $FF, $00, $00, $0D, $00, $00, $07, $C0, $89,
    $45, $FC, $8B, $45, $FC, $59, $59, $5D, $C2, $04, $00);
  {$ENDIF}

function GetLockerFunctionName(Lock: Boolean): String;
begin
  if Lock then
    Result := 'LockWindowStation'
  else
    Result := 'UnlockWindowStation';
end;

function UsrxLockerPrepare(var Data: TUsrxLockerParam; Lock: Boolean)
  : TNtxStatus;
var
  hUser32: HMODULE;
begin
  // Winlogon always loads user32.dll, so we don't need to check it
  Result := LdrxGetDllHandle(user32, hUser32);

  if not Result.IsSuccess then
    Exit;

  Data.GetProcessWindowStation := LdrxGetProcedureAddress(hUser32,
    'GetProcessWindowStation', Result);

  if not Result.IsSuccess then
    Exit;

  Data.LockWindowStation := LdrxGetProcedureAddress(hUser32,
    AnsiString(GetLockerFunctionName(Lock)), Result);

  if not Result.IsSuccess then
    Exit;

  Data.RtlGetLastWin32Error := LdrxGetProcedureAddress(hNtdll,
    'RtlGetLastWin32Error', Result);
end;

function UsrxLockWindowStation(Lock: Boolean): TNtxStatus;
var
  Param: TUsrxLockerParam;
  Processes: TArray<TProcessEntry>;
  hProcess: THandle;
  i, ind: Integer;
begin
  // Winlogon always has the same bitness as the OS. So should we.
  Result := NtxAssertNotWoW64;

  if not Result.IsSuccess then
    Exit;

  // Prepare the thread parameter
  Result := UsrxLockerPrepare(Param, Lock);

  if not Result.IsSuccess then
    Exit;

  // We need to find current session's winlogon
  Result := NtxEnumerateProcesses(Processes);

  if not Result.IsSuccess then
    Exit;

  NtxFilterProcessessByImage(Processes, 'winlogon.exe', ftInclude);

  ind := -1;
  for i := 0 to High(Processes) do
    if Processes[i].Process.SessionId = RtlGetCurrentPeb.SessionId then
    begin
      ind := i;
      Break;
    end;

  if ind = -1 then
  begin
    Result.Location := '[Searching for winlogon.exe]';
    Result.Status := STATUS_NOT_FOUND;
    Exit;
  end;

  // Open it
  Result := NtxOpenProcess(hProcess, Processes[ind].Process.ProcessId,
    PROCESS_INJECT_ACCESS);

  if not Result.IsSuccess then
    Exit;

  // Inject the assembly, create a new thread, and wait for the result
  Result := RtlxInvokeAssemblySyncProcess(hProcess, @UsrxLockerAsm,
    SizeOf(UsrxLockerAsm), @Param, SizeOf(Param),
    'Winlogon::' + GetLockerFunctionName(Lock));

  NtxSafeClose(hProcess);
end;

end.
