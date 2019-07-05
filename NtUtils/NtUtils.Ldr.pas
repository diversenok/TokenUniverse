unit NtUtils.Ldr;

interface

uses
  NtUtils.Exceptions;

{ Delayed import }

// Check if a function presents in ntdll
function LdrxCheckNtDelayedImport(Name: AnsiString): TNtxStatus;

// Check if a function presents in a dll
function LdrxCheckModuleDelayedImport(ModuleName: String;
  ProcedureName: AnsiString): TNtxStatus;

{ Other }

// Get base address of a loaded dll
function LdrxGetDllHandle(DllName: String; out DllHandle: HMODULE): TNtxStatus;

// Get a function address
function LdrxGetProcedureAddress(DllHandle: HMODULE; ProcedureName: AnsiString;
  out Address: Pointer): TNtxStatus;

implementation

uses
  System.SysUtils, System.Generics.Collections, Ntapi.ntdef, Ntapi.ntldr;

var
  ImportCache: TDictionary<AnsiString, NTSTATUS>;
  OldFailureHook: TDelayedLoadHook;

function LdrxCheckNtDelayedImport(Name: AnsiString): TNtxStatus;
var
  ProcName: ANSI_STRING;
  ProcAddr: Pointer;
begin
  if not Assigned(ImportCache) then
    ImportCache := TDictionary<AnsiString,NTSTATUS>.Create;

  Result.Location := 'LdrGetProcedureAddress("' + String(Name) + '")';
  if ImportCache.TryGetValue(Name, Result.Status) then
    Exit;

  ProcName.FromString(Name);
  Result.Status := LdrGetProcedureAddress(hNtdll, ProcName, 0, ProcAddr);
  ImportCache.Add(Name, Result.Status);
end;

function LdrxCheckModuleDelayedImport(ModuleName: String;
  ProcedureName: AnsiString): TNtxStatus;
var
  DllName: UNICODE_STRING;
  ProcName: ANSI_STRING;
  hDll: NativeUInt;
  ProcAddr: Pointer;
begin
  DllName.FromString(ModuleName);

  Result.Location := 'LdrGetDllHandle';
  Result.Status := LdrGetDllHandle(nil, nil, DllName, hDll);

  if not NT_SUCCESS(Result.Status) then
  begin
    // Try to load it
    Result.Location := 'LdrLoadDll';
    Result.Status := LdrLoadDll(nil, nil, DllName, hDll);

    if not NT_SUCCESS(Result.Status) then
      Exit;
  end;

  ProcName.FromString(ProcedureName);

  Result.Location := 'LdrGetProcedureAddress';
  Result.Status := LdrGetProcedureAddress(hDll, ProcName, 0, ProcAddr);
end;

function NtxpDelayedLoadHook(dliNotify: dliNotification;
  pdli: PDelayLoadInfo): Pointer; stdcall;
const
  DELAY_MSG = 'Delayed load of ';
begin
  // Report delayed load errors
  case dliNotify of
    dliFailLoadLibrary:
      ENtError.Report(NTSTATUS_FROM_WIN32(pdli.dwLastError),
        DELAY_MSG + pdli.szDll);
    dliFailGetProcAddress:
      ENtError.Report(NTSTATUS_FROM_WIN32(pdli.dwLastError),
        DELAY_MSG + pdli.dlp.szProcName);
  end;

  if Assigned(OldFailureHook) then
    OldFailureHook(dliNotify, pdli);

  Result := nil;
end;

function LdrxGetDllHandle(DllName: String; out DllHandle: HMODULE): TNtxStatus;
var
  DllNameStr: UNICODE_STRING;
begin
  DllNameStr.FromString(DllName);

  Result.Location := 'LdrGetDllHandle("' + DllName + '")';
  Result.Status := LdrGetDllHandle(nil, nil, DllNameStr, DllHandle);
end;

function LdrxGetProcedureAddress(DllHandle: HMODULE; ProcedureName: AnsiString;
  out Address: Pointer): TNtxStatus;
var
  ProcNameStr: ANSI_STRING;
begin
  ProcNameStr.FromString(ProcedureName);

  Result.Location := 'LdrGetProcedureAddress("' + String(ProcedureName) + '")';
  Result.Status := LdrGetProcedureAddress(DllHandle, ProcNameStr, 0, Address);
end;

initialization
  OldFailureHook := SetDliFailureHook2(NtxpDelayedLoadHook);
finalization
  ImportCache.Free;
end.
