unit NtUtils.DelayedImport;

interface

uses
  NtUtils.Exceptions;

function NtxCheckNtDelayedImport(Name: AnsiString): TNtxStatus;

function NtxCheckModuleDelayedImport(ModuleName: String;
  ProcedureName: AnsiString): TNtxStatus;

implementation

uses
  System.SysUtils, System.Generics.Collections, Ntapi.ntdef, Ntapi.ntldr,
  Ntapi.ntstatus;

var
  ImportCache: TDictionary<AnsiString, NTSTATUS>;
  OldFailureHook: TDelayedLoadHook;

function NtxCheckNtDelayedImport(Name: AnsiString): TNtxStatus;
var
  ProcName: ANSI_STRING;
  ProcAddr: Pointer;
begin
  if not Assigned(ImportCache) then
    ImportCache := TDictionary<AnsiString,NTSTATUS>.Create;

  Result.Location := 'LdrGetProcedureAddress';
  if ImportCache.TryGetValue(Name, Result.Status) then
    Exit;

  ProcName.FromString(Name);
  Result.Status := LdrGetProcedureAddress(hNtdll, ProcName, 0, ProcAddr);
  ImportCache.Add(Name, Result.Status);
end;

function NtxCheckModuleDelayedImport(ModuleName: String;
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
      EWinError.Report(pdli.dwLastError, DELAY_MSG + pdli.szDll);
    dliFailGetProcAddress:
      EWinError.Report(pdli.dwLastError, DELAY_MSG + pdli.dlp.szProcName);
  end;

  if Assigned(OldFailureHook) then
    OldFailureHook(dliNotify, pdli);

  Result := nil;
end;

initialization
  OldFailureHook := SetDliFailureHook2(NtxpDelayedLoadHook);
finalization
  ImportCache.Free;
end.
