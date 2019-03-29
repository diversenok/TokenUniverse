unit NtUtils.DelayedImport;

interface

function NtxCheckDelayedImport(Name: AnsiString): Boolean;

implementation

uses
  System.SysUtils, System.Generics.Collections, Ntapi.ntdef, Ntapi.ntldr,
  NtUtils.Exceptions;

var
  ImportCache: TDictionary<AnsiString,Boolean>;
  OldFailureHook: TDelayedLoadHook;

function NtxCheckDelayedImport(Name: AnsiString): Boolean;
var
  ProcName: ANSI_STRING;
  ProcAddr: Pointer;
begin
  if not Assigned(ImportCache) then
    ImportCache := TDictionary<AnsiString,Boolean>.Create;

  if ImportCache.TryGetValue(Name, Result) then
    Exit;

  ProcName.FromString(Name);
  Result := NT_SUCCESS(LdrGetProcedureAddress(hNtdll, ProcName, 0, ProcAddr));
  ImportCache.Add(Name, Result);
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
