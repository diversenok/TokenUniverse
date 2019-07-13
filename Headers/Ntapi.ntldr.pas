unit Ntapi.ntldr;

{$MINENUMSIZE 4}

interface

uses
  Ntapi.ntdef;

function LdrLoadDll(DllPath: PWideChar; DllCharacteristics: PCardinal;
  const DllName: UNICODE_STRING; out DllHandle: HMODULE): NTSTATUS; stdcall;
  external ntdll;

function LdrUnloadDll(DllHandle: HMODULE): NTSTATUS; stdcall; external ntdll;

function LdrGetDllHandle(DllPath: PWideChar;
  DllCharacteristics: PCardinal; const DllName: UNICODE_STRING;
  out DllHandle: HMODULE): NTSTATUS; stdcall; external ntdll;

function LdrGetDllHandleByMapping(BaseAddress: Pointer; out DllHandle: HMODULE):
  NTSTATUS; stdcall; external ntdll;

function LdrGetDllHandleByName(BaseDllName: PUNICODE_STRING;
  FullDllName: PUNICODE_STRING; out DllHandle: HMODULE): NTSTATUS; stdcall;
  external ntdll;

function LdrGetDllFullName(DllHandle: Pointer; out FullDllName: UNICODE_STRING):
  NTSTATUS; stdcall; external ntdll;

function LdrGetDllDirectory(out DllDirectory: UNICODE_STRING): NTSTATUS;
  stdcall; external ntdll;

function LdrSetDllDirectory(const DllDirectory: UNICODE_STRING): NTSTATUS;
  stdcall; external ntdll;

function LdrGetProcedureAddress(DllHandle: HMODULE;
  const ProcedureName: ANSI_STRING; ProcedureNumber: Cardinal;
  out ProcedureAddress: Pointer): NTSTATUS; stdcall; external ntdll;

function LdrGetKnownDllSectionHandle(DllName: PWideChar; KnownDlls32: Boolean;
  out Section: THandle): NTSTATUS; stdcall; external ntdll;

function LdrQueryImageFileExecutionOptions(const SubKey: UNICODE_STRING;
  ValueName: PWideChar; ValueSize: Cardinal; Buffer: Pointer;
  BufferSize: Cardinal; ReturnedLength: PCardinal): NTSTATUS; stdcall;
  external ntdll;

function hNtdll: HMODULE;

implementation

var
  hNtdllInit: Boolean;
  hNtdllValue: HMODULE;

function hNtdll: HMODULE;
var
  FileName: UNICODE_STRING;
begin
  if not hNtdllInit then
  begin
    FileName.FromString(ntdll);
    hNtdllInit := NT_SUCCESS(LdrGetDllHandle(nil, nil, FileName, hNtdllValue));
  end;

  Result := hNtdllValue;
end;

end.
