unit Ntapi.ntldr;

interface

uses
  Ntapi.ntdef;

function LdrLoadDll(DllPath: PWideChar; DllCharacteristics: PCardinal;
  const DllName: UNICODE_STRING; out DllHandle: HMODULE): NTSTATUS; stdcall;
  external ntdll;

function LdrGetDllHandle(DllPath: PWideChar;
  DllCharacteristics: PCardinal; const DllName: UNICODE_STRING;
  out DllHandle: HMODULE): NTSTATUS; stdcall; external ntdll;

function LdrGetProcedureAddress(DllHandle: HMODULE;
  const ProcedureName: ANSI_STRING; ProcedureNumber: Cardinal;
  out ProcedureAddress: Pointer): NTSTATUS; stdcall; external ntdll;

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
