unit Ntapi.ntrtl;
{$MINENUMSIZE 4}

interface

uses
  Ntapi.ntdef, NtApi.ntpebteb;

function RtlGetCurrentPeb: PPEB; stdcall; external ntdll;

implementation

end.
