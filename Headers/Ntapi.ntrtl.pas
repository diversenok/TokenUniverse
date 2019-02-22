unit Ntapi.ntrtl;
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef, NtApi.ntpebteb;

function RtlGetCurrentPeb: PPeb; stdcall; external ntdll;

function RtlLengthRequiredSid(SubAuthorityCount: Cardinal): Cardinal;
  stdcall; external ntdll;

function RtlSubAuthoritySid(Sid: PSid; SubAuthority: Cardinal): PCardinal;
  stdcall; external ntdll;

function RtlSubAuthorityCountSid(Sid: PSid): PByte; stdcall; external ntdll;

procedure RtlFreeSid(Sid: PSid); stdcall; external ntdll;

function RtlInitializeSid(Sid: PSid; const IdentifierAuthority:
  TSidIdentifierAuthority; SubAuthorityCount: Byte): NTSTATUS; stdcall;
  external ntdll;

function RtlGetNtGlobalFlags: Cardinal; stdcall; external ntdll;

implementation

end.
