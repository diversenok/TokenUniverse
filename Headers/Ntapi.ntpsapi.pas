unit Ntapi.ntpsapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, Ntapi.ntdef;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000; // move to WinNT
  THREAD_QUERY_INFORMATION = $0040;

type
  TProcessInformationClass = (
    ProcessAccessToken = 9, // s: TProcessAccessToken
    ProcessImageFileName = 27, // q: UNICODE_STRING
    ProcessImageFileNameWin32 = 43 // q: UNICODE_STRING
  );

  // ProcessAccessToken
  TProcessAccessToken = record
    Token: THandle; // needs TOKEN_ASSIGN_PRIMARY
    Thread: THandle; // needs THREAD_QUERY_INFORMATION
  end;

function NtOpenProcess(out ProcessHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes; const ClientId: TClientId):
  NTSTATUS; stdcall; external ntdll;

function NtGetNextProcess(ProcessHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Flags: Cardinal; out NewProcessHandle: THandle):
  NTSTATUS; stdcall; external ntdll;

function NtGetNextThread(ProcessHandle: THandle; ThreadHandle: THandle;
  DesiredAccess: TAccessMask; HandleAttributes: Cardinal; Flags: Cardinal;
  out NewThreadHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtQueryInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: TProcessInformationClass;
  ProcessInformation: Pointer; ProcessInformationLength: Cardinal;
  ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

function NtSetInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: TProcessInformationClass;
  ProcessInformation: Pointer; ProcessInformationLength: Cardinal): NTSTATUS;
  stdcall; external ntdll;

implementation

end.
