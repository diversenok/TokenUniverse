unit Ntapi.ntpsapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, Ntapi.ntdef;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000; // move to WinNT
  THREAD_QUERY_INFORMATION = $0040;
  THREAD_SET_THREAD_TOKEN = $0080;

type
  TProcessInfoClass = (
    ProcessAccessToken = 9, // s: TProcessAccessToken
    ProcessImageFileName = 27, // q: UNICODE_STRING
    ProcessImageFileNameWin32 = 43 // q: UNICODE_STRING
  );

  TThreadInfoClass = (
    ThreadImpersonationToken = 5 // s: THandle
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
  ProcessInformationClass: TProcessInfoClass; ProcessInformation: Pointer;
  ProcessInformationLength: Cardinal; ReturnLength: PCardinal): NTSTATUS;
  stdcall; external ntdll;

function NtSetInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: TProcessInfoClass; ProcessInformation: Pointer;
  ProcessInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtOpenThread(out ThreadHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes; const ClientId: TClientId):
  NTSTATUS; stdcall; external ntdll;

function NtSetInformationThread(ThreadHandle: THandle;
  ThreadInformationClass: TThreadInfoClass; ThreadInformation: Pointer;
  ThreadInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

implementation

end.
