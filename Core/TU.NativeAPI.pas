unit TU.NativeAPI;

interface

uses
  Winapi.Windows, TU.Winapi;

{$MINENUMSIZE 4}

type
  NTSTATUS = Cardinal;

const
  ntdll = 'ntdll.dll';

  STATUS_SUCCESS: NTSTATUS = $00000000;
  STATUS_UNSUCCESSFUL: NTSTATUS = $C0000001;

  STATUS_INFO_LENGTH_MISMATCH: NTSTATUS = $C0000004;
  STATUS_ACCESS_DENIED: NTSTATUS = $C0000022;
  STATUS_BUFFER_TOO_SMALL: NTSTATUS = $C0000023;
  STATUS_PRIVILEGE_NOT_HELD: NTSTATUS = $C0000061;
  STATUS_BAD_IMPERSONATION_LEVEL: NTSTATUS = $C00000A5;
  STATUS_NOT_SUPPORTED: NTSTATUS = $C00000BB;
  STATUS_IMPLEMENTATION_LIMIT: NTSTATUS = $C000042B;

type
  UNICODE_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;
  PUNICODE_STRING = ^UNICODE_STRING;

  TObjectAttributes = record
    Length: Cardinal;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: Cardinal;
    SecurityDescriptor: PSecurityDescriptor;
    SecurityQualityOfService: PSecurityQualityOfService;
  end;
  PObjectAttributes = ^TObjectAttributes;

  { SystemInformation class }

  TSystemInformationClass = (
    SystemProcessInformation = 5, // q: TSystemProcessInformation
    SystemExtendedHandleInformation = 64 // q: TSystemHandleInformationEx
  );

  // SystemProcessInformation
  TSystemProcessInformation = record
    NextEntryOffset: Cardinal;
    NumberOfThreads: Cardinal;
    Reserved: array [0 .. 2] of Int64;
    CreateTime: Int64;
    UserTime: Int64;
    KernelTime: Int64;
    ImageName: UNICODE_STRING;
    BasePriority: Cardinal;
    ProcessId: NativeUInt;
    InheritedFromProcessId: NativeUInt;
    HandleCount: Cardinal;
    SessionId: Cardinal;
  end;
  PSystemProcessInformation = ^TSystemProcessInformation;

  TSystemHandleTableEntryInfoEx = record
    PObject: Pointer;
    UniqueProcessId: NativeUInt;
    HandleValue: NativeUInt;
    GrantedAccess: ACCESS_MASK;
    CreatorBackTraceIndex: Word;
    ObjectTypeIndex: Word;
    HandleAttributes: Cardinal;
    Reserved: Cardinal;
  end;
  PSystemHandleTableEntryInfoEx = ^TSystemHandleTableEntryInfoEx;

  // SystemExtendedHandleInformation
  TSystemHandleInformationEx = record
    NumberOfHandles: NativeUInt;
    Reserved: NativeUInt;
    Handles: array [Word] of TSystemHandleTableEntryInfoEx;
  end;
  PSystemHandleInformationEx = ^TSystemHandleInformationEx;

  { ObjectInformation class }

  TObjectInformationClass = (ObjectBasicInformation);

  // ObjectBasicInformation
  TObjectBasicInformaion = record
    Attributes: Cardinal;
    GrantedAccess: ACCESS_MASK;
    HandleCount: Cardinal;
    PointerCount: Cardinal;
    Reserved: array [0..9] of Cardinal;
  end;

  { ProcessInformation class }

  TProcessInformationClass = (ProcessAccessToken = 9);

  // ProcessAccessToken
  TProcessAccessToken = record
    Token: THandle; // needs TOKEN_ASSIGN_PRIMARY
    Thread: THandle; // needs THREAD_QUERY_INFORMATION
  end;

  // TODO: ObjectTypesInformation for token type

  { Ntdll api calls }

function NT_SUCCESS(Status: NTSTATUS): Boolean; inline;
function NtGetCurrentSession: Cardinal;

function NtQuerySystemInformation(SystemInformationClass
  : TSystemInformationClass; SystemInformation: Pointer;
  SystemInformationLength: Cardinal; out ReturnLength: Cardinal): NTSTATUS;
  stdcall; external ntdll;

function NtQueryObject(ObjectHandle: THandle; ObjectInformationClass:
  TObjectInformationClass; ObjectInformation: Pointer; ObjectInformationLength:
  Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

function NtSetInformationProcess(ProcessHandle: THandle;
  ProcessInformationClass: TProcessInformationClass;
  ProcessInformation: Pointer; ProcessInformationLength: Cardinal): NTSTATUS;
  stdcall; external ntdll;

function NtGetNextThread(ProcessHandle: THandle; ThreadHandle: THandle;
  DesiredAccess: ACCESS_MASK; HandleAttributes: Cardinal; Flags: Cardinal;
  out NewThreadHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtDuplicateToken(ExistingTokenHandle: THandle;
  DesiredAccess: ACCESS_MASK; ObjectAttributes: PObjectAttributes;
  EffectiveOnly: LongBool; TokenType: TTokenType; out NewTokenHandle: THandle)
  : NTSTATUS; stdcall; external ntdll;

function NtFilterToken(ExistingTokenHandle: THandle; Flags: Cardinal;
  SidsToDisable: PTokenGroups; PrivilegesToDelete: PTokenPrivileges;
  RestrictedSids: PTokenGroups; out NewTokenHandle: THandle): NTSTATUS;
  stdcall; external ntdll;

function NtCreateToken(out TokenHandle: THandle; DesiredAccess: ACCESS_MASK;
  ObjectAttributes: PObjectAttributes; TokenType: TTokenType;
  AuthenticationId: PLUID; ExpirationTime: PInt64; User: PTokenUser;
  Groups: PTokenGroups; Privileges: PTokenPrivileges; Owner: PTokenOwner;
  PrimaryGroup: PTokenPrimaryGroup; DefaultDacl: PTokenDefaultDacl;
  Source: PTokenSource): NTSTATUS; stdcall; external ntdll;

implementation

function NT_SUCCESS(Status: NTSTATUS): Boolean;
begin
  Result := Integer(Status) >= 0;
end;

function RtlGetCurrentPeb: Pointer; stdcall; external ntdll;

function NtGetCurrentSession: Cardinal;
begin
  // Current session ID is always stored in the PEB
  {$POINTERMATH ON}
  try
    // We use hardcoded offsets of SessionID field from PPEB structure
    {$IFDEF WIN64}
      Result := PCardinal(PByte(RtlGetCurrentPeb) + $2C0)^;
    {$ELSE}
      Result := PCardinal(PByte(RtlGetCurrentPeb) + $1D4)^;
    {$ENDIF}
  except
    Result := 0;
    OutputDebugString('Exception while reading PEB');
  end;
  {$POINTERMATH OFF}
end;

end.
