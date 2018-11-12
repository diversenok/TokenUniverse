unit TU.NativeAPI;

interface

uses
  Winapi.Windows;

{$MINENUMSIZE 4}

type
  NTSTATUS = Cardinal;

const
  ntdll = 'ntdll.dll';

  STATUS_SUCCESS: NTSTATUS = $00000000;
  STATUS_UNSUCCESSFUL: NTSTATUS = $C0000001;

  STATUS_INFO_LENGTH_MISMATCH: NTSTATUS = $C0000004;
  STATUS_BUFFER_TOO_SMALL: NTSTATUS = $C0000023;
  STATUS_IMPLEMENTATION_LIMIT = $C000042B;

type
  UNICODE_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;

  { SystemInformation class }

  TSystemInformationClass = (
    SystemProcessInformation = 5, // q: TSystemProcessInformation
    SystemExtendedHandleInformation = 64 // q: TSystemHandleInformationEx
  );

  { SystemProcessInformation = 5 }

  TSystemProcessInformation = record
    NextEntryOffset: Cardinal;
    NumberOfThreads: Cardinal;
    Reserved: array [0 .. 5] of Int64;
    ImageName: UNICODE_STRING;
    BasePriority: Cardinal;
    ProcessId: NativeUInt;
    InheritedFromProcessId: NativeUInt;
  end;
  PSystemProcessInformation = ^TSystemProcessInformation;

  { SystemExtendedHandleInformation = 64 }

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

  TSystemHandleInformationEx = record
    NumberOfHandles: NativeUInt;
    Reserved: NativeUInt;
    Handles: array [Word] of TSystemHandleTableEntryInfoEx;
  end;
  PSystemHandleInformationEx = ^TSystemHandleInformationEx;

  { ObjectInformation class }

  TObjectInformationClass = (ObjectBasicInformation);

  TObjectBasicInformaion = record
    Attributes: Cardinal;
    GrantedAccess: ACCESS_MASK;
    HandleCount: Cardinal;
    PointerCount: Cardinal;
    Reserved: array [0..9] of Cardinal;
  end;

  // TODO: ObjectTypesInformation for token type

  { Ntdll api calls }

function NT_SUCCESS(Status: NTSTATUS): Boolean; inline;

function NtQuerySystemInformation(SystemInformationClass
  : TSystemInformationClass; SystemInformation: Pointer;
  SystemInformationLength: Cardinal; out ReturnLength: Cardinal): NTSTATUS;
  stdcall; external ntdll;

function NtQueryObject(ObjectHandle: THandle; ObjectInformationClass:
  TObjectInformationClass; ObjectInformation: Pointer; ObjectInformationLength:
  Cardinal; ReturnLength: PCardinal): LongWord; stdcall; external ntdll;

function NtCreateToken(var TokenHandle: THandle; DesiredAccess: ACCESS_MASK;
  ObjectAttributes: Pointer; TokenType: TTokenType; var AuthenticationId: LUID;
  var ExpirationTime: Int64; var User: TTokenUser; var Groups: TTokenGroups;
  var Privileges: TTokenPrivileges; var Owner: TTokenOwner;
  var PrimaryGroup: TTokenPrimaryGroup; DefaultDacl: PTokenDefaultDacl;
  var Source: TTokenSource): NTSTATUS; stdcall; external ntdll;

type
  TByteArray = array [Word] of Byte;
  PByteArray = ^TByteArray;

implementation

function NT_SUCCESS(Status: NTSTATUS): Boolean;
begin
  Result := Integer(Status) >= 0;
end;

end.
