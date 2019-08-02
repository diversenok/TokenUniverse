unit Ntapi.ntobapi;

{$WARN SYMBOL_PLATFORM OFF}
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

const
  DIRECTORY_QUERY = $0001;
  DIRECTORY_TRAVERSE = $0002;
  DIRECTORY_CREATE_OBJECT = $0004;
  DIRECTORY_CREATE_SUBDIRECTORY = $0008;
  DIRECTORY_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $000f;

  SYMBOLIC_LINK_QUERY = $0001;
  SYMBOLIC_LINK_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $0001;

  DUPLICATE_CLOSE_SOURCE = $00000001;
  DUPLICATE_SAME_ACCESS = $00000002;
  DUPLICATE_SAME_ATTRIBUTES = $00000004;

  // rev
  OB_TYPE_INDEX_TABLE_TYPE_OFFSET = 2;


type
  TObjectInformationClass = (
    ObjectBasicInformation = 0,     // q: TObjectBasicInformaion
    ObjectNameInformation = 1,      // q: UNICODE_STRING
    ObjectTypeInformation = 2,      // q: TObjectTypeInformation
    ObjectTypesInformation = 3,     // q: TObjectTypesInformation + TObjectTypeInformation
    ObjectHandleFlagInformation = 4 // q+s: TObjectHandleFlagInformation
  );

  TObjectBasicInformaion = record
    Attributes: Cardinal;
    GrantedAccess: TAccessMask;
    HandleCount: Cardinal;
    PointerCount: Cardinal;
    PagedPoolCharge: Cardinal;
    NonPagedPoolCharge: Cardinal;
    Reserved: array [0..2] of Cardinal;
    NameInfoSize: Cardinal;
    TypeInfoSize: Cardinal;
    SecurityDescriptorSize: Cardinal;
    CreationTime: TLargeInteger;
  end;
  PObjectBasicInformaion = ^TObjectBasicInformaion;

  TObjectTypeInformation = record
    TypeName: UNICODE_STRING;
    TotalNumberOfObjects: Cardinal;
    TotalNumberOfHandles: Cardinal;
    TotalPagedPoolUsage: Cardinal;
    TotalNonPagedPoolUsage: Cardinal;
    TotalNamePoolUsage: Cardinal;
    TotalHandleTableUsage: Cardinal;
    HighWaterNumberOfObjects: Cardinal;
    HighWaterNumberOfHandles: Cardinal;
    HighWaterPagedPoolUsage: Cardinal;
    HighWaterNonPagedPoolUsage: Cardinal;
    HighWaterNamePoolUsage: Cardinal;
    HighWaterHandleTableUsage: Cardinal;
    InvalidAttributes: Cardinal;
    GenericMapping: TGenericMapping;
    ValidAccessMask: Cardinal;
    SecurityRequired: Boolean;
    MaintainHandleCount: Boolean;
    TypeIndex: Byte;
    ReservedByte: Byte;
    PoolType: Cardinal;
    DefaultPagedPoolCharge: Cardinal;
    DefaultNonPagedPoolCharge: Cardinal;
  end;
  PObjectTypeInformation = ^TObjectTypeInformation;

  TObjectTypesInformation = record
    NumberOfTypes: Cardinal;
    // + aligned array of [0..NumberOfTypes - 1] of TObjectTypeInformation
  end;
  PObjectTypesInformation = ^TObjectTypesInformation;

  TObjectHandleFlagInformation = record
    Inherit: Boolean;
    ProtectFromClose: Boolean;
  end;

  TObjectDirectoryInformation = record
    Name: UNICODE_STRING;
    TypeName: UNICODE_STRING;
  end;
  PObjectDirectoryInformation = ^TObjectDirectoryInformation;

function NtQueryObject(ObjectHandle: THandle; ObjectInformationClass:
  TObjectInformationClass; ObjectInformation: Pointer; ObjectInformationLength:
  Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

function NtSetInformationObject(Handle: THandle;
  ObjectInformationClass: TObjectInformationClass; ObjectInformation: Pointer;
  ObjectInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtDuplicateObject(SourceProcessHandle: THandle;
  SourceHandle: THandle; TargetProcessHandle: THandle;
  out TargetHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Options: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtMakeTemporaryObject(Handle: THandle): NTSTATUS; stdcall;
  external ntdll;

function NtMakePermanentObject(Handle: THandle): NTSTATUS; stdcall;
  external ntdll;

function NtWaitForSingleObject(Handle: THandle; Alertable: LongBool;
  var Timeout: TLargeInteger): NTSTATUS; stdcall; external ntdll; overload;

function NtWaitForSingleObject(Handle: THandle; Alertable: LongBool;
  pTimeout: PLargeInteger = nil): NTSTATUS; stdcall; external ntdll; overload;

function NtSetSecurityObject(Handle: THandle;
  SecurityInformation: TSecurityInformation;
  const SecurityDescriptor: TSecurityDescriptor): NTSTATUS; stdcall;
  external ntdll;

function NtQuerySecurityObject(Handle: THandle;
  SecurityInformation: TSecurityInformation;
  SecurityDescriptor: PSecurityDescriptor; Length: Cardinal;
  out LengthNeeded: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtClose(Handle: THandle): NTSTATUS; stdcall; external ntdll;

// Win 10 THRESHOLD+
function NtCompareObjects(FirstObjectHandle: THandle;
  SecondObjectHandle: THandle): NTSTATUS; stdcall; external ntdll delayed;

function NtCreateDirectoryObject(out DirectoryHandle: THandle; DesiredAccess:
  TAccessMask; const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall;
  external ntdll;

function NtOpenDirectoryObject(out DirectoryHandle: THandle; DesiredAccess:
  TAccessMask; const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall;
  external ntdll;

function NtQueryDirectoryObject(DirectoryHandle: THandle;
  Buffer: Pointer; Length: Cardinal; ReturnSingleEntry: Boolean;
  RestartScan: Boolean; var Context: Cardinal; ReturnLength: PCardinal):
  NTSTATUS; stdcall; external ntdll;

function NtCreateSymbolicLinkObject(out LinkHandle: THandle; DesiredAccess:
  TAccessMask; const ObjectAttributes: TObjectAttributes; const LinkTarget:
  UNICODE_STRING): NTSTATUS; stdcall; external ntdll;

function NtOpenSymbolicLinkObject(out LinkHandle: THandle; DesiredAccess:
  TAccessMask; const ObjectAttributes: TObjectAttributes): NTSTATUS; stdcall;
  external ntdll;

function NtQuerySymbolicLinkObject(LinkHandle: THandle; var LinkTarget:
  UNICODE_STRING; ReturnedLength: PCardinal): NTSTATUS; stdcall;
  external ntdll;

implementation

end.
