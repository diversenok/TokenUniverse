unit Ntapi.ntobapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

type
  TObjectInformationClass = (
    ObjectBasicInformation = 0, // q: TObjectBasicInformaion
    ObjectNameInformation = 1, // q: UNICODE_STRING
    ObjectTypeInformation = 2, // q: TObjectTypeInformation
    ObjectHandleFlagInformation = 4 // q+s: TObjectHandleFlagInformation
  );

  // TODO: ObjectTypesInformation for token type

  // ObjectBasicInformation
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

  TObjectHandleFlagInformation = record
    Inherit: Boolean;
    ProtectFromClose: Boolean;
  end;

const
  DUPLICATE_CLOSE_SOURCE = $00000001;
  DUPLICATE_SAME_ACCESS = $00000002;
  DUPLICATE_SAME_ATTRIBUTES = $00000004;

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
  Timeout: PLargeInteger): NTSTATUS; stdcall; external ntdll;

function NtSetSecurityObject(Handle: THandle;
  SecurityInformation: TSecurityInformation;
  SecurityDescriptor: PSecurityDescriptor): NTSTATUS; stdcall; external ntdll;

function NtQuerySecurityObject(Handle: THandle;
  SecurityInformation: TSecurityInformation;
  SecurityDescriptor: PSecurityDescriptor; Length: Cardinal;
  out LengthNeeded: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtClose(Handle: THandle): NTSTATUS; stdcall; external ntdll;

implementation

end.
