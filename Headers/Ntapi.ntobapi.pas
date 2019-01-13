unit Ntapi.ntobapi;
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, Ntapi.ntdef;

type
  TObjectInformationClass = (
    ObjectBasicInformation = 0 // q: TObjectBasicInformaion
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
    Reserved: array [0..7] of Cardinal;
  end;
  PObjectBasicInformaion = ^TObjectBasicInformaion;

function NtQueryObject(ObjectHandle: THandle; ObjectInformationClass:
  TObjectInformationClass; ObjectInformation: Pointer; ObjectInformationLength:
  Cardinal; ReturnLength: PCardinal): NTSTATUS; stdcall; external ntdll;

function NtDuplicateObject(SourceProcessHandle: THandle;
  SourceHandle: THandle; TargetProcessHandle: THandle;
  out TargetHandle: THandle; DesiredAccess: TAccessMask;
  HandleAttributes: Cardinal; Options: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtClose(Handle: THandle): NTSTATUS; stdcall; external ntdll;

implementation

end.
