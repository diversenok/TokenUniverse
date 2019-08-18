unit NtUtils.Sections;

interface

uses
  Winapi.WinNt, Ntapi.ntmmapi, NtUtils.Exceptions;

// Create a section
function NtxCreateSection(out hSection: THandle; MaximumSize: UInt64;
  PageProtection: Cardinal; AllocationAttributes: Cardinal = SEC_COMMIT;
  hFile: THandle = 0; ObjectName: String = ''; RootDirectory: THandle = 0;
  HandleAttributes: Cardinal = 0): TNtxStatus;

// Open a section
function NtxOpenSection(out hSection: THandle; DesiredAccess: TAccessMask;
  ObjectName: String; RootDirectory: THandle = 0; HandleAttributes
  : Cardinal = 0): TNtxStatus;

// Map a section
function NtxMapViewOfSection(hSection: THandle; hProcess: THandle;
  Win32Protect: Cardinal; out Status: TNtxStatus; SectionOffset: UInt64 = 0;
  Size: NativeUInt = 0): Pointer;

type
  NtxSection = class
    // Query fixed-size information
    class function Query<T>(hSection: THandle;
      InfoClass: TSectionInformationClass; out Buffer: T): TNtxStatus; static;
  end;

implementation

uses
  Ntapi.ntdef, Ntapi.ntpsapi, NtUtils.Access.Expected;

function NtxCreateSection(out hSection: THandle; MaximumSize: UInt64;
  PageProtection, AllocationAttributes: Cardinal; hFile: THandle;
  ObjectName: String; RootDirectory: THandle; HandleAttributes: Cardinal)
  : TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  NameStr: UNICODE_STRING;
begin
  if ObjectName <> '' then
  begin
    NameStr.FromString(ObjectName);
    InitializeObjectAttributes(ObjAttr, @NameStr, HandleAttributes,
      RootDirectory);
  end
  else
    InitializeObjectAttributes(ObjAttr, nil, HandleAttributes);

  // TODO: Expected file handle access

  Result.Location := 'NtCreateSection';
  Result.Status := NtCreateSection(hSection, SECTION_ALL_ACCESS, @ObjAttr,
    @MaximumSize, PageProtection, AllocationAttributes, 0);
end;

function NtxOpenSection(out hSection: THandle; DesiredAccess: TAccessMask;
  ObjectName: String; RootDirectory: THandle; HandleAttributes: Cardinal):
  TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  NameStr: UNICODE_STRING;
begin
  NameStr.FromString(ObjectName);
  InitializeObjectAttributes(ObjAttr, @NameStr, HandleAttributes,
    RootDirectory);

  Result.Location := 'NtOpenSection';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := objNtSection;

  Result.Status := NtOpenSection(hSection, DesiredAccess, ObjAttr);
end;

function NtxMapViewOfSection(hSection: THandle; hProcess: THandle;
  Win32Protect: Cardinal; out Status: TNtxStatus; SectionOffset: UInt64;
  Size: NativeUInt): Pointer;
var
  ViewSize: NativeUInt;
begin
  Status.Location := 'NtMapViewOfSection';
  RtlxComputeSectionMapAccess(Status.LastCall, Win32Protect);
  Status.LastCall.Expects(PROCESS_VM_OPERATION, objNtProcess);

  Result := nil;
  ViewSize := Size;
  Status.Status := NtMapViewOfSection(hSection, hProcess, Result, 0, 0,
    @SectionOffset, ViewSize, ViewUnmap, 0, Win32Protect);
end;

class function NtxSection.Query<T>(hSection: THandle;
  InfoClass: TSectionInformationClass; out Buffer: T): TNtxStatus;
begin
  Result.Location := 'NtQuerySection';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TSectionInformationClass);
  Result.LastCall.Expects(SECTION_QUERY, objNtSection);

  Result.Status := NtQuerySection(hSection, InfoClass, @Buffer, SizeOf(Buffer),
    nil);
end;

end.
