unit Ntapi.ntdef;
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows;

type
  NTSTATUS = Cardinal;
  TAccessMask = ACCESS_MASK; // Move to WinNT

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

  TClientId = record
    UniqueProcess: THandle;
    UniqueThread: THandle;
  end;
  PClientId = ^TClientId;

const
  ntdll = 'ntdll.dll';

  OBJ_PROTECT_CLOSE = $00000001;
  OBJ_INHERIT = $00000002;
  OBJ_AUDIT_OBJECT_CLOSE = $00000004;
  OBJ_PERMANENT = $00000010;
  OBJ_EXCLUSIVE = $00000020;
  OBJ_CASE_INSENSITIVE = $00000040;
  OBJ_OPENIF  = $00000080;
  OBJ_OPENLINK  = $00000100;

function NT_SUCCESS(Status: NTSTATUS): Boolean; inline;
function NT_INFORMATION(Status: NTSTATUS): Boolean; inline;
function NT_WARNING(Status: NTSTATUS): Boolean; inline;
function NT_ERROR(Status: NTSTATUS): Boolean; inline;

procedure InitializeObjectAttributes(var ObjAttr: TObjectAttributes;
  ObjectName: PUNICODE_STRING; Attributes: Cardinal; RootDirectory: THandle = 0;
  QoS: PSecurityQualityOfService = nil); inline;

implementation

function NT_SUCCESS(Status: NTSTATUS): Boolean;
begin
  Result := Integer(Status) >= 0;
end;

function NT_INFORMATION(Status: NTSTATUS): Boolean;
begin
  Result := (Status shr 30) = 1;
end;

function NT_WARNING(Status: NTSTATUS): Boolean;
begin
  Result := (Status shr 30) = 2;
end;

function NT_ERROR(Status: NTSTATUS): Boolean;
begin
  Result := (Status shr 30) = 3;
end;

procedure InitializeObjectAttributes(var ObjAttr: TObjectAttributes;
  ObjectName: PUNICODE_STRING; Attributes: Cardinal; RootDirectory: THandle;
  QoS: PSecurityQualityOfService);
begin
  ObjAttr.Length := SizeOf(ObjAttr);
  ObjAttr.ObjectName := ObjectName;
  ObjAttr.Attributes := Attributes;
  ObjAttr.RootDirectory := RootDirectory;
  ObjAttr.SecurityQualityOfService := QoS;
end;

end.
