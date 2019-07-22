unit Ntapi.ntregapi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

const
  REG_PATH_MACHINE = '\Registry\Machine';
  REG_PATH_USER = '\Registry\User';
  REG_PATH_USER_DEFAULT = '\Registry\User\.Default';

  REG_SYMLINK_VALUE_NAME = 'SymbolicLinkValue';

  // WinNt.21186, access masks
  KEY_QUERY_VALUE = $0001;
  KEY_SET_VALUE = $0002;
  KEY_CREATE_SUB_KEY = $0004;
  KEY_ENUMERATE_SUB_KEYS = $0008;
  KEY_NOTIFY = $0010;
  KEY_CREATE_LINK = $0020;

  KEY_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $3F;

  // WinNt.21230, open/create options
  REG_OPTION_VOLATILE = $00000001;
  REG_OPTION_CREATE_LINK = $00000002;
  REG_OPTION_BACKUP_RESTORE = $00000004;

  // WinNt.21271, open/create disposition
  REG_CREATED_NEW_KEY = $00000001;
  REG_OPENED_EXISTING_KEY = $00000002;

  // WinNt.21285, load/restore flags
  REG_WHOLE_HIVE_VOLATILE = $00000001;
  REG_REFRESH_HIVE = $00000002;
  REG_NO_LAZY_FLUSH = $00000004;
  REG_FORCE_RESTORE = $00000008;
  REG_APP_HIVE = $00000010;
  REG_PROCESS_PRIVATE = $00000020;
  REG_START_JOURNAL = $00000040;
  REG_HIVE_EXACT_FILE_GROWTH = $00000080;
  REG_HIVE_NO_RM = $00000100;
  REG_HIVE_SINGLE_LOG = $00000200;
  REG_BOOT_HIVE = $00000400;
  REG_LOAD_HIVE_OPEN_HANDLE = $00000800;
  REG_FLUSH_HIVE_FILE_GROWTH = $00001000;
  REG_OPEN_READ_ONLY = $00002000;
  REG_IMMUTABLE = $00004000;

type
  // WinNt.21333, value types
  TRegValueType = (
    REG_NONE = 0,
    REG_SZ = 1,
    REG_EXPAND_SZ = 2,
    REG_BINARY = 3,
    REG_DWORD = 4,
    REG_DWORD_BIG_ENDIAN = 5,
    REG_LINK = 6,
    REG_MULTI_SZ = 7,
    REG_RESOURCE_LIST = 8,
    REG_FULL_RESOURCE_DESCRIPTOR = 9,
    REG_RESOURCE_REQUIREMENTS_LIST = 10,
    REG_QWORD = 11
  );

  TKeyInformationClass = (
    KeyBasicInformation = 0,          // TKeyBasicInformation
    KeyNodeInformation = 1,
    KeyFullInformation = 2,
    KeyNameInformation = 3,           // TKeyNameInformation
    KeyCachedInformation = 4,
    KeyFlagsInformation = 5,          // Cardinal
    KeyVirtualizationInformation = 6, // Cardinal
    KeyHandleTagsInformation = 7,     // Cardinal
    KeyTrustInformation = 8,          // Cardinal
    KeyLayerInformation = 9
  );

  TKeyBasicInformation = record
    LastWriteTime: TLargeInteger;
    TitleIndex: Cardinal;
    NameLength: Cardinal;
    Name: array [ANYSIZE_ARRAY] of WideChar;
  end;
  PKeyBasicInformation = ^TKeyBasicInformation;

  TKeyNameInformation = record
    NameLength: Cardinal;
    Name: array [ANYSIZE_ARRAY] of WideChar;
  end;
  PKeyNameInformation = ^TKeyNameInformation;

  TKeySetInformationClass = (
    KeyWriteTimeInformation = 0,         // TLargeInteger
    KeyWow64FlagsInformation = 1,        // Cardinal
    KeyControlFlagsInformation = 2,      // Cardinal
    KeySetVirtualizationInformation = 3, // Cardinal
    KeySetDebugInformation = 4,
    KeySetHandleTagsInformation = 5,     // Cardinal
    KeySetLayerInformation = 6           // Cardinal
  );

  TKeyValueInformationClass = (
    KeyValueBasicInformation = 0,       // TKeyValueBasicInformation
    KeyValueFullInformation = 1,
    KeyValuePartialInformation = 2,     // TKeyValuePartialInfromation
    KeyValueFullInformationAlign64 = 3,
    KeyValuePartialInformationAlign64 = 4,
    KeyValueLayerInformation = 5
  );

  TKeyValueBasicInformation = record
    TitleIndex: Cardinal;
    ValueType: TRegValueType;
    NameLength: Cardinal;
    Name: array [ANYSIZE_ARRAY] of WideChar;
  end;
  PKeyValueBasicInformation = ^TKeyValueBasicInformation;

  TKeyValuePartialInfromation = record
    TitleIndex: Cardinal;
    ValueType: TRegValueType;
    DataLength: Cardinal;
    Data: array [ANYSIZE_ARRAY] of Byte;
  end;
  PKeyValuePartialInfromation = ^TKeyValuePartialInfromation;

function NtCreateKey(out KeyHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes; TitleIndex: Cardinal;
  ClassName: PUNICODE_STRING; CreateOptions: Cardinal; Disposition: PCardinal):
  NTSTATUS; stdcall; external ntdll;

function NtOpenKeyEx(out KeyHandle: THandle; DesiredAccess: TAccessMask;
  const ObjectAttributes: TObjectAttributes; OpenOptions: Cardinal): NTSTATUS;
    stdcall; external ntdll;

function NtDeleteKey(KeyHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtRenameKey(KeyHandle: THandle; const NewName: UNICODE_STRING):
  NTSTATUS; stdcall; external ntdll;

function NtDeleteValueKey(KeyHandle: THandle; const ValueName: UNICODE_STRING):
  NTSTATUS; stdcall; external ntdll;

function NtQueryKey(KeyHandle: THandle; KeyInformationClass:
  TKeyInformationClass; KeyInformation: Pointer; Length: Cardinal;
  out ResultLength: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtSetInformationKey(KeyHandle: THandle; KeySetInformationClass:
  TKeySetInformationClass; KeySetInformation: Pointer;
  KeySetInformationLength: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtQueryValueKey(KeyHandle: THandle; const ValueName: UNICODE_STRING;
  KeyValueInformationClass: TKeyValueInformationClass; KeyValueInformation:
  Pointer; Length: Cardinal; out ResultLength: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtSetValueKey(KeyHandle: THandle; const ValueName: UNICODE_STRING;
  TitleIndex: Cardinal; ValueType: TRegValueType; Data: Pointer;
  DataSize: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtEnumerateKey(KeyHandle: THandle; Index: Cardinal;
  KeyInformationClass: TKeyInformationClass; KeyInformation: Pointer;
  Length: Cardinal; out ResultLength: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtEnumerateValueKey(KeyHandle: THandle; Index: Cardinal;
  KeyValueInformationClass: TKeyValueInformationClass;
  KeyValueInformation: Pointer; Length: Cardinal; out ResultLength: Cardinal):
  NTSTATUS; stdcall; external ntdll;

function NtFlushKey(KeyHandle: THandle): NTSTATUS; stdcall; external ntdll;

function NtCompressKey(Key: THandle): NTSTATUS; stdcall; external ntdll;

function NtLoadKey(const TargetKey: TObjectAttributes;
  const SourceFile: TObjectAttributes): NTSTATUS; stdcall; external ntdll;

function NtSaveKey(KeyHandle: THandle; FileHandle: THandle): NTSTATUS; stdcall;
  external ntdll;

function NtUnloadKey(const TargetKey: TObjectAttributes): NTSTATUS; stdcall;
  external ntdll;

function NtQueryOpenSubKeys(const TargetKey: TObjectAttributes;
  out HandleCount: Cardinal): NTSTATUS; stdcall; external ntdll;

function NtFreezeRegistry(TimeOutInSeconds: Cardinal): NTSTATUS; stdcall;
  external ntdll;

function NtThawRegistry: NTSTATUS; stdcall; external ntdll;

implementation

end.
