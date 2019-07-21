unit NtUtils.Registry;

interface

uses
  Winapi.WinNt, Ntapi.ntregapi, NtUtils.Exceptions;

type
  TRegValueType = Ntapi.ntregapi.TRegValueType;

  TKeyBasicInfo = record
    LastWriteTime: TLargeInteger;
    TitleIndex: Cardinal;
    Name: String;
  end;

  TRegValueEntry = record
    ValueType: TRegValueType;
    ValueName: String;
  end;

{ Keys }

// Open a key
function NtxOpenKey(out hKey: THandle; Name: String; DesiredAccess: TAccessMask;
  Root: THandle = 0; OpenOptions: Cardinal = 0; Attributes: Cardinal = 0)
  : TNtxStatus;

// Create a key
function NtxCreateKey(out hKey: THandle; Name: String;
  DesiredAccess: TAccessMask; Root: THandle = 0; CreateOptions: Cardinal = 0;
  Attributes: Cardinal = 0; Disposition: PCardinal = nil): TNtxStatus;

// Delete a key
function NtxDeleteKey(hKey: THandle): TNtxStatus;

// Rename a key
function NtxRenameKey(hKey: THandle; NewName: String): TNtxStatus;

// Enumerate sub-keys
function NtxEnumerateSubKeys(hKey: THandle; out SubKeys: TArray<String>)
  : TNtxStatus;

// Query variable-length key information
function NtxQueryInformationKey(hKey: THandle; InfoClass: TKeyInformationClass;
  out Status: TNtxStatus): Pointer;

// Query key basic information
function NtxQueryBasicKey(hKey: THandle; out Info: TKeyBasicInfo): TNtxStatus;

type
  NtxKey = class
    // Query fixed-size key information
    class function Query<T>(hKey: THandle; InfoClass: TKeyInformationClass;
      out Buffer: T): TNtxStatus; static;

    // Set fixed-size key information
    class function SetInfo<T>(hKey: THandle; InfoClass: TKeySetInformationClass;
      const Buffer: T): TNtxStatus; static;
  end;

{ Values }

// Enumerate values of a key
function NtxEnumerateValuesKey(hKey: THandle;
  out ValueNames: TArray<TRegValueEntry>): TNtxStatus;

// Query variable-length value information
function NtxQueryValueKey(hKey: THandle; ValueName: String;
  InfoClass: TKeyValueInformationClass; out Status: TNtxStatus): Pointer;

// Query value of a DWORD type
function NtxQueryDwordValueKey(hKey: THandle; ValueName: String;
  out Value: Cardinal): TNtxStatus;

// Query value of a string type
function NtxQueryStringValueKey(hKey: THandle; ValueName: String;
  out Value: String): TNtxStatus;

// Query value of a multi-string type
function NtxQueryMultiStringValueKey(hKey: THandle; ValueName: String;
  out Value: TArray<String>): TNtxStatus;

// Set value
function NtxSetValueKey(hKey: THandle; ValueName: String;
  ValueType: TRegValueType; Data: Pointer; DataSize: Cardinal): TNtxStatus;

// Set a DWORD value
function NtxSetDwordValueKey(hKey: THandle; ValueName: String; Value: Cardinal)
  : TNtxStatus;

// Set a string value
function NtxSetStringValueKey(hKey: THandle; ValueName: String; Value: String;
  ValueType: TRegValueType = REG_SZ): TNtxStatus;

// Set a multi-string value
function NtxSetMultiStringValueKey(hKey: THandle; ValueName: String;
  Value: TArray<String>): TNtxStatus;

// Delete a value
function NtxDeleteValueKey(hKey: THandle; ValueName: String): TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus;

{ Keys }

function NtxOpenKey(out hKey: THandle; Name: String; DesiredAccess: TAccessMask;
  Root: THandle; OpenOptions: Cardinal; Attributes: Cardinal): TNtxStatus;
var
  NameStr: UNICODE_STRING;
  ObjAttr: TObjectAttributes;
begin
  NameStr.FromString(Name);
  InitializeObjectAttributes(ObjAttr, @NameStr, Attributes or
    OBJ_CASE_INSENSITIVE, Root);

  Result.Location := 'NtOpenKeyEx';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objNtKey;

  Result.Status := NtOpenKeyEx(hKey, DesiredAccess, ObjAttr, OpenOptions);
end;

function NtxCreateKey(out hKey: THandle; Name: String;
  DesiredAccess: TAccessMask; Root: THandle; CreateOptions: Cardinal;
  Attributes: Cardinal; Disposition: PCardinal): TNtxStatus;
var
  NameStr: UNICODE_STRING;
  ObjAttr: TObjectAttributes;
begin
  NameStr.FromString(Name);
  InitializeObjectAttributes(ObjAttr, @NameStr, Attributes or
    OBJ_CASE_INSENSITIVE, Root);

  Result.Location := 'NtCreateKey';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objNtKey;

  Result.Status := NtCreateKey(hKey, DesiredAccess, ObjAttr, 0, nil,
    CreateOptions, Disposition);
end;

function NtxDeleteKey(hKey: THandle): TNtxStatus;
begin
  Result.Location := 'NtDeleteKey';
  Result.Status := NtDeleteKey(hKey);
end;

function NtxRenameKey(hKey: THandle; NewName: String): TNtxStatus;
var
  NewNameStr: UNICODE_STRING;
begin
  NewNameStr.FromString(NewName);
  Result.Location := 'NtRenameKey';
  Result.Status := NtRenameKey(hKey, NewNameStr)
end;

function NtxEnumerateSubKeys(hKey: THandle; out SubKeys: TArray<String>)
  : TNtxStatus;
var
  Index: Integer;
  Buffer: PKeyBasicInformation;
  BufferSize, Required: Cardinal;
begin
  Result.Location := 'NtEnumerateKey';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(KeyBasicInformation);
  Result.LastCall.InfoClassType := TypeInfo(TKeyInformationClass);

  SetLength(SubKeys, 0);

  Index := 0;
  repeat

    // Query sub-key name
    BufferSize := 0;
    repeat
      Buffer := AllocMem(BufferSize);

      Required := 0;
      Result.Status := NtEnumerateKey(hKey, Index, KeyBasicInformation, Buffer,
        BufferSize, Required);

      if Result.IsSuccess then
        Break
      else
        FreeMem(Buffer);

      if Required < BufferSize then
        Break;

      BufferSize := Required;
    until not NtxTryCheckBuffer(Result.Status, BufferSize);

    if Result.IsSuccess then
    begin
      SetLength(SubKeys, Length(SubKeys) + 1);
      SetString(SubKeys[High(SubKeys)], PWideChar(@Buffer.Name),
        Buffer.NameLength div SizeOf(WideChar));
    end;

    Inc(Index);
  until not Result.IsSuccess;

  if Result.Status = STATUS_NO_MORE_ENTRIES then
    Result.Status := STATUS_SUCCESS;
end;

function NtxQueryInformationKey(hKey: THandle; InfoClass: TKeyInformationClass;
  out Status: TNtxStatus): Pointer;
var
  NameStr: UNICODE_STRING;
  BufferSize, Required: Cardinal;
begin
  Status.Location := 'NtQueryKey';
  Status.LastCall.CallType := lcQuerySetCall;
  Status.LastCall.InfoClass := Cardinal(InfoClass);
  Status.LastCall.InfoClassType := TypeInfo(TKeyInformationClass);

  BufferSize := 0;
  repeat
    Result := AllocMem(BufferSize);

    Required := 0;
    Status.Status := NtQueryKey(hKey, InfoClass, Result, BufferSize, Required);

    if Status.IsSuccess then
      Break
    else
    begin
      FreeMem(Result);
      Result := nil;
    end;

    if Required < BufferSize then
      Break;

    BufferSize := Required;

  until not NtxTryCheckBuffer(Status.Status, BufferSize);
end;

function NtxQueryBasicKey(hKey: THandle; out Info: TKeyBasicInfo): TNtxStatus;
var
  Buffer: PKeyBasicInformation;
begin
  Buffer := NtxQueryInformationKey(hKey, KeyBasicInformation, Result);

  if Result.IsSuccess then
  begin
    Info.LastWriteTime := Buffer.LastWriteTime;
    Info.TitleIndex := Buffer.TitleIndex;
    SetString(Info.Name, PWideChar(@Buffer.Name), Buffer.NameLength);
    FreeMem(Buffer);
  end;
end;

class function NtxKey.Query<T>(hKey: THandle; InfoClass: TKeyInformationClass;
  out Buffer: T): TNtxStatus;
var
  Returned: Cardinal;
begin
  Result.Location := 'NtQueryKey';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TKeyInformationClass);

  Result.Status := NtQueryKey(hKey, InfoClass, @Buffer, SizeOf(Buffer),
    Returned);
end;

class function NtxKey.SetInfo<T>(hKey: THandle;
  InfoClass: TKeySetInformationClass; const Buffer: T): TNtxStatus;
begin
  Result.Location := 'NtSetInformationKey';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TKeySetInformationClass);

  Result.Status := NtSetInformationKey(hKey, InfoClass, @Buffer,
    SizeOf(Buffer));
end;

{ Values }

function NtxEnumerateValuesKey(hKey: THandle;
  out ValueNames: TArray<TRegValueEntry>): TNtxStatus;
var
  Index: Integer;
  Buffer: PKeyValueBasicInformation;
  BufferSize, Required: Cardinal;
begin
  Result.Location := 'NtEnumerateValueKey';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(KeyValueBasicInformation);
  Result.LastCall.InfoClassType := TypeInfo(TKeyValueInformationClass);

  SetLength(ValueNames, 0);

  Index := 0;
  repeat

    // Query value name
    BufferSize := 0;
    repeat
      Buffer := AllocMem(BufferSize);

      Required := 0;
      Result.Status := NtEnumerateValueKey(hKey, Index,
        KeyValueBasicInformation, Buffer, BufferSize, Required);

      if Result.IsSuccess then
        Break
      else
        FreeMem(Buffer);

      if Required < BufferSize then
        Break;

      BufferSize := Required;
    until not NtxTryCheckBuffer(Result.Status, BufferSize);

    if Result.IsSuccess then
    begin
      SetLength(ValueNames, Length(ValueNames) + 1);
      ValueNames[High(ValueNames)].ValueType := Buffer.ValueType;
      SetString(ValueNames[High(ValueNames)].ValueName, PWideChar(@Buffer.Name),
        Buffer.NameLength div SizeOf(WideChar));
    end;

    Inc(Index);
  until not Result.IsSuccess;

  if Result.Status = STATUS_NO_MORE_ENTRIES then
    Result.Status := STATUS_SUCCESS;
end;

function NtxQueryValueKey(hKey: THandle; ValueName: String;
  InfoClass: TKeyValueInformationClass; out Status: TNtxStatus): Pointer;
var
  NameStr: UNICODE_STRING;
  BufferSize, Required: Cardinal;
begin
  NameStr.FromString(ValueName);

  Status.Location := 'NtQueryValueKey';
  Status.LastCall.CallType := lcQuerySetCall;
  Status.LastCall.InfoClass := Cardinal(InfoClass);
  Status.LastCall.InfoClassType := TypeInfo(TKeyValueInformationClass);

  BufferSize := 0;

  repeat
    Result := AllocMem(BufferSize);

    Required := 0;
    Status.Status := NtQueryValueKey(hKey, NameStr, InfoClass, Result,
      BufferSize, Required);

    if Status.IsSuccess then
      Break
    else
    begin
      FreeMem(Result);
      Result := nil;
    end;

    if Required < BufferSize then
      Break;

    BufferSize := Required;

  until not NtxTryCheckBuffer(Status.Status, BufferSize);
end;

function NtxQueryDwordValueKey(hKey: THandle; ValueName: String;
  out Value: Cardinal): TNtxStatus;
var
  Buffer: PKeyValuePartialInfromation;
begin
  Buffer := NtxQueryValueKey(hKey, ValueName,
    KeyValuePartialInformation, Result);

  if not Result.IsSuccess then
    Exit;

  if Buffer.DataLength < SizeOf(Cardinal) then
  begin
    Result.Status := STATUS_INFO_LENGTH_MISMATCH;
    Exit;
  end;

  case Buffer.ValueType of
    REG_DWORD:
      Value := PCardinal(@Buffer.Data)^;
  else
    Result.Status := STATUS_OBJECT_TYPE_MISMATCH;
  end;

  FreeMem(Buffer);
end;

function NtxQueryStringValueKey(hKey: THandle; ValueName: String;
  out Value: String): TNtxStatus;
var
  Buffer: PKeyValuePartialInfromation;
begin
  Buffer := NtxQueryValueKey(hKey, ValueName,
    KeyValuePartialInformation, Result);

  if not Result.IsSuccess then
    Exit;

  case Buffer.ValueType of
    REG_SZ, REG_EXPAND_SZ, REG_LINK:
      SetString(Value, PWideChar(@Buffer.Data),
        Buffer.DataLength div SizeOf(WideChar));
  else
    Result.Status := STATUS_OBJECT_TYPE_MISMATCH;
  end;

  FreeMem(Buffer);
end;

function NtxQueryMultiStringValueKey(hKey: THandle; ValueName: String;
  out Value: TArray<String>): TNtxStatus;
var
  Buffer: PKeyValuePartialInfromation;
  Count, j: Integer;
  pCurrentChar, pItemStart, pBlockEnd: PWideChar;
begin
  Buffer := NtxQueryValueKey(hKey, ValueName,
    KeyValuePartialInformation, Result);

  if not Result.IsSuccess then
    Exit;

  case Buffer.ValueType of
    REG_SZ, REG_EXPAND_SZ, REG_LINK:
      begin
        SetLength(Value, 1);
        SetString(Value[0], PWideChar(@Buffer.Data), Buffer.DataLength);
      end;

    REG_MULTI_SZ:
      begin
        // Save where the buffer ends to make sure we don't pass this point
        pBlockEnd := PWideChar(@Buffer.Data);
        Inc(pBlockEnd, Buffer.DataLength);

        // Count strings
        Count := 0;
        pCurrentChar := PWideChar(@Buffer.Data);

        while (pCurrentChar < pBlockEnd) and (pCurrentChar^ <> #0) do
        begin
          // Skip one zero-terminated string
          while (pCurrentChar < pBlockEnd) and (pCurrentChar^ <> #0) do
            Inc(pCurrentChar);

          Inc(Count);
          Inc(pCurrentChar);
        end;

        SetLength(Value, Count);

        // Save the content
        j := 0;
        pCurrentChar := PWideChar(@Buffer.Data);

        while (pCurrentChar < pBlockEnd) and (pCurrentChar^ <> #0) do
        begin
          // Parse one string
          Count := 0;
          pItemStart := pCurrentChar;

          while (pCurrentChar < pBlockEnd) and (pCurrentChar^ <> #0) do
          begin
            Inc(pCurrentChar);
            Inc(Count);
          end;

          // Save it
          SetString(Value[j], pItemStart, Count);

          Inc(j);
          Inc(pCurrentChar);
        end;
      end
  else
    Result.Status := STATUS_OBJECT_TYPE_MISMATCH;
  end;

  FreeMem(Buffer);
end;

function NtxSetValueKey(hKey: THandle; ValueName: String;
  ValueType: TRegValueType; Data: Pointer; DataSize: Cardinal): TNtxStatus;
var
  ValueNameStr: UNICODE_STRING;
begin
  ValueNameStr.FromString(ValueName);
  Result.Location := 'NtSetValueKey';
  Result.Status := NtSetValueKey(hKey, ValueNameStr, 0, ValueType, Data,
    DataSize);
end;

function NtxSetDwordValueKey(hKey: THandle; ValueName: String; Value: Cardinal)
  : TNtxStatus;
begin
  Result := NtxSetValueKey(hKey, ValueName, REG_DWORD, @Value, SizeOf(Value));
end;

function NtxSetStringValueKey(hKey: THandle; ValueName: String; Value: String;
  ValueType: TRegValueType): TNtxStatus;
begin
  Result := NtxSetValueKey(hKey, ValueName, ValueType, PWideChar(Value),
    Length(Value) * SizeOf(WideChar));
end;

function NtxSetMultiStringValueKey(hKey: THandle; ValueName: String;
  Value: TArray<String>): TNtxStatus;
var
  Buffer, pCurrentPosition: PWideChar;
  BufferSize: Cardinal;
  i: Integer;
begin
  // Calculate required memory
  BufferSize := SizeOf(WideChar); // Include ending #0
  for i := 0 to High(Value) do
    Inc(BufferSize, (Length(Value[i]) + 1) * SizeOf(WideChar));

  Buffer := AllocMem(BufferSize);

  pCurrentPosition := Buffer;
  for i := 0 to High(Value) do
  begin
    Move(PWideChar(Value[i])^, pCurrentPosition^,
      Length(Value[i]) * SizeOf(WideChar));
    Inc(pCurrentPosition, Length(Value[i]) + 1);
  end;

  Result := NtxSetValueKey(hKey, ValueName, REG_MULTI_SZ, Buffer, BufferSize);
  FreeMem(Buffer);
end;

function NtxDeleteValueKey(hKey: THandle; ValueName: String): TNtxStatus;
var
  ValueNameStr: UNICODE_STRING;
begin
  ValueNameStr.FromString(ValueName);
  Result.Location := 'NtDeleteValueKey';
  Result.Status := NtDeleteValueKey(hKey, ValueNameStr);
end;

end.
