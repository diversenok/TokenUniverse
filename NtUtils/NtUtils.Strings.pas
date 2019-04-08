unit NtUtils.Strings;

interface

uses
  Winapi.WinNt, NtUtils.Lsa;

type
  TBitFlagMode = (bmGroupFlags);

// Bit Flags
function MapKnownFlags(Value: Cardinal; Mode: TBitFlagMode): String;

// Enumerations
function SidTypeToString(Value: TSidNameUse): String;

// States
function GroupStateToString(Value: Cardinal): String;
function PrivilegeStateToString(Value: Cardinal): String;

// Misc
function BuildSidHint(SID: TTranslatedName; Attributes: Cardinal;
  AttributesPresent: Boolean = True): String;

implementation

uses
  System.SysUtils, DelphiUtils.Strings, NtUtils.Types;

{ Bit Flags }

const
  GroupFlags: array [0..5] of TFlagName = (
    (Value: SE_GROUP_MANDATORY; Name: 'Mandatory'),
    (Value: SE_GROUP_OWNER; Name: 'Owner'),
    (Value: SE_GROUP_INTEGRITY; Name: 'Integrity'),
    (Value: SE_GROUP_RESOURCE; Name: 'Resource'),
    (Value: SE_GROUP_LOGON_ID; Name: 'Logon Id'),
    (Value: SE_GROUP_USE_FOR_DENY_ONLY; Name: 'Use for deny only')
  );

function MapKnownFlags(Value: Cardinal; Mode: TBitFlagMode): String;
begin
  case Mode of
    bmGroupFlags: Result := MapFlags(Value, GroupFlags);
  end;
end;

{ Enumerations }

function SidTypeToString(Value: TSidNameUse): String;
const
  SidTypeNames: array [TSidNameUse] of String = ('Undefined', 'User', 'Group',
    'Domain', 'Alias', 'Well-known Group', 'Deleted Account', 'Invalid',
    'Unknown', 'Computer', 'Label', 'Logon Session');
begin
  if (Low(Value) <= Value) and (Value <= High(Value)) then
    Result := SidTypeNames[Value]
  else
    Result := IntToStr(Cardinal(Value)) + ' (out of bound)';
end;

{ States }

function GroupStateToString(Value: Cardinal): String;
begin
  if Contains(Value, SE_GROUP_ENABLED) then
  begin
    if Contains(Value, SE_GROUP_ENABLED_BY_DEFAULT) then
      Result := 'Enabled'
    else
      Result := 'Enabled (modified)';
  end
  else
  begin
    if Contains(Value, SE_GROUP_ENABLED_BY_DEFAULT) then
      Result := 'Disabled (modified)'
    else
      Result := 'Disabled';
  end;

  if Contains(Value, SE_GROUP_INTEGRITY_ENABLED) then
  begin
    if Contains(Value, SE_GROUP_ENABLED) or
      Contains(Value, SE_GROUP_ENABLED_BY_DEFAULT) then
      Result := 'Integrity Enabled, Group ' + Result
    else
      Exit('Integrity Enabled');
  end;
end;

function PrivilegeStateToString(Value: Cardinal): String;
begin
  if Contains(Value, SE_PRIVILEGE_ENABLED) then
  begin
    if Contains(Value, SE_PRIVILEGE_ENABLED_BY_DEFAULT) then
      Result := 'Enabled'
    else
      Result := 'Enabled (modified)';
  end
  else
  begin
    if Contains(Value, SE_PRIVILEGE_ENABLED_BY_DEFAULT) then
      Result := 'Disabled (modified)'
    else
      Result := 'Disabled';
  end;

  if Contains(Value, SE_PRIVILEGE_REMOVED) then
    Result := 'Removed, ' + Result;

  if Contains(Value, SE_PRIVILEGE_USED_FOR_ACCESS) then
    Result := 'Used for access, ' + Result;
end;

{ Misc }

function BuildSidHint(SID: TTranslatedName; Attributes: Cardinal;
  AttributesPresent: Boolean): String;
const
  ITEM_FORMAT = '%s:'#$D#$A'  %s';
var
  Items: array of String;
  Index: Integer;
begin
  // Set maximum count
  SetLength(Items, 5);
  Index := 0;

  if SID.HasName then
  begin
    Items[Index] := Format(ITEM_FORMAT, ['Pretty name', SID.FullName]);
    Inc(Index);
  end;

  Items[Index] := Format(ITEM_FORMAT, ['SID', SID.SDDL]);
  Inc(Index);

  Items[Index] := Format(ITEM_FORMAT,
    ['Type', SidTypeToString(SID.SidType)]);
  Inc(Index);

  if AttributesPresent then
  begin
    Items[Index] := Format(ITEM_FORMAT,
      ['State', GroupStateToString(Attributes)]);
    Inc(Index);

    if ContainsAny(Attributes, SE_GROUP_ALL_FLAGS) then
    begin
      Items[Index] := Format(ITEM_FORMAT,
        ['Flags', MapKnownFlags(Attributes, bmGroupFlags)]);
      Inc(Index);
    end;
  end;

  SetLength(Items, Index);
  Result := String.Join(#$D#$A, Items);
end;

end.
