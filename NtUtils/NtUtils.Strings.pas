unit NtUtils.Strings;

interface

uses
  Winapi.WinNt, Winapi.NtSecApi, NtUtils.Lsa;

type
  TBitFlagMode = (bmGroupFlags, bmLogonFlags);

// Bit Flags
function MapKnownFlags(Value: Cardinal; Mode: TBitFlagMode): String;
function MapKnownFlagsHint(Value: Cardinal; Mode: TBitFlagMode): String;

// Enumerations
function EnumOutOfBoundString(Value: Cardinal): String;
function EnumElevationToString(Value: TTokenElevationType): String;
function EnumSidTypeToString(Value: TSidNameUse): String;
function EnumLogonTypeToString(Value: TSecurityLogonType): String;

// States
function StateOfGroupToString(Value: Cardinal): String;
function StateOfPrivilegeToString(Value: Cardinal): String;

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

  LogonFlags: array [0..18] of TFlagName = (
    (Value: LOGON_GUEST; Name: 'Guest'),
    (Value: LOGON_NOENCRYPTION; Name: 'No Encryption'),
    (Value: LOGON_CACHED_ACCOUNT; Name: 'Cached Account'),
    (Value: LOGON_USED_LM_PASSWORD; Name: 'Used LM Password'),
    (Value: LOGON_EXTRA_SIDS; Name: 'Extra SIDs'),
    (Value: LOGON_SUBAUTH_SESSION_KEY; Name: 'Subauth Session Key'),
    (Value: LOGON_SERVER_TRUST_ACCOUNT; Name: 'Server Trust Account'),
    (Value: LOGON_NTLMV2_ENABLED; Name: 'NTLMv2 Enabled'),
    (Value: LOGON_RESOURCE_GROUPS; Name: 'Resource Groups'),
    (Value: LOGON_PROFILE_PATH_RETURNED; Name: 'Profile Path Returned'),
    (Value: LOGON_NT_V2; Name: 'NTv2'),
    (Value: LOGON_LM_V2; Name: 'LMv2'),
    (Value: LOGON_NTLM_V2; Name: 'NTLMv2'),
    (Value: LOGON_OPTIMIZED; Name: 'Optimized'),
    (Value: LOGON_WINLOGON; Name: 'Winlogon'),
    (Value: LOGON_PKINIT; Name: 'Pkinit'),
    (Value: LOGON_NO_OPTIMIZED; Name: 'No Optimized'),
    (Value: LOGON_NO_ELEVATION; Name: 'No Elevation'),
    (Value: LOGON_MANAGED_SERVICE; Name: 'Managed Service')
  );

function MapKnownFlags(Value: Cardinal; Mode: TBitFlagMode): String;
begin
  case Mode of
    bmGroupFlags: Result := MapFlags(Value, GroupFlags);
    bmLogonFlags: Result := MapFlags(Value, LogonFlags);
  else
    Result := '';
  end;
end;

function MapKnownFlagsHint(Value: Cardinal; Mode: TBitFlagMode): String;
begin
  case Mode of
    bmGroupFlags: Result := MapFlagsHint(Value, GroupFlags);
    bmLogonFlags: Result := MapFlagsHint(Value, LogonFlags);
  else
    Result := '';
  end;
end;

{ Enumerations }

function EnumOutOfBoundString(Value: Cardinal): String;
begin
  Result := IntToStr(Value) + ' (out of bound)';
end;

function EnumElevationToString(Value: TTokenElevationType): String;
begin
  case Value of
    TokenElevationTypeDefault: Result := 'N/A';
    TokenElevationTypeFull: Result := 'Full';
    TokenElevationTypeLimited: Result := 'Limited';
  else
     Result := EnumOutOfBoundString(Cardinal(Value));
  end;
end;

function EnumSidTypeToString(Value: TSidNameUse): String;
const
  SidTypeNames: array [TSidNameUse] of String = ('Undefined', 'User', 'Group',
    'Domain', 'Alias', 'Well-known Group', 'Deleted Account', 'Invalid',
    'Unknown', 'Computer', 'Label', 'Logon Session');
begin
  if (Low(Value) <= Value) and (Value <= High(Value)) then
    Result := SidTypeNames[Value]
  else
    Result := EnumOutOfBoundString(Cardinal(Value));
end;

function EnumLogonTypeToString(Value: TSecurityLogonType): String;
const
  Mapping: array [TSecurityLogonType] of String = ('System', 'Reserved',
    'Interactive', 'Network', 'Batch', 'Service', 'Proxy', 'Unlock',
    'Network clear text', 'New credentials', 'Remote interactive',
    'Cached interactive', 'Cached remote interactive', 'Cached unlock');
begin
  if (Low(Value) <= Value) and (Value <= High(Value)) then
    Result := Mapping[Value]
  else
    Result := EnumOutOfBoundString(Cardinal(Value));
end;

{ States }

function StateOfGroupToString(Value: Cardinal): String;
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

function StateOfPrivilegeToString(Value: Cardinal): String;
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
    ['Type', EnumSidTypeToString(SID.SidType)]);
  Inc(Index);

  if AttributesPresent then
  begin
    Items[Index] := Format(ITEM_FORMAT,
      ['State', StateOfGroupToString(Attributes)]);
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
