unit NtUtils.Strings;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, DelphiUtils.Strings, NtUtils.Security.Sid;

const
  GroupAttributeFlags: array [0..5] of TFlagName = (
    (Value: SE_GROUP_MANDATORY; Name: 'Mandatory'),
    (Value: SE_GROUP_OWNER; Name: 'Owner'),
    (Value: SE_GROUP_INTEGRITY; Name: 'Integrity'),
    (Value: SE_GROUP_RESOURCE; Name: 'Resource'),
    (Value: SE_GROUP_LOGON_ID; Name: 'Logon Id'),
    (Value: SE_GROUP_USE_FOR_DENY_ONLY; Name: 'Use for deny only')
  );

  ObjAttributesFlags: array [0..1] of TFlagName = (
    (Value: OBJ_PERMANENT; Name: 'Permanent'),
    (Value: OBJ_EXCLUSIVE; Name: 'Exclusive')
  );

  TokenFlagsNames: array [0..8] of TFlagName = (
    (Value: TOKEN_WRITE_RESTRICTED; Name: 'Write-only restricted'),
    (Value: TOKEN_IS_RESTRICTED; Name: 'Restricted'),
    (Value: TOKEN_SESSION_NOT_REFERENCED; Name: 'Session not referenced'),
    (Value: TOKEN_SANDBOX_INERT; Name: 'Sandbox inert'),
    (Value: TOKEN_VIRTUALIZE_ALLOWED; Name: 'Virtualization allowed'),
    (Value: TOKEN_VIRTUALIZE_ENABLED; Name: 'Virtualization enabled'),
    (Value: TOKEN_IS_FILTERED; Name: 'Filtered'),
    (Value: TOKEN_UIACCESS; Name: 'UIAccess'),
    (Value: TOKEN_NOT_LOW; Name: 'Not low')
  );

function ElevationToString(Value: TTokenElevationType): String;
function IntegrityToString(Rid: Cardinal): String;
function StateOfGroupToString(Value: Cardinal): String;
function StateOfPrivilegeToString(Value: Cardinal): String;
function NativeTimeToString(NativeTime: TLargeInteger): String;

implementation

uses
  System.SysUtils;

function ElevationToString(Value: TTokenElevationType): String;
begin
  case Value of
    TokenElevationTypeDefault: Result := 'N/A';
    TokenElevationTypeFull: Result := 'Full';
    TokenElevationTypeLimited: Result := 'Limited';
  else
     Result := OutOfBound(Integer(Value));
  end;
end;

function IntegrityToString(Rid: Cardinal): String;
begin
  case Rid of
    SECURITY_MANDATORY_UNTRUSTED_RID:         Result := 'Untrusted';
    SECURITY_MANDATORY_LOW_RID:               Result := 'Low';
    SECURITY_MANDATORY_MEDIUM_RID:            Result := 'Medium';
    SECURITY_MANDATORY_MEDIUM_PLUS_RID:       Result := 'Medium +';
    SECURITY_MANDATORY_HIGH_RID:              Result := 'High';
    SECURITY_MANDATORY_SYSTEM_RID:            Result := 'System';
    SECURITY_MANDATORY_PROTECTED_PROCESS_RID: Result := 'Protected';
  else
    Result := IntToHexEx(Rid, 4);
  end;
end;

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

function NativeTimeToString(NativeTime: TLargeInteger): String;
begin
  if NativeTime.QuadPart = 0 then
    Result := 'Never'
  else if NativeTime.QuadPart = Int64.MaxValue then
    Result := 'Infinite'
  else
    Result := DateTimeToStr(NativeTime.ToDateTime);
end;

end.

