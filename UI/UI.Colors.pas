unit UI.Colors;

interface

uses
  Vcl.Graphics, Winapi.Windows, TU.Tokens, TU.Tokens.Types;

const
  clStale: TColor = $F5DCC2;
  clEnabledModified: TColor = $C0F0C0;
  clEnabled: TColor = $E0F0E0;
  clDisabled: TColor = $E0E0F0;
  clDisabledModified: TColor = $D0D0F0;
  clRemoved: TColor = $E0E0E0;
  clIntegrity: TColor = $F0E0E0;
  clSuspended: TColor = $AAAAAA;
  clGuiThread: TColor = $77FFFF;

function GroupAttributesToColor(Attributes: TGroupAttributes): TColor;

function PrivilegeToColor(Privilege: TPrivilege): TColor;

implementation

function GroupAttributesToColor(Attributes: TGroupAttributes): TColor;
begin
  if Attributes.Contain(GroupIntegrityEnabled) then
    Exit(clIntegrity);

  if Attributes.Contain(GroupEnabled) then
  begin
    if Attributes.Contain(GroupEnabledByDefault) then
      Result := clEnabled
    else
      Result := clEnabledModified;
  end
  else
  begin
    if Attributes.Contain(GroupEnabledByDefault) then
      Result := clDisabledModified
    else
      Result := clDisabled;
  end;
end;

function PrivilegeToColor(Privilege: TPrivilege): TColor;
begin
  if Privilege.AttributesContain(SE_PRIVILEGE_REMOVED) then
    Exit(clRemoved);

  if Privilege.AttributesContain(SE_PRIVILEGE_ENABLED) then
  begin
    if Privilege.AttributesContain(SE_PRIVILEGE_ENABLED_BY_DEFAULT) then
      Result := clEnabled
    else
      Result := clEnabledModified;
  end
  else
  begin
    if Privilege.AttributesContain(SE_PRIVILEGE_ENABLED_BY_DEFAULT) then
      Result := clDisabledModified
    else
      Result := clDisabled;
  end;
end;

end.
