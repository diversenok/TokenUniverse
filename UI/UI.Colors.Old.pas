unit UI.Colors.Old;

interface

uses
  Vcl.Graphics, TU.Tokens.Types, Ntapi.ntseapi;

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

function GroupAttributesToColor(Attributes: Cardinal): TColor;

implementation

function Contains(Flags, Mask: Cardinal): Boolean;
begin
  Result := Flags and Mask <> 0;
end;

function GroupAttributesToColor(Attributes: Cardinal): TColor;
begin
  if Contains(Attributes, SE_GROUP_INTEGRITY_ENABLED) then
    Exit(clIntegrity);

  if Contains(Attributes, SE_GROUP_ENABLED) then
  begin
    if Contains(Attributes, SE_GROUP_ENABLED_BY_DEFAULT) then
      Result := clEnabled
    else
      Result := clEnabledModified;
  end
  else
  begin
    if Contains(Attributes, SE_GROUP_ENABLED_BY_DEFAULT) then
      Result := clDisabledModified
    else
      Result := clDisabled;
  end;
end;

end.
