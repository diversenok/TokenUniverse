unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt, Ntapi.ntseapi;

type
  TPrivilegeRec = record
    NameValid, DisplayNameValid: Boolean;
    Name, DisplayName: String;
  end;

  TPrivilegeCache = class
  private
    class var Cache: array [SE_MIN_WELL_KNOWN_PRIVILEGE ..
      SE_MAX_WELL_KNOWN_PRIVILEGE] of TPrivilegeRec;
    class function InRange(Value: TLuid): Boolean; static; inline;
    class function TryQueryName(Value: Int64; out Name: String): Boolean;
      static;
  public
    class function QueryName(Value: Int64): String; static;
    class function QueryDisplayName(Value: Int64): String; static;
    class function AllPrivileges: TArray<TLuid>;
  end;

implementation

uses
  NtUtils.Lsa, System.SysUtils, NtUtils.Exceptions;


{ TPrivilegeCache }

class function TPrivilegeCache.AllPrivileges: TArray<TLuid>;
var
  i: Integer;
  Privileges: TArray<TPrivilegeDefinition>;
  Value: TLuid;
begin
  // Ask LSA to enumerate the privileges.
  if LsaxEnumeratePrivileges(Privileges).IsSuccess then
  begin
    SetLength(Result, Length(Privileges));

    for i := 0 to High(Privileges) do
    begin
      Value := Privileges[i].LocalValue;

      // Save into the result list
      Result[i] := Value;

      // Cache privilege name
      if InRange(Value) then
      begin
        Cache[Value].NameValid := True;
        Cache[Value].Name := Privileges[i].Name;
      end;
    end;
  end
  else
  begin
    // Query was unsuccessful. Just return the range from min to max
    SetLength(Result, SE_MAX_WELL_KNOWN_PRIVILEGE - SE_MIN_WELL_KNOWN_PRIVILEGE);

    for i := 0 to High(Result) do
      Result[i] := SE_MIN_WELL_KNOWN_PRIVILEGE + i;
  end;
end;

class function TPrivilegeCache.InRange(Value: TLuid): Boolean;
begin
  Result := (SE_MIN_WELL_KNOWN_PRIVILEGE <= Value) and
    (Value <= SE_MAX_WELL_KNOWN_PRIVILEGE);
end;

class function TPrivilegeCache.QueryDisplayName(Value: Int64): String;
var
  Name: String;
begin
  // Note: we need privilege name to obtain its display name

  if InRange(Value) and Cache[Value].DisplayNameValid then
    Result := Cache[Value].DisplayName
  else if TryQueryName(Value, Name) and
    LsaxQueryDescriptionPrivilege(Name, Result).IsSuccess then
  begin
    // Cache it if applicable
    if InRange(Value) then
    begin
      Cache[Value].DisplayNameValid := True;
      Cache[Value].DisplayName := Result;
    end;
  end
  else
    Result := '';
end;

class function TPrivilegeCache.QueryName(Value: Int64): String;
begin
  if not TryQueryName(Value, Result) then
    Result := 'Unknown privilege ' + IntToStr(Value);
end;

class function TPrivilegeCache.TryQueryName(Value: Int64; out Name: String):
  Boolean;
begin
  Result := True;

  // Try cache first, then query LSA
  if InRange(Value) and Cache[Value].NameValid then
    Name := Cache[Value].Name
  else if LsaxQueryNamePrivilege(Value, Name).IsSuccess then
  begin
    // Cache it if applicable
    if InRange(Value) then
    begin
      Cache[Value].NameValid := True;
      Cache[Value].Name := Name;
    end;
  end
  else
    Result := False;
end;

end.
