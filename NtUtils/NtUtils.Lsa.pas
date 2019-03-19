unit NtUtils.Lsa;

interface

uses
  Winapi.WinNt, Ntapi.ntdef;

type
  TPrivilegeDefinition = record
    Name: String;
    LocalValue: TLuid;
  end;

  TPrivDefArray = array of TPrivilegeDefinition;

/// <summary> Ask LSA to enumerate privileges on the local system. </summary>
function EnumeratePrivileges(out Privileges: TPrivDefArray):
  NTSTATUS;

function QueryPrivilegeName(Luid: TLuid; out Name: String): NTSTATUS;
function QueryPrivilegeDisplayName(const Name: String; out DisplayName: String):
  NTSTATUS;

implementation

uses
  Winapi.NtSecApi, Winapi.ntlsa;

function EnumeratePrivileges(out Privileges: TPrivDefArray):
  NTSTATUS;
var
  ObjAttr: TObjectAttributes;
  hPolicy: TLsaHandle;
  EnumContext: Cardinal;
  Count, i: Integer;
  Buf: PPolicyPrivilegeDefinitionArray;
begin
  SetLength(Privileges, 0);
  InitializeObjectAttributes(ObjAttr);
  Result := LsaOpenPolicy(nil, ObjAttr, POLICY_VIEW_LOCAL_INFORMATION, hPolicy);

  if NT_SUCCESS(Result) then
  begin
    EnumContext := 0;

    Result := LsaEnumeratePrivileges(hPolicy, EnumContext, Buf,
      MAX_PREFERRED_LENGTH, Count);

    if NT_SUCCESS(Result) then
    begin
      SetLength(Privileges, Count);

      for i := 0 to Count - 1 do
      begin
        Privileges[i].Name := Buf[i].Name.ToString;
        Privileges[i].LocalValue := Buf[i].LocalValue;
      end;

      LsaFreeMemory(Buf);
    end;

    LsaClose(hPolicy);
  end;
end;

function QueryPrivilegeName(Luid: TLuid; out Name: String): NTSTATUS;
var
  ObjAttr: TObjectAttributes;
  hPolicy: TLsaHandle;
  NameBuf: PLsaUnicodeString;
begin
  Name := '';
  InitializeObjectAttributes(ObjAttr);
  Result := LsaOpenPolicy(nil, ObjAttr, POLICY_LOOKUP_NAMES, hPolicy);

  if NT_SUCCESS(Result) then
  begin
    Result := LsaLookupPrivilegeName(hPolicy, Luid, NameBuf);

    if NT_SUCCESS(Result) then
    begin
      Name := NameBuf.ToString;
      LsaFreeMemory(NameBuf);
    end;

    LsaClose(hPolicy);
  end;
end;

function QueryPrivilegeDisplayName(const Name: String; out DisplayName: String):
  NTSTATUS;
var
  ObjAttr: TObjectAttributes;
  hPolicy: TLsaHandle;
  NameBuf: TLsaUnicodeString;
  DisplayNameBuf: PLsaUnicodeString;
  LangId: SmallInt;
begin
  DisplayName := '';
  InitializeObjectAttributes(ObjAttr);
  Result := LsaOpenPolicy(nil, ObjAttr, POLICY_LOOKUP_NAMES, hPolicy);

  if NT_SUCCESS(Result) then
  begin
    NameBuf.FromString(Name);

    Result := LsaLookupPrivilegeDisplayName(hPolicy, NameBuf, DisplayNameBuf,
      LangId);

    if NT_SUCCESS(Result) then
    begin
      DisplayName := DisplayNameBuf.ToString;
      LsaFreeMemory(DisplayNameBuf);
    end;

    LsaClose(hPolicy);
  end;
end;

end.
