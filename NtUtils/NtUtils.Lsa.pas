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

  TTranslatedName = record
    Domain, User, SDDL: String;
    SidType: TSidNameUse;
    function HasName: Boolean;
    function FullName: String;
  end;

  TTranslatedNames = array of TTranslatedName;

// LsaEnumeratePrivileges
function LsaxEnumeratePrivileges(out Privileges: TPrivDefArray): NTSTATUS;

// LsaLookupPrivilegeName
function LsaxQueryNamePrivilege(Luid: TLuid; out Name: String): NTSTATUS;

// LsaLookupPrivilegeDisplayName
function LsaxQueryDescriptionPrivilege(const Name: String;
  out DisplayName: String): NTSTATUS;

// LsaLookupSids mixed with RtlConvertSidToUnicodeString, always succeeds
procedure LsaxLookupSids(Sids: TSidDynArray;
  out TranslatedNames: TTranslatedNames);

implementation

uses
  Winapi.NtSecApi, Winapi.ntlsa, Ntapi.ntstatus,
  NtUtils.ApiExtension, System.SysUtils;

function LsaxEnumeratePrivileges(out Privileges: TPrivDefArray): NTSTATUS;
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

function LsaxQueryNamePrivilege(Luid: TLuid; out Name: String): NTSTATUS;
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

function LsaxQueryDescriptionPrivilege(const Name: String;
  out DisplayName: String): NTSTATUS;
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

procedure LsaxLookupSids(Sids: TSidDynArray;
  out TranslatedNames: TTranslatedNames);
var
  Status: NTSTATUS;
  LsaHandle: TLsaHandle;
  ObjAttr: TObjectAttributes;
  ReferencedDomains: PLsaReferencedDomainList;
  Names: PLsaTranslatedNameArray;
  i: Integer;
begin
  InitializeObjectAttributes(ObjAttr);

  // Connect to LSA
  Status := LsaOpenPolicy(nil, ObjAttr, POLICY_LOOKUP_NAMES, LsaHandle);

  if NT_SUCCESS(Status) then
  begin
    // Request translation of all SIDs at once
    Status := LsaLookupSids(LsaHandle, Length(Sids), Sids, ReferencedDomains,
      Names);

    // Even without mapping names it converts most of them to SDDL
    if Status = STATUS_NONE_MAPPED then
      Status := STATUS_SOME_NOT_MAPPED;
  end;

  SetLength(TranslatedNames, Length(SIDs));

  if NT_SUCCESS(Status) then
  begin
    for i := 0 to High(Sids) do
    begin
      TranslatedNames[i].SidType := Names[i].Use;

      // If an SID has a known name, LsaLookupSids returns it.
      // Otherwise, Names[i].Name field contains SID's SDDL representation.
      // However, sometimes it does not. In this case we convert it explicitly.

      if Names[i].Use in [SidTypeInvalid, SidTypeUnknown] then
      begin
        // Only SDDL representation is suitable for these SID types

        if Names[i].Name.ToString.StartsWith('S-1-') then
          TranslatedNames[i].SDDL := Names[i].Name.ToString
        else
          TranslatedNames[i].SDDL := RtlxConvertSidToString(Sids[i]);
      end
      else
      begin
        TranslatedNames[i].User := Names[i].Name.ToString;

        // Negative DomainIndex means the SID does not reference a domain
        if (Names[i].DomainIndex >= 0) and
          (Names[i].DomainIndex < ReferencedDomains.Entries) then
          TranslatedNames[i].Domain := ReferencedDomains.Domains[
            Names[i].DomainIndex].Name.ToString
        else
          TranslatedNames[i].Domain := '';

        TranslatedNames[i].SDDL := RtlxConvertSidToString(Sids[i]);
      end;
    end;

    LsaFreeMemory(ReferencedDomains);
    LsaFreeMemory(Names);
  end
  else
  begin
    // Lookup failed, only SDDL representations available
    for i := 0 to High(Sids) do
      TranslatedNames[i].SDDL := RtlxConvertSidToString(Sids[i]);
  end;
end;

{ TTranslatedName }

function TTranslatedName.FullName: String;
begin
  if (User <> '') and (Domain <> '') then
    Result := Domain + '\' + User
  else if (Domain <> '') then
    Result := Domain
  else if (User <> '') then
    Result := User
  else
    Result := SDDL;
end;

function TTranslatedName.HasName: Boolean;
begin
  Result := (User <> '') or (Domain <> '');
end;

end.
