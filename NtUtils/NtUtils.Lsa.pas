unit NtUtils.Lsa;

interface

uses
  Winapi.WinNt, Ntapi.ntdef, NtUtils.Exceptions;

type
  TNtxStatus = NtUtils.Exceptions.TNtxStatus;

  TAuditEntitiy = record
    Value: TGuid;
    Name: String;
  end;

  TAuditCategories = record
    Categories: array of TAuditEntitiy;
    SubCategories: array of array of TAuditEntitiy;
  end;

  TPrivilegeDefinition = record
    Name: String;
    LocalValue: TLuid;
  end;

  TPrivDefArray = array of TPrivilegeDefinition;

  TLogonRightRec = record
    Value: Cardinal;
    AllowedType: Boolean;
    Name, Description: String;
  end;

  TLogonRightRecArray = array of TLogonRightRec;

  TTranslatedName = record
    DomainName, UserName, SDDL: String;
    SidType: TSidNameUse;
    function HasName: Boolean;
    function FullName: String;
  end;

  TTranslatedNames = array of TTranslatedName;

{ ---------------------------------- Audit ---------------------------------- }

// AuditEnumerateCategories & AuditEnumerateSubCategories
function LsaxEnumerateAuditCategiries(out Items: TAuditCategories): NTSTATUS;

{ -------------------------------- Privileges ------------------------------- }

// LsaEnumeratePrivileges
function LsaxEnumeratePrivileges(out Privileges: TPrivDefArray): NTSTATUS;

// LsaLookupPrivilegeName
function LsaxQueryNamePrivilege(Luid: TLuid; out Name: String): NTSTATUS;

// LsaLookupPrivilegeDisplayName
function LsaxQueryDescriptionPrivilege(const Name: String;
  out DisplayName: String): NTSTATUS;

// Get the minimal integrity level required to use a specific privilege
function LsaxQueryIntegrityPrivilege(Luid: TLuid): Cardinal;

// LsaEnumeratePrivilegesOfAccount
function LsaxEnumerateAccountPrivileges(Sid: PSid;
  out Privileges: TPrivilegeArray): TNtxStatus;

// LsaAddPrivilegesToAccount & LsaRemovePrivilegesFromAccount
function LsaxManagePrivilegesAccount(Sid: PSid; RemoveAll: Boolean;
  PrivilegesToAdd, PrivilegesToRemove: TPrivilegeArray): TNtxStatus;

{ ------------------------------- Logon Rights ------------------------------ }

// Enumerate known logon rights
function LsaxEnumerateLogonRights: TLogonRightRecArray;

// LsaGetSystemAccessAccount
function LsaxQueryRightsAccount(Sid: PSid; out SystemAccess: Cardinal):
  TNtxStatus;

// LsaSetSystemAccessAccount
function LsaxSetRightsAccount(Sid: PSid; SystemAccess: Cardinal): TNtxStatus;

{ ----------------------------- SID translation ----------------------------- }

// LsaLookupSids mixed with RtlConvertSidToUnicodeString, always succeeds
function LsaxLookupSid(Sid: PSid): TTranslatedName;
function LsaxLookupSids(Sids: TSidDynArray): TTranslatedNames;

// LsaLookupNames2, on success the SID buffer must be freed using FreeMem
function LsaxLookupUserName(UserName: String; out Sid: PSid): NTSTATUS;

implementation

uses
  Winapi.NtSecApi, Winapi.ntlsa, Ntapi.ntstatus, Ntapi.ntrtl, Ntapi.ntseapi,
  System.SysUtils, NtUtils.ApiExtension;

{ Audit }

function LsaxEnumerateAuditCategiries(out Items: TAuditCategories): NTSTATUS;
var
  Guids, SubGuids: PGuidArray;
  Count, SubCount: Cardinal;
  Ind, SubInd: Integer;
  Buffer: PWideChar;
begin
  Result := STATUS_SUCCESS;
  SetLength(Items.Categories, 0);
  SetLength(Items.SubCategories, 0, 0);

  // Query categories
  if not AuditEnumerateCategories(Guids, Count) then
    Exit(RtlxGetLastNtStatus);

  SetLength(Items.Categories, Count);
  SetLength(Items.SubCategories, Count, 0);

  // Go through all categories
  for Ind := 0 to High(Items.Categories) do
  begin
    Items.Categories[Ind].Value := Guids[Ind];

    // Query category name
    if AuditLookupCategoryNameW(Guids[Ind], Buffer) then
    begin
       Items.Categories[Ind].Name := String(Buffer);
       AuditFree(Buffer);
    end
    else
      Items.Categories[Ind].Name := GUIDToString(Guids[Ind]);

    // Query subcategories of this category
    if not AuditEnumerateSubCategories(Guids[Ind], False, SubGuids, SubCount)
      then
      Exit(RtlxGetLastNtStatus);

    SetLength(Items.SubCategories[Ind], SubCount);

    // Go through all subcategories
    for SubInd := 0 to High(Items.SubCategories[Ind]) do
    begin
      Items.SubCategories[Ind, SubInd].Value := SubGuids[SubInd];

      // Query subcategory name
      if AuditLookupSubCategoryNameW(SubGuids[SubInd], Buffer) then
      begin
        Items.SubCategories[Ind, SubInd].Name := String(Buffer);
        AuditFree(Buffer);
      end
      else
        Items.SubCategories[Ind, SubInd].Name := GUIDToString(SubGuids[SubInd]);
    end;

    AuditFree(SubGuids);
  end;

  AuditFree(Guids);
end;

{ Privileges }

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

function LsaxQueryIntegrityPrivilege(Luid: TLuid): Cardinal;
begin
  // Some privileges require a specific integrity level to be enabled.
  // The ones that require more than Medium also trigger UAC to split logon
  // sessions. The following data is gathered by experimenting and should be
  // maintained in sync with Windows behavior when new privileges are
  // introduced.

  case Luid of
    // Ten of them require High
    SE_CREATE_TOKEN_PRIVILEGE,
    SE_TCB_PRIVILEGE,
    SE_TAKE_OWNERSHIP_PRIVILEGE,
    SE_LOAD_DRIVER_PRIVILEGE,
    SE_BACKUP_PRIVILEGE,
    SE_RESTORE_PRIVILEGE,
    SE_DEBUG_PRIVILEGE,
    SE_IMPERSONATE_PRIVILEGE,
    SE_RELABEL_PRIVILEGE,
    SE_DELEGATE_SESSION_USER_IMPERSONATE_PRIVILEGE:
      Result := SECURITY_MANDATORY_HIGH_RID;

    // Three of them does not require anything
    SE_CHANGE_NOTIFY_PRIVILEGE,
    SE_UNDOCK_PRIVILEGE,
    SE_INC_WORKING_SET_PRIVILEGE:
      Result := SECURITY_MANDATORY_UNTRUSTED_RID;

  else
    // All other require Medium
    Result := SECURITY_MANDATORY_MEDIUM_RID;
  end;
end;

function LsaxEnumerateAccountPrivileges(Sid: PSid;
  out Privileges: TPrivilegeArray): TNtxStatus;
var
  LsaHandle, AccountHandle: TLsaHandle;
  ObjAttr: TObjectAttributes;
  PrivilegeSet: PPrivilegeSet;
  i: Integer;
begin
  SetLength(Privileges, 0);
  InitializeObjectAttributes(ObjAttr);

  Result.Location := 'LsaOpenPolicy';
  Result.Status := LsaOpenPolicy(nil, ObjAttr, POLICY_VIEW_LOCAL_INFORMATION,
    LsaHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaOpenAccount';
  Result.Status := LsaOpenAccount(LsaHandle, Sid, ACCOUNT_VIEW, AccountHandle);
  LsaClose(LsaHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaEnumeratePrivilegesOfAccount';
  Result.Status := LsaEnumeratePrivilegesOfAccount(AccountHandle, PrivilegeSet);
  LsaClose(AccountHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  SetLength(Privileges, PrivilegeSet.PrivilegeCount);

  for i := 0 to High(Privileges) do
    Privileges[i] := PrivilegeSet.Privilege[i];

  LsaFreeMemory(PrivilegeSet);
end;

function LsaxManagePrivilegesAccount(Sid: PSid; RemoveAll: Boolean;
  PrivilegesToAdd, PrivilegesToRemove: TPrivilegeArray): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  LsaHandle, AccountHandle: TLsaHandle;
  PrivSet: PPrivilegeSet;
  i: Integer;
begin
  InitializeObjectAttributes(ObjAttr);

  Result.Location := 'LsaOpenPolicy for POLICY_VIEW_LOCAL_INFORMATION';
  Result.Status := LsaOpenPolicy(nil, ObjAttr, POLICY_VIEW_LOCAL_INFORMATION,
    LsaHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaOpenAccount for ACCOUNT_ADJUST_PRIVILEGES';
  Result.Status := LsaOpenAccount(LsaHandle, Sid, ACCOUNT_ADJUST_PRIVILEGES,
    AccountHandle);

  LsaClose(LsaHandle);

  // If the account does not exist in the LSA database we should create it
  if Result.Status = STATUS_OBJECT_NAME_NOT_FOUND then
  begin
    // Connect to LSA for creating a new account
    Result.Location := 'LsaOpenPolicy for POLICY_CREATE_ACCOUNT';
    Result.Status := LsaOpenPolicy(nil, ObjAttr, POLICY_CREATE_ACCOUNT,
      LsaHandle);

    if not NT_SUCCESS(Result.Status) then
      Exit;

    Result.Location := 'LsaCreateAccount for ACCOUNT_ADJUST_PRIVILEGES';
    Result.Status := LsaCreateAccount(LsaHandle, Sid,
      ACCOUNT_ADJUST_PRIVILEGES, AccountHandle);

    LsaClose(LsaHandle);
  end;

  if not NT_SUCCESS(Result.Status) then
    Exit;

  // Add privileges
  if Length(PrivilegesToAdd) > 0 then
  begin
    PrivSet := AllocMem(SizeOf(Cardinal) + SizeOf(Cardinal) +
      SizeOf(TLuidAndAttributes) * Length(PrivilegesToAdd));

    try
      PrivSet.PrivilegeCount := Length(PrivilegesToAdd);
      PrivSet.Control := 0;

      for i := 0 to High(PrivilegesToAdd) do
        PrivSet.Privilege[i] := PrivilegesToAdd[i];

      Result.Location := 'LsaAddPrivilegesToAccount';
      Result.Status := LsaAddPrivilegesToAccount(AccountHandle, PrivSet);
    finally
      FreeMem(PrivSet);
    end;

    // Quit on error
    if not NT_SUCCESS(Result.Status) then
    begin
      LsaClose(AccountHandle);
      Exit;
    end;
  end;

  // Remove privileges
  if RemoveAll then
  begin
    Result.Location := 'LsaRemovePrivilegesFromAccount';
    Result.Status := LsaRemovePrivilegesFromAccount(AccountHandle, True, nil);
  end
  else if Length(PrivilegesToRemove) > 0 then
  begin
    PrivSet := AllocMem(SizeOf(Cardinal) + SizeOf(Cardinal) +
      SizeOf(TLuidAndAttributes) * Length(PrivilegesToRemove));

    try
      PrivSet.PrivilegeCount := Length(PrivilegesToRemove);
      PrivSet.Control := 0;

      for i := 0 to High(PrivilegesToRemove) do
        PrivSet.Privilege[i] := PrivilegesToRemove[i];

      Result.Location := 'LsaRemovePrivilegesFromAccount';
      Result.Status := LsaRemovePrivilegesFromAccount(AccountHandle, False,
        PrivSet);
    finally
      FreeMem(PrivSet);
    end;
  end;

  LsaClose(AccountHandle);
end;

{ Logon rights }

function LsaxEnumerateLogonRights: TLogonRightRecArray;
begin
  // If someone knows a system function to enumerate logon rights on the system
  // you are welcome to use it here.

  SetLength(Result, 10);

  Result[0].Value := SECURITY_ACCESS_INTERACTIVE_LOGON;
  Result[0].AllowedType := True;
  Result[0].Name := SE_INTERACTIVE_LOGON_NAME;
  Result[0].Description := 'Allow interactive logon';

  Result[1].Value := SECURITY_ACCESS_NETWORK_LOGON;
  Result[1].AllowedType := True;
  Result[1].Name := SE_NETWORK_LOGON_NAME;
  Result[1].Description := 'Allow network logon';

  Result[2].Value := SECURITY_ACCESS_BATCH_LOGON;
  Result[2].AllowedType := True;
  Result[2].Name := SE_BATCH_LOGON_NAME;
  Result[2].Description := 'Allow batch job logon';

  Result[3].Value := SECURITY_ACCESS_SERVICE_LOGON;
  Result[3].AllowedType := True;
  Result[3].Name := SE_SERVICE_LOGON_NAME;
  Result[3].Description := 'Allow service logon';

  Result[4].Value := SECURITY_ACCESS_REMOTE_INTERACTIVE_LOGON;
  Result[4].AllowedType := True;
  Result[4].Name := SE_REMOTE_INTERACTIVE_LOGON_NAME;
  Result[4].Description := 'Allow Remote Desktop Services logon';

  Result[5].Value := SECURITY_ACCESS_DENY_INTERACTIVE_LOGON;
  Result[5].AllowedType := False;
  Result[5].Name := SE_DENY_INTERACTIVE_LOGON_NAME;
  Result[5].Description := 'Deny interactive logon';

  Result[6].Value := SECURITY_ACCESS_DENY_NETWORK_LOGON;
  Result[6].AllowedType := False;
  Result[6].Name := SE_DENY_NETWORK_LOGON_NAME;
  Result[6].Description := 'Deny network logon';

  Result[7].Value := SECURITY_ACCESS_DENY_BATCH_LOGON;
  Result[7].AllowedType := False;
  Result[7].Name := SE_DENY_BATCH_LOGON_NAME;
  Result[7].Description := 'Deny batch job logon';

  Result[8].Value := SECURITY_ACCESS_DENY_SERVICE_LOGON;
  Result[8].AllowedType := False;
  Result[8].Name := SE_DENY_SERVICE_LOGON_NAME;
  Result[8].Description := 'Deny service logon';

  Result[9].Value := SECURITY_ACCESS_DENY_REMOTE_INTERACTIVE_LOGON;
  Result[9].AllowedType := False;
  Result[9].Name := SE_DENY_REMOTE_INTERACTIVE_LOGON_NAME;
  Result[9].Description := 'Deny Remote Desktop Services logon';
end;

function LsaxQueryRightsAccount(Sid: PSid; out SystemAccess: Cardinal):
  TNtxStatus;
var
  LsaHandle, AccountHandle: TLsaHandle;
  ObjAttr: TObjectAttributes;
begin
  SystemAccess := 0;
  InitializeObjectAttributes(ObjAttr);

  Result.Location := 'LsaOpenPolicy';
  Result.Status := LsaOpenPolicy(nil, ObjAttr, POLICY_VIEW_LOCAL_INFORMATION,
    LsaHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaOpenAccount';
  Result.Status := LsaOpenAccount(LsaHandle, Sid, ACCOUNT_VIEW, AccountHandle);
  LsaClose(LsaHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaGetSystemAccessAccount';
  Result.Status := LsaGetSystemAccessAccount(AccountHandle, SystemAccess);
  LsaClose(AccountHandle);
end;

function LsaxSetRightsAccount(Sid: PSid; SystemAccess: Cardinal): TNtxStatus;
var
  LsaHandle, AccountHandle: TLsaHandle;
  ObjAttr: TObjectAttributes;
begin
  InitializeObjectAttributes(ObjAttr);

  // Connect to LSA for opening existing account
  Result.Location := 'LsaOpenPolicy for POLICY_VIEW_LOCAL_INFORMATION';
  Result.Status := LsaOpenPolicy(nil, ObjAttr, POLICY_VIEW_LOCAL_INFORMATION,
    LsaHandle);

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaOpenAccount for ACCOUNT_ADJUST_SYSTEM_ACCESS';
  Result.Status := LsaOpenAccount(LsaHandle, Sid, ACCOUNT_ADJUST_SYSTEM_ACCESS,
    AccountHandle);

  LsaClose(LsaHandle);

  // If the account does not exist in the LSA database we should create it
  if Result.Status = STATUS_OBJECT_NAME_NOT_FOUND then
  begin
    // Connect to LSA for creating a new account
    Result.Location := 'LsaOpenPolicy for POLICY_CREATE_ACCOUNT';
    Result.Status := LsaOpenPolicy(nil, ObjAttr, POLICY_CREATE_ACCOUNT,
      LsaHandle);

    if not NT_SUCCESS(Result.Status) then
      Exit;

    Result.Location := 'LsaCreateAccount for ACCOUNT_ADJUST_SYSTEM_ACCESS';
    Result.Status := LsaCreateAccount(LsaHandle, Sid,
      ACCOUNT_ADJUST_SYSTEM_ACCESS, AccountHandle);

    LsaClose(LsaHandle);
  end;

  if not NT_SUCCESS(Result.Status) then
    Exit;

  Result.Location := 'LsaSetSystemAccessAccount';
  Result.Status := LsaSetSystemAccessAccount(AccountHandle, SystemAccess);
end;

{ SID translation}

function LsaxLookupSid(Sid: PSid): TTranslatedName;
var
  Sids: TSidDynArray;
begin
  SetLength(Sids, 1);
  Sids[0] := Sid;

  Result := LsaxLookupSids(Sids)[0];
end;

function LsaxLookupSids(Sids: TSidDynArray): TTranslatedNames;
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

    LsaClose(LsaHandle);
  end;

  SetLength(Result, Length(SIDs));

  if NT_SUCCESS(Status) then
  begin
    for i := 0 to High(Sids) do
    begin
      Result[i].SidType := Names[i].Use;

      // If an SID has a known name, LsaLookupSids returns it.
      // Otherwise, Names[i].Name field contains SID's SDDL representation.
      // However, sometimes it does not. In this case we convert it explicitly.

      if Names[i].Use in [SidTypeInvalid, SidTypeUnknown] then
      begin
        // Only SDDL representation is suitable for these SID types

        Result[i].SDDL := Names[i].Name.ToString;

        if not Result[i].SDDL.StartsWith('S-1-') then
          Result[i].SDDL := RtlxConvertSidToString(Sids[i]);
      end
      else
      begin
        Result[i].UserName := Names[i].Name.ToString;

        // Negative DomainIndex means the SID does not reference a domain
        if (Names[i].DomainIndex >= 0) and
          (Names[i].DomainIndex < ReferencedDomains.Entries) then
          Result[i].DomainName := ReferencedDomains.Domains[
            Names[i].DomainIndex].Name.ToString
        else
          Result[i].DomainName := '';

        Result[i].SDDL := RtlxConvertSidToString(Sids[i]);
      end;
    end;

    LsaFreeMemory(ReferencedDomains);
    LsaFreeMemory(Names);
  end
  else
  begin
    // Lookup failed, only SDDL representations available
    for i := 0 to High(Sids) do
      Result[i].SDDL := RtlxConvertSidToString(Sids[i]);
  end;
end;

function LsaxLookupUserName(UserName: String; out Sid: PSid): NTSTATUS;
var
  LsaHandle: TLsaHandle;
  ObjAttr: TObjectAttributes;
  Name: TLsaUnicodeString;
  ReferencedDomain: PLsaReferencedDomainList;
  TranslatedSid: PLsaTranslatedSid2;
  BufferSize: Cardinal;
begin
  Sid := nil;
  Name.FromString(UserName);
  InitializeObjectAttributes(ObjAttr);

  // Connect to LSA
  Result := LsaOpenPolicy(nil, ObjAttr, POLICY_LOOKUP_NAMES, LsaHandle);

  if not NT_SUCCESS(Result) then
    Exit;

  // Request translation of one name
  Result := LsaLookupNames2(LsaHandle, 0, 1, Name, ReferencedDomain,
    TranslatedSid);

  if Result = STATUS_NONE_MAPPED then
  begin
    LsaFreeMemory(ReferencedDomain);
    LsaFreeMemory(TranslatedSid);
    Exit;
  end;

  if NT_SUCCESS(Result) then
  begin
    // Allocate memory and copy SID

    BufferSize := RtlLengthSid(TranslatedSid.Sid);
    Sid := AllocMem(BufferSize);

    Result := RtlCopySid(BufferSize, Sid, TranslatedSid.Sid);

    if not NT_SUCCESS(Result) then
    begin
      FreeMem(Sid);
      Sid := nil;
    end;

    LsaFreeMemory(ReferencedDomain);
    LsaFreeMemory(TranslatedSid);
  end;
end;

{ TTranslatedName }

function TTranslatedName.FullName: String;
begin
  if (UserName <> '') and (DomainName <> '') then
    Result := DomainName + '\' + UserName
  else if (DomainName <> '') then
    Result := DomainName
  else if (UserName <> '') then
    Result := UserName
  else
    Result := SDDL;
end;

function TTranslatedName.HasName: Boolean;
begin
  Result := (UserName <> '') or (DomainName <> '');
end;

end.
