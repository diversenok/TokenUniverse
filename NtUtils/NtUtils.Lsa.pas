unit NtUtils.Lsa;

interface

uses
  Winapi.WinNt, Winapi.ntlsa, NtUtils.Exceptions, NtUtils.Security.Sid;

type
  TNtxStatus = NtUtils.Exceptions.TNtxStatus;
  TTranslatedName = NtUtils.Security.Sid.TTranslatedName;
  TLsaHandle = Winapi.ntlsa.TLsaHandle;

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

{ ---------------------------- Basic operations ----------------------------- }

// Open LSA for desired access
function LsaxOpenPolicy(out PolicyHandle: TLsaHandle;
  DesiredAccess: TAccessMask): TNtxStatus;

// Close LSA handle
procedure LsaxClose(var LsaHandle: TLsaHandle);

// Open an account from LSA database
function LsaxOpenAccount(out AccountHandle: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Add an account to LSA database
function LsaxCreateAccount(out AccountHandle: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;

{ -------------------------------- Privileges ------------------------------- }

// Enumerate all privileges on the system
function LsaxEnumeratePrivileges(out Privileges: TPrivDefArray): TNtxStatus;

// Convert a numerical privilege value to internal name
function LsaxQueryNamePrivilege(Luid: TLuid; out Name: String): TNtxStatus;

// Convert an privilege's internal name to a description
function LsaxQueryDescriptionPrivilege(const Name: String;
  out DisplayName: String): TNtxStatus;

// Get the minimal integrity level required to use a specific privilege
function LsaxQueryIntegrityPrivilege(Luid: TLuid): Cardinal;

// Enumerate privileges assigned to an account
function LsaxEnumerateAccountPrivileges(Sid: PSid;
  out Privileges: TPrivilegeArray): TNtxStatus;

// Add and remove privileges from account
function LsaxManagePrivilegesAccount(Sid: PSid; RemoveAll: Boolean;
  PrivilegesToAdd, PrivilegesToRemove: TPrivilegeArray): TNtxStatus;

{ ------------------------------- Logon Rights ------------------------------ }

// Enumerate known logon rights
function LsaxEnumerateLogonRights: TLogonRightRecArray;

// Query logon rights of an account
function LsaxQueryRightsAccount(Sid: PSid; out SystemAccess: Cardinal):
  TNtxStatus;

// Set logon rights of an account
function LsaxSetRightsAccount(Sid: PSid; SystemAccess: Cardinal): TNtxStatus;

{ ----------------------------- SID translation ----------------------------- }

// Convert SIDs to account names or at least to SDDL; always succeeds
function LsaxLookupSid(Sid: PSid): TTranslatedName;
function LsaxLookupSids(Sids: TSidDynArray): TTranslatedNames;

// Lookup an account on the machine
function LsaxLookupUserName(UserName: String; out Sid: ISid): TNtxStatus;

// Get current user name and domain
function LsaxGetUserName(out Domain, UserName: String): TNtxStatus;

{ ------------------------------ Logon Sessions ----------------------------- }

// Enumerate logon sessions
function LsaxEnumerateLogonSessions(out Luids: TLuidDynArray): TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Winapi.NtSecApi, Ntapi.ntseapi, System.SysUtils;

{ Basic operation }

function LsaxOpenPolicy(out PolicyHandle: TLsaHandle; DesiredAccess: TAccessMask):
  TNtxStatus;
var
  ObjAttr: TObjectAttributes;
begin
  InitializeObjectAttributes(ObjAttr);

  Result.Location := 'LsaOpenPolicy';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objLsaPolicy;

  Result.Status := LsaOpenPolicy(nil, ObjAttr, DesiredAccess, PolicyHandle);
end;

procedure LsaxClose(var LsaHandle: TLsaHandle);
begin
  LsaClose(LsaHandle);
  LsaHandle := 0;
end;

function LsaxOpenAccount(out AccountHandle: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hPolicy: TLsaHandle;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_VIEW_LOCAL_INFORMATION);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaOpenAccount';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objLsaAccount;

  Result.Status := LsaOpenAccount(hPolicy, AccountSid, DesiredAccess,
    AccountHandle);

  LsaxClose(hPolicy);
end;

function LsaxCreateAccount(out AccountHandle: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hPolicy: TLsaHandle;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_CREATE_ACCOUNT);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaCreateAccount';
  Result.Status := LsaCreateAccount(hPolicy, AccountSid, DesiredAccess,
    AccountHandle);

  LsaxClose(hPolicy);
end;

{ Privileges }

function LsaxEnumeratePrivileges(out Privileges: TPrivDefArray): TNtxStatus;
var
  hPolicy: TLsaHandle;
  EnumContext: Cardinal;
  Count, i: Integer;
  Buffer: PPolicyPrivilegeDefinitionArray;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_VIEW_LOCAL_INFORMATION);

  if not Result.IsSuccess then
    Exit;

  EnumContext := 0;
  Result.Location := 'LsaEnumeratePrivileges';
  Result.Status := LsaEnumeratePrivileges(hPolicy, EnumContext, Buffer,
    MAX_PREFERRED_LENGTH, Count);

  if Result.IsSuccess then
  begin
    SetLength(Privileges, Count);

    for i := 0 to Count - 1 do
    begin
      Privileges[i].Name := Buffer[i].Name.ToString;
      Privileges[i].LocalValue := Buffer[i].LocalValue;
    end;

    LsaFreeMemory(Buffer);
  end;

  LsaxClose(hPolicy);
end;

function LsaxQueryNamePrivilege(Luid: TLuid; out Name: String): TNtxStatus;
var
  hPolicy: TLsaHandle;
  Buffer: PLsaUnicodeString;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_LOOKUP_NAMES);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaLookupPrivilegeName';
  Result.Status := LsaLookupPrivilegeName(hPolicy, Luid, Buffer);

  if Result.IsSuccess then
  begin
    Name := Buffer.ToString;
    LsaFreeMemory(Buffer);
  end;

  LsaxClose(hPolicy);
end;

function LsaxQueryDescriptionPrivilege(const Name: String;
  out DisplayName: String): TNtxStatus;
var
  hPolicy: TLsaHandle;
  NameStr: TLsaUnicodeString;
  BufferDisplayName: PLsaUnicodeString;
  LangId: SmallInt;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_LOOKUP_NAMES);

  if not Result.IsSuccess then
    Exit;

  NameStr.FromString(Name);

  Result.Location := 'LsaLookupPrivilegeDisplayName';
  Result.Status := LsaLookupPrivilegeDisplayName(hPolicy, NameStr,
    BufferDisplayName, LangId);

  if Result.IsSuccess then
  begin
    DisplayName := BufferDisplayName.ToString;
    LsaFreeMemory(BufferDisplayName);
  end;

  LsaxClose(hPolicy);
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
  hAccount: TLsaHandle;
  PrivilegeSet: PPrivilegeSet;
  i: Integer;
begin
  Result := LsaxOpenAccount(hAccount, Sid, ACCOUNT_VIEW);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaEnumeratePrivilegesOfAccount';
  Result.Status := LsaEnumeratePrivilegesOfAccount(hAccount, PrivilegeSet);

  LsaxClose(hAccount);

  if Result.IsSuccess then
  begin
    SetLength(Privileges, PrivilegeSet.PrivilegeCount);

    for i := 0 to High(Privileges) do
      Privileges[i] := PrivilegeSet.Privilege[i];
  end;

  LsaFreeMemory(PrivilegeSet);
end;

function LsaxManagePrivilegesAccount(Sid: PSid; RemoveAll: Boolean;
  PrivilegesToAdd, PrivilegesToRemove: TPrivilegeArray): TNtxStatus;
var
  hAccount: TLsaHandle;
  PrivSet: PPrivilegeSet;
  i: Integer;
begin
  Result := LsaxOpenAccount(hAccount, Sid, ACCOUNT_ADJUST_PRIVILEGES);

  // If the account does not exist in the LSA database we should create it
  if Result.Status = STATUS_OBJECT_NAME_NOT_FOUND then
    Result := LsaxCreateAccount(hAccount, Sid, ACCOUNT_ADJUST_PRIVILEGES);

  if not Result.IsSuccess then
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
      Result.Status := LsaAddPrivilegesToAccount(hAccount, PrivSet);
    finally
      FreeMem(PrivSet);
    end;

    // Quit on error
    if not Result.IsSuccess then
    begin
      LsaxClose(hAccount);
      Exit;
    end;
  end;

  // Remove privileges
  if RemoveAll then
  begin
    Result.Location := 'LsaRemovePrivilegesFromAccount';
    Result.Status := LsaRemovePrivilegesFromAccount(hAccount, True, nil);
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
      Result.Status := LsaRemovePrivilegesFromAccount(hAccount, False,
        PrivSet);
    finally
      FreeMem(PrivSet);
    end;
  end;

  LsaxClose(hAccount);
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
  hAccount: TLsaHandle;
begin
  Result := LsaxOpenAccount(hAccount, Sid, ACCOUNT_VIEW);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaGetSystemAccessAccount';
  Result.Status := LsaGetSystemAccessAccount(hAccount, SystemAccess);

  LsaxClose(hAccount);
end;

function LsaxSetRightsAccount(Sid: PSid; SystemAccess: Cardinal): TNtxStatus;
var
  hAccount: TLsaHandle;
begin
  Result := LsaxOpenAccount(hAccount, Sid, ACCOUNT_ADJUST_SYSTEM_ACCESS);

  // If the account does not exist in the LSA database we should create it
  if Result.Status = STATUS_OBJECT_NAME_NOT_FOUND then
    Result := LsaxCreateAccount(hAccount, Sid, ACCOUNT_ADJUST_SYSTEM_ACCESS);

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaSetSystemAccessAccount';
  Result.Status := LsaSetSystemAccessAccount(hAccount, SystemAccess);
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
  hPolicy: TLsaHandle;
  Status: TNtxStatus;
  BufferDomains: PLsaReferencedDomainList;
  BufferNames: PLsaTranslatedNameArray;
  i: Integer;
begin
  Status := LsaxOpenPolicy(hPolicy, POLICY_LOOKUP_NAMES);

  if Status.IsSuccess then
  begin
    // Request translation of all SIDs at once
    Status.Location := 'LsaLookupSids';
    Status.Status := LsaLookupSids(hPolicy, Length(Sids), Sids, BufferDomains,
      BufferNames);

    // Even without mapping BufferNames it converts most of them to SDDL
    if Status.Status = STATUS_NONE_MAPPED then
      Status.Status := STATUS_SOME_NOT_MAPPED;

    LsaxClose(hPolicy);
  end;

  SetLength(Result, Length(SIDs));

  if Status.IsSuccess then
  begin
    for i := 0 to High(Sids) do
    begin
      Result[i].SidType := BufferNames[i].Use;

      // If an SID has a known name, LsaLookupSids returns it.
      // Otherwise, BufferNames[i].Name field contains SID's SDDL representation.
      // However, sometimes it does not. In this case we convert it explicitly.

      if BufferNames[i].Use in [SidTypeInvalid, SidTypeUnknown] then
      begin
        // Only SDDL representation is suitable for these SID types

        Result[i].SDDL := BufferNames[i].Name.ToString;

        if not Result[i].SDDL.StartsWith('S-1-') then
          Result[i].SDDL := RtlxConvertSidToString(Sids[i]);
      end
      else
      begin
        Result[i].UserName := BufferNames[i].Name.ToString;

        // Negative DomainIndex means the SID does not reference a domain
        if (BufferNames[i].DomainIndex >= 0) and
          (BufferNames[i].DomainIndex < BufferDomains.Entries) then
          Result[i].DomainName := BufferDomains.Domains[
            BufferNames[i].DomainIndex].Name.ToString
        else
          Result[i].DomainName := '';

        Result[i].SDDL := RtlxConvertSidToString(Sids[i]);
      end;
    end;

    LsaFreeMemory(BufferDomains);
    LsaFreeMemory(BufferNames);
  end
  else
  begin
    // Lookup failed, only SDDL representations available
    for i := 0 to High(Sids) do
      Result[i].SDDL := RtlxConvertSidToString(Sids[i]);
  end;
end;

function LsaxLookupUserName(UserName: String; out Sid: ISid): TNtxStatus;
var
  hPolicy: TLsaHandle;
  Name: TLsaUnicodeString;
  BufferDomain: PLsaReferencedDomainList;
  BufferTranslatedSid: PLsaTranslatedSid2;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_LOOKUP_NAMES);

  if not Result.IsSuccess then
    Exit;

  Name.FromString(UserName);

  // Request translation of one name
  Result.Location := 'LsaLookupNames2';
  Result.Status := LsaLookupNames2(hPolicy, 0, 1, Name, BufferDomain,
    BufferTranslatedSid);

  if Result.IsSuccess then
    Sid := TSid.CreateCopy(BufferTranslatedSid.Sid);

  // LsaLookupNames2 allocates memory even on some errors
  if Result.IsSuccess or (Result.Status = STATUS_NONE_MAPPED)  then
  begin
    LsaFreeMemory(BufferDomain);
    LsaFreeMemory(BufferTranslatedSid);
  end;
end;

function LsaxGetUserName(out Domain, UserName: String): TNtxStatus;
var
  BufferUser, BufferDomain: PLsaUnicodeString;
begin
  Result.Location := 'LsaGetUserName';
  Result.Status := LsaGetUserName(BufferUser, BufferDomain);

  if not Result.IsSuccess then
    Exit;

  Domain := BufferDomain.ToString;
  UserName := BufferUser.ToString;

  LsaFreeMemory(BufferUser);
  LsaFreeMemory(BufferDomain);
end;

{ Logon Sessions }

function LsaxEnumerateLogonSessions(out Luids: TLuidDynArray): TNtxStatus;
var
  Count, i: Integer;
  Buffer: PLuidArray;
begin
  Result.Location := 'LsaEnumerateLogonSessions';
  Result.Status := LsaEnumerateLogonSessions(Count, Buffer);

  if not Result.IsSuccess then
    Exit;

  // TODO: manually add anonymous 3E6 logon

  SetLength(Luids, Count);

  // Invert the order so that later logons appear later in the list
  for i := 0 to Count - 1 do
    Luids[i] := Buffer[Count - 1 - i];

  LsaFreeReturnBuffer(Buffer);
end;

end.
