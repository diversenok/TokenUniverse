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

  TLogonRightRec = record
    Value: Cardinal;
    IsAllowedType: Boolean;
    Name, Description: String;
  end;

{ --------------------------------- Policy ---------------------------------- }

// Open LSA for desired access
function LsaxOpenPolicy(out PolicyHandle: TLsaHandle;
  DesiredAccess: TAccessMask; SystemName: String = ''): TNtxStatus;

// Close LSA handle
procedure LsaxClose(var LsaHandle: TLsaHandle);

// Query policy information; free memory with LsaFreeMemory
function LsaxQueryPolicy(hPolicy: TLsaHandle; InfoClass:
  TPolicyInformationClass; out Status: TNtxStatus): Pointer;

// Set policy information
function LsaxSetPolicy(hPolicy: TLsaHandle; InfoClass: TPolicyInformationClass;
  Data: Pointer): TNtxStatus;

{ --------------------------------- Accounts -------------------------------- }

// Open an account from LSA database
function LsaxOpenAccount(out hAccount: TLsaHandle; hPolicy: TLsaHandle;
  AccountSid: PSid; DesiredAccess: TAccessMask): TNtxStatus;

function LsaxOpenAccountEx(out hAccount: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Add an account to LSA database
function LsaxCreateAccount(out hAccount: TLsaHandle; hPolicy: TLsaHandle;
  AccountSid: PSid; DesiredAccess: TAccessMask): TNtxStatus;

function LsaxCreateAccountEx(out hAccount: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;

// Delete account from LSA database
function LsaxDeleteAccount(hAccount: TLsaHandle): TNtxStatus;

// Enumerate account in the LSA database
function LsaxEnumerateAccounts(hPolicy: TLsaHandle; out Accounts: TArray<ISid>):
  TNtxStatus;

// Enumerate privileges assigned to an account
function LsaxEnumeratePrivilegesAccount(hAccount: TLsaHandle;
  out Privileges: TArray<TPrivilege>): TNtxStatus;

function LsaxEnumeratePrivilegesAccountBySid(AccountSid: PSid;
  out Privileges: TArray<TPrivilege>): TNtxStatus;

// Assign privileges to an account
function LsaxAddPrivilegesAccount(hAccount: TLsaHandle;
  Privileges: TArray<TPrivilege>): TNtxStatus;

// Revoke privileges to an account
function LsaxRemovePrivilegesAccount(hAccount: TLsaHandle; RemoveAll: Boolean;
  Privileges: TArray<TPrivilege>): TNtxStatus;

// Assign & revoke privileges to account in one operation
function LsaxManagePrivilegesAccount(AccountSid: PSid; RemoveAll: Boolean;
  Add, Remove: TArray<TPrivilege>): TNtxStatus;

// Query logon rights of an account
function LsaxQueryRightsAccount(hAccount: TLsaHandle;
  out SystemAccess: Cardinal): TNtxStatus;

function LsaxQueryRightsAccountBySid(AccountSid: PSid;
  out SystemAccess: Cardinal): TNtxStatus;

// Set logon rights of an account
function LsaxSetRightsAccount(hAccount: TLsaHandle; SystemAccess: Cardinal):
  TNtxStatus;

function LsaxSetRightsAccountBySid(AccountSid: PSid; SystemAccess: Cardinal):
  TNtxStatus;

{ -------------------------------- Privileges ------------------------------- }

// Enumerate all privileges on the system
function LsaxEnumeratePrivileges(hPolicy: TLsaHandle;
  out Privileges: TArray<TPrivilegeDefinition>): TNtxStatus;

function LsaxEnumeratePrivilegesLocal(
  out Privileges: TArray<TPrivilegeDefinition>): TNtxStatus;

// Convert a numerical privilege value to internal name
function LsaxQueryNamePrivilege(hPolicy: TLsaHandle; Luid: TLuid;
  out Name: String): TNtxStatus;

// Convert an privilege's internal name to a description
function LsaxQueryDescriptionPrivilege(hPolicy: TLsaHandle; const Name: String;
  out DisplayName: String): TNtxStatus;

// Lookup multiple privilege names and descriptions at once
function LsaxLookupMultiplePrivileges(Luids: TArray<TLuid>;
  out Names, Descriptions: TArray<String>): TNtxStatus;

// Get the minimal integrity level required to use a specific privilege
function LsaxQueryIntegrityPrivilege(Luid: TLuid): Cardinal;

{ ------------------------------- Logon Rights ------------------------------ }

// Enumerate known logon rights
function LsaxEnumerateLogonRights: TArray<TLogonRightRec>;

{ ----------------------------- SID translation ----------------------------- }

// Convert SIDs to account names or at least to SDDL; always succeeds
function LsaxLookupSid(Sid: PSid; var Name: TTranslatedName): TNtxStatus;
function LsaxLookupSids(Sids: TArray<PSid>; out Names: TArray<TTranslatedName>):
   TNtxStatus;

// Lookup an account on the machine
function LsaxLookupUserName(UserName: String; out Sid: ISid): TNtxStatus;

// Get current user name and domain
function LsaxGetUserName(out Domain, UserName: String): TNtxStatus; overload;
function LsaxGetUserName(out FullName: String): TNtxStatus; overload;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, Winapi.NtSecApi, Ntapi.ntseapi, System.SysUtils,
  NtUtils.Tokens.Misc, NtUtils.Access.Expected;

{ Basic operation }

function LsaxOpenPolicy(out PolicyHandle: TLsaHandle;
  DesiredAccess: TAccessMask; SystemName: String): TNtxStatus;
var
  ObjAttr: TObjectAttributes;
  SystemNameStr: TLsaUnicodeString;
  pSystemNameStr: PLsaUnicodeString;
begin
  InitializeObjectAttributes(ObjAttr);

  if SystemName <> '' then
  begin
    SystemNameStr.FromString(SystemName);
    pSystemNameStr := @SystemNameStr;
  end
  else
    pSystemNameStr := nil;

  Result.Location := 'LsaOpenPolicy';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objLsaPolicy;

  Result.Status := LsaOpenPolicy(pSystemNameStr, ObjAttr, DesiredAccess,
    PolicyHandle);
end;

procedure LsaxClose(var LsaHandle: TLsaHandle);
begin
  LsaClose(LsaHandle);
  LsaHandle := 0;
end;

function LsaxQueryPolicy(hPolicy: TLsaHandle; InfoClass:
  TPolicyInformationClass; out Status: TNtxStatus): Pointer;
begin
  Status.Location := 'LsaQueryInformationPolicy';
  Status.LastCall.CallType := lcQuerySetCall;
  Status.LastCall.InfoClass := Cardinal(InfoClass);
  Status.LastCall.InfoClassType := TypeInfo(TPolicyInformationClass);
  RtlxComputePolicyQueryAccess(Status.LastCall, InfoClass);

  Status.Status := LsaQueryInformationPolicy(hPolicy, InfoClass, Result);
end;

function LsaxSetPolicy(hPolicy: TLsaHandle; InfoClass: TPolicyInformationClass;
  Data: Pointer): TNtxStatus;
begin
  Result.Location := 'LsaSetInformationPolicy';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(InfoClass);
  Result.LastCall.InfoClassType := TypeInfo(TPolicyInformationClass);
  RtlxComputePolicySetAccess(Result.LastCall, InfoClass);

  Result.Status := LsaSetInformationPolicy(hPolicy, InfoClass, Data);
end;

{ Accounts }

function LsaxOpenAccount(out hAccount: TLsaHandle; hPolicy: TLsaHandle;
  AccountSid: PSid; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'LsaOpenAccount';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objLsaAccount;
  Result.LastCall.Expects(POLICY_VIEW_LOCAL_INFORMATION, objLsaPolicy);

  Result.Status := LsaOpenAccount(hPolicy, AccountSid, DesiredAccess, hAccount);
end;

function LsaxOpenAccountEx(out hAccount: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hPolicy: TLsaHandle;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_VIEW_LOCAL_INFORMATION);

  if Result.IsSuccess then
  begin
    Result := LsaxOpenAccount(hAccount, hPolicy, AccountSid, DesiredAccess);
    LsaxClose(hPolicy);
  end;
end;

function LsaxCreateAccount(out hAccount: TLsaHandle; hPolicy: TLsaHandle;
  AccountSid: PSid; DesiredAccess: TAccessMask): TNtxStatus;
begin
  Result.Location := 'LsaCreateAccount';
  Result.LastCall.Expects(POLICY_CREATE_ACCOUNT, objLsaPolicy);

  Result.Status := LsaCreateAccount(hPolicy, AccountSid, DesiredAccess,
    hAccount);
end;

function LsaxCreateAccountEx(out hAccount: TLsaHandle; AccountSid: PSid;
  DesiredAccess: TAccessMask): TNtxStatus;
var
  hPolicy: TLsaHandle;
begin
  // Try to open account if it already exists
  Result := LsaxOpenAccountEx(hAccount, AccountSid, DesiredAccess);

  if Result.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsaOpenAccount') then
  begin
    // Create it (requires different access to the policy object)
    Result := LsaxOpenPolicy(hPolicy, POLICY_CREATE_ACCOUNT);

    if Result.IsSuccess then
    begin
      Result := LsaxCreateAccount(hAccount, hPolicy, AccountSid, DesiredAccess);
      LsaxClose(hPolicy);
    end;
  end;
end;

function LsaxDeleteAccount(hAccount: TLsaHandle): TNtxStatus;
begin
  Result.Location := 'LsaDelete';
  Result.LastCall.Expects(_DELETE, objLsaAccount);
  Result.Status := LsaDelete(hAccount);
end;

function LsaxEnumerateAccounts(hPolicy: TLsaHandle; out Accounts: TArray<ISid>):
  TNtxStatus;
var
  EnumContext: TLsaEnumerationHandle;
  Buffer: PSidArray;
  Count, i: Integer;
begin
  EnumContext := 0;
  Result.Location := 'LsaEnumerateAccounts';
  Result.LastCall.Expects(POLICY_VIEW_LOCAL_INFORMATION, objLsaPolicy);

  Result.Status := LsaEnumerateAccounts(hPolicy, EnumContext, Buffer,
    MAX_PREFERRED_LENGTH, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Accounts, Count);

  for i := 0 to High(Accounts) do
    Accounts[i] := TSid.CreateCopy(Buffer{$R-}[i]{$R+});

  LsaFreeMemory(Buffer);
end;

function LsaxEnumeratePrivilegesAccount(hAccount: TLsaHandle;
  out Privileges: TArray<TPrivilege>): TNtxStatus;
var
  PrivilegeSet: PPrivilegeSet;
  i: Integer;
begin
  Result.Location := 'LsaEnumeratePrivilegesOfAccount';
  Result.LastCall.Expects(ACCOUNT_VIEW, objLsaAccount);

  Result.Status := LsaEnumeratePrivilegesOfAccount(hAccount, PrivilegeSet);

  if not Result.IsSuccess then
    Exit;

  SetLength(Privileges, PrivilegeSet.PrivilegeCount);

  for i := 0 to High(Privileges) do
    Privileges[i] := PrivilegeSet.Privilege{$R-}[i]{$R+};

  LsaFreeMemory(PrivilegeSet);
end;

function LsaxEnumeratePrivilegesAccountBySid(AccountSid: PSid;
  out Privileges: TArray<TPrivilege>): TNtxStatus;
var
  hAccount: TLsaHandle;
begin
  Result := LsaxOpenAccountEx(hAccount, AccountSid, ACCOUNT_VIEW);

  if Result.IsSuccess then
  begin
    Result := LsaxEnumeratePrivilegesAccount(hAccount, Privileges);
    LsaxClose(hAccount);
  end;
end;

function LsaxAddPrivilegesAccount(hAccount: TLsaHandle;
  Privileges: TArray<TPrivilege>): TNtxStatus;
var
  PrivSet: PPrivilegeSet;
begin
  PrivSet := NtxpAllocPrivilegeSet(Privileges);

  Result.Location := 'LsaAddPrivilegesToAccount';
  Result.LastCall.Expects(ACCOUNT_ADJUST_PRIVILEGES, objLsaAccount);

  Result.Status := LsaAddPrivilegesToAccount(hAccount, PrivSet);
  FreeMem(PrivSet);
end;

function LsaxRemovePrivilegesAccount(hAccount: TLsaHandle; RemoveAll: Boolean;
  Privileges: TArray<TPrivilege>): TNtxStatus;
var
  PrivSet: PPrivilegeSet;
begin
  PrivSet := NtxpAllocPrivilegeSet(Privileges);

  Result.Location := 'LsaRemovePrivilegesFromAccount';
  Result.LastCall.Expects(ACCOUNT_ADJUST_PRIVILEGES, objLsaAccount);

  Result.Status := LsaRemovePrivilegesFromAccount(hAccount, RemoveAll, PrivSet);
  FreeMem(PrivSet);
end;

function LsaxManagePrivilegesAccount(AccountSid: PSid; RemoveAll: Boolean;
  Add, Remove: TArray<TPrivilege>): TNtxStatus;
var
  hAccount: TLsaHandle;
begin
  if (Length(Add) = 0) and (Length(Remove) = 0) and not RemoveAll then
  begin
    Result.Status := STATUS_SUCCESS;
    Exit;
  end;

  // Open account when only removing, create account when adding
  if Length(Add) = 0 then
    Result := LsaxOpenAccountEx(hAccount, AccountSid,
      ACCOUNT_ADJUST_PRIVILEGES)
  else
    Result := LsaxCreateAccountEx(hAccount, AccountSid,
      ACCOUNT_ADJUST_PRIVILEGES);

  if not Result.IsSuccess then
    Exit;

  // Add privileges
  if Length(Add) > 0 then
    Result := LsaxAddPrivilegesAccount(hAccount, Add);

  // Remove privileges
  if Result.IsSuccess and (RemoveAll or (Length(Remove) > 0)) then
    Result := LsaxRemovePrivilegesAccount(hAccount, RemoveAll, Remove);

  LsaxClose(hAccount);
end;

function LsaxQueryRightsAccount(hAccount: TLsaHandle;
  out SystemAccess: Cardinal): TNtxStatus;
begin
  Result.Location := 'LsaGetSystemAccessAccount';
  Result.LastCall.Expects(ACCOUNT_VIEW, objLsaAccount);

  Result.Status := LsaGetSystemAccessAccount(hAccount, SystemAccess);
end;

function LsaxQueryRightsAccountBySid(AccountSid: PSid;
  out SystemAccess: Cardinal): TNtxStatus;
var
  hAccount: TLsaHandle;
begin
  Result := LsaxOpenAccountEx(hAccount, AccountSid, ACCOUNT_VIEW);

  if Result.IsSuccess then
  begin
    Result := LsaxQueryRightsAccount(hAccount, SystemAccess);
    LsaxClose(hAccount);
  end;
end;

function LsaxSetRightsAccount(hAccount: TLsaHandle; SystemAccess: Cardinal)
  : TNtxStatus;
begin
  Result.Location := 'LsaSetSystemAccessAccount';
  Result.LastCall.Expects(ACCOUNT_ADJUST_SYSTEM_ACCESS, objLsaAccount);

  Result.Status := LsaSetSystemAccessAccount(hAccount, SystemAccess);
end;

function LsaxSetRightsAccountBySid(AccountSid: PSid; SystemAccess: Cardinal):
  TNtxStatus;
var
  hAccount: TLsaHandle;
begin
  Result := LsaxCreateAccountEx(hAccount, AccountSid,
    ACCOUNT_ADJUST_SYSTEM_ACCESS);

  if Result.IsSuccess then
  begin
    Result := LsaxSetRightsAccount(hAccount, SystemAccess);
    LsaxClose(hAccount);
  end;
end;

{ Privileges }

function LsaxEnumeratePrivileges(hPolicy: TLsaHandle;
  out Privileges: TArray<TPrivilegeDefinition>): TNtxStatus;
var
  EnumContext: TLsaEnumerationHandle;
  Count, i: Integer;
  Buffer: PPolicyPrivilegeDefinitionArray;
begin
  EnumContext := 0;
  Result.Location := 'LsaEnumeratePrivileges';
  Result.LastCall.Expects(POLICY_VIEW_LOCAL_INFORMATION, objLsaPolicy);

  Result.Status := LsaEnumeratePrivileges(hPolicy, EnumContext, Buffer,
    MAX_PREFERRED_LENGTH, Count);

  if not Result.IsSuccess then
    Exit;

  SetLength(Privileges, Count);

  for i := 0 to High(Privileges) do
  begin
    Privileges[i].Name := Buffer{$R-}[i]{$R+}.Name.ToString;
    Privileges[i].LocalValue := Buffer{$R-}[i]{$R+}.LocalValue;
  end;

  LsaFreeMemory(Buffer);
end;

function LsaxEnumeratePrivilegesLocal(
  out Privileges: TArray<TPrivilegeDefinition>): TNtxStatus;
var
  hPolicy: TLsaHandle;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_VIEW_LOCAL_INFORMATION);

  if Result.IsSuccess then
  begin
    Result := LsaxEnumeratePrivileges(hPolicy, Privileges);
    LsaxClose(hPolicy);
  end;
end;

function LsaxQueryNamePrivilege(hPolicy: TLsaHandle; Luid: TLuid;
  out Name: String): TNtxStatus;
var
  Buffer: PLsaUnicodeString;
begin
  Result.Location := 'LsaLookupPrivilegeName';
  Result.LastCall.Expects(POLICY_LOOKUP_NAMES, objLsaPolicy);

  Result.Status := LsaLookupPrivilegeName(hPolicy, Luid, Buffer);

  if Result.IsSuccess then
  begin
    Name := Buffer.ToString;
    LsaFreeMemory(Buffer);
  end;
end;

function LsaxQueryDescriptionPrivilege(hPolicy: TLsaHandle; const Name: String;
  out DisplayName: String): TNtxStatus;
var
  NameStr: TLsaUnicodeString;
  BufferDisplayName: PLsaUnicodeString;
  LangId: SmallInt;
begin
  NameStr.FromString(Name);

  Result.Location := 'LsaLookupPrivilegeDisplayName';
  Result.LastCall.Expects(POLICY_LOOKUP_NAMES, objLsaPolicy);

  Result.Status := LsaLookupPrivilegeDisplayName(hPolicy, NameStr,
    BufferDisplayName, LangId);

  if Result.IsSuccess then
  begin
    DisplayName := BufferDisplayName.ToString;
    LsaFreeMemory(BufferDisplayName);
  end;
end;

function LsaxLookupMultiplePrivileges(Luids: TArray<TLuid>;
  out Names, Descriptions: TArray<String>): TNtxStatus;
var
  hPolicy: TLsaHandle;
  i: Integer;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_LOOKUP_NAMES);

  if not Result.IsSuccess then
    Exit;

  SetLength(Names, Length(Luids));
  SetLength(Descriptions, Length(Luids));

  for i := 0 to High(Luids) do
    if not LsaxQueryNamePrivilege(hPolicy, Luids[i], Names[i]).IsSuccess or
      not LsaxQueryDescriptionPrivilege(hPolicy, Names[i],
        Descriptions[i]).IsSuccess then
    begin
      Result.Location := 'LsaxQueryNamesPrivileges';
      Result.Status := STATUS_SOME_NOT_MAPPED;
    end;
end;

function LsaxQueryIntegrityPrivilege(Luid: TLuid): Cardinal;
begin
  // Some privileges require a specific integrity level to be enabled.
  // The ones that require more than Medium also trigger UAC to split logon
  // sessions. The following data is gathered by experimenting and should be
  // maintained in sync with Windows behavior when new privileges are
  // introduced.

  case TSeWellKnownPrivilege(Luid) of
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
    SE_INCREASE_WORKING_SET_PRIVILEGE:
      Result := SECURITY_MANDATORY_UNTRUSTED_RID;

  else
    // All other require Medium
    Result := SECURITY_MANDATORY_MEDIUM_RID;
  end;
end;

{ Logon rights }

function LsaxEnumerateLogonRights: TArray<TLogonRightRec>;
begin
  // If someone knows a system function to enumerate logon rights on the system
  // you are welcome to use it here.

  SetLength(Result, 10);

  Result[0].Value := SECURITY_ACCESS_INTERACTIVE_LOGON;
  Result[0].IsAllowedType := True;
  Result[0].Name := SE_INTERACTIVE_LOGON_NAME;
  Result[0].Description := 'Allow interactive logon';

  Result[1].Value := SECURITY_ACCESS_NETWORK_LOGON;
  Result[1].IsAllowedType := True;
  Result[1].Name := SE_NETWORK_LOGON_NAME;
  Result[1].Description := 'Allow network logon';

  Result[2].Value := SECURITY_ACCESS_BATCH_LOGON;
  Result[2].IsAllowedType := True;
  Result[2].Name := SE_BATCH_LOGON_NAME;
  Result[2].Description := 'Allow batch job logon';

  Result[3].Value := SECURITY_ACCESS_SERVICE_LOGON;
  Result[3].IsAllowedType := True;
  Result[3].Name := SE_SERVICE_LOGON_NAME;
  Result[3].Description := 'Allow service logon';

  Result[4].Value := SECURITY_ACCESS_REMOTE_INTERACTIVE_LOGON;
  Result[4].IsAllowedType := True;
  Result[4].Name := SE_REMOTE_INTERACTIVE_LOGON_NAME;
  Result[4].Description := 'Allow Remote Desktop Services logon';

  Result[5].Value := SECURITY_ACCESS_DENY_INTERACTIVE_LOGON;
  Result[5].IsAllowedType := False;
  Result[5].Name := SE_DENY_INTERACTIVE_LOGON_NAME;
  Result[5].Description := 'Deny interactive logon';

  Result[6].Value := SECURITY_ACCESS_DENY_NETWORK_LOGON;
  Result[6].IsAllowedType := False;
  Result[6].Name := SE_DENY_NETWORK_LOGON_NAME;
  Result[6].Description := 'Deny network logon';

  Result[7].Value := SECURITY_ACCESS_DENY_BATCH_LOGON;
  Result[7].IsAllowedType := False;
  Result[7].Name := SE_DENY_BATCH_LOGON_NAME;
  Result[7].Description := 'Deny batch job logon';

  Result[8].Value := SECURITY_ACCESS_DENY_SERVICE_LOGON;
  Result[8].IsAllowedType := False;
  Result[8].Name := SE_DENY_SERVICE_LOGON_NAME;
  Result[8].Description := 'Deny service logon';

  Result[9].Value := SECURITY_ACCESS_DENY_REMOTE_INTERACTIVE_LOGON;
  Result[9].IsAllowedType := False;
  Result[9].Name := SE_DENY_REMOTE_INTERACTIVE_LOGON_NAME;
  Result[9].Description := 'Deny Remote Desktop Services logon';
end;

{ SID translation}

function LsaxLookupSid(Sid: PSid; var Name: TTranslatedName): TNtxStatus;
var
  Sids: TArray<PSid>;
  Names: TArray<TTranslatedName>;
begin
  SetLength(Sids, 1);
  Sids[0] := Sid;

  Result := LsaxLookupSids(Sids, Names);

  if Result.IsSuccess then
    Name := Names[0];
end;

function LsaxLookupSids(Sids: TArray<PSid>; out Names: TArray<TTranslatedName>):
  TNtxStatus;
var
  hPolicy: TLsaHandle;
  BufferDomains: PLsaReferencedDomainList;
  BufferNames: PLsaTranslatedNameArray;
  i: Integer;
begin
  Result := LsaxOpenPolicy(hPolicy, POLICY_LOOKUP_NAMES);

  if not Result.IsSuccess then
    Exit;

  // Request translation for all SIDs at once
  Result.Location := 'LsaLookupSids';
  Result.Status := LsaLookupSids(hPolicy, Length(Sids), Sids, BufferDomains,
    BufferNames);

  LsaxClose(hPolicy);

  // Even without mapping we get to know SID types
  if Result.Status = STATUS_NONE_MAPPED then
    Result.Status := STATUS_SOME_NOT_MAPPED;

  if not Result.IsSuccess then
    Exit;

  SetLength(Names, Length(SIDs));

  for i := 0 to High(Sids) do
  begin
    Names[i].SidType := BufferNames{$R-}[i]{$R+}.Use;

    // Note: for some SID types LsaLookupSids might return SID's SDDL
    // representation in the Name field. In rare cases it might be empty.

    Names[i].UserName := BufferNames{$R-}[i]{$R+}.Name.ToString;

    if Names[i].SidType in [SidTypeInvalid, SidTypeUnknown] then
      RtlxpApplySddlOverrides(Sids[i], Names[i].UserName);

    // Negative DomainIndex means the SID does not reference a domain
    if (BufferNames{$R-}[i]{$R+}.DomainIndex >= 0) and
      (BufferNames{$R-}[i]{$R+}.DomainIndex < BufferDomains.Entries) then
      Names[i].DomainName := BufferDomains.Domains[
        BufferNames{$R-}[i]{$R+}.DomainIndex].Name.ToString
    else
      Names[i].DomainName := '';
  end;

  LsaFreeMemory(BufferDomains);
  LsaFreeMemory(BufferNames);
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

function LsaxGetUserName(out FullName: String): TNtxStatus;
var
  Domain, UserName: String;
begin
  Result := LsaxGetUserName(Domain, UserName);

  if not Result.IsSuccess then
    Exit;

  if (Domain <> '') and (UserName <> '') then
    FullName := Domain + '\' + UserName
  else if Domain <> '' then
    FullName := Domain
  else if UserName <> '' then
    FullName := UserName
  else
  begin
    Result.Location := 'LsaxGetUserName';
    Result.Status := STATUS_UNSUCCESSFUL;
  end;
end;

end.
