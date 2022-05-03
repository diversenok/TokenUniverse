unit TU.AccountRights;

{
  This module retrieves detailed textual desciptions of logon rights and
  privileges.
}

interface

uses
  Ntapi.ntseapi, Ntapi.ntlsa, NtUtils;

// Lookup a privilege description from the group policy editor
function GetPrivilegeDescription(
  Privilege: TSeWellKnownPrivilege;
  out Description: String
): TNtxStatus;

// Lookup a logon right description from the group policy editor
function GetLogonRightDescription(
  LogonRight: TSystemAccessIndex;
  out Description: String
): TNtxStatus;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntldr, NtUtils.Ldr, NtUtils.Sections,
  NtUtils.Files.Open;

const
  wsecedit = '\SystemRoot\System32\wsecedit.dll';

  // These values are portable across OS versions
  PrivilegeMessageIDs: array [TSeWellKnownPrivilege] of Cardinal = (
    0,    // Reserved 0
    0,    // Reserved 1
    1933, // SeCreateTokenPrivilege
    1957, // SeAssignPrimaryTokenPrivilege
    1948, // SeLockMemoryPrivilege
    1926, // SeIncreaseQuotaPrivilege
    1925, // SeMachineAccountPrivilege
    1924, // SeTcbPrivilege
    1951, // SeSecurityPrivilege
    1961, // SeTakeOwnershipPrivilege
    1947, // SeLoadDriverPrivilege
    1955, // SeSystemProfilePrivilege
    1931, // SeSystemtimePrivilege
    1954, // SeProfileSingleProcessPrivilege
    1946, // SeIncreaseBasePriorityPrivilege
    1932, // SeCreatePagefilePrivilege
    1935, // SeCreatePermanentPrivilege
    1929, // SeBackupPrivilege
    1958, // SeRestorePrivilege
    1959, // SeShutdownPrivilege
    1936, // SeDebugPrivilege
    1944, // SeAuditPrivilege
    1952, // SeSystemEnvironmentPrivilege
    1930, // SeChangeNotifyPrivilege
    1943, // SeRemoteShutdownPrivilege
    1956, // SeUndockPrivilege
    1960, // SeSyncAgentPrivilege
    1942, // SeEnableDelegationPrivilege
    1953, // SeManageVolumePrivilege
    1945, // SeImpersonatePrivilege
    1934, // SeCreateGlobalPrivilege
    2060, // SeTrustedCredManAccessPrivilege
    2058, // SeRelabelPrivilege
    2062, // SeIncreaseWorkingSetPrivilege
    2061, // SeTimeZonePrivilege
    2057, // SeCreateSymbolicLinkPrivilege
    2080  // SeDelegateSessionUserImpersonatePrivilege
  );

  // These values are portable across OS versions
  LogonRightMessageIDs: array [TSystemAccessIndex] of Cardinal = (
    1927, // SeInteractiveLogonRight
    1923, // SeNetworkLogonRight
    1949, // SeBatchLogonRight
    0,    // Reserved 3
    1950, // SeServiceLogonRight
    0,    // Reserved 5
    1940, // SeDenyInteractiveLogonRight
    1937, // SeDenyNetworkLogonRight
    1938, // SeDenyBatchLogonRight
    1939, // SeDenyServiceLogonRight
    1928, // SeRemoteInteractiveLogonRight
    1941  // SeDenyRemoteInteractiveLogonRight
  );

var
  DescriptionsInitialized: Boolean;
  PrivilegeDescriptions: array [TSeWellKnownPrivilege] of String;
  LogonRightDescriptions: array [TSystemAccessIndex] of String;

function LoadDescriptions: TNtxStatus;
var
  ResourceDll: IMemory;
  Privilege: TSeWellKnownPrivilege;
  Right: TSystemAccessIndex;
begin
  // Map wsecedit.dll for extracting embedded strings
  Result := RtlxMapReadonlyFile(ResourceDll, FileOpenParameters
    .UseFileName(wsecedit));

  if not Result.IsSuccess then
    Exit;

  // Cache all privileges
  for Privilege := Low(TSeWellKnownPrivilege) to High(TSeWellKnownPrivilege) do
    if PrivilegeMessageIDs[Privilege] <> 0 then
      RtlxLoadString(PrivilegeDescriptions[Privilege], PByte(ResourceDll.Data) +
        LDR_MAPPED_AS_DATAFILE, PrivilegeMessageIDs[Privilege]);

  // Cache all logon rights
  for Right := Low(TSystemAccessIndex) to High(TSystemAccessIndex) do
    if LogonRightMessageIDs[Right] <> 0 then
      RtlxLoadString(LogonRightDescriptions[Right], PByte(ResourceDll.Data) +
        LDR_MAPPED_AS_DATAFILE, LogonRightMessageIDs[Right]);

  DescriptionsInitialized := True;
end;

function GetPrivilegeDescription;
begin
  // Make sure the cache is populated
  if not DescriptionsInitialized then
  begin
    Result := LoadDescriptions;

    if not Result.IsSuccess then
      Exit;
  end;

  // Verify the value
  if (Privilege < Low(TSeWellKnownPrivilege)) or
    (Privilege > High(TSeWellKnownPrivilege)) then
  begin
    Result.Location := 'GetPrivilegeDescription';
    Result.Status := STATUS_NO_SUCH_PRIVILEGE;
    Exit;
  end;

  Description := PrivilegeDescriptions[Privilege];

  if Description = '' then
  begin
    Result.Location := 'GetPrivilegeDescription';
    Result.Status := STATUS_NOT_FOUND;
  end
  else
    Result.Status := STATUS_SUCCESS;
end;

function GetLogonRightDescription;
begin
  // Make sure the cache is populated
  if not DescriptionsInitialized then
  begin
    Result := LoadDescriptions;

    if not Result.IsSuccess then
      Exit;
  end;

  // Verify the value
  if not (LogonRight in VALID_SYSTEM_ACCESS) then
  begin
    Result.Location := 'GetLogonRightDescription';
    Result.Status := STATUS_UNKNOWN_REVISION;
    Exit;
  end;

  Description := LogonRightDescriptions[LogonRight];

  if Description = '' then
  begin
    Result.Location := 'GetLogonRightDescription';
    Result.Status := STATUS_NOT_FOUND;
  end
  else
    Result.Status := STATUS_SUCCESS;
end;

end.
