unit NtUtils.Tokens.Logon;

interface

uses
  Winapi.WinNt, Winapi.WinBase, Winapi.NtSecApi, NtUtils.Exceptions,
  NtUtils.Security.Sid;

// Logon a user
function NtxLogonUser(out hToken: THandle; Domain, Username: String;
  Password: PWideChar; LogonType: TSecurityLogonType;
  LogonProvider: TLogonProvider; AdditionalGroups: TGroupArray): TNtxStatus;

// Logon a user without a password using S4U logon
function NtxLogonS4U(out hToken: THandle; Domain, Username: String;
  LogonType: TSecurityLogonType; const TokenSource: TTokenSource;
  AdditionalGroups: TGroupArray): TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntstatus, NtUtils.Processes, NtUtils.Tokens.Misc;

function NtxLogonUser(out hToken: THandle; Domain, Username: String;
  Password: PWideChar; LogonType: TSecurityLogonType;
  LogonProvider: TLogonProvider; AdditionalGroups: TGroupArray): TNtxStatus;
var
  GroupsBuffer: PTokenGroups;
  i: Integer;
begin
  if Length(AdditionalGroups) = 0 then
  begin
    // Use regular LogonUserW if the caller had not specified additional groups
    Result.Location := 'LogonUserW';
    Result.Win32Result := LogonUserW(PWideChar(Username), PWideChar(Domain),
      Password, LogonType, LogonProvider, hToken);
  end
  else
  begin
    // Prepare PTokenGroups
    GroupsBuffer := AllocMem(SizeOf(Integer) +
      Length(AdditionalGroups) * SizeOf(TSIDAndAttributes));

    GroupsBuffer.GroupCount := Length(AdditionalGroups);
    for i := 0 to High(AdditionalGroups) do
    begin
      GroupsBuffer.Groups[i].Sid := AdditionalGroups[i].SecurityIdentifier.Sid;
      GroupsBuffer.Groups[i].Attributes := AdditionalGroups[i].Attributes;
    end;

    // Call LogonUserExExW that allows us to add arbitrary groups to a token.
    // Note: this action requires SeTcbPrivilege.
    Result.Location := 'LogonUserExExW';
    Result.Win32Result := LogonUserExExW(PWideChar(Username), PWideChar(Domain),
      Password, LogonType, LogonProvider, GroupsBuffer, hToken, nil, nil, nil,
      nil);

    FreeMem(GroupsBuffer);
  end;
end;

function NtxLogonS4U(out hToken: THandle; Domain, Username: String;
  LogonType: TSecurityLogonType; const TokenSource: TTokenSource;
  AdditionalGroups: TGroupArray): TNtxStatus;
var
  SubStatus: NTSTATUS;
  LsaHandle: TLsaHandle;
  PkgName: ANSI_STRING;
  AuthPkg: Cardinal;
  Buffer: PKERB_S4U_LOGON;
  BufferSize: Cardinal;
  OriginName: ANSI_STRING;
  GroupArray: PTokenGroups;
  ProfileBuffer: Pointer;
  ProfileSize: Cardinal;
  LogonId: TLuid;
  Quotas: TQuotaLimits;
begin
  // TODO -c WoW64: LsaLogonUser overwrites our memory for some reason
  Result := NtxAssertNotWoW64;

  if not Result.IsSuccess then
    Exit;

  // Connect to LSA
  Result.Location := 'LsaConnectUntrusted';
  Result.Status := LsaConnectUntrusted(LsaHandle);

  // Lookup for Negotiate package
  PkgName.FromString(NEGOSSP_NAME_A);
  Result.Location := 'LsaLookupAuthenticationPackage';
  Result.Status := LsaLookupAuthenticationPackage(LsaHandle, PkgName, AuthPkg);

  if not Result.IsSuccess then
  begin
    LsaDeregisterLogonProcess(LsaHandle);
    Exit;
  end;

  // We need to prepare a blob where KERB_S4U_LOGON is followed by the username
  // and the domain.
  BufferSize := SizeOf(KERB_S4U_LOGON) + Length(Username) * SizeOf(WideChar) +
    Length(Domain) * SizeOf(WideChar);
  Buffer := AllocMem(BufferSize);

  Buffer.MessageType := KerbS4ULogon;

  Buffer.ClientUpn.Length := Length(Username) * SizeOf(WideChar);
  Buffer.ClientUpn.MaximumLength := Buffer.ClientUpn.Length;

  // Place the username just after the structure
  Buffer.ClientUpn.Buffer := Pointer(NativeUInt(Buffer) +
    SizeOf(KERB_S4U_LOGON));
  Move(PWideChar(Username)^, Buffer.ClientUpn.Buffer^, Buffer.ClientUpn.Length);

  Buffer.ClientRealm.Length := Length(Domain) * SizeOf(WideChar);
  Buffer.ClientRealm.MaximumLength := Buffer.ClientRealm.Length;

  // Place the domain after the username
  Buffer.ClientRealm.Buffer := Pointer(NativeUInt(Buffer) +
    SizeOf(KERB_S4U_LOGON) + Buffer.ClientUpn.Length);
  Move(PWideChar(Domain)^, Buffer.ClientRealm.Buffer^,
    Buffer.ClientRealm.Length);

  OriginName.FromString('S4U');

  // Allocate PTokenGroups if necessary
  if Length(AdditionalGroups) > 0 then
    GroupArray := NtxpAllocGroups2(AdditionalGroups)
  else
    GroupArray := nil;

  // Perform the logon
  SubStatus := STATUS_SUCCESS;
  Result.Location := 'LsaLogonUser';
  Result.Status := LsaLogonUser(LsaHandle, OriginName, LogonType, AuthPkg,
    Buffer, BufferSize, GroupArray, TokenSource, ProfileBuffer, ProfileSize,
    LogonId, hToken, Quotas, SubStatus);

  // Prefer a more detailed status
  if not NT_SUCCESS(SubStatus) then
    Result.Status := SubStatus;
    
  // Clean up
  LsaFreeReturnBuffer(ProfileBuffer);
  LsaDeregisterLogonProcess(LsaHandle);  

  if Assigned(GroupArray) then
    FreeMem(GroupArray);

  FreeMem(Buffer);  
end;

end.
