unit NtUtils.Lsa.Logon;

interface

uses
  Winapi.WinNt, Winapi.ntsecapi, NtUtils.Exceptions, NtUtils.Security.Sid,
  DelphiUtils.Strings;

const
  LogonFlags: array [0..18] of TFlagName = (
    (Value: LOGON_GUEST; Name: 'Guest'),
    (Value: LOGON_NOENCRYPTION; Name: 'No Encryption'),
    (Value: LOGON_CACHED_ACCOUNT; Name: 'Cached Account'),
    (Value: LOGON_USED_LM_PASSWORD; Name: 'Used LM Password'),
    (Value: LOGON_EXTRA_SIDS; Name: 'Extra SIDs'),
    (Value: LOGON_SUBAUTH_SESSION_KEY; Name: 'Subauth Session Key'),
    (Value: LOGON_SERVER_TRUST_ACCOUNT; Name: 'Server Trust Account'),
    (Value: LOGON_NTLMV2_ENABLED; Name: 'NTLMv2 Enabled'),
    (Value: LOGON_RESOURCE_GROUPS; Name: 'Resource Groups'),
    (Value: LOGON_PROFILE_PATH_RETURNED; Name: 'Profile Path Returned'),
    (Value: LOGON_NT_V2; Name: 'NTv2'),
    (Value: LOGON_LM_V2; Name: 'LMv2'),
    (Value: LOGON_NTLM_V2; Name: 'NTLMv2'),
    (Value: LOGON_OPTIMIZED; Name: 'Optimized'),
    (Value: LOGON_WINLOGON; Name: 'Winlogon'),
    (Value: LOGON_PKINIT; Name: 'PKINIT'),
    (Value: LOGON_NO_OPTIMIZED; Name: 'Not Optimized'),
    (Value: LOGON_NO_ELEVATION; Name: 'No Elevation'),
    (Value: LOGON_MANAGED_SERVICE; Name: 'Managed Service')
  );

type
  TLogonDataClass = (lsLogonId, lsSecurityIdentifier, lsUserName, lsLogonDomain,
    lsAuthPackage, lsLogonType, lsSession, lsLogonTime, lsLogonServer,
    lsDnsDomainName, lsUpn, lsUserFlags, lsLastSuccessfulLogon,
    lsLastFailedLogon, lsFailedAttemptSinceSuccess, lsLogonScript,
    lsProfilePath, lsHomeDirectory, lsHomeDirectoryDrive, lsLogoffTime,
    lsKickOffTime, lsPasswordLastSet, lsPasswordCanChange, lsPasswordMustChange
  );

  ILogonSession = interface
    function LogonId: TLuid;
    function RawData: PSecurityLogonSessionData;
    function User: ISid;
    function QueryString(InfoClass: TLogonDataClass): String;
  end;

// Enumerate logon sessions
function LsaxEnumerateLogonSessions(out Luids: TArray<TLuid>): TNtxStatus;

// Query logon session information; always returns LogonSession parameter
function LsaxQueryLogonSession(LogonId: TLuid; out LogonSession: ILogonSession):
  TNtxStatus;

implementation

uses
  NtUtils.Processes, NtUtils.Strings, System.SysUtils;

type
  TLogonSession = class(TInterfacedObject, ILogonSession)
  private
    FLuid: TLuid;
    FSid: ISid;
    Data: PSecurityLogonSessionData;
  public
    constructor Create(Id: TLuid; Buffer: PSecurityLogonSessionData);
    function LogonId: TLuid;
    function RawData: PSecurityLogonSessionData;
    function User: ISid;
    function QueryString(InfoClass: TLogonDataClass): String;
    destructor Destroy; override;
  end;

{ TLogonSession }

constructor TLogonSession.Create(Id: TLuid; Buffer: PSecurityLogonSessionData);
begin
  FLuid := Id;
  Data := Buffer;

  // Construct well known SIDs
  if not Assigned(Data) then
    case FLuid of
      SYSTEM_LUID:
        FSid := TSid.CreateNew(SECURITY_NT_AUTHORITY, 1,
          SECURITY_LOCAL_SYSTEM_RID);

      ANONYMOUS_LOGON_LUID:
        FSid := TSid.CreateNew(SECURITY_NT_AUTHORITY, 1,
          SECURITY_ANONYMOUS_LOGON_RID);

      LOCALSERVICE_LUID:
        FSid := TSid.CreateNew(SECURITY_NT_AUTHORITY, 1,
          SECURITY_LOCAL_SERVICE_RID);

      NETWORKSERVICE_LUID:
        FSid := TSid.CreateNew(SECURITY_NT_AUTHORITY, 1,
          SECURITY_NETWORK_SERVICE_RID);
    end
  else if Assigned(Data.Sid) then
    FSid := TSid.CreateCopy(Data.Sid);
end;

destructor TLogonSession.Destroy;
begin
  if Assigned(Data) then
    LsaFreeReturnBuffer(Data);
  inherited;
end;

function TLogonSession.LogonId: TLuid;
begin
  Result := FLuid;
end;

function TLogonSession.QueryString(InfoClass: TLogonDataClass): String;
begin
  Result := 'Unknown';

  // Only a few data classes are available when the query failed
  case InfoClass of
    lsLogonId:
      Result := IntToHexEx(FLuid);

    lsSecurityIdentifier:
      if Assigned(FSid) then
        Result := FSid.AsString
      else if Assigned(Data) then
        Result := 'No User';
  end;

  if not Assigned(Data) then
    Exit;

  case InfoClass of
    lsUserName:
      Result := Data.UserName.ToString;

    lsLogonDomain:
      Result := Data.LogonDomain.ToString;

    lsAuthPackage:
      Result := Data.AuthenticationPackage.ToString;

    lsLogonType:
      Result := PrettifyCamelCaseEnum('LogonType', TypeInfo(TSecurityLogonType),
        Integer(Data.LogonType));

    lsSession:
      Result := Data.Session.ToString;

    lsLogonTime:
      Result := NativeTimeToString(Data.LogonTime);

    lsLogonServer:
      Result := Data.LogonServer.ToString;

    lsDnsDomainName:
      Result := Data.DnsDomainName.ToString;

    lsUpn:
      Result := Data.Upn.ToString;

    lsUserFlags:
      Result := MapFlags(Data.UserFlags, LogonFlags);

    lsLastSuccessfulLogon:
      Result := NativeTimeToString(Data.LastLogonInfo.LastSuccessfulLogon);

    lsLastFailedLogon:
      Result := NativeTimeToString(Data.LastLogonInfo.LastFailedLogon);

    lsFailedAttemptSinceSuccess:
      Result := Data.LastLogonInfo.FailedAttemptCountSinceLastSuccessfulLogon.
        ToString;

    lsLogonScript:
      Result := Data.LogonScript.ToString;

    lsProfilePath:
      Result := Data.ProfilePath.ToString;

    lsHomeDirectory:
      Result := Data.HomeDirectory.ToString;

    lsHomeDirectoryDrive:
      Result := Data.HomeDirectoryDrive.ToString;

    lsLogoffTime:
      Result := NativeTimeToString(Data.LogoffTime);

    lsKickOffTime:
      Result := NativeTimeToString(Data.KickOffTime);

    lsPasswordLastSet:
      Result := NativeTimeToString(Data.PasswordLastSet);

    lsPasswordCanChange:
      Result := NativeTimeToString(Data.PasswordCanChange);

    lsPasswordMustChange:
      Result := NativeTimeToString(Data.PasswordMustChange);

  end;
end;

function TLogonSession.RawData: PSecurityLogonSessionData;
begin
  Result := Data;
end;

function TLogonSession.User: ISid;
begin
  Result := FSid;
end;

{ Functions }

function LsaxEnumerateLogonSessions(out Luids: TArray<TLuid>): TNtxStatus;
var
  Count, i: Integer;
  Buffer: PLuidArray;
  HasAnonymousLogon: Boolean;
begin
  Result.Location := 'LsaEnumerateLogonSessions';
  Result.Status := LsaEnumerateLogonSessions(Count, Buffer);

  if not Result.IsSuccess then
    Exit;

  SetLength(Luids, Count);

  // Invert the order so that later logons appear later in the list
  for i := 0 to Count - 1 do
    Luids[i] := Buffer[Count - 1 - i];

  LsaFreeReturnBuffer(Buffer);

  // Make sure anonymous logon is in the list (most likely it is not)
  HasAnonymousLogon := False;

  for i := 0 to High(Luids) do
    if Luids[i] = ANONYMOUS_LOGON_LUID then
    begin
      HasAnonymousLogon := True;
      Break;
    end;

  if not HasAnonymousLogon then
    Insert(ANONYMOUS_LOGON_LUID, Luids, 0);
end;

function LsaxQueryLogonSession(LogonId: TLuid; out LogonSession: ILogonSession):
  TNtxStatus;
var
  Buffer: PSecurityLogonSessionData;
begin
  // TODO -c WoW64: LsaGetLogonSessionData returns a weird pointer
  Result := NtxAssertNotWoW64;

  if Result.IsSuccess then
  begin
    Result.Location := 'LsaGetLogonSessionData';
    Result.Status := LsaGetLogonSessionData(LogonId, Buffer);
  end;

  if not Result.IsSuccess then
    Buffer := nil;

  LogonSession := TLogonSession.Create(LogonId, Buffer)
end;

end.
