unit TU.LsaApi;

{$MINENUMSIZE 4}

interface

uses
  NtUtils.Security.Sid, Winapi.WinNt, Ntapi.ntseapi, Winapi.NtSecApi,
  NtUtils.Exceptions;

type
  TLogonDataClass = (lsLogonId, lsSecurityIdentifier, lsUserName, lsLogonDomain,
    lsAuthPackage, lsLogonType, lsSession, lsLogonTime, lsLogonServer,
    lsDnsDomainName, lsUpn, lsUserFlags, lsLastSuccessfulLogon,
    lsLastFailedLogon, lsFailedAttemptSinceSuccess, lsLogonScript,
    lsProfilePath, lsHomeDirectory, lsHomeDirectoryDrive, lsLogoffTime,
    lsKickOffTime, lsPasswordLastSet, lsPasswordCanChange, lsPasswordMustChange
  );

  ILogonSession = interface
    function RawData: PSecurityLogonSessionData;
    function UserPresent: Boolean;
    function User: ISid;
    function GetString(InfoClass: TLogonDataClass): String;
    function UserFlagsHint: String;
  end;

  TLogonSession = class(TInterfacedObject, ILogonSession)
  private
    Data: PSecurityLogonSessionData;
  public
    class function Query(LogonId: TLuid;
      out Self: ILogonSession): TNtxStatus; static;
    destructor Destroy; override;

    function RawData: PSecurityLogonSessionData;
    function UserPresent: Boolean;
    function User: ISid;
    function GetString(InfoClass: TLogonDataClass): String;
    function UserFlagsHint: String;
  end;

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
  NtUtils.Lsa, System.SysUtils, DelphiUtils.Strings, NtUtils.Processes,
  TU.Tokens.Types, NtUtils.Strings;

{ TLogonSessionInfo }

destructor TLogonSession.Destroy;
begin
  LsaFreeReturnBuffer(Data);
  inherited;
end;

function TLogonSession.GetString(InfoClass: TLogonDataClass): String;
begin
  case InfoClass of
    lsLogonId:
      Result := IntToHexEx(Data.LogonId);

    lsSecurityIdentifier:
      if UserPresent then
        Result := User.AsString
      else
        Result := 'No User';

    lsUserName:
      Result := Data.UserName.ToString;

    lsLogonDomain:
      Result := Data.LogonDomain.ToString;

    lsAuthPackage:
      Result := Data.AuthenticationPackage.ToString;

    lsLogonType:
      Result := EnumLogonTypeToString(Data.LogonType);

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
      Result := MapKnownFlags(Data.UserFlags, bmLogonFlags);

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

class function TLogonSession.Query(LogonId: TLuid;
  out Self: ILogonSession): TNtxStatus;
var
  Buffer: PSecurityLogonSessionData;
  Obj: TLogonSession;
begin
  // TODO -c WoW64: LsaGetLogonSessionData returns a weird pointer
  Result := NtxAssertNotWoW64;

  if not Result.IsSuccess then
    Exit;

  Result.Location := 'LsaGetLogonSessionData';
  Result.Status := LsaGetLogonSessionData(LogonId, Buffer);

  if Result.IsSuccess then
  begin
    Obj := TLogonSession.Create;
    Obj.Data := Buffer;
    Self := Obj;
  end;
end;

function TLogonSession.RawData: PSecurityLogonSessionData;
begin
  Result := Data;
end;

function TLogonSession.User: ISid;
begin
  if UserPresent then
    Result := TSid.CreateCopy(Data.Sid)
  else
    Result := nil;
end;

function TLogonSession.UserFlagsHint: String;
begin
  Result := MapKnownFlagsHint(Data.UserFlags, bmLogonFlags);
end;

function TLogonSession.UserPresent: Boolean;
begin
  Result := Assigned(Data.Sid)
end;


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
