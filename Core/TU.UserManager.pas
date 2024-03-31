unit TU.UserManager;

{
  This module provides helper functions for exposing User Manager token APIs.
}

interface

uses
  Ntapi.WinNt, Ntapi.ntseapi;

type
  TUmgrContextEntry = record
    Context: TLuid;
    SessionIdValid: Boolean;
    SessionId: TSessionId;
  end;

// Collect known User Manager context values
[RequiredPrivilege(SE_TCB_PRIVILEGE, rpForExtendedFunctionality)]
function TuCollectUmgrContexts: TArray<TUmgrContextEntry>;

implementation

uses
  Ntapi.WinSvc, NtUtils, NtUtils.UserManager, NtUtils.Svc, NtUtils.SysUtils,
  DelphiUtils.Arrays;

function TuCollectUmgrContexts;
var
  Status: TNtxStatus;
  UserContexts: TArray<TSessionUserContext>;
  Services: TArray<TServiceEntry>;
  IDs: TArray<TLuid>;
  i: Integer;
begin
  // Ask User Manager service to return all contexts (TCB only)
  Status := UmgrxEnumerateSessionUsers(UserContexts);

  if Status.IsSuccess then
  begin
    // Convert the structure
    SetLength(Result, Length(UserContexts));

    for i := 0 to High(Result) do
    begin
      Result[i].SessionIdValid := True;
      Result[i].SessionId := UserContexts[i].SessionId;
      Result[i].Context := UserContexts[i].ContextToken;
    end;

    Exit;
  end;

  // As a fallback, enumerate user service instances
  Status := ScmxEnumerateServices(Services, SERVICE_USERSERVICE_INSTANCE);

  if not Status.IsSuccess then
    Exit(nil);

  // Extract context IDs from service names
  IDs := TArray.Convert<TServiceEntry, TLuid>(Services,
    function (const Service: TServiceEntry; out ID: TLuid): Boolean
    begin
      // We need to parse the second portion of "{service-name}_{hex-digits}"
      Result := RtlxStrToUInt64(RtlxExtractNamePath(Service.ServiceName, '_'),
        UInt64(ID), nsHexadecimal, []);
    end
  );

  // Extract unique values
  IDs := TArray.RemoveDuplicates<TLuid>(IDs, TArray.DefaultEqualityCheck<TLuid>);

  // Convert
  SetLength(Result, Length(IDs));

  for i := 0 to High(Result) do
  begin
    Result[i].SessionIdValid := False;
    Result[i].SessionId := 0;
    Result[i].Context := IDs[i];
  end;
end;

end.
