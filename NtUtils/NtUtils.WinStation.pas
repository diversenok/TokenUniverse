unit NtUtils.WinStation;

interface

uses
  Winapi.winsta, NtUtils.Exceptions;

type
  TSessionArray = array of TSessionIdW;

// Enumerate all session on the server for which we have Query access
function WsxEnumerateSessions(out Sessions: TSessionArray;
  hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;

// Query basic information about a session
function WsxQueryInformation(out Info: TWinStationInformation;
  SessionId: Cardinal; hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;

// Format a name of a session, always succeeds with at least an ID
function WsxQuerySessionName(SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): String;

// Open session token
function WsxQuerySessionToken(out hToken: THandle; SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;

implementation

uses
  System.SysUtils;

function WsxEnumerateSessions(out Sessions: TSessionArray;
  hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;
var
  Buffer: PSessionIdArrayW;
  Count, i: Integer;
begin
  Result.Location := 'WinStationEnumerateW';
  Result.Win32Result := WinStationEnumerateW(hServer, Buffer, Count);

  if Result.IsSuccess then
  begin
    SetLength(Sessions, Count);

    for i := 0 to Count - 1 do
      Sessions[i] := Buffer[i];

    WinStationFreeMemory(Buffer);
  end;
end;

function WsxQueryInformation(out Info: TWinStationInformation;
  SessionId: Cardinal; hServer: TWinStaHandle): TNtxStatus;
var
  Rerurned: Cardinal;
begin
  Result.Location := 'WinStationQueryInformationW';
  Result.Win32Result := WinStationQueryInformationW(hServer, SessionId,
    WinStationInformation, @Info, SizeOf(Info), Rerurned);
end;

function WsxQuerySessionName(SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): String;
var
  Info: TWinStationInformation;
begin
  Result := IntToStr(SessionId);

  if WsxQueryInformation(Info, SessionId, hServer).IsSuccess then
  begin
    if Info.WinStationName <> '' then
      Result := Result + ': ' + String(Info.WinStationName);

    Result := Result + ' (' + Info.FullUserName + ')';
  end
end;

function WsxQuerySessionToken(out hToken: THandle; SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;
var
  UserToken: TWinStationUserToken;
  Returned: Cardinal;
begin
  FillChar(UserToken, SizeOf(UserToken), 0);

  // TODO: fall back to WTS Api to workaround a bug with Sandboxie where this
  // call inserts a handle to SbieSvc.exe's handle table and not into ours

  Result.Location := 'WinStationQueryInformationW';
  Result.Win32Result := WinStationQueryInformationW(hServer, SessionId,
    WinStationUserToken, @UserToken, SizeOf(UserToken), Returned);

  if Result.IsSuccess then
    hToken := UserToken.UserToken;
end;

end.
