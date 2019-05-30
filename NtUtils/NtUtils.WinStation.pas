unit NtUtils.WinStation;

interface

uses
  Winapi.winsta, NtUtils.Exceptions;

type
  TSessionArray = array of TSessionIdW;

// WinStationEnumerateW
function WsxEnumerateSessions(out Sessions: TSessionArray;
  hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;

// WinStationQueryInformationW
function WsxQueryInformation(out Info: TWinStationInformation;
  SessionId: Cardinal; hServer: TWinStaHandle = SERVER_CURRENT): TNtxStatus;

function WsxQuerySessionName(SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): String;

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

end.
