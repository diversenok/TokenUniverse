unit NtUtils.WinStation;

interface

uses
  Winapi.winsta;

type
  TSessionArray = array of TSessionIdW;

function EnumerateSessions(hServer: TWinStaHandle = SERVER_CURRENT): TSessionArray;
function FindSessionById(Sessions: TSessionArray; SessionId: Cardinal): Integer;
function QuerySessionFullName(SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): String;

implementation

uses
  Winapi.WinError, System.SysUtils;

function EnumerateSessions(hServer: TWinStaHandle): TSessionArray;
var
  Buffer: PSessionIdArrayW;
  Count, i: Integer;
begin
  if WinStationEnumerateW(hServer, Buffer, Count) then
  try
    SetLength(Result, Count);
    for i := 0 to Count - 1 do
      Result[i] := Buffer[i];
  finally
    WinStationFreeMemory(Buffer);
  end
  else
    SetLength(Result, 0);
end;

function FindSessionById(Sessions: TSessionArray; SessionId: Cardinal): Integer;
var
  i: Integer;
begin
  for i := 0 to High(Sessions) do
    if Sessions[i].SessionId = SessionId then
      Exit(i);

  Result := -1;
end;

function QuerySessionFullName(SessionId: Cardinal;
  hServer: TWinStaHandle = SERVER_CURRENT): String;
var
  Info: TWinStationInformation;
  Rerurned: Cardinal;
begin
  Result := IntToStr(SessionId);

  if WinStationQueryInformationW(hServer, SessionId, WinStationInformation,
    @Info, SizeOf(Info), Rerurned) then
  begin
    if Info.WinStationName <> '' then
      Result := Result + ': ' + String(Info.WinStationName);

    Result := Result + ' (' + Info.FullUserName + ')';
  end
end;

end.
