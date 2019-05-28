unit NtUtils.WinUser;

interface

uses
  Winapi.WinUser;

type
  TStringArray = Winapi.WinUser.TStringArray;

// GetUserObjectInformationW
function UsrxQueryObjectName(hObj: THandle; out Name: String): Boolean;

// EnumWindowStationsW
function UsrxEnumWindowStations(out WinStations: TStringArray): Boolean;

// EnumDesktopsW
function UsrxEnumDesktops(WinSta: HWINSTA; out Desktops: TStringArray): Boolean;

// EnumWindowStationsW + EnumDesktopsW
function UsrxEnumAllDesktops: TStringArray;

// GetThreadDesktop + GetUserObjectInformationW
function UsrxCurrentDesktopName: String;

implementation

uses
  NtUtils.Exceptions, Winapi.ProcessThreadsApi, Ntapi.ntpsapi;

function UsrxQueryObjectName(hObj: THandle; out Name: String): Boolean;
var
  Buffer: PWideChar;
  BufferSize: Cardinal;
begin
  BufferSize := 0;
  GetUserObjectInformationW(hObj, UserObjectName, nil, 0, @BufferSize);

  if not WinTryCheckBuffer(BufferSize) then
    Exit(False);

  Buffer := AllocMem(BufferSize);
  try
    Result := GetUserObjectInformationW(hObj, UserObjectName, Buffer,
      BufferSize, nil);

    if Result then
      Name := String(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

function EnumCallback(Name: PWideChar; var Context: TStringArray): LongBool;
  stdcall;
begin
  SetLength(Context, Length(Context) + 1);
  Context[High(Context)] := String(Name);
  Result := True;
end;

function UsrxEnumWindowStations(out WinStations: TStringArray): Boolean;
begin
  SetLength(WinStations, 0);
  Result := EnumWindowStationsW(EnumCallback, WinStations);
end;

function UsrxEnumDesktops(WinSta: HWINSTA; out Desktops: TStringArray): Boolean;
begin
  SetLength(Desktops, 0);
  Result := EnumDesktopsW(WinSta, EnumCallback, Desktops);
end;

function UsrxEnumAllDesktops: TStringArray;
var
  i, j: Integer;
  hWinStation: HWINSTA;
  WinStations, Desktops: TStringArray;
begin
  SetLength(Result, 0);

  if not UsrxEnumWindowStations(WinStations) then
    Exit;

  for i := 0 to High(WinStations) do
  begin
    // Open each window station
    hWinStation := OpenWindowStationW(PWideChar(WinStations[i]), False,
      WINSTA_ENUMDESKTOPS);

    if hWinStation = 0 then
      Continue;

    // Enumerate desktops of this window station
    if UsrxEnumDesktops(hWinStation, Desktops) then
    begin
      // Expand each name
      for j := 0 to High(Desktops) do
        Desktops[j] := WinStations[i] + '\' + Desktops[j];

      Insert(Desktops, Result, Length(Result));
    end;

   CloseWindowStation(hWinStation);
  end;
end;

function UsrxCurrentDesktopName: String;
var
  WinStaName: String;
  StartupInfo: TStartupInfoW;
begin
  if UsrxQueryObjectName(GetThreadDesktop(NtCurrentThreadId), Result) then
  begin
    if UsrxQueryObjectName(GetProcessWindowStation, WinStaName) then
      Result := WinStaName + '\' + Result;
  end
  else
  begin
    // This is very unlikely to happen. Fall back to using the value
    // from the startupinfo structure.
    GetStartupInfoW(StartupInfo);
    Result := String(StartupInfo.lpDesktop);
  end;
end;

end.
