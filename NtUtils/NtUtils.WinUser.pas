unit NtUtils.WinUser;

interface

uses
  Winapi.WinUser, NtUtils.Exceptions;

type
  TNtxStatus = NtUtils.Exceptions.TNtxStatus;
  TStringArray = Winapi.WinUser.TStringArray;

// Quer user object name
function UsrxQueryObjectName(hObj: THandle; out Name: String): TNtxStatus;

// Enumerate window stations of current session
function UsrxEnumWindowStations(out WinStations: TStringArray): TNtxStatus;

// Enumerate desktops of a window station
function UsrxEnumDesktops(WinSta: HWINSTA; out Desktops: TStringArray):
  TNtxStatus;

// Enumerate all accessable desktops from different window stations
function UsrxEnumAllDesktops: TStringArray;

// Query a name of a current desktop
function UsrxCurrentDesktopName: String;

implementation

uses
  Winapi.ProcessThreadsApi, Ntapi.ntpsapi;

function UsrxQueryObjectName(hObj: THandle; out Name: String): TNtxStatus;
var
  Buffer: PWideChar;
  BufferSize: Cardinal;
begin
  BufferSize := 0;

  // Determine required buffer size
  Result.Location := 'GetUserObjectInformationW';
  Result.LastCall.CallType := lcQuerySetCall;
  Result.LastCall.InfoClass := Cardinal(UserObjectName);
  Result.LastCall.InfoClassType := TypeInfo(TUserObjectInfoClass);

  Result.Win32Result := GetUserObjectInformationW(hObj, UserObjectName, nil, 0,
    @BufferSize);

  if not NtxTryCheckBuffer(Result.Status, BufferSize) then
    Exit;

  Buffer := AllocMem(BufferSize);
  try
    Result.Win32Result := GetUserObjectInformationW(hObj, UserObjectName,
      Buffer, BufferSize, nil);

    if Result.IsSuccess then
      Name := String(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

function EnumCallback(Name: PWideChar; var Context: TStringArray): LongBool;
  stdcall;
begin
  // Save the value and succeed
  SetLength(Context, Length(Context) + 1);
  Context[High(Context)] := String(Name);
  Result := True;
end;

function UsrxEnumWindowStations(out WinStations: TStringArray): TNtxStatus;
begin
  SetLength(WinStations, 0);
  Result.Location := 'EnumWindowStationsW';
  Result.Win32Result := EnumWindowStationsW(EnumCallback, WinStations);
end;

function UsrxEnumDesktops(WinSta: HWINSTA; out Desktops: TStringArray):
  TNtxStatus;
begin
  SetLength(Desktops, 0);
  Result.Location := 'EnumDesktopsW';
  Result.Win32Result := EnumDesktopsW(WinSta, EnumCallback, Desktops);
end;

function UsrxEnumAllDesktops: TStringArray;
var
  i, j: Integer;
  hWinStation: HWINSTA;
  WinStations, Desktops: TStringArray;
begin
  SetLength(Result, 0);

  // Enumerate accessable window stations
  if not UsrxEnumWindowStations(WinStations).IsSuccess then
    Exit;

  for i := 0 to High(WinStations) do
  begin
    // Open each window station
    hWinStation := OpenWindowStationW(PWideChar(WinStations[i]), False,
      WINSTA_ENUMDESKTOPS);

    if hWinStation = 0 then
      Continue;

    // Enumerate desktops of this window station
    if UsrxEnumDesktops(hWinStation, Desktops).IsSuccess then
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
  // Read our thread's desktop and query its name
  if UsrxQueryObjectName(GetThreadDesktop(NtCurrentThreadId), Result).IsSuccess
    then
  begin
    if UsrxQueryObjectName(GetProcessWindowStation, WinStaName).IsSuccess then
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
