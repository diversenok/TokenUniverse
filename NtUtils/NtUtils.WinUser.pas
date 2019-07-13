unit NtUtils.WinUser;

interface

uses
  Winapi.WinNt, Winapi.WinUser, NtUtils.Exceptions, NtUtils.Security.Sid;

type
  TNtxStatus = NtUtils.Exceptions.TNtxStatus;

{ Open }

// Open desktop
function UsrxOpenDesktop(out hDesktop: THandle; Name: String;
  DesiredAccess: TAccessMask; InheritHandle: Boolean = False): TNtxStatus;

// Open window station
function UsrxOpenWindowStation(out hWinSta: THandle; Name: String;
  DesiredAccess: TAccessMask; InheritHandle: Boolean = False): TNtxStatus;

{ Query information }

// Query any information
function UsrxQueryBufferObject(hObj: THandle; InfoClass: TUserObjectInfoClass;
  out Status: TNtxStatus): Pointer;

// Quer user object name
function UsrxQueryObjectName(hObj: THandle; out Name: String): TNtxStatus;

// Query user object SID
function UsrxQueryObjectSid(hObj: THandle; out Sid: ISid): TNtxStatus;

// Query a name of a current desktop
function UsrxCurrentDesktopName: String;

{ Enumerations }

// Enumerate window stations of current session
function UsrxEnumWindowStations(out WinStations: TArray<String>): TNtxStatus;

// Enumerate desktops of a window station
function UsrxEnumDesktops(WinSta: HWINSTA; out Desktops: TArray<String>):
  TNtxStatus;

// Enumerate all accessable desktops from different window stations
function UsrxEnumAllDesktops: TArray<String>;

implementation

uses
  Winapi.ProcessThreadsApi, Ntapi.ntpsapi;

function UsrxOpenDesktop(out hDesktop: THandle; Name: String;
  DesiredAccess: TAccessMask; InheritHandle: Boolean): TNtxStatus;
begin
  Result.Location := 'OpenDesktopW';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objUsrDesttop;

  hDesktop := OpenDesktopW(PWideChar(Name), 0, InheritHandle, DesiredAccess);
  Result.Win32Result := (hDesktop <> 0);
end;

function UsrxOpenWindowStation(out hWinSta: THandle; Name: String;
  DesiredAccess: TAccessMask; InheritHandle: Boolean): TNtxStatus;
begin
  Result.Location := 'OpenWindowStationW';
  Result.LastCall.CallType := lcOpenCall;
  Result.LastCall.AccessMask := DesiredAccess;
  Result.LastCall.AccessMaskType := TAccessMaskType.objUsrWindowStation;

  hWinSta := OpenWindowStationW(PWideChar(Name), InheritHandle, DesiredAccess);
  Result.Win32Result := (hWinSta <> 0);
end;

function UsrxQueryBufferObject(hObj: THandle; InfoClass: TUserObjectInfoClass;
  out Status: TNtxStatus): Pointer;
var
  BufferSize: Cardinal;
begin
  BufferSize := 0;
  Status.Location := 'GetUserObjectInformationW';
  Status.LastCall.CallType := lcQuerySetCall;
  Status.LastCall.InfoClass := Cardinal(InfoClass);
  Status.LastCall.InfoClassType := TypeInfo(TUserObjectInfoClass);

  // Probe call
  Status.Win32Result := GetUserObjectInformationW(hObj, InfoClass, nil, 0,
    @BufferSize);

  if not NtxTryCheckBuffer(Status.Status, BufferSize) then
    Exit(nil);

  Result := AllocMem(BufferSize);

  // TODO: fix race condition

  Status.Win32Result := GetUserObjectInformationW(hObj, InfoClass,
      Result, BufferSize, nil);

  if not Status.IsSuccess then
  begin
    FreeMem(Result);
    Result := nil;
  end;
end;

function UsrxQueryObjectName(hObj: THandle; out Name: String): TNtxStatus;
var
  Buffer: PWideChar;
begin
  Buffer := UsrxQueryBufferObject(hObj, UserObjectName, Result);

  if not Result.IsSuccess then
    Exit;

  Name := String(Buffer);
  FreeMem(Buffer);
end;

function UsrxQueryObjectSid(hObj: THandle; out Sid: ISid): TNtxStatus;
var
  Buffer: PSid;
begin
  Buffer := UsrxQueryBufferObject(hObj, UserObjectUserSid, Result);

  if not Result.IsSuccess then
    Exit;

  if Assigned(Buffer) then
    Sid := TSid.CreateCopy(Buffer)
  else
    Sid := nil;

  FreeMem(Buffer);
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

function EnumCallback(Name: PWideChar; var Context: TArray<String>): LongBool;
  stdcall;
begin
  // Save the value and succeed
  SetLength(Context, Length(Context) + 1);
  Context[High(Context)] := String(Name);
  Result := True;
end;

function UsrxEnumWindowStations(out WinStations: TArray<String>): TNtxStatus;
begin
  SetLength(WinStations, 0);
  Result.Location := 'EnumWindowStationsW';
  Result.Win32Result := EnumWindowStationsW(EnumCallback, WinStations);
end;

function UsrxEnumDesktops(WinSta: HWINSTA; out Desktops: TArray<String>):
  TNtxStatus;
begin
  SetLength(Desktops, 0);
  Result.Location := 'EnumDesktopsW';
  Result.Win32Result := EnumDesktopsW(WinSta, EnumCallback, Desktops);
end;

function UsrxEnumAllDesktops: TArray<String>;
var
  i, j: Integer;
  hWinStation: HWINSTA;
  WinStations, Desktops: TArray<String>;
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

end.
