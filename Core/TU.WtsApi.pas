unit TU.WtsApi;

interface

uses
  Winapi.Windows;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

const
  wtsapi = 'wtsapi32.dll';

type
  TSessionItem = record
    SessionId: Cardinal;
    Name: String;
    Domain: String;
    User: String;
    function ToString: String;
  end;

  TSessionList = class
  private
    function GetCount: Integer;
    function GetSession(ind: Integer): TSessionItem;
  protected
    FSessions: array of TSessionItem;
    procedure CreateStage2(hServer: THandle);
  public
    constructor Create(Server: String);
    constructor CreateCurrentServer;
    property Sessions[ind: Integer]: TSessionItem read GetSession; default;
    property Count: Integer read GetCount;
    /// <summary> Searches for the specified session ID. </summary>
    /// <returns>
    ///  <para> The index of this session if it exists. </para>
    ///  <para> <c>-1</c> otherwise </para>
    ///</returns>
    function Find(SessionId: Cardinal): Integer;
  end;

function WTSQueryUserToken(SessionId: Cardinal; out hToken: THandle): LongBool;
  stdcall; external wtsapi delayed;

implementation

uses
  System.SysUtils;

const
  WTS_CURRENT_SERVER_HANDLE = 0;

type
  TWtsConnectStateClass = (WTSActive, WTSConnected, WTSConnectQuery, WTSShadow,
    WTSDisconnected, WTSIdle, WTSListen, WTSReset, WTSDown, WTSInit);

  TWtsSessionInfoW = record
    SessionId: Cardinal;
    WinStationName: PWideChar;
    State: TWtsConnectStateClass;
  end;

  TWtsSessionInfoArrayW = record
    SessionInfo: array [Word] of TWtsSessionInfoW;
  end;

  PWtsSessionInfoArrayW = ^TWtsSessionInfoArrayW;

  TWtsInfoClass = (WTSInitialProgram, WTSApplicationName, WTSWorkingDirectory,
    WTSOEMId, WTSSessionId, WTSUserName, WTSWinStationName, WTSDomainName,
    WTSConnectState, WTSClientBuildNumber, WTSClientName, WTSClientDirectory,
    WTSClientProductId, WTSClientHardwareId, WTSClientAddress, WTSClientDisplay,
    WTSClientProtocolType, WTSIdleTime, WTSLogonTime, WTSIncomingBytes,
    WTSOutgoingBytes, WTSIncomingFrames, WTSOutgoingFrames, WTSClientInfo,
    WTSSessionInfo, WTSSessionInfoEx, WTSConfigInfo, WTSValidationInfo,
    WTSSessionAddressV4, WTSIsRemoteSession);

function WTSOpenServerW(ServerName: PWideChar): THandle; stdcall;
  external wtsapi delayed;

procedure WTSCloseServer(hServer: THandle); stdcall;
  external wtsapi delayed;

function WTSEnumerateSessionsW(hServer: THandle; Reserved: Cardinal;
  Version: Cardinal; out SessionInfo: PWtsSessionInfoArrayW;
  out Count: Cardinal): LongBool; stdcall; external wtsapi delayed;

function WTSQuerySessionInformationW(hServer: THandle; SessionId: Cardinal;
  WTSInfoClass: TWtsInfoClass; out Buffer: PWideChar;
  out BytesReturne: Cardinal): LongBool; stdcall; external wtsapi delayed;

procedure WTSFreeMemory(Memory: Pointer); stdcall; external wtsapi delayed;

{ TSessionList }

constructor TSessionList.Create(Server: String);
var
  hServer: THandle;
begin
  hServer := WTSOpenServerW(PWideChar(Server));
  CreateStage2(hServer);
  WTSCloseServer(hServer);
end;

constructor TSessionList.CreateCurrentServer;
begin
  CreateStage2(WTS_CURRENT_SERVER_HANDLE);
end;

procedure TSessionList.CreateStage2(hServer: THandle);
var
  SIA: PWtsSessionInfoArrayW;
  StrBuf: PWideChar;
  Count, Returned: Cardinal;
  i: integer;
begin
  if not WTSEnumerateSessionsW(hServer, 0, 1, SIA, Count) then
    Exit;

  SetLength(FSessions, Count);
  for i := 0 to Count - 1 do
  begin
    FSessions[i].SessionId := SIA.SessionInfo[i].SessionId;
    FSessions[i].Name := String(SIA.SessionInfo[i].WinStationName);

    if WTSQuerySessionInformationW(hServer, FSessions[i].SessionId,
      WTSDomainName, StrBuf, Returned) then
    begin
      SetString(FSessions[i].Domain, StrBuf, Returned div SizeOf(WideChar) - 1);
      WTSFreeMemory(StrBuf);
    end;

    if WTSQuerySessionInformationW(hServer, FSessions[i].SessionId,
      WTSUserName, StrBuf, Returned) then
    begin
      SetString(FSessions[i].User, StrBuf, Returned div SizeOf(WideChar) - 1);
      WTSFreeMemory(StrBuf);
    end;
  end;

  WTSFreeMemory(SIA);
end;

function TSessionList.Find(SessionId: Cardinal): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(FSessions) do
    if FSessions[i].SessionId = SessionId then
      Exit(i);
end;

function TSessionList.GetCount: Integer;
begin
  Result := Length(FSessions);
end;

function TSessionList.GetSession(ind: Integer): TSessionItem;
begin
  Result := FSessions[ind];
end;

{ TSessionItem }

function TSessionItem.ToString: String;
begin
  if (User <> '') and (Domain <> '') then
  begin
    if Name <> '' then
      Result := Format('%d: %s (%s/%s)', [SessionID, Name, Domain, User])
    else
      Result := Format('%d: (%s/%s)', [SessionID, Domain, User]);
  end
  else
    Result := Format('%d: %s', [SessionID, Name])
end;

end.
