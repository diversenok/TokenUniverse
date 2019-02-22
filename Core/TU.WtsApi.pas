unit TU.WtsApi;

interface

uses
  Winapi.WinNt;

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

const
  wtsapi = 'wtsapi32.dll';

type
  /// <summary>
  ///  A record to hold information about a terminal server session.
  /// </summary>
  TSessionInformation = record
    SessionId: Cardinal;
    Name: String;
    Domain: String;
    User: String;
    function ToString: String;
  end;

  /// <summary> Stores a snapshot of all sessions on the system. </summary>
  TSessionList = class
  private
    function GetCount: Integer;
    function GetSession(ind: Integer): TSessionInformation;
  protected
    FSessions: array of TSessionInformation;
    procedure CreateStage2(hServer: THandle);
  public
    /// <summary>
    ///  Captures a snapshot of sessions on the specified server.
    /// </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create(Server: String);

    /// <summary> Captures a local snapshot of sessions. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor CreateCurrentServer;

    property Sessions[ind: Integer]: TSessionInformation read GetSession; default;
    property Count: Integer read GetCount;

    /// <summary> Searches for the specified session ID. </summary>
    /// <returns>
    ///  <para> The index of this session if it exists. </para>
    ///  <para> <c>-1</c> otherwise </para>
    ///</returns>
    function Find(SessionId: Cardinal): Integer;
  end;

function WTSQueryUserToken(SessionId: Cardinal; out hToken: THandle): LongBool;
  stdcall; external wtsapi;

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
  external wtsapi;

procedure WTSCloseServer(hServer: THandle); stdcall;
  external wtsapi;

function WTSEnumerateSessionsW(hServer: THandle; Reserved: Cardinal;
  Version: Cardinal; out SessionInfo: PWtsSessionInfoArrayW;
  out Count: Cardinal): LongBool; stdcall; external wtsapi;

function WTSQuerySessionInformationW(hServer: THandle; SessionId: Cardinal;
  WTSInfoClass: TWtsInfoClass; out Buffer: PWideChar;
  out BytesReturne: Cardinal): LongBool; stdcall; external wtsapi;

procedure WTSFreeMemory(Memory: Pointer); stdcall; external wtsapi;

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
  // Try to enumerate the sessions
  if not WTSEnumerateSessionsW(hServer, 0, 1, SIA, Count) then
    Exit;

  // Allocate memory
  SetLength(FSessions, Count);

  for i := 0 to Count - 1 do
  begin
    FSessions[i].SessionId := SIA.SessionInfo[i].SessionId;
    FSessions[i].Name := WideCharToString(SIA.SessionInfo[i].WinStationName);

    // Query owner's domain
    if WTSQuerySessionInformationW(hServer, FSessions[i].SessionId,
      WTSDomainName, StrBuf, Returned) then
    begin
      SetString(FSessions[i].Domain, StrBuf, Returned div SizeOf(WideChar) - 1);
      WTSFreeMemory(StrBuf);
    end;

    // Query owner's user name
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

function TSessionList.GetSession(ind: Integer): TSessionInformation;
begin
  Result := FSessions[ind];
end;

{ TSessionInformation }

function TSessionInformation.ToString: String;
begin
  if (User <> '') and (Domain <> '') then
  begin
    if Name <> '' then
      Result := Format('%d: %s (%s\%s)', [SessionID, Name, Domain, User])
    else
      Result := Format('%d: (%s\%s)', [SessionID, Domain, User]);
  end
  else
    Result := Format('%d: %s', [SessionID, Name])
end;

end.
