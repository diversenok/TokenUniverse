unit UI.Prototypes.Logon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  UI.ListViewEx, UI.Prototypes,
  TU.Tokens, TU.LsaApi, Winapi.WinNt, Vcl.StdCtrls;

type
  TFrameLogon = class(TFrame)
    ListView: TListViewEx;
    ComboOrigin: TComboBox;
    StaticOrigin: TStaticText;
    BtnSetOrigin: TButton;
    procedure ComboOriginChange(Sender: TObject);
    procedure BtnSetOriginClick(Sender: TObject);
  private
    Token: TToken;
    LogonSource: TLogonSessionSource;
    IndexOfLogon, IndexOfOrigin: Integer;
    procedure OnLogonIdChange(NewLogonId: TLuid);
    procedure OnOriginChange(NewOrigin: TLuid);
    function GetSubscribed: Boolean;
    function InitItems(Group: Integer): Integer;
    procedure SetItems(StartInd: Integer; LogonInfo: TLogonSessionInfo);
    procedure SetGroupId(StartInd: Integer; GroupId: Integer);
  public
    property Subscribed: Boolean read GetSubscribed;
    procedure SubscribeToken(Token: TToken);
    procedure UnsubscribeToken(Dummy: TToken = nil);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Graphics, UI.Colors, TU.Tokens.Types;

{$R *.dfm}

const
  LogonDataNames: array [TLogonDataClass] of String = ('Logon ID', 'User name',
  'Logon domain', 'Authentication package', 'Logon type', 'Session',
  'Logon time', 'Logon server', 'DNS domain', 'UPN', 'Logon flags',
  'Last succcessful attempt', 'Last failed attempt',
  'Failed attempts since last successful logon',
  'Logon script', 'Profile path', 'Home directory', 'Home drive', 'Logoff time',
  'Kickoff time', 'Password last set', 'Password can change',
  'Password must change');

  // Be consistent with ListView GroupIDs
  GROUP_IND_HIDE = -1;
  GROUP_IND_LOGON = 0;
  GROUP_IND_ORIGIN = 1;

{ TFrameLogon }

procedure TFrameLogon.BtnSetOriginClick(Sender: TObject);
begin
  Assert(Assigned(Token));
  try
    Token.InfoClass.Origin := LogonSource.SelectedLogonSession;
  finally
    // TODO: think how to fix duplicate events
    ComboOrigin.Color := clWindow;
    if Token.InfoClass.Query(tdTokenOrigin) then
      OnOriginChange(Token.InfoClass.Origin);
  end;
end;

procedure TFrameLogon.ComboOriginChange(Sender: TObject);
begin
  ComboOrigin.Color := clStale;
end;

constructor TFrameLogon.Create(AOwner: TComponent);
begin
  inherited;
  LogonSource := TLogonSessionSource.Create(ComboOrigin);

  ListView.Items.BeginUpdate;
  begin
    // Add items for Logon ID
    IndexOfLogon := InitItems(GROUP_IND_LOGON);

    // Add items for Originating Logon
    IndexOfOrigin := InitItems(GROUP_IND_ORIGIN);
  end;
  ListView.Items.EndUpdate;
end;

destructor TFrameLogon.Destroy;
begin
  UnsubscribeToken;
  LogonSource.Free;
  inherited;
end;

function TFrameLogon.GetSubscribed: Boolean;
begin
  Result := Assigned(Token);
end;

function TFrameLogon.InitItems(Group: Integer): Integer;
var
  i: TLogonDataClass;
begin
  Result := ListView.Items.Count;
  for i := Low(TLogonDataClass) to High(TLogonDataClass) do
  begin
    with ListView.Items.Add do
    begin
      Caption := LogonDataNames[i];
      GroupID := Group;
      Cell[1] := 'Unable to query';
    end;
  end;
end;

procedure TFrameLogon.OnLogonIdChange(NewLogonId: TLuid);
var
  LogonData: TLogonSessionInfo;
begin
  ListView.Items[IndexOfLogon].Cell[1] := LuidToString(NewLogonId);

  ListView.Items.BeginUpdate;
  begin
    // Update detailed information
    LogonData := TLogonSessionInfo.Query(NewLogonId);
    SetItems(IndexOfLogon, LogonData);
    LogonData.Free;
  end;
  ListView.Items.EndUpdate;
end;

procedure TFrameLogon.OnOriginChange(NewOrigin: TLuid);
var
  CurrentLogon: TLuid;
  OriginData: TLogonSessionInfo;
begin
  ComboOrigin.Color := clWindow;
  LogonSource.SelectedLogonSession := NewOrigin;

  ListView.Items.BeginUpdate;
  begin
    // Obtain current logon to compare with
    if Token.InfoClass.Query(tdTokenStatistics) then
      CurrentLogon := Token.InfoClass.Statistics.AuthenticationId
    else
      CurrentLogon := 0;

    if NewOrigin = 0 then
      ListView.Items[IndexOfOrigin].Cell[1] := '0 (value not set)'
    else if CurrentLogon = NewOrigin then
      ListView.Items[IndexOfOrigin].Cell[1] := 'Same as current'
    else
      ListView.Items[IndexOfOrigin].Cell[1] := LuidToString(NewOrigin);

    // Show or hide detailed information
    if (NewOrigin <> 0) and (NewOrigin <> CurrentLogon) then
    begin
      SetGroupId(IndexOfOrigin, GROUP_IND_ORIGIN);
      OriginData := TLogonSessionInfo.Query(NewOrigin);
      SetItems(IndexOfOrigin, OriginData);
      OriginData.Free;
    end
    else
      SetGroupId(IndexOfOrigin, GROUP_IND_HIDE);
  end;
  ListView.Items.EndUpdate;
end;

procedure TFrameLogon.SetGroupId(StartInd, GroupId: Integer);
var
  i: TLogonDataClass;
begin
  for i := Succ(Low(TLogonDataClass)) to High(TLogonDataClass) do
    ListView.Items[StartInd + Integer(i)].GroupID := GroupId;
end;

procedure TFrameLogon.SetItems(StartInd: Integer; LogonInfo: TLogonSessionInfo);
var
  i: TLogonDataClass;
begin
  // Note: LogonInfo can be nil, GetString works fine with it
  for i := Succ(Low(TLogonDataClass)) to High(TLogonDataClass) do
    ListView.Items[StartInd + Integer(i)].Cell[1] := LogonInfo.GetString(i);
end;

procedure TFrameLogon.SubscribeToken(Token: TToken);
begin
  UnsubscribeToken;

  Self.Token := Token;
  Token.OnClose.Add(UnsubscribeToken);

  LogonSource.UpdateLogonSessions;

  if Token.InfoClass.Query(tdTokenStatistics) then
    OnLogonIdChange(Token.InfoClass.Statistics.AuthenticationId);

  Token.Events.OnOriginChange.Add(OnOriginChange);
end;

procedure TFrameLogon.UnsubscribeToken(Dummy: TToken);
begin
  if Assigned(Token) then
  begin
    Token.Events.OnOriginChange.Delete(OnOriginChange);
    Token.OnClose.Delete(UnsubscribeToken);
    Token := nil;
  end;
end;

end.
