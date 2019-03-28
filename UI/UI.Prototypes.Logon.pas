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
    IndexOfLogon: Integer;
    procedure OnOriginChange(NewOrigin: TLuid);
    function GetSubscribed: Boolean;
  public
    property Subscribed: Boolean read GetSubscribed;
    procedure SubscribeToken(Token: TToken);
    procedure UnsubscribeToken(Dummy: TToken = nil);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TLogonInfoSource = class
    class function InitItems(ListView: TListViewEx; GroupInd: Integer):
      Integer; static;
    class procedure SetItems(ListView: TListViewEx; StartInd: Integer;
      LogonInfo: TLogonSessionInfo); static;
  end;

implementation

uses
  Vcl.Graphics, UI.Colors, TU.Tokens.Types, DelphiUtils.Strings;

{$R *.dfm}

const
  LogonDataNames: array [TLogonDataClass] of String = ('Logon ID',
  'Security identifier', 'User name', 'Logon domain', 'Authentication package',
  'Logon type', 'Session', 'Logon time', 'Logon server', 'DNS domain', 'UPN',
  'Logon flags', 'Last succcessful attempt', 'Last failed attempt',
  'Failed attempts since last successful logon',
  'Logon script', 'Profile path', 'Home directory', 'Home drive', 'Logoff time',
  'Kickoff time', 'Password last set', 'Password can change from',
  'Password must change before');

  // Be consistent with ListView
  ITEM_IND_ORIGIN = 0;
  GROUP_IND_LOGON = 0;

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
  // TODO: TLogonSessionSource triggers enumeration, postpone it until
  // the user actually switches to the tab
  LogonSource := TLogonSessionSource.Create(ComboOrigin);

  // Add items for Logon ID
  IndexOfLogon := TLogonInfoSource.InitItems(ListView, GROUP_IND_LOGON);
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

procedure TFrameLogon.OnOriginChange(NewOrigin: TLuid);
begin
  ComboOrigin.Color := clWindow;
  LogonSource.SelectedLogonSession := NewOrigin;

  if NewOrigin = 0 then
    ListView.Items[ITEM_IND_ORIGIN].Cell[1] := '0 (value not set)'
  else if Token.InfoClass.Query(tdTokenStatistics) and
    (Token.InfoClass.Statistics.AuthenticationId = NewOrigin) then
    ListView.Items[ITEM_IND_ORIGIN].Cell[1] := 'Same as current'
  else
    ListView.Items[ITEM_IND_ORIGIN].Cell[1] := IntToHexEx(NewOrigin);
end;

procedure TFrameLogon.SubscribeToken(Token: TToken);
begin
  UnsubscribeToken;

  Self.Token := Token;
  Token.OnClose.Subscribe(UnsubscribeToken);

  LogonSource.UpdateLogonSessions;

  // Update Logon ID value
  if Token.InfoClass.Query(tdTokenStatistics) then
    ListView.Items[IndexOfLogon].Cell[1] := IntToHexEx(
      Token.InfoClass.Statistics.AuthenticationId)
  else
    ListView.Items[IndexOfLogon].Cell[1] := 'Unknown';

  // Update detailed information
  if Token.InfoClass.Query(tdLogonInfo) then
    TLogonInfoSource.SetItems(ListView, IndexOfLogon,
      Token.InfoClass.LogonSessionInfo)
  else
    TLogonInfoSource.SetItems(ListView, IndexOfLogon, nil);

  Token.Events.OnOriginChange.Subscribe(OnOriginChange);
end;

procedure TFrameLogon.UnsubscribeToken(Dummy: TToken);
begin
  if Assigned(Token) then
  begin
    Token.Events.OnOriginChange.Unsubscribe(OnOriginChange);
    Token.OnClose.Unsubscribe(UnsubscribeToken);
    Token := nil;
  end;
end;

{ TLogonInfoSource }

class function TLogonInfoSource.InitItems(ListView: TListViewEx;
  GroupInd: Integer): Integer;
var
  i: TLogonDataClass;
begin
  ListView.Items.BeginUpdate;

  Result := ListView.Items.Count;
  for i := Low(TLogonDataClass) to High(TLogonDataClass) do
  begin
    with ListView.Items.Add do
    begin
      Caption := LogonDataNames[i];
      GroupID := GroupInd;
    end;
  end;

  ListView.Items.EndUpdate;
end;

class procedure TLogonInfoSource.SetItems(ListView: TListViewEx;
  StartInd: Integer; LogonInfo: TLogonSessionInfo);
var
  i: TLogonDataClass;
begin
  // Note: LogonInfo can be nil, GetString and UserFlagsHint work fine with it

  for i := Succ(Low(TLogonDataClass)) to High(TLogonDataClass) do
    ListView.Items[StartInd + Integer(i)].Cell[1] := LogonInfo.GetString(i);

  // Build hint for UserFlags
  ListView.Items[StartInd + Integer(lsUserFlags)].Hint :=
    LogonInfo.UserFlagsHint;

  // Build hint for the user
  if LogonInfo.UserPresent then
    ListView.Items[StartInd + Integer(lsSecurityIdentifier)].Hint :=
      TGroupsSource.BuildHint(LogonInfo.User, GroupExUser, False);
end;

end.
