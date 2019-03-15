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
    procedure OnOriginChange(NewOrigin: TLuid);
    function GetSubscribed: Boolean;
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
end;

procedure TFrameLogon.SubscribeToken(Token: TToken);
begin
  UnsubscribeToken;

  Self.Token := Token;
  Token.OnClose.Add(UnsubscribeToken);

  LogonSource.UpdateLogonSessions;
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
