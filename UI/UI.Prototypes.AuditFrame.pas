unit UI.Prototypes.AuditFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, UI.ListViewEx, TU.Tokens, NtUtils.Types;

type
  TFrameAudit = class(TFrame)
    ListView: TListViewEx;
    AuditPopup: TPopupMenu;
    AuditIncSucc: TMenuItem;
    AuditExcSucc: TMenuItem;
    AuditIncFail: TMenuItem;
    AuditExcFail: TMenuItem;
    ButtonApply: TButton;
    LabelNote: TLabel;
    procedure ListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure AuditIncSuccClick(Sender: TObject);
    procedure AuditExcSuccClick(Sender: TObject);
    procedure AuditIncFailClick(Sender: TObject);
    procedure AuditExcFailClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Token: TToken;
    Policy: ITokenPerUserAudit;
    procedure OnAuditChange(NewAudit: ITokenPerUserAudit);
    procedure ProcessPopupAction(Flag: Integer; NewState: Boolean);
    procedure FillRow(Index: Integer);
    function GetSubscribed: Boolean;
  public
    property Subscribed: Boolean read GetSubscribed;
    procedure SubscribeToken(Token: TToken);
    procedure UnsubscribeToken(Dummy: TToken = nil);
    procedure UpdateCategories;
    destructor Destroy; override;
  end;

implementation

uses
  Winapi.NtSecApi, Ntapi.ntdef, NtUtils.Lsa, UI.Colors, DelphiUtils.Strings;

{$R *.dfm}

{ TFrameAudit }

procedure TFrameAudit.AuditExcFailClick(Sender: TObject);
begin
  ProcessPopupAction(PER_USER_AUDIT_FAILURE_EXCLUDE, not AuditExcFail.Checked);
end;

procedure TFrameAudit.AuditExcSuccClick(Sender: TObject);
begin
  ProcessPopupAction(PER_USER_AUDIT_SUCCESS_EXCLUDE, not AuditExcSucc.Checked);
end;

procedure TFrameAudit.AuditIncFailClick(Sender: TObject);
begin
  ProcessPopupAction(PER_USER_AUDIT_FAILURE_INCLUDE, not AuditIncFail.Checked);
end;

procedure TFrameAudit.AuditIncSuccClick(Sender: TObject);
begin
  ProcessPopupAction(PER_USER_AUDIT_SUCCESS_INCLUDE, not AuditIncSucc.Checked);
end;

procedure TFrameAudit.ButtonApplyClick(Sender: TObject);
begin
  try
    Token.InfoClass.AuditPolicy := Policy;
  except
    SubscribeToken(Token);
    raise;
  end;
end;

destructor TFrameAudit.Destroy;
begin
  UnsubscribeToken;
  inherited;
end;

procedure TFrameAudit.FillRow(Index: Integer);
begin
  Assert(Assigned(Policy));

  with ListView.Items[Index] do
    begin
      Cell[1] := CheckboxToString(Policy.ContainsFlag(Index,
        PER_USER_AUDIT_SUCCESS_INCLUDE));

      Cell[2] := CheckboxToString(Policy.ContainsFlag(Index,
        PER_USER_AUDIT_SUCCESS_EXCLUDE));

      Cell[3] := CheckboxToString(Policy.ContainsFlag(Index,
        PER_USER_AUDIT_FAILURE_INCLUDE));

      Cell[4] :=  CheckboxToString(Policy.ContainsFlag(Index,
        PER_USER_AUDIT_FAILURE_EXCLUDE));
    end
end;

function TFrameAudit.GetSubscribed: Boolean;
begin
  Result := Assigned(Token);
end;

procedure TFrameAudit.ListViewContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  i: Integer;
begin
  if not Assigned(Policy) then
  begin
    Handled := True;
    Exit;
  end;

  AuditIncSucc.Checked := False;
  AuditExcSucc.Checked := False;
  AuditIncFail.Checked := False;
  AuditExcFail.Checked := False;

  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin    
      if Policy.ContainsFlag(i, PER_USER_AUDIT_SUCCESS_INCLUDE) then
        AuditIncSucc.Checked := True;

      if Policy.ContainsFlag(i, PER_USER_AUDIT_SUCCESS_EXCLUDE) then
        AuditExcSucc.Checked := True;

      if Policy.ContainsFlag(i, PER_USER_AUDIT_FAILURE_INCLUDE) then
        AuditIncFail.Checked := True;

      if Policy.ContainsFlag(i, PER_USER_AUDIT_FAILURE_EXCLUDE) then
        AuditExcFail.Checked := True;
    end;
end;

procedure TFrameAudit.OnAuditChange(NewAudit: ITokenPerUserAudit);
begin
  Policy := NewAudit;
  ButtonApply.Enabled := Assigned(Policy);
  UpdateCategories;
end;

procedure TFrameAudit.ProcessPopupAction(Flag: Integer; NewState: Boolean);
var
  i: Integer;
begin
  Assert(Assigned(Policy));

  ListView.Items.BeginUpdate;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      Policy.SetFlag(i, Flag, NewState);
      FillRow(i);
      ListView.Items[i].Color := clStale;
    end;
  ListView.Items.EndUpdate;
end;

procedure TFrameAudit.SubscribeToken(Token: TToken);
begin
  UnsubscribeToken;

  Self.Token := Token;
  Token.OnClose.Subscribe(UnsubscribeToken);

  if Token.InfoClass.Query(tdTokenAuditPolicy) then
    OnAuditChange(Token.InfoClass.AuditPolicy)
  else
    OnAuditChange(nil);

  Token.Events.OnAuditChange.Subscribe(OnAuditChange, False);
end;

procedure TFrameAudit.UnsubscribeToken(Dummy: TToken);
begin
  if Assigned(Token) then
  begin
    Token.Events.OnAuditChange.Unsubscribe(OnAuditChange);
    Token.OnClose.Unsubscribe(UnsubscribeToken);
    Token := nil;
    OnAuditChange(nil);
  end;
end;

procedure TFrameAudit.UpdateCategories;
var
  AuditEnties: TAuditCategories;
  Ind, SubInd: Integer;
begin
  Assert(Assigned(ListView));

  if not NT_SUCCESS(LsaxEnumerateAuditCategiries(AuditEnties))then
    Exit;

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  // Update ListView Groups
  begin
    ListView.Groups.BeginUpdate;
    ListView.Groups.Clear;

    for Ind := 0 to High(AuditEnties.Categories) do
      with ListView.Groups.Add do
      begin
        Header := AuditEnties.Categories[Ind].Name;
        State := State + [lgsCollapsible];
      end;

    ListView.Groups.EndUpdate;
  end;

  // Update ListView Items
  for Ind := 0 to High(AuditEnties.SubCategories) do
    for SubInd := 0 to High(AuditEnties.SubCategories[Ind]) do
      with ListView.Items.Add do
      begin
        Caption := AuditEnties.SubCategories[Ind, SubInd].Name;
        GroupID := Ind;
      end;

  // Fill up data from the token
  if Assigned(Policy) then
    for Ind := 0 to ListView.Items.Count - 1 do
      FillRow(Ind)
  else
    for Ind := 0 to ListView.Items.Count - 1 do
      for SubInd := 1 to 4 do
        ListView.Items[Ind].SubItems.Add('?');

  ListView.Items.EndUpdate;
end;

end.
