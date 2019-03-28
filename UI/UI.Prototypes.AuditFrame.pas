unit UI.Prototypes.AuditFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, UI.ListViewEx, TU.Tokens, TU.Tokens.Types;

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
    Policy: TTokenPerUserAudit;
    procedure OnAuditChange(NewAudit: TTokenPerUserAudit);
    procedure ProcessPopupAction(Enable: Boolean; Column: TAuditState);
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
  Winapi.WinError, NtUtils.Audit, UI.Colors;

{$R *.dfm}

{ TFrameAudit }

procedure TFrameAudit.AuditExcFailClick(Sender: TObject);
begin
  ProcessPopupAction(not AuditExcFail.Checked, asExcludeFailure);
end;

procedure TFrameAudit.AuditExcSuccClick(Sender: TObject);
begin
  ProcessPopupAction(not AuditExcSucc.Checked, asExcludeSuccess);
end;

procedure TFrameAudit.AuditIncFailClick(Sender: TObject);
begin
  ProcessPopupAction(not AuditIncFail.Checked, asIncludeFailure);
end;

procedure TFrameAudit.AuditIncSuccClick(Sender: TObject);
begin
  ProcessPopupAction(not AuditIncSucc.Checked, asIncludeSuccess);
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
  Policy.Free;
  inherited;
end;

procedure TFrameAudit.FillRow(Index: Integer);
var
  State: TAuditState;
begin
  Assert(Assigned(Policy));

  with ListView.Items[Index] do
    begin
      State := Policy.SubCategory[Index];
      Cell[1] := CheckboxToString(State.Contains(asIncludeSuccess));
      Cell[2] := CheckboxToString(State.Contains(asExcludeSuccess));
      Cell[3] := CheckboxToString(State.Contains(asIncludeFailure));
      Cell[4] := CheckboxToString(State.Contains(asExcludeFailure));
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
  State: TAuditState;
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
      State := Policy.SubCategory[i];

      if State.Contains(asIncludeSuccess) then
        AuditIncSucc.Checked := True;
      if State.Contains(asExcludeSuccess) then
        AuditExcSucc.Checked := True;
      if State.Contains(asIncludeFailure) then
        AuditIncFail.Checked := True;
      if State.Contains(asExcludeFailure) then
        AuditExcFail.Checked := True;
    end;
end;

procedure TFrameAudit.OnAuditChange(NewAudit: TTokenPerUserAudit);
begin
  Policy.Free;

  if Assigned(NewAudit) then
    Policy := TTokenPerUserAudit.CreateCopy(NewAudit)
  else
    Policy := nil;

  ButtonApply.Enabled := Assigned(Policy);

  UpdateCategories;
end;

procedure TFrameAudit.ProcessPopupAction(Enable: Boolean; Column: TAuditState);
var
  i: Integer;
  State: TAuditState;
begin
  Assert(Assigned(Policy));

  ListView.Items.BeginUpdate;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      State := Policy.SubCategory[i];

      if Enable then
        State.Include(Column)
      else
        State.Exclude(Column);

      Policy.SubCategory[i] := State;
      FillRow(i);

      ListView.Items[i].Color := clStale;
    end;
  ListView.Items.EndUpdate;
end;

procedure TFrameAudit.SubscribeToken(Token: TToken);
begin
  UnsubscribeToken;

  Self.Token := Token;
  Token.OnClose.Add(UnsubscribeToken);

  if Token.InfoClass.Query(tdTokenAuditPolicy) then
    OnAuditChange(Token.InfoClass.AuditPolicy)
  else
    OnAuditChange(nil);

  Token.Events.OnAuditChange.Add(OnAuditChange, False);
end;

procedure TFrameAudit.UnsubscribeToken(Dummy: TToken);
begin
  if Assigned(Token) then
  begin
    Token.Events.OnAuditChange.Delete(OnAuditChange);
    Token.OnClose.Delete(UnsubscribeToken);
    Token := nil;

    Policy.Free;
    Policy := nil;
    ButtonApply.Enabled := False;
  end;
end;

procedure TFrameAudit.UpdateCategories;
var
  AuditEnties: TAuditCategories;
  Ind, SubInd: Integer;
begin
  Assert(Assigned(ListView));

  if EnumerateAuditCategiries(AuditEnties) <> ERROR_SUCCESS then
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
