unit UI.Prototypes.AuditFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, UI.ListViewEx, NtUtils.Lsa.Audit;

type
  TApplyProc = procedure (AuditPolicy: IPerUserAudit) of object;

  TFrameAudit = class(TFrame)
    ListView: TListViewEx;
    AuditPopup: TPopupMenu;
    AuditIncSucc: TMenuItem;
    AuditExcSucc: TMenuItem;
    AuditIncFail: TMenuItem;
    AuditExcFail: TMenuItem;
    ButtonApply: TButton;
    LabelStatus: TLabel;
    procedure ListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure AuditIncSuccClick(Sender: TObject);
    procedure AuditExcSuccClick(Sender: TObject);
    procedure AuditIncFailClick(Sender: TObject);
    procedure AuditExcFailClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Categories: TGuidDynArray;
    SubCategories: TGuidDynArray2;
    Policy: IPerUserAudit;
    ApplyProc: TApplyProc;

    procedure ProcessPopupAction(Flag: Integer; NewState: Boolean);
    procedure FillRow(Index: Integer);
    procedure SetApplyEvent(Value: TApplyProc);
  public
    procedure DelayedCreate;
    procedure Load(AuditPoicy: IPerUserAudit);
    procedure LoadForSid(Sid: PSid);
    property OnApplyClick: TApplyProc read ApplyProc write SetApplyEvent;
  end;

implementation

uses
  Winapi.NtSecApi, Ntapi.ntstatus, UI.Colors, DelphiUtils.Strings;

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
var
  i: Integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
    ListView.Items[i].ColorEnabled := False;

  if Assigned(Policy) and Assigned(ApplyProc)then
    ApplyProc(Policy);
end;

procedure TFrameAudit.FillRow(Index: Integer);
const
  ColumnToFlag: array [1 .. 4] of Integer = (
    PER_USER_AUDIT_SUCCESS_INCLUDE, PER_USER_AUDIT_SUCCESS_EXCLUDE,
    PER_USER_AUDIT_FAILURE_INCLUDE, PER_USER_AUDIT_FAILURE_EXCLUDE
  );
var
  ColumnInd: Integer;
begin
  with ListView.Items[Index] do
    for ColumnInd := 1 to 4 do
      if Assigned(Policy) then
        Cell[ColumnInd] := CheckboxToString(Policy.ContainsFlag(Index,
          ColumnToFlag[ColumnInd]))
      else
        Cell[ColumnInd] := '?';
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

procedure TFrameAudit.Load(AuditPoicy: IPerUserAudit);
var
  Ind: Integer;
begin
  DelayedCreate;

  Policy := AuditPoicy;
  ButtonApply.Enabled := Assigned(Policy) and Assigned(ApplyProc);

  ListView.Items.BeginUpdate;

  for Ind := 0 to ListView.Items.Count - 1 do
  begin
    ListView.Items[Ind].ColorEnabled := False;
    FillRow(Ind);
  end;

  ListView.Items.EndUpdate;
end;

procedure TFrameAudit.LoadForSid(Sid: PSid);
var
  StatusEx: TNtxStatus;
begin
  Policy := nil;
  StatusEx := TPerUserAudit.CreateLoadForUser(Sid, Policy);

  if StatusEx.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsarQueryAuditPolicy') then
  begin
    LabelStatus.Caption := 'Audit policy is not defined for the account';
    LabelStatus.Hint := '';
    StatusEx := TPerUserAudit.CreateEmpty(Policy);
  end;

  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.MessageHint;
  end;

  Load(Policy);
end;

procedure TFrameAudit.ProcessPopupAction(Flag: Integer; NewState: Boolean);
var
  i: Integer;
begin
  if not Assigned(Policy) then
    Exit;

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

procedure TFrameAudit.SetApplyEvent(Value: TApplyProc);
begin
  ApplyProc := Value;
  ButtonApply.Enabled := Assigned(Policy) and Assigned(Value);
end;

procedure TFrameAudit.DelayedCreate;
var
  Ind, SubInd: Integer;
  StatusEx: TNtxStatus;
begin
  if Length(Categories) > 0 then
    Exit; // Already done

  StatusEx := LsaxEnumerateAuditCategiries(Categories, SubCategories);
  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.MessageHint;
    Exit;
  end;

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  // Each audit category is a group
  begin
    ListView.Groups.BeginUpdate;
    ListView.Groups.Clear;

    for Ind := 0 to High(Categories) do
      with ListView.Groups.Add do
      begin
        Header := LsaxLookupAuditCategoryName(Categories[Ind]);
        State := State + [lgsCollapsible];
      end;

    ListView.Groups.EndUpdate;
  end;

  // Each subcategory is and item
  for Ind := 0 to High(SubCategories) do
    for SubInd := 0 to High(SubCategories[Ind]) do
      with ListView.Items.Add do
      begin
        Caption := LsaxLookupAuditSubCategoryName(SubCategories[Ind, SubInd]);
        GroupID := Ind;
      end;

  ListView.Items.EndUpdate;
end;

end.
