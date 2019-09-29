unit UI.Prototypes.AuditFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, VclEx.ListView, NtUtils.Lsa.Audit;

type
  TApplyProc = procedure (AuditPolicy: IAudit) of object;

  TFrameAudit = class(TFrame)
    ListView: TListViewEx;
    PopupPerUser: TPopupMenu;
    MenuIncSucc: TMenuItem;
    MenuExcSucc: TMenuItem;
    MenuIncFail: TMenuItem;
    MenuExcFail: TMenuItem;
    ButtonApply: TButton;
    LabelStatus: TLabel;
    PopupSystem: TPopupMenu;
    MenuSuccess: TMenuItem;
    MenuFailure: TMenuItem;
    procedure ListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure MenuIncSuccClick(Sender: TObject);
    procedure MenuExcSuccClick(Sender: TObject);
    procedure MenuIncFailClick(Sender: TObject);
    procedure MenuExcFailClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure MenuSuccessClick(Sender: TObject);
    procedure MenuFailureClick(Sender: TObject);
  private
    Policy: IAudit;
    ApplyProc: TApplyProc;
    procedure FillRow(Index: Integer);
    procedure SetApplyEvent(Value: TApplyProc);
  public
    procedure DelayedCreate;
    procedure Load(AuditPoicy: IAudit);
    procedure LoadForSid(Sid: PSid);
    procedure LoadForSystem;
    procedure ModifySelected(Flag: Integer; NewState: Boolean);
    property OnApplyClick: TApplyProc read ApplyProc write SetApplyEvent;
  end;

implementation

uses
  Winapi.NtSecApi, Ntapi.ntstatus, UI.Colors, DelphiUtils.Strings,
  NtUtils.Exceptions;

{$R *.dfm}

{ TFrameAudit }

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
  // Note: since
  // PER_USER_AUDIT_SUCCESS_INCLUDE = POLICY_AUDIT_EVENT_SUCCESS and
  // PER_USER_AUDIT_SUCCESS_EXCLUDE = POLICY_AUDIT_EVENT_FAILURE
  // this code works for system audit as well

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

  if ListView.PopupMenu = PopupPerUser then
  begin
    MenuIncSucc.Checked := False;
    MenuExcSucc.Checked := False;
    MenuIncFail.Checked := False;
    MenuExcFail.Checked := False;

    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].Selected then
      begin
        if Policy.ContainsFlag(i, PER_USER_AUDIT_SUCCESS_INCLUDE) then
          MenuIncSucc.Checked := True;

        if Policy.ContainsFlag(i, PER_USER_AUDIT_SUCCESS_EXCLUDE) then
          MenuExcSucc.Checked := True;

        if Policy.ContainsFlag(i, PER_USER_AUDIT_FAILURE_INCLUDE) then
          MenuIncFail.Checked := True;

        if Policy.ContainsFlag(i, PER_USER_AUDIT_FAILURE_EXCLUDE) then
          MenuExcFail.Checked := True;
      end;
  end
  else if ListView.PopupMenu = PopupSystem then
  begin
    MenuSuccess.Checked := False;
    MenuFailure.Checked := False;

    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].Selected then
      begin
        if Policy.ContainsFlag(i, POLICY_AUDIT_EVENT_SUCCESS) then
          MenuSuccess.Checked := True;

        if Policy.ContainsFlag(i, POLICY_AUDIT_EVENT_FAILURE) then
          MenuFailure.Checked := True;
      end;
  end;
end;

procedure TFrameAudit.Load(AuditPoicy: IAudit);
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
  Policy := TPerUserAudit.CreateLoadForUser(Sid, StatusEx);

  if StatusEx.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsarQueryAuditPolicy') then
  begin
    LabelStatus.Caption := 'Audit policy is not defined for the account';
    LabelStatus.Hint := '';
    Policy := TPerUserAudit.CreateEmpty(StatusEx);
  end;

  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.MessageHint;
  end;

  Load(Policy);
end;

procedure TFrameAudit.LoadForSystem;
var
  StatusEx: TNtxStatus;
begin
  Policy := TSystemAudit.CreateQuery(StatusEx);

  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.MessageHint;
  end;

  Load(Policy);
end;

procedure TFrameAudit.MenuExcFailClick(Sender: TObject);
begin
  ModifySelected(PER_USER_AUDIT_FAILURE_EXCLUDE, not MenuExcFail.Checked);
end;

procedure TFrameAudit.MenuExcSuccClick(Sender: TObject);
begin
  ModifySelected(PER_USER_AUDIT_SUCCESS_EXCLUDE, not MenuExcSucc.Checked);
end;

procedure TFrameAudit.MenuFailureClick(Sender: TObject);
begin
  ModifySelected(POLICY_AUDIT_EVENT_FAILURE, not MenuFailure.Checked);
end;

procedure TFrameAudit.MenuIncFailClick(Sender: TObject);
begin
  ModifySelected(PER_USER_AUDIT_FAILURE_INCLUDE, not MenuIncFail.Checked);
end;

procedure TFrameAudit.MenuIncSuccClick(Sender: TObject);
begin
  ModifySelected(PER_USER_AUDIT_SUCCESS_INCLUDE, not MenuIncSucc.Checked);
end;

procedure TFrameAudit.MenuSuccessClick(Sender: TObject);
begin
  ModifySelected(POLICY_AUDIT_EVENT_SUCCESS, not MenuSuccess.Checked);
end;

procedure TFrameAudit.ModifySelected(Flag: Integer; NewState: Boolean);
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
  i: Integer;
  StatusEx: TNtxStatus;
  SubCategories: TArray<TGuid>;
  Mapping: TAuditCategoryMapping;
begin
  if ListView.Items.Count > 0 then
    Exit; // Already done

  StatusEx := LsaxEnumerateAuditSubCategories(SubCategories);
  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.MessageHint;
    Exit;
  end;

  StatusEx := LsaxQueryAuditCategoryMapping(Mapping);
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

    for i := 0 to High(Mapping.Categories) do
      with ListView.Groups.Add do
      begin
        Header := LsaxLookupAuditCategoryName(Mapping.Categories[i]);
        State := State + [lgsCollapsible];
      end;

    ListView.Groups.EndUpdate;
  end;

  // Each subcategory is an item
  for i := 0 to High(SubCategories) do
    with ListView.Items.Add do
    begin
      Caption := LsaxLookupAuditSubCategoryName(SubCategories[i]);
      GroupID := Mapping.Find(SubCategories[i]);
    end;

  ListView.Items.EndUpdate;
end;

end.
