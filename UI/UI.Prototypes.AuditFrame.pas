unit UI.Prototypes.AuditFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, VclEx.ListView, NtUtils.Lsa.Audit, NtUtils;

type
  TApplyProc = procedure (const AuditPolicy: TArray<TAuditPolicyEntry>) of object;

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
    AuditPolicy: TArray<TAuditPolicyEntry>;
    ApplyProc: TApplyProc;
    procedure FillRow(Index: Integer);
    procedure SetApplyEvent(Value: TApplyProc);
  public
    procedure DelayedCreate;
    procedure Load(const Policy: TArray<TAuditPolicyEntry>);
    procedure LoadForSid(const Sid: ISid);
    procedure LoadForSystem;
    procedure ModifySelected(Flag: Integer; NewState: Boolean);
    property OnApplyClick: TApplyProc read ApplyProc write SetApplyEvent;
  end;

implementation

uses
  Ntapi.NtSecApi, Ntapi.ntstatus, NtUiCommon.Colors,
  DelphiUiLib.Strings, NtUiLib.Errors, Ntapi.ntdef;

{$R *.dfm}

{ TFrameAudit }

procedure TFrameAudit.ButtonApplyClick;
var
  i: Integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
    ListView.Items[i].ColorEnabled := False;

  if Assigned(AuditPolicy) and Assigned(ApplyProc)then
    ApplyProc(AuditPolicy);
end;

procedure TFrameAudit.FillRow;
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
      if Assigned(AuditPolicy) then
        Cell[ColumnInd] := CheckboxToString(BitTest(AuditPolicy[Index].Policy
          and ColumnToFlag[ColumnInd]))
      else
        Cell[ColumnInd] := '?';
end;

procedure TFrameAudit.ListViewContextPopup;
var
  i: Integer;
begin
  if not Assigned(AuditPolicy) then
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
        if BitTest(AuditPolicy[i].PolicyOverride and
          PER_USER_AUDIT_SUCCESS_INCLUDE) then
          MenuIncSucc.Checked := True;

        if BitTest(AuditPolicy[i].PolicyOverride and
          PER_USER_AUDIT_SUCCESS_EXCLUDE) then
          MenuExcSucc.Checked := True;

        if BitTest(AuditPolicy[i].PolicyOverride and
          PER_USER_AUDIT_FAILURE_INCLUDE) then
          MenuIncFail.Checked := True;

        if BitTest(AuditPolicy[i].PolicyOverride and
          PER_USER_AUDIT_FAILURE_EXCLUDE) then
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
        if BitTest(AuditPolicy[i].Policy and POLICY_AUDIT_EVENT_SUCCESS) then
          MenuSuccess.Checked := True;

        if BitTest(AuditPolicy[i].Policy and POLICY_AUDIT_EVENT_FAILURE) then
          MenuFailure.Checked := True;
      end;
  end;
end;

procedure TFrameAudit.Load;
var
  Ind: Integer;
begin
  DelayedCreate;

  AuditPolicy := Policy;
  ButtonApply.Enabled := Assigned(AuditPolicy) and Assigned(ApplyProc);

  ListView.Items.BeginUpdate;

  for Ind := 0 to ListView.Items.Count - 1 do
  begin
    ListView.Items[Ind].ColorEnabled := False;
    FillRow(Ind);
  end;

  ListView.Items.EndUpdate;
end;

procedure TFrameAudit.LoadForSid;
var
  StatusEx: TNtxStatus;
begin
  AuditPolicy := nil;
  StatusEx := LsaxQueryUserAudit(Sid, AuditPolicy);

  if StatusEx.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'AuditQueryPerUserPolicy') then
  begin
    LabelStatus.Caption := 'Audit policy is not defined for the account';
    LabelStatus.Hint := '';
    StatusEx := LsaxCreateEmptyAudit(AuditPolicy);
  end;

  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.Description;
  end;

  Load(AuditPolicy);
end;

procedure TFrameAudit.LoadForSystem;
var
  StatusEx: TNtxStatus;
begin
  AuditPolicy := nil;
  StatusEx := LsaxQuerySystemAudit(AuditPolicy);

  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.Description;
  end;

  Load(AuditPolicy);
end;

procedure TFrameAudit.MenuExcFailClick;
begin
  ModifySelected(PER_USER_AUDIT_FAILURE_EXCLUDE, not MenuExcFail.Checked);
end;

procedure TFrameAudit.MenuExcSuccClick;
begin
  ModifySelected(PER_USER_AUDIT_SUCCESS_EXCLUDE, not MenuExcSucc.Checked);
end;

procedure TFrameAudit.MenuFailureClick;
begin
  ModifySelected(POLICY_AUDIT_EVENT_FAILURE, not MenuFailure.Checked);
end;

procedure TFrameAudit.MenuIncFailClick;
begin
  ModifySelected(PER_USER_AUDIT_FAILURE_INCLUDE, not MenuIncFail.Checked);
end;

procedure TFrameAudit.MenuIncSuccClick;
begin
  ModifySelected(PER_USER_AUDIT_SUCCESS_INCLUDE, not MenuIncSucc.Checked);
end;

procedure TFrameAudit.MenuSuccessClick;
begin
  ModifySelected(POLICY_AUDIT_EVENT_SUCCESS, not MenuSuccess.Checked);
end;

procedure TFrameAudit.ModifySelected;
var
  i: Integer;
begin
  if not Assigned(AuditPolicy) then
    Exit;

  ListView.Items.BeginUpdate;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      if NewState then
        AuditPolicy[i].PolicyOverride := (AuditPolicy[i].PolicyOverride and $0F)
          or Byte(Flag)
      else
        AuditPolicy[i].PolicyOverride := (AuditPolicy[i].PolicyOverride and $0F)
          and not Byte(Flag);

      FillRow(i);
      ListView.Items[i].Color := ColorSettings.clBackgroundUnsaved;
    end;
  ListView.Items.EndUpdate;
end;

procedure TFrameAudit.SetApplyEvent;
begin
  ApplyProc := Value;
  ButtonApply.Enabled := Assigned(AuditPolicy) and Assigned(Value);
end;

procedure TFrameAudit.DelayedCreate;
var
  i, j: Integer;
  StatusEx: TNtxStatus;
  Mapping: TArray<TAuditCategoryMapping>;
  Name: String;
begin
  if ListView.Items.Count > 0 then
    Exit; // Already done

  StatusEx := LsaxEnumerateAuditMapping(Mapping);
  if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.Description;
    Exit;
  end;

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  // Each audit category is a group
  ListView.Groups.BeginUpdate;
  ListView.Groups.Clear;

  for i := 0 to High(Mapping) do
  begin
    with ListView.Groups.Add do
    begin
      if not LsaxLookupAuditCategoryName(Mapping[i].Category,
        Name).IsSuccess then
        Name := Mapping[i].Category.ToString;

      Header := Name;
      State := State + [lgsCollapsible];
    end;

    for j := 0 to High(Mapping[i].SubCategories) do
      with ListView.Items.Add do
      begin
        if not LsaxLookupAuditSubCategoryName(Mapping[i].SubCategories[j],
          Name).IsSuccess then
          Name := Mapping[i].SubCategories[j].ToString;

        Caption := Name;
        GroupID := i;
      end;
  end;

  ListView.Groups.EndUpdate;
  ListView.Items.EndUpdate;
end;

end.
