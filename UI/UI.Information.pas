unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens, System.ImageList, Vcl.ImgList,
  UI.ListViewEx, UI.Prototypes, UI.Prototypes.ChildForm, TU.Common, TU.WtsApi,
  TU.Tokens.Types;

type
  TInfoDialog = class(TChildTaskbarForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    StaticUser: TStaticText;
    EditUser: TEdit;
    ButtonClose: TButton;
    ListViewGroups: TListViewEx;
    ListViewPrivileges: TListViewEx;
    TabRestricted: TTabSheet;
    ListViewRestricted: TListViewEx;
    StaticSession: TStaticText;
    StaticIntegrity: TStaticText;
    ComboSession: TComboBox;
    ComboIntegrity: TComboBox;
    ImageList: TImageList;
    BtnSetIntegrity: TSpeedButton;
    BtnSetSession: TSpeedButton;
    PrivilegePopup: TPopupMenu;
    MenuPrivEnable: TMenuItem;
    MenuPrivDisable: TMenuItem;
    MenuPrivRemove: TMenuItem;
    GroupPopup: TPopupMenu;
    MenuGroupEnable: TMenuItem;
    MenuGroupDisable: TMenuItem;
    MenuGroupReset: TMenuItem;
    TabAdvanced: TTabSheet;
    ListViewAdvanced: TListViewEx;
    StaticOwner: TStaticText;
    ComboOwner: TComboBox;
    BtnSetOwner: TSpeedButton;
    BtnSetPrimary: TSpeedButton;
    ComboPrimary: TComboBox;
    StaticPrimary: TStaticText;
    StaticUIAccess: TStaticText;
    ComboUIAccess: TComboBox;
    BtnSetUIAccess: TSpeedButton;
    StaticText1: TStaticText;
    SpeedButton1: TSpeedButton;
    ListViewGeneral: TListViewEx;
    CheckBoxNoWriteUp: TCheckBox;
    CheckBoxNewProcessMin: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnSetIntegrityClick(Sender: TObject);
    procedure BtnSetSessionClick(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetStaleColor(Sender: TObject);
    procedure ActionPrivilegeEnable(Sender: TObject);
    procedure ActionPrivilegeDisable(Sender: TObject);
    procedure ActionPrivilegeRemove(Sender: TObject);
    procedure ActionGroupEnable(Sender: TObject);
    procedure ActionGroupDisable(Sender: TObject);
    procedure ActionGroupReset(Sender: TObject);
    procedure ListViewGroupsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure BtnSetUIAccessClick(Sender: TObject);
    procedure BtnSetMandatoryPolicy(Sender: TObject);
    procedure ListViewAdvancedResize(Sender: TObject);
    procedure BtnSetPrimaryClick(Sender: TObject);
    procedure BtnSetOwnerClick(Sender: TObject);
    procedure CheckBoxNoWriteUpClick(Sender: TObject);
    procedure CheckBoxNewProcessMinClick(Sender: TObject);
  private
    Token: TToken;
    SessionSource: TSessionSource;
    IntegritySource: TIntegritySource;
    PrivilegesSource: TPrivilegesSource;
    GroupsSource, RestrictedSIDsSource: TGroupsSource;
    procedure ChangedCaption(NewCaption: String);
    procedure ChangedIntegrity(NewIntegrity: TTokenIntegrity);
    procedure ChangedSession(NewSession: Cardinal);
    procedure ChangedUIAccess(NewUIAccess: LongBool);
    procedure ChangedPolicy(NewPolicy: TMandatoryPolicy);
    procedure ChangedPrivileges(NewPrivileges: TPrivilegeArray);
    procedure ChangedGroups(NewGroups: TGroupArray);
    procedure ChangedStatistics(NewStatistics: TTokenStatistics);
    procedure ChangedOwner(NewOwner: TSecurityIdentifier);
    procedure ChangedPrimaryGroup(NewPrimary: TSecurityIdentifier);
    procedure Refresh;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Colors, TU.LsaApi;

{$R *.dfm}

procedure TInfoDialog.ActionGroupDisable(Sender: TObject);
begin
  if ListViewGroups.SelCount <> 0 then
    Token.GroupAdjust(GroupsSource.SelectedGroups, gaDisable);
end;

procedure TInfoDialog.ActionGroupEnable(Sender: TObject);
begin
  if ListViewGroups.SelCount <> 0 then
    Token.GroupAdjust(GroupsSource.SelectedGroups, gaEnable);
end;

procedure TInfoDialog.ActionGroupReset(Sender: TObject);
begin
  if ListViewGroups.SelCount <> 0 then
    Token.GroupAdjust(GroupsSource.SelectedGroups, gaResetDefault);
end;

procedure TInfoDialog.ActionPrivilegeDisable(Sender: TObject);
begin
  if ListViewPrivileges.SelCount <> 0 then
    Token.PrivilegeAdjust(PrivilegesSource.SelectedPrivileges, paDisable);
end;

procedure TInfoDialog.ActionPrivilegeEnable(Sender: TObject);
begin
  if ListViewPrivileges.SelCount <> 0 then
    Token.PrivilegeAdjust(PrivilegesSource.SelectedPrivileges, paEnable);
end;

procedure TInfoDialog.ActionPrivilegeRemove(Sender: TObject);
begin
  if ListViewPrivileges.SelCount <> 0 then
    Token.PrivilegeAdjust(PrivilegesSource.SelectedPrivileges, paRemove);
end;

procedure TInfoDialog.BtnSetIntegrityClick(Sender: TObject);
begin
  try
    Token.InfoClass.IntegrityLevel := IntegritySource.SelectedIntegrity;
    ComboIntegrity.Color := clWindow;
  except
    if Token.InfoClass.Query(tdTokenIntegrity) then
      ChangedIntegrity(Token.InfoClass.Integrity);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetMandatoryPolicy(Sender: TObject);
var
  Policy: TMandatoryPolicy;
begin
  try
    Policy := MandatoryPolicyOff;
    if CheckBoxNoWriteUp.Checked then
      Policy := Policy or MandatoryPolicyNoWriteUp;
    if CheckBoxNewProcessMin.Checked then
      Policy := Policy or MandatoryPolicyNewProcessMin;

    Token.InfoClass.MandatoryPolicy := Policy;

    CheckBoxNoWriteUp.Font.Style := [];
    CheckBoxNewProcessMin.Font.Style := [];
  except
    if Token.InfoClass.Query(tdTokenMandatoryPolicy) then
      ChangedPolicy(Token.InfoClass.MandatoryPolicy);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetOwnerClick(Sender: TObject);
begin
  try
    Token.InfoClass.Owner := TSecurityIdentifier.CreateFromString(
      ComboOwner.Text);
    ComboOwner.Color := clWindow;
  except
    if Token.InfoClass.Query(tdTokenOwner) then
      ChangedOwner(Token.InfoClass.Owner);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetPrimaryClick(Sender: TObject);
begin
  try
    Token.InfoClass.PrimaryGroup := TSecurityIdentifier.CreateFromString(
      ComboPrimary.Text);
    ComboPrimary.Color := clWindow;
  except
    if Token.InfoClass.Query(tdTokenPrimaryGroup) then
      ChangedPrimaryGroup(Token.InfoClass.PrimaryGroup);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetSessionClick(Sender: TObject);
begin
  try
    Token.InfoClass.Session := SessionSource.SelectedSession;
  except
    if Token.InfoClass.Query(tdTokenSessionId) then
      ChangedSession(Token.InfoClass.Session);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetUIAccessClick(Sender: TObject);
begin
  try
    if ComboUIAccess.ItemIndex = -1 then
      Token.InfoClass.UIAccess := LongBool(StrToUIntEx(ComboUIAccess.Text,
        'UIAccess value'))
    else
      Token.InfoClass.UIAccess := LongBool(ComboUIAccess.ItemIndex);
    ComboUIAccess.Color := clWindow;
  except
    if Token.InfoClass.Query(tdTokenUIAccess) then
        ChangedUIAccess(Token.InfoClass.UIAccess);
    raise;
  end;
end;

procedure TInfoDialog.ChangedCaption(NewCaption: String);
begin
  Caption := Format('Token Information for "%s"', [NewCaption]);
end;

procedure TInfoDialog.ChangedGroups(NewGroups: TGroupArray);
var
  i: Integer;
begin
  TabGroups.Caption := Format('Groups (%d)', [Length(NewGroups)]);

  // Update suggestions for Owner and Primary Group
  ComboOwner.Items.BeginUpdate;
  ComboPrimary.Items.BeginUpdate;

  ComboOwner.Items.Clear;
  ComboPrimary.Items.Clear;

  // Add User since it is always assignable
  if Token.InfoClass.Query(tdTokenUser) then
  begin
    ComboOwner.Items.Add(Token.InfoClass.User.SecurityIdentifier.ToString);
    ComboPrimary.Items.Add(Token.InfoClass.User.SecurityIdentifier.ToString);
  end;

  // Add all groups for Primary Group and only those with specific attribtes
  // for Owner.
  for i := 0 to High(NewGroups) do
  begin
    ComboPrimary.Items.Add(NewGroups[i].SecurityIdentifier.ToString);
    if NewGroups[i].Attributes.Contain(GroupOwner) then
      ComboOwner.Items.Add(NewGroups[i].SecurityIdentifier.ToString);
  end;

  ComboPrimary.Items.EndUpdate;
  ComboOwner.Items.EndUpdate;
end;

procedure TInfoDialog.ChangedIntegrity(NewIntegrity: TTokenIntegrity);
begin
  ComboIntegrity.Color := clWindow;
  IntegritySource.SetIntegrity(NewIntegrity);
end;

procedure TInfoDialog.ChangedOwner(NewOwner: TSecurityIdentifier);
begin
  ComboOwner.Color := clWindow;
  ComboOwner.Text := NewOwner.ToString;
end;

procedure TInfoDialog.ChangedPolicy(NewPolicy: TMandatoryPolicy);
begin
  CheckBoxNoWriteUp.Checked := (NewPolicy and
    MandatoryPolicyNoWriteUp <> 0);
  CheckBoxNewProcessMin.Checked := (NewPolicy and
    MandatoryPolicyNewProcessMin <> 0);

  CheckBoxNoWriteUp.Font.Style := [];
  CheckBoxNewProcessMin.Font.Style := [];
end;

procedure TInfoDialog.ChangedPrimaryGroup(NewPrimary: TSecurityIdentifier);
begin
  ComboPrimary.Color := clWindow;
  ComboPrimary.Text := NewPrimary.ToString;
end;

procedure TInfoDialog.ChangedPrivileges(NewPrivileges: TPrivilegeArray);
begin
  TabPrivileges.Caption := Format('Privileges (%d)', [Length(NewPrivileges)]);
end;

procedure TInfoDialog.ChangedSession(NewSession: Cardinal);
begin
  ComboSession.Color := clWindow;
  SessionSource.SelectedSession := NewSession;
end;

procedure TInfoDialog.ChangedStatistics(NewStatistics: TTokenStatistics);
begin
  with ListViewAdvanced do
  begin
    Items[2].SubItems[0] := Token.InfoClass.QueryString(tsTokenID);
    Items[3].SubItems[0] := Token.InfoClass.QueryString(tsLogonID);
    Items[4].SubItems[0] := Token.InfoClass.QueryString(tsExprires);
    Items[5].SubItems[0] := Token.InfoClass.QueryString(tsDynamicCharged);
    Items[6].SubItems[0] := Token.InfoClass.QueryString(tsDynamicAvailable);
    Items[7].SubItems[0] := Token.InfoClass.QueryString(tsGroupCount);
    Items[8].SubItems[0] := Token.InfoClass.QueryString(tsPrivilegeCount);
    Items[9].SubItems[0] := Token.InfoClass.QueryString(tsModifiedID);

    Items[12].SubItems[0] := Token.InfoClass.QueryString(tsLogonID);
    Items[13].SubItems[0] := Token.InfoClass.QueryString(tsLogonUserName);

    if Token.InfoClass.Query(tdLogonInfo) and
      Token.InfoClass.LogonSessionInfo.UserPresent then
      Items[13].Hint := TGroupsSource.BuildHint(
        Token.InfoClass.LogonSessionInfo.User, TGroupAttributes(0), False);

    Items[14].SubItems[0] := Token.InfoClass.QueryString(tsLogonAuthPackage);
    Items[15].SubItems[0] := Token.InfoClass.QueryString(tsLogonServer);
    Items[16].SubItems[0] := Token.InfoClass.QueryString(tsLogonType);
    Items[17].SubItems[0] := Token.InfoClass.QueryString(tsLogonWtsSession);
    Items[18].SubItems[0] := Token.InfoClass.QueryString(tsLogonTime);

    // TODO: Error hints
  end;
end;

procedure TInfoDialog.ChangedUIAccess(NewUIAccess: LongBool);
begin
  ComboUIAccess.Color := clWhite;
  ComboUIAccess.ItemIndex := Integer(NewUIAccess = True);
end;

procedure TInfoDialog.CheckBoxNewProcessMinClick(Sender: TObject);
begin
  CheckBoxNewProcessMin.Font.Style := [fsBold];
end;

procedure TInfoDialog.CheckBoxNoWriteUpClick(Sender: TObject);
begin
  CheckBoxNoWriteUp.Font.Style := [fsBold];
end;

constructor TInfoDialog.CreateFromToken(AOwner: TComponent; SrcToken: TToken);
begin
  Assert(Assigned(SrcToken));
  Token := SrcToken;
  inherited Create(AOwner);
  Show;
end;

procedure TInfoDialog.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TInfoDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Token.Events.OnPrimaryChange.Delete(ChangedPrimaryGroup);
  Token.Events.OnOwnerChange.Delete(ChangedOwner);
  Token.Events.OnStatisticsChange.Delete(ChangedStatistics);
  Token.Events.OnGroupsChange.Delete(ChangedGroups);
  Token.Events.OnPrivilegesChange.Delete(ChangedPrivileges);
  Token.Events.OnPolicyChange.Delete(ChangedPolicy);
  Token.Events.OnIntegrityChange.Delete(ChangedIntegrity);
  Token.Events.OnUIAccessChange.Delete(ChangedUIAccess);
  Token.Events.OnSessionChange.Delete(ChangedSession);
  Token.OnCaptionChange.Delete(ChangedCaption);
  RestrictedSIDsSource.Free;
  GroupsSource.Free;
  PrivilegesSource.Free;
  IntegritySource.Free;
  SessionSource.Free;
  UnsubscribeTokenCanClose(Token);
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
begin
  SubscribeTokenCanClose(Token, Caption);
  SessionSource := TSessionSource.Create(ComboSession);
  IntegritySource := TIntegritySource.Create(ComboIntegrity);
  PrivilegesSource := TPrivilegesSource.Create(ListViewPrivileges);
  GroupsSource := TGroupsSource.Create(ListViewGroups);
  RestrictedSIDsSource := TGroupsSource.Create(ListViewRestricted);

  // "Refresh" queries all the information, stores changeble one in the event
  // handler, and distributes changed one to every existing event listener
  Refresh;

  // Than subscribtion calls our event listeners with the latest availible
  // information that is stored in the event handlers. By doing that in this
  // order we avoid multiple calls while sharing the data between different
  // tokens pointing the same kernel object.
  Token.Events.OnSessionChange.Add(ChangedSession);
  Token.Events.OnUIAccessChange.Add(ChangedUIAccess);
  Token.Events.OnIntegrityChange.Add(ChangedIntegrity);
  Token.Events.OnPolicyChange.Add(ChangedPolicy);
  Token.Events.OnPrivilegesChange.Add(ChangedPrivileges);
  Token.Events.OnGroupsChange.Add(ChangedGroups);
  Token.Events.OnStatisticsChange.Add(ChangedStatistics);
  Token.Events.OnOwnerChange.Add(ChangedOwner);
  Token.Events.OnPrimaryChange.Add(ChangedPrimaryGroup);
  PrivilegesSource.SubscribeToken(Token);
  GroupsSource.SubscribeToken(Token, gsGroups);
  RestrictedSIDsSource.SubscribeToken(Token, gsRestrictedSIDs);

  Token.OnCaptionChange.Add(ChangedCaption);
  Token.OnCaptionChange.Invoke(Token.Caption);

  TabRestricted.Caption := Format('Restricting SIDs (%d)',
    [ListViewRestricted.Items.Count]);
end;

procedure TInfoDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    Refresh;
end;

procedure TInfoDialog.ListViewAdvancedResize(Sender: TObject);
begin
  // HACK: designs-time AutoSize causes horizontal scrollbar to appear
  ListViewAdvanced.Columns[1].AutoSize := True;
  ListViewRestricted.OnResize := nil;
end;

procedure TInfoDialog.ListViewGroupsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  MenuGroupEnable.Visible := ListViewGroups.SelCount <> 0;
  MenuGroupDisable.Visible := ListViewGroups.SelCount <> 0;
end;

procedure TInfoDialog.Refresh;
begin
  SessionSource.RefreshSessionList(False);

  ListViewGeneral.Items.BeginUpdate;
  with ListViewGeneral do
  begin
    Items[0].SubItems[0] := Token.InfoClass.QueryString(tsObjectAddress);
    Items[1].SubItems[0] := Token.InfoClass.QueryString(tsHandle);
    Items[2].SubItems[0] := Token.InfoClass.QueryString(tsAccess, True);
    Items[3].SubItems[0] := Token.InfoClass.QueryString(tsTokenType);
    Items[4].SubItems[0] := Token.InfoClass.QueryString(tsElevation);
  end;
  ListViewGeneral.Items.EndUpdate;

  ListViewAdvanced.Items.BeginUpdate;
  with ListViewAdvanced do
  begin
    Items[0].SubItems[0] := Token.InfoClass.QueryString(tsSourceName);
    Items[1].SubItems[0] := Token.InfoClass.QueryString(tsSourceLUID);
    Items[10].SubItems[0] := Token.InfoClass.QueryString(tsSandboxInert);
    Items[11].SubItems[0] := Token.InfoClass.QueryString(tsHasRestrictions);
  end;
  ListViewAdvanced.Items.EndUpdate;

  // This triggers InfoClass if the value has changed
  Token.InfoClass.ReQuery(tdTokenIntegrity);
  Token.InfoClass.ReQuery(tdTokenSessionId);
  Token.InfoClass.ReQuery(tdTokenUIAccess);
  Token.InfoClass.ReQuery(tdTokenMandatoryPolicy);
  Token.InfoClass.ReQuery(tdTokenPrivileges);
  Token.InfoClass.ReQuery(tdTokenGroups);
  Token.InfoClass.ReQuery(tdTokenStatistics);
  Token.InfoClass.ReQuery(tdTokenOwner);
  Token.InfoClass.ReQuery(tdTokenPrimaryGroup);

  if Token.InfoClass.Query(tdTokenUser) then
    with Token.InfoClass.User, EditUser do
    begin
      Text := SecurityIdentifier.ToString;
      Hint := TGroupsSource.BuildHint(SecurityIdentifier,
        Attributes);

      if Attributes.Contain(GroupUforDenyOnly) then
        Color := clDisabled
      else
        Color := clEnabled;
    end;
end;

procedure TInfoDialog.SetStaleColor(Sender: TObject);
begin
  Assert(Sender is TComboBox);
  (Sender as TComboBox).Color := clStale;
end;

end.
