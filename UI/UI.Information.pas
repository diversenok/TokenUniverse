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
    ComboPrimary: TComboBox;
    StaticPrimary: TStaticText;
    StaticUIAccess: TStaticText;
    ComboUIAccess: TComboBox;
    StaticText1: TStaticText;
    ListViewGeneral: TListViewEx;
    CheckBoxNoWriteUp: TCheckBox;
    CheckBoxNewProcessMin: TCheckBox;
    StaticVirtualization: TStaticText;
    CheckBoxVAllowed: TCheckBox;
    CheckBoxVEnabled: TCheckBox;
    BtnSetSession: TButton;
    BtnSetIntegrity: TButton;
    BtnSetUIAccess: TButton;
    BtnSetPolicy: TButton;
    BtnSetOwner: TButton;
    BtnSetPrimary: TButton;
    BtnSetVEnabled: TButton;
    BtnSetAEnabled: TButton;
    TabObject: TTabSheet;
    ListViewProcesses: TListViewEx;
    ListViewObject: TListViewEx;
    TabSecurity: TTabSheet;
    TabDefaultDacl: TTabSheet;
    TabAudit: TTabSheet;
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
    procedure BtnSetPolicyClick(Sender: TObject);
    procedure ListViewAdvancedResize(Sender: TObject);
    procedure BtnSetPrimaryClick(Sender: TObject);
    procedure BtnSetOwnerClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure BtnSetVEnabledClick(Sender: TObject);
    procedure BtnSetVAllowedClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
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
    procedure ChangedGroups(NewGroups: TGroupArray);
    procedure ChangedStatistics(NewStatistics: TTokenStatistics);
    procedure ChangedOwner(NewOwner: TSecurityIdentifier);
    procedure ChangedPrimaryGroup(NewPrimary: TSecurityIdentifier);
    procedure ChangedVAllowed(NewVAllowed: LongBool);
    procedure ChangedVEnabled(NewVEnabled: LongBool);
    procedure Refresh(Force: Boolean);
    procedure UpdateObjectTab;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Colors, TU.LsaApi, TU.Handles, UI.ProcessList,
  TU.Processes, UI.Settings;

const
  TAB_INVALIDATED = 0;
  TAB_UPDATED = 1;

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

procedure TInfoDialog.BtnSetPolicyClick(Sender: TObject);
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

procedure TInfoDialog.BtnSetVAllowedClick(Sender: TObject);
begin
  try
    Token.InfoClass.VirtualizationAllowed := CheckBoxVAllowed.Checked;
    CheckBoxVAllowed.Font.Style := [];
  except
    if Token.InfoClass.Query(tdTokenVirtualizationAllowed) then
      ChangedVAllowed(Token.InfoClass.VirtualizationAllowed);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetVEnabledClick(Sender: TObject);
begin
  try
    Token.InfoClass.VirtualizationEnabled := CheckBoxVEnabled.Checked;
    CheckBoxVEnabled.Font.Style := [];
  except
    if Token.InfoClass.Query(tdTokenVirtualizationEnabled) then
      ChangedVEnabled(Token.InfoClass.VirtualizationEnabled);
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
  ComboIntegrity.Hint := TGroupsSource.BuildHint(NewIntegrity.Group);
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

  TabGroups.Caption := Format('Groups (%d)', [NewStatistics.GroupCount]);
  TabPrivileges.Caption := Format('Privileges (%d)',
    [NewStatistics.PrivilegeCount]);
end;

procedure TInfoDialog.ChangedUIAccess(NewUIAccess: LongBool);
begin
  ComboUIAccess.Color := clWhite;
  ComboUIAccess.ItemIndex := Integer(NewUIAccess = True);
end;

procedure TInfoDialog.ChangedVAllowed(NewVAllowed: LongBool);
begin
  CheckBoxVAllowed.OnClick := nil;
  CheckBoxVAllowed.Font.Style := [];
  CheckBoxVAllowed.Checked := NewVAllowed;
  CheckBoxVAllowed.OnClick := CheckBoxClick;
end;

procedure TInfoDialog.ChangedVEnabled(NewVEnabled: LongBool);
begin
  CheckBoxVEnabled.OnClick := nil;
  CheckBoxVEnabled.Font.Style := [];
  CheckBoxVEnabled.Checked := NewVEnabled;
  CheckBoxVEnabled.OnClick := CheckBoxClick;
end;

procedure TInfoDialog.CheckBoxClick(Sender: TObject);
begin
  Assert(Sender is TCheckBox);
  (Sender as TCheckBox).Font.Style := [fsBold];
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
  Token.Events.OnVirtualizationEnabledChange.Delete(ChangedVEnabled);
  Token.Events.OnVirtualizationAllowedChange.Delete(ChangedVAllowed);
  Token.Events.OnPrimaryChange.Delete(ChangedPrimaryGroup);
  Token.Events.OnOwnerChange.Delete(ChangedOwner);
  Token.Events.OnStatisticsChange.Delete(ChangedStatistics);
  Token.Events.OnGroupsChange.Delete(ChangedGroups);
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
  // The token should not be destroyed until the dialog is closed
  SubscribeTokenCanClose(Token, Caption);

  // Manage the dialog caption
  ChangedCaption(Token.Caption);
  Token.OnCaptionChange.Add(ChangedCaption);

  // Create objects that control UI components which dynamically
  // display token-related information
  SessionSource := TSessionSource.Create(ComboSession, False);
  IntegritySource := TIntegritySource.Create(ComboIntegrity);
  PrivilegesSource := TPrivilegesSource.Create(ListViewPrivileges);
  GroupsSource := TGroupsSource.Create(ListViewGroups);
  RestrictedSIDsSource := TGroupsSource.Create(ListViewRestricted);

  // Query every changable info class to make sure it is in the cache for
  // future use.
  Refresh(False);

  // Subscribe event listeners. Since the information is already queried inside
  // Refresh it also calls our event listeners with the latest availible data.
  // By doing this in such order we avoid multiple calls while sharing the data
  // between different tokens pointing the same kernel object.
  Token.Events.OnSessionChange.Add(ChangedSession);
  Token.Events.OnUIAccessChange.Add(ChangedUIAccess);
  Token.Events.OnIntegrityChange.Add(ChangedIntegrity);
  Token.Events.OnPolicyChange.Add(ChangedPolicy);
  Token.Events.OnGroupsChange.Add(ChangedGroups);
  Token.Events.OnStatisticsChange.Add(ChangedStatistics);
  Token.Events.OnOwnerChange.Add(ChangedOwner);
  Token.Events.OnPrimaryChange.Add(ChangedPrimaryGroup);
  Token.Events.OnVirtualizationAllowedChange.Add(ChangedVAllowed);
  Token.Events.OnVirtualizationEnabledChange.Add(ChangedVEnabled);
  PrivilegesSource.SubscribeToken(Token);
  GroupsSource.SubscribeToken(Token, gsGroups);
  RestrictedSIDsSource.SubscribeToken(Token, gsRestrictedSIDs);
end;

procedure TInfoDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    Refresh(True);
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

procedure TInfoDialog.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePageIndex = TabObject.TabIndex then
    UpdateObjectTab;
end;

procedure TInfoDialog.Refresh(Force: Boolean);
begin
  if Force or TSettings.ForceUpdateOnInfoDialog then
    Token.InfoClass.InvalidateAll;

  ListViewGeneral.Items.BeginUpdate;
  with ListViewGeneral do
  begin
    Items[0].SubItems[0] := Token.InfoClass.QueryString(tsObjectAddress);
    ListViewObject.Items[0].SubItems[0] := Items[0].SubItems[0];
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

  // This triggers event if the value has changed
  Token.InfoClass.Query(tdTokenIntegrity);
  Token.InfoClass.Query(tdTokenSessionId);
  Token.InfoClass.Query(tdTokenUIAccess);
  Token.InfoClass.Query(tdTokenMandatoryPolicy);
  Token.InfoClass.Query(tdTokenGroups);
  Token.InfoClass.Query(tdTokenStatistics);
  Token.InfoClass.Query(tdTokenOwner);
  Token.InfoClass.Query(tdTokenPrimaryGroup);
  Token.InfoClass.Query(tdTokenVirtualizationAllowed);
  Token.InfoClass.Query(tdTokenVirtualizationEnabled);

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

  if Token.InfoClass.Query(tdTokenRestrictedSids) then
    TabRestricted.Caption := Format('Restricting SIDs (%d)',
      [Length(Token.InfoClass.RestrictedSids)]);

  TabObject.Tag := TAB_INVALIDATED;
  PageControlChange(Self);
end;

procedure TInfoDialog.SetStaleColor(Sender: TObject);
begin
  Assert(Sender is TComboBox);
  (Sender as TComboBox).Color := clStale;
end;

procedure TInfoDialog.UpdateObjectTab;
var
  Handles: THandleList;
  DoSnapshotProcesses: Boolean;
  Processes: TProcessList;
  i: Integer;
begin
  if TabObject.Tag = TAB_UPDATED then
    Exit;

  // Update basic object information
  if Token.InfoClass.Query(tdObjectInfo) then
    with ListViewObject, Token.InfoClass.ObjectInformation do
    begin
      Items[1].SubItems[0] := ObjectAttributesToString(Attributes);
      Items[2].SubItems[0] := BytesToString(PagedPoolCharge);
      Items[3].SubItems[0] := BytesToString(NonPagedPoolCharge);
      Items[4].SubItems[0] := IntToStr(PointerCount);
      Items[5].SubItems[0] := IntToStr(HandleCount);
    end;

  ListViewProcesses.Items.BeginUpdate;
  ListViewProcesses.Items.Clear;
  ListViewProcesses.SmallImages := TProcessIcons.ImageList;
  Handles := THandleList.Create;

  DoSnapshotProcesses := False;

  // Add current process
  for i := 0 to Handles.Count - 1 do
    if (Handles[i].KernelObjectAddress =
      Token.HandleInformation.KernelObjectAddress) then
    begin
      // Add handles from current process
      if Handles[i].ContextPID = GetCurrentProcessId then
        with ListViewProcesses.Items.Add do
        begin
          Caption := 'Current process';
          SubItems.Add(IntToStr(GetCurrentProcessId));
          SubItems.Add(Format('0x%x', [Handles[i].Handle]));
          SubItems.Add(AccessToString(Handles[i].Access));
          ImageIndex := TProcessIcons.GetIcon(ParamStr(0));
        end
      else
        DoSnapshotProcesses := True;
    end;

  // Add handles from other processes
  if DoSnapshotProcesses then
  begin
    Processes := TProcessList.Create;

    for i := 0 to Handles.Count - 1 do
    if (Handles[i].KernelObjectAddress =
      Token.HandleInformation.KernelObjectAddress)
      and (Handles[i].ContextPID <> GetCurrentProcessId) then
      with ListViewProcesses.Items.Add do
      begin
        Caption := Processes.FindName(Handles[i].ContextPID);
        SubItems.Add(IntToStr(Handles[i].ContextPID));
        SubItems.Add(Format('0x%x', [Handles[i].Handle]));
        SubItems.Add(AccessToString(Handles[i].Access));
        ImageIndex := TProcessIcons.GetIcon(TProcessItem.QueryFullName(
          Handles[i].ContextPID));
      end;

    Processes.Free;
  end;

  Handles.Free;
  ListViewProcesses.Items.EndUpdate;

  TabObject.Tag := TAB_UPDATED;
end;

end.
