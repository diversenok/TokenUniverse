unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens, System.ImageList, Vcl.ImgList,
  UI.ListViewEx, UI.Prototypes, UI.Prototypes.ChildForm, NtUtils.Types,
  TU.Tokens.Types, Winapi.WinNt, UI.Prototypes.AuditFrame, UI.Prototypes.Logon,
  UI.Prototypes.Privileges, UI.Prototypes.Groups;

type
  TInfoDialog = class(TChildTaskbarForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    StaticUser: TStaticText;
    EditUser: TEdit;
    ButtonClose: TButton;
    TabRestricted: TTabSheet;
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
    FrameAudit: TFrameAudit;
    TabLogon: TTabSheet;
    FrameLogon: TFrameLogon;
    FramePrivileges: TFramePrivileges;
    FrameGroupSIDs: TFrameGroups;
    FrameRestrictSIDs: TFrameGroups;
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
    procedure ListViewGeneralDblClick(Sender: TObject);
  private
    Token: TToken;
    SessionSource: TSessionSource;
    IntegritySource: TIntegritySource;
    procedure ChangedCaption(NewCaption: String);
    procedure ChangedIntegrity(NewIntegrity: TTokenIntegrity);
    procedure ChangedSession(NewSession: Cardinal);
    procedure ChangedUIAccess(NewUIAccess: LongBool);
    procedure ChangedPolicy(NewPolicy: Cardinal);
    procedure ChangedPrivileges(NewPrivileges: TPrivilegeArray);
    procedure ChangedGroups(NewGroups: TGroupArray);
    procedure ChangedStatistics(NewStatistics: TTokenStatistics);
    procedure ChangedOwner(NewOwner: ISid);
    procedure ChangedPrimaryGroup(NewPrimary: ISid);
    procedure ChangedVAllowed(NewVAllowed: LongBool);
    procedure ChangedVEnabled(NewVEnabled: LongBool);
    procedure Refresh;
    procedure UpdateObjectTab;
    procedure UpdateAuditTab;
    procedure UpdateLogonTab;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Colors, TU.LsaApi, UI.ProcessList,
  UI.Information.Access, NtUtils.Processes, NtUtils.Handles,
  DelphiUtils.Strings, NtUtils.Strings, Ntapi.ntpsapi;

const
  TAB_INVALIDATED = 0;
  TAB_UPDATED = 1;

{$R *.dfm}

procedure TInfoDialog.ActionGroupDisable(Sender: TObject);
begin
  if FrameGroupSIDs.ListView.SelCount <> 0 then
    Token.GroupAdjust(FrameGroupSIDs.SelectedGroups, gaDisable);
end;

procedure TInfoDialog.ActionGroupEnable(Sender: TObject);
begin
  if FrameGroupSIDs.ListView.SelCount <> 0 then
    Token.GroupAdjust(FrameGroupSIDs.SelectedGroups, gaEnable);
end;

procedure TInfoDialog.ActionGroupReset(Sender: TObject);
begin
  if FrameGroupSIDs.ListView.SelCount <> 0 then
    Token.GroupAdjust(FrameGroupSIDs.SelectedGroups, gaResetDefault);
end;

procedure TInfoDialog.ActionPrivilegeDisable(Sender: TObject);
begin
  if FramePrivileges.ListView.SelCount <> 0 then
    Token.PrivilegeAdjust(FramePrivileges.SelectedPrivileges, paDisable);
end;

procedure TInfoDialog.ActionPrivilegeEnable(Sender: TObject);
begin
  if FramePrivileges.ListView.SelCount <> 0 then
    Token.PrivilegeAdjust(FramePrivileges.SelectedPrivileges, paEnable);
end;

procedure TInfoDialog.ActionPrivilegeRemove(Sender: TObject);
begin
  if FramePrivileges.ListView.SelCount = 0 then
    Exit;

  if TaskMessageDlg('Remove these privileges from the token?',
    'This action can''t be undone.', mtWarning, mbYesNo, -1) <> idYes then
    Exit;

  Token.PrivilegeAdjust(FramePrivileges.SelectedPrivileges, paRemove);
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
    Token.InfoClass.Owner := TSid.CreateFromString(
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
  Policy: Cardinal;
begin
  try
    Policy := 0;

    if CheckBoxNoWriteUp.Checked then
      Policy := Policy or TOKEN_MANDATORY_POLICY_NO_WRITE_UP;

    if CheckBoxNewProcessMin.Checked then
      Policy := Policy or TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

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
    Token.InfoClass.PrimaryGroup := TSid.CreateFromString(ComboPrimary.Text);
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
  TabGroups.Caption := Format('Groups (%d)', [Length(NewGroups)]);

  // Update group list
  with FrameGroupSIDs do
  begin
    ListView.Items.BeginUpdate(True);

    Clear;
    AddGroups(NewGroups);

    ListView.Items.EndUpdate(True);
  end;

  // Update suggestions for Owner and Primary Group
  ComboOwner.Items.BeginUpdate;
  ComboPrimary.Items.BeginUpdate;

  ComboOwner.Items.Clear;
  ComboPrimary.Items.Clear;

  // Add User since it is always assignable
  if Token.InfoClass.Query(tdTokenUser) then
  begin
    ComboOwner.Items.Add(Token.InfoClass.User.SecurityIdentifier.Lookup.FullName);
    ComboPrimary.Items.Add(Token.InfoClass.User.SecurityIdentifier.Lookup.FullName);
  end;

  // Add all groups for Primary Group and only those with specific attribtes
  // for Owner.
  for i := 0 to High(NewGroups) do
  begin
    ComboPrimary.Items.Add(NewGroups[i].SecurityIdentifier.Lookup.FullName);
    if Contains(NewGroups[i].Attributes, SE_GROUP_OWNER) then
      ComboOwner.Items.Add(NewGroups[i].SecurityIdentifier.Lookup.FullName);
  end;

  ComboPrimary.Items.EndUpdate;
  ComboOwner.Items.EndUpdate;
end;

procedure TInfoDialog.ChangedIntegrity(NewIntegrity: TTokenIntegrity);
begin
  ComboIntegrity.Color := clWindow;
  IntegritySource.SetIntegrity(NewIntegrity);
  ComboIntegrity.Hint := BuildSidHint(
    NewIntegrity.Group.SecurityIdentifier.Lookup, NewIntegrity.Group.Attributes);
end;

procedure TInfoDialog.ChangedOwner(NewOwner: ISid);
begin
  ComboOwner.Color := clWindow;
  ComboOwner.Text := NewOwner.Lookup.FullName;
end;

procedure TInfoDialog.ChangedPolicy(NewPolicy: Cardinal);
begin
  CheckBoxNoWriteUp.Checked := Contains(NewPolicy,
    TOKEN_MANDATORY_POLICY_NO_WRITE_UP);

  CheckBoxNewProcessMin.Checked := Contains(NewPolicy,
    TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN);

  CheckBoxNoWriteUp.Font.Style := [];
  CheckBoxNewProcessMin.Font.Style := [];
end;

procedure TInfoDialog.ChangedPrimaryGroup(NewPrimary: ISid);
begin
  ComboPrimary.Color := clWindow;
  ComboPrimary.Text := NewPrimary.Lookup.FullName;
end;

procedure TInfoDialog.ChangedPrivileges(NewPrivileges: TPrivilegeArray);
begin
  TabPrivileges.Caption := Format('Privileges (%d)', [Length(NewPrivileges)]);

  FramePrivileges.ListView.Items.BeginUpdate(True);

  FramePrivileges.Clear;
  FramePrivileges.AddPrivileges(NewPrivileges);

  FramePrivileges.ListView.Items.EndUpdate(True);
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
    // TODO: Error hints
  end;
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
  Token.Events.OnVirtualizationEnabledChange.Unsubscribe(ChangedVEnabled);
  Token.Events.OnVirtualizationAllowedChange.Unsubscribe(ChangedVAllowed);
  Token.Events.OnPrimaryChange.Unsubscribe(ChangedPrimaryGroup);
  Token.Events.OnOwnerChange.Unsubscribe(ChangedOwner);
  Token.Events.OnStatisticsChange.Unsubscribe(ChangedStatistics);
  Token.Events.OnGroupsChange.Unsubscribe(ChangedGroups);
  Token.Events.OnPrivilegesChange.Unsubscribe(ChangedPrivileges);
  Token.Events.OnPolicyChange.Unsubscribe(ChangedPolicy);
  Token.Events.OnIntegrityChange.Unsubscribe(ChangedIntegrity);
  Token.Events.OnUIAccessChange.Unsubscribe(ChangedUIAccess);
  Token.Events.OnSessionChange.Unsubscribe(ChangedSession);
  Token.OnCaptionChange.Unsubscribe(ChangedCaption);
  IntegritySource.Free;
  SessionSource.Free;
  UnsubscribeTokenCanClose(Token);
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
begin
  SubscribeTokenCanClose(Token, Caption);
  SessionSource := TSessionSource.Create(ComboSession, False);
  IntegritySource := TIntegritySource.Create(ComboIntegrity);

  // "Refresh" queries all the information, stores changeble one in the event
  // handler, and distributes changed one to every existing event listener
  Refresh;

  // Than subscribtion calls our event listeners with the latest availible
  // information that is stored in the event handlers. By doing that in this
  // order we avoid multiple calls while sharing the data between different
  // tokens pointing the same kernel object.
  Token.Events.OnSessionChange.Subscribe(ChangedSession);
  Token.Events.OnUIAccessChange.Subscribe(ChangedUIAccess);
  Token.Events.OnIntegrityChange.Subscribe(ChangedIntegrity);
  Token.Events.OnPolicyChange.Subscribe(ChangedPolicy);
  Token.Events.OnPrivilegesChange.Subscribe(ChangedPrivileges);
  Token.Events.OnGroupsChange.Subscribe(ChangedGroups);
  Token.Events.OnStatisticsChange.Subscribe(ChangedStatistics);
  Token.Events.OnOwnerChange.Subscribe(ChangedOwner);
  Token.Events.OnPrimaryChange.Subscribe(ChangedPrimaryGroup);
  Token.Events.OnVirtualizationAllowedChange.Subscribe(ChangedVAllowed);
  Token.Events.OnVirtualizationEnabledChange.Subscribe(ChangedVEnabled);

  Token.OnCaptionChange.Subscribe(ChangedCaption);
  Token.OnCaptionChange.Invoke(Token.Caption);

  TabRestricted.Caption := Format('Restricting SIDs (%d)',
    [FrameRestrictSIDs.ListView.Items.Count]);
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
end;

procedure TInfoDialog.ListViewGeneralDblClick(Sender: TObject);
begin
  if Assigned(ListViewGeneral.Selected) and
    (ListViewGeneral.Selected.Index = 2) then
    TDialogGrantedAccess.Execute(Self, Token.HandleInformation.GrantedAccess);
end;

procedure TInfoDialog.ListViewGroupsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  MenuGroupEnable.Visible := FrameGroupSIDs.ListView.SelCount <> 0;
  MenuGroupDisable.Visible := FrameGroupSIDs.ListView.SelCount <> 0;
end;

procedure TInfoDialog.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePageIndex = TabObject.TabIndex then
    UpdateObjectTab
  else if PageControl.ActivePageIndex = TabAudit.TabIndex then
    UpdateAuditTab
  else if PageControl.ActivePageIndex = TabLogon.TabIndex then
    UpdateLogonTab;
end;

procedure TInfoDialog.Refresh;
begin
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
    Items[12].SubItems[0] := Token.InfoClass.QueryString(tsIsRestricted);
  end;
  ListViewAdvanced.Items.EndUpdate;

  // This triggers events if the value has changed
  Token.InfoClass.ReQuery(tdTokenIntegrity);
  Token.InfoClass.ReQuery(tdTokenSessionId);
  Token.InfoClass.ReQuery(tdTokenOrigin);
  Token.InfoClass.ReQuery(tdTokenUIAccess);
  Token.InfoClass.ReQuery(tdTokenMandatoryPolicy);
  Token.InfoClass.ReQuery(tdTokenPrivileges);
  Token.InfoClass.ReQuery(tdTokenGroups);
  Token.InfoClass.ReQuery(tdTokenStatistics);
  Token.InfoClass.ReQuery(tdTokenOwner);
  Token.InfoClass.ReQuery(tdTokenPrimaryGroup);
  Token.InfoClass.ReQuery(tdTokenVirtualizationAllowed);
  Token.InfoClass.ReQuery(tdTokenVirtualizationEnabled);

  if Token.InfoClass.Query(tdTokenUser) then
    with Token.InfoClass.User, EditUser do
    begin
      Text := SecurityIdentifier.Lookup.FullName;
      Hint := BuildSidHint(SecurityIdentifier.Lookup, Attributes);

      if Contains(Attributes, SE_GROUP_USE_FOR_DENY_ONLY) then
        Color := clDisabled
      else
        Color := clEnabled;
    end;

  if Token.InfoClass.Query(tdTokenRestrictedSids) then
    with FrameRestrictSIDs do
    begin
      ListView.Items.BeginUpdate;
      Clear;
      AddGroups(Token.InfoClass.RestrictedSids);
      ListView.Items.EndUpdate;
    end;

  TabObject.Tag := TAB_INVALIDATED;
  TabAudit.Tag := TAB_INVALIDATED;
  PageControlChange(Self);
end;

procedure TInfoDialog.SetStaleColor(Sender: TObject);
begin
  Assert(Sender is TComboBox);
  (Sender as TComboBox).Color := clStale;
end;

procedure TInfoDialog.UpdateAuditTab;
begin
  if TabAudit.Tag = TAB_UPDATED then
    Exit;

  Token.InfoClass.ReQuery(tdTokenAuditPolicy);
  if not FrameAudit.Subscribed then
    FrameAudit.SubscribeToken(Token);

  TabAudit.Tag := TAB_UPDATED;
end;

procedure TInfoDialog.UpdateLogonTab;
begin
  if TabLogon.Tag = TAB_UPDATED then
    Exit;

  Token.InfoClass.ReQuery(tdTokenStatistics);
  Token.InfoClass.ReQuery(tdTokenOrigin);

  if not FrameLogon.Subscribed then
    FrameLogon.SubscribeToken(Token);

  TabLogon.Tag := TAB_UPDATED;
end;

procedure TInfoDialog.UpdateObjectTab;
var
  Handles: THandleInfoArray;
  OpenedSomewhereElse: Boolean;
  ProcSnapshot: TProcessSnapshot;
  Process: PProcessInfo;
  CreatorImageName: String;
  ObjectSnapshot: TObjectSnapshot;
  ObjInfo: PObjectInfo;
  i: Integer;
begin
  if TabObject.Tag = TAB_UPDATED then
    Exit;

  ProcSnapshot := nil;

  // Update basic object information
  if Token.InfoClass.ReQuery(tdObjectInfo) then
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

  // Snapshot handles and find the ones pointing to that object
  Handles := THandleSnapshot.OfObject(Token.HandleInformation.PObject);
  OpenedSomewhereElse := False;

  // Add handle from current process and check if there are any other
  for i := 0 to High(Handles) do
    if Handles[i].UniqueProcessId = NtCurrentProcessId then
      with ListViewProcesses.Items.Add do
      begin
        Caption := 'Current process';
        SubItems.Add(IntToStr(NtCurrentProcessId));
        SubItems.Add(IntToHexEx(Handles[i].HandleValue));
        SubItems.Add(AccessToString(Handles[i].GrantedAccess));
        ImageIndex := TProcessIcons.GetIcon(ParamStr(0));
      end
    else
      OpenedSomewhereElse := True;

  // Add handles from other processes
  if OpenedSomewhereElse then
  begin
    ProcSnapshot := TProcessSnapshot.Create;

    for i := 0 to High(Handles) do
    if (Handles[i].UniqueProcessId <> NtCurrentProcessId) then
      with ListViewProcesses.Items.Add do
      begin
        Process := ProcSnapshot.FindByPID(Handles[i].UniqueProcessId);
        Caption := Process.GetImageName;
        SubItems.Add(IntToStr(Handles[i].UniqueProcessId));
        SubItems.Add(IntToHexEx(Handles[i].HandleValue));
        SubItems.Add(AccessToString(Handles[i].GrantedAccess));
        ImageIndex := TProcessIcons.GetIcon(Process.QueryFullImageName);
      end;
  end;

  // Obtain object creator by snapshotting objects on the system
  with ListViewObject.Items[6] do
    if TObjectSnapshot.FeatureSupported then
    begin
      ObjectSnapshot := TObjectSnapshot.Create;

      ObjInfo := ObjectSnapshot.FindObject(objToken,
        Token.HandleInformation.PObject);

      // Determine the cteator
      if not ObjectSnapshot.IsSuccessful then
        SubItems[0] := 'Unknown'
      else if Assigned(ObjInfo) then
      begin
        if ObjInfo.CreatorUniqueProcess = NtCurrentProcessId then
           CreatorImageName := 'Current process'
        else
        begin
          // The creator is somone else, we need to snapshot processes
          // if it's not done already.
          if not Assigned(ProcSnapshot) then
            ProcSnapshot := TProcessSnapshot.Create;

          Process := ProcSnapshot.FindByPID(ObjInfo.CreatorUniqueProcess);

          if Assigned(Process) then
          begin
            Hint := 'Since process IDs might be reused, ' +
                    'image name might be incorrect';
            CreatorImageName := Process.GetImageName;
          end
          else // Use default unknown name
            CreatorImageName := PProcessInfo(nil).GetImageName;
        end;

        SubItems[0] := Format('PID %d (%s)', [ObjInfo.CreatorUniqueProcess,
          CreatorImageName]);
      end
      else
        SubItems[0] := 'Kernel';

      ObjectSnapshot.Free;
    end
    else
      Hint := 'Enable global flag FLG_MAINTAIN_OBJECT_TYPELIST (0x4000).';

  if Assigned(ProcSnapshot) then
    ProcSnapshot.Free;

  ListViewProcesses.Items.EndUpdate;
  TabObject.Tag := TAB_UPDATED;
end;

end.
