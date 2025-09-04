unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList,
  VclEx.ListView, UI.Prototypes, NtUiCommon.Forms, NtUtils.Security.Sid,
  TU.Tokens.Old.Types, Ntapi.WinNt, UI.Prototypes.AuditFrame, UI.Prototypes.Logon,
  UI.Prototypes.Privileges, UI.Prototypes.Groups, NtUtils.Lsa.Audit,
  Ntapi.ntseapi, NtUtils, Vcl.ExtCtrls, TU.Tokens, NtUiFrame, NtUiFrame.Acl;

type
  TInfoDialog = class(TChildForm)
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
    TabDefaultDacl: TTabSheet;
    TabAudit: TTabSheet;
    FrameAudit: TFrameAudit;
    TabLogon: TTabSheet;
    FrameLogon: TFrameLogon;
    StaticAppContainer: TStaticText;
    EditAppContainer: TEdit;
    GroupsRestrictedFrame: TFrameGroups;
    GroupsMemberFrame: TFrameGroups;
    PrivilegesFrame: TFramePrivileges;
    PanelGeneral: TPanel;
    PanelObject: TPanel;
    DefaultDaclFrame: TAclFrame;
    btnDaclApply: TButton;
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
    procedure EditUserDblClick(Sender: TObject);
    procedure EditAppContainerDblClick(Sender: TObject);
    procedure btnDaclApplyClick(Sender: TObject);
  private
    Token: IToken;
    SessionSource: TSessionSource;
    IntegritySource: TIntegritySource;
    CaptionSubscription: IAutoReleasable;
    IntegritySubscription: IAutoReleasable;
    SessionSubscription: IAutoReleasable;
    UIAccessSubscription: IAutoReleasable;
    PolicySubscription: IAutoReleasable;
    PrivilegesSubscription: IAutoReleasable;
    GroupsSubscription: IAutoReleasable;
    StatisticsSubscription: IAutoReleasable;
    OwnerSubscription: IAutoReleasable;
    PrimaryGroupSubscription: IAutoReleasable;
    VAllowedSubscription: IAutoReleasable;
    VEnabledSubscription: IAutoReleasable;
    ElevationSubscription: IAutoReleasable;
    DefaultDaclSubscription: IAutoReleasable;
    FlagsSubscription: IAutoReleasable;
    procedure ChangedCaption(const InfoClass: TTokenStringClass; const NewCaption: String);
    procedure ChangedIntegrity(const Status: TNtxStatus; const NewIntegrity: TGroup);
    procedure ChangedSession(const Status: TNtxStatus; const NewSession: TSessionId);
    procedure ChangedUIAccess(const Status: TNtxStatus; const NewUIAccess: LongBool);
    procedure ChangedPolicy(const Status: TNtxStatus; const NewPolicy: TTokenMandatoryPolicy);
    procedure ChangedPrivileges(const Status: TNtxStatus; const NewPrivileges: TArray<TPrivilege>);
    procedure ChangedGroups(const Status: TNtxStatus; const NewGroups: TArray<TGroup>);
    procedure ChangedStatistics(const Status: TNtxStatus; const NewStatistics: TTokenStatistics);
    procedure ChangedOwner(const Status: TNtxStatus; const NewOwner: ISid);
    procedure ChangedPrimaryGroup(const Status: TNtxStatus; const NewPrimary: ISid);
    procedure ChangedVAllowed(const Status: TNtxStatus; const NewVAllowed: LongBool);
    procedure ChangedVEnabled(const Status: TNtxStatus; const NewVEnabled: LongBool);
    procedure ChangedElevation(const Status: TNtxStatus; const NewElevation: TTokenElevationInfo);
    procedure ChangedFlags(const Status: TNtxStatus; const NewFlags: TTokenFlags);
    procedure ChangedDefaultDacl(const Status: TNtxStatus; const NewDacl: IAcl);
    procedure SetAuditPolicy(const Audit: TArray<TAuditPolicyEntry>);
    procedure InspectGroup(const Group: TGroup);
    procedure Refresh;
    procedure UpdateObjectTab;
    procedure UpdateAuditTab;
    procedure UpdateLogonTab;
  public
    constructor CreateFromToken(AOwner: TComponent; const SrcToken: IToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, NtUiCommon.Colors, UI.ProcessList, Ntapi.ntstatus,
  UI.Sid.View, NtUtils.Objects.Snapshots, NtUiLib.Errors, DelphiUiLib.Strings,
  NtUtils.Security.AppContainer, NtUiCommon.Prototypes, Ntapi.ntpsapi,
  NtUtils.Processes, DelphiUiLib.Reflection, NtUtils.Profiles, NtUtils.Lsa.Sid,
  DelphiUtils.Arrays, NtUiCommon.Icons, Ntapi.Versions, Ntapi.ntobapi,
  NtUiBackend.AppContainers, NtUiLib.Exceptions, NtUtils.Security.Acl,
  NtUiCommon.Interfaces;

const
  TAB_INVALIDATED = 0;
  TAB_UPDATED = 1;

{$R *.dfm}

function GroupsToSids(const Groups: TArray<TGroup>): TArray<ISid>;
var
  i: Integer;
begin
  SetLength(Result, Length(Groups));

  for i := 0 to High(Groups) do
    Result[i] := Groups[i].Sid;
end;

procedure TInfoDialog.ActionGroupDisable;
begin
  if GroupsMemberFrame.VST.SelectedCount > 0 then
    Token.AdjustGroups(GroupsToSids(GroupsMemberFrame.Selected),
      SE_GROUP_DISABLED).RaiseOnError;
end;

procedure TInfoDialog.ActionGroupEnable;
begin
  if GroupsMemberFrame.VST.SelectedCount > 0 then
    Token.AdjustGroups(GroupsToSids(GroupsMemberFrame.Selected),
      SE_GROUP_ENABLED).RaiseOnError;
end;

procedure TInfoDialog.ActionGroupReset;
begin
  if GroupsMemberFrame.VST.SelectedCount > 0 then
    Token.AdjustGroupsReset.RaiseOnError;
end;

function PrivilegesToWellKnown(
  const Privileges: TArray<TPrivilege>
): TArray<TSeWellKnownPrivilege>;
var
  i: Integer;
begin
  SetLength(Result, Length(Privileges));

  for i := 0 to High(Privileges) do
    Result[i] := TSeWellKnownPrivilege(Privileges[i].Luid);
end;

procedure RaiseOnWarningOrError(const Status: TNtxStatus);
begin
  if Status.Status = STATUS_NOT_ALL_ASSIGNED then
    raise ENtError.Create(Status)
  else
    Status.RaiseOnError;
end;

procedure TInfoDialog.ActionPrivilegeDisable;
begin
  if PrivilegesFrame.VST.SelectedCount <> 0 then
    RaiseOnWarningOrError(Token.AdjustPrivileges(
      PrivilegesToWellKnown(PrivilegesFrame.Selected), SE_PRIVILEGE_DISABLED,
      True));
end;

procedure TInfoDialog.ActionPrivilegeEnable;
begin
  if PrivilegesFrame.VST.SelectedCount <> 0 then
    RaiseOnWarningOrError(Token.AdjustPrivileges(
      PrivilegesToWellKnown(PrivilegesFrame.Selected), SE_PRIVILEGE_ENABLED,
      True));
end;

procedure TInfoDialog.ActionPrivilegeRemove;
begin
  if PrivilegesFrame.VST.SelectedCount = 0 then
    Exit;

  if TaskMessageDlg('Remove these privileges from the token?',
    'This action can''t be undone.', mtWarning, mbYesNo, -1) <> idYes then
    Exit;

  RaiseOnWarningOrError(Token.AdjustPrivileges(
    PrivilegesToWellKnown(PrivilegesFrame.Selected), SE_PRIVILEGE_REMOVED,
    True));
end;

procedure TInfoDialog.btnDaclApplyClick;
var
  DefaultDacl: IAcl;
begin
  RtlxBuildAcl(DefaultDacl, DefaultDaclFrame.Aces).RaiseOnError;
  Token.SetDefaultDacl(DefaultDacl).RaiseOnError;
end;

procedure TInfoDialog.BtnSetIntegrityClick;
var
  Status: TNtxStatus;
begin
  Status := Token.SetIntegrity(IntegritySource.SelectedIntegrity);
  ComboIntegrity.Color := clWindow;

  if not Status.IsSuccess then
  begin
    IntegritySubscription := nil;
    IntegritySubscription := Token.ObserveIntegrity(ChangedIntegrity);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetOwnerClick;
var
  Sid: ISid;
  Status: TNtxStatus;
begin
  LsaxLookupNameOrSddl(ComboOwner.Text, Sid).RaiseOnError;
  Status := Token.SetOwner(Sid);
  ComboOwner.Color := clWindow;

  if not Status.IsSuccess then
  begin
    OwnerSubscription := nil;
    OwnerSubscription := Token.ObserveOwner(ChangedOwner);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetPolicyClick;
var
  Policy: TTokenMandatoryPolicy;
  Status: TNtxStatus;
begin
  Policy := 0;

  if CheckBoxNoWriteUp.Checked then
    Policy := Policy or TOKEN_MANDATORY_POLICY_NO_WRITE_UP;

  if CheckBoxNewProcessMin.Checked then
    Policy := Policy or TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  Status := Token.SetMandatoryPolicy(Policy);
  CheckBoxNoWriteUp.Font.Style := [];
  CheckBoxNewProcessMin.Font.Style := [];

  if not Status.IsSuccess then
  begin
    PolicySubscription := nil;
    PolicySubscription := Token.ObserveMandatoryPolicy(ChangedPolicy);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetPrimaryClick;
var
  Sid: ISid;
  Status: TNtxStatus;
begin
  LsaxLookupNameOrSddl(ComboPrimary.Text, Sid).RaiseOnError;
  Status := Token.SetPrimaryGroup(Sid);
  ComboPrimary.Color := clWindow;

  if not Status.IsSuccess then
  begin
    PrimaryGroupSubscription := nil;
    PrimaryGroupSubscription := Token.ObservePrimaryGroup(
      ChangedPrimaryGroup);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetSessionClick;
var
  Status: TNtxStatus;
begin
  Status := Token.SetSessionId(SessionSource.SelectedSession);

  if not Status.IsSuccess then
  begin
    SessionSubscription := nil;
    SessionSubscription := Token.ObserveSessionId(ChangedSession);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetUIAccessClick;
var
  UIAccess: LongBool;
  Status: TNtxStatus;
begin
  if ComboUIAccess.ItemIndex = -1 then
    UIAccess := LongBool(
      UiLibStringToUIntRaiseOnError(ComboUIAccess.Text, 'UIAccess value'))
  else
    UIAccess := LongBool(ComboUIAccess.ItemIndex);

  Status := Token.SetUIAccess(UIAccess);
  ComboUIAccess.Color := clWindow;

  if not Status.IsSuccess then
  begin
    UIAccessSubscription := nil;
    UIAccessSubscription := Token.ObserveUIAccess(ChangedUIAccess);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetVAllowedClick;
var
  Status: TNtxStatus;
begin
  Status := Token.SetVirtualizationAllowed(CheckBoxVAllowed.Checked);
  CheckBoxVAllowed.Font.Style := [];

  if not Status.IsSuccess then
  begin
    VAllowedSubscription := nil;
    VAllowedSubscription := Token.ObserveVirtualizationAllowed(
      ChangedVAllowed);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetVEnabledClick;
var
  Status: TNtxStatus;
begin
  Status := Token.SetVirtualizationEnabled(CheckBoxVEnabled.Checked);
  CheckBoxVEnabled.Font.Style := [];

  if not Status.IsSuccess then
  begin
    VEnabledSubscription := nil;
    VEnabledSubscription  := Token.ObserveVirtualizationEnabled(
      ChangedVEnabled);
  end;
end;

procedure TInfoDialog.ChangedCaption;
begin
  Caption := Format('Token Information for "%s"', [NewCaption]);
end;

procedure TInfoDialog.ChangedDefaultDacl;
var
  Result: TNtxStatus;
  Aces: TArray<TAceData>;
begin
  if Status.IsSuccess then
    Result := RtlxDumpAcl(NewDacl, Aces)
  else
    Result := Status;

  if Result.IsSuccess then
  begin
    DefaultDaclFrame.SetEmptyMessage('No items to display');
    DefaultDaclFrame.LoadAces(Aces, TypeInfo(TAccessMask),
      Default(TGenericMapping));
  end
  else
    DefaultDaclFrame.SetEmptyMessage('Unable to query'#$D#$A +
      Result.ToString);
end;

procedure TInfoDialog.ChangedElevation;
begin
  if Status.IsSuccess then
    ListViewGeneral.Items[4].SubItems[0] :=
      Token.QueryString(tsElevation);
end;

procedure TInfoDialog.ChangedFlags;
begin
  if Status.IsSuccess then
    ListViewAdvanced.Items[10].SubItems[0] := Token.QueryString(tsFlags);
end;

procedure TInfoDialog.ChangedGroups;
var
  i: Integer;
  User: TGroup;
  UserName: String;
begin
  if not Status.IsSuccess then
    Exit;

  TabGroups.Caption := Format('Groups (%d)', [Length(NewGroups)]);

  // Update group list
  GroupsMemberFrame.Load(NewGroups);

  // Update suggestions for Owner and Primary Group
  ComboOwner.Items.BeginUpdate;
  ComboPrimary.Items.BeginUpdate;

  ComboOwner.Items.Clear;
  ComboPrimary.Items.Clear;

  // Add User since it is always assignable
  if Token.QueryUser(User).IsSuccess then
  begin
    UserName := LsaxSidToString(User.Sid);
    ComboOwner.Items.Add(UserName);
    ComboPrimary.Items.Add(UserName);
  end;

  // Add all groups for Primary Group and only those with specific attributes
  // for Owner.
  for i := 0 to High(NewGroups) do
  begin
    ComboPrimary.Items.Add(LsaxSidToString(
      NewGroups[i].Sid));
    if NewGroups[i].Attributes and SE_GROUP_OWNER <> 0 then
      ComboOwner.Items.Add(LsaxSidToString(
        NewGroups[i].Sid));
  end;

  ComboPrimary.Items.EndUpdate;
  ComboOwner.Items.EndUpdate;
end;

procedure TInfoDialog.ChangedIntegrity;
begin
  if Status.IsSuccess then
  begin
    ComboIntegrity.Color := clWindow;
    IntegritySource.SelectedIntegrity := RtlxRidSid(NewIntegrity.Sid);
    ComboIntegrity.Hint := TType.Represent(NewIntegrity).Hint;
  end;
end;

procedure TInfoDialog.ChangedOwner;
begin
  if Status.IsSuccess then
  begin
    ComboOwner.Color := clWindow;
    ComboOwner.Text := LsaxSidToString(NewOwner);
  end;
end;

procedure TInfoDialog.ChangedPolicy;
begin
  if not Status.IsSuccess then
    Exit;

  CheckBoxNoWriteUp.Checked := NewPolicy and
    TOKEN_MANDATORY_POLICY_NO_WRITE_UP <> 0;

  CheckBoxNewProcessMin.Checked := NewPolicy and
    TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN <> 0;

  CheckBoxNoWriteUp.Font.Style := [];
  CheckBoxNewProcessMin.Font.Style := [];
end;

procedure TInfoDialog.ChangedPrimaryGroup;
begin
  if Status.IsSuccess then
  begin
    ComboPrimary.Color := clWindow;
    ComboPrimary.Text := LsaxSidToString(NewPrimary);
  end;
end;

procedure TInfoDialog.ChangedPrivileges;
begin
  if Status.IsSuccess then
  begin
    TabPrivileges.Caption := Format('Privileges (%d)', [Length(NewPrivileges)]);
    PrivilegesFrame.Load(NewPrivileges);
  end;
end;

procedure TInfoDialog.ChangedSession;
begin
  if Status.IsSuccess then
  begin
    ComboSession.Color := clWindow;
    SessionSource.SelectedSession := NewSession;
  end;
end;

procedure TInfoDialog.ChangedStatistics;
begin
  if Status.IsSuccess then
    with ListViewAdvanced do
    begin
      Items[2].SubItems[0] := Token.QueryString(tsTokenID);
      Items[3].SubItems[0] := Token.QueryString(tsLogonID);
      Items[4].SubItems[0] := Token.QueryString(tsExpires);
      Items[5].SubItems[0] := Token.QueryString(tsDynamicCharged);
      Items[6].SubItems[0] := Token.QueryString(tsDynamicAvailable);
      Items[7].SubItems[0] := Token.QueryString(tsGroups);
      Items[8].SubItems[0] := Token.QueryString(tsPrivileges);
      Items[9].SubItems[0] := Token.QueryString(tsModifiedID);
      // TODO: Error hints
    end;
end;

procedure TInfoDialog.ChangedUIAccess;
begin
  if Status.IsSuccess then
  begin
    ComboUIAccess.Color := clWhite;
    ComboUIAccess.ItemIndex := Integer(NewUIAccess = True);
  end;
end;

procedure TInfoDialog.ChangedVAllowed;
begin
  if Status.IsSuccess then
  begin
    CheckBoxVAllowed.OnClick := nil;
    CheckBoxVAllowed.Font.Style := [];
    CheckBoxVAllowed.Checked := NewVAllowed;
    CheckBoxVAllowed.OnClick := CheckBoxClick;
  end;
end;

procedure TInfoDialog.ChangedVEnabled;
begin
  if Status.IsSuccess then
  begin
    CheckBoxVEnabled.OnClick := nil;
    CheckBoxVEnabled.Font.Style := [];
    CheckBoxVEnabled.Checked := NewVEnabled;
    CheckBoxVEnabled.OnClick := CheckBoxClick;
  end;
end;

procedure TInfoDialog.CheckBoxClick;
begin
  Assert(Sender is TCheckBox);
  (Sender as TCheckBox).Font.Style := [fsBold];
end;

constructor TInfoDialog.CreateFromToken;
begin
  Assert(Assigned(SrcToken));
  Token := SrcToken;
  inherited Create(AOwner, cfmDesktop);

  GroupsRestrictedFrame.OnDefaultAction := InspectGroup;
  GroupsMemberFrame.OnDefaultAction := InspectGroup;
  Show;
end;

procedure TInfoDialog.DoCloseForm;
begin
  Close;
end;

procedure TInfoDialog.EditAppContainerDblClick;
var
  Info: TRtlxAppContainerInfo;
begin
  if Assigned(NtUiLibShowAppContainer) and
    Token.QueryAppContainerInfo(Info).IsSuccess then
    NtUiLibShowAppContainer(Info);
end;

procedure TInfoDialog.EditUserDblClick;
var
  User: TGroup;
begin
  if Token.QueryUser(User).IsSuccess then
    TDialogSidView.CreateView(FormMain, User.Sid);
end;

procedure TInfoDialog.FormClose;
begin
  IntegritySource.Free;
  SessionSource.Free;
end;

procedure TInfoDialog.FormCreate;
begin
  SessionSource := TSessionSource.Create(ComboSession, False);
  IntegritySource := TIntegritySource.Create(ComboIntegrity);
  FrameAudit.OnApplyClick := SetAuditPolicy;

  // "Refresh" queries all the information, stores changeable one in the event
  // handler, and distributes changed one to every existing event listener
  Refresh;

  IntegritySubscription := Token.ObserveIntegrity(ChangedIntegrity);
  SessionSubscription := Token.ObserveSessionId(ChangedSession);
  UIAccessSubscription := Token.ObserveUIAccess(ChangedUIAccess);
  PolicySubscription := Token.ObserveMandatoryPolicy(ChangedPolicy);
  PrivilegesSubscription := Token.ObservePrivileges(ChangedPrivileges);
  GroupsSubscription := Token.ObserveGroups(ChangedGroups);
  StatisticsSubscription := Token.ObserveStatistics(ChangedStatistics);
  OwnerSubscription := Token.ObserveOwner(ChangedOwner);
  PrimaryGroupSubscription := Token.ObservePrimaryGroup(ChangedPrimaryGroup);
  VAllowedSubscription := Token.ObserveVirtualizationAllowed(ChangedVAllowed);
  VEnabledSubscription := Token.ObserveVirtualizationEnabled(ChangedVEnabled);
  ElevationSubscription := Token.ObserveElevation(ChangedElevation);
  FlagsSubscription := Token.ObserveFlags(ChangedFlags);
  CaptionSubscription := Token.ObserveString(tsCaption, ChangedCaption);
  DefaultDaclSubscription := Token.ObserveDefaultDacl(ChangedDefaultDacl);

  TabRestricted.Caption := Format('Restricting SIDs (%d)',
    [GroupsRestrictedFrame.VST.RootNodeCount]);
end;

procedure TInfoDialog.FormKeyDown;
begin
  if Key = VK_F5 then
    Refresh;
end;

procedure TInfoDialog.InspectGroup;
begin
  TDialogSidView.CreateView(Self, Group.Sid);
end;

procedure TInfoDialog.ListViewAdvancedResize;
begin
  // HACK: designs-time AutoSize causes horizontal scrollbar to appear
  ListViewAdvanced.Columns[1].AutoSize := True;
end;

procedure TInfoDialog.ListViewGeneralDblClick;
var
  BasicInfo: TObjectBasicInformation;
begin
  if Assigned(NtUiLibShowAccessMask) and Assigned(ListViewGeneral.Selected) and
    (ListViewGeneral.Selected.Index = 2) and
    Token.QueryBasicInfo(BasicInfo).IsSuccess then
    NtUiLibShowAccessMask(BasicInfo.GrantedAccess, TypeInfo(TTokenAccessMask),
      TokenGenericMapping);
end;

procedure TInfoDialog.PageControlChange;
begin
  if PageControl.ActivePageIndex = TabObject.TabIndex then
    UpdateObjectTab
  else if PageControl.ActivePageIndex = TabAudit.TabIndex then
    UpdateAuditTab
  else if PageControl.ActivePageIndex = TabLogon.TabIndex then
    UpdateLogonTab;

  (DefaultDaclFrame as IObservesActivation).SetActive(
    PageControl.ActivePageIndex = TabDefaultDacl.TabIndex);
end;

procedure TInfoDialog.Refresh;
var
  Repr: TRepresentation;
  User: TGroup;
  Package: ISid;
  RestrictedSids: TArray<TGroup>;
  AppContainerInfo: TRtlxAppContainerInfo;
begin
  Token.SmartRefresh;

  ListViewGeneral.Items.BeginUpdate;
  with ListViewGeneral do
  begin
    Items[0].SubItems[0] := Token.QueryString(tsAddress);
    ListViewObject.Items[0].SubItems[0] := Items[0].SubItems[0];
    Items[1].SubItems[0] := Token.QueryString(tsHandle);
    Items[2].SubItems[0] := Token.QueryString(tsAccess, True);
    Items[3].SubItems[0] := Token.QueryString(tsType);
  end;
  ListViewGeneral.Items.EndUpdate;

  ListViewAdvanced.Items.BeginUpdate;
  with ListViewAdvanced do
  begin
    Items[0].SubItems[0] := Token.QueryString(tsSourceName);
    Items[1].SubItems[0] := Token.QueryString(tsSourceId);
  end;
  ListViewAdvanced.Items.EndUpdate;

  if Token.QueryUser(User).IsSuccess then
  begin
    // For user, 0 means default (enabled) state, but it can also
    // be use-for-deny-only.
    if User.Attributes = 0 then
      User.Attributes := SE_GROUP_ENABLED or SE_GROUP_ENABLED_BY_DEFAULT;

    Repr := TType.Represent(User);

    EditUser.Text := Repr.Text;
    EditUser.Hint := Repr.Hint;

    if User.Attributes and SE_GROUP_USE_FOR_DENY_ONLY <> 0 then
      EditUser.Color := ColorSettings.clBackgroundDeny
    else
      EditUser.Color := ColorSettings.clBackgroundAllow;

    // AppContainer is user-specific
    if not RtlOsVersionAtLeast(OsWin8) then
      EditAppContainer.Text := 'Not supported'
    else if Token.QueryAppContainerInfo(AppContainerInfo).IsSuccess then
    begin
      EditAppContainer.Text := AppContainerInfo.FriendlyName;
      EditAppContainer.Hint := AppContainerInfo.Hint;
    end
    else if Token.QueryAppContainerSid(Package).IsSuccess then
    begin
      if not Assigned(Package) then
        EditAppContainer.Text := 'N/A'
      else
        EditAppContainer.Text := RtlxSidToString(Package);
    end;
  end;

  if Token.QueryRestrictedSids(RestrictedSids).IsSuccess then
    GroupsRestrictedFrame.Load(RestrictedSids);

  TabObject.Tag := TAB_INVALIDATED;
  TabAudit.Tag := TAB_INVALIDATED;
  PageControlChange(Self);
end;

procedure TInfoDialog.SetAuditPolicy;
begin
  try
    Token.SetAuditPolicy(LsaxUserAuditToTokenAudit(Audit)).RaiseOnError;
  finally
    TabAudit.Tag := TAB_INVALIDATED;
    UpdateAuditTab;
  end;
end;

procedure TInfoDialog.SetStaleColor;
begin
  Assert(Sender is TComboBox);
  (Sender as TComboBox).Color := ColorSettings.clBackgroundUnsaved;
end;

procedure TInfoDialog.UpdateAuditTab;
var
  AuditOverrides: TArray<TAuditPolicyEntry>;
  AuditPolicy: ITokenAuditPolicy;
begin
  if TabAudit.Tag = TAB_UPDATED then
    Exit;

  // TODO: Subscribe event
  if Token.QueryAuditPolicy(AuditPolicy).IsSuccess and
    LsaxTokenAuditToUserAudit(AuditPolicy.Data, AuditOverrides).IsSuccess then
    FrameAudit.Load(AuditOverrides)
  else
    FrameAudit.Load(nil);

  TabAudit.Tag := TAB_UPDATED;
end;

procedure TInfoDialog.UpdateLogonTab;
begin
  if TabLogon.Tag = TAB_UPDATED then
    Exit;

  Token.RefreshStatistics;
  Token.RefreshOrigin;

  if not FrameLogon.Subscribed then
    FrameLogon.SubscribeToken(Token);

  TabLogon.Tag := TAB_UPDATED;
end;

procedure TInfoDialog.UpdateObjectTab;
var
  BasicInfo: TObjectBasicInformation;
  Handles: TArray<TSystemHandleEntry>;
  i: Integer;
begin
  if TabObject.Tag = TAB_UPDATED then
    Exit;

  // Update basic object information
  if Token.QueryBasicInfo(BasicInfo).IsSuccess then
    with ListViewObject do
    begin
      Items[1].SubItems[0] := TType.Represent(BasicInfo.Attributes).Text;
      Items[2].SubItems[0] := UiLibBytesToString(BasicInfo.PagedPoolCharge);
      Items[3].SubItems[0] := UiLibBytesToString(BasicInfo.NonPagedPoolCharge);
      Items[4].SubItems[0] := UiLibUIntToDec(BasicInfo.PointerCount);
      Items[5].SubItems[0] := UiLibUIntToDec(BasicInfo.HandleCount);
    end;

  ListViewProcesses.Items.BeginUpdate;
  ListViewProcesses.Items.Clear;
  ListViewProcesses.SmallImages := TProcessIcons.ImageList;

  // Snapshot handles that point to that object
  if Token.QueryHandles(Handles).IsSuccess then
  begin
    for i := 0 to High(Handles) do
      with ListViewProcesses.Items.Add do
      begin
        if Handles[i].UniqueProcessId = NtCurrentProcessId then
        begin
          Caption := 'Current process';
          ImageIndex := TProcessIcons.GetIcon(ParamStr(0));
        end
        else
        begin
          Caption := TType.Represent(Handles[i].UniqueProcessId).Text;
          ImageIndex := TProcessIcons.GetIconByPid(Handles[i].UniqueProcessId);
        end;

        SubItems.Add(UiLibUIntToDec(Handles[i].UniqueProcessId));
        SubItems.Add(UiLibUIntToHex(Handles[i].HandleValue, 4 or
          NUMERIC_WIDTH_ROUND_TO_BYTE));
        SubItems.Add(TType.Represent<TTokenAccessMask>(Handles[i].GrantedAccess).Text);
      end;
  end;

  ListViewObject.Items[6].SubItems[0] := Token.QueryString(tsCreator);
  ListViewProcesses.Items.EndUpdate;
  TabObject.Tag := TAB_UPDATED;
end;

end.
