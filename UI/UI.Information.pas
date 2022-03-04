unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens, System.ImageList, Vcl.ImgList,
  VclEx.ListView, UI.Prototypes, UI.Prototypes.Forms, NtUtils.Security.Sid,
  TU.Tokens.Types, Ntapi.WinNt, UI.Prototypes.AuditFrame, UI.Prototypes.Logon,
  UI.Prototypes.Privileges, UI.Prototypes.Groups, NtUtils.Lsa.Audit,
  Ntapi.ntseapi, NtUtils, Vcl.ExtCtrls, UI.Prototypes.Acl, TU.Tokens3;

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
    TabSecurity: TTabSheet;
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
    FrameDefaultDacl: TFrameAcl;
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
    procedure SetAuditPolicy(const Audit: TArray<TAuditPolicyEntry>);
    procedure InspectGroup(const Group: TGroup);
    procedure Refresh;
    procedure UpdateObjectTab;
    procedure UpdateAuditTab;
    procedure UpdateLogonTab;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: IToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Colors, UI.ProcessList, Ntapi.ntstatus,
  UI.Information.Access, UI.Sid.View, NtUtils.Objects.Snapshots,
  NtUiLib.Errors, DelphiUiLib.Strings,
  DelphiUiLib.Reflection.Strings, NtUiLib.Reflection.AccessMasks,
  Ntapi.ntpsapi, NtUtils.Processes, DelphiUiLib.Reflection, NtUtils.Profiles,
  NtUtils.Lsa.Sid, DelphiUtils.Arrays, DelphiUiLib.Reflection.Numeric,
  UI.ProcessIcons, Ntapi.Versions, UI.AppContainer.View, NtUiLib.Exceptions,
  Ntapi.ntobapi;

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

procedure TInfoDialog.ActionGroupDisable(Sender: TObject);
begin
  if GroupsMemberFrame.VST.SelectedCount > 0 then
    (Token as IToken3).AdjustGroups(GroupsToSids(GroupsMemberFrame.Selected),
      SE_GROUP_DISABLED).RaiseOnError;
end;

procedure TInfoDialog.ActionGroupEnable(Sender: TObject);
begin
  if GroupsMemberFrame.VST.SelectedCount > 0 then
    (Token as IToken3).AdjustGroups(GroupsToSids(GroupsMemberFrame.Selected),
      SE_GROUP_ENABLED).RaiseOnError;
end;

procedure TInfoDialog.ActionGroupReset(Sender: TObject);
begin
  if GroupsMemberFrame.VST.SelectedCount > 0 then
    (Token as IToken3).AdjustGroupsReset.RaiseOnError;
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

procedure TInfoDialog.ActionPrivilegeDisable(Sender: TObject);
begin
  if PrivilegesFrame.VST.SelectedCount <> 0 then
    RaiseOnWarningOrError((Token as IToken3).AdjustPrivileges(
      PrivilegesToWellKnown(PrivilegesFrame.Selected), SE_PRIVILEGE_DISABLED,
      True));
end;

procedure TInfoDialog.ActionPrivilegeEnable(Sender: TObject);
begin
  if PrivilegesFrame.VST.SelectedCount <> 0 then
    RaiseOnWarningOrError((Token as IToken3).AdjustPrivileges(
      PrivilegesToWellKnown(PrivilegesFrame.Selected), SE_PRIVILEGE_ENABLED,
      True));
end;

procedure TInfoDialog.ActionPrivilegeRemove(Sender: TObject);
begin
  if PrivilegesFrame.VST.SelectedCount = 0 then
    Exit;

  if TaskMessageDlg('Remove these privileges from the token?',
    'This action can''t be undone.', mtWarning, mbYesNo, -1) <> idYes then
    Exit;

  RaiseOnWarningOrError((Token as IToken3).AdjustPrivileges(
    PrivilegesToWellKnown(PrivilegesFrame.Selected), SE_PRIVILEGE_REMOVED,
    True));
end;

procedure TInfoDialog.BtnSetIntegrityClick(Sender: TObject);
var
  Status: TNtxStatus;
begin
  Status := (Token as IToken3).SetIntegrity(IntegritySource.SelectedIntegrity);
  ComboIntegrity.Color := clWindow;

  if not Status.IsSuccess then
  begin
    IntegritySubscription := nil;
    IntegritySubscription := (Token as IToken3).ObserveIntegrity(ChangedIntegrity);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetOwnerClick(Sender: TObject);
var
  Sid: ISid;
  Status: TNtxStatus;
begin
  LsaxLookupNameOrSddl(ComboOwner.Text, Sid).RaiseOnError;
  Status := (Token as IToken3).SetOwner(Sid);
  ComboOwner.Color := clWindow;

  if not Status.IsSuccess then
  begin
    OwnerSubscription := nil;
    OwnerSubscription := (Token as IToken3).ObserveOwner(ChangedOwner);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetPolicyClick(Sender: TObject);
var
  Policy: TTokenMandatoryPolicy;
  Status: TNtxStatus;
begin
  Policy := 0;

  if CheckBoxNoWriteUp.Checked then
    Policy := Policy or TOKEN_MANDATORY_POLICY_NO_WRITE_UP;

  if CheckBoxNewProcessMin.Checked then
    Policy := Policy or TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  Status := (Token as IToken3).SetMandatoryPolicy(Policy);
  CheckBoxNoWriteUp.Font.Style := [];
  CheckBoxNewProcessMin.Font.Style := [];

  if not Status.IsSuccess then
  begin
    PolicySubscription := nil;
    PolicySubscription := (Token as IToken3).ObserveMandatoryPolicy(ChangedPolicy);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetPrimaryClick(Sender: TObject);
var
  Sid: ISid;
  Status: TNtxStatus;
begin
  LsaxLookupNameOrSddl(ComboPrimary.Text, Sid).RaiseOnError;
  Status := (Token as IToken3).SetPrimaryGroup(Sid);
  ComboPrimary.Color := clWindow;

  if not Status.IsSuccess then
  begin
    PrimaryGroupSubscription := nil;
    PrimaryGroupSubscription := (Token as IToken3).ObservePrimaryGroup(
      ChangedPrimaryGroup);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetSessionClick(Sender: TObject);
var
  Status: TNtxStatus;
begin
  Status := (Token as IToken3).SetSessionId(SessionSource.SelectedSession);

  if not Status.IsSuccess then
  begin
    SessionSubscription := nil;
    SessionSubscription := (Token as IToken3).ObserveSessionId(ChangedSession);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetUIAccessClick(Sender: TObject);
var
  UIAccess: LongBool;
  Status: TNtxStatus;
begin
  if ComboUIAccess.ItemIndex = -1 then
    UIAccess := LongBool(StrToUIntEx(ComboUIAccess.Text, 'UIAccess value'))
  else
    UIAccess := LongBool(ComboUIAccess.ItemIndex);

  Status := (Token as IToken3).SetUIAccess(UIAccess);
  ComboUIAccess.Color := clWindow;

  if not Status.IsSuccess then
  begin
    UIAccessSubscription := nil;
    UIAccessSubscription := (Token as IToken3).ObserveUIAccess(ChangedUIAccess);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetVAllowedClick(Sender: TObject);
var
  Status: TNtxStatus;
begin
  Status := (Token as IToken3).SetVirtualizationAllowed(CheckBoxVAllowed.Checked);
  CheckBoxVAllowed.Font.Style := [];

  if not Status.IsSuccess then
  begin
    VAllowedSubscription := nil;
    VAllowedSubscription := (Token as IToken3).ObserveVirtualizationAllowed(
      ChangedVAllowed);
  end;

  Status.RaiseOnError;
end;

procedure TInfoDialog.BtnSetVEnabledClick(Sender: TObject);
var
  Status: TNtxStatus;
begin
  Status := (Token as IToken3).SetVirtualizationEnabled(CheckBoxVEnabled.Checked);
  CheckBoxVEnabled.Font.Style := [];

  if not Status.IsSuccess then
  begin
    VEnabledSubscription := nil;
    VEnabledSubscription  := (Token as IToken3).ObserveVirtualizationEnabled(
      ChangedVEnabled);
  end;
end;

procedure TInfoDialog.ChangedCaption;
begin
  Caption := Format('Token Information for "%s"', [NewCaption]);
end;

procedure TInfoDialog.ChangedElevation;
begin
  if Status.IsSuccess then
    ListViewGeneral.Items[4].SubItems[0] :=
      (Token as IToken3).QueryString(tsElevation);
end;

procedure TInfoDialog.ChangedFlags;
begin
  if Status.IsSuccess then
    ListViewAdvanced.Items[10].SubItems[0] := (Token as IToken3).QueryString(tsFlags);
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
  if (Token as IToken3).QueryUser(User).IsSuccess then
  begin
    UserName := LsaxSidToString(User.Sid);
    ComboOwner.Items.Add(UserName);
    ComboPrimary.Items.Add(UserName);
  end;

  // Add all groups for Primary Group and only those with specific attribtes
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
      Items[2].SubItems[0] := (Token as IToken3).QueryString(tsTokenID);
      Items[3].SubItems[0] := (Token as IToken3).QueryString(tsLogonID);
      Items[4].SubItems[0] := (Token as IToken3).QueryString(tsExprires);
      Items[5].SubItems[0] := (Token as IToken3).QueryString(tsDynamicCharged);
      Items[6].SubItems[0] := (Token as IToken3).QueryString(tsDynamicAvailable);
      Items[7].SubItems[0] := (Token as IToken3).QueryString(tsGroups);
      Items[8].SubItems[0] := (Token as IToken3).QueryString(tsPrivileges);
      Items[9].SubItems[0] := (Token as IToken3).QueryString(tsModifiedID);
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

procedure TInfoDialog.CheckBoxClick(Sender: TObject);
begin
  Assert(Sender is TCheckBox);
  (Sender as TCheckBox).Font.Style := [fsBold];
end;

constructor TInfoDialog.CreateFromToken(AOwner: TComponent; SrcToken: IToken);
begin
  Assert(Assigned(SrcToken));
  Token := SrcToken;
  inherited CreateChild(AOwner, cfmDesktop);

  GroupsRestrictedFrame.OnDefaultAction := InspectGroup;
  GroupsMemberFrame.OnDefaultAction := InspectGroup;
  Show;
end;

procedure TInfoDialog.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TInfoDialog.EditAppContainerDblClick(Sender: TObject);
var
  Info: TAppContainerInfo;
begin
  if (Token as IToken3).QueryAppContainerInfo(Info).IsSuccess then
    TDialogAppContainer.Execute(FormMain, Info.User, Info.Package);
end;

procedure TInfoDialog.EditUserDblClick(Sender: TObject);
var
  User: TGroup;
begin
  if (Token as IToken3).QueryUser(User).IsSuccess then
    TDialogSidView.CreateView(FormMain, User.Sid);
end;

procedure TInfoDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IntegritySource.Free;
  SessionSource.Free;
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
begin
  SessionSource := TSessionSource.Create(ComboSession, False);
  IntegritySource := TIntegritySource.Create(ComboIntegrity);
  FrameAudit.OnApplyClick := SetAuditPolicy;

  // "Refresh" queries all the information, stores changeble one in the event
  // handler, and distributes changed one to every existing event listener
  Refresh;

  IntegritySubscription := (Token as IToken3).ObserveIntegrity(ChangedIntegrity);
  SessionSubscription := (Token as IToken3).ObserveSessionId(ChangedSession);
  UIAccessSubscription := (Token as IToken3).ObserveUIAccess(ChangedUIAccess);
  PolicySubscription := (Token as IToken3).ObserveMandatoryPolicy(ChangedPolicy);
  PrivilegesSubscription := (Token as IToken3).ObservePrivileges(ChangedPrivileges);
  GroupsSubscription := (Token as IToken3).ObserveGroups(ChangedGroups);
  StatisticsSubscription := (Token as IToken3).ObserveStatistics(ChangedStatistics);
  OwnerSubscription := (Token as IToken3).ObserveOwner(ChangedOwner);
  PrimaryGroupSubscription := (Token as IToken3).ObservePrimaryGroup(ChangedPrimaryGroup);
  VAllowedSubscription := (Token as IToken3).ObserveVirtualizationAllowed(ChangedVAllowed);
  VEnabledSubscription := (Token as IToken3).ObserveVirtualizationEnabled(ChangedVEnabled);
  ElevationSubscription := (Token as IToken3).ObserveElevation(ChangedElevation);
  FlagsSubscription := (Token as IToken3).ObserveFlags(ChangedFlags);
  CaptionSubscription := (Token as IToken3).ObserveString(tsCaption, ChangedCaption);

  TabRestricted.Caption := Format('Restricting SIDs (%d)',
    [GroupsRestrictedFrame.VST.RootNodeCount]);
end;

procedure TInfoDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    Refresh;
end;

procedure TInfoDialog.InspectGroup;
begin
  TDialogSidView.CreateView(Self, Group.Sid);
end;

procedure TInfoDialog.ListViewAdvancedResize(Sender: TObject);
begin
  // HACK: designs-time AutoSize causes horizontal scrollbar to appear
  ListViewAdvanced.Columns[1].AutoSize := True;
end;

procedure TInfoDialog.ListViewGeneralDblClick(Sender: TObject);
var
  BasicInfo: TObjectBasicInformation;
begin
  if Assigned(ListViewGeneral.Selected) and
    (ListViewGeneral.Selected.Index = 2) and
    (Token as IToken3).QueryBasicInfo(BasicInfo).IsSuccess then
    TDialogGrantedAccess.Execute(Owner, BasicInfo.GrantedAccess);
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
var
  Repr: TRepresentation;
  DefaultDacl: IAcl;
  User: TGroup;
  Package: ISid;
  RestrictedSids: TArray<TGroup>;
begin
  (Token as IToken3).SmartRefresh;

  ListViewGeneral.Items.BeginUpdate;
  with ListViewGeneral do
  begin
    Items[0].SubItems[0] := (Token as IToken3).QueryString(tsAddress);
    ListViewObject.Items[0].SubItems[0] := Items[0].SubItems[0];
    Items[1].SubItems[0] := (Token as IToken3).QueryString(tsHandle);
    Items[2].SubItems[0] := (Token as IToken3).QueryString(tsAccess, True);
    Items[3].SubItems[0] := (Token as IToken3).QueryString(tsType);
  end;
  ListViewGeneral.Items.EndUpdate;

  ListViewAdvanced.Items.BeginUpdate;
  with ListViewAdvanced do
  begin
    Items[0].SubItems[0] := (Token as IToken3).QueryString(tsSourceName);
    Items[1].SubItems[0] := (Token as IToken3).QueryString(tsSourceId);
  end;
  ListViewAdvanced.Items.EndUpdate;

  if (Token as IToken3).QueryDefaultDacl(DefaultDacl).IsSuccess then
    FrameDefaultDacl.Load(Auto.RefOrNil<PAcl>(DefaultDacl), nil);

  if (Token as IToken3).QueryUser(User).IsSuccess then
  begin
    // For user, 0 means default (enabled) state, but it can also
    // be use-for-deny-only.
    if User.Attributes = 0 then
      User.Attributes := SE_GROUP_ENABLED or SE_GROUP_ENABLED_BY_DEFAULT;

    Repr := TType.Represent(User);

    EditUser.Text := Repr.Text;
    EditUser.Hint := Repr.Hint;

    if User.Attributes and SE_GROUP_USE_FOR_DENY_ONLY <> 0 then
      EditUser.Color := ColorSettings.clDisabled
    else
      EditUser.Color := ColorSettings.clEnabled;

    // AppContainer is user-specific
    if not RtlOsVersionAtLeast(OsWin8) then
      EditAppContainer.Text := 'Not supported'
    else if (Token as IToken3).QueryAppContainerSid(Package).IsSuccess then
    begin
      if not Assigned(Package) then
        EditAppContainer.Text := 'No'
      else
      begin
        EditAppContainer.Text := RtlxSidToString(Package);
        EditAppContainer.Enabled := True;
      end;
    end;
  end;

  if (Token as IToken3).QueryRestrictedSids(RestrictedSids).IsSuccess then
    GroupsRestrictedFrame.Load(RestrictedSids);

  TabObject.Tag := TAB_INVALIDATED;
  TabAudit.Tag := TAB_INVALIDATED;
  PageControlChange(Self);
end;

procedure TInfoDialog.SetAuditPolicy;
begin
  try
    (Token as IToken3).SetAuditPolicy(LsaxUserAuditToTokenAudit(Audit)).RaiseOnError;
  finally
    TabAudit.Tag := TAB_INVALIDATED;
    UpdateAuditTab;
  end;
end;

procedure TInfoDialog.SetStaleColor(Sender: TObject);
begin
  Assert(Sender is TComboBox);
  (Sender as TComboBox).Color := ColorSettings.clStale;
end;

procedure TInfoDialog.UpdateAuditTab;
var
  AuditOverrides: TArray<TAuditPolicyEntry>;
  AuditPolicy: ITokenAuditPolicy;
begin
  if TabAudit.Tag = TAB_UPDATED then
    Exit;

  // TODO: Subscribe event
  if (Token as IToken3).QueryAuditPolicy(AuditPolicy).IsSuccess and
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

  (Token as IToken3).RefreshStatistics;
  (Token as IToken3).RefreshOrigin;

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
  if (Token as IToken3).QueryBasicInfo(BasicInfo).IsSuccess then
    with ListViewObject do
    begin
      Items[1].SubItems[0] := TNumeric.Represent(BasicInfo.Attributes).Text;
      Items[2].SubItems[0] := BytesToString(BasicInfo.PagedPoolCharge);
      Items[3].SubItems[0] := BytesToString(BasicInfo.NonPagedPoolCharge);
      Items[4].SubItems[0] := IntToStr(BasicInfo.PointerCount);
      Items[5].SubItems[0] := IntToStr(BasicInfo.HandleCount);
    end;

  ListViewProcesses.Items.BeginUpdate;
  ListViewProcesses.Items.Clear;
  ListViewProcesses.SmallImages := TProcessIcons.ImageList;

  // Snapshot handles that point to that object
  if (Token as IToken3).QueryHandles(Handles).IsSuccess then
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

        SubItems.Add(IntToStr(Handles[i].UniqueProcessId));
        SubItems.Add(IntToHexEx(Handles[i].HandleValue));
        SubItems.Add(Handles[i].GrantedAccess.Format<TTokenAccessMask>);
      end;
  end;

  ListViewObject.Items[6].SubItems[0] := (Token as IToken3).QueryString(tsCreator);
  ListViewProcesses.Items.EndUpdate;
  TabObject.Tag := TAB_UPDATED;
end;

end.
