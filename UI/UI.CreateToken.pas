unit UI.CreateToken;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.Forms,
  UI.Prototypes, VclEx.ListView, UI.MainForm, TU.Tokens, TU.Tokens.Types,
  NtUtils.Security.Sid, UI.Prototypes.Privileges, UI.Prototypes.Groups,
  NtUtils;

type
  TGroupUpdateType = (guEditOne, guEditMultiple, guRemove);

  TDialogCreateToken = class(TChildForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    ButtonAddSID: TButton;
    StaticLogonID: TStaticText;
    StaticOwner: TStaticText;
    StaticPrimaryGroup: TStaticText;
    ComboLogonSession: TComboBox;
    ComboUser: TComboBox;
    ButtonPickUser: TButton;
    PopupMenuGroups: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    ComboOwner: TComboBox;
    ComboPrimary: TComboBox;
    CheckBoxUserState: TCheckBox;
    GroupBoxUser: TGroupBox;
    TabAdvanced: TTabSheet;
    GroupBoxExpires: TGroupBox;
    CheckBoxInfinite: TCheckBox;
    DateExpires: TDateTimePicker;
    TimeExpires: TDateTimePicker;
    GroupBoxSource: TGroupBox;
    EditSourceName: TEdit;
    StaticSourceName: TStaticText;
    StaticSourceLuid: TStaticText;
    EditSourceLuid: TEdit;
    ButtonAllocLuid: TButton;
    PopupMenuPrivileges: TPopupMenu;
    MenuDisabled: TMenuItem;
    MenuDisabledModif: TMenuItem;
    MenuEnabled: TMenuItem;
    MenuEnabledModif: TMenuItem;
    TabDefaltDacl: TTabSheet;
    ButtonLoad: TButton;
    GroupBoxPostCreation: TGroupBox;
    CheckBoxNoWriteUp: TCheckBox;
    CheckBoxNewProcMin: TCheckBox;
    CheckBoxSession: TCheckBox;
    GroupsFrame: TFrameGroups;
    PrivilegesFrame: TFramePrivileges;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure ButtonPickUserClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxInfiniteClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure ButtonAllocLuidClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ComboUserChange(Sender: TObject);
    procedure MenuDisabledClick(Sender: TObject);
    procedure MenuDisabledModifClick(Sender: TObject);
    procedure MenuEnabledClick(Sender: TObject);
    procedure MenuEnabledModifClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    LogonIDSource: TLogonSessionSource;
    procedure AddGroup(NewGroup: TGroup);
    procedure UpdatePrimaryAndOwner(Mode: TGroupUpdateType);
    procedure EditSingleGroup(const Value: TGroup);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  UI.Modal.PickUser, TU.Winapi, VirtualTrees, UI.Settings, UI.Modal.PickToken,
  System.UITypes, NtUtils.Lsa.Sid, Ntapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi,
  Ntapi.ntseapi, Ntapi.ntpebteb, NtUiLib.Errors, DelphiUiLib.Strings,
  DelphiUiLib.Reflection.Strings, UI.Builtin.DsObjectPicker, TU.Tokens3,
  Ntapi.ntobapi, TU.Tokens3.Open;

{$R *.dfm}

procedure TDialogCreateToken.AddGroup;
begin
  GroupsFrame.Add([NewGroup]);

  if BitTest(NewGroup.Attributes and SE_GROUP_OWNER) then
    ComboOwner.Items.Add(LsaxSidToString(NewGroup.Sid));

  ComboPrimary.Items.Add(LsaxSidToString(NewGroup.Sid));
end;

procedure TDialogCreateToken.ButtonAddSIDClick;
begin
  AddGroup(TDialogPickUser.PickNew(Self));
end;

procedure TDialogCreateToken.ButtonAllocLuidClick;
var
  NewLuid: TLuid;
begin
  if NT_SUCCESS(NtAllocateLocallyUniqueId(NewLuid)) then
    EditSourceLuid.Text := IntToHexEx(NewLuid);
end;

procedure TDialogCreateToken.ButtonCancelClick;
begin
  Close;
end;

procedure TDialogCreateToken.ButtonLoadClick;
var
  Source: IToken3;
  Expiration: TDateTime;
  BasicInfo: TObjectBasicInformation;
  User: TGroup;
  Statistics: TTokenStatistics;
  Groups: TArray<TGroup>;
  Owner, PrimaryGroup: ISid;
  Privileges: TArray<TPrivilege>;
  TokenSource: TTokenSource;
begin
  Source := TDialogPickToken.Execute(Self) as IToken3;

  if not Source.QueryBasicInfo(BasicInfo).IsSuccess or
    not BitTest(BasicInfo.GrantedAccess and TOKEN_QUERY) then
  begin
    MessageDlg('This token does not have Query access.', mtError, mbOKCancel,
      -1);
    Abort;
  end;

  // User
  if Source.QueryUser(User).IsSuccess then
  begin
    ComboUser.Text := LsaxSidToString(User.Sid);
    ComboUserChange(Sender);
    CheckBoxUserState.Checked := BitTest(User.Attributes and
      SE_GROUP_USE_FOR_DENY_ONLY);
  end;

  // Logon ID & Expiration
  if Source.QueryStatistics(Statistics).IsSuccess then
  begin
    LogonIDSource.SelectedLogonSession := Statistics.AuthenticationId;

    CheckBoxInfinite.Checked := (Statistics.ExpirationTime = Int64.MaxValue);

    if not CheckBoxInfinite.Checked then
    begin
      Expiration := LargeIntegerToDateTime(Statistics.ExpirationTime);

      // Date only
      DateExpires.DateTime := Trunc(Expiration);

      // Time only
      TimeExpires.DateTime := Expiration - Trunc(Expiration);
    end;
  end;

  // Groups
  if Source.QueryGroups(Groups).IsSuccess then
    GroupsFrame.Load(Groups);

  UpdatePrimaryAndOwner(guEditOne);

  // Owner
  if Source.QueryOwner(Owner).IsSuccess then
    ComboOwner.Text := LsaxSidToString(Owner);

  // Primary group
  if Source.QueryPrimaryGroup(PrimaryGroup).IsSuccess then
    ComboPrimary.Text := LsaxSidToString(PrimaryGroup);

  // Privileges
  if Source.QueryPrivileges(Privileges).IsSuccess then
    PrivilegesFrame.Checked := Privileges;

  // Source
  if Source.QuerySource(TokenSource).IsSuccess then
  begin
    EditSourceName.Text := TokenSource.Name;
    EditSourceLuid.Text := IntToHexEx(TokenSource.SourceIdentifier);
  end;
end;

procedure TDialogCreateToken.ButtonOKClick;
var
  Token: IToken;
  Expires: TLargeInteger;
  OwnerGroupName, PrimaryGroupName: String;
  NewPolicy: TTokenMandatoryPolicy;
  Source: TTokenSource;
  User, Owner, PrimaryGroup: ISid;
begin
  if CheckBoxInfinite.Checked then
    Expires := Int64.MaxValue
  else if TimeExpires.Checked then
    Expires := DateTimeToLargeInteger(DateExpires.Date + TimeExpires.Time)
  else
    Expires := DateTimeToLargeInteger(DateExpires.Date);

  // ComboOwner may contain '< Same as user >' value
  if ComboOwner.ItemIndex = 0 then
    OwnerGroupName := ComboUser.Text
  else
    OwnerGroupName := ComboOwner.Text;

  // ComboPrimary may contain '< Same as user >' value
  if ComboPrimary.ItemIndex = 0 then
    PrimaryGroupName := ComboUser.Text
  else
    PrimaryGroupName := ComboPrimary.Text;

  Source.Name := EditSourceName.Text;
  Source.SourceIdentifier := StrToUInt64Ex(EditSourceLuid.Text, 'Source LUID');

  LsaxLookupNameOrSddl(ComboUser.Text, User).RaiseOnError;
  LsaxLookupNameOrSddl(OwnerGroupName, Owner).RaiseOnError;
  LsaxLookupNameOrSddl(PrimaryGroupName, PrimaryGroup).RaiseOnError;

  MakeNewToken(Token, ttPrimary, User, CheckBoxUserState.Checked,
    GroupsFrame.All, PrivilegesFrame.Checked, LogonIDSource.SelectedLogonSession,
    PrimaryGroup, Source, Owner, nil, Expires).RaiseOnError;

  FormMain.TokenView.Add(Token);

  // Post-creation: change mandatory policy
  NewPolicy := 0;
  if CheckBoxNoWriteUp.Checked then
    NewPolicy := NewPolicy or TOKEN_MANDATORY_POLICY_NO_WRITE_UP;
  if CheckBoxNewProcMin.Checked then
    NewPolicy := NewPolicy or TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  if NewPolicy <> 0 then
    (Token as IToken3).SetMandatoryPolicy(NewPolicy).RaiseOnError;

  // Post-creation: change session
  if CheckBoxSession.Checked then
    (Token as IToken3).SetSessionId(RtlGetCurrentPeb.SessionId).RaiseOnError;

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogCreateToken.ButtonPickUserClick;
var
  Account: String;
  Sid: ISid;
begin
  with ComxCallDsObjectPicker(Handle, Account) do
    if IsHResult and (HResult = S_FALSE) then
      Abort
    else
      RaiseOnError;

  LsaxLookupNameOrSddl(Account, Sid).RaiseOnError;
  ComboUser.Text := LsaxSidToString(Sid);
  ComboUserChange(ButtonPickUser);
end;

procedure TDialogCreateToken.CheckBoxInfiniteClick;
begin
  DateExpires.Enabled := not CheckBoxInfinite.Checked;
  TimeExpires.Enabled := not CheckBoxInfinite.Checked;
end;

procedure TDialogCreateToken.ComboUserChange;
var
  NewUser: String;
  SavedOwnerIndex, SavedPrimaryIndex: Integer;
begin
  NewUser := ComboUser.Text;
  if NewUser = '' then
    NewUser := '< Same as user >';

  // Save selected indexes since changes will reset it
  SavedOwnerIndex := ComboOwner.ItemIndex;
  SavedPrimaryIndex := ComboPrimary.ItemIndex;

  if ComboOwner.Items.Count > 0 then
    ComboOwner.Items[0] := NewUser;

  if ComboPrimary.Items.Count > 0 then
    ComboPrimary.Items[0] := NewUser;

  // Forcibly update the Text field
  ComboOwner.ItemIndex := SavedOwnerIndex;
  ComboPrimary.ItemIndex := SavedPrimaryIndex;
end;

constructor TDialogCreateToken.Create;
begin
  inherited CreateChild(AOwner, cfmDesktop);
end;

procedure TDialogCreateToken.EditSingleGroup;
begin
  GroupsFrame.EditSelectedGroup(
    procedure (var Group: TGroup)
    begin
      Group := TDialogPickUser.PickEditOne(Self, Group);
    end
  );
end;

procedure TDialogCreateToken.FormClose;
begin
  LogonIDSource.Free;
end;

procedure TDialogCreateToken.FormCreate;
begin
  LogonIDSource := TLogonSessionSource.Create(ComboLogonSession);
  CheckBoxSession.Enabled := RtlGetCurrentPeb.SessionId <> 0;
  ButtonAllocLuidClick(Self);

  GroupsFrame.OnDefaultAction := EditSingleGroup;
  PrivilegesFrame.ColoringChecked := pcStateBased;
  PrivilegesFrame.ColoringUnChecked := pcRemoved;
  PrivilegesFrame.LoadEvery;
end;

procedure TDialogCreateToken.MenuDisabledClick;
begin
  PrivilegesFrame.AdjustSelected(0);
end;

procedure TDialogCreateToken.MenuDisabledModifClick;
begin
  PrivilegesFrame.AdjustSelected(SE_PRIVILEGE_ENABLED_BY_DEFAULT);
end;

procedure TDialogCreateToken.MenuEditClick;
begin
  if GroupsFrame.VST.SelectedCount = 1 then
  begin
    EditSingleGroup(Default(TGroup));
    UpdatePrimaryAndOwner(guEditOne);
  end
  else if GroupsFrame.VST.SelectedCount > 1 then
  begin
    GroupsFrame.EditSelectedGroups(
      procedure (
        const Groups: TArray<TGroup>;
        var AttributesToClear: TGroupAttributes;
        var AttributesToSet: TGroupAttributes
      )
      begin
        TDialogPickUser.PickEditMultiple(Self, Groups, AttributesToSet,
          AttributesToClear);
      end
    );
    UpdatePrimaryAndOwner(guEditMultiple);
  end;
end;

procedure TDialogCreateToken.MenuEnabledClick(Sender: TObject);
begin
 PrivilegesFrame.AdjustSelected(SE_PRIVILEGE_ENABLED_BY_DEFAULT or
   SE_PRIVILEGE_ENABLED);
end;

procedure TDialogCreateToken.MenuEnabledModifClick(Sender: TObject);
begin
  PrivilegesFrame.AdjustSelected(SE_PRIVILEGE_ENABLED);
end;

procedure TDialogCreateToken.MenuRemoveClick(Sender: TObject);
begin
  GroupsFrame.VST.DeleteSelectedNodes;
  UpdatePrimaryAndOwner(guRemove);
end;

procedure TDialogCreateToken.UpdatePrimaryAndOwner(Mode: TGroupUpdateType);
var
  i: Integer;
  Group: TGroup;
  SavedPrimaryIndex, SavedOwnerIndex, SavedOwnerCount: Integer;
  SavedPrimary, SavedOwner: String;
  Node: PVirtualNode;
begin
  // Save current state to be able to restore it back
  SavedOwnerCount := ComboOwner.Items.Count;
  SavedOwnerIndex := ComboOwner.ItemIndex;
  SavedPrimaryIndex := ComboPrimary.ItemIndex;
  SavedOwner := ComboOwner.Text;
  SavedPrimary := ComboPrimary.Text;

  // Refresh potential owners list
  begin
    ComboOwner.Items.BeginUpdate;

    for i := Pred(ComboOwner.Items.Count) downto 1 do
      ComboOwner.Items.Delete(i);

    // Only groups with Owner flag can be assigned as owners
    for Group in GroupsFrame.All do
      if BitTest(Group.Attributes and SE_GROUP_OWNER) then
        ComboOwner.Items.Add(LsaxSidToString(Group.Sid));

    // Restore choise
    if (Mode = guEditOne) and (SavedOwnerCount = ComboOwner.Items.Count) then
      ComboOwner.ItemIndex := SavedOwnerIndex // Restore by index
    else
    begin
      // Restore by name or fall back to user

      ComboOwner.ItemIndex := 0;
      for i := 1 to ComboOwner.Items.Count - 1 do
        if ComboOwner.Items[i] = SavedOwner then
          ComboOwner.ItemIndex := i;

      if ComboOwner.ItemIndex = 0 then
        ComboOwner.Text := ComboOwner.Items[0];
    end;

    ComboOwner.Items.EndUpdate;
  end;

  // Refresh potential primary group list.
  // Optimization: multiple edit does not change their list
  if Mode <> guEditMultiple then
  begin
    ComboPrimary.Items.BeginUpdate;

    for i := ComboPrimary.Items.Count - 1 downto 1 do
      ComboPrimary.Items.Delete(i);

    // Any group present in the token can be assigned as a primary
    for Node in GroupsFrame.VST.Nodes do
      ComboPrimary.Items.Add(GroupsFrame.VST.Text[Node, 0]);

    // Restore choise
    case Mode of
      guRemove:
        // Restore by name or fall back to user
        begin
          ComboPrimary.ItemIndex := 0;
          for i := 1 to ComboPrimary.Items.Count - 1 do
            if ComboPrimary.Items[i] = SavedPrimary then
              ComboPrimary.ItemIndex := i;

          if ComboPrimary.ItemIndex = 0 then
            ComboPrimary.Text := ComboPrimary.Items[0];
        end;

      guEditOne:
        // Restore by index
        ComboPrimary.ItemIndex := SavedPrimaryIndex;
    end;

    ComboPrimary.Items.EndUpdate;
  end;
end;

end.
