unit UI.CreateToken;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Prototypes.ChildForm, Vcl.StdCtrls,
  UI.Prototypes, UI.ListViewEx, Vcl.ComCtrls, UI.MainForm, Vcl.Menus,
  TU.Tokens, TU.Tokens.Types;

type
  TGroupUpdateType = (guEditOne, guEditMultiple, guRemove);

  TDialogCreateToken = class(TChildTaskbarForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    ListViewGroups: TListViewEx;
    ListViewPrivileges: TListViewEx;
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
    procedure ListViewPrivilegesItemChecked(Sender: TObject; Item: TListItem);
  private
    LogonIDSource: TLogonSessionSource;
    GroupsSource: TGroupsSource;
    PrivilegesSource: TPrivilegesSource;
    procedure ObjPickerUserCallback(UserName: String);
    procedure AddGroup(NewGroup: TGroup);
    procedure UpdatePrimaryAndOwner(Mode: TGroupUpdateType);
    procedure SetPrivilegesAttributes(NewValue: Cardinal);
  end;

implementation

uses
  TU.LsaApi, UI.Modal.PickUser, TU.ObjPicker, TU.Winapi,
  TU.Common, UI.Settings, UI.Modal.PickToken, System.UITypes;

{$R *.dfm}

procedure TDialogCreateToken.AddGroup(NewGroup: TGroup);
begin
  GroupsSource.AddGroup(NewGroup);

  if NewGroup.Attributes.Contain(GroupOwner) then
    ComboOwner.Items.Add(NewGroup.SecurityIdentifier.ToString);

  ComboPrimary.Items.Add(NewGroup.SecurityIdentifier.ToString);
end;

procedure TDialogCreateToken.ButtonAddSIDClick(Sender: TObject);
begin
  AddGroup(TDialogPickUser.PickNew(Self));
end;

procedure TDialogCreateToken.ButtonAllocLuidClick(Sender: TObject);
var
  NewLuid: Int64;
begin
  if Winapi.Windows.AllocateLocallyUniqueId(NewLuid) then
    EditSourceLuid.Text := Format('0x%x', [NewLuid]);
end;

procedure TDialogCreateToken.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogCreateToken.ButtonLoadClick(Sender: TObject);
var
  Source: TToken;
  i, j: Integer;
begin
  Source := TDialogPickToken.Execute(Self);

  if Source.HandleInformation.GrantedAccess and TOKEN_QUERY = 0 then
  begin
    MessageDlg('This token does not have Query access.', mtError, mbOKCancel,
      -1);
    Abort;
  end;

  // User
  if Source.InfoClass.Query(tdTokenUser) then
  begin
    ComboUser.Text := Source.InfoClass.User.SecurityIdentifier.ToString;
    ComboUserChange(Sender);
    CheckBoxUserState.Checked := Source.InfoClass.User.Attributes.Contain(
      GroupUforDenyOnly);
  end;

  // Logon ID & Expiration
  if Source.InfoClass.Query(tdTokenStatistics) then
  begin
    LogonIDSource.SelectedLogonSession :=
      Source.InfoClass.Statistics.AuthenticationId;

    CheckBoxInfinite.Checked := (Source.InfoClass.Statistics.ExpirationTime =
      Int64.MaxValue);

    if not CheckBoxInfinite.Checked then
    begin
      DateExpires.DateTime := NativeTimeToLocalDateTime(
        Source.InfoClass.Statistics.ExpirationTime);

      TimeExpires.DateTime := DateExpires.DateTime;
    end;
  end;

  // Groups
  if Source.InfoClass.Query(tdTokenGroups) then
  begin
    ListViewGroups.Items.BeginUpdate;

    GroupsSource.Clear;
    for i := 0 to High(Source.InfoClass.Groups) do
      AddGroup(Source.InfoClass.Groups[i]);

    ListViewGroups.Items.EndUpdate;
  end;

  // Owner
  if Source.InfoClass.Query(tdTokenOwner) then
    ComboOwner.Text := Source.InfoClass.Owner.ToString;

  // Primary group
  if Source.InfoClass.Query(tdTokenPrimaryGroup) then
    ComboPrimary.Text := Source.InfoClass.PrimaryGroup.ToString;

  // Privileges
  if Source.InfoClass.Query(tdTokenPrivileges) then
  begin
    ListViewPrivileges.Items.BeginUpdate;

    // Uncheck everything
    for i := 0 to ListViewPrivileges.Items.Count - 1 do
      ListViewPrivileges.Items[i].Checked := False;

    for i := 0 to High(Source.InfoClass.Privileges) do
    begin
      // Locate a privilege from the token in the list
      j := PrivilegesSource.Find(Source.InfoClass.Privileges[i]);

      // Set appropriate state and check it
      if j = -1 then
        // Add if necessary
        PrivilegesSource.AddPrivilege(Source.InfoClass.Privileges[i]).Checked :=
          True
      else
      begin
        PrivilegesSource.Privilege[j] := Source.InfoClass.Privileges[i];
        ListViewPrivileges.Items[j].Checked := True;
      end;
    end;

    ListViewPrivileges.Items.EndUpdate;
  end;

  // Source
  if Source.InfoClass.Query(tdTokenSource) then
  begin
    EditSourceName.Text := TokeSourceNameToString(Source.InfoClass.Source);
    EditSourceLuid.Text := LuidToString(
      Source.InfoClass.Source.SourceIdentifier);
  end;
end;

procedure TDialogCreateToken.ButtonOKClick(Sender: TObject);
var
  Token: TToken;
  Expires: Int64;
  OwnerGroupName, PrimaryGroupName: String;
  NewPolicy: TMandatoryPolicy;
begin
  if CheckBoxInfinite.Checked then
    Expires := Int64.MaxValue
  else if TimeExpires.Checked then
    Expires := DateTimeToNative(DateExpires.Date + TimeExpires.Time)
  else
    Expires := DateTimeToNative(DateExpires.Date);

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

  Token := TToken.CreateNtCreateToken(
    TSecurityIdentifier.CreateFromString(ComboUser.Text),
    CheckBoxUserState.Checked,
    GroupsSource.Groups,
    PrivilegesSource.CheckedPrivileges,
    LogonIDSource.SelectedLogonSession,
    TSecurityIdentifier.CreateFromString(OwnerGroupName),
    TSecurityIdentifier.CreateFromString(PrimaryGroupName),
    CreateTokenSource(EditSourceName.Text,
      StrToUInt64Ex(EditSourceLuid.Text, 'Source LUID')),
    Expires
  );

  FormMain.TokenView.Add(Token);

  // Post-creation: change mandatory policy
  NewPolicy.Create(CheckBoxNoWriteUp.Checked, CheckBoxNewProcMin.Checked);
  if NewPolicy <> MandatoryPolicyOff then
    Token.InfoClass.MandatoryPolicy := NewPolicy;

  // Post-creation: change session
  if CheckBoxSession.Checked then
    Token.InfoClass.Session := GetCurrentSession;

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogCreateToken.ButtonPickUserClick(Sender: TObject);
begin
  CallObjectPicker(Handle, ObjPickerUserCallback);
end;

procedure TDialogCreateToken.CheckBoxInfiniteClick(Sender: TObject);
begin
  DateExpires.Enabled := not CheckBoxInfinite.Checked;
  TimeExpires.Enabled := not CheckBoxInfinite.Checked;
end;

procedure TDialogCreateToken.ComboUserChange(Sender: TObject);
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

procedure TDialogCreateToken.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  PrivilegesSource.Free;
  GroupsSource.Free;
  LogonIDSource.Free;
end;

procedure TDialogCreateToken.FormCreate(Sender: TObject);
begin
  LogonIDSource := TLogonSessionSource.Create(ComboLogonSession);
  GroupsSource := TGroupsSource.Create(ListViewGroups);
  PrivilegesSource := TPrivilegesSource.Create(ListViewPrivileges);
  PrivilegesSource.AddAllPrivileges;
  CheckBoxSession.Enabled := GetCurrentSession <> 0;
  ButtonAllocLuidClick(Self);
end;

procedure TDialogCreateToken.ListViewPrivilegesItemChecked(Sender: TObject;
  Item: TListItem);
begin
  if Assigned(Item) then
    PrivilegesSource.ValidateColor(Item.Index);
end;

procedure TDialogCreateToken.MenuDisabledClick(Sender: TObject);
begin
  SetPrivilegesAttributes(0);
end;

procedure TDialogCreateToken.MenuDisabledModifClick(Sender: TObject);
begin
  SetPrivilegesAttributes(SE_PRIVILEGE_ENABLED_BY_DEFAULT);
end;

procedure TDialogCreateToken.MenuEditClick(Sender: TObject);
var
  SelCount: Integer;
begin
  SelCount := ListViewGroups.SelCount;

  GroupsSource.UiEditSelected(Self);

  case SelCount of
    0:
      ; // Nothing to update
    1:
      UpdatePrimaryAndOwner(guEditOne);
  else
    UpdatePrimaryAndOwner(guEditMultiple);
  end;
end;

procedure TDialogCreateToken.MenuEnabledClick(Sender: TObject);
begin
 SetPrivilegesAttributes(SE_PRIVILEGE_ENABLED_BY_DEFAULT or
   SE_PRIVILEGE_ENABLED);
end;

procedure TDialogCreateToken.MenuEnabledModifClick(Sender: TObject);
begin
  SetPrivilegesAttributes(SE_PRIVILEGE_ENABLED);
end;

procedure TDialogCreateToken.MenuRemoveClick(Sender: TObject);
begin
  if Assigned(ListViewGroups.Selected) then
  begin
    GroupsSource.RemoveGroup(ListViewGroups.Selected.Index);
    UpdatePrimaryAndOwner(guRemove);
  end;
end;

procedure TDialogCreateToken.ObjPickerUserCallback(UserName: String);
begin
  ComboUser.Text := TSecurityIdentifier.CreateFromString(UserName).ToString;
  ComboUserChange(ButtonPickUser);
end;

procedure TDialogCreateToken.SetPrivilegesAttributes(NewValue: Cardinal);
var
  Priv: TPrivilege;
  i: Integer;
begin
  ListViewPrivileges.Items.BeginUpdate;
  for i := 0 to ListViewPrivileges.Items.Count - 1 do
    if ListViewPrivileges.Items[i].Selected then
    begin
      Priv := PrivilegesSource.Privilege[i];
      Priv.Attributes := NewValue;
      PrivilegesSource.Privilege[i] := Priv;
    end;
  ListViewPrivileges.Items.EndUpdate;
end;

procedure TDialogCreateToken.UpdatePrimaryAndOwner(Mode: TGroupUpdateType);
var
  i: Integer;
  SavedPrimaryIndex, SavedOwnerIndex, SavedOwnerCount: Integer;
  SavedPrimary, SavedOwner: String;
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

    for i := ComboOwner.Items.Count - 1 downto 1 do
      ComboOwner.Items.Delete(i);

    // Only groups with Owner flag can be assigned as owners
    for i := 0 to ListViewGroups.Items.Count - 1 do
      if GroupsSource.Group[i].Attributes.Contain(GroupOwner) then
        ComboOwner.Items.Add(ListViewGroups.Items[i].Caption);

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
    for i := 0 to ListViewGroups.Items.Count - 1 do
      ComboPrimary.Items.Add(ListViewGroups.Items[i].Caption);

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
