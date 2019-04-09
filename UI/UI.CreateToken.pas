unit UI.CreateToken;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.ChildForm,
  UI.Prototypes, UI.ListViewEx, UI.MainForm, TU.Tokens, TU.Tokens.Types,
  NtUtils.Types, UI.Prototypes.Privileges, UI.Prototypes.Groups;

type
  TGroupUpdateType = (guEditOne, guEditMultiple, guRemove);

  TDialogCreateToken = class(TChildTaskbarForm)
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
    FramePrivileges: TFramePrivileges;
    FrameGroups: TFrameGroups;
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
    procedure ObjPickerUserCallback(UserName: String);
    procedure AddGroup(NewGroup: TGroup);
    procedure UpdatePrimaryAndOwner(Mode: TGroupUpdateType);
    procedure SetPrivilegesAttributes(NewValue: Cardinal);
  end;

implementation

uses
  TU.LsaApi, UI.Modal.PickUser, TU.ObjPicker, TU.Winapi, DelphiUtils.Strings,
  UI.Settings, UI.Modal.PickToken, System.UITypes,
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi, Ntapi.ntseapi, Ntapi.ntrtl;

{$R *.dfm}

procedure TDialogCreateToken.AddGroup(NewGroup: TGroup);
begin
  FrameGroups.AddGroup(NewGroup);

  if Contains(NewGroup.Attributes, SE_GROUP_OWNER) then
    ComboOwner.Items.Add(NewGroup.SecurityIdentifier.Lookup.FullName);

  ComboPrimary.Items.Add(NewGroup.SecurityIdentifier.Lookup.FullName);
end;

procedure TDialogCreateToken.ButtonAddSIDClick(Sender: TObject);
begin
  AddGroup(TDialogPickUser.PickNew(Self));
end;

procedure TDialogCreateToken.ButtonAllocLuidClick(Sender: TObject);
var
  NewLuid: TLuid;
begin
  if NT_SUCCESS(NtAllocateLocallyUniqueId(NewLuid)) then
    EditSourceLuid.Text := IntToHexEx(NewLuid);
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
    ComboUser.Text := Source.InfoClass.User.SecurityIdentifier.Lookup.FullName;
    ComboUserChange(Sender);
    CheckBoxUserState.Checked := Contains(Source.InfoClass.User.Attributes,
      SE_GROUP_USE_FOR_DENY_ONLY);
  end;

  // Logon ID & Expiration
  if Source.InfoClass.Query(tdTokenStatistics) then
  begin
    LogonIDSource.SelectedLogonSession :=
      Source.InfoClass.Statistics.AuthenticationId;

    CheckBoxInfinite.Checked := (
      Source.InfoClass.Statistics.ExpirationTime.QuadPart = Int64.MaxValue);

    if not CheckBoxInfinite.Checked then
    begin
      // TODO: extract date/time only

      DateExpires.DateTime :=
        Source.InfoClass.Statistics.ExpirationTime.ToDateTime;

      TimeExpires.DateTime := DateExpires.DateTime;
    end;
  end;

  // Groups
  if Source.InfoClass.Query(tdTokenGroups) then
  with FrameGroups do
    begin
      ListView.Items.BeginUpdate;

      Clear;
      for i := 0 to High(Source.InfoClass.Groups) do
        AddGroup(Source.InfoClass.Groups[i]);

      ListView.Items.EndUpdate;
    end;

  // TODO: Owner and Primary group ItemIndexes are not updated properly

  // Owner
  if Source.InfoClass.Query(tdTokenOwner) then
    ComboOwner.Text := Source.InfoClass.Owner.Lookup.FullName;

  // Primary group
  if Source.InfoClass.Query(tdTokenPrimaryGroup) then
    ComboPrimary.Text := Source.InfoClass.PrimaryGroup.Lookup.FullName;

  // Privileges
  if Source.InfoClass.Query(tdTokenPrivileges) then
  begin
    FramePrivileges.ListView.Items.BeginUpdate;

    // Uncheck everything
    for i := 0 to FramePrivileges.ListView.Items.Count - 1 do
      FramePrivileges.ListView.Items[i].Checked := False;

    for i := 0 to High(Source.InfoClass.Privileges) do
    begin
      // Locate a privilege from the token in the list
      j := FramePrivileges.Find(Source.InfoClass.Privileges[i].Luid);

      // Set appropriate state and check it
      if j = -1 then
        // Add if necessary
        FramePrivileges.AddPrivilege(Source.InfoClass.Privileges[i]).Checked :=
          True
      else
      begin
        FramePrivileges.Privilege[j] := Source.InfoClass.Privileges[i];
        FramePrivileges.ListView.Items[j].Checked := True;
      end;
    end;

    FramePrivileges.ListView.Items.EndUpdate;
  end;

  // Source
  if Source.InfoClass.Query(tdTokenSource) then
  begin
    EditSourceName.Text := TokeSourceNameToString(Source.InfoClass.Source);
    EditSourceLuid.Text := IntToHexEx(
      Source.InfoClass.Source.SourceIdentifier);
  end;
end;

procedure TDialogCreateToken.ButtonOKClick(Sender: TObject);
var
  Token: TToken;
  Expires: TLargeInteger;
  OwnerGroupName, PrimaryGroupName: String;
  NewPolicy: TMandatoryPolicy;
begin
  if CheckBoxInfinite.Checked then
    Expires.QuadPart := Int64.MaxValue
  else if TimeExpires.Checked then
    Expires.FromDateTime(DateExpires.Date + TimeExpires.Time)
  else
    Expires.FromDateTime(DateExpires.Date);

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
    TSid.CreateFromString(ComboUser.Text),
    CheckBoxUserState.Checked,
    FrameGroups.Groups,
    FramePrivileges.CheckedPrivileges,
    LogonIDSource.SelectedLogonSession,
    TSid.CreateFromString(OwnerGroupName),
    TSid.CreateFromString(PrimaryGroupName),
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
    Token.InfoClass.Session := RtlGetCurrentPeb.SessionId;

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
  LogonIDSource.Free;
end;

procedure TDialogCreateToken.FormCreate(Sender: TObject);
begin
  LogonIDSource := TLogonSessionSource.Create(ComboLogonSession);
  CheckBoxSession.Enabled := RtlGetCurrentPeb.SessionId <> 0;
  ButtonAllocLuidClick(Self);

  FramePrivileges.AddAllPrivileges;
  FramePrivileges.ColorMode := pmGrayUnchecked;
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
  SelCount := FrameGroups.ListView.SelCount;

  FrameGroups.UiEditSelected(Self);

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
  if Assigned(FrameGroups.ListView.Selected) then
  begin
    FrameGroups.RemoveGroup(FrameGroups.ListView.Selected.Index);
    UpdatePrimaryAndOwner(guRemove);
  end;
end;

procedure TDialogCreateToken.ObjPickerUserCallback(UserName: String);
var
  Sid: ISid;
begin
  Sid := TSid.CreateFromString(UserName);
  ComboUser.Text := Sid.Lookup.FullName;
  ComboUserChange(ButtonPickUser);
end;

procedure TDialogCreateToken.SetPrivilegesAttributes(NewValue: Cardinal);
var
  Priv: TPrivilege;
  i: Integer;
begin
  FramePrivileges.ListView.Items.BeginUpdate;

  for i := 0 to FramePrivileges.ListView.Items.Count - 1 do
    if FramePrivileges.ListView.Items[i].Selected then
    begin
      Priv := FramePrivileges.Privilege[i];
      Priv.Attributes := NewValue;
      FramePrivileges.Privilege[i] := Priv;
    end;

  FramePrivileges.ListView.Items.EndUpdate;
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
    for i := 0 to FrameGroups.ListView.Items.Count - 1 do
      if Contains(FrameGroups.Group[i].Attributes, SE_GROUP_OWNER) then
        ComboOwner.Items.Add(FrameGroups.ListView.Items[i].Caption);

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
    for i := 0 to FrameGroups.ListView.Items.Count - 1 do
      ComboPrimary.Items.Add(FrameGroups.ListView.Items[i].Caption);

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
