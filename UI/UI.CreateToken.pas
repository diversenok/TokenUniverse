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
    PrivilegesFrame: TPrivilegesFrame;
    GroupsFrame: TGroupsFrame;
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
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  UI.Modal.PickUser, TU.ObjPicker, TU.Winapi,
  UI.Settings, UI.Modal.PickToken, System.UITypes, NtUtils.Lsa.Sid,
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi, Ntapi.ntseapi, Ntapi.ntpebteb,
  NtUiLib.Exceptions, DelphiUiLib.Strings, DelphiUiLib.Reflection.Strings;

{$R *.dfm}

procedure TDialogCreateToken.AddGroup(NewGroup: TGroup);
begin
  GroupsFrame.Add([NewGroup]);

  if NewGroup.Attributes and SE_GROUP_OWNER <> 0 then
    ComboOwner.Items.Add(LsaxSidToString(NewGroup.Sid.Data));

  ComboPrimary.Items.Add(LsaxSidToString(NewGroup.Sid.Data));
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
  Source: IToken;
  Expiration: TDateTime;
begin
  Source := TDialogPickToken.Execute(Self);

  if not Source.InfoClass.Query(tdObjectInfo) or not
    (Source.InfoClass.ObjectInformation.GrantedAccess and TOKEN_QUERY <> 0) then
  begin
    MessageDlg('This token does not have Query access.', mtError, mbOKCancel,
      -1);
    Abort;
  end;

  // User
  if Source.InfoClass.Query(tdTokenUser) then
  begin
    ComboUser.Text := LsaxSidToString(
      Source.InfoClass.User.Sid.Data);
    ComboUserChange(Sender);
    CheckBoxUserState.Checked := Source.InfoClass.User.Attributes and
      SE_GROUP_USE_FOR_DENY_ONLY <> 0;
  end;

  // Logon ID & Expiration
  if Source.InfoClass.Query(tdTokenStatistics) then
  begin
    LogonIDSource.SelectedLogonSession :=
      Source.InfoClass.Statistics.AuthenticationId;

    CheckBoxInfinite.Checked := (
      Source.InfoClass.Statistics.ExpirationTime = Int64.MaxValue);

    if not CheckBoxInfinite.Checked then
    begin
      Expiration := LargeIntegerToDateTime(Source.InfoClass.Statistics.
        ExpirationTime);

      // Date only
      DateExpires.DateTime := Trunc(Expiration);

      // Time only
      TimeExpires.DateTime := Expiration - Trunc(Expiration);
    end;
  end;

  // Groups
  if Source.InfoClass.Query(tdTokenGroups) then
    GroupsFrame.Load(Source.InfoClass.Groups);

  // TODO: Owner and Primary group ItemIndexes are not updated properly

  // Owner
  if Source.InfoClass.Query(tdTokenOwner) then
    ComboOwner.Text := LsaxSidToString(Source.InfoClass.Owner.Data);

  // Primary group
  if Source.InfoClass.Query(tdTokenPrimaryGroup) then
    ComboPrimary.Text := LsaxSidToString(
      Source.InfoClass.PrimaryGroup.Data);

  // Privileges
  if Source.InfoClass.Query(tdTokenPrivileges) then
    PrivilegesFrame.Checked := Source.InfoClass.Privileges;

  // Source
  if Source.InfoClass.Query(tdTokenSource) then
  begin
    EditSourceName.Text := Source.InfoClass.Source.ToString;
    EditSourceLuid.Text := IntToHexEx(
      Source.InfoClass.Source.SourceIdentifier);
  end;
end;

procedure TDialogCreateToken.ButtonOKClick(Sender: TObject);
var
  Token: IToken;
  Expires: TLargeInteger;
  OwnerGroupName, PrimaryGroupName: String;
  NewPolicy: Cardinal;
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

  Source.FromString(EditSourceName.Text);
  Source.SourceIdentifier := StrToUInt64Ex(EditSourceLuid.Text, 'Source LUID');

  LsaxLookupNameOrSddl(ComboUser.Text, User).RaiseOnError;
  LsaxLookupNameOrSddl(OwnerGroupName, Owner).RaiseOnError;
  LsaxLookupNameOrSddl(PrimaryGroupName, PrimaryGroup).RaiseOnError;

  Token := TToken.CreateNtCreateToken(
    User,
    CheckBoxUserState.Checked,
    GroupsFrame.All,
    PrivilegesFrame.Checked,
    LogonIDSource.SelectedLogonSession,
    Owner,
    PrimaryGroup,
    Source,
    Expires
  );

  FormMain.TokenView.Add(Token);

  // Post-creation: change mandatory policy
  NewPolicy := 0;
  if CheckBoxNoWriteUp.Checked then
    NewPolicy := NewPolicy or TOKEN_MANDATORY_POLICY_NO_WRITE_UP;
  if CheckBoxNewProcMin.Checked then
    NewPolicy := NewPolicy or TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  if NewPolicy <> 0 then
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

constructor TDialogCreateToken.Create(AOwner: TComponent);
begin
  inherited CreateChild(AOwner, True);
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

  PrivilegesFrame.ColoringChecked := pcStateBased;
  PrivilegesFrame.ColoringUnChecked := pcRemoved;
  PrivilegesFrame.LoadEvery;
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
  ItemsToModify: Integer;
  AddAttributes, RemoveAttriutes: TGroupAttributes;
begin
  ItemsToModify := GroupsFrame.ListViewEx.SelCount;

  with GroupsFrame.ListViewEx do
  begin
    // Edit one group: SID and attributes
    if (SelCount = 1) and Assigned(Selected) then
      GroupsFrame[Selected.Index] := TDialogPickUser.PickEditOne(Self,
        GroupsFrame[Selected.Index]);

    // Edit multiple groups: only attributes
    if SelCount > 1 then
    begin
      TDialogPickUser.PickEditMultiple(Self, GroupsFrame.Selected, AddAttributes,
        RemoveAttriutes);

      GroupsFrame.UpdateSelected(AddAttributes, RemoveAttriutes);
    end;
  end;

  case ItemsToModify of
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
  GroupsFrame.RemoveSelected;
  UpdatePrimaryAndOwner(guRemove);
end;

procedure TDialogCreateToken.ObjPickerUserCallback(UserName: String);
var
  Sid: ISid;
begin
  LsaxLookupNameOrSddl(UserName, Sid).RaiseOnError;
  ComboUser.Text := LsaxSidToString(Sid.Data);
  ComboUserChange(ButtonPickUser);
end;

procedure TDialogCreateToken.SetPrivilegesAttributes(NewValue: Cardinal);
var
  i: Integer;
begin
  with PrivilegesFrame.ListViewEx do
  begin
    Items.BeginUpdate;

    for i := 0 to Pred(Items.Count) do
      if Items[i].Selected then
        PrivilegesFrame.UpdateState(i, NewValue);

    Items.EndUpdate;
  end;
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
    for i := 0 to Pred(GroupsFrame.ListViewEx.Items.Count) do
      if GroupsFrame.Group[i].Attributes and SE_GROUP_OWNER <> 0 then
        ComboOwner.Items.Add(GroupsFrame.ListViewEx.Items[i].Caption);

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
    for i := 0 to Pred(GroupsFrame.ListViewEx.Items.Count) do
      ComboPrimary.Items.Add(GroupsFrame.ListViewEx.Items[i].Caption);

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
