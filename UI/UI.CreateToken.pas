unit UI.CreateToken;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.Forms,
  UI.Prototypes, VclEx.ListView, UI.MainForm, TU.Tokens, TU.Tokens.Old.Types,
  NtUtils.Security.Sid, UI.Prototypes.Privileges, UI.Prototypes.Groups,
  NtUtils, UI.Prototypes.Sid.Edit;

type
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
    PopupMenuGroups: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    ComboOwner: TComboBox;
    ComboPrimary: TComboBox;
    CheckBoxUserState: TCheckBox;
    GroupBoxUser: TGroupBox;
    PopupMenuPrivileges: TPopupMenu;
    MenuDisabled: TMenuItem;
    MenuDisabledModif: TMenuItem;
    MenuEnabled: TMenuItem;
    MenuEnabledModif: TMenuItem;
    ButtonLoad: TButton;
    GroupBoxPostCreation: TGroupBox;
    CheckBoxNoWriteUp: TCheckBox;
    CheckBoxNewProcMin: TCheckBox;
    CheckBoxSession: TCheckBox;
    GroupsFrame: TFrameGroups;
    PrivilegesFrame: TFramePrivileges;
    ButtonPickUser: TButton;
    SidEditor: TSidEditor;
    GroupBoxSource: TGroupBox;
    EditSourceName: TEdit;
    StaticSourceName: TStaticText;
    StaticSourceLuid: TStaticText;
    EditSourceLuid: TEdit;
    ButtonAllocLuid: TButton;
    GroupBoxExpires: TGroupBox;
    CheckBoxInfinite: TCheckBox;
    DateExpires: TDateTimePicker;
    TimeExpires: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxInfiniteClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure ButtonAllocLuidClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure MenuDisabledClick(Sender: TObject);
    procedure MenuDisabledModifClick(Sender: TObject);
    procedure MenuEnabledClick(Sender: TObject);
    procedure MenuEnabledModifClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    LogonIDSource: TLogonSessionSource;
    PotentialOwners: TArray<TGroup>;
    PotentialPrimary: TArray<TGroup>;
    function FindGroupIndex(const Groups: TArray<TGroup>; const Sid: ISid): Integer;
    function RetrieveGroup(const Groups: TArray<TGroup>; Index: Integer): ISid;
    procedure ChangedGroups;
    procedure ChangedGroupsInternal(
      var Groups: TArray<TGroup>;
      const NewGroups: TArray<TGroup>;
      ComboBox: TComboBox
    );
    procedure AddGroup(const NewGroup: TGroup);
    procedure EditSingleGroup(const Value: TGroup);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  UI.Modal.PickUser, TU.Winapi, VirtualTrees, UI.Settings, UI.Modal.PickToken,
  System.UITypes, NtUtils.Lsa.Sid, Ntapi.WinNt, Ntapi.ntexapi, Ntapi.ntseapi,
  Ntapi.ntpebteb, NtUtils.Errors, NtUiLib.Errors, DelphiUiLib.Strings,
  DelphiUiLib.Reflection.Strings, DelphiUtils.Arrays, TU.Tokens.Open,
  NtUtils.Tokens.Info;

{$R *.dfm}

procedure TDialogCreateToken.AddGroup;
begin
  GroupsFrame.Add([NewGroup]);
  ChangedGroups;
end;

procedure TDialogCreateToken.ButtonAddSIDClick;
begin
  AddGroup(TDialogPickUser.PickNew(Self));
end;

procedure TDialogCreateToken.ButtonAllocLuidClick;
var
  NewLuid: TLuid;
begin
  if NtAllocateLocallyUniqueId(NewLuid).IsSuccess then
    EditSourceLuid.Text := IntToHexEx(NewLuid);
end;

procedure TDialogCreateToken.ButtonCancelClick;
begin
  Close;
end;

procedure TDialogCreateToken.ButtonLoadClick;
var
  Source: IToken;
  Expiration: TDateTime;
  User: TGroup;
  Statistics: TTokenStatistics;
  Groups: TArray<TGroup>;
  Owner, PrimaryGroup: ISid;
  Privileges: TArray<TPrivilege>;
  TokenSource: TTokenSource;
begin
  Source := TDialogPickToken.Execute(Self);

  // User
  Source.QueryUser(User).RaiseOnError;
  SidEditor.Sid := User.Sid;
  CheckBoxUserState.Checked := BitTest(User.Attributes and
    SE_GROUP_USE_FOR_DENY_ONLY);

  // Logon ID & Expiration
  Source.QueryStatistics(Statistics).RaiseOnError;
  LogonIDSource.SelectedLogonSession := Statistics.AuthenticationId;
  CheckBoxInfinite.Checked := (Statistics.ExpirationTime = Int64.MaxValue);
  if not CheckBoxInfinite.Checked then
  begin
    Expiration := LargeIntegerToDateTime(Statistics.ExpirationTime);
    DateExpires.DateTime := Trunc(Expiration);
    TimeExpires.DateTime := Expiration - Trunc(Expiration);
  end;

  // Groups
  Source.QueryGroups(Groups).RaiseOnError;
  GroupsFrame.Load(Groups);
  ChangedGroups;

  // Owner
  Source.QueryOwner(Owner).RaiseOnError;
  ComboOwner.ItemIndex := FindGroupIndex(PotentialOwners, Owner) + 1;

  // Primary group
  Source.QueryPrimaryGroup(PrimaryGroup).RaiseOnError;
  ComboPrimary.ItemIndex := FindGroupIndex(PotentialPrimary, PrimaryGroup) + 1;

  // Privileges
  Source.QueryPrivileges(Privileges).RaiseOnError;
  PrivilegesFrame.Checked := Privileges;

  // Source
  Source.QuerySource(TokenSource).RaiseOnError;
  EditSourceName.Text := TokenSource.Name;
  EditSourceLuid.Text := IntToHexEx(TokenSource.SourceIdentifier);
end;

procedure TDialogCreateToken.ButtonOKClick;
var
  Token: IToken;
  Expires: TLargeInteger;
  NewPolicy: TTokenMandatoryPolicy;
  Source: TTokenSource;
  User, Owner, PrimaryGroup: ISid;
  DefaultDacl: IAcl;
  LogonSession: TLogonId;
begin
  if CheckBoxInfinite.Checked then
    Expires := Int64.MaxValue
  else if TimeExpires.Checked then
    Expires := DateTimeToLargeInteger(DateExpires.Date + TimeExpires.Time)
  else
    Expires := DateTimeToLargeInteger(DateExpires.Date);

  User := SidEditor.Sid;
  Owner := RetrieveGroup(PotentialOwners, ComboOwner.ItemIndex);
  PrimaryGroup := RetrieveGroup(PotentialPrimary, ComboPrimary.ItemIndex);
  Source.Name := EditSourceName.Text;
  Source.SourceIdentifier := StrToUInt64Ex(EditSourceLuid.Text, 'Source LUID');
  LogonSession := LogonIDSource.SelectedLogonSession;

  MakeNewToken(Token, ttPrimary, User, CheckBoxUserState.Checked,
    GroupsFrame.All, PrivilegesFrame.Checked, LogonSession,
    PrimaryGroup, Source, Owner, nil, Expires).RaiseOnError;

  FormMain.TokenView.Add(Token);

  // Post-creation: change mandatory policy
  NewPolicy := 0;
  if CheckBoxNoWriteUp.Checked then
    NewPolicy := NewPolicy or TOKEN_MANDATORY_POLICY_NO_WRITE_UP;
  if CheckBoxNewProcMin.Checked then
    NewPolicy := NewPolicy or TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN;

  if NewPolicy <> 0 then
    Token.SetMandatoryPolicy(NewPolicy).RaiseOnError;

  // Post-creation: change session
  if CheckBoxSession.Checked then
    Token.SetSessionId(RtlGetCurrentPeb.SessionId).RaiseOnError;

  // TODO: add default DACL support in token creation UI
  // For now, just set a sensible default instead of empty one
  if NtxMakeDefaultDaclToken(Token.Handle, DefaultDacl).IsSuccess then
    Token.SetDefaultDacl(DefaultDacl);

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogCreateToken.ChangedGroupsInternal;
var
  Old: ISid;
  i: Integer;
  Lookup: TArray<TTranslatedGroup>;
begin
  // Save the current state
  Old := RetrieveGroup(Groups, ComboBox.ItemIndex);

  // Reset the list
  ComboBox.Items.BeginUpdate;
  Auto.Delay(
    procedure
    begin
      ComboBox.Items.EndUpdate;
    end
  );
  ComboBox.Items.Clear;
  ComboBox.Items.Add('< Same as user >');

  // Add new groups
  Groups := NewGroups;
  LsaxLookupGroups(Groups, Lookup);
  for i := 0 to High(Groups) do
    ComboBox.Items.Add(Lookup[i].Name.FullName);

  // Restore selection
  ComboBox.ItemIndex := FindGroupIndex(Groups, Old) + 1;
end;

procedure TDialogCreateToken.ChangedGroups;
begin
  ChangedGroupsInternal(PotentialPrimary, GroupsFrame.All, ComboPrimary);
  ChangedGroupsInternal(PotentialOwners, TArray.Filter<TGroup>(GroupsFrame.All,
    function (const Group: TGroup): Boolean
    begin
      Result := BitTest(Group.Attributes and SE_GROUP_OWNER);
    end
  ), ComboOwner);
end;

procedure TDialogCreateToken.CheckBoxInfiniteClick;
begin
  DateExpires.Enabled := not CheckBoxInfinite.Checked;
  TimeExpires.Enabled := not CheckBoxInfinite.Checked;
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

function TDialogCreateToken.FindGroupIndex;
var
  i: Integer;
begin
  for i := 0 to High(Groups) do
    if RtlxEqualSids(Groups[i].Sid, Sid) then
      Exit(i);

  Result := -1;
end;

procedure TDialogCreateToken.FormClose;
begin
  LogonIDSource.Free;
end;

procedure TDialogCreateToken.FormCreate;
begin
  LogonIDSource := TLogonSessionSource.Create(ComboLogonSession);
  LogonIDSource.SelectedLogonSession := ANONYMOUS_LOGON_LUID;
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
  if GroupsFrame.VST.SelectedCount <= 0 then
    Exit;

  if GroupsFrame.VST.SelectedCount = 1 then
    EditSingleGroup(Default(TGroup))
  else
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

  ChangedGroups;
end;

procedure TDialogCreateToken.MenuEnabledClick;
begin
 PrivilegesFrame.AdjustSelected(SE_PRIVILEGE_ENABLED_BY_DEFAULT or
   SE_PRIVILEGE_ENABLED);
end;

procedure TDialogCreateToken.MenuEnabledModifClick;
begin
  PrivilegesFrame.AdjustSelected(SE_PRIVILEGE_ENABLED);
end;

procedure TDialogCreateToken.MenuRemoveClick;
begin
  GroupsFrame.VST.DeleteSelectedNodes;
  ChangedGroups;
end;

function TDialogCreateToken.RetrieveGroup;
begin
  if Index > 0 then
    Result := Groups[Index - 1].Sid
  else
    Result := SidEditor.Sid
end;

end.
