unit UI.CreateToken;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Prototypes.ChildForm, Vcl.StdCtrls,
  UI.Prototypes, UI.ListViewEx, Vcl.ComCtrls, UI.MainForm, Vcl.Menus, TU.Tokens;

type
  TDialogCreateToken = class(TChildForm)
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
    StaticDacl: TStaticText;
    ComboLogonSession: TComboBox;
    ComboUser: TComboBox;
    ButtonPickUser: TButton;
    ButtonLoad: TButton;
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
  private
    LogonIDSource: TLogonSessionSource;
    GroupsSource: TGroupsSource;
    PrivilegesSource: TPrivilegesSource;
    procedure ObjPickerUserCallback(UserName: String);
  end;

var
  DialogCreateToken: TDialogCreateToken;

implementation

uses
  TU.LsaApi, TU.Tokens.Types, UI.Modal.PickUser, TU.ObjPicker, TU.Winapi,
  TU.Common;

{$R *.dfm}

procedure TDialogCreateToken.ButtonAddSIDClick(Sender: TObject);
begin
  GroupsSource.AddGroup(TDialogPickUser.PickNew(Self));
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

procedure TDialogCreateToken.ButtonOKClick(Sender: TObject);
var
  Token: TToken;
  Expires: Int64;
begin
  if CheckBoxInfinite.Checked then
    Expires := Int64.MaxValue
  else if TimeExpires.Checked then
    Expires := DateTimeToNative(DateExpires.Date + TimeExpires.Time)
  else
    Expires := DateTimeToNative(DateExpires.Date);

  Token := TToken.CreateNtCreateToken(
    TSecurityIdentifier.CreateFromString(ComboUser.Text),
    CheckBoxUserState.Checked,
    GroupsSource.Groups,
    PrivilegesSource.Privileges,
    LogonIDSource.SelectedLogonSession,
    TSecurityIdentifier.CreateFromString(ComboOwner.Text),
    TSecurityIdentifier.CreateFromString(ComboPrimary.Text),
    CreateTokenSource(EditSourceName.Text,
      StrToUInt64Ex(EditSourceLuid.Text, 'Source LUID')),
    Expires
  );

  FormMain.Frame.AddToken(Token);
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
  ButtonAllocLuidClick(Self);
end;

procedure TDialogCreateToken.MenuEditClick(Sender: TObject);
begin
  GroupsSource.UiEditSelected(Self);
end;

procedure TDialogCreateToken.MenuRemoveClick(Sender: TObject);
begin
  if Assigned(ListViewGroups.Selected) then
    GroupsSource.RemoveGroup(ListViewGroups.Selected.Index);
end;

procedure TDialogCreateToken.ObjPickerUserCallback(UserName: String);
begin
  ComboUser.Text := TSecurityIdentifier.CreateFromString(UserName).ToString;
end;

end.
