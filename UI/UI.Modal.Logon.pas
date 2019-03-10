unit UI.Modal.Logon;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.ChildForm, Vcl.ComCtrls,
  UI.ListViewEx, UI.Prototypes, TU.LsaApi, Winapi.WinBase;

type
  TLogonDialog = class(TChildForm)
    ComboLogonType: TComboBox;
    ComboLogonProvider: TComboBox;
    LabelType: TLabel;
    LabelProvider: TLabel;
    ButtonCancel: TButton;
    ButtonContinue: TButton;
    ListViewGroups: TListViewEx;
    ButtonAddSID: TButton;
    LabelGroups: TLabel;
    PopupMenu: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    GroupBoxSource: TGroupBox;
    EditSourceName: TEdit;
    StaticSourceName: TStaticText;
    StaticSourceLuid: TStaticText;
    EditSourceLuid: TEdit;
    ButtonAllocLuid: TButton;
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboLogonProviderChange(Sender: TObject);
    procedure ButtonAllocLuidClick(Sender: TObject);
  private
    GroupsSource: TGroupsSource;
    procedure TokenCreationCallback(Domain, User: String; Password: PWideChar);
    function GetLogonType: TSecurityLogonType;
    function GetLogonProvider: TLogonProvider;
  end;

implementation

uses
  TU.Common, TU.Credentials, TU.Tokens, UI.MainForm, UI.Modal.PickUser,
  TU.Tokens.Types, Winapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi;

{$R *.dfm}

const
  S4U_INDEX = 5; // Make sure to be consisten with the combobox

procedure TLogonDialog.ButtonAddSIDClick(Sender: TObject);
begin
  GroupsSource.AddGroup(TDialogPickUser.PickNew(Self));
  ButtonContinue.SetFocus;
end;

procedure TLogonDialog.ButtonAllocLuidClick(Sender: TObject);
var
  NewLuid: TLuid;
begin
  if NT_SUCCESS(NtAllocateLocallyUniqueId(NewLuid)) then
    EditSourceLuid.Text := Format('0x%x', [NewLuid]);
end;

procedure TLogonDialog.ButtonContinueClick(Sender: TObject);
begin
  PromptCredentialsUI(Handle, TokenCreationCallback,
    ComboLogonProvider.ItemIndex = S4U_INDEX);
  ModalResult := mrOk;
end;

procedure TLogonDialog.ComboLogonProviderChange(Sender: TObject);
begin
  EditSourceName.Enabled := (ComboLogonProvider.ItemIndex = S4U_INDEX);
  EditSourceLuid.Enabled := EditSourceName.Enabled;
  ButtonAllocLuid.Enabled := EditSourceName.Enabled;
end;

procedure TLogonDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GroupsSource.Free;
end;

procedure TLogonDialog.FormCreate(Sender: TObject);
begin
  GroupsSource := TGroupsSource.Create(ListViewGroups);
  ButtonAllocLuidClick(Sender);
end;

function TLogonDialog.GetLogonProvider: TLogonProvider;
begin
  Result := TLogonProvider(ComboLogonProvider.ItemIndex);
end;

function TLogonDialog.GetLogonType: TSecurityLogonType;
const
  LogonTypeMapping: array [0 .. 6] of TSecurityLogonType = (ltInteractive,
    ltNetwork, ltNetworkCleartext, ltNewCredentials, ltUnlock, ltBatch,
    ltService);
begin
  Result := LogonTypeMapping[ComboLogonType.ItemIndex];
end;

procedure TLogonDialog.MenuEditClick(Sender: TObject);
begin
  GroupsSource.UiEditSelected(Self);
end;

procedure TLogonDialog.MenuRemoveClick(Sender: TObject);
begin
  if Assigned(ListViewGroups.Selected) then
    GroupsSource.RemoveGroup(ListViewGroups.Selected.Index);
end;

procedure TLogonDialog.TokenCreationCallback(Domain, User: String;
  Password: PWideChar);
var
  Source: TTokenSource;
begin
  if ComboLogonProvider.ItemIndex = S4U_INDEX then
  begin
    // Use Services 4 Users logon
    Source := CreateTokenSource(EditSourceName.Text, StrToUInt64Ex(
      EditSourceLuid.Text, 'Source LUID'));

    FormMain.TokenView.Add(TToken.CreateS4ULogon(GetLogonType, Domain, User,
      Source, GroupsSource.Groups));
  end
  else
    FormMain.TokenView.Add(TToken.CreateWithLogon(GetLogonType,
      GetLogonProvider, Domain, User, Password, GroupsSource.Groups));
end;

end.
