unit UI.Modal.Logon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  UI.Prototypes.ChildForm, Vcl.ComCtrls, UI.ListViewEx, UI.Prototypes,
  Vcl.Menus, TU.LsaApi;

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
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    GroupsSource: TGroupsSource;
    procedure TokenCreationCallback(Domain, User: String; Password: PWideChar);
    function GetLogonType: TLogonType;
    function GetLogonProvider: TLogonProvider;
  end;

implementation

uses
  TU.Common, TU.Credentials, TU.Tokens, UI.MainForm, UI.Modal.PickUser;

{$R *.dfm}

procedure TLogonDialog.ButtonAddSIDClick(Sender: TObject);
begin
  GroupsSource.AddGroup(TDialogPickUser.Execute(Self));
  ButtonContinue.SetFocus;
end;

procedure TLogonDialog.ButtonContinueClick(Sender: TObject);
begin
  PromptCredentialsUI(Handle, TokenCreationCallback);
  ModalResult := mrOk;
end;

procedure TLogonDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GroupsSource.Free;
end;

procedure TLogonDialog.FormCreate(Sender: TObject);
begin
  GroupsSource := TGroupsSource.Create(ListViewGroups);
end;

function TLogonDialog.GetLogonProvider: TLogonProvider;
begin
  Result := TLogonProvider(ComboLogonProvider.ItemIndex);
end;

function TLogonDialog.GetLogonType: TLogonType;
const
  LogonTypeMapping: array [0 .. 6] of TLogonType = (ltInteractive, ltNetwork,
    ltNetworkCleartext, ltNewCredentials, ltUnlock, ltBatch, ltService);
begin
  Result := LogonTypeMapping[ComboLogonType.ItemIndex];
end;

procedure TLogonDialog.MenuEditClick(Sender: TObject);
var
  Ind: Integer;
begin
  // TODO: modifying attributes of multiple groups simultaneously
  if Assigned(ListViewGroups.Selected) then
  begin
    Ind := ListViewGroups.Selected.Index;
    GroupsSource.Group[Ind] := TDialogPickUser.Execute(Self,
      GroupsSource.Group[Ind]);
  end;
end;

procedure TLogonDialog.MenuRemoveClick(Sender: TObject);
begin
  if Assigned(ListViewGroups.Selected) then
    GroupsSource.RemoveGroup(ListViewGroups.Selected.Index);
end;

procedure TLogonDialog.TokenCreationCallback(Domain, User: String;
  Password: PWideChar);
begin
  FormMain.Frame.AddToken(TToken.CreateWithLogon(GetLogonType, GetLogonProvider,
    Domain, User, Password, GroupsSource.Groups));
end;

end.
