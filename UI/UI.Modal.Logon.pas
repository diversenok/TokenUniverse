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
    ListViewGroups: TGroupListViewEx;
    ButtonAddSID: TButton;
    LabelGroups: TLabel;
    PopupMenu: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
  private
    procedure TokenCreationCallback(Domain, User: String; Password: PWideChar);
    function GetLogonType: TLogonType;
    function GetLogonProvider: TLogonProvider;
  public
    { Public declarations }
  end;

implementation

uses
  TU.Common, TU.Credentials, TU.Tokens, UI.MainForm, UI.Modal.PickUser;

{$R *.dfm}

procedure TLogonDialog.ButtonAddSIDClick(Sender: TObject);
begin
  ListViewGroups.AddGroup(TDialogPickUser.Execute(Self));
  ButtonContinue.SetFocus;
end;

procedure TLogonDialog.ButtonContinueClick(Sender: TObject);
begin
  PromptCredentialsUI(Handle, TokenCreationCallback);
  ModalResult := mrOk;
end;

function TLogonDialog.GetLogonProvider: TLogonProvider;
begin
  Result := TLogonProvider(ComboLogonProvider.ItemIndex);
end;

function TLogonDialog.GetLogonType: TLogonType;
const
  LogonTypeMapping: array [0 .. 5] of TLogonType = (ltInteractive, ltBatch,
    ltNetwork, ltNetworkCleartext, ltNewCredentials, ltService);
begin
  // TODO: add Unlock logon type
  Result := LogonTypeMapping[ComboLogonType.ItemIndex];
end;

procedure TLogonDialog.MenuEditClick(Sender: TObject);
var
  Ind: Integer;
begin
  if Assigned(ListViewGroups.Selected) then
  begin
    Ind := ListViewGroups.Selected.Index;
    ListViewGroups.Groups[Ind] := TDialogPickUser.Execute(Self,
      ListViewGroups.Groups[Ind]);
  end;
end;

procedure TLogonDialog.MenuRemoveClick(Sender: TObject);
begin
  if Assigned(ListViewGroups.Selected) then
    ListViewGroups.RemoveGroup(ListViewGroups.Selected.Index);
end;

procedure TLogonDialog.TokenCreationCallback(Domain, User: String;
  Password: PWideChar);
begin
  FormMain.Frame.AddToken(TToken.CreateWithLogon(GetLogonType, GetLogonProvider,
    Domain, User, Password, ListViewGroups.AllGroups));
end;

end.
