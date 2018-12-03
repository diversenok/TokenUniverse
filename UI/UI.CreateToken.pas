unit UI.CreateToken;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Prototypes.ChildForm, Vcl.StdCtrls,
  UI.Prototypes, UI.ListViewEx, Vcl.ComCtrls, UI.MainForm, Vcl.Menus;

type
  TDialogCreateToken = class(TChildForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    ListViewGroups: TGroupListViewEx;
    ListViewPrivileges: TListViewEx;
    ButtonAddSID: TButton;
    ComboBoxView: TComboBox;
    StaticLogonID: TStaticText;
    StaticOwner: TStaticText;
    StaticPrimaryGroup: TStaticText;
    StaticDacl: TStaticText;
    StaticSource: TStaticText;
    EditSource: TEdit;
    ComboLogonSession: TComboBox;
    StaticTokenType: TStaticText;
    ComboTokenType: TComboBox;
    ComboUser: TComboBox;
    StaticUser: TStaticText;
    ButtonPickUser: TButton;
    ButtonLoad: TButton;
    PopupMenuGroups: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure ButtonPickUserClick(Sender: TObject);
  private
    LogonIDSource: TLogonSessionSource;
    procedure ObjPickerCallback(UserName: String);
  public
    { Public declarations }
  end;

var
  DialogCreateToken: TDialogCreateToken;

implementation

uses
  TU.LsaApi, TU.Tokens, TU.Tokens.Types, UI.Modal.PickUser, TU.ObjPicker;

{$R *.dfm}

procedure TDialogCreateToken.ButtonAddSIDClick(Sender: TObject);
begin
  ListViewGroups.AddGroup(TDialogPickUser.Execute(Self));
end;

procedure TDialogCreateToken.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogCreateToken.ButtonPickUserClick(Sender: TObject);
begin
  CallObjectPicker(Handle, ObjPickerCallback);
end;

procedure TDialogCreateToken.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  LogonIDSource.Free;
end;

procedure TDialogCreateToken.FormCreate(Sender: TObject);
begin
  LogonIDSource := TLogonSessionSource.Create(ComboLogonSession);
end;

procedure TDialogCreateToken.ObjPickerCallback(UserName: String);
begin
  ComboUser.Text := TSecurityIdentifier.CreateFromString(UserName).ToString;
end;

end.
