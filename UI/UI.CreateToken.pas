unit UI.CreateToken;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Prototypes.ChildForm, Vcl.StdCtrls,
  UI.Prototypes, UI.ListViewEx, Vcl.ComCtrls, UI.MainForm, UI.Modal.PickUser;

type
  TDialogCreateToken = class(TChildForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    ListViewGroups: TGroupListViewEx;
    ListViewPrivileges: TPrivilegesListViewEx;
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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCancelClick(Sender: TObject);
  private
    LogonIDSource: TLogonSessionSource;
  public
    { Public declarations }
  end;

var
  DialogCreateToken: TDialogCreateToken;

implementation

uses
  TU.LsaApi;

{$R *.dfm}

procedure TDialogCreateToken.ButtonCancelClick(Sender: TObject);
begin
  Close;
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

end.
