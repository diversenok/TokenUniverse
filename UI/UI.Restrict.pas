unit UI.Restrict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.CheckLst, TU.Common, TU.Tokens, Vcl.ComCtrls, UI.ListViewEx, UI.Prototypes;

type
  TDialogRestrictToken = class(TForm)
    CheckBoxDisableMaxPriv: TCheckBox;
    CheckBoxSandboxInert: TCheckBox;
    CheckBoxLUA: TCheckBox;
    CheckBoxWriteRestrict: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl1: TPageControl;
    TabSheetSidDisable: TTabSheet;
    TabSheetSidRestict: TTabSheet;
    TabSheetPrivDelete: TTabSheet;
    ListViewPrivileges: TPrivilegesListViewEx;
    ListViewRestrictSID: TGroupListViewEx;
    ListViewDisableSID: TGroupListViewEx;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    Token: TToken;
    procedure ConfirmTokenClose(Sender: TToken);
    function GetFlags: Cardinal;
  public
    procedure Refresh;
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonOKClick(Sender: TObject);
begin
  FormMain.Frame.AddToken(TToken.CreateRestricted(Token, GetFlags,
    ListViewDisableSID.CheckedGroups,
    ListViewRestrictSID.CheckedGroups,
    ListViewPrivileges.CheckedPrivileges));
  Close;
end;

procedure TDialogRestrictToken.ConfirmTokenClose(Sender: TToken);
const
  CONFIRM_CLOSE = 'This token has an opened "Create restricted token" ' +
    'dialog window for it. Do you want close it?';
begin
  // The main window should not close the token until any windows are opened for it.
  if MessageDlg(CONFIRM_CLOSE, mtConfirmation, mbYesNoCancel, 0) = IDYES then
    Close
  else
    Abort;
end;

constructor TDialogRestrictToken.CreateFromToken(AOwner: TComponent;
  SrcToken: TToken);
begin
  Token := SrcToken;
  inherited Create(AOwner);
  Show;
end;

procedure TDialogRestrictToken.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TDialogRestrictToken.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Token.OnCanClose.Delete(ConfirmTokenClose);
  FormMain.OnMainFormClose.Delete(DoCloseForm);
  Action := caFree;
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
begin
  FormMain.OnMainFormClose.Add(DoCloseForm);
  Token.OnCanClose.Add(ConfirmTokenClose);

  ListViewDisableSID.Token := Token;
  ListViewRestrictSID.Token := Token;
  ListViewPrivileges.Token := Token;

  Refresh;
end;

function TDialogRestrictToken.GetFlags: Cardinal;
const
  DISABLE_MAX_PRIVILEGE = 1;
  SANDBOX_INERT = 2;
  LUA_TOKEN = 4;
  WRITE_RESTRICTED = 8; // TODO: Check windows version, this value depends on it
begin
  Result := 0;
  if CheckBoxDisableMaxPriv.Checked then
    Result := Result or DISABLE_MAX_PRIVILEGE;
  if CheckBoxSandboxInert.Checked then
    Result := Result or SANDBOX_INERT;
  if CheckBoxLUA.Checked then
    Result := Result or LUA_TOKEN;
  if CheckBoxWriteRestrict.Checked then
    Result := Result or WRITE_RESTRICTED;
end;

procedure TDialogRestrictToken.Refresh;
begin
  //
end;

end.
