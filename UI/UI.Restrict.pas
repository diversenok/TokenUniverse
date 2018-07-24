unit UI.Restrict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.CheckLst, TU.Common, TU.Tokens, Vcl.ComCtrls;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
  private
    Token: TToken;
    procedure ConfirmTokenClose(Sender: TObject);
    procedure ChangedPrivileges(Sender: TObject);
    procedure ChangedGroups(Sender: TObject);
  public
    procedure Refresh;
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ChangedGroups(Sender: TObject);
begin

end;

procedure TDialogRestrictToken.ChangedPrivileges(Sender: TObject);
begin

end;

procedure TDialogRestrictToken.ConfirmTokenClose(Sender: TObject);
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
  Token.OnPrivilegesChange.Delete(ChangedPrivileges);
  FormMain.OnMainFormClose.Delete(DoCloseForm);
  Action := caFree;
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
begin
  FormMain.OnMainFormClose.Add(DoCloseForm);
  Token.OnPrivilegesChange.Add(ChangedPrivileges);
  Refresh;
end;

procedure TDialogRestrictToken.Refresh;
begin
  ChangedPrivileges(Token);
  ChangedGroups(Token);
end;

end.
