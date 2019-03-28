unit UI.Prototypes.ChildForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, TU.Tokens;

type
  TChildForm = class(TForm)
  private
    FConfirmComment: String;
    procedure ConfirmTokenClose(Sender: TToken);
    procedure OnMainFormClose(Sender: TObject);
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoCreate; override;
  public
    procedure SubscribeTokenCanClose(Token: TToken; ConfirmComment: String);
    procedure UnsubscribeTokenCanClose(Token: TToken);
    function ShowModal: Integer; override;
  end;

  TChildTaskbarForm = class(TChildForm)
  private
    const idOnTop = 10001;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCreate; override;
  end;

implementation

uses
  Vcl.Dialogs, System.UITypes, UI.MainForm;

{ TChildForm }

procedure TChildForm.ConfirmTokenClose(Sender: TToken);
const
  CONFIRM_CLOSE = 'This token has an opened "%s" window for it. ' +
    'Do you want close it?';
begin
  // The main window should not close the token until there are
  // any child windows opened for it.
  if MessageDlg(Format(CONFIRM_CLOSE, [FConfirmComment]), mtConfirmation,
    mbYesNoCancel, 0) = IDYES then
    Close
  else
    Abort;
end;

procedure TChildForm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
  inherited;
  FormMain.OnMainFormClose.Unsubscribe(OnMainFormClose);
end;

procedure TChildForm.DoCreate;
begin
  inherited;
  FormMain.OnMainFormClose.Subscribe(OnMainFormClose);
end;

procedure TChildForm.OnMainFormClose(Sender: TObject);
begin
  Close;
end;

function TChildForm.ShowModal: Integer;
begin
  Result := inherited;

  if ModalResult = mrCancel then
    Abort;
end;

procedure TChildForm.SubscribeTokenCanClose(Token: TToken;
  ConfirmComment: String);
begin
  FConfirmComment := ConfirmComment;
  Token.OnCanClose.Subscribe(ConfirmTokenClose);
end;

procedure TChildForm.UnsubscribeTokenCanClose(Token: TToken);
begin
  Token.OnCanClose.Unsubscribe(ConfirmTokenClose);
end;

{ TChildTaskbarForm }

procedure TChildTaskbarForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := HWND_DESKTOP;
end;

procedure TChildTaskbarForm.DoCreate;
begin
  inherited;
  InsertMenu(GetSystemMenu(Handle, False), 0, MF_STRING, idOnTop, 'Stay On Top');
end;

procedure TChildTaskbarForm.WMSysCommand(var Message: TWMSysCommand);
var
  hSysMenu: HMENU;
begin
  if Message.CmdType = idOnTop then
  begin
    hSysMenu := GetSystemMenu(Handle, False);
    if FormStyle = fsNormal then
    begin
      FormStyle := fsStayOnTop;
      CheckMenuItem(hSysMenu, idOnTop, MF_CHECKED);
    end
    else
    begin
      FormStyle := fsNormal;
      CheckMenuItem(hSysMenu, idOnTop, MF_UNCHECKED);
    end;
  end
  else
    inherited;
end;

end.
