unit UI.Prototypes.ChildForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, TU.Common, TU.Tokens;

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
  protected
    procedure CreateParams(var Params: TCreateParams); override;
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
  FormMain.OnMainFormClose.Delete(OnMainFormClose);
end;

procedure TChildForm.DoCreate;
begin
  inherited;
  FormMain.OnMainFormClose.Add(OnMainFormClose);
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
  Token.OnCanClose.Add(ConfirmTokenClose);
end;

procedure TChildForm.UnsubscribeTokenCanClose(Token: TToken);
begin
  Token.OnCanClose.Delete(ConfirmTokenClose);
end;

{ TChildTaskbarForm }

procedure TChildTaskbarForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := HWND_DESKTOP;
end;

end.
