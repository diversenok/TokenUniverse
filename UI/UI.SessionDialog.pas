unit UI.SessionDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.SessionComboBox;

type
  TSessionDialog = class(TForm)
    SessionCombo: TSessionComboBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    class function Execute(AOwner: TComponent): Cardinal;
  end;

var
  SessionDialog: TSessionDialog;

implementation

{$R *.dfm}

{ TSessionDialog }

constructor TSessionDialog.Create(AOwner: TComponent);
begin
  inherited;
  SessionCombo.RefreshSessionList;
end;

class function TSessionDialog.Execute(AOwner: TComponent): Cardinal;
begin
  with TSessionDialog.Create(AOwner) do
  begin
    if ShowModal <> mrOk then
      Abort;

    Result := SessionCombo.SelectedSession;
  end;
end;

procedure TSessionDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
