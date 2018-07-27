unit UI.SessionDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes;

type
  TSessionDialog = class(TForm)
    SessionCombo: TSessionComboBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    class function Execute(AOwner: TComponent): Cardinal;
  end;

var
  SessionDialog: TSessionDialog;

implementation

{$R *.dfm}

{ TSessionDialog }

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

procedure TSessionDialog.FormCreate(Sender: TObject);
begin
  SessionCombo.RefreshSessionList;
end;

end.
