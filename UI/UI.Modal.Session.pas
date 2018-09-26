unit UI.Modal.Session;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes, UI.Prototypes.ChildForm;

type
  TSessionDialog = class(TChildForm)
    SessionCombo: TSessionComboBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
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
    ShowModal;
    Result := SessionCombo.SelectedSession;
  end;
end;

procedure TSessionDialog.FormCreate(Sender: TObject);
begin
  SessionCombo.RefreshSessionList(True);
end;

end.
