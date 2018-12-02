unit UI.Modal.Session;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes, UI.Prototypes.ChildForm;

type
  TSessionDialog = class(TChildForm)
    SessionCombo: TComboBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
  private
    SessionSource: TSessionSource;
  public
    destructor Destroy; override;
    class function Execute(AOwner: TComponent): Cardinal;
  end;

var
  SessionDialog: TSessionDialog;

implementation

{$R *.dfm}

{ TSessionDialog }

destructor TSessionDialog.Destroy;
begin
  SessionSource.Free;
  inherited;
end;

class function TSessionDialog.Execute(AOwner: TComponent): Cardinal;
begin
  with TSessionDialog.Create(AOwner) do
  begin
    ShowModal;
    Result := SessionSource.SelectedSession;
  end;
end;

procedure TSessionDialog.FormCreate(Sender: TObject);
begin
  SessionSource := TSessionSource.Create(SessionCombo);
  SessionSource.RefreshSessionList(True);
end;

end.
