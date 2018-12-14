unit UI.Modal.ComboDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes, UI.Prototypes.ChildForm, TU.Tokens.Types;

type
  TComboDialog = class(TChildForm)
    ComboBox: TComboBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
  public
    class function PickSession(AOwner: TComponent): Cardinal;
    class function PickIntegrity(AOwner: TComponent): TTokenIntegrityLevel;
  end;

var
  ComboDialog: TComboDialog;

implementation

{$R *.dfm}

{ TSessionDialog }

class function TComboDialog.PickIntegrity(AOwner: TComponent):
  TTokenIntegrityLevel;
var
  IntegritySource: TIntegritySource;
begin
  with TComboDialog.Create(AOwner) do
  begin
    Caption := 'Choose integrity level';

    IntegritySource := TIntegritySource.Create(ComboBox);
    ComboBox.ItemIndex := 2;
    try
      ShowModal;

      Result := IntegritySource.SelectedIntegrity;
    finally
      IntegritySource.Free;
    end;
  end;
end;

class function TComboDialog.PickSession(AOwner: TComponent): Cardinal;
var
  SessionSource: TSessionSource;
begin
  with TComboDialog.Create(AOwner) do
  begin
    Caption := 'Choose session';
    ComboBox.Text := 'Unable to query. Insert it manually.';

    SessionSource := TSessionSource.Create(ComboBox, True);
    try
      ShowModal;

      Result := SessionSource.SelectedSession;
    finally
      SessionSource.Free;
    end;
  end;
end;

end.
