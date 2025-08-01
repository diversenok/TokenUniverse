unit UI.Modal.ComboDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes, NtUiCommon.Forms, TU.Tokens.Old.Types;

type
  TComboDialog = class(TChildForm)
    ComboBox: TComboBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
  public
    class function PickSession(AOwner: TComponent): Cardinal;
  end;

var
  ComboDialog: TComboDialog;

implementation

{$R *.dfm}

{ TSessionDialog }

class function TComboDialog.PickSession;
var
  SessionSource: TSessionSource;
begin
  with TComboDialog.Create(AOwner, cfmApplication) do
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
