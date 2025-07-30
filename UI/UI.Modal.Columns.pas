unit UI.Modal.Columns;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, VclEx.ListView, NtUiCommon.Forms;

type
  TDialogColumns = class(TChildForm)
    ListViewColumns: TListViewEx;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DialogColumns: TDialogColumns;

implementation

uses
  UI.Settings, TU.Tokens;

{$R *.dfm}

procedure TDialogColumns.ButtonOKClick;
var
  i: Integer;
begin
  TSettings.SelectedColumns := [];
  for i := 0 to ListViewColumns.Items.Count - 1 do
    if ListViewColumns.Items[i].Checked then
      Include(TSettings.SelectedColumns, TTokenStringClass(i));
end;

procedure TDialogColumns.FormCreate;
var
  tsc: TTokenStringClass;
begin
  for tsc := Low(TTokenStringClass) to High(TTokenStringClass) do
    with ListViewColumns.Items.Add do
    begin
      Caption := ColumnsInfo[tsc].Caption;
      GroupID := 0;
      Checked := tsc in TSettings.SelectedColumns;
    end;
end;

end.
