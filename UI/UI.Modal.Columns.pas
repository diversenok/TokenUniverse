unit UI.Modal.Columns;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, UI.ListViewEx, UI.Prototypes.ChildForm;

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
  TU.Tokens, UI.Settings;

{$R *.dfm}

type
  TUICategory = (uicGeneral, uicAdvanced, uicStatistics, uicLogon, uicSource);

const
  DataClassToCategory: array [TTokenStringClass] of TUICategory = (uicGeneral,
    uicGeneral, uicGeneral, uicGeneral, uicGeneral, uicGeneral, uicGeneral,
    uicAdvanced, uicAdvanced, uicAdvanced, uicAdvanced, uicAdvanced,
    uicAdvanced, uicAdvanced, uicAdvanced, uicAdvanced, uicStatistics,
    uicStatistics, uicStatistics, uicStatistics, uicStatistics, uicStatistics,
    uicStatistics, uicLogon, uicLogon, uicLogon, uicLogon, uicLogon, uicLogon,
    uicLogon, uicSource, uicSource, uicAdvanced);

  DataClassCaption: array [TTokenStringClass] of String = ('Token Type',
    'Granted Access', 'User Name', 'User State', 'Session', 'Elevated',
    'Integrity', 'Object Address', 'Handle', 'No-write-up', 'New-process-min',
    'UIAccess', 'Owner', 'Primary Group', 'Sandbox Inert', 'Has Restrictions',
    'Token ID', 'Exprires', 'Dynamic Charged', 'Dynamic Available',
    'Group Count', 'Privilege Count', 'Modified ID', 'Logon ID',
    'Logon Auth Package', 'Logon Server', 'Logon WTS Session', 'Logon Time',
    'Logon Type', 'Logon User Name', 'Source LUID', 'Source Name', 'Origin');

procedure TDialogColumns.ButtonOKClick(Sender: TObject);
var
  i: Integer;
begin
  TSettings.SelectedColumns := [];
  for i := 0 to ListViewColumns.Items.Count - 1 do
    if ListViewColumns.Items[i].Checked then
      Include(TSettings.SelectedColumns, TTokenStringClass(i));
end;

procedure TDialogColumns.FormCreate(Sender: TObject);
var
  tsc: TTokenStringClass;
begin
  for tsc := Low(TTokenStringClass) to High(TTokenStringClass) do
    with ListViewColumns.Items.Add do
    begin
      Caption := DataClassCaption[tsc];
      GroupID := Integer(DataClassToCategory[tsc]);
      Checked := tsc in TSettings.SelectedColumns;
    end;
end;

end.
