unit UI.ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, TU.EnumProcesses, Vcl.ExtCtrls;

type
  TProcessItemHolder = class
    Data: TProcessItem;
    Node: TTreeNode;
    ParentExists: Boolean;
    ParentIndex: Integer;
    Added: Boolean;
    constructor Create(Src: TProcessItem);
  end;

  TProcessListDialog = class(TForm)
    TreeView: TTreeView;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonRefresh: TButton;
    SearchBox: TButtonedEdit;
    procedure ButtonRefreshClick(Sender: TObject);
    class function Execute(AOwner: TComponent): Cardinal; // returns PID
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SearchBoxChange(Sender: TObject);
  private
    Holders: array of TProcessItemHolder;
  public
    { Public declarations }
  end;

var
  ProcessListDialog: TProcessListDialog;

implementation

{$R *.dfm}

function ProcessToString(const Data: TProcessItem): String;
begin
  Result := Format('%s (%d)', [Data.ImageName, Data.PID]);
end;

procedure TProcessListDialog.ButtonRefreshClick(Sender: TObject);
var
  i, j, LoopAdded: integer;
begin
  TreeView.Items.BeginUpdate;

  TreeView.Items.Clear;
  for i := 0 to High(Holders) do
    Holders[i].Free;

  // Create a list of holders
  with TProcessList.Create do
  begin
    SetLength(Holders, Count);
    for i := 0 to High(Holders) do
      Holders[i] := TProcessItemHolder.Create(Items[i]);
    Free;
  end;

  // Check if parent still exists for each holder
  for i := 0 to High(Holders) do
    for j := 0 to High(Holders) do
      if (i <> j) and (Holders[i].Data.ParentPID = Holders[j].Data.PID) then
      begin
        Holders[i].ParentExists := True;
        Holders[i].ParentIndex := j;
        Break;
      end;

  // Add all items without parents
  for i := 0 to High(Holders) do
    if not Holders[i].ParentExists then
    begin
      Holders[i].Node := TreeView.Items.Add(nil,
        ProcessToString(Holders[i].Data));
      Holders[i].Node.Data := Holders[i];
      Holders[i].Added := True;
    end;

  // Add all other items
  repeat
    LoopAdded := 0;
    for i := 0 to High(Holders) do
      if Holders[i].ParentExists and (not Holders[i].Added) and
        Holders[Holders[i].ParentIndex].Added then
      begin
        Holders[i].Node := TreeView.Items.AddChild(
          Holders[Holders[i].ParentIndex].Node,
          ProcessToString(Holders[i].Data));
        Holders[Holders[i].ParentIndex].Node.Expand(False);
        Holders[i].Node.Data := Holders[i];
        Holders[i].Added := True;
        Inc(LoopAdded);
      end;
  until LoopAdded = 0;

  TreeView.Items.EndUpdate;
end;

{ TProcessItemHolder }

constructor TProcessItemHolder.Create;
begin
  Data := Src;
end;

class function TProcessListDialog.Execute(AOwner: TComponent): Cardinal;
begin
  with TProcessListDialog.Create(AOwner) do
  begin
    ShowModal; // And wait until the dialog closes
    Result := 0;
  end;
end;

procedure TProcessListDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: integer;
begin
  for i := 0 to High(Holders) do
    Holders[i].Free;
  Action := caFree;
end;

procedure TProcessListDialog.SearchBoxChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(Holders) do
    if Holders[i].Data.ImageName.Contains(SearchBox.Text) then
      Holders[i].Node;
end;

end.
