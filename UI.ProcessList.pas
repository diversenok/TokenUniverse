unit UI.ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TProcessListDialog = class(TForm)
    TreeView: TTreeView;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonRefresh: TButton;
    procedure ButtonRefreshClick(Sender: TObject);
    class function Execute(AOwner: TComponent): Cardinal;
    procedure FormClose(Sender: TObject; var Action: TCloseAction); // returns PID
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProcessListDialog: TProcessListDialog;

implementation

uses
  TU.EnumProcesses;

{$R *.dfm}

type
  TProcessItemHolder = class
    Data: TProcessItem;
    Node: TTreeNode;
    ParentExists: Boolean;
    ParentIndex: Integer;
    Added: Boolean;
    constructor Create(Src: TProcessItem);
  end;

procedure TProcessListDialog.ButtonRefreshClick(Sender: TObject);
var
  Holders: array of TProcessItemHolder;
  i, j, LoopAdded: integer;
begin
  TreeView.Items.Clear;
  TreeView.Items.BeginUpdate;

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
      end;

  // Add all items without parents
  for i := 0 to High(Holders) do
    if not Holders[i].ParentExists then
    begin
      Holders[i].Node := TreeView.Items.Add(nil, Holders[i].Data.ImageName);
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
          Holders[Holders[i].ParentIndex].Node, Holders[i].Data.ImageName);
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
  Item: TTreeNode;
begin
  for Item in TreeView.Items do
    TProcessItemHolder(Item.Data).Free;
  Action := caFree;
end;

end.
