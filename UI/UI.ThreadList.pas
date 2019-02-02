unit UI.ThreadList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  UI.Prototypes.ChildForm, UI.ListViewEx, TU.Processes;

type
  TThreadListDialog = class(TChildForm)
    ListViewThreads: TListViewEx;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    procedure ListViewThreadsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewThreadsDblClick(Sender: TObject);
  public
    constructor CreateFrom(AOwner: TComponent; Process: PProcessInfo);
    class function Execute(AOwner: TComponent;
      Process: PProcessInfo): NativeUInt;
  end;

var
  ThreadListDialog: TThreadListDialog;

implementation

uses
  TU.Common;

{$R *.dfm}

{ TThreadListDialog }

constructor TThreadListDialog.CreateFrom(AOwner: TComponent;
  Process: PProcessInfo);
var
  i: Integer;
begin
  inherited Create(AOwner);

  Caption := Format('Threads of %s [%d]', [Process.GetImageName,
    Process.ProcessId]);

  ListViewThreads.Items.BeginUpdate;

  for i := 0 to Process.NumberOfThreads - 1 do
  with ListViewThreads.Items.Add do
  begin
    Caption := IntToStr(Process.Threads[i].ClientId.UniqueThread);
    SubItems.Add(DateTimeToStr(NativeTimeToLocalDateTime(
      Process.Threads[i].CreateTime)));
  end;

  ListViewThreads.Items.EndUpdate;
end;

class function TThreadListDialog.Execute(AOwner: TComponent;
  Process: PProcessInfo): NativeUInt;
begin
  with TThreadListDialog.CreateFrom(AOwner, Process) do
  begin
    ShowModal;

    if not Assigned(ListViewThreads.Selected) then
      Abort;

    Result := Process.Threads[ListViewThreads.Selected.Index].ClientId.UniqueThread;

    Free;
  end;
end;

procedure TThreadListDialog.ListViewThreadsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TThreadListDialog.ListViewThreadsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonOk.Enabled := (ListViewThreads.SelCount <> 0);
end;

end.
