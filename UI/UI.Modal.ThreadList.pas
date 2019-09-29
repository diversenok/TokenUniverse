unit UI.Modal.ThreadList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  UI.Prototypes.ChildForm, VclEx.ListView, NtUtils.Processes.Snapshots;

type
  TThreadListDialog = class(TChildForm)
    ListViewThreads: TListViewEx;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    procedure ListViewThreadsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewThreadsDblClick(Sender: TObject);
  public
    constructor CreateFrom(AOwner: TComponent; const Process: TProcessEntry);
    class function Execute(AOwner: TComponent;
      const Process: TProcessEntry): NativeUInt;
  end;

var
  ThreadListDialog: TThreadListDialog;

implementation

uses
  Ntapi.ntkeapi, UI.Colors;

{$R *.dfm}

type
  TGuiThreadInfo = record
    cbSize: Cardinal;
    flags: Cardinal;
    hwndActive: HWND;
    hwndFocus: HWND;
    hwndCapture: HWND;
    hwndMenuOwner: HWND;
    hwndMoveSize: HWND;
    hwndCaret: HWND;
    rcCaret: TRect;
  end;

function GetGUIThreadInfo(idThread: Cardinal; var gui: TGuiThreadInfo):
  LongBool; stdcall; external 'user32.dll';

{ TThreadListDialog }

constructor TThreadListDialog.CreateFrom(AOwner: TComponent;
  const Process: TProcessEntry);
var
  i: Integer;
  GuiInfo: TGuiThreadInfo;
begin
  inherited Create(AOwner);

  Caption := Format('Threads of %s [%d]', [Process.Process.GetImageName,
    Process.Process.ProcessId]);

  ListViewThreads.Items.BeginUpdate;

  for i := 0 to Process.Process.NumberOfThreads - 1 do
  with ListViewThreads.Items.Add do
  begin
    Caption := IntToStr(Process.Threads[i].ClientId.UniqueThread);
    SubItems.Add(DateTimeToStr(Process.Threads[i].CreateTime.ToDateTime));
    if Process.Threads[i].WaitReason = Suspended then
      Color := clSuspended
    else
    begin
      // Check wether the thread owns any GUI objects
      FillChar(GuiInfo, SizeOf(GuiInfo), 0);
      GuiInfo.cbSize := SizeOf(GuiInfo);

      if GetGUIThreadInfo(Process.Threads[i].ClientId.UniqueThread, GuiInfo) then
        Color := clGuiThread;
    end;
  end;

  if ListViewThreads.Items.Count > 0 then
    ListViewThreads.Items[0].Selected := True;

  ListViewThreads.Items.EndUpdate;
end;

class function TThreadListDialog.Execute(AOwner: TComponent;
  const Process: TProcessEntry): NativeUInt;
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
