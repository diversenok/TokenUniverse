unit UI.Modal.ThreadList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  UI.Prototypes.Forms, VclEx.ListView, NtUtils.Processes.Snapshots, Vcl.Menus;

type
  TThreadListDialog = class(TChildForm)
    ListViewThreads: TListViewEx;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    PopupMenu: TPopupMenu;
    cmTerminate: TMenuItem;
    cmSuspend: TMenuItem;
    cmResume: TMenuItem;
    procedure ListViewThreadsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewThreadsDblClick(Sender: TObject);
    procedure cmActionClick(Sender: TObject);
  private
    FProcess: TProcessEntry;
  public
    constructor CreateFrom(AOwner: TComponent; const Process: TProcessEntry);
    class function Execute(AOwner: TComponent;
      const Process: TProcessEntry): NativeUInt;
  end;

implementation

uses
  Ntapi.ntexapi, UI.Colors, NtUtils.WinUser, Ntapi.WinNt, Ntapi.ntpsapi,
  Ntapi.ntdef, NtUtils, NtUtils.Threads, NtUiLib.Errors, Ntapi.ntstatus,
  Ntapi.ntpebteb, System.UITypes;

{$R *.dfm}

{ TThreadListDialog }

procedure TThreadListDialog.cmActionClick;
var
  Thread: TClientId;
  hxThread: IHandle;
  Verb: String;
  Access: TAccessMask;
begin
  if not Assigned(ListViewThreads.Selected) then
    Exit;

  Access := THREAD_SUSPEND_RESUME;

  if Sender = cmTerminate then
  begin
    Verb := 'terminate';
    Access := THREAD_TERMINATE;
  end
  else if Sender = cmSuspend then
    Verb := 'suspend'
  else if Sender = cmResume then
    Verb := 'resume'
  else
    Exit;

  Thread := FProcess.Threads[ListViewThreads.Selected.Index].Basic.ClientId;

  if TaskMessageDlg('Are you sure you want to ' + Verb + ' this thread?',
    'This action might interfere with the usual workflow of some programs.',
    mtWarning, mbYesNoCancel, -1, mbYes) = IDYES then
  begin
    NtxOpenThread(hxThread, Thread.UniqueThread, Access).RaiseOnError;

    if Sender = cmTerminate then
      NtxTerminateThread(hxThread, STATUS_CANCELLED).RaiseOnError
    else if Sender = cmSuspend then
      NtxSuspendThread(hxThread).RaiseOnError
    else
      NtxResumeThread(hxThread).RaiseOnError;
  end;
end;

constructor TThreadListDialog.CreateFrom;
var
  i: Integer;
begin
  inherited CreateChild(AOwner, cfmApplication);

  Caption := Format('Threads of %s [%d]', [Process.ImageName,
    Process.Basic.ProcessId]);

  ListViewThreads.Items.BeginUpdate;

  for i := 0 to Process.Basic.NumberOfThreads - 1 do
  with ListViewThreads.Items.Add do
  begin
    FProcess := Process;
    Caption := IntToStr(Process.Threads[i].Basic.ClientID.UniqueThread);
    SubItems.Add(DateTimeToStr(LargeIntegerToDateTime(
      Process.Threads[i].Basic.CreateTime)));
    if Process.Threads[i].Basic.WaitReason = TWaitReason.Suspended then
      Color := ColorSettings.clBackgroundInactive
    else
    begin
      // Check whether the thread owns any GUI objects
      if UsrxIsGuiThread(Process.Threads[i].Basic.ClientId.UniqueThread) then
        Color := ColorSettings.clBackgroundGuiThread;
    end;
  end;

  if ListViewThreads.Items.Count > 0 then
    ListViewThreads.Items[0].Selected := True;

  ListViewThreads.Items.EndUpdate;
end;

class function TThreadListDialog.Execute;
begin
  with TThreadListDialog.CreateFrom(AOwner, Process) do
  begin
    ShowModal;

    if not Assigned(ListViewThreads.Selected) then
      Abort;

    Result := Process.Threads[ListViewThreads.Selected.Index].Basic.ClientId.
      UniqueThread;

    Free;
  end;
end;

procedure TThreadListDialog.ListViewThreadsDblClick;
begin
  ModalResult := mrOk;
end;

procedure TThreadListDialog.ListViewThreadsSelectItem;
begin
  ButtonOk.Enabled := (ListViewThreads.SelCount <> 0);
end;

end.
