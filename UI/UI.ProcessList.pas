unit UI.ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ImgList, System.Generics.Collections,
  VclEx.ListView, UI.Prototypes.ChildForm, NtUtils.Processes.Snapshots;

type
  TProcessItemEx = class
    Process: TProcessEntry;
    SearchKeyword: string;
    Enabled: Boolean; // by search
    Added: Boolean;
    Parent: TProcessItemEx;
    ListItemRef: TListItem;
    ImageIndex: Integer;
    constructor Create(const Src: TProcessEntry);
  end;
  PProcessItemEx = ^TProcessItemEx;

  TClientIdEx = record
    ProcessID, ThreadID: NativeUInt;
    ImageName: String;
  end;

  TProcessListDialog = class(TChildForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonRefresh: TButton;
    SearchBox: TButtonedEdit;
    ListView: TListViewEx;
    procedure ReloadProcessList(Sender: TObject);
    procedure ReloadProcessIcons;
    procedure SearchBoxChange(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    destructor Destroy; override;
    procedure SearchBoxRightButtonClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
  private
    ProcessListEx: array of TProcessItemEx;
    PickThread: Boolean;
    ThreadID: NativeUInt;
    function AddChild(ParentIndex: Integer): TListItem;
  public
    class function Execute(AOwner: TComponent; AllowSelectThread: Boolean):
      TClientIdEx;
  end;

implementation

uses
  Winapi.WinUser, Winapi.Shell, UI.Modal.ThreadList, NtUtils.Processes,
  NtUiLib.Icons, NtUtils;

{$R *.dfm}

{ TProcessListDialog }

function TProcessListDialog.AddChild(ParentIndex: Integer): TListItem;
var
  NextSibling, ParentIndent: Integer;
begin
  ParentIndent := ListView.Items[ParentIndex].Indent;

  NextSibling := ListView.Items.Count;
  for NextSibling := ParentIndex + 1 to ListView.Items.Count - 1 do
    if ListView.Items[NextSibling].Indent <= ParentIndent then
      Break;

  if NextSibling = ListView.Items.Count then
    Result := ListView.Items.Add
  else
    Result := ListView.Items.Insert(NextSibling);

  Result.Indent := ParentIndent + 1;
end;

procedure TProcessListDialog.ButtonOkClick(Sender: TObject);
begin
  if PickThread and Assigned(ListView.Selected) then
    ThreadID := TThreadListDialog.Execute(Self,
      PProcessItemEx(ListView.Selected.Data).Process);

  ModalResult := mrOk;
end;

destructor TProcessListDialog.Destroy;
var
  i: integer;
begin
  for i := 0 to High(ProcessListEx) do
    ProcessListEx[i].Free;

  inherited;
end;

class function TProcessListDialog.Execute(AOwner: TComponent;
  AllowSelectThread: Boolean): TClientIdEx;
var
  Process: PProcessEntry;
begin
  with TProcessListDialog.Create(AOwner) do
  begin
    PickThread := AllowSelectThread;

    ShowModal;

    if not Assigned(ListView.Selected) then
      Abort;

    if AllowSelectThread and (ThreadID = 0) then
      Abort;

    Process := @PProcessItemEx(ListView.Selected.Data).Process;
    Result.ProcessID := Process.Basic.ProcessId;
    Result.ThreadID := ThreadID;
    Result.ImageName := Process.ImageName;
  end;
end;

procedure TProcessListDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    ReloadProcessList(Sender);
  if (Key = Ord('F')) and (Shift = [ssCtrl]) then
    SearchBox.SetFocus;
  if Key = VK_ESCAPE then
  begin
    if SearchBox.Focused and (SearchBox.Text <> '') then
      SearchBox.Text := ''
    else
      Close;
  end;
end;

procedure TProcessListDialog.ListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonOk.Enabled := (ListView.SelCount <> 0);
end;

procedure TProcessListDialog.ReloadProcessIcons;
var
  i: integer;
begin
  // TODO: Setting for disabling process icons on slow systems
  TProcessIcons.ImageList.BeginUpdate;

  for i := 0 to High(ProcessListEx) do
    ProcessListEx[i].ImageIndex := TProcessIcons.GetIconByPid(
      ProcessListEx[i].Process.Basic.ProcessId);

  TProcessIcons.ImageList.EndUpdate;
end;

procedure TProcessListDialog.ReloadProcessList(Sender: TObject);
var
  i, ChildInd, ParentInd: integer;
  Processes: TArray<TProcessEntry>;
begin
  ListView.SmallImages := TProcessIcons.ImageList;

  for i := 0 to High(ProcessListEx) do
    ProcessListEx[i].Free;

  if not NtxEnumerateProcesses(Processes).IsSuccess then
    Exit;

  SetLength(ProcessListEx, Length(Processes));
  for i := 0 to High(ProcessListEx) do
  begin
    ProcessListEx[i] := TProcessItemEx.Create(Processes[i]);
    ProcessListEx[i].SearchKeyword :=
      LowerCase(ProcessListEx[i].Process.ImageName) + ' ' +
      IntToStr(ProcessListEx[i].Process.Basic.ProcessId);
  end;

  // Check if parent still exists for each process.
  // NOTE: since PIDs can be reused we also need to
  // check that the parent was created before the child.
  for ChildInd := 0 to High(ProcessListEx) do
    for ParentInd := 0 to High(ProcessListEx) do
      if (ChildInd <> ParentInd) and ParentProcessChecker(
        ProcessListEx[ParentInd].Process, ProcessListEx[ChildInd].Process) then
      begin
        ProcessListEx[ChildInd].Parent := ProcessListEx[ParentInd];
        Break;
      end;

  ReloadProcessIcons;
  SearchBoxChange(Sender);
end;

procedure TProcessListDialog.SearchBoxChange(Sender: TObject);
var
  i, LoopAdded: integer;
  SearchQuery: String;
begin
  SearchBox.RightButton.Visible := SearchBox.Text <> '';

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  // Enable items that match search and disable that not
  SearchQuery := LowerCase(SearchBox.Text);
  for i := 0 to High(ProcessListEx) do
    with ProcessListEx[i] do
    begin
      Enabled := (SearchBox.Text = '') or SearchKeyword.Contains(SearchQuery);
      Added := False;
    end;

  // Add all items without parents or with disabled parents
  for i := 0 to High(ProcessListEx) do
    with ProcessListEx[i] do
      if Enabled and ((Parent = nil) or (not Parent.Enabled))  then
      begin
        ListItemRef := ListView.Items.Add;
        ListItemRef.Caption := ' ' + Process.ImageName;
        ListItemRef.SubItems.Add(IntToStr(Process.Basic.ProcessId));
        ListItemRef.ImageIndex := ImageIndex;
        ListItemRef.Data := @ProcessListEx[i];
        Added := True;
      end;

  // Add all other items
  repeat
    LoopAdded := 0;
    for i := 0 to High(ProcessListEx) do
      with ProcessListEx[i] do
        if Enabled and (not Added) and (Parent <> nil) and Parent.Added then
      begin
        ListItemRef := AddChild(Parent.ListItemRef.Index);
        ListItemRef.Caption := ' ' + Process.ImageName;
        ListItemRef.SubItems.Add(IntToStr(Process.Basic.ProcessId));
        ListItemRef.ImageIndex := ImageIndex;
        ListItemRef.Data := @ProcessListEx[i];
        Added := True;
        Inc(LoopAdded);
      end;
  until LoopAdded = 0;

  if ListView.Items.Count > 0 then
    ListView.Items[0].Selected := True;

  ListView.Items.EndUpdate;
end;

procedure TProcessListDialog.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

{ TProcessItemEx }

constructor TProcessItemEx.Create(const Src: TProcessEntry);
begin
  Process := Src;
end;

end.
