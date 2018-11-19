unit UI.ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, TU.Processes, Vcl.ExtCtrls, System.ImageList,
  Vcl.ImgList;

type
  TProcessItemEx = class
    Process: TProcessInformation;
    SearchKeyword: string;
    Enabled: Boolean; // by search
    Added: Boolean;
    Parent: TProcessItemEx;
    ListItemRef: TListItem;
    ImageIndex: Integer;
    constructor Create(Src: TProcessInformation);
  end;
  PProcessItemEx = ^TProcessItemEx;

  TProcessListDialog = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonRefresh: TButton;
    SearchBox: TButtonedEdit;
    ListView: TListView;
    ImageList: TImageList;
    SearchButtons: TImageList;
    procedure ReloadProcessList(Sender: TObject);
    procedure ReloadProcessIcons;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SearchBoxChange(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    /// <summary>
    ///  Displays the dialog and returns PID of the selected process.
    /// </summary>
    class function Execute(AOwner: TComponent): Cardinal;
    destructor Destroy; override;
    procedure SearchBoxRightButtonClick(Sender: TObject);
  private
    ProcessListEx: array of TProcessItemEx;
    function AddChild(ParentIndex: Integer): TListItem;
    function AddIcon(SrcFile: PWideChar; DefaultIcon: integer): Integer;
  public
    { Public declarations }
  end;

var
  ProcessListDialog: TProcessListDialog;

implementation

uses
  Winapi.ShellApi;

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

function TProcessListDialog.AddIcon(SrcFile: PWideChar;
  DefaultIcon: integer): Integer;
var
  ObjIcon: TIcon;
  LargeHIcon, SmallHIcon: HICON;
begin
  if (ExtractIconExW(SrcFile, 0, LargeHIcon, SmallHIcon, 1) <> 0) and
    (SmallHIcon <> 0)  then
  begin
    ObjIcon := TIcon.Create;
    ObjIcon.Handle := SmallHIcon;
    Result := ImageList.AddIcon(ObjIcon);
    ObjIcon.Free;
  end
  else
    Result := DefaultIcon;

  DestroyIcon(SmallHIcon);
  DestroyIcon(LargeHIcon);
end;

destructor TProcessListDialog.Destroy;
var
  i: integer;
begin
  for i := 0 to High(ProcessListEx) do
    ProcessListEx[i].Free;
  inherited;
end;

class function TProcessListDialog.Execute(AOwner: TComponent): Cardinal;
begin
  with TProcessListDialog.Create(AOwner) do
  begin
    ShowModal; // And wait until the dialog closes

    // The form wouldn't be actually destroyed until Application.ProcessMessages

    if (ModalResult <> mrOk) or (ListView.Selected = nil) then
      Abort;

    Result := PProcessItemEx(ListView.Selected.Data).Process.PID;
  end;
end;

procedure TProcessListDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
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
  ButtonOk.Enabled := Selected;
end;

procedure TProcessListDialog.ReloadProcessIcons;
var
  i: integer;
  hProcess: THandle;
  Buffer: PWideChar;
  BufferSize: Cardinal;
  DefaultIcon: integer;
begin
  // TODO: Setting for disabling process icons on slow systems

  ImageList.BeginUpdate;
  ImageList.Clear;

  DefaultIcon := AddIcon(PWideChar(GetEnvironmentVariable('SystemRoot')
    + '\system32\user32.dll'), -1);

  Buffer := AllocMem(NT_FILENAME_MAX);
  try
    for i := 0 to High(ProcessListEx) do
    with ProcessListEx[i] do
    begin
      ImageIndex := DefaultIcon;

      BufferSize := NT_FILENAME_MAX;
      hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False,
        Process.PID);

      if hProcess = 0 then
        Continue;

      if QueryFullProcessImageNameW(hProcess, 0, Buffer, BufferSize) then
      begin
        ImageIndex := AddIcon(Buffer, DefaultIcon);
        CloseHandle(hProcess);
      end;
    end;
  finally
    FreeMem(Buffer);
    ImageList.EndUpdate;
  end;
end;

procedure TProcessListDialog.ReloadProcessList(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to High(ProcessListEx) do
    ProcessListEx[i].Free;

  // Create a list of processes
  with TProcessList.Create do
  begin
    SetLength(ProcessListEx, Count);
    for i := 0 to High(ProcessListEx) do
    begin
      ProcessListEx[i] := TProcessItemEx.Create(Items[i]);
      ProcessListEx[i].SearchKeyword :=
        LowerCase(ProcessListEx[i].Process.ImageName) + ' ' +
        IntToStr(ProcessListEx[i].Process.PID);
    end;
    Free;
  end;

  // Check if parent still exists for each process
  for i := 0 to High(ProcessListEx) do
    for j := 0 to High(ProcessListEx) do
      if (i <> j) and (ProcessListEx[i].Process.ParentPID = ProcessListEx[j].Process.PID) then
      begin
        ProcessListEx[i].Parent := ProcessListEx[j];
        Break;
      end;

  try
    ReloadProcessIcons;
  finally
    SearchBoxChange(Sender);
  end;
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
        ListItemRef.SubItems.Add(IntToStr(Process.PID));
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
        ListItemRef.SubItems.Add(IntToStr(Process.PID));
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

constructor TProcessItemEx.Create(Src: TProcessInformation);
begin
  Process := Src;
end;

end.
