unit UI.ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ImgList, System.Generics.Collections,
  UI.ListViewEx, UI.Prototypes.ChildForm, TU.Processes;

type
  TProcessIcons = class
  strict private
    class var Images: TImageList;
    class var Mapping: TDictionary<string,Integer>;
  public
    class constructor Create;
    class destructor Destroy;
    class property ImageList: TImageList read Images;
    class function GetIcon(FileName: string): Integer; static;
  end;

  TProcessItemEx = class
    Process: TProcessItem;
    SearchKeyword: string;
    Enabled: Boolean; // by search
    Added: Boolean;
    Parent: TProcessItemEx;
    ListItemRef: TListItem;
    ImageIndex: Integer;
    constructor Create(Src: TProcessItem);
  end;
  PProcessItemEx = ^TProcessItemEx;

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
  private
    ProcessListEx: array of TProcessItemEx;
    function AddChild(ParentIndex: Integer): TListItem;
  public
    class function Execute(AOwner: TComponent): Cardinal; overload;
    class function Execute(AOwner: TComponent; out ImgName: string): Cardinal;
      overload;
  end;

var
  ProcessListDialog: TProcessListDialog;

implementation

uses
  Winapi.ShellApi;

{$R *.dfm}

{ TProcessIcons }

class constructor TProcessIcons.Create;
begin
  Mapping := TDictionary<string,Integer>.Create;

  Images := TImageList.Create(nil);
  Images.ColorDepth := cd32Bit;
  Images.AllocBy := 32;

  GetIcon(GetEnvironmentVariable('SystemRoot') + '\system32\user32.dll');
end;

class destructor TProcessIcons.Destroy;
begin
  Images.Free;
  Mapping.Free;
end;

class function TProcessIcons.GetIcon(FileName: string): Integer;
var
  ObjIcon: TIcon;
  LargeHIcon, SmallHIcon: HICON;
begin
  Result := 0; // Default icon. See the constructor.

  // Unknown filename means defalut icon
  if FileName = '' then
    Exit;

  // Check if the icon for this file is already here
  if Mapping.TryGetValue(FileName, Result) then
    Exit;

  LargeHIcon := 0;
  SmallHIcon := 0;

  // Try to query the icon. Save it to our ImageList on success.
  if (ExtractIconExW(PWideChar(FileName), 0, LargeHIcon, SmallHIcon, 1) <> 0)
    and (SmallHIcon <> 0) then
  begin
    ObjIcon := TIcon.Create;
    ObjIcon.Handle := SmallHIcon;
    Result := Images.AddIcon(ObjIcon);
    ObjIcon.Free;
  end;

  DestroyIcon(SmallHIcon);
  DestroyIcon(LargeHIcon);

  // Save the icon index for future use
  Mapping.Add(FileName, Result);
end;

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
    ShowModal;

    if not Assigned(ListView.Selected) then
      Abort;

    Result := PProcessItemEx(ListView.Selected.Data).Process.PID;
  end;
end;

class function TProcessListDialog.Execute(AOwner: TComponent;
  out ImgName: string): Cardinal;
begin
  with TProcessListDialog.Create(AOwner) do
  begin
    ShowModal;

    if not Assigned(ListView.Selected) then
      Abort;

    with PProcessItemEx(ListView.Selected.Data).Process do
    begin
      Result := PID;
      ImgName := ImageName;
    end;
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
  ButtonOk.Enabled := Selected;
end;

procedure TProcessListDialog.ReloadProcessIcons;
var
  i: integer;
begin
  // TODO: Setting for disabling process icons on slow systems
  TProcessIcons.ImageList.BeginUpdate;

  for i := 0 to High(ProcessListEx) do
    ProcessListEx[i].ImageIndex := TProcessIcons.GetIcon(
      ProcessListEx[i].Process.QueryFullName);

  TProcessIcons.ImageList.EndUpdate;
end;

procedure TProcessListDialog.ReloadProcessList(Sender: TObject);
var
  i, ChildInd, ParentInd: integer;
begin
  ListView.SmallImages := TProcessIcons.ImageList;

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

  // Check if parent still exists for each process.
  // NOTE: since PIDs can be reused we also need to
  // check that the parent was created before the child.
  for ChildInd := 0 to High(ProcessListEx) do
    for ParentInd := 0 to High(ProcessListEx) do
      if (ChildInd <> ParentInd) and
        (ProcessListEx[ChildInd].Process.ParentPID =
        ProcessListEx[ParentInd].Process.PID) and
        (ProcessListEx[ChildInd].Process.CreateTime >=
        ProcessListEx[ParentInd].Process.CreateTime) then
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

constructor TProcessItemEx.Create(Src: TProcessItem);
begin
  Process := Src;
end;

end.
