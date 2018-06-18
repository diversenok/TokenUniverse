unit UI.ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, TU.EnumProcesses, Vcl.ExtCtrls, System.ImageList,
  Vcl.ImgList;

type
  TProcessListDialog = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonRefresh: TButton;
    SearchBox: TButtonedEdit;
    ListView: TListView;
    ImageList: TImageList;
    procedure ButtonRefreshClick(Sender: TObject);
    class function Execute(AOwner: TComponent): Cardinal; // returns PID
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MethodNoIndent;
    procedure MethodIndent;
    procedure UpdateProcessIcons;
  private
    ProcListSnapshot: TProcessList;
    function AddChild(ParentIndex: Integer): TListItem;
    function ImageListAddIcon(SrcFile: PWideChar; DefaultIcon: integer): Integer;
  public
    { Public declarations }
  end;

var
  ProcessListDialog: TProcessListDialog;

implementation

uses
  Winapi.ShellApi;

{$R *.dfm}

type
  TProcessItemHolder = class
    Data: TProcessItem;
    Item: TListItem;
    ParentExists: Boolean;
    ParentIndex: Integer;
    Added: Boolean;
    constructor Create(Src: TProcessItem);
  end;
  PProcessItemHolder = ^TProcessItemHolder;

function TProcessListDialog.AddChild(ParentIndex: Integer): TListItem;
var
  NextSibling, ParentIndent: Integer;
begin
  ParentIndent := ListView.Items[ParentIndex].Indent;
  for NextSibling := ParentIndex + 1 to ListView.Items.Count - 1 do
    if ListView.Items[NextSibling].Indent <= ParentIndent then
      Break;

  if NextSibling = ListView.Items.Count then
    Result := ListView.Items.Add
  else
    Result := ListView.Items.Insert(NextSibling);

  Result.Indent := ParentIndent + 1;
end;

procedure TProcessListDialog.ButtonRefreshClick(Sender: TObject);
begin
  //MethodNoIndent;
  MethodIndent;
  UpdateProcessIcons;
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
  ProcListSnapshot.Free;
  Action := caFree;
end;

procedure TProcessListDialog.UpdateProcessIcons;
var
  i: integer;
  hProcess: THandle;
  Buffer: PWideChar;
  BufferSize: Cardinal;
  DefaultIcon: integer;
begin
  ImageList.Clear;
  DefaultIcon := ImageListAddIcon(PWideChar(GetEnvironmentVariable('SystemRoot')
    + '\system32\user32.dll'), -1);

  Buffer := AllocMem(NT_FILENAME_MAX);
  try
    for i := 0 to ProcListSnapshot.Count - 1 do
    begin
      ListView.Items[i].ImageIndex := DefaultIcon;

      BufferSize := NT_FILENAME_MAX;
      //hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, ProcListSnapshot[i].PID);
      hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, PProcessItemHolder(ListView.Items[i].Data).Data.PID);

      if hProcess = 0 then
        Continue;

      if QueryFullProcessImageNameW(hProcess, 0, Buffer, BufferSize) then
      begin
        ListView.Items[i].ImageIndex := ImageListAddIcon(Buffer, DefaultIcon);
        CloseHandle(hProcess);
      end;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure TProcessListDialog.MethodIndent;
var
  i, j, LoopAdded: integer;
  Holders: array of TProcessItemHolder;
begin
  ProcListSnapshot.Free;
  ProcListSnapshot := TProcessList.Create;

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

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
      Holders[i].Item := ListView.Items.Add;
      Holders[i].Item.Caption := ' ' + Holders[i].Data.ImageName;
      Holders[i].Item.SubItems.Add(IntToStr(Holders[i].Data.PID));
      Holders[i].Item.Data := @Holders[i];
      Holders[i].Added := True;
    end;

  // Add all other items
  repeat
    LoopAdded := 0;
    for i := 0 to High(Holders) do
      if Holders[i].ParentExists and (not Holders[i].Added) and
        Holders[Holders[i].ParentIndex].Added then
      begin
        Holders[i].Item := AddChild(Holders[Holders[i].ParentIndex].Item.Index);
        Holders[i].Item.Caption := ' ' + Holders[i].Data.ImageName;
        Holders[i].Item.SubItems.Add(IntToStr(Holders[i].Data.PID));
        Holders[i].Item.Data := @Holders[i];
        Holders[i].Added := True;
        Inc(LoopAdded);
      end;
  until LoopAdded = 0;

  ListView.Items.EndUpdate;

  {for i := 0 to 4 do
    ShowMessage(PProcessItemHolder(ListView.Items[i].Data).Data.ImageName);}

end;

function TProcessListDialog.ImageListAddIcon(SrcFile: PWideChar;
  DefaultIcon: integer): Integer;
var
  ObjIcon: TIcon;
  LargeHIcon, SmallHIcon: HICON;
begin
  if ExtractIconExW(SrcFile, 0, LargeHIcon, SmallHIcon, 1) <> 0  then
  begin
    ObjIcon := TIcon.Create;
    ObjIcon.Handle := SmallHIcon;
    Result := ImageList.AddIcon(ObjIcon);
    ObjIcon.Free;
    DestroyIcon(SmallHIcon);
    DestroyIcon(LargeHIcon);
  end
  else
    Result := DefaultIcon;
end;

procedure TProcessListDialog.MethodNoIndent;
var
  i: integer;
begin
  ProcListSnapshot.Free;
  ProcListSnapshot := TProcessList.Create;

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  for i := 0 to ProcListSnapshot.Count - 1 do
    with ListView.Items.Add do
    begin
      Caption := ' ' + ProcListSnapshot[i].ImageName;
      SubItems.Add(IntToStr(ProcListSnapshot[i].PID));
    end;

  ListView.Items.EndUpdate;
//  UpdateProcessList;
end;

{ TProcessItemHolder }

constructor TProcessItemHolder.Create(Src: TProcessItem);
begin
  Data := Src;
end;

end.
