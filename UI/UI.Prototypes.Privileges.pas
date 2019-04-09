unit UI.Prototypes.Privileges;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, UI.ListViewEx, NtUtils.Types, Winapi.WinNt;

type
  TPrivilegeColorMode = (pmDefault, pmGrayChecked, pmGrayUnchecked);

  TFramePrivileges = class(TFrame)
    ListView: TListViewEx;
    procedure ListViewItemChecked(Sender: TObject; Item: TListItem);
  private
    FPrivileges: TPrivilegeArray;
    FMode: TPrivilegeColorMode;
    function GetPrivilege(Ind: Integer): TPrivilege;
    procedure SetPrivilege(Ind: Integer; const Value: TPrivilege);
    function SetItemData(Item: TListItemEx; Privilege: TPrivilege): TListItemEx;
    procedure SetItemColor(Index: Cardinal);
    procedure SetColorMode(const Value: TPrivilegeColorMode);
  public
    property Privilege[Ind: Integer]: TPrivilege read GetPrivilege write SetPrivilege;
    function Privileges: TPrivilegeArray;

    function AddPrivilege(const NewPrivilege: TPrivilege): TListItemEx;
    procedure AddPrivileges(NewPrivileges: TPrivilegeArray);
    procedure AddAllPrivileges;

    procedure RemovePrivilege(Index: Integer);
    procedure Clear;

    function Find(Luid: TLuid): Integer;
    function SelectedPrivileges: TPrivilegeArray;
    function CheckedPrivileges: TPrivilegeArray;

    property ColorMode: TPrivilegeColorMode read FMode write SetColorMode;
  end;

implementation

uses
  NtUtils.Strings, TU.LsaApi, UI.Colors;

{$R *.dfm}

{ TFramePrivileges }

procedure TFramePrivileges.AddAllPrivileges;
var
  LuidArray: TLuidDynArray;
  Priv: TPrivilege;
  i: Integer;
begin
  LuidArray := TPrivilegeCache.AllPrivileges;

  ListView.Items.BeginUpdate;
  for i := 0 to High(LuidArray) do
  begin
    Priv.Luid := LuidArray[i];

    // Check and enable only SeChangeNotify by default
    if TPrivilegeCache.QueryName(LuidArray[i]) = 'SeChangeNotifyPrivilege' then
    begin
      Priv.Attributes := SE_PRIVILEGE_ENABLED_BY_DEFAULT or
        SE_PRIVILEGE_ENABLED;
      AddPrivilege(Priv).Checked := True;
    end
    else
    begin
      Priv.Attributes := 0;
      AddPrivilege(Priv);
    end;
  end;
  ListView.Items.EndUpdate;
end;

function TFramePrivileges.AddPrivilege(const NewPrivilege: TPrivilege):
  TListItemEx;
begin
  SetLength(FPrivileges, Length(FPrivileges) + 1);
  FPrivileges[High(FPrivileges)] := NewPrivilege;
  Result := SetItemData(ListView.Items.Add, NewPrivilege);
end;

procedure TFramePrivileges.AddPrivileges(NewPrivileges: TPrivilegeArray);
var
  i: Integer;
begin
  FPrivileges := Concat(FPrivileges, NewPrivileges);

  ListView.Items.BeginUpdate;

  for i := 0 to High(NewPrivileges) do
    SetItemData(ListView.Items.Add, NewPrivileges[i]);

  ListView.Items.EndUpdate;
end;

function TFramePrivileges.CheckedPrivileges: TPrivilegeArray;
var
  i, Count: integer;
begin
  Assert(Length(FPrivileges) = ListView.Items.Count);

  // Count all the checked items
  Count := 0;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Checked then
      Inc(Count);

  SetLength(Result, Count);

  // Collect them
  Count := 0;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Checked then
    begin
      Result[Count] := GetPrivilege(i);
      Inc(Count);
    end;
end;

procedure TFramePrivileges.Clear;
begin
  SetLength(FPrivileges, 0);
  ListView.Items.Clear;
end;

function TFramePrivileges.Find(Luid: TLuid): Integer;
var
  i: Integer;
begin
  for i := 0 to High(FPrivileges) do
    if FPrivileges[i].Luid = Luid then
      Exit(i);

  Result := -1;
end;

function TFramePrivileges.GetPrivilege(Ind: Integer): TPrivilege;
begin
  if (0 <= Ind) and (Ind <= High(FPrivileges)) then
    Result := FPrivileges[Ind]
  else
    raise ERangeError.Create('TFramePrivileges.GetPrivilege');
end;

procedure TFramePrivileges.ListViewItemChecked(Sender: TObject;
  Item: TListItem);
begin
  if Assigned(Item) then
    SetItemColor(Item.Index);
end;

function TFramePrivileges.Privileges: TPrivilegeArray;
begin
  // Do not let anyone edit our array by reference
  Result := Copy(FPrivileges, 0, Length(FPrivileges));
end;

procedure TFramePrivileges.RemovePrivilege(Index: Integer);
begin
  if (Index < 0) or (Index > High(FPrivileges)) then
    raise ERangeError.Create('TFramePrivileges.RemovePrivilege');

  Delete(FPrivileges, Index, 1);
  ListView.Items.Delete(Index);
end;

function TFramePrivileges.SelectedPrivileges: TPrivilegeArray;
var
  i, j: integer;
begin
  Assert(Length(FPrivileges) = ListView.Items.Count);

  SetLength(Result, ListView.SelCount);

  j := 0;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      Result[j] := GetPrivilege(i);
      Inc(j);
    end;
end;

procedure TFramePrivileges.SetColorMode(const Value: TPrivilegeColorMode);
var
  i: Integer;
begin
  FMode := Value;

  ListView.Items.BeginUpdate;
  for i := 0 to ListView.Items.Count - 1 do
    SetItemColor(i);
  ListView.Items.EndUpdate;
end;

procedure TFramePrivileges.SetItemColor(Index: Cardinal);
begin
  if ListView.Checkboxes then
  begin
    if ListView.Items[Index].Checked and (FMode = pmGrayChecked) then
    begin
      ListView.Items[Index].Color := clRemoved;
      Exit;
    end;

    if not ListView.Items[Index].Checked and (FMode = pmGrayUnchecked) then
    begin
      ListView.Items[Index].Color := clRemoved;
      Exit;
    end;
  end;

  ListView.Items[Index].Color := PrivilegeToColor(Privilege[Index]);
end;

function TFramePrivileges.SetItemData(Item: TListItemEx;
  Privilege: TPrivilege): TListItemEx;
begin
  Item.Cell[0] := TPrivilegeCache.QueryName(Privilege.Luid);
  Item.Cell[1] := PrivilegeStateToString(Privilege.Attributes);
  Item.Cell[2] := TPrivilegeCache.QueryDisplayName(Privilege.Luid);
  Item.Cell[3] := IntToStr(Privilege.Luid);
  SetItemColor(Item.Index);
  Result := Item;
end;

procedure TFramePrivileges.SetPrivilege(Ind: Integer; const Value: TPrivilege);
begin
  if (Ind < 0) or (Ind > High(FPrivileges)) then
    raise ERangeError.Create('TFramePrivileges.SetPrivilege');

  FPrivileges[Ind] := Value;
  SetItemData(ListView.Items[Ind], Value);
end;

end.
