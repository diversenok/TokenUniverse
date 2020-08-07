unit UI.Prototypes.Groups;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, VclEx.ListView, NtUtils.Security.Sid,
  NtUtils;

type
  TFrameGroups = class(TFrame)
    ListView: TListViewEx;
    procedure ListViewDblClick(Sender: TObject);
  private
    FGroups: TArray<TGroup>;
    function SetItemData(Item: TListItemEx; Group: TGroup): TListItemEx;
    function GetGroup(Ind: Integer): TGroup;
    procedure SetGroup(Ind: Integer; const Value: TGroup);
  public
    property Group[Ind: Integer]: TGroup read GetGroup write SetGroup;
    function Groups: TArray<TGroup>;
    function Count: Integer;

    function AddGroup(const NewGroup: TGroup): TListItemEx;
    procedure AddGroups(NewGroups: TArray<TGroup>);

    procedure RemoveGroup(Index: Integer);
    procedure Clear;

    function Find(Sid: ISid): Integer;
    function SelectedGroups: TArray<TGroup>;
    function CheckedGroups: TArray<TGroup>;
    procedure UiEditSelected(AOwner: TComponent;
      DisableAttributes: Boolean = False);
  end;

implementation

uses
  UI.Colors.Old, Ntapi.ntrtl, DelphiApi.Reflection,
  UI.Modal.PickUser, UI.Sid.View, Winapi.WinNt, NtUtils.Lsa.Sid, Ntapi.ntseapi,
  DelphiUiLib.Reflection, DelphiUiLib.Reflection.Numeric;

{$R *.dfm}

{ TFrameGroups }

function TFrameGroups.AddGroup(const NewGroup: TGroup): TListItemEx;
begin
  SetLength(FGroups, Length(FGroups) + 1);
  FGroups[High(FGroups)] := NewGroup;
  Result := SetItemData(ListView.Items.Add, NewGroup);
end;

procedure TFrameGroups.AddGroups(NewGroups: TArray<TGroup>);
var
  i: Integer;
begin
  FGroups := Concat(FGroups, NewGroups);

  ListView.Items.BeginUpdate;

  for i := 0 to High(NewGroups) do
    SetItemData(ListView.Items.Add, NewGroups[i]);

  ListView.Items.EndUpdate;
end;

function TFrameGroups.CheckedGroups: TArray<TGroup>;
var
  i, Count: integer;
begin
  Assert(Length(FGroups) = ListView.Items.Count);

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
      Result[Count] := GetGroup(i);
      Inc(Count);
    end;
end;

procedure TFrameGroups.Clear;
begin
  SetLength(FGroups, 0);
  ListView.Items.Clear;
end;

function TFrameGroups.Count: Integer;
begin
  Result := Length(FGroups);
end;

function TFrameGroups.Find(Sid: ISid): Integer;
var
  i: Integer;
begin
  for i := 0 to High(FGroups) do
    if RtlEqualSid(FGroups[i].Sid.Data, Sid.Data) then
      Exit(i);

  Result := -1;
end;

function TFrameGroups.GetGroup(Ind: Integer): TGroup;
begin
  if (0 <= Ind) and (Ind <= High(FGroups)) then
    Result := FGroups[Ind]
  else
    raise ERangeError.Create('TFrameGroups.GetGroup');
end;

function TFrameGroups.Groups: TArray<TGroup>;
begin
  // Do not let anyone edit our array by reference
  Result := Copy(FGroups, 0, Length(FGroups));
end;

procedure TFrameGroups.ListViewDblClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
    TDialogSidView.CreateView(Group[ListView.Selected.Index].Sid);
end;

procedure TFrameGroups.RemoveGroup(Index: Integer);
begin
  if (Index < 0) or (Index > High(FGroups)) then
    raise ERangeError.Create('TFrameGroups.RemoveGroup');

  Delete(FGroups, Index, 1);
  ListView.Items.Delete(Index);
end;

function TFrameGroups.SelectedGroups: TArray<TGroup>;
var
  i, j: integer;
begin
  Assert(Length(FGroups) = ListView.Items.Count);

  SetLength(Result, ListView.SelCount);

  j := 0;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      Result[j] := GetGroup(i);
      Inc(j);
    end;
end;

procedure TFrameGroups.SetGroup(Ind: Integer; const Value: TGroup);
begin
  if (Ind < 0) or (Ind > High(FGroups)) then
    raise ERangeError.Create('TFrameGroups.SetGroup');

  FGroups[Ind] := Value;
  SetItemData(ListView.Items[Ind], Value);
end;

function TFrameGroups.SetItemData(Item: TListItemEx; Group: TGroup): TListItemEx;
var
  SA: TSidAndAttributes;
  Representation: TRepresentation;
  NoState: IgnoreSubEnumsAttribute;
begin
  SA.Sid := Group.Sid.Data;
  SA.Attributes := Group.Attributes;
  Representation := RepresentType(TypeInfo(TSidAndAttributes), SA);

  Item.Cell[0] := Representation.Text;
  Item.Cell[1] := TNumeric.Represent<TGroupAttributes>(Group.Attributes and
    SE_GROUP_STATE_MASK).Text;

  NoState := IgnoreSubEnumsAttribute.Create;
  try
    Item.Cell[2] := TNumeric.Represent<TGroupAttributes>(Group.Attributes and
      not SE_GROUP_STATE_MASK, [NoState]).Text;
  finally
    NoState.Free;
  end;

  Item.Hint := Representation.Hint;
  Item.Color := GroupAttributesToColor(Group.Attributes);
  Result := Item;
end;

procedure TFrameGroups.UiEditSelected(AOwner: TComponent;
  DisableAttributes: Boolean);
var
  AttributesToAdd, AttributesToDelete: TGroupAttributes;
  i: integer;
  NewGroup: TGroup;
begin
  Assert(Length(FGroups) = ListView.Items.Count);

  // Edit one group: SID and [optionaly] attributes
  if (ListView.SelCount = 1) and Assigned(ListView.Selected) then
    with ListView.Selected do
      Group[Index] := TDialogPickUser.PickEditOne(AOwner, GetGroup(Index),
        DisableAttributes);

  // Edit several groups: only attributes
  if not DisableAttributes and (ListView.SelCount > 1) then
  begin
    TDialogPickUser.PickEditMultiple(AOwner, SelectedGroups, AttributesToAdd,
      AttributesToDelete);

    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].Selected then
      begin
        NewGroup := GetGroup(i);
        NewGroup.Attributes := NewGroup.Attributes and not AttributesToDelete
          or AttributesToAdd;
        SetGroup(i, NewGroup);
      end;
  end;
end;

end.
