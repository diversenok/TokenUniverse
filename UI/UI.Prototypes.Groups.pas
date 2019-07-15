unit UI.Prototypes.Groups;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, UI.ListViewEx, NtUtils.Security.Sid;

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

function BuildSidHint(Sid: ISid; Attributes: Cardinal;
  AttributesPresent: Boolean = True): String;

implementation

uses
  UI.Colors, NtUtils.Strings, DelphiUtils.Strings,
  UI.Modal.PickUser, UI.Sid.View, Winapi.WinNt;

{$R *.dfm}

function BuildSidHint(Sid: ISid; Attributes: Cardinal;
  AttributesPresent: Boolean): String;
const
  SE_GROUP_ALL_FLAGS = SE_GROUP_MANDATORY or SE_GROUP_OWNER or
                       SE_GROUP_USE_FOR_DENY_ONLY or SE_GROUP_INTEGRITY or
                       SE_GROUP_RESOURCE or SE_GROUP_LOGON_ID;
var
  Sections: array of THintSection;
begin
  SetLength(Sections, 5);

  Sections[0].Title := 'Friendly name';
  Sections[0].Enabled := Sid.AsString <> Sid.SDDL;
  Sections[0].Content := Sid.AsString;

  Sections[1].Title := 'SID';
  Sections[1].Enabled := True;
  Sections[1].Content := Sid.SDDL;

  Sections[2].Title := 'Type';
  Sections[2].Enabled := Sid.SidType <> SidTypeUndefined;
  Sections[2].Content := PrettifyCamelCaseEnum('SidType', TypeInfo(TSidNameUse),
    Integer(Sid.SidType));

  Sections[3].Title := 'State';
  Sections[3].Enabled := AttributesPresent;
  Sections[3].Content := StateOfGroupToString(Attributes);

  if AttributesPresent and ContainsAny(Attributes, SE_GROUP_ALL_FLAGS) then
  begin
    Sections[4].Title := 'Flags';
    Sections[4].Enabled := True;
    Sections[4].Content := MapFlags(Attributes, GroupAttributeFlags);
  end
  else
    Sections[4].Enabled := False;

  Result := BuildHint(Sections);
end;

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
    if FGroups[i].SecurityIdentifier.EqualsTo(Sid.Sid) then
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
    TDialogSidView.CreateView(Group[ListView.Selected.Index].SecurityIdentifier);
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
begin
  Group.SecurityIdentifier.RefreshLookup;
  Item.Cell[0] := Group.SecurityIdentifier.AsString;
  Item.Cell[1] := StateOfGroupToString(Group.Attributes);
  Item.Cell[2] := MapFlags(Group.Attributes, GroupAttributeFlags);
  Item.Hint := BuildSidHint(Group.SecurityIdentifier, Group.Attributes);
  Item.Color := GroupAttributesToColor(Group.Attributes);
  Result := Item;
end;

procedure TFrameGroups.UiEditSelected(AOwner: TComponent;
  DisableAttributes: Boolean);
var
  AttributesToAdd, AttributesToDelete: Cardinal;
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
