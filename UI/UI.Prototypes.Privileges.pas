unit UI.Prototypes.Privileges;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, VclEx.ListView, NtUtils.Security.Sid, Winapi.WinNt;

type
  TPrivilegeColorMode = (pcDefault, pcGrayChecked, pcGrayUnchecked,
    pcColorChecked);

  TPrivilegeEx = record
    Value: TLuid;
    Attributes: Cardinal;
    Name, Description: string;
  end;

  TFramePrivileges = class(TFrame)
    ListView: TListViewEx;
    procedure ListViewItemChecked(Sender: TObject; Item: TListItem);
  private
    FPrivileges: TArray<TPrivilege>;
    FColorMode: TPrivilegeColorMode;
    function GetPrivilege(Ind: Integer): TPrivilege;
    procedure SetPrivilege(Ind: Integer; const Value: TPrivilege);
    function SetItemData(Item: TListItemEx; Privilege: TPrivilegeEx):
      TListItemEx;
    procedure SetItemColor(Index: Cardinal);
    procedure SetColorMode(const Value: TPrivilegeColorMode);
    function GetAttributes(Ind: Integer): Cardinal;
    procedure SetAttributes(Ind: Integer; const Value: Cardinal);
  public
    property Privilege[Ind: Integer]: TPrivilege read GetPrivilege write SetPrivilege;
    property PrivAttributes[Ind: Integer]: Cardinal read GetAttributes write SetAttributes;
    function Privileges: TArray<TPrivilege>;
    function PrivilegeCount: Integer;

    function AddPrivilege(const NewPrivilege: TPrivilege): TListItemEx;
    procedure AddPrivileges(NewPrivileges: TArray<TPrivilege>);
    procedure AddAllPrivileges;

    procedure RemovePrivilege(Index: Integer);
    procedure Clear;

    function Find(Luid: TLuid): Integer;
    function SelectedPrivileges: TArray<TPrivilege>;
    function CheckedPrivileges: TArray<TPrivilege>;

    property ColorMode: TPrivilegeColorMode read FColorMode write SetColorMode;
  end;

implementation

uses
  NtUtils.Strings, DelphiUtils.Strings, UI.Colors, NtUtils.Lsa, Ntapi.ntseapi,
  NtUtils.Exceptions;

{$R *.dfm}

{ TFramePrivileges }

function GetPrivilegeGroupId(Value: TLuid): Integer;
const
  // Be consistent with ListView's groups
  GROUP_ID_HIGH = 0;
  GROUP_ID_MEDIUM = 1;
  GROUP_ID_LOW = 2;
begin
  if LsaxQueryIntegrityPrivilege(Value) > SECURITY_MANDATORY_MEDIUM_RID  then
    Result := GROUP_ID_HIGH
  else if LsaxQueryIntegrityPrivilege(Value) < SECURITY_MANDATORY_MEDIUM_RID
    then Result := GROUP_ID_LOW
  else
    Result := GROUP_ID_MEDIUM;
end;

function FormatHint(const Privilege: TPrivilegeEx): String;
var
  Sections: array of THintSection;
begin
  SetLength(Sections, 3);

  Sections[0].Title := 'Name';
  Sections[0].Enabled := True;
  Sections[0].Content := Privilege.Name;

  Sections[1].Title := 'Description';
  Sections[1].Enabled := True;
  Sections[1].Content := Privilege.Description;

  Sections[2].Title := 'Value';
  Sections[2].Enabled := True;
  Sections[2].Content := IntToStr(Privilege.Value);

  Result := BuildHint(Sections);
end;

procedure TFramePrivileges.AddAllPrivileges;
var
  PrivDefArray: TArray<TPrivilegeDefinition>;
  PrivArray: TArray<TPrivilege>;
  i: Integer;
begin
  if not LsaxEnumeratePrivilegesLocal(PrivDefArray).IsSuccess then
  begin
    // Privilege enumeration don't work, simply use all range
    SetLength(PrivDefArray, SE_MAX_WELL_KNOWN_PRIVILEGE -
      SE_MIN_WELL_KNOWN_PRIVILEGE + 1);

    for i := 0 to High(PrivDefArray) do
      PrivDefArray[i].LocalValue := SE_MIN_WELL_KNOWN_PRIVILEGE + i;
  end;

  SetLength(PrivArray, Length(PrivDefArray));
  for i := 0 to High(PrivDefArray) do
  begin
    PrivArray[i].Luid := PrivDefArray[i].LocalValue;

    // Enable only SeChangeNotifyPrivilege by default
    if PrivArray[i].Luid = TLuid(SE_CHANGE_NOTIFY_PRIVILEGE) then
      PrivArray[i].Attributes := SE_PRIVILEGE_ENABLED_BY_DEFAULT or
        SE_PRIVILEGE_ENABLED
    else
      PrivArray[i].Attributes := 0;
  end;

  ListView.Items.BeginUpdate;
  begin
    AddPrivileges(PrivArray);

    // Check SeChangeNotifyPrivilege
    i := Find(TLuid(SE_CHANGE_NOTIFY_PRIVILEGE));
      if i <> -1 then
        ListView.Items[i].Checked := True;
  end;
  ListView.Items.EndUpdate;
end;

function TFramePrivileges.AddPrivilege(const NewPrivilege: TPrivilege):
  TListItemEx;
var
  NewPrivileges: TArray<TPrivilege>;
begin
  SetLength(NewPrivileges, 1);
  NewPrivileges[0] := NewPrivilege;
  AddPrivileges(NewPrivileges);
  Result := ListView.Items[ListView.Items.Count - 1];
end;

procedure TFramePrivileges.AddPrivileges(NewPrivileges: TArray<TPrivilege>);
var
  i: Integer;
  NewPrivilegesEx: array of TPrivilegeEx;
  Names, Descriptions: TArray<String>;
begin
  SetLength(NewPrivilegesEx, Length(NewPrivileges));

  for i := 0 to High(NewPrivileges) do
  begin
    NewPrivilegesEx[i].Value := NewPrivileges[i].Luid;
    NewPrivilegesEx[i].Attributes := NewPrivileges[i].Attributes;
  end;

  // Lookup names
  if LsaxLookupMultiplePrivileges(PrivilegesToLuids(NewPrivileges), Names,
    Descriptions).IsSuccess then
    for i := 0 to High(NewPrivileges) do
    begin
      NewPrivilegesEx[i].Name := Names[i];
      NewPrivilegesEx[i].Description := Descriptions[i];
    end;

  FPrivileges := Concat(FPrivileges, NewPrivileges);

  ListView.Items.BeginUpdate;

  for i := 0 to High(NewPrivileges) do
    SetItemData(ListView.Items.Add, NewPrivilegesEx[i]);

  ListView.Items.EndUpdate;
end;

function TFramePrivileges.CheckedPrivileges: TArray<TPrivilege>;
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

function TFramePrivileges.GetAttributes(Ind: Integer): Cardinal;
begin
  if (0 <= Ind) and (Ind <= High(FPrivileges)) then
    Result := FPrivileges[Ind].Attributes
  else
    raise ERangeError.Create('TFramePrivileges.GetAttributes');
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

function TFramePrivileges.PrivilegeCount: Integer;
begin
  Assert(Length(FPrivileges) = ListView.Items.Count);
  Result := Length(FPrivileges);
end;

function TFramePrivileges.Privileges: TArray<TPrivilege>;
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

function TFramePrivileges.SelectedPrivileges: TArray<TPrivilege>;
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

procedure TFramePrivileges.SetAttributes(Ind: Integer; const Value: Cardinal);
begin
  if (Ind < 0) or (Ind > High(FPrivileges)) then
    raise ERangeError.Create('TFramePrivileges.SetAttributes');

  FPrivileges[Ind].Attributes := Value;

  ListView.Items.BeginUpdate;
  ListView.Items[Ind].Cell[1] := StateOfPrivilegeToString(Value);
  SetItemColor(Ind);
  ListView.Items.EndUpdate;
end;

procedure TFramePrivileges.SetColorMode(const Value: TPrivilegeColorMode);
var
  i: Integer;
begin
  FColorMode := Value;

  ListView.Items.BeginUpdate;
  for i := 0 to ListView.Items.Count - 1 do
    SetItemColor(i);
  ListView.Items.EndUpdate;
end;

procedure TFramePrivileges.SetItemColor(Index: Cardinal);
begin
  if ListView.Checkboxes then
  begin
    if ListView.Items[Index].Checked and (FColorMode = pcGrayChecked) then
    begin
      ListView.Items[Index].Color := clRemoved;
      Exit;
    end;

    if not ListView.Items[Index].Checked and (FColorMode = pcGrayUnchecked) then
    begin
      ListView.Items[Index].Color := clRemoved;
      Exit;
    end;

    if not ListView.Items[Index].Checked and (FColorMode = pcColorChecked) then
    begin
      ListView.Items[Index].ColorEnabled := False;
      Exit;
    end;
  end;

  ListView.Items[Index].Color := PrivilegeToColor(Privilege[Index]);
end;

function TFramePrivileges.SetItemData(Item: TListItemEx;
  Privilege: TPrivilegeEx): TListItemEx;
begin
  if Privilege.Name = '' then
    Privilege.Name := 'Unknown privilege ' + IntToStr(Privilege.Value);

  Item.Cell[0] := PrettifyCamelCase('Se', Privilege.Name);
  Item.Cell[1] := StateOfPrivilegeToString(Privilege.Attributes);
  Item.Cell[2] := Privilege.Description;
  Item.Cell[3] := IntToStr(Privilege.Value);
  Item.Hint := FormatHint(Privilege);
  Item.GroupID := GetPrivilegeGroupId(Privilege.Value);

  SetItemColor(Item.Index);
  Result := Item;
end;

procedure TFramePrivileges.SetPrivilege(Ind: Integer; const Value: TPrivilege);
var
  Luids: TArray<TLuid>;
  Names, Descriptions: TArray<String>;
  PrivEx: TPrivilegeEx;
begin
  if (Ind < 0) or (Ind > High(FPrivileges)) then
    raise ERangeError.Create('TFramePrivileges.SetPrivilege');

  PrivEx.Value := Value.Luid;
  PrivEx.Attributes := Value.Attributes;

  SetLength(Luids, 1);
  Luids[0] := Value.Luid;

  if LsaxLookupMultiplePrivileges(Luids, Names, Descriptions).IsSuccess then
  begin
    PrivEx.Name := Names[0];
    PrivEx.Description := Descriptions[0];
  end
  else
  begin
    PrivEx.Name := '';
    PrivEx.Description := '';
  end;

  FPrivileges[Ind] := Value;
  SetItemData(ListView.Items[Ind], PrivEx);
end;

end.
