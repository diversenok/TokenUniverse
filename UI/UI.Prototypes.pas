unit UI.Prototypes;

interface

uses
  System.SysUtils, System.Classes, Vcl.ComCtrls, Vcl.StdCtrls, Winapi.Windows,
  UI.ListViewEx, TU.Tokens, TU.WtsApi, TU.LsaApi, TU.Tokens.Types;

type
  TPrivilegesSource = class
  private
    ListView: TListViewEx;
    Token: TToken;
    FTokenPrivileges, FAdditionalPrivileges: TPrivilegeArray;
    function GetPrivilege(Ind: Integer): TPrivilege;
    procedure OnPrivilegeChange(NewPrivileges: TPrivilegeArray);
    function SetItem(Privilege: TPrivilege): TListItemEx;
  public
    constructor Create(OwnedListView: TListViewEx);
    destructor Destroy; override;
    procedure SubscribeToken(Token: TToken);
    procedure UnsubscribeToken(Dummy: TToken = nil);
    function SelectedPrivileges: TPrivilegeArray;
    function CheckedPrivileges: TPrivilegeArray;
    property Privilege[Ind: Integer]: TPrivilege read GetPrivilege;
    function Privileges: TPrivilegeArray;
    function AddPrivilege(Privilege: TPrivilege): TListItemEx;
    function RemovePrivilege(Index: Integer): Boolean;
  end;

  TGroupsSourceMode = (gsGroups, gsRestrictedSIDs);

  TGroupsSource = class
  private
    ListView: TListViewEx;
    Token: TToken;
    FTokenGroups, FAdditionalGroups: TGroupArray;
    Mode: TGroupsSourceMode;
    procedure OnGroupsChange(NewGroups: TGroupArray);
    function SetItem(Item: TListItemEx; Group: TGroup): TListItemEx;
    function GetGroup(Ind: Integer): TGroup;
    procedure SetGroup(Ind: Integer; const Value: TGroup);
  public
    constructor Create(OwnedListView: TListViewEx);
    destructor Destroy; override;
    procedure SubscribeToken(Token: TToken; Mode: TGroupsSourceMode);
    procedure UnsubscribeToken(Dummy: TToken = nil);
    property Group[Ind: Integer]: TGroup read GetGroup write SetGroup;
    function Groups: TGroupArray;
    function SelectedGroups: TGroupArray;
    function CheckedGroups: TGroupArray;
    function AddGroup(Group: TGroup): TListItemEx;
    function IsAdditional(Index: Integer): Boolean;
    procedure RemoveGroup(Index: Integer);
    procedure UiEditSelected(AOwner: TComponent; DisableAttributes: Boolean =
      False);
    class function BuildHint(SID: TSecurityIdentifier;
      Attributes: TGroupAttributes; AttributesPresent: Boolean = True): String;
      static;
  end;

  TSessionSource = class
  private
    SessionList: TSessionList;
    ComboBox: TComboBox;
    function GetSession: Cardinal;
    procedure SetSession(const Value: Cardinal);
  public
    destructor Destroy; override;
    constructor Create(OwnedComboBox: TComboBox; SelectCurrent: Boolean);
    procedure RefreshSessionList(SelectCurrent: Boolean);
    property SelectedSession: Cardinal read GetSession write SetSession;
  end;

  TIntegritySource = class
  private
    IsIntermediate: Boolean;
    IntermediateValue: TTokenIntegrityLevel;
    IntermediateIndex: Integer;
    ComboBox: TComboBox;
    function GetIntegrityLevel: TTokenIntegrityLevel;
    procedure RefreshList;
  public
    constructor Create(OwnedComboBox: TComboBox);
    procedure SetIntegrity(Value: TTokenIntegrity);
    property SelectedIntegrity: TTokenIntegrityLevel read GetIntegrityLevel;
  end;

  TAccessMaskSource = class
    class procedure InitAccessEntries(ListView: TListView; Access: ACCESS_MASK);
    class function GetAccessMask(ListView: TListView): ACCESS_MASK;
  end;

  TLogonSessionSource = class
  private
    FLogonSessions: TLuidDynArray;
    ComboBox: TComboBox;
    function GetSelected: LUID;
    procedure SetSelected(const Value: LUID);
  public
    constructor Create(OwnedComboBox: TComboBox);
    procedure UpdateLogonSessions;
    property SelectedLogonSession: LUID read GetSelected write SetSelected;
  end;

  TTokenViewSource = class;
  TRowSource = class;

  TCellSource = class
  private
    Row: TRowSource;
    ColumnIndex: Integer;
    procedure SetTextCallback(Value: String);
  public
    constructor Create(Row: TRowSource; ColumnIndex: Integer);
    destructor Destroy; override;
  end;

  TRowSource = class
  private
    Cells: array of TCellSource;
    procedure TokenCaptionCallback(Value: String);
  public
    Token: TToken;
    Item: TListItemEx;
    Owner: TTokenViewSource;
    constructor Create(Token: TToken; Owner: TTokenViewSource);
    destructor Destroy; override;
  end;

  TTokenViewSource = class
  private
    ListView: TListViewEx;
    DataClasses: array of TTokenStringClass;
    function GetToken(Ind: Integer): TToken;
  public
    constructor Create(OwnedListView: TListViewEx);
    function Add(Token: TToken): TToken;
    procedure Delete(Index: Integer);
    function Selected: TToken;
    property Tokens[Ind: Integer]: TToken read GetToken;
    destructor Destroy; override;
  end;

implementation

uses
  System.Generics.Collections,
  UI.Colors, UI.Modal.PickUser, UI.Settings,
  TU.Winapi, TU.Common;

{ TPrivilegesSource }

function TPrivilegesSource.AddPrivilege(Privilege: TPrivilege): TListItemEx;
begin
  SetLength(FAdditionalPrivileges, Length(FAdditionalPrivileges) + 1);
  FAdditionalPrivileges[High(FAdditionalPrivileges)] := Privilege;
  Result := SetItem(Privilege);
end;

function TPrivilegesSource.CheckedPrivileges: TPrivilegeArray;
var
  i, Count: integer;
begin
  Assert(Assigned(ListView));

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

constructor TPrivilegesSource.Create(OwnedListView: TListViewEx);
begin
  ListView := OwnedListView;
end;

destructor TPrivilegesSource.Destroy;
begin
  UnsubscribeToken;
  inherited;
end;

function TPrivilegesSource.GetPrivilege(Ind: Integer): TPrivilege;
begin
  if Ind <= High(FTokenPrivileges) then
    Result := FTokenPrivileges[Ind]
  else
    Result := FAdditionalPrivileges[Ind - Length(FTokenPrivileges)];
end;

procedure TPrivilegesSource.OnPrivilegeChange(NewPrivileges: TPrivilegeArray);
var
  i: integer;
begin
  Assert(Assigned(ListView));

  with ListView do
  begin
    Items.BeginUpdate(True);
    Clear;

    // Update privileges from the token
    FTokenPrivileges := NewPrivileges;
    for i := 0 to High(NewPrivileges) do
      SetItem(NewPrivileges[i]);

    // Also show preserve additional ones
    for i := 0 to High(FAdditionalPrivileges) do
      SetItem(FAdditionalPrivileges[i]);

    Items.EndUpdate(True);
  end;
end;

function TPrivilegesSource.Privileges: TPrivilegeArray;
begin
  Result := Concat(FTokenPrivileges, FAdditionalPrivileges);
end;

function TPrivilegesSource.RemovePrivilege(Index: Integer): Boolean;
begin
  Result := (Index > High(FTokenPrivileges)) and
    (Index <= Length(FTokenPrivileges) + High(FAdditionalPrivileges));

  Assert(Assigned(ListView));

  if Result then
  begin
    Delete(FAdditionalPrivileges, Index - Length(FTokenPrivileges), 1);
    ListView.Items.Delete(Index);
  end;
end;

function TPrivilegesSource.SelectedPrivileges: TPrivilegeArray;
var
  i, j: integer;
begin
  Assert(Assigned(ListView));

  SetLength(Result, ListView.SelCount);
  j := 0;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      Result[j] := GetPrivilege(i);
      Inc(j);
    end;
end;

function TPrivilegesSource.SetItem(Privilege: TPrivilege): TListItemEx;
begin
  Assert(Assigned(ListView));

  Result := ListView.Items.Add;
  Result.Caption := Privilege.Name;
  Result.SubItems.Add(Privilege.AttributesToString);
  Result.SubItems.Add(Privilege.Description);
  Result.SubItems.Add(Privilege.Luid.ToString);
  Result.Color := PrivilegeToColor(Privilege);
end;

procedure TPrivilegesSource.SubscribeToken(Token: TToken);
begin
  UnsubscribeToken;

  Self.Token := Token;
  Token.OnClose.Add(UnsubscribeToken);

  Token.InfoClass.Query(tdTokenPrivileges);
  Token.Events.OnPrivilegesChange.Add(OnPrivilegeChange);
end;

procedure TPrivilegesSource.UnsubscribeToken(Dummy: TToken = nil);
begin
  if Assigned(Token) then
  begin
    Token.Events.OnPrivilegesChange.Delete(OnPrivilegeChange);
    Token.OnClose.Delete(UnsubscribeToken);
    Token := nil;
  end;
end;

{ TGroupsSource }

function TGroupsSource.AddGroup(Group: TGroup): TListItemEx;
begin
  Assert(Assigned(ListView));

  SetLength(FAdditionalGroups, Length(FAdditionalGroups) + 1);
  FAdditionalGroups[High(FAdditionalGroups)] := Group;

  Result := SetItem(ListView.Items.Add, Group);
end;

class function TGroupsSource.BuildHint(SID: TSecurityIdentifier;
  Attributes: TGroupAttributes; AttributesPresent: Boolean): String;
const
  ITEM_FORMAT = '%s:'#$D#$A'  %s';
var
  Items: TList<String>;
begin
  Items := TList<String>.Create;
  try
    if SID.HasPrettyName then
      Items.Add(Format(ITEM_FORMAT, ['Pretty name', SID.ToString]));
    Items.Add(Format(ITEM_FORMAT, ['SID', SID.SID]));
    Items.Add(Format(ITEM_FORMAT, ['Type', SID.SIDTypeToString]));
    if AttributesPresent then
    begin
      Items.Add(Format(ITEM_FORMAT, ['State', Attributes.StateToString]));
      if Attributes.ContainAnyFlags then
        Items.Add(Format(ITEM_FORMAT, ['Flags', Attributes.FlagsToString]));
    end;
    Result := String.Join(#$D#$A, Items.ToArray);
  finally
    Items.Free;
  end;
end;

function TGroupsSource.CheckedGroups: TGroupArray;
var
  i, Count: integer;
begin
  Assert(Assigned(ListView));

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

constructor TGroupsSource.Create(OwnedListView: TListViewEx);
begin
  ListView := OwnedListView;
end;

destructor TGroupsSource.Destroy;
begin
  UnsubscribeToken;
  inherited;
end;

function TGroupsSource.GetGroup(Ind: Integer): TGroup;
begin
  if Ind < Length(FTokenGroups) then
    Result := FTokenGroups[Ind]
  else
    Result := FAdditionalGroups[Ind - Length(FTokenGroups)];
end;

function TGroupsSource.Groups: TGroupArray;
begin
  Result := Concat(FTokenGroups, FAdditionalGroups);
end;

function TGroupsSource.IsAdditional(Index: Integer): Boolean;
begin
  Result := Index >= Length(FTokenGroups);
end;

procedure TGroupsSource.OnGroupsChange(NewGroups: TGroupArray);
var
  i: Integer;
begin
  Assert(Assigned(ListView));

  FTokenGroups := NewGroups;

  with ListView do
  begin
    Items.BeginUpdate(True);
    Items.Clear;

    // Show groups from the token
    for i := 0 to High(NewGroups) do
      SetItem(Items.Add, NewGroups[i]);

    // Show additional groups
    for i := 0 to High(FAdditionalGroups) do
      SetItem(Items.Add, FAdditionalGroups[i]);

    Items.EndUpdate(True);
  end;
end;

procedure TGroupsSource.RemoveGroup(Index: Integer);
begin
  Assert(Assigned(ListView));

  if Index > Length(FTokenGroups) + High(FAdditionalGroups) then
    raise EArgumentOutOfRangeException.Create('Index is out of range');

  if not IsAdditional(Index) then
    Exit;

  Delete(FAdditionalGroups, Index - Length(FTokenGroups), 1);
  ListView.Items.Delete(Index);
end;

function TGroupsSource.SelectedGroups: TGroupArray;
var
  i, j: integer;
begin
  Assert(Assigned(ListView));

  SetLength(Result, ListView.SelCount);
  j := 0;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
    begin
      Result[j] := GetGroup(i);
      Inc(j);
    end;
end;

procedure TGroupsSource.SetGroup(Ind: Integer; const Value: TGroup);
begin
  Assert(Assigned(ListView));

  if IsAdditional(Ind) then
  begin
    FAdditionalGroups[Ind - Length(FTokenGroups)] := Value;
    SetItem(ListView.Items[Ind], Value);
  end;
end;

function TGroupsSource.SetItem(Item: TListItemEx; Group: TGroup): TListItemEx;
begin
  Item.Caption := Group.SecurityIdentifier.ToString;
  Item.Hint := BuildHint(Group.SecurityIdentifier, Group.Attributes);
  Item.SubItems.Clear;
  Item.SubItems.Add(Group.Attributes.StateToString);
  Item.SubItems.Add(Group.Attributes.FlagsToString);
  Item.Color := GroupAttributesToColor(Group.Attributes);
  Result := Item;
end;

procedure TGroupsSource.SubscribeToken(Token: TToken; Mode: TGroupsSourceMode);
begin
  UnsubscribeToken;

  Self.Mode := Mode;
  Self.Token := Token;
  Token.OnClose.Add(UnsubscribeToken);

  if Mode = gsGroups then
  begin
    // Query groups and subcribe for the event. The subscription will
    // automatically invoke our event listener.
    Token.InfoClass.Query(tdTokenGroups);
    Token.Events.OnGroupsChange.Add(OnGroupsChange);
  end
  else if Mode = gsRestrictedSIDs then
  begin
    // Restricted SIDs do not have an associated event.
    // Invoke the event listener manually.
    if Token.InfoClass.Query(tdTokenRestrictedSids) then
      OnGroupsChange(Token.InfoClass.RestrictedSids);
  end;
end;

procedure TGroupsSource.UiEditSelected(AOwner: TComponent;
  DisableAttributes: Boolean);
var
  AttributesToAdd, AttributesToDelete: Cardinal;
  i: integer;
  NewGroup: TGroup;
begin
  Assert(Assigned(ListView));

  if ListView.SelCount = 0 then
    Exit;

  // Edit one group: SID and [optionaly] attributes
  if (ListView.SelCount = 1) and Assigned(ListView.Selected) then
    with ListView.Selected do
      if IsAdditional(Index) then
        Group[Index] := TDialogPickUser.PickEditOne(AOwner, Group[Index],
          DisableAttributes);

  // Edit several groups: only attributes
  if not DisableAttributes and (ListView.SelCount > 1) then
  begin
    TDialogPickUser.PickEditMultiple(AOwner, SelectedGroups, AttributesToAdd,
      AttributesToDelete);

    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].Selected and IsAdditional(i) then
      begin
        NewGroup := Group[i];
        NewGroup.Attributes := TGroupAttributes(Cardinal(NewGroup.Attributes)
          and not AttributesToDelete or AttributesToAdd);
        Group[i] := NewGroup;
      end;
  end;
end;

procedure TGroupsSource.UnsubscribeToken(Dummy: TToken);
begin
  if Assigned(Token) then
  begin
    // Restricted SIDs do not have an event, but Groups do
    if Mode = gsGroups then
      Token.Events.OnGroupsChange.Delete(OnGroupsChange);

    Token.OnClose.Delete(UnsubscribeToken);
    Token := nil;
  end;
end;

{ TSessionSource }

constructor TSessionSource.Create(OwnedComboBox: TComboBox;
  SelectCurrent: Boolean);
begin
  ComboBox := OwnedComboBox;
  RefreshSessionList(SelectCurrent);
end;

destructor TSessionSource.Destroy;
begin
  SessionList.Free;
  inherited;
end;

function TSessionSource.GetSession: Cardinal;
begin
  Assert(Assigned(ComboBox));

  if ComboBox.ItemIndex = -1 then
    Result := StrToUIntEx(ComboBox.Text, 'session')
  else
    Result := SessionList[ComboBox.ItemIndex].SessionId;
end;

procedure TSessionSource.RefreshSessionList(SelectCurrent: Boolean);
var
  i: Integer;
begin
  Assert(Assigned(ComboBox));

  // THINK: Should we preserve the selection? Note that Info window re-assigns
  // the value on Refresh action because it re-queries it.

  SessionList.Free;
  SessionList := TSessionList.CreateCurrentServer;

  ComboBox.Items.BeginUpdate;
  ComboBox.Items.Clear;

  for i := 0 to SessionList.Count - 1 do
    ComboBox.Items.Add(SessionList[i].ToString);

  if SelectCurrent and (SessionList.Count > 0) then
    ComboBox.ItemIndex := SessionList.Find(GetCurrentSession);

  ComboBox.Items.EndUpdate;
end;

procedure TSessionSource.SetSession(const Value: Cardinal);
begin
  Assert(Assigned(ComboBox));

  ComboBox.ItemIndex := SessionList.Find(Value);
  if ComboBox.ItemIndex = -1 then
    ComboBox.Text := IntToStr(Value);
end;

{ TIntegritySource }

constructor TIntegritySource.Create(OwnedComboBox: TComboBox);
begin
  ComboBox := OwnedComboBox;
  RefreshList;
end;

function TIntegritySource.GetIntegrityLevel: TTokenIntegrityLevel;
const
  IndexToIntegrity: array [0 .. 6] of TTokenIntegrityLevel = (ilUntrusted,
    ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem, ilProtected);
begin
  Assert(Assigned(ComboBox));

  with ComboBox do
  begin
    if ItemIndex = -1 then
      Result := TTokenIntegrityLevel(StrToUIntEx(Text, 'integrity'))
    else if not IsIntermediate or (ItemIndex < IntermediateIndex) then
      Result := IndexToIntegrity[ItemIndex]
    else if ItemIndex > IntermediateIndex then
      Result := IndexToIntegrity[ItemIndex - 1]
    else
      Result := IntermediateValue;
  end;
end;

procedure TIntegritySource.RefreshList;
begin
  Assert(Assigned(ComboBox));

  with ComboBox do
  begin
    Items.BeginUpdate;
    Clear;

    Items.Add('Untrusted (0x0000)');
    Items.Add('Low (0x1000)');
    Items.Add('Medium (0x2000)');
    Items.Add('Medium Plus (0x2100)');
    Items.Add('High (0x3000)');
    Items.Add('System (0x4000)');
    Items.Add('Protected (0x5000)');

    Items.EndUpdate;
  end;
end;

procedure TIntegritySource.SetIntegrity(Value: TTokenIntegrity);
begin
  Assert(Assigned(ComboBox));

  with ComboBox do
  begin
    Items.BeginUpdate;
    RefreshList;

    // If the value is not a well-known one insert it in between two well knowns
    IsIntermediate := not Value.IsWellKnown;
    if IsIntermediate then
    begin
      IntermediateValue := Value.Level;

      if Value.Level < ilLow then
        IntermediateIndex := 1
      else if Value.Level < ilMedium then
        IntermediateIndex := 2
      else if Value.Level < ilMediumPlus then
        IntermediateIndex := 3
      else if Value.Level < ilHigh then
        IntermediateIndex := 4
      else if Value.Level < ilSystem then
        IntermediateIndex := 5
      else if Value.Level < ilProtected then
        IntermediateIndex := 6
      else
        IntermediateIndex := 7;

      Items.Insert(IntermediateIndex,
        Format('Itermediate (0x%0.4x)', [Cardinal(Value.Level)]));
    end;

    // Select appropriate item
    if Value.Level = ilUntrusted then
      ItemIndex := 0
    else if Value.Level <= ilLow then
      ItemIndex := 1
    else if Value.Level <= ilMedium then
      ItemIndex := 2
    else if Value.Level <= ilMediumPlus then
      ItemIndex := 3
    else if Value.Level <= ilHigh then
      ItemIndex := 4
    else if Value.Level <= ilSystem then
      ItemIndex := 5
    else if Value.Level <= ilProtected then
      ItemIndex := 6
    else
      ItemIndex := 7;

    Items.EndUpdate;
  end;
end;

{ TAccessMaskSource }

class function TAccessMaskSource.GetAccessMask(
  ListView: TListView): ACCESS_MASK;
var
  i: integer;
begin
  Assert(ListView.Items.Count = ACCESS_COUNT);

  Result := 0;
  for i := 0 to ACCESS_COUNT - 1 do
    if ListView.Items[i].Checked then
      Result := Result or AccessValues[i];
end;

class procedure TAccessMaskSource.InitAccessEntries(ListView: TListView;
  Access: ACCESS_MASK);
var
  i: integer;
  AccessGroup: TAccessGroup;
begin
  ListView.Groups.Clear;
  ListView.Items.Clear;

  for AccessGroup := Low(TAccessGroup) to High(TAccessGroup) do
  with ListView.Groups.Add do
  begin
    Header := AccessGroupStrings[AccessGroup];
    State := State + [lgsCollapsible];
  end;

  for i := 0 to ACCESS_COUNT - 1 do
  with ListView.Items.Add do
  begin
    Caption := AccessStrings[i];
    GroupID := Cardinal(AccessGroupValues[i]);
    ListView.Items[i].Checked := (Access and AccessValues[i] = AccessValues[i]);
  end;
end;

{ TLogonSessionSource }

constructor TLogonSessionSource.Create(OwnedComboBox: TComboBox);
begin
  ComboBox := OwnedComboBox;
  UpdateLogonSessions;
end;

function TLogonSessionSource.GetSelected: LUID;
var
  LogonId: UInt64;
begin
  Assert(ComboBox.Items.Count = Length(FLogonSessions));

  if ComboBox.ItemIndex = -1 then
  begin
    LogonId := StrToUInt64Ex(ComboBox.Text, 'logon ID');
    Result := PLUID(@LogonId)^;
  end
  else
    Result := FLogonSessions[ComboBox.ItemIndex];
end;

procedure TLogonSessionSource.SetSelected(const Value: LUID);
var
  i: integer;
begin
  Assert(ComboBox.Items.Count = Length(FLogonSessions));

  for i := 0 to High(FLogonSessions) do
    if Value.ToUInt64 = FLogonSessions[i].ToUInt64 then
    begin
      ComboBox.ItemIndex := i;
      Exit;
    end;

  ComboBox.ItemIndex := -1;
  ComboBox.Text := Value.ToString;
end;

procedure TLogonSessionSource.UpdateLogonSessions;
var
  i: integer;
  S: String;
begin
  FLogonSessions := EnumerateLogonSessions;
  ComboBox.Items.Clear;
  for i := 0 to High(FLogonSessions) do
  begin
    S := FLogonSessions[i].ToString;
      with QueryLogonSession(FLogonSessions[i]) do
        if IsValid and Value.UserPresent and (Value.User.User <> '') then
          S := Format('%s (%s #%d)', [S, Value.User.User, Value.Session]);
    ComboBox.Items.Add(S);
  end;
end;

{ TCellSource }

constructor TCellSource.Create(Row: TRowSource; ColumnIndex: Integer);
begin
  Self.Row := Row;
  Self.ColumnIndex := ColumnIndex;

  // Each cell subscribes corresponding string querying event
  Row.Token.Events.SubscribeString(
    Row.Owner.DataClasses[ColumnIndex], SetTextCallback, Row.Token);
end;

destructor TCellSource.Destroy;
begin
  Row.Token.Events.UnSubscribeString(
    Row.Owner.DataClasses[ColumnIndex], SetTextCallback);
  inherited;
end;

procedure TCellSource.SetTextCallback(Value: String);
begin
  Assert(Row.Item.SubItems.Count > ColumnIndex);
  Row.Item.SubItems[ColumnIndex] := Value;
end;

{ TRowSource }

constructor TRowSource.Create(Token: TToken; Owner: TTokenViewSource);
var
  i: integer;
begin
  Self.Token := Token; // TODO: Subscribe Token.OnClose
  Self.Owner := Owner;

  // Add ListView Item and store it
  Item := Owner.ListView.Items.Add;
  Item.OwnedData := Self;

  // Subscribe main column updates
  Token.OnCaptionChange.Add(TokenCaptionCallback, False);
  TokenCaptionCallback(Token.Caption);

  // Initialize sources for all other columns
  SetLength(Cells, Length(Owner.DataClasses));
  for i := 0 to High(Cells) do
  begin
    Item.SubItems.Add('Unknown');
    Cells[i] := TCellSource.Create(Self, i);
  end;
end;

destructor TRowSource.Destroy;
var
  i: Integer;
begin
  // Unsubscribe all column event listeners and free them
  for i := 0 to High(Cells) do
    Cells[i].Free;

  Token.OnCaptionChange.Delete(TokenCaptionCallback);
  Token.Free;

  inherited;
end;

procedure TRowSource.TokenCaptionCallback(Value: String);
begin
  Item.Caption := Value;
end;

{ TTokenViewSource }

function TTokenViewSource.Add(Token: TToken): TToken;
begin
  // This will create a new ListView Item and assign this object
  // as it's OwnedData
  TRowSource.Create(Token, Self);
  Result := Token;
end;

constructor TTokenViewSource.Create(OwnedListView: TListViewEx);
var
  tsc: TTokenStringClass;
  ColumnCount: Integer;
begin
  ListView := OwnedListView;

  with ListView.Columns do
  begin
    BeginUpdate;
    Clear;

    // Add the main and editable column
    with Add do
    begin
      Caption := 'Description';
      Width := 170;
    end;

    // Count all the columns from the settings
    ColumnCount := 0;
    for tsc in TSettings.SelectedColumns do
      Inc(ColumnCount);

    // Prepare a place to store column meanings
    SetLength(DataClasses, ColumnCount);

    // Add all other columns according to the settings
    ColumnCount := 0;
    for tsc in TSettings.SelectedColumns do
      with Add do
      begin
        Caption := ColumsInfo[tsc].Caption;
        Width := ColumsInfo[tsc].Width;
        Alignment := ColumsInfo[tsc].Alignment;
        DataClasses[ColumnCount] := tsc;
        Inc(ColumnCount);
      end;

    EndUpdate;
  end;
end;

procedure TTokenViewSource.Delete(Index: Integer);
begin
  if GetToken(Index).CanBeFreed then
  begin
    // This will delete the item, the assiciated row object, unsubscribe
    // all column events and close the token
    ListView.Items.Delete(Index);
  end;
end;

destructor TTokenViewSource.Destroy;
begin
  ListView.Items.Clear;
  inherited;
end;

function TTokenViewSource.GetToken(Ind: Integer): TToken;
begin
  Result := (ListView.Items[Ind].OwnedData as TRowSource).Token;
end;

function TTokenViewSource.Selected: TToken;
begin
  if Assigned(ListView.Selected) then
    Result := (ListView.Selected.OwnedData as TRowSource).Token
  else
  begin
    Result := nil;
    Abort;
  end;
end;

end.

