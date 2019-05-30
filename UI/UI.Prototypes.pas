unit UI.Prototypes;

interface

uses
  System.SysUtils, Vcl.ComCtrls, Vcl.StdCtrls, UI.ListViewEx, TU.Tokens,
  TU.LsaApi, TU.Tokens.Types, Winapi.WinNt, NtUtils.WinStation;

type
  TSessionSource = class
  private
    Sessions: TSessionArray;
    ComboBox: TComboBox;
    function GetSession: Cardinal;
    procedure SetSession(const Value: Cardinal);
  public
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
    class procedure InitAccessEntries(ListView: TListView; Access: TAccessMask);
    class function GetAccessMask(ListView: TListView): TAccessMask;
  end;

  TLogonSessionSource = class
  private
    FLogonSessions: TLuidDynArray;
    ComboBox: TComboBox;
    function GetSelected: TLuid;
    procedure SetSelected(const Value: TLuid);
  public
    const NO_LOGON = '0 (value not set)';
    constructor Create(OwnedComboBox: TComboBox);
    procedure UpdateLogonSessions;
    property SelectedLogonSession: TLuid read GetSelected write SetSelected;
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
    function GetCount: Integer;
  public
    constructor Create(OwnedListView: TListViewEx);
    function Add(Token: TToken): TToken;
    procedure Delete(Index: Integer);
    function Selected: TToken;
    property Count: Integer read GetCount;
    property Tokens[Ind: Integer]: TToken read GetToken;
    destructor Destroy; override;
  end;

implementation

uses
  DelphiUtils.Strings, UI.Settings, TU.Winapi, Ntapi.ntrtl, NtUtils.Lsa;

{ TSessionSource }

constructor TSessionSource.Create(OwnedComboBox: TComboBox;
  SelectCurrent: Boolean);
begin
  ComboBox := OwnedComboBox;
  RefreshSessionList(SelectCurrent);
end;

function TSessionSource.GetSession: Cardinal;
begin
  Assert(Assigned(ComboBox));

  if ComboBox.ItemIndex = -1 then
    Result := StrToUIntEx(ComboBox.Text, 'session')
  else
    Result := Sessions[ComboBox.ItemIndex].SessionId;
end;

procedure TSessionSource.RefreshSessionList(SelectCurrent: Boolean);
var
  i: Integer;
begin
  Assert(Assigned(ComboBox));

  // THINK: Should we preserve the selection? Note that Info window re-assigns
  // the value on Refresh action because it re-queries it.

  if not WsxEnumerateSessions(Sessions).IsSuccess then
    SetLength(Sessions, 0);

  ComboBox.Items.BeginUpdate;
  ComboBox.Items.Clear;

  for i := 0 to High(Sessions) do
    ComboBox.Items.Add(WsxQuerySessionName(Sessions[i].SessionId));

  if SelectCurrent and (Length(Sessions) > 0) then
    SetSession(RtlGetCurrentPeb.SessionId);

  ComboBox.Items.EndUpdate;
end;

procedure TSessionSource.SetSession(const Value: Cardinal);
var
  i: Integer;
begin
  Assert(Assigned(ComboBox));

  ComboBox.ItemIndex := -1;

  for i := 0 to High(Sessions) do
    if Sessions[i].SessionId = Value then
    begin
      ComboBox.ItemIndex := i;
      Break;
    end;

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
  ListView: TListView): TAccessMask;
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
  Access: TAccessMask);
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

function TLogonSessionSource.GetSelected: TLuid;
begin
  Assert(ComboBox.Items.Count = Length(FLogonSessions));

  if ComboBox.ItemIndex = -1 then
  begin
    if ComboBox.Text = NO_LOGON then
      Result := 0
    else
    {$R-}
      Result := TLuid(StrToUInt64Ex(ComboBox.Text, 'logon ID'));
    {$R+}
  end
  else
    Result := FLogonSessions[ComboBox.ItemIndex];
end;

procedure TLogonSessionSource.SetSelected(const Value: TLuid);
var
  i: integer;
begin
  Assert(ComboBox.Items.Count = Length(FLogonSessions));

  for i := 0 to High(FLogonSessions) do
    if Value = FLogonSessions[i] then
    begin
      ComboBox.ItemIndex := i;
      Exit;
    end;

  ComboBox.ItemIndex := -1;
  if Value <> 0 then
    ComboBox.Text := IntToHexEx(Value)
  else
    ComboBox.Text := NO_LOGON;
end;

procedure TLogonSessionSource.UpdateLogonSessions;
var
  i: integer;
  S: String;
  LogonData: ILogonSession;
begin
  if not LsaxEnumerateLogonSessions(FLogonSessions).IsSuccess then
    SetLength(FLogonSessions, 0);

  ComboBox.Items.BeginUpdate;
  ComboBox.Items.Clear;
  for i := 0 to High(FLogonSessions) do
  begin
    S := IntToHexEx(FLogonSessions[i]);

    if TLogonSession.Query(FLogonSessions[i], LogonData).IsSuccess then
      if LogonData.UserPresent and (LogonData.User.Lookup.UserName <> '') then
        S := Format('%s (%s @ %d)', [S, LogonData.User.Lookup.UserName,
          LogonData.RawData.Session]);

    ComboBox.Items.Add(S);
  end;
  ComboBox.Items.EndUpdate;
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
  Token.OnCaptionChange.Subscribe(TokenCaptionCallback, False);
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

  Token.OnCaptionChange.Unsubscribe(TokenCaptionCallback);
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

function TTokenViewSource.GetCount: Integer;
begin
  Assert(Assigned(ListView));
  Result := ListView.Items.Count;
end;

function TTokenViewSource.GetToken(Ind: Integer): TToken;
begin
  Assert(Assigned(ListView));
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

