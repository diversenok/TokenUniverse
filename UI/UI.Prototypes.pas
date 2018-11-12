unit UI.Prototypes;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  Winapi.Windows, UI.ListViewEx, TU.Tokens, TU.Common, TU.WtsApi, TU.LsaApi;

type
  TTokenedListViewEx = class(TListViewEx)
  private
    FToken: TToken;
    procedure ReleaseToken(Sender: TToken);
    procedure SetToken(const Value: TToken);
  protected
    procedure SubscribeToken; virtual;
    procedure UnsubscribeToken; virtual;
  public
    destructor Destroy; override;
    property Token: TToken read FToken write SetToken;
  end;

  TPrivilegesListViewEx = class(TTokenedListViewEx)
  private
    FPrivileges: TPrivilegeArray;
    procedure ChangedPrivileges(NewPrivileges: TPrivilegeArray);
  protected
    procedure SubscribeToken; override;
    procedure UnsubscribeToken; override;
  public
    property Privileges: TPrivilegeArray read FPrivileges;
    function SelectedPrivileges: TPrivilegeLUIDArray;
    function CheckedPrivileges: TPrivilegeArray;
  end;

  TGroupViewAs = (gvUser, gvSID);
  TGroupSource = (gsGroups, gsRestrictedSIDs);

  TGroupListViewEx = class(TTokenedListViewEx)
  private
    FGroups, FAdditional: TGroupArray;
    FViewAs: TGroupViewAs;
    FSource: TGroupSource;
    procedure ChangedGroups(NewGroups: TGroupArray);
    procedure SetGroupItem(Item: TListItemEx; Group: TGroup);
    procedure SetSource(const Value: TGroupSource);
    procedure SetViewAs(const Value: TGroupViewAs);
    function GetGroup(Ind: Integer): TGroup;
    procedure SetGroup(Ind: Integer; const Value: TGroup);
  protected
    procedure SubscribeToken; override;
    procedure UnsubscribeToken; override;
  public
    property Groups[Ind: Integer]: TGroup read GetGroup write SetGroup;
    function AddGroup(Group: TGroup): Integer;
    function IsAdditional(Index: Integer): Boolean;
    procedure RemoveGroup(Index: Integer);
    class function BuildHint(SID: TSecurityIdentifier;
      Attributes: TGroupAttributes; AttributesPresent: Boolean = True): String;
      static;
  published
    property ViewAs: TGroupViewAs read FViewAs write SetViewAs default gvUser;
    property Source: TGroupSource read FSource write SetSource default gsGroups;
    function SelectedGroups: TGroupArray;
    function AllGroups: TGroupArray;
    function CheckedGroups: TGroupArray;
  end;

  TSessionComboBox = class(TComboBox)
  private
    Sessions: TSessionList;
    function GetSession: Cardinal;
    procedure SetSession(const Value: Cardinal);
  public
    destructor Destroy; override;
    procedure RefreshSessionList(SelectSomething: Boolean);
    property SelectedSession: Cardinal read GetSession write SetSession;
  end;

  TIntegrityComboBox = class(TComboBox)
  private
    FIsIntermediate: Boolean;
    FIntermediateValue: TTokenIntegrityLevel;
    FIntermediateIndex: Integer;
    function GetIntegrityLevel: TTokenIntegrityLevel;
  public
    procedure SetIntegrity(NewIntegrity: CanFail<TTokenIntegrity>);
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

procedure Register;

implementation

uses
  System.Generics.Collections, UI.Colors, TU.Tokens.Winapi;

procedure Register;
begin
  RegisterComponents('Token Universe', [TSessionComboBox, TIntegrityComboBox,
    TPrivilegesListViewEx, TGroupListViewEx]);
end;

{ TTokenedListViewEx }

destructor TTokenedListViewEx.Destroy;
begin
  ReleaseToken(FToken);
  inherited;
end;

procedure TTokenedListViewEx.ReleaseToken(Sender: TToken);
begin
  if Assigned(FToken) then
  begin
    UnsubscribeToken;
    FToken := nil;
  end;
end;

procedure TTokenedListViewEx.SetToken(const Value: TToken);
begin
  ReleaseToken(FToken);
  FToken := Value;
  if Assigned(FToken) then  
    SubscribeToken;
end;

procedure TTokenedListViewEx.SubscribeToken;
begin
  Token.OnClose.Add(ReleaseToken);
end;

procedure TTokenedListViewEx.UnsubscribeToken;
begin
  Token.OnClose.Delete(ReleaseToken);
end;

{ TPrivilegesListViewEx }

procedure TPrivilegesListViewEx.ChangedPrivileges(
  NewPrivileges: TPrivilegeArray);
var
  i: integer;
begin
  Items.BeginUpdate(True);
  Clear;
  FPrivileges := NewPrivileges;
  for i := 0 to High(NewPrivileges) do
  with NewPrivileges[i], Items.Add do
  begin
    Caption := Name;
    SubItems.Add(NewPrivileges[i].AttributesToString);
    SubItems.Add(NewPrivileges[i].Description);
    SubItems.Add(NewPrivileges[i].Luid.ToString);
    Color := PrivilegeToColor(NewPrivileges[i]);
  end;
  Items.EndUpdate(True);
end;

function TPrivilegesListViewEx.CheckedPrivileges: TPrivilegeArray;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    if Items[i].Checked then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Privileges[i];
    end;
end;

function TPrivilegesListViewEx.SelectedPrivileges: TPrivilegeLUIDArray;
var
  i, j: integer;
begin
  SetLength(Result, SelCount);
  j := 0;
  for i := 0 to Items.Count - 1 do
    if Items[i].Selected then
    begin
      Result[j] := Privileges[i].Luid;
      Inc(j);
    end;
end;

procedure TPrivilegesListViewEx.SubscribeToken;
begin
  inherited;
  Token.Events.OnPrivilegesChange.Add(ChangedPrivileges);
end;

procedure TPrivilegesListViewEx.UnsubscribeToken;
begin
  Token.Events.OnPrivilegesChange.Delete(ChangedPrivileges);
  inherited;
end;

{ TGroupListViewEx }

function TGroupListViewEx.AddGroup(Group: TGroup): Integer;
begin
  SetLength(FAdditional, Length(FAdditional) + 1);
  FAdditional[High(FAdditional)] := Group;
  SetGroupItem(Items.Add, Group);
end;

procedure TGroupListViewEx.SetGroup(Ind: Integer; const Value: TGroup);
begin
  if Ind >= Length(FGroups) then
  begin
    FAdditional[Ind - Length(FGroups)] := Value;
    SetGroupItem(Items[Ind], Value);
  end;
end;

procedure TGroupListViewEx.SetGroupItem(Item: TListItemEx; Group: TGroup);
begin
  with Group, Item do
  begin
    case FViewAs of
      gvUser: Caption := SecurityIdentifier.ToString;
      gvSID: Caption := SecurityIdentifier.SID;
    end;
    Hint := BuildHint(SecurityIdentifier, Attributes);
    SubItems.Clear;
    SubItems.Add(Attributes.StateToString);
    SubItems.Add(Attributes.FlagsToString);
    Color := GroupAttributesToColor(Attributes);
  end;
end;

function TGroupListViewEx.AllGroups: TGroupArray;
begin
  Result := Concat(FGroups, FAdditional);
end;

class function TGroupListViewEx.BuildHint(SID: TSecurityIdentifier;
  Attributes: TGroupAttributes; AttributesPresent: Boolean): String;
const
  ITEM_FORMAT = '%s:'#$D#$A'  %s';
var
  Items: TList<String>;
begin
  Items := TList<String>.Create;;
  try
    if SID.HasPrettyName then
      Items.Add(Format(ITEM_FORMAT, ['Pretty name', SID.ToString]));
    Items.Add(Format(ITEM_FORMAT, ['SID', SID.SID]));
    Items.Add(Format(ITEM_FORMAT, ['Type', SID.SIDType.ToString]));
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

procedure TGroupListViewEx.ChangedGroups(NewGroups: TGroupArray);
var
  i: Integer;
begin
  FGroups := NewGroups;

  Items.BeginUpdate(True);
  Clear;
  for i := 0 to High(FGroups) do
    SetGroupItem(Items.Add, FGroups[i]);
  for i := 0 to High(FAdditional) do
    SetGroupItem(Items.Add, FAdditional[i]);
  Items.EndUpdate(True);
end;

function TGroupListViewEx.CheckedGroups: TGroupArray;
var
  i: integer;
begin
  SetLength(Result, 0);
  for i := 0 to Items.Count - 1 do
    if Items[i].Checked then
    begin
      SetLength(Result, Length(Result) + 1);
      if i < Length(FGroups) then
        Result[High(Result)] := FGroups[i]
      else
        Result[High(Result)] := FAdditional[i - Length(FGroups)];
    end;
end;

function TGroupListViewEx.GetGroup(Ind: Integer): TGroup;
begin
  if Ind < Length(FGroups) then
    Result := FGroups[Ind]
  else
    Result := FAdditional[Ind - Length(FGroups)];
end;

function TGroupListViewEx.IsAdditional(Index: Integer): Boolean;
begin
  Result := Index >= Length(FGroups);
end;

procedure TGroupListViewEx.RemoveGroup(Index: Integer);
var
  i: Integer;
begin
  // Groups from token should be immutable
  if Index < Length(FGroups) then
    Exit;

  for i := Index - Length(FGroups) to High(FAdditional) - 1 do
    FAdditional[i] := FAdditional[i + 1];

  Items.Delete(Index);
end;

function TGroupListViewEx.SelectedGroups: TGroupArray;
var
  i, j: integer;
begin
  SetLength(Result, SelCount);
  j := 0;
  for i := 0 to Items.Count - 1 do
    if Items[i].Selected then
    begin
      if i < Length(FGroups) then
        Result[j] := FGroups[i]
      else
        Result[j] := FAdditional[i - Length(FGroups)];
      Inc(j);
    end;
end;

procedure TGroupListViewEx.SetSource(const Value: TGroupSource);
begin
  if FSource = Value then
    Exit;

  if Assigned(Token) then  
    UnsubscribeToken;
    
  FSource := Value;

  if Assigned(Token) then  
    SubscribeToken;
end;

procedure TGroupListViewEx.SetViewAs(const Value: TGroupViewAs);
begin
  if FViewAs = Value then
    Exit;

  FViewAs := Value;
  if Assigned(Token) then
    ChangedGroups(FGroups);
end;

procedure TGroupListViewEx.SubscribeToken;
begin
  inherited;
  if FSource = gsGroups then
    Token.Events.OnGroupsChange.Add(ChangedGroups)
  else if FSource = gsRestrictedSIDs then
    with Token.RestrictedSids do
        if IsValid then
          ChangedGroups(Value);
end;

procedure TGroupListViewEx.UnsubscribeToken;
begin
  if FSource = gsGroups then
    Token.Events.OnGroupsChange.Delete(ChangedGroups);
  inherited;
end;

{ TSessionComboBox }

destructor TSessionComboBox.Destroy;
begin
  Sessions.Free;
  inherited;
end;

function TSessionComboBox.GetSession: Cardinal;
begin
  if ItemIndex = -1 then
    Result := StrToUIntEx(Text, 'session')
  else
    Result := Sessions[ItemIndex].SessionId;
end;

procedure TSessionComboBox.RefreshSessionList(SelectSomething: Boolean);
var
  i: integer;
begin
  Sessions.Free;
  Sessions := TSessionList.CreateCurrentServer;
  Items.BeginUpdate;
  Items.Clear;

  for i := 0 to Sessions.Count - 1 do
    Items.Add(Sessions[i].ToString);

  if SelectSomething and (Sessions.Count > 0) then
    ItemIndex := 0;

  Items.EndUpdate;
end;

procedure TSessionComboBox.SetSession(const Value: Cardinal);
begin
  ItemIndex := Sessions.Find(Value);
  if ItemIndex = -1 then
    Text := IntToStr(Value);
end;

{ TIntegrityComboBox }

function TIntegrityComboBox.GetIntegrityLevel: TTokenIntegrityLevel;
const
  IndexToIntegrity: array [0 .. 5] of TTokenIntegrityLevel = (ilUntrusted,
    ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem);
begin
  if ItemIndex = -1 then
    Result := TTokenIntegrityLevel(StrToUIntEx(Text, 'integrity'))
  else if not FIsIntermediate or (ItemIndex < FIntermediateIndex) then
    Result := IndexToIntegrity[ItemIndex]
  else if ItemIndex > FIntermediateIndex then
    Result := IndexToIntegrity[ItemIndex - 1]
  else
    Result := FIntermediateValue;
end;

procedure TIntegrityComboBox.SetIntegrity(
  NewIntegrity: CanFail<TTokenIntegrity>);
begin
  Items.BeginUpdate;
  Clear;

  Items.Add('Untrusted (0x0000)');
  Items.Add('Low (0x1000)');
  Items.Add('Medium (0x2000)');
  Items.Add('Medium Plus (0x2100)');
  Items.Add('High (0x3000)');
  Items.Add('System (0x4000)');

  with NewIntegrity do
    if IsValid then
    begin
      FIsIntermediate := not Value.Level.IsWellKnown;

      if FIsIntermediate then
      begin
        FIntermediateValue := Value.Level;

        if Value.Level < ilLow then
          FIntermediateIndex := 1
        else if Value.Level < ilMedium then
          FIntermediateIndex := 2
        else if Value.Level < ilMediumPlus then
          FIntermediateIndex := 3
        else if Value.Level < ilHigh then
          FIntermediateIndex := 4
        else if Value.Level < ilSystem then
          FIntermediateIndex := 5
        else
          FIntermediateIndex := 6;

        Items.Insert(FIntermediateIndex, Format('Itermediate (0x%.4x)',
          [Cardinal(Value.Level)]));
      end;

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
      else
        ItemIndex := 6;
    end
    else
    begin
      ItemIndex := -1;
      Text := 'Unknown integrity';
    end;

  Items.EndUpdate;
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
      with GetLogonSessionInformation(FLogonSessions[i]) do
        if IsValid and Value.UserPresent then
          S := S + ' (' + Value.User.ToString + ')';
    ComboBox.Items.Add(S);
  end;
end;

end.
