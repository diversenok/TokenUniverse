unit UI.Prototypes;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  UI.ListViewEx, TU.Tokens, TU.Common, TU.WtsApi;

type
  TTokenedListViewEx = class(TListViewEx)
  private
    FToken: TToken;
    procedure ReleaseToken;
    procedure SetToken(const Value: TToken);
  protected
    procedure SubscribeToken; virtual; abstract;
    procedure UnsubscribeToken; virtual; abstract;
  public
    destructor Destroy; override;
    property Token: TToken read FToken write SetToken;
  end;

  TPrivilegesListViewEx = class(TTokenedListViewEx)
  private
    FPrivileges: TPrivilegeArray;
    procedure ChangedPrivileges(NewPrivileges: CanFail<TPrivilegeArray>);
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
    FGroups: TGroupArray;
    FViewAs: TGroupViewAs;
    FSource: TGroupSource;
    procedure ChangedGroups(NewGroups: CanFail<TGroupArray>);
    procedure SetSource(const Value: TGroupSource);
    procedure SetViewAs(const Value: TGroupViewAs);
  protected
    procedure SubscribeToken; override;
    procedure UnsubscribeToken; override;
  public
    property Groups: TGroupArray read FGroups;
  published
    property ViewAs: TGroupViewAs read FViewAs write SetViewAs default gvUser;
    property Source: TGroupSource read FSource write SetSource default gsGroups;
    function SelectedGroups: TGroupArray;
    function CheckedGroups: TGroupArray;
  end;

  TSessionComboBox = class(TComboBox)
  private
    Sessions: TSessionList;
    function GetSession: Cardinal;
    procedure SetSession(const Value: Cardinal);
  public
    destructor Destroy; override;
    procedure RefreshSessionList;
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

procedure Register;

implementation

uses
  UI.Colors;

procedure Register;
begin
  RegisterComponents('Token Universe', [TSessionComboBox, TIntegrityComboBox,
    TPrivilegesListViewEx, TGroupListViewEx]);
end;

{ TTokenedListViewEx }

destructor TTokenedListViewEx.Destroy;
begin
  ReleaseToken;
  inherited;
end;

procedure TTokenedListViewEx.ReleaseToken;
begin
  if Assigned(FToken) then
  begin
    UnsubscribeToken;
    FToken := nil;
  end;
end;

procedure TTokenedListViewEx.SetToken(const Value: TToken);
begin
  ReleaseToken;
  FToken := Value;
  if Assigned(FToken) then  
    SubscribeToken;
end;

{ TPrivilegesListViewEx }

procedure TPrivilegesListViewEx.ChangedPrivileges(NewPrivileges: CanFail<TPrivilegeArray>);
var
  i: integer;
begin
  Items.BeginUpdate(True);
  Clear;
  with NewPrivileges do
    if IsValid then
    begin
      FPrivileges := Value;
      for i := 0 to High(Value) do
      with Value[i], Items.Add do
      begin
        Caption := Name;
        SubItems.Add(Value[i].AttributesToString);
        SubItems.Add(Value[i].Description);
        SubItems.Add(Value[i].Luid.ToString);
        Color := PrivilegeToColor(Value[i], Self.Color);
      end;
    end
    else
      SetLength(FPrivileges, 0);
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
  Token.OnPrivilegesChange.Add(ChangedPrivileges);
  ChangedPrivileges(Token.Privileges);
end;

procedure TPrivilegesListViewEx.UnsubscribeToken;
begin
  Token.OnPrivilegesChange.Delete(ChangedPrivileges);
end;

{ TGroupListViewEx }

procedure TGroupListViewEx.ChangedGroups(NewGroups: CanFail<TGroupArray>);
var
  i: Integer;
begin
  Items.BeginUpdate(True);
  Clear;
  with NewGroups do
    if IsValid then
    begin
      FGroups := Value;
      for i := 0 to High(Value) do
      with Value[i], Items.Add do
      begin
        case FViewAs of
          gvUser: Caption := SecurityIdentifier.ToString;
          gvSID: Caption := SecurityIdentifier.SID;
        end;
        SubItems.Add(Attributes.StateToString);
        SubItems.Add(Attributes.FlagsToString);
        Color := GroupAttributesToColor(Attributes);
      end;
    end
    else
      SetLength(FGroups, 0);
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
      Result[High(Result)] := Groups[i];
    end;
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
      Result[j] := Groups[i];
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
    ChangedGroups(CanFail<TGroupArray>.SucceedWith(FGroups));
end;

procedure TGroupListViewEx.SubscribeToken;
begin
  if FSource = gsGroups then
    Token.OnGroupsChange.Add(ChangedGroups);

  case FSource of
    gsGroups: ChangedGroups(Token.Groups);
    gsRestrictedSIDs: ChangedGroups(Token.RestrictedSids);
  end;
end;

procedure TGroupListViewEx.UnsubscribeToken;
begin
  if FSource = gsGroups then
    Token.OnGroupsChange.Delete(ChangedGroups);
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
    Result := StrToIntEx(Text, 'session')
  else
    Result := Sessions[ItemIndex].SessionId;
end;

procedure TSessionComboBox.RefreshSessionList;
var
  i: integer;
begin
  Sessions.Free;
  Sessions := TSessionList.CreateCurrentServer;
  Items.BeginUpdate;
  Items.Clear;

  for i := 0 to Sessions.Count - 1 do
    Items.Add(Sessions[i].ToString);

  if Sessions.Count > 0 then
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
    Result := TTokenIntegrityLevel(StrToIntEx(Text, 'integrity'))
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

end.
