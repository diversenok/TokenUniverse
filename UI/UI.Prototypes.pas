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
    procedure InitChange;
  protected
    procedure SubscribeToken; override;
    procedure UnsubscribeToken; override;
  public
    property Groups: TGroupArray read FGroups;
  published
    property ViewAs: TGroupViewAs read FViewAs write SetViewAs default gvUser;
    property Source: TGroupSource read FSource write SetSource default gsGroups;
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

procedure Register;

implementation

uses
  UI.Colors;

procedure Register;
begin
  RegisterComponents('Token Universe', [TSessionComboBox,
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

procedure TGroupListViewEx.InitChange;
begin
  case FSource of
    gsGroups: ChangedGroups(Token.Groups);
    gsRestrictedSIDs: ChangedGroups(Token.RestrictedSids);
  end;
end;

procedure TGroupListViewEx.SetSource(const Value: TGroupSource);
begin
  if Assigned(Token) then  
    UnsubscribeToken;
    
  FSource := Value;

  if Assigned(Token) then  
    SubscribeToken;
end;

procedure TGroupListViewEx.SetViewAs(const Value: TGroupViewAs);
begin
  FViewAs := Value;
  if Assigned(Token) then
    ChangedGroups(CanFail<TGroupArray>.SucceedWith(FGroups));
end;

procedure TGroupListViewEx.SubscribeToken;
begin
  InitChange;
end;

procedure TGroupListViewEx.UnsubscribeToken;
begin

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

end.
