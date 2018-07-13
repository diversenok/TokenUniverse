unit UI.TokenListFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.ComCtrls,
  TU.Tokens, TU.Handles;

type
  TTokenItem = class
    Token: TToken;
    StringData: array of String;
    OriginalIndex: Integer;
    GroupIndex: Integer;
    destructor Destroy; override;
  end;

  TFrameTokenList = class(TFrame)
    ListViewTokens: TListView;
    SearchButtons: TImageList;
    SearchBox: TButtonedEdit;
    ComboBoxColumn: TComboBox;
    procedure SearchBoxRightButtonClick(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
  protected
    TokenDB: array of TTokenItem;
    procedure AddStringData(Index: Integer; Caption: String);
    procedure AddTokenToList(TokenIndex: Integer);
    function TokenMatchesSearch(Search: String; TokenIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAll;
    function AddToken(SrcToken: TToken; GroupID: Integer = -1): TToken;
    procedure DeleteToken(Item: TListItem; SelectNext: Boolean = False);
    procedure RenameToken(NewName: String; Item: TListItem);
    function GetSelectedToken: TToken;
    function GetToken(Item: TListItem): TToken;
  end;

implementation

{$R *.dfm}

procedure TFrameTokenList.AddStringData(Index: Integer; Caption: String);
const
  ERROR_MSG = 'Unknown';
var
  i: integer;
begin
  with TokenDB[Index] do
  begin
    SetLength(StringData, ListViewTokens.Columns.Count);
    StringData[0] := Caption;

    for i := 1 to ListViewTokens.Columns.Count - 1 do
      StringData[i] := ERROR_MSG;

    with Token.Access do
      if IsValid then
        StringData[2] := AccessToString(Value);

    if Token.IsValidToken then
    begin
      with Token.TokenTypeInfo do
        if IsValid then
          StringData[1] := Value.ToString;

      with Token.User do
        if IsValid then
          StringData[3] := Value.ToString;

      with Token.TryGetSession  do
        if IsValid then
          StringData[4] := Value.ToString;

      with Token.Elevation do
        if IsValid then
          StringData[5] := Value.ToString;

      with Token.TryGetIntegrity do
        if IsValid then
          StringData[6] := Value.ToString;
    end;
  end;
end;

function TFrameTokenList.AddToken(SrcToken: TToken; GroupID: Integer = -1): TToken;
begin
  // TODO: Need a EventHandler's feedback with StringData
  Result := SrcToken;
  SetLength(TokenDB, Length(TokenDB) + 1);
  TokenDB[High(TokenDB)] := TTokenItem.Create;
  with TokenDB[High(TokenDB)] do
  begin
    Token := SrcToken;
    OriginalIndex := High(TokenDB);
    GroupIndex := GroupID;
    AddStringData(High(TokenDB), Token.Caption);
  end;

  // Show the newly created item regardless of the search filter
  AddTokenToList(High(TokenDB));
end;

procedure TFrameTokenList.AddTokenToList(TokenIndex: Integer);
var
  i: integer;
begin
  with TokenDB[TokenIndex], ListViewTokens.Items.Add do
  begin
    Caption := StringData[0];
    Data := TokenDB[TokenIndex];
    GroupID := GroupIndex;
    for i := 1 to High(StringData) do
      SubItems.Add(StringData[i]);
  end;
end;

procedure TFrameTokenList.ClearAll;
var
  i: integer;
begin
  ListViewTokens.Items.Clear;
  ListViewTokens.Groups.Clear;
  for i := 0 to High(TokenDB) do
    TokenDB[i].Token.Free;

  SetLength(TokenDB, 0);
end;

constructor TFrameTokenList.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  for i := 0 to ListViewTokens.Columns.Count - 1 do
    ComboBoxColumn.Items.Add(ListViewTokens.Columns[i].Caption);
end;

procedure TFrameTokenList.DeleteToken(Item: TListItem;
  SelectNext: Boolean = False);
var
  OriginalIndex, i: integer;
begin
  OriginalIndex := TTokenItem(Item.Data).OriginalIndex;
  TokenDB[OriginalIndex].Free;

  for i := OriginalIndex + 1  to High(TokenDB) do
  begin
    TokenDB[i - 1] := TokenDB[i];
    TokenDB[i - 1].OriginalIndex := i - 1;
  end;

  SetLength(TokenDB, Length(TokenDB) - 1);
  Item.Delete;

  if SelectNext and (OriginalIndex < ListViewTokens.Items.Count) then
    ListViewTokens.Items[OriginalIndex].Selected := True;
end;

destructor TFrameTokenList.Destroy;
var
  i: integer;
begin
  for i := 0 to High(TokenDB) do
    TokenDB[i].Free;

  inherited;
end;

function TFrameTokenList.GetSelectedToken: TToken;
begin
  Result := TTokenItem(ListViewTokens.Selected.Data).Token;
end;

function TFrameTokenList.GetToken(Item: TListItem): TToken;
begin
  Result := TTokenItem(Item.Data).Token;
end;

procedure TFrameTokenList.RenameToken(NewName: String; Item: TListItem);
var
  OriginalIndex: Integer;
begin
  OriginalIndex := TTokenItem(Item.Data).OriginalIndex;
  TokenDB[OriginalIndex].Token.Caption := NewName;
  Item.Caption := NewName;
end;

procedure TFrameTokenList.SearchBoxChange(Sender: TObject);
var
  i: integer;
  SearchKey: String;
begin
  SearchBox.RightButton.Visible := SearchBox.Text <> '';
  SearchKey := LowerCase(SearchBox.Text);

  ListViewTokens.Items.BeginUpdate;
  ListViewTokens.Items.Clear;

  for i := 0 to High(TokenDB) do
    if TokenMatchesSearch(SearchKey, i) then
      AddTokenToList(i);

  if ListViewTokens.Items.Count > 0 then
    ListViewTokens.Items[0].Selected := True;

  ListViewTokens.Items.EndUpdate;
end;

procedure TFrameTokenList.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

function TFrameTokenList.TokenMatchesSearch(Search: String;
  TokenIndex: Integer): Boolean;
var
  i: integer;
begin
  // TODO: Inverse search (not something)

  if Search = '' then
    Exit(True);

  with TokenDB[TokenIndex] do
  if ComboBoxColumn.ItemIndex = 0 then // searching in all columns
  begin
    Result := False;
    for i := 0 to High(StringData) do
      if LowerCase(StringData[i]).Contains(Search) then
      begin
        Result := True;
        Break;
      end;
  end
  else
    Result := LowerCase(StringData[ComboBoxColumn.ItemIndex - 1])
      .Contains(Search);
end;

{ TTokenItem }

destructor TTokenItem.Destroy;
begin
  Token.Free;
  inherited;
end;

end.
