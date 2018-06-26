unit UI.TokenListFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.ComCtrls, TU.Tokens;

type
  TTokenItem = class
    Token: TToken;
    StringData: array of String;
    OriginalIndex: Integer;
    destructor Destroy; override;
  end;

  TFrameTokenList = class(TFrame)
    ListViewTokens: TListView;
    SearchButtons: TImageList;
    SearchBox: TButtonedEdit;
    ComboBoxColumn: TComboBox;
    procedure SearchBoxRightButtonClick(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
  private
    TokenDB: array of TTokenItem;
    procedure AddTokenToList(TokenIndex: Integer);
    function TokenMatchesSearch(Search: String; TokenIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddToken(SrcToken: TToken): TToken;
    procedure DeleteToken(Item: TListItem);
    procedure RenameToken(NewName: String; Item: TListItem);
    function GetToken(Item: TListItem): TToken;
    function GetSelectedToken: TToken;
  end;

implementation

{$R *.dfm}

function TFrameTokenList.AddToken(SrcToken: TToken): TToken;
const
  ERROR_MSG = 'Unknown';
begin
  Result := SrcToken;
  SetLength(TokenDB, Length(TokenDB) + 1);
  TokenDB[High(TokenDB)] := TTokenItem.Create;
  with TokenDB[High(TokenDB)] do
  begin
    OriginalIndex := High(TokenDB);
    Token := SrcToken;

    SetLength(StringData, ListViewTokens.Columns.Count);
    StringData[0] := Token.Caption;

    try StringData[1] := Token.TokenTypeAndImpersonation.ToString;
    except on E: EOSError do StringData[1] := ERROR_MSG; end;

    try StringData[2] := AccessToString(Token.Access);
    except on E: EOSError do StringData[2] := ERROR_MSG; end;

    try StringData[3] := Token.User.ToString;
    except on E: EOSError do StringData[3] := ERROR_MSG; end;

    try StringData[4] := Token.Session.ToString;
    except on E: EOSError do StringData[4] := ERROR_MSG; end;

    try StringData[5] := Token.Elevation.ToString;
    except on E: EOSError do StringData[5] := ERROR_MSG; end;

    try StringData[6] := Token.Integrity.ToString;
    except on E: EOSError do StringData[6] := ERROR_MSG; end;
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
    for i := 1 to High(StringData) do
      SubItems.Add(StringData[i]);
  end;
end;

constructor TFrameTokenList.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  for i := 0 to ListViewTokens.Columns.Count - 1 do
    ComboBoxColumn.Items.Add(ListViewTokens.Columns[i].Caption);
end;

procedure TFrameTokenList.DeleteToken(Item: TListItem);
var
  OriginalIndex, i: integer;
begin
  OriginalIndex := TTokenItem(Item.Data).OriginalIndex;
  TokenDB[OriginalIndex].Free;

  for i := OriginalIndex + 1  to High(TokenDB) do
    TokenDB[i - 1] := TokenDB[i];

  SetLength(TokenDB, Length(TokenDB) - 1);
  Item.Delete;
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
