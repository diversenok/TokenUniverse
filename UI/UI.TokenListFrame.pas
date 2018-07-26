unit UI.TokenListFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.ComCtrls,
  TU.Tokens, TU.Handles, UI.ListViewEx;

type
  TFrameTokenList = class(TFrame)
    ListViewTokens: TListViewEx;
    SearchButtons: TImageList;
    SearchBox: TButtonedEdit;
    ComboBoxColumn: TComboBox;
    procedure SearchBoxRightButtonClick(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ClearAll;
    function AddToken(Token: TToken; Group: Integer = -1): TToken;
    procedure DeleteToken(Item: TListItemEx; SelectNext: Boolean = False);
    procedure RenameToken(NewName: String; Item: TListItemEx);
    function GetSelectedToken: TToken;
    function GetToken(Item: TListItemEx): TToken;
  end;

implementation

{$R *.dfm}

function TFrameTokenList.AddToken(Token: TToken; Group: Integer = -1): TToken;
const
  ERROR_MSG = 'Unknown';
var
  Item: TListItemEx;
  i: Integer;
begin
  // TODO: Need EventHandler's feedback
  Result := Token;

  ListViewTokens.Items.BeginUpdate;
  Item := ListViewTokens.Items.Add;

  Item.Caption := Token.Caption;
  Item.OwnedData := Token;
  Item.GroupID := Group;
  for i := 1 to ListViewTokens.Columns.Count - 1 do
    Item.SubItems.Add(ERROR_MSG);

  with Token.Access do
      if IsValid then
        Item.SubItems[1] := AccessToString(Value);

  if Token.IsValidToken then
  begin
    with Token.TokenTypeInfo do
      if IsValid then
        Item.SubItems[0] := Value.ToString;

    with Token.User do
      if IsValid then
        Item.SubItems[2] := Value.ToString;

    with Token.TryGetSession  do
      if IsValid then
        Item.SubItems[3] := Value.ToString;

    with Token.Elevation do
      if IsValid then
        Item.SubItems[4] := Value.ToString;

    with Token.TryGetIntegrity do
      if IsValid then
        Item.SubItems[5] := Value.ToString;
  end;

  ListViewTokens.Items.EndUpdate;
end;

procedure TFrameTokenList.ClearAll;
begin
  ListViewTokens.Items.Clear;
  ListViewTokens.Groups.Clear;
end;

constructor TFrameTokenList.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  for i := 0 to ListViewTokens.Columns.Count - 1 do
    ComboBoxColumn.Items.Add(ListViewTokens.Columns[i].Caption);
end;

procedure TFrameTokenList.DeleteToken(Item: TListItemEx;
  SelectNext: Boolean = False);
begin
  Item.Delete;

  {if SelectNext and (OriginalIndex < ListViewTokens.Items.Count) then
    ListViewTokens.Items[OriginalIndex].Selected := True;}
end;

function TFrameTokenList.GetSelectedToken: TToken;
begin
  Result := ListViewTokens.Selected.OwnedData as TToken;
end;

function TFrameTokenList.GetToken(Item: TListItemEx): TToken;
begin
  Result := TToken(Item.OwnedData);
end;

procedure TFrameTokenList.RenameToken(NewName: String; Item: TListItemEx);
begin
  TToken(Item.OwnedData).Caption := NewName;
  Item.Caption := NewName; // TODO: move to OnCaptionChange
end;

procedure TFrameTokenList.SearchBoxChange(Sender: TObject);
begin
  ListViewTokens.Filter(SearchBox.Text, ComboBoxColumn.ItemIndex - 1);
end;

procedure TFrameTokenList.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

end.
