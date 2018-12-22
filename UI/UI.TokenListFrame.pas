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
    function AddToken(Token: TToken; Group: Integer = 0): TToken;
    procedure DeleteToken(Item: TListItemEx; SelectNext: Boolean = False);
    procedure RenameToken(NewName: String; Item: TListItemEx);
    function GetSelectedToken: TToken;
    function GetToken(Item: TListItemEx): TToken;
  end;

implementation

uses
  TU.Tokens.Types;

{$R *.dfm}

function TFrameTokenList.AddToken(Token: TToken; Group: Integer): TToken;
var
  Item: TListItemEx;
begin
  // TODO: Need EventHandler's feedback
  Result := Token;

  ListViewTokens.Items.BeginUpdate;
  Item := ListViewTokens.Items.Add;

  Item.Caption := Token.Caption;
  Item.OwnedData := Token;
  Item.GroupID := Group;

  Item.SubItems.Add(Token.InfoClass.QueryString(tsTokenType));
  Item.SubItems.Add(Token.InfoClass.QueryString(tsAccess));
  Item.SubItems.Add(Token.InfoClass.QueryString(tsUserName));
  Item.SubItems.Add(Token.InfoClass.QueryString(tsSession));
  Item.SubItems.Add(Token.InfoClass.QueryString(tsElevation));
  Item.SubItems.Add(Token.InfoClass.QueryString(tsIntegrity));

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
  if GetToken(Item).CanBeFreed then
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
var
  SearchPattern: String;
  i: Integer;
begin
  SearchPattern := String(SearchBox.Text).ToLower;

  ListViewTokens.GroupView := SearchPattern <> '';
  SearchBox.RightButton.Visible := SearchPattern <> '';

  if ListViewTokens.GroupView then
    for i := 0 to ListViewTokens.Items.Count - 1 do
      with ListViewTokens.Items[i] do
        if Matches(SearchPattern, ComboBoxColumn.ItemIndex - 1) then
          GroupID := 0
        else
          GroupID := -1;
end;

procedure TFrameTokenList.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

end.
