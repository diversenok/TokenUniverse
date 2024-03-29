unit UI.TokenListFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.ComCtrls,
  TU.Tokens, VclEx.ListView;

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
    function AddToken(const Token: IToken; Group: Integer = 0): IToken;
    procedure DeleteToken(Item: TListItemEx; SelectNext: Boolean = False);
    procedure RenameToken(NewName: String; Item: TListItemEx);
    function GetSelectedToken: IToken;
    function GetToken(Item: TListItemEx): IToken;
  end;

implementation

uses
  TU.Tokens.Old.Types;

{$R *.dfm}

function TFrameTokenList.AddToken;
var
  Item: TListItemEx;
begin
  // TODO: Need EventHandler's feedback
  Result := Token;

  ListViewTokens.Items.BeginUpdate;
  Item := ListViewTokens.Items.Add;

  Item.Caption := Token.Caption;
  Item.OwnedIData := Token;
  Item.GroupID := Group;
  Item.Data := Pointer(Group);

  Item.SubItems.Add(Token.QueryString(tsType));
  Item.SubItems.Add(Token.QueryString(tsAccess));
  Item.SubItems.Add(Token.QueryString(tsUser));
  Item.SubItems.Add(Token.QueryString(tsSessionId));
  Item.SubItems.Add(Token.QueryString(tsElevation));
  Item.SubItems.Add(Token.QueryString(tsIntegrity));

  ListViewTokens.Items.EndUpdate;
end;

procedure TFrameTokenList.ClearAll;
begin
  ListViewTokens.Items.Clear;
  ListViewTokens.Groups.Clear;
  SearchBox.Text := '';
end;

constructor TFrameTokenList.Create;
var
  i: integer;
begin
  inherited;
  for i := 0 to ListViewTokens.Columns.Count - 1 do
    ComboBoxColumn.Items.Add(ListViewTokens.Columns[i].Caption);
end;

procedure TFrameTokenList.DeleteToken;
begin
  Item.Delete;

  {if SelectNext and (OriginalIndex < ListViewTokens.Items.Count) then
    ListViewTokens.Items[OriginalIndex].Selected := True;}
end;

function TFrameTokenList.GetSelectedToken;
begin
  if Assigned(ListViewTokens.Selected) then
    Result := IToken(ListViewTokens.Selected.OwnedIData)
  else
    Result := nil;
end;

function TFrameTokenList.GetToken;
begin
  Result := IToken(Item.OwnedIData);
end;

procedure TFrameTokenList.RenameToken;
begin
  IToken(Item.OwnedIData).Caption := NewName;
  Item.Caption := NewName; // TODO: move to OnCaptionChange
end;

procedure TFrameTokenList.SearchBoxChange;
var
  SearchPattern: String;
  i: Integer;
begin
  SearchPattern := String(SearchBox.Text).ToLower;

  SearchBox.RightButton.Visible := SearchPattern <> '';

  for i := 0 to ListViewTokens.Items.Count - 1 do
    with ListViewTokens.Items[i] do
      if SearchPattern = '' then
        GroupID := Integer(ListViewTokens.Items[i].Data)
      else if Matches(SearchPattern, ComboBoxColumn.ItemIndex - 1) then
        GroupID := 0
      else
        GroupID := -1;
end;

procedure TFrameTokenList.SearchBoxRightButtonClick;
begin
  SearchBox.Text := '';
end;

end.
