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
    function AddToken(Token: IToken3; Group: Integer = 0): IToken3;
    procedure DeleteToken(Item: TListItemEx; SelectNext: Boolean = False);
    procedure RenameToken(NewName: String; Item: TListItemEx);
    function GetSelectedToken: IToken3;
    function GetToken(Item: TListItemEx): IToken3;
  end;

implementation

uses
  TU.Tokens.Old.Types;

{$R *.dfm}

function TFrameTokenList.AddToken(Token: IToken3; Group: Integer): IToken3;
var
  Item: TListItemEx;
begin
  // TODO: Need EventHandler's feedback
  Result := Token;

  ListViewTokens.Items.BeginUpdate;
  Item := ListViewTokens.Items.Add;

  Item.Caption := (Token as IToken3).Caption;
  Item.OwnedIData := Token;
  Item.GroupID := Group;
  Item.Data := Pointer(Group);

  Item.SubItems.Add((Token as IToken3).QueryString(tsType));
  Item.SubItems.Add((Token as IToken3).QueryString(tsAccess));
  Item.SubItems.Add((Token as IToken3).QueryString(tsUser));
  Item.SubItems.Add((Token as IToken3).QueryString(tsSessionId));
  Item.SubItems.Add((Token as IToken3).QueryString(tsElevation));
  Item.SubItems.Add((Token as IToken3).QueryString(tsIntegrity));

  ListViewTokens.Items.EndUpdate;
end;

procedure TFrameTokenList.ClearAll;
begin
  ListViewTokens.Items.Clear;
  ListViewTokens.Groups.Clear;
  SearchBox.Text := '';
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

function TFrameTokenList.GetSelectedToken: IToken3;
begin
  if Assigned(ListViewTokens.Selected) then
    Result := IToken3(ListViewTokens.Selected.OwnedIData)
  else
    Result := nil;
end;

function TFrameTokenList.GetToken(Item: TListItemEx): IToken3;
begin
  Result := IToken3(Item.OwnedIData);
end;

procedure TFrameTokenList.RenameToken(NewName: String; Item: TListItemEx);
begin
  IToken3(Item.OwnedIData).Caption := NewName;
  Item.Caption := NewName; // TODO: move to OnCaptionChange
end;

procedure TFrameTokenList.SearchBoxChange(Sender: TObject);
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

procedure TFrameTokenList.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

end.
