unit UI.Modal.PickToken;

interface

uses
  Winapi.Messages, System.SysUtils,  System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes.Forms, TU.Tokens;

type
  TDialogPickToken = class(TChildForm)
    ListViewTokens: TListViewEx;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    procedure ListViewTokensSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  public
    class function Execute(AOwner: TComponent): IToken;
  end;

implementation

uses
  UI.MainForm;

{$R *.dfm}

{ TDialogSelectToken }

class function TDialogPickToken.Execute(AOwner: TComponent): IToken;
var
  Tokens: TArray<IToken>;
  i: Integer;
begin
  with TDialogPickToken.Create(AOwner) do
  begin
    Tokens := FormMain.TokenView.Tokens;

    for i := 0 to High(Tokens) do
      ListViewTokens.Items.Add.Caption := Tokens[i].Caption;

    ShowModal;

    if not Assigned(ListViewTokens.Selected) then
      Abort;

    Result := Tokens[ListViewTokens.Selected.Index];
  end;
end;

procedure TDialogPickToken.ListViewTokensSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonOK.Enabled := Assigned(ListViewTokens.Selected);
end;

end.
