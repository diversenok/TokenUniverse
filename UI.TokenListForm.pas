unit UI.TokenListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, TU.TokenUtils,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Menus;

type
  TFormMain = class(TForm)
    TokenListView: TListView;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    PopupMenuItem: TPopupMenu;
    ActionDuplicate: TMenuItem;
    ActionClose: TMenuItem;
    PopupMenuNothing: TPopupMenu;
    Otherprocess1: TMenuItem;
    Createnewtoken2: TMenuItem;
    CreatefromSaferAPI1: TMenuItem;
    LogonUser1: TMenuItem;
    Fromotherprocess1: TMenuItem;
    Searchforhandles1: TMenuItem;
    hread1: TMenuItem;
    WTSQueryUserToken1: TMenuItem;
    New1: TMenuItem;
    Opencurrentprocess2: TMenuItem;
    N1: TMenuItem;
    N3: TMenuItem;
    function AddToken(Token: TToken): TToken;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TokenListViewEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure TokenListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ActionDuplicateClick(Sender: TObject);
    procedure ActionCloseClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  TU.Common, UI.Information, UI.Duplicate;

{$R *.dfm}

{ TForm1 }

procedure TFormMain.ActionCloseClick(Sender: TObject);
begin
  if TokenListView.Selected.Data = nil then Exit;
  with TokenListView.Selected do
  begin
    TToken(Data).Free;
    Delete;
  end;
end;

procedure TFormMain.ActionDuplicateClick(Sender: TObject);
begin
  if TokenListView.Selected.Data = nil then Exit;
  AddToken(TDuplicateDialog.Execute(Self, TokenListView.Selected.Data));
end;

function TFormMain.AddToken(Token: TToken): TToken;
const
  ERROR_MSG = 'Unknown';
begin
  Result := Token;
  with TokenListView.Items.Add do
  begin
    Data := Token;
    Caption := Token.Caption;

    try SubItems.Add(Token.TokenTypeAndImpersonation.ToString);
    except on E: EOSError do SubItems.Add(ERROR_MSG); end;

    try SubItems.Add(TTokenAccess.ToString(Token.Access));
    except on E: EOSError do SubItems.Add(ERROR_MSG); end;

    try SubItems.Add(Token.User.ToString);
    except on E: EOSError do SubItems.Add(ERROR_MSG); end;

    try SubItems.Add(Token.Session.ToString);
    except on E: EOSError do SubItems.Add(ERROR_MSG); end;

    try SubItems.Add(Token.Elevation.ToString);
    except on E: EOSError do SubItems.Add(ERROR_MSG); end;

    try SubItems.Add(Token.Integrity.ToString);
    except on E: EOSError do SubItems.Add(ERROR_MSG); end;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Item: TListItem;
begin
  for Item in TokenListView.Items do
    TToken(Item.Data).Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Token: TToken;
begin
  try
    with AddToken(TToken.CreateFromCurrent) do
      if Elevation <> TokenElevationTypeDefault then
        AddToken(LinkedToken);
  except
    on E: EOSError do;
  end;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.TokenListViewEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  TToken(Item.Data).Caption := S;
end;

procedure TFormMain.TokenListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    TokenListView.PopupMenu := PopupMenuItem
  else
    TokenListView.PopupMenu := PopupMenuNothing;
end;

end.
