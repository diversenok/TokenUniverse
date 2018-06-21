unit UI.TokenListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, TU.TokenUtils,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs;

type
  TFormMain = class(TForm)
    TokenListView: TListView;
    Panel1: TPanel;
    Button1: TButton;
    MainMenu: TMainMenu;
    Program1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    RunasAdministrator1: TMenuItem;
    RunasSYSTEM1: TMenuItem;
    RunasSYSTEM2: TMenuItem;
    PopupMenu: TPopupMenu;
    TokenDuplicate: TMenuItem;
    TokenRestrict: TMenuItem;
    TokenRename: TMenuItem;
    TokenClose: TMenuItem;
    HLine1: TMenuItem;
    TokenRun: TMenuItem;
    TokenSendHandle: TMenuItem;
    NewMenu: TMenuItem;
    NewOpenSelf: TMenuItem;
    NewOpenProcess: TMenuItem;
    NewOpenThread: TMenuItem;
    HLine3: TMenuItem;
    NewLogonUser: TMenuItem;
    NewQueryUserToken: TMenuItem;
    NewSaferApi: TMenuItem;
    NewNtCreateToken: TMenuItem;
    HLine4: TMenuItem;
    NewCopyHandle: TMenuItem;
    NewSearchHandle: TMenuItem;
    ProgramRun: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    HLine2: TMenuItem;
    RunTaskAsInteractiveUser1: TMenuItem;
    function AddToken(Token: TToken): TToken;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TokenListViewEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ActionDuplicate(Sender: TObject);
    procedure ActionClose(Sender: TObject);
    procedure ActionOpenProcess(Sender: TObject);
    procedure ActionRename(Sender: TObject);
    procedure ActionRunWithToken(Sender: TObject);
    procedure TokenListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ActionOpenSelf(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  TU.Common, UI.Information, UI.Duplicate, UI.ProcessList, UI.Run;

{$R *.dfm}

{ TForm1 }

procedure TFormMain.ActionClose(Sender: TObject);
begin
  with TokenListView.Selected do
  begin
    TToken(Data).Free;
    Delete;
  end;
end;

procedure TFormMain.ActionDuplicate(Sender: TObject);
begin
  AddToken(TDuplicateDialog.Execute(Self, TokenListView.Selected.Data));
end;

procedure TFormMain.ActionOpenProcess(Sender: TObject);
begin
  AddToken(TToken.CreateFromProcess(TProcessListDialog.Execute(Self)));
end;

procedure TFormMain.ActionOpenSelf(Sender: TObject);
begin
  AddToken(TToken.CreateFromCurrent);
end;

procedure TFormMain.ActionRename(Sender: TObject);
var
  NewName: string;
begin
  if InputQuery('Rename token', 'New token name: ', NewName) then
  begin
    TokenListView.Selected.Caption := NewName;
    TokenListViewEdited(Sender, TokenListView.Selected, NewName);
  end;
end;

procedure TFormMain.ActionRunWithToken(Sender: TObject);
begin
  TRunDialog.Execute(Self, TokenListView.Selected.Data);
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
  TokenDuplicate.Enabled := Selected;
  TokenRename.Enabled := Selected;
  TokenClose.Enabled := Selected;
  TokenRun.Enabled := Selected;
end;

end.
