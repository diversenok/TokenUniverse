unit UI.HandleSearch;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls,
  UI.TokenListFrame, UI.ListViewEx;

type
  TFormHandleSearch = class(TForm)
    Frame: TFrameTokenList;
    ButtonClose: TButton;
    LabelStatistics: TLabel;
    ButtonRefresh: TButton;
    PopupMenu: TPopupMenu;
    TokenObtain: TMenuItem;
    procedure ButtonRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionObtain(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHandleSearch: TFormHandleSearch;

implementation

uses
  System.UITypes, NtUtils.Handles, NtUtils.Processes, TU.Tokens, UI.MainForm;

{$R *.dfm}

procedure TFormHandleSearch.ActionObtain(Sender: TObject);
var
  i: integer;
  Total, Failed: Integer;
begin
  Total := 0;
  Failed := 0;
  for i := 0 to Frame.ListViewTokens.Items.Count - 1 do
    if Frame.ListViewTokens.Items[i].Selected then
    begin
      Inc(Total);
{      try FormMain.Frame.AddToken(TToken.CreateDuplicateHandle(
        Frame.GetToken(Frame.ListViewTokens.Items[i]), 0, True));
      except
        on E: EOSError do
          Inc(Failed);
      end;}
    end;

  if Failed = 0 then
    MessageDlg(Format('Successfully copied all %d handles.', [Total]),
      mtInformation, [mbOK], 0)
  else
    MessageDlg(Format('Failed to copy %d of %d handles.', [Failed, Total]),
      mtWarning, [mbOK], 0);
end;

procedure TFormHandleSearch.ButtonRefreshClick(Sender: TObject);
begin
  //
end;

procedure TFormHandleSearch.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
