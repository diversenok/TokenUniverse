unit UI.HandleSearch;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls,
  UI.TokenListFrame;

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
    procedure FrameListViewTokensContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHandleSearch: TFormHandleSearch;

implementation

uses
  System.UITypes, TU.Handles, TU.Processes, TU.Tokens, UI.MainForm;

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
      try FormMain.Frame.AddToken(TToken.CreateDuplicateHandle(
        Frame.GetToken(Frame.ListViewTokens.Items[i]), 0, True));
      except
        on E: EOSError do
          Inc(Failed);
      end;
    end;

  if Failed = 0 then
    MessageDlg(Format('Successfully copied all %d handles.', [Total]),
      mtInformation, [mbOK], 0)
  else
    MessageDlg(Format('Failed to copy %d of %d handles.', [Failed, Total]),
      mtWarning, [mbOK], 0);
end;

procedure TFormHandleSearch.ButtonRefreshClick(Sender: TObject);
var
  HandleSnapshot: THandleList;
  ProcessSnapshot: TProcessList;
  PID: NativeUInt;
  hProcess: THandle;
  HandleItem: THandleItem;
begin
  Frame.ClearAll;
  HandleSnapshot := THandleList.Create;
  ProcessSnapshot := TProcessList.Create;
  Frame.ListViewTokens.Groups.BeginUpdate;
  Frame.ListViewTokens.Items.BeginUpdate;

  for PID in HandleSnapshot.Processes do
    if PID <> GetCurrentProcessId then
    with Frame.ListViewTokens.Groups.Add do
    begin
      State := [lgsCollapsible];
      Header := Format('%s (%d)', [ProcessSnapshot.FindName(PID), PID]);

      hProcess := OpenProcess(PROCESS_DUP_HANDLE, False, PID);
      if hProcess <> 0 then // TODO: Or setting
      begin
        for HandleItem in HandleSnapshot.ProcessHandles[PID] do
          Frame.AddToken(TToken.CreateFromHandleItem(HandleItem, hProcess),
            GroupID);
        CloseHandle(hProcess);
      end;
    end;

  // TODO: Calculate handles and processes correctly
  LabelStatistics.Caption := Format('Found %d opened handles in %d processes',
    [Frame.ListViewTokens.Items.Count, Frame.ListViewTokens.Groups.Count]);

  Frame.ListViewTokens.Groups.EndUpdate;
  Frame.ListViewTokens.Items.EndUpdate;
  HandleSnapshot.Free;
  ProcessSnapshot.Free;
end;

procedure TFormHandleSearch.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormHandleSearch.FrameListViewTokensContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := Frame.ListViewTokens.SelCount = 0;
end;

end.
