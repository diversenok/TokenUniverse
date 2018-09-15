unit UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, TU.Common, TU.Tokens,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs, UI.TokenListFrame, System.ImageList,
  Vcl.ImgList, Vcl.AppEvnts, UI.ListViewEx;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    MainMenu: TMainMenu;
    Program1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    RunAsAdmin: TMenuItem;
    RunAsSystem: TMenuItem;
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
    TokenDuplicateHandle: TMenuItem;
    Propmtonhandleclose1: TMenuItem;
    Showiconsinprocesslist1: TMenuItem;
    Frame: TFrameTokenList;
    TokenImpersonate: TMenuItem;
    Displayallsearchresults1: TMenuItem;
    TokenOpenLinked: TMenuItem;
    TokenOpenInfo: TMenuItem;
    SmallIcons: TImageList;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure ActionDuplicate(Sender: TObject);
    procedure ActionClose(Sender: TObject);
    procedure ActionOpenProcess(Sender: TObject);
    procedure ActionRename(Sender: TObject);
    procedure ActionRunWithToken(Sender: TObject);
    procedure ListViewTokenSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ActionOpenSelf(Sender: TObject);
    procedure RunAsAdminClick(Sender: TObject);
    procedure ActionSendHandle(Sender: TObject);
    procedure ActionDuplicateHandle(Sender: TObject);
    procedure ActionSearch(Sender: TObject);
    procedure ActionOpenLinked(Sender: TObject);
    procedure ActionOpen(Sender: TObject);
    procedure FrameListViewTokensDblClick(Sender: TObject);
    procedure RunAsSystemClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ActionSteal(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FrameListViewTokensEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ActionWTSQuery(Sender: TObject);
    procedure ActionRestrict(Sender: TObject);
    procedure ActionLogon(Sender: TObject);
  public
    var OnMainFormClose: TNotifyEventHandler;
  end;

var
  FormMain: TFormMain;

implementation

uses
  Winapi.ShellApi, System.UITypes,
  TU.Handles, TU.RestartSvc, TU.Suggestions, TU.WtsApi,
  UI.Information, UI.ProcessList, UI.Run, UI.HandleSearch, UI.SessionDialog,
  UI.Restrict, UI.Modal.Logon, UI.Modal.AccessAndType;

{$R *.dfm}

{ TForm1 }

procedure TFormMain.ActionClose(Sender: TObject);
begin
  Frame.DeleteToken(Frame.ListViewTokens.Selected, True);
end;

procedure TFormMain.ActionDuplicate(Sender: TObject);
begin
  Frame.AddToken(TDialogAccessAndType.ExecuteDuplication(Self,
    Frame.GetSelectedToken));
end;

procedure TFormMain.ActionDuplicateHandle(Sender: TObject);
begin
  // TODO: An option to grab maximum access forsibly
  Frame.AddToken(TToken.CreateDuplicateHandle(Frame.GetSelectedToken, 0, True));
end;

procedure TFormMain.ActionLogon(Sender: TObject);
begin
  TLogonDialog.Create(Self).ShowModal;
end;

procedure TFormMain.ActionOpen(Sender: TObject);
begin
  TInfoDialog.CreateFromToken(Self, Frame.GetSelectedToken);
end;

procedure TFormMain.ActionOpenLinked(Sender: TObject);
begin
  Frame.AddToken(Frame.GetSelectedToken.LinkedToken.GetValueOrRaise);
end;

procedure TFormMain.ActionOpenProcess(Sender: TObject);
begin
  Frame.AddToken(TToken.CreateFromProcess(TProcessListDialog.Execute(Self)));
end;

procedure TFormMain.ActionOpenSelf(Sender: TObject);
begin
  Frame.AddToken(TToken.CreateFromCurrent);
end;

procedure TFormMain.ActionRename(Sender: TObject);
var
  NewName: string;
begin
  if InputQuery('Rename token', 'New token name: ', NewName) then
    Frame.RenameToken(NewName, Frame.ListViewTokens.Selected);
end;

procedure TFormMain.ActionRestrict(Sender: TObject);
begin
  TDialogRestrictToken.CreateFromToken(Self, Frame.GetSelectedToken);
end;

procedure TFormMain.ActionRunWithToken(Sender: TObject);
begin
  TRunDialog.Execute(Self, Frame.GetSelectedToken);
end;

procedure TFormMain.ActionSearch(Sender: TObject);
begin
  TFormHandleSearch.Create(Self).Show;
end;

procedure TFormMain.ActionSendHandle(Sender: TObject);
var
  NewHandle: NativeUInt;
begin
  NewHandle := Frame.GetSelectedToken.SendHandleToProcess(
    TProcessListDialog.Execute(Self));

  MessageDlg(Format('The handle was successfully sent.'#$D#$A +
    'It''s value is %d (0x%x)', [NewHandle, NewHandle]), mtInformation,
    [mbOK], 0);
end;

procedure TFormMain.ActionSteal(Sender: TObject);
begin
  TProcessListDialog.Execute(Self);//TODO: Copy handle from process
end;

procedure TFormMain.ActionWTSQuery(Sender: TObject);
begin
  Frame.AddToken(TToken.CreateQueryWts(TSessionDialog.Execute(Self)));
end;

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  ShowErrorSuggestions(E);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OnMainFormClose.InvokeIgnoringErrors(Self);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  with Frame.AddToken(TToken.CreateFromCurrent), Elevation do
    if IsValid then
      if Elevation.Value <> TokenElevationTypeDefault then
        with LinkedToken do
          if IsValid then
            Frame.AddToken(Value);
  SetForegroundWindow(Handle);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F3 then
    Frame.SearchBox.SetFocus;

  if Key = VK_ESCAPE then
    Frame.SearchBox.Text := '';
end;

procedure TFormMain.FrameListViewTokensDblClick(Sender: TObject);
begin
  if Frame.ListViewTokens.SelCount <> 0 then
    ActionOpen(Self);
end;

procedure TFormMain.FrameListViewTokensEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  Frame.RenameToken(S, Item as TListItemEx);
end;

procedure TFormMain.RunAsAdminClick(Sender: TObject);
begin
  ReSvcDelegate(Handle, False);
  Close;
end;

procedure TFormMain.RunAsSystemClick(Sender: TObject);
begin
  try
    ReSvcCreateService;
  except
    on E: EOSError do
      if E.ErrorCode = ERROR_ACCESS_DENIED then
        ReSvcDelegate(Handle, True);
      else
        raise;
  end;
  Close;
end;

procedure TFormMain.ListViewTokenSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Menu: TMenuItem;
begin
  for Menu in PopupMenu.Items do
    if (Menu <> NewMenu) and (Menu <> ProgramRun)  then
      Menu.Enabled := Selected;
end;

end.
