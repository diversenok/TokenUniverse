unit UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, TU.Tokens,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs, UI.TokenListFrame;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    MainMenu: TMainMenu;
    Program1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    RunAsAdmin: TMenuItem;
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
    TokenDuplicateHandle: TMenuItem;
    Propmtonhandleclose1: TMenuItem;
    Showiconsinprocesslist1: TMenuItem;
    Frame: TFrameTokenList;
    TokenImpersonate: TMenuItem;
    Displayallsearchresults1: TMenuItem;
    TokenOpenLinked: TMenuItem;
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
  end;

var
  FormMain: TFormMain;

implementation

uses
  Winapi.ShellApi,
  TU.Common, TU.Handles,
  UI.Information, UI.Duplicate, UI.ProcessList, UI.Run, UI.HandleSearch;

{$R *.dfm}

{ TForm1 }

procedure TFormMain.ActionClose(Sender: TObject);
begin
  Frame.DeleteToken(Frame.ListViewTokens.Selected);
end;

procedure TFormMain.ActionDuplicate(Sender: TObject);
begin
  Frame.AddToken(TDuplicateDialog.Execute(Self, Frame.GetSelectedToken));
end;

procedure TFormMain.ActionDuplicateHandle(Sender: TObject);
begin
  Frame.AddToken(TToken.CreateDuplicateHandle(Frame.GetSelectedToken, 0, True));
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
    'It''s value is %d (0x%0.6x)', [NewHandle, NewHandle]), mtInformation,
    [mbOK], 0);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  try
    with Frame.AddToken(TToken.CreateFromCurrent), Elevation do
      if IsValid then
        if Elevation.Value <> TokenElevationTypeDefault then
          Frame.AddToken(LinkedToken);
  except
    on E: EOSError do;
  end;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.RunAsAdminClick(Sender: TObject);
var
  ExecInfo: TShellExecuteInfoW;
begin
  FillChar(ExecInfo, SizeOf(ExecInfo), 0);
  with ExecInfo do
  begin
    cbSize := SizeOf(ExecInfo);
    Wnd := Handle;
    lpVerb := PWideChar('runas');
    lpFile := PWideChar(ParamStr(0));
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_UNICODE or SEE_MASK_FLAG_NO_UI;
    nShow := SW_SHOWNORMAL;
  end;
  if not ShellExecuteExW(@ExecInfo) then
    RaiseLastOSError
  else
    Close;
end;

procedure TFormMain.ListViewTokenSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  TokenDuplicate.Enabled := Selected;
  TokenDuplicateHandle.Enabled := Selected;
  TokenRename.Enabled := Selected;
  TokenClose.Enabled := Selected;
  TokenSendHandle.Enabled := Selected;
  TokenRun.Enabled := Selected;
end;

end.
