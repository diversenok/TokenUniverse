unit UI.MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ImgList, Vcl.AppEvnts,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs, System.ImageList,
  UI.ListViewEx, UI.Prototypes, TU.Common;

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
    RunAsSystemPlus: TMenuItem;
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
    MenuPromptHandleClose: TMenuItem;
    Showiconsinprocesslist1: TMenuItem;
    TokenImpersonate: TMenuItem;
    Displayallsearchresults1: TMenuItem;
    TokenOpenLinked: TMenuItem;
    TokenOpenInfo: TMenuItem;
    SmallIcons: TImageList;
    ApplicationEvents: TApplicationEvents;
    N1: TMenuItem;
    MenuExit: TMenuItem;
    SelectColumns: TMenuItem;
    AssignToProcess: TMenuItem;
    MenuCloseCreationDlg: TMenuItem;
    ListViewTokens: TListViewEx;
    SearchButtons: TImageList;
    SearchBox: TButtonedEdit;
    ComboBoxColumn: TComboBox;
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
    procedure RunAsSystemClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ActionSteal(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionWTSQuery(Sender: TObject);
    procedure ActionRestrict(Sender: TObject);
    procedure ActionLogon(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure NewNtCreateTokenClick(Sender: TObject);
    procedure SelectColumnsClick(Sender: TObject);
    procedure AssignToProcessClick(Sender: TObject);
    procedure RunAsSystemPlusClick(Sender: TObject);
    procedure MenuCloseCreationDlgClick(Sender: TObject);
    procedure MenuPromptHandleCloseClick(Sender: TObject);
    procedure FrameListViewTokensEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FrameListViewTokensEditingEnd(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
    procedure ListViewTokensEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure SearchBoxRightButtonClick(Sender: TObject);
  public
    TokenView: TTokenViewSource;
    OnMainFormClose: TNotifyEventHandler;
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UITypes,
  TU.Handles, TU.RestartSvc, TU.Suggestions, TU.WtsApi, TU.Tokens,
  UI.Information, UI.ProcessList, UI.Run, UI.HandleSearch, UI.Modal.ComboDlg,
  UI.Restrict, UI.CreateToken, UI.Modal.Columns, UI.Modal.Access,
  UI.Modal.Logon, UI.Modal.AccessAndType, UI.Modal.PickUser, UI.Settings;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.ActionClose(Sender: TObject);
begin
  if TSettings.PromptOnHandleClose then
    if MessageDlg('Are you sure you want to close this handle?', mtConfirmation,
      mbYesNoCancel, 0) <> IDYES then
        Abort;

  TokenView.Delete(ListViewTokens.Selected.Index);
end;

procedure TFormMain.ActionDuplicate(Sender: TObject);
begin
  TokenView.Add(TDialogAccessAndType.ExecuteDuplication(Self,
    TokenView.Selected));
end;

procedure TFormMain.ActionDuplicateHandle(Sender: TObject);
begin
  TokenView.Add(TDialogAccess.ExecuteDuplication(Self, TokenView.Selected));
end;

procedure TFormMain.ActionLogon(Sender: TObject);
begin
  TLogonDialog.Create(Self).ShowModal;
end;

procedure TFormMain.ActionOpen(Sender: TObject);
begin
  TInfoDialog.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionOpenLinked(Sender: TObject);
begin
  TokenView.Add(TokenView.Selected.OpenLinkedToken.GetValueOrRaise);
end;

procedure TFormMain.ActionOpenProcess(Sender: TObject);
var
  ImageName: String;
begin
  TokenView.Add(TToken.CreateOpenProcess(
    TProcessListDialog.Execute(Self, ImageName), ImageName));
end;

procedure TFormMain.ActionOpenSelf(Sender: TObject);
begin
  TokenView.Add(TToken.CreateOpenCurrent);
end;

procedure TFormMain.ActionRename(Sender: TObject);
begin
  if Assigned(ListViewTokens.Selected) then
    ListViewTokens.Selected.EditCaption;
end;

procedure TFormMain.ActionRestrict(Sender: TObject);
begin
  TDialogRestrictToken.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionRunWithToken(Sender: TObject);
begin
  TRunDialog.Execute(Self, TokenView.Selected);
end;

procedure TFormMain.ActionSearch(Sender: TObject);
begin
  TFormHandleSearch.Create(Self).Show;
end;

procedure TFormMain.ActionSendHandle(Sender: TObject);
var
  NewHandle: NativeUInt;
begin
  NewHandle := TokenView.Selected.SendHandleToProcess(
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
  TokenView.Add(TToken.CreateQueryWts(TComboDialog.PickSession(Self)));
end;

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  ShowErrorSuggestions(E);
end;

procedure TFormMain.AssignToProcessClick(Sender: TObject);
begin
  TokenView.Selected.AssignToProcess(TProcessListDialog.Execute(Self));

  MessageDlg('The token was successfully assigned to the process.',
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OnMainFormClose.Invoke(Self);
  TokenView.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  TokenView := TTokenViewSource.Create(ListViewTokens);

  with THandleList.CreateOnly(GetCurrentProcessId) do
  begin
    for i := 0 to Count - 1 do
      TokenView.Add(TToken.CreateByHandle(Handles[i]));
    Free;
  end;

  with TokenView.Add(TToken.CreateOpenCurrent) do
    if InfoClass.Query(tdTokenElevation) and
      (InfoClass.Elevation <> TokenElevationTypeDefault) then
      with OpenLinkedToken do
        if IsValid then
          TokenView.Add(Value);

  SetForegroundWindow(Handle);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F3 then
    SearchBox.SetFocus;

  if Key = VK_ESCAPE then
    SearchBox.Text := '';
end;

procedure TFormMain.FrameListViewTokensEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  // Disable the shortcut while editing item caption so that pressing <Enter>
  // would not open the information window.
  TokenOpenInfo.ShortCut := 0;
end;

procedure TFormMain.FrameListViewTokensEditingEnd(Sender: TObject);
begin
  // Editing is over. Restrore the shortcut back so that pressing <Enter>
  // would open the information window.
  TokenOpenInfo.ShortCut := VK_RETURN;
end;

procedure TFormMain.RunAsAdminClick(Sender: TObject);
begin
  ReSvcDelegate(Handle, False, False);
  Close;
end;

procedure TFormMain.RunAsSystemClick(Sender: TObject);
begin
  try
    ReSvcCreateService(False);
  except
    on E: EOSError do
      if E.ErrorCode = ERROR_ACCESS_DENIED then
        ReSvcDelegate(Handle, True, False);
      else
        raise;
  end;
  Close;
end;

procedure TFormMain.RunAsSystemPlusClick(Sender: TObject);
begin
  try
    ReSvcCreateService(True);
  except
    on E: EOSError do
      if E.ErrorCode = ERROR_ACCESS_DENIED then
        ReSvcDelegate(Handle, True, True);
      else
        raise;
  end;
  Close;
end;

procedure TFormMain.SearchBoxChange(Sender: TObject);
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

procedure TFormMain.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

procedure TFormMain.SelectColumnsClick(Sender: TObject);
begin
  TDialogColumns.Create(Self).ShowModal;
end;

procedure TFormMain.ListViewTokensEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  TokenView.Selected.Caption := S;
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

procedure TFormMain.MenuCloseCreationDlgClick(Sender: TObject);
begin
  TSettings.NoCloseCreationDialogs := not TSettings.NoCloseCreationDialogs;
  MenuCloseCreationDlg.Checked := TSettings.NoCloseCreationDialogs;
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuPromptHandleCloseClick(Sender: TObject);
begin
  TSettings.PromptOnHandleClose := not TSettings.PromptOnHandleClose;
  MenuPromptHandleClose.Checked := TSettings.PromptOnHandleClose;
end;

procedure TFormMain.NewNtCreateTokenClick(Sender: TObject);
begin
  TDialogCreateToken.Create(Self).Show;
end;

end.
