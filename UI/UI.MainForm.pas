unit UI.MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ImgList, Vcl.AppEvnts,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs, System.ImageList,
  VclEx.ListView, UI.Prototypes, VclEx.Form;

type
  TFormMain = class(TFormEx)
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
    NewNtCreateToken: TMenuItem;
    HLine4: TMenuItem;
    NewCopyHandle: TMenuItem;
    NewSearchHandle: TMenuItem;
    TokenDuplicateHandle: TMenuItem;
    MenuPromptHandleClose: TMenuItem;
    Showiconsinprocesslist1: TMenuItem;
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
    AssignToThread: TMenuItem;
    N2: TMenuItem;
    RevertThread: TMenuItem;
    N3: TMenuItem;
    NewAnonymous: TMenuItem;
    TokenRestrictSafer: TMenuItem;
    NewOpenEffective: TMenuItem;
    MenuSafeImpersonation: TMenuItem;
    MenuTools: TMenuItem;
    MenuSystemAudit: TMenuItem;
    MenuRunProgram: TMenuItem;
    TimerStateCheck: TTimer;
    RevertCurrentThread: TMenuItem;
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
    procedure MenuCloseCreationDlgClick(Sender: TObject);
    procedure MenuPromptHandleCloseClick(Sender: TObject);
    procedure FrameListViewTokensEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FrameListViewTokensEditingEnd(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
    procedure ListViewTokensEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure SearchBoxRightButtonClick(Sender: TObject);
    procedure ActionOpenThread(Sender: TObject);
    procedure ActionRevertThread(Sender: TObject);
    procedure ActionAssignToProcess(Sender: TObject);
    procedure ActionAssignToThread(Sender: TObject);
    procedure NewAnonymousClick(Sender: TObject);
    procedure TokenRestrictSaferClick(Sender: TObject);
    procedure ActionOpenEffective(Sender: TObject);
    procedure MenuSafeImpersonationClick(Sender: TObject);
    procedure MenuSystemAuditClick(Sender: TObject);
    procedure MenuRunProgramClick(Sender: TObject);
    procedure CurrentUserChanged(Sender: TObject);
    procedure ActionRevertCurrentThread(Sender: TObject);
  public
    TokenView: TTokenViewSource;
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UITypes, TU.Tokens.Types, Ntapi.WinNt,
  NtUtils.Objects.Snapshots, TU.RestartSvc, TU.Suggestions, TU.Tokens,
  UI.Information, UI.ProcessList, UI.HandleSearch, UI.Modal.ComboDlg,
  UI.Restrict, UI.CreateToken, UI.Modal.Columns, UI.Modal.Access,
  UI.Modal.Logon, UI.Modal.AccessAndType, UI.Modal.PickUser, UI.Settings,
  UI.New.Safer, Ntapi.ntpsapi, UI.Audit.System, UI.Process.Run, Ntapi.ntstatus,
  DelphiUtils.Arrays, NtUiLib.Errors, Ntapi.ntseapi, NtUtils,
  NtUiLib.Exceptions.Dialog, UI.Prototypes.Forms;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.ActionAssignToProcess(Sender: TObject);
var
  Token: IToken;
begin
  Token := TokenView.Selected;

  // Ask the user if he wants to duplicate non-primary tokens
  if Token.InfoClass.Query(tdTokenType) and
    (Token.InfoClass.TokenTypeInfo <> ttPrimary) then
    case TaskMessageDlg(USE_TYPE_MISMATCH, USE_NEED_PRIMARY, mtWarning,
      [mbYes, mbIgnore, mbCancel], -1) of

      idCancel:
        Abort;

      // Duplicate existing token to a primary one and add to the list
      idYes:
        Token := TokenView.Add(TToken.CreateDuplicateToken(Token,
          MAXIMUM_ALLOWED, ttPrimary, False));

      idIgnore:
        ;
    end;

  Token.AssignToProcess(TProcessListDialog.Execute(Self, False).ProcessID);

  MessageDlg('The token was successfully assigned to the process.',
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionAssignToThread(Sender: TObject);
var
  Token: IToken;
begin
  Token := TokenView.Selected;

  // Ask the user if he wants to duplicate primary tokens
  if Token.InfoClass.Query(tdTokenType) and
    (Token.InfoClass.TokenTypeInfo = ttPrimary) then
    case TaskMessageDlg(USE_TYPE_MISMATCH, USE_NEED_IMPERSONATION, mtWarning,
      [mbYes, mbIgnore, mbCancel], -1) of

      idCancel:
        Abort;

      // Duplicate existing token to an impersonation one and add to the list
      idYes:
        Token := TokenView.Add(TToken.CreateDuplicateToken(Token,
          MAXIMUM_ALLOWED, ttImpersonation, False));

      idIgnore:
        ;
    end;

  if TSettings.UseSafeImpersonation then
    Token.AssignToThreadSafe(TProcessListDialog.Execute(Self, True).ThreadID)
  else
    Token.AssignToThread(TProcessListDialog.Execute(Self, True).ThreadID);

  CurrentUserChanged(Self);
  MessageDlg('The token was successfully assigned to the thread.',
    mtInformation, [mbOK], 0);
end;

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
  TLogonDialog.Create(Self).Show;
end;

procedure TFormMain.ActionOpen(Sender: TObject);
begin
  TInfoDialog.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionOpenEffective(Sender: TObject);
var
  Client: TClientIdEx;
begin
  Client := TProcessListDialog.Execute(Self, True);
  TokenView.Add(TToken.CreateOpenEffective(Client.ThreadID,
    Client.ImageName));
end;

procedure TFormMain.ActionOpenLinked(Sender: TObject);
var
  Linked: IToken;
begin
  TokenView.Selected.OpenLinkedToken(Linked).RaiseOnError;
  TokenView.Add(Linked);
end;

procedure TFormMain.ActionOpenProcess(Sender: TObject);
var
  Client: TClientIdEx;
begin
  Client := TProcessListDialog.Execute(Self, False);
  TokenView.Add(TToken.CreateOpenProcess(Client.ProcessID, Client.ImageName));
end;

procedure TFormMain.ActionOpenSelf(Sender: TObject);
begin
  TokenView.Add(TToken.CreateOpenCurrent);
end;

procedure TFormMain.ActionOpenThread(Sender: TObject);
var
  Client: TClientIdEx;
begin
  Client := TProcessListDialog.Execute(Self, True);
  TokenView.Add(TToken.CreateOpenThread(Client.ThreadID, Client.ImageName));
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

procedure TFormMain.ActionRevertCurrentThread(Sender: TObject);
begin
  TToken.RevertThreadToken(NtCurrentThreadId);

  CurrentUserChanged(Self);
  MessageDlg('The token was successfully revoked from the current thread.',
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionRevertThread(Sender: TObject);
var
  TID: TThreadId;
begin
  TID := TProcessListDialog.Execute(Self, True).ThreadID;
  TToken.RevertThreadToken(TID);

  if TID = NtCurrentThreadId then
    CurrentUserChanged(Self);

  MessageDlg('The token was successfully revoked from the thread.',
    mtInformation, [mbOK], 0);
end;

procedure TFormMain.ActionRunWithToken(Sender: TObject);
begin
  if Assigned(ListViewTokens.Selected) then
    with TDialogRun.Create(Self) do
    begin
      UseToken := TokenView.Selected;
      Show;
    end;
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
    TProcessListDialog.Execute(Self, False).ProcessID);

  MessageDlg(Format('The handle was successfully sent.'#$D#$A +
    'Its value is %d (0x%x)', [NewHandle, NewHandle]), mtInformation,
    [mbOK], 0);
end;

procedure TFormMain.ActionSteal(Sender: TObject);
begin
  TProcessListDialog.Execute(Self, False);//TODO: Copy handle from process
end;

procedure TFormMain.ActionWTSQuery(Sender: TObject);
begin
  TokenView.Add(TToken.CreateQueryWts(TComboDialog.PickSession(Self)));
end;

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  ShowNtxException(Application.Handle, E);
end;

procedure TFormMain.CurrentUserChanged(Sender: TObject);
begin
  if Active or (Sender <> TimerStateCheck) then
    Caption := 'Token Universe :: Main Window [' + FormatCurrentState + ']';
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TFormEvents.OnMainFormClose.Invoke(Self);
  TokenView.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Handles: TArray<TProcessHandleEntry>;
  Linked: IToken;
  i: integer;
begin
  TokenView := TTokenViewSource.Create(ListViewTokens);
  CurrentUserChanged(Self);

  // Search for inherited handles
  if NtxEnumerateHandlesProcess(NtCurrentProcess, Handles).IsSuccess then
  begin
    // TODO: obtain token's type index in runtime
    TArray.FilterInline<TProcessHandleEntry>(Handles, ByType(5));

    for i := 0 to High(Handles) do
      TokenView.Add(TToken.CreateByHandle(Handles[i].HandleValue));
  end;

  // Open current process and, maybe, its linked token
  with TokenView.Add(TToken.CreateOpenCurrent) do
    if InfoClass.Query(tdTokenElevation) and
      (InfoClass.Elevation <> TokenElevationTypeDefault) and
      OpenLinkedToken(Linked).IsSuccess then
        TokenView.Add(Linked);

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
  // Disable some shortcuts while editing item caption so that pressing <Enter>
  // or <Delete> would not open the information window or close the handle.
  TokenOpenInfo.ShortCut := 0;
  TokenClose.ShortCut := 0;
end;

procedure TFormMain.FrameListViewTokensEditingEnd(Sender: TObject);
begin
  // Editing is over. Restrore the shortcuts back.
  TokenOpenInfo.ShortCut := VK_RETURN;
  TokenClose.ShortCut := VK_DELETE;
end;

procedure TFormMain.RunAsAdminClick(Sender: TObject);
begin
  ReSvcDelegate(rmElevate);
  Close;
end;

procedure TFormMain.RunAsSystemClick(Sender: TObject);
var
  Status: TNtxStatus;
begin
  Status := ReSvcCreateService(Sender = RunAsSystemPlus);

  if Status.Status = STATUS_ACCESS_DENIED then
  begin
    if Sender = RunAsSystemPlus then
      ReSvcDelegate(rmDelegateSystemPlus)
    else
      ReSvcDelegate(rmDelegateSystem);
  end
  else
    Status.RaiseOnError;

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

procedure TFormMain.TokenRestrictSaferClick(Sender: TObject);
begin
  TDialogSafer.CreateFromToken(Self, TokenView.Selected);
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
    if (Menu <> NewMenu) and (Menu <> RevertThread) and
      (Menu <> RevertCurrentThread) then
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

procedure TFormMain.MenuRunProgramClick(Sender: TObject);
begin
  TDialogRun.Create(Self).Show;
end;

procedure TFormMain.MenuSafeImpersonationClick(Sender: TObject);
begin
  TSettings.UseSafeImpersonation := not TSettings.UseSafeImpersonation;
  MenuSafeImpersonation.Checked := TSettings.UseSafeImpersonation;
end;

procedure TFormMain.MenuSystemAuditClick(Sender: TObject);
begin
  TDialogSystemAudit.Create(Self).Show;
end;

procedure TFormMain.NewAnonymousClick(Sender: TObject);
begin
  TokenView.Add(TToken.CreateAnonymous);
end;

procedure TFormMain.NewNtCreateTokenClick(Sender: TObject);
begin
  TDialogCreateToken.Create(Self).Show;
end;

end.
