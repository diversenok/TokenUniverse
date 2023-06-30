unit UI.MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs, System.ImageList,
  VclEx.ListView, UI.Prototypes, VclEx.Form, UI.New.TokenFrame,
  VirtualTrees;

type
  TFormMain = class(TFormEx)
    MainMenu: TMainMenu;
    Program1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    RunAsAdmin: TMenuItem;
    RunAsSystem: TMenuItem;
    RunAsSystemPlus: TMenuItem;
    TokenMenu: TPopupMenu;
    TokenDuplicate: TMenuItem;
    TokenRestrict: TMenuItem;
    TokenRename: TMenuItem;
    TokenClose: TMenuItem;
    HLine1: TMenuItem;
    TokenRun: TMenuItem;
    TokenSendHandle: TMenuItem;
    TokenDuplicateHandle: TMenuItem;
    MenuPromptHandleClose: TMenuItem;
    Showiconsinprocesslist1: TMenuItem;
    Displayallsearchresults1: TMenuItem;
    TokenOpenLinked: TMenuItem;
    SmallIcons: TImageList;
    N1: TMenuItem;
    MenuExit: TMenuItem;
    SelectColumns: TMenuItem;
    AssignToProcess: TMenuItem;
    MenuCloseCreationDlg: TMenuItem;
    SearchButtons: TImageList;
    AssignToThread: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    TokenRestrictSafer: TMenuItem;
    MenuSafeImpersonation: TMenuItem;
    MenuTools: TMenuItem;
    MenuSystemAudit: TMenuItem;
    MenuRunProgram: TMenuItem;
    TimerStateCheck: TTimer;
    TokenView: TFrameTokens;
    cmToken: TMenuItem;
    cmOpenCurrent: TMenuItem;
    cmOpenProcess: TMenuItem;
    cmOpenThread: TMenuItem;
    cmOpenEffective: TMenuItem;
    N4: TMenuItem;
    cmLogonUser: TMenuItem;
    cmQuerySession: TMenuItem;
    cmCreateToken: TMenuItem;
    cmAnonymousToken: TMenuItem;
    N5: TMenuItem;
    cmCopyHandle: TMenuItem;
    cmSearchToken: TMenuItem;
    N6: TMenuItem;
    cmRevokeCurrent: TMenuItem;
    cmRevokeToken: TMenuItem;
    cmAllocConsole: TMenuItem;
    N7: TMenuItem;
    cmAccess: TMenuItem;
    MenuSecurePrompt: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ActionDuplicate(Sender: TObject);
    procedure ActionClose(Sender: TObject);
    procedure ActionOpenProcess(Sender: TObject);
    procedure ActionRename(Sender: TObject);
    procedure ActionRunWithToken(Sender: TObject);
    procedure ActionOpenSelf(Sender: TObject);
    procedure RunAsAdminClick(Sender: TObject);
    procedure ActionSendHandle(Sender: TObject);
    procedure ActionDuplicateHandle(Sender: TObject);
    procedure ActionSearch(Sender: TObject);
    procedure ActionOpenLinked(Sender: TObject);
    procedure ActionOpen(Node: PVirtualNode);
    procedure RunAsSystemClick(Sender: TObject);
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
    procedure ListViewTokensEdited(Sender: TObject; Item: TListItem;
      var S: string);
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
    procedure TokenViewVSTGetPopupMenu(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure cmAllocConsoleClick(Sender: TObject);
    procedure cmAccessClick(Sender: TObject);
    procedure MenuSecurePromptClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UITypes, TU.Tokens.Old.Types, Ntapi.WinNt,
  NtUtils.Objects.Snapshots, TU.RestartSvc, TU.Suggestions, TU.Tokens,
  UI.Information, UI.ProcessList, UI.HandleSearch, UI.Modal.ComboDlg,
  UI.Restrict, UI.CreateToken, UI.Modal.Columns, UI.Modal.Access,
  UI.Modal.Logon, UI.Modal.AccessAndType, UI.Modal.PickUser, UI.Settings,
  UI.New.Safer, Ntapi.ntpsapi, UI.Audit.System, UI.Process.Run, Ntapi.ntstatus,
  DelphiUtils.Arrays, NtUiLib.Errors, Ntapi.ntseapi, NtUtils, UI.Access,
  NtUiLib.Exceptions.Dialog, UI.Prototypes.Forms, TU.Tokens.Open,
  NtUtils.Tokens.Impersonate, NtUtils.Processes, NtUtils.Objects, TU.Startup;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.ActionAssignToProcess;
var
  Token: IToken;
begin
  Token := TokenView.Selected;

  if AskConvertToPrimary(Handle, Token) then
    TokenView.Add(Token);

  Token.AssignToProcessById(TProcessListDialog
    .Execute(Self, False).ProcessID)
    .RaiseOnError;

  ShowSuccessMessage(Handle, 'The token was successfully assigned to the process.');
end;

procedure TFormMain.ActionAssignToThread;
var
  Token: IToken;
  TID: TThreadId;
begin
  Token := TokenView.Selected;

  if AskConvertToImpersonation(Handle, Token) then
    TokenView.Add(Token);

  TID := TProcessListDialog.Execute(Self, True).ThreadID;

  if TSettings.UseSafeImpersonation then
    Token.AssignToThreadSafeById(TID).RaiseOnError
  else
    Token.AssignToThreadById(TID).RaiseOnError;

  if TID = NtCurrentThreadId then
    CurrentUserChanged(Self);

  ShowSuccessMessage(Handle, 'The token was successfully assigned to the thread.');
end;

procedure TFormMain.ActionClose;
begin
  if not TSettings.PromptOnHandleClose or AskForConfirmation(Handle,
    'Are you sure you want to close selected handles?') then
    TokenView.DeleteSelected;
end;

procedure TFormMain.ActionDuplicate;
begin
  TokenView.Add(TDialogAccessAndType.ExecuteDuplication(Self,
    TokenView.Selected));
end;

procedure TFormMain.ActionDuplicateHandle;
begin
  TokenView.Add(TDialogAccess.ExecuteDuplication(Self, TokenView.Selected));
end;

procedure TFormMain.ActionLogon;
begin
  TLogonDialog.Create(Self).Show;
end;

procedure TFormMain.ActionOpen;
begin
  TInfoDialog.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionOpenEffective;
var
  Token: IToken;
begin
  MakeCopyViaDirectImpersonation(Token, nil, TProcessListDialog.Execute(Self,
    True).ThreadID, SecurityImpersonation).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.ActionOpenLinked;
var
  Linked: IToken;
begin
  TokenView.Selected.QueryLinkedToken(Linked).RaiseOnError;
  TokenView.Add(Linked);
end;

procedure TFormMain.ActionOpenProcess;
var
  Token: IToken;
begin
  MakeOpenProcessToken(Token, nil, TProcessListDialog.Execute(Self,
    False).ProcessID).RaiseOnError;

  TokenView.Add(Token);
end;

procedure TFormMain.ActionOpenSelf;
var
  Token: IToken;
begin
  MakeOpenProcessToken(Token, nil, NtCurrentProcessId).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.ActionOpenThread;
var
  Token: IToken;
begin
  MakeOpenThreadToken(Token, nil, TProcessListDialog.Execute(Self,
    True).ThreadID).RaiseOnError;

  TokenView.Add(Token);
end;

procedure TFormMain.ActionRename;
begin
  if TokenView.VST.SelectedCount = 1 then
    TokenView.VST.EditNode(TokenView.VST.FocusedNode, Integer(tsCaption));
end;

procedure TFormMain.ActionRestrict;
begin
  TDialogRestrictToken.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionRevertCurrentThread;
begin
  NtxSetThreadTokenById(NtCurrentThreadId, nil).RaiseOnError;

  CurrentUserChanged(Self);
  ShowSuccessMessage(Handle, 'The token was successfully revoked from the current thread.');
end;

procedure TFormMain.ActionRevertThread;
var
  TID: TThreadId;
begin
  TID := TProcessListDialog.Execute(Self, True).ThreadID;
  NtxSetThreadTokenById(TID, nil).RaiseOnError;

  if TID = NtCurrentThreadId then
    CurrentUserChanged(Self);

  ShowSuccessMessage(Handle, 'The token was successfully revoked from the thread.');
end;

procedure TFormMain.ActionRunWithToken;
begin
  if Assigned(TokenView.Selected) then
    with TDialogRun.Create(Self) do
    begin
      UseToken := TokenView.Selected;
      Show;
    end;
end;

procedure TFormMain.ActionSearch;
begin
  TFormHandleSearch.Create(Self).Show;
end;

procedure TFormMain.ActionSendHandle;
var
  hxTargetProcess: IHandle;
  NewHandle: NativeUInt;
begin
  NtxOpenProcess(hxTargetProcess, TProcessListDialog.Execute(Self,
    False).ProcessID, PROCESS_DUP_HANDLE).RaiseOnError;

  NtxDuplicateHandleTo(hxTargetProcess.Handle,
    TokenView.Selected.Handle.Handle, NewHandle).RaiseOnError;

  ShowSuccessMessage(Handle, Format('The handle was successfully sent.'#$D#$A +
    'Its value is %d (0x%X)', [NewHandle, NewHandle]));
end;

procedure TFormMain.ActionSteal;
begin
  TProcessListDialog.Execute(Self, False);//TODO: Copy handle from process
end;

procedure TFormMain.ActionWTSQuery;
var
  Token: IToken;
begin
  MakeSessionToken(Token, TComboDialog.PickSession(Self)).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.cmAccessClick(Sender: TObject);
begin
  TAccessCheckForm.CreateChild(Self, cfmDesktop).Show;
end;

procedure TFormMain.cmAllocConsoleClick(Sender: TObject);
var
  Result: TNtxStatus;
begin
  if not cmAllocConsole.Checked then
  begin
    Result.Location := 'AllocConsole';
    Result.Win32Result := AllocConsole;
  end
  else
  begin
    Result.Location := 'FreeConsole';
    Result.Win32Result := FreeConsole;
  end;

  Result.RaiseOnError;
  cmAllocConsole.Checked := not cmAllocConsole.Checked;
end;

procedure TFormMain.CurrentUserChanged;
begin
  if Active or (Sender <> TimerStateCheck) then
    Caption := 'Token Universe :: Main Window [' + FormatCurrentState + ']';
end;

procedure TFormMain.FormClose;
begin
  TFormEvents.OnMainFormClose.Invoke;
  TokenView.Free;

  if cmAllocConsole.Checked then
    FreeConsole;
end;

procedure TFormMain.FormCreate;
var
  Token: IToken;
  TokenType: TObjectTypeInfo;
  Elevation: TTokenElevationInfo;
  Handles: TArray<TProcessHandleEntry>;
  Linked: IToken;
  i: integer;
begin
  TokenView.VST.OnMainAction := ActionOpen;
  CurrentUserChanged(Self);

  // Search for inherited handles
  if RtlxFindKernelType('Token', TokenType).IsSuccess and
    NtxEnumerateHandlesProcess(NtCurrentProcess, Handles).IsSuccess then
  begin
    TArray.FilterInline<TProcessHandleEntry>(Handles,
      ByType(TokenType.Other.TypeIndex));

    for i := 0 to High(Handles) do
      TokenView.Add(CaptureTokenHandle(Auto.CaptureHandle(
        Handles[i].HandleValue), Format('Inherited %d [0x%x]',
        [Handles[i].HandleValue, Handles[i].HandleValue])));
  end;

  MakeOpenProcessToken(Token, nil, NtCurrentProcessId).RaiseOnError;
  TokenView.Add(Token);

  // Open current process and, maybe, its linked token
  if Token.QueryElevation(Elevation).IsSuccess and
    (Elevation.ElevationType <> TokenElevationTypeDefault) and
    Token.QueryLinkedToken(Linked).IsSuccess then
      TokenView.Add(Linked, nil, False);

  // Load useful delay dependencies while we can
  TuPreloadDelayModules;

  SetForegroundWindow(TokenView.VST.Handle);
end;

procedure TFormMain.RunAsAdminClick;
begin
  ReSvcDelegate(rmElevate);
  Close;
end;

procedure TFormMain.RunAsSystemClick;
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

procedure TFormMain.SelectColumnsClick;
begin
  TDialogColumns.CreateChild(Self, cfmApplication).ShowModal;
end;

procedure TFormMain.TokenRestrictSaferClick(Sender: TObject);
begin
  TDialogSafer.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.TokenViewVSTGetPopupMenu;
var
  Menu: TMenuItem;
begin
  for Menu in TokenMenu.Items do
    if Menu <> TokenClose then
      Menu.Visible := TokenView.VST.SelectedCount = 1;
end;

procedure TFormMain.ListViewTokensEdited;
begin
  TokenView.Selected.Caption := S;
end;

procedure TFormMain.MenuCloseCreationDlgClick;
begin
  TSettings.NoCloseCreationDialogs := not TSettings.NoCloseCreationDialogs;
  MenuCloseCreationDlg.Checked := TSettings.NoCloseCreationDialogs;
end;

procedure TFormMain.MenuExitClick;
begin
  Close;
end;

procedure TFormMain.MenuPromptHandleCloseClick;
begin
  TSettings.PromptOnHandleClose := not TSettings.PromptOnHandleClose;
  MenuPromptHandleClose.Checked := TSettings.PromptOnHandleClose;
end;

procedure TFormMain.MenuRunProgramClick;
begin
  TDialogRun.Create(Self).Show;
end;

procedure TFormMain.MenuSafeImpersonationClick;
begin
  TSettings.UseSafeImpersonation := not TSettings.UseSafeImpersonation;
  MenuSafeImpersonation.Checked := TSettings.UseSafeImpersonation;
end;

procedure TFormMain.MenuSecurePromptClick(Sender: TObject);
begin
  TSettings.PromtOnSecureDesktop := not TSettings.PromtOnSecureDesktop ;
  MenuSecurePrompt.Checked := TSettings.PromtOnSecureDesktop;
end;

procedure TFormMain.MenuSystemAuditClick;
begin
  TDialogSystemAudit.Create(Self).Show;
end;

procedure TFormMain.NewAnonymousClick;
var
  Token: IToken;
begin
  MakeAnonymousToken(Token).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.NewNtCreateTokenClick;
begin
  TDialogCreateToken.Create(Self).Show;
end;

end.
