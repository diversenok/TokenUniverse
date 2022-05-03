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
  NtUiLib.Exceptions.Dialog, UI.Prototypes.Forms, TU.Tokens3, TU.Tokens3.Open;

{$R *.dfm}

{ TFormMain }

procedure TFormMain.ActionAssignToProcess(Sender: TObject);
var
  Token: IToken;
begin
  Token := TokenView.Selected;

  if AskConvertToPrimary(Handle, Token) then
    TokenView.Add(Token);

  (Token as IToken3).AssignToProcessById(TProcessListDialog
    .Execute(Self, False).ProcessID)
    .RaiseOnError;

  ShowSuccessMessage(Handle, 'The token was successfully assigned to the process.');
end;

procedure TFormMain.ActionAssignToThread(Sender: TObject);
var
  Token: IToken;
  TID: TThreadId;
begin
  Token := TokenView.Selected;

  if AskConvertToImpersonation(Handle, Token) then
    TokenView.Add(Token);

  TID := TProcessListDialog.Execute(Self, True).ThreadID;

  if TSettings.UseSafeImpersonation then
    (Token as IToken3).AssignToThreadSafeById(TID).RaiseOnError
  else
    (Token as IToken3).AssignToThreadById(TID).RaiseOnError;

  if TID = NtCurrentThreadId then
    CurrentUserChanged(Self);

  ShowSuccessMessage(Handle, 'The token was successfully assigned to the thread.');
end;

procedure TFormMain.ActionClose(Sender: TObject);
begin
  if not TSettings.PromptOnHandleClose or AskForConfirmation(Handle,
    'Are you sure you want to close selected handles?') then
    TokenView.DeleteSelected;
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

procedure TFormMain.ActionOpen;
begin
  TInfoDialog.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionOpenEffective(Sender: TObject);
var
  Token: IToken;
begin
  MakeCopyViaDirectImpersonation(Token, nil, TProcessListDialog.Execute(Self,
    True).ThreadID, SecurityImpersonation).RaiseOnError;
  TokenView.Add(Token);
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
  Token: IToken;
begin
  MakeOpenProcessToken(Token, nil, TProcessListDialog.Execute(Self,
    False).ProcessID).RaiseOnError;

  TokenView.Add(Token);
end;

procedure TFormMain.ActionOpenSelf(Sender: TObject);
var
  Token: IToken;
begin
  MakeOpenProcessToken(Token, nil, NtCurrentProcessId).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.ActionOpenThread(Sender: TObject);
var
  Token: IToken;
begin
  MakeOpenThreadToken(Token, nil, TProcessListDialog.Execute(Self,
    True).ThreadID).RaiseOnError;

  TokenView.Add(Token);
end;

procedure TFormMain.ActionRename(Sender: TObject);
begin
  if TokenView.VST.SelectedCount = 1 then
    TokenView.VST.EditNode(TokenView.VST.FocusedNode, Integer(tsCaption));
end;

procedure TFormMain.ActionRestrict(Sender: TObject);
begin
  TDialogRestrictToken.CreateFromToken(Self, TokenView.Selected);
end;

procedure TFormMain.ActionRevertCurrentThread(Sender: TObject);
begin
  TToken.RevertThreadToken(NtCurrentThreadId);

  CurrentUserChanged(Self);
  ShowSuccessMessage(Handle, 'The token was successfully revoked from the current thread.');
end;

procedure TFormMain.ActionRevertThread(Sender: TObject);
var
  TID: TThreadId;
begin
  TID := TProcessListDialog.Execute(Self, True).ThreadID;
  TToken.RevertThreadToken(TID);

  if TID = NtCurrentThreadId then
    CurrentUserChanged(Self);

  ShowSuccessMessage(Handle, 'The token was successfully revoked from the thread.');
end;

procedure TFormMain.ActionRunWithToken(Sender: TObject);
begin
  if Assigned(TokenView.Selected) then
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

  ShowSuccessMessage(Handle, Format('The handle was successfully sent.'#$D#$A +
    'Its value is %d (0x%X)', [NewHandle, NewHandle]));
end;

procedure TFormMain.ActionSteal(Sender: TObject);
begin
  TProcessListDialog.Execute(Self, False);//TODO: Copy handle from process
end;

procedure TFormMain.ActionWTSQuery(Sender: TObject);
var
  Token: IToken;
begin
  MakeSessionToken(Token, TComboDialog.PickSession(Self)).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.CurrentUserChanged(Sender: TObject);
begin
  if Active or (Sender <> TimerStateCheck) then
    Caption := 'Token Universe :: Main Window [' + FormatCurrentState + ']';
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TFormEvents.OnMainFormClose.Invoke;
  TokenView.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Token: IToken;
  Elevation: TTokenElevationInfo;
  Handles: TArray<TProcessHandleEntry>;
  Linked: IToken;
  i: integer;
begin
  TokenView.VST.OnInspectNode := ActionOpen;
  CurrentUserChanged(Self);

  // Search for inherited handles
  if NtxEnumerateHandlesProcess(NtCurrentProcess, Handles).IsSuccess then
  begin
    // TODO: obtain token's type index in runtime
    TArray.FilterInline<TProcessHandleEntry>(Handles, ByType(5));

    for i := 0 to High(Handles) do
      TokenView.Add(TToken.CreateByHandle(Handles[i].HandleValue));
  end;

  MakeOpenProcessToken(Token, nil, NtCurrentProcessId).RaiseOnError;
  TokenView.Add(Token);

  // Open current process and, maybe, its linked token
  if (Token as IToken3).QueryElevation(Elevation).IsSuccess and
    (Elevation.ElevationType <> TokenElevationTypeDefault) and
    Token.OpenLinkedToken(Linked).IsSuccess then
      TokenView.Add(Linked);

  LoadLibrary('xmllite.dll');

  SetForegroundWindow(Handle);
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

procedure TFormMain.SelectColumnsClick(Sender: TObject);
begin
  TDialogColumns.CreateChild(Self, cfmApplication).ShowModal;
end;

procedure TFormMain.TokenRestrictSaferClick(Sender: TObject);
begin
  TDialogSafer.CreateFromToken(Self, TokenView.Selected as IToken3);
end;

procedure TFormMain.TokenViewVSTGetPopupMenu;
var
  Menu: TMenuItem;
begin
  for Menu in TokenMenu.Items do
    if Menu <> TokenClose then
      Menu.Visible := TokenView.VST.SelectedCount = 1;
end;

procedure TFormMain.ListViewTokensEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  TokenView.Selected.Caption := S;
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
var
  Token: IToken;
begin
  MakeAnonymousToken(Token).RaiseOnError;
  TokenView.Add(Token);
end;

procedure TFormMain.NewNtCreateTokenClick(Sender: TObject);
begin
  TDialogCreateToken.Create(Self).Show;
end;

end.
