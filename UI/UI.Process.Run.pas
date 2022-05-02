unit UI.Process.Run;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, UI.Prototypes.Forms,
  Vcl.ExtCtrls, Vcl.Menus, TU.Tokens, NtUtils.Environment,
  NtUtils.Objects, Ntapi.WinUser, NtUtils, Ntapi.ProcessThreadsApi,
  NtUtils.Processes.Create, Ntapi.ntpsapi, TU.Tokens3;

type
  TDialogRun = class(TChildForm)
    PageControl: TPageControl;
    TabParams: TTabSheet;
    EditExe: TLabeledEdit;
    ButtonBrowse: TButton;
    EditParams: TLabeledEdit;
    GroupBoxFlags: TGroupBox;
    CheckBoxInherit: TCheckBox;
    CheckBoxSuspended: TCheckBox;
    CheckBoxBreakaway: TCheckBox;
    ComboBoxLogonFlags: TComboBox;
    EditDir: TLabeledEdit;
    ComboBoxDesktop: TComboBox;
    ButtonClose: TButton;
    ButtonRun: TButton;
    LabelDesktop: TLabel;
    LabelLogonFlags: TLabel;
    PopupMenuExe: TPopupMenu;
    MenuCmd: TMenuItem;
    MenuSelf: TMenuItem;
    CheckBoxRunas: TCheckBox;
    OpenDlg: TOpenDialog;
    LabelShowMode: TLabel;
    ComboBoxShowMode: TComboBox;
    LinkLabelToken: TLinkLabel;
    ButtonChooseParent: TButton;
    EditParent: TEdit;
    PopupClearParent: TPopupMenu;
    MenuClearParent: TMenuItem;
    CheckBoxNewConsole: TCheckBox;
    CheckBoxRunAsInvoker: TCheckBox;
    EditAppContainer: TEdit;
    LabelAppContainer: TLabel;
    ButtonAC: TButton;
    PopupClearAC: TPopupMenu;
    MenuClearAC: TMenuItem;
    cbxOpenToken: TCheckBox;
    ComboMethod: TComboBox;
    LabelMethod: TLabel;
    Label1: TLabel;
    CheckBoxForceBreakaway: TCheckBox;
    procedure MenuSelfClick(Sender: TObject);
    procedure MenuCmdClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangedExecMethod(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure LinkLabelTokenLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonChooseParentClick(Sender: TObject);
    procedure MenuClearParentClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonACClick(Sender: TObject);
    procedure MenuClearACClick(Sender: TObject);
  private
    ExecMethod: TCreateProcessMethod;
    FToken: IToken;
    ParentAccessMask: TProcessAccessMask;
    hxParentProcess: IHandle;
    AppContainerSid: ISid;
    CaptionSubscription: IAutoReleasable;
    procedure UpdateEnabledState;
    procedure OnCaptionChange(const InfoClass: TTokenStringClass; const NewCaption: String);
    procedure SetToken(const Value: IToken);
    procedure UpdateDesktopList;
  public
    property UseToken: IToken read FToken write SetToken;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.Shlwapi, NtUtils.WinUser, Ntapi.ntseapi,
  NtUtils.Processes, NtUiLib.Errors, NtUtils.Tokens.Info,
  NtUtils.Processes.Create.Win32, NtUtils.Processes.Create.Shell,
  NtUtils.Processes.Create.Native, NtUtils.Processes.Create.Com,
  NtUtils.Processes.Create.Remote,  NtUtils.Processes.Create.Manual,
  NtUtils.Profiles, NtUtils.Tokens, TU.Exec,
  UI.Information, UI.ProcessList, UI.AppContainer.List, UI.MainForm,
  TU.Credentials;

{$R *.dfm}

procedure TDialogRun.ButtonACClick(Sender: TObject);
var
  hxToken: IHandle;
  User: ISid;
begin
  if Assigned(FToken) then
    hxToken := FToken.Handle
  else
    hxToken := NtxCurrentEffectiveToken;

  NtxQuerySidToken(hxToken, TokenUser, User).RaiseOnError;

  AppContainerSid := TDialogACProfiles.ExecuteSelect(FormMain, User);
  EditAppContainer.Text := UnvxAppContainerToString(AppContainerSid, User);
end;

procedure TDialogRun.ButtonBrowseClick(Sender: TObject);
begin
  if OpenDlg.Execute(Handle) then
  begin
    EditExe.Text := OpenDlg.FileName;
    ButtonRun.SetFocus;
  end;
end;

procedure TDialogRun.ButtonChooseParentClick(Sender: TObject);
var
  ClientIdEx: TClientIdEx;
begin
  ClientIdEx := TProcessListDialog.Execute(Self, False);
  NtxOpenProcess(hxParentProcess, ClientIdEx.ProcessID,
    ParentAccessMask).RaiseOnError;

  EditParent.Text := Format('%s [%d]', [ClientIdEx.ImageName,
    ClientIdEx.ProcessID]);
end;

procedure TDialogRun.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogRun.ButtonRunClick(Sender: TObject);
var
  Options: TCreateProcessOptions;
  ProcInfo: TProcessInfo;
  hxToken: IHandle;
begin
  Options := Default(TCreateProcessOptions);
  Options.Application := EditExe.Text;
  Options.Parameters := EditParams.Text;
  Options.CurrentDirectory := EditDir.Text;
  Options.Desktop := ComboBoxDesktop.Text;
  Options.hxParentProcess := hxParentProcess;
  Options.AppContainer := AppContainerSid;
  Options.LogonFlags := TProcessLogonFlags(ComboBoxLogonFlags.ItemIndex);
  Options.WindowMode := TShowMode(ComboBoxShowMode.ItemIndex);

  if Assigned(FToken) then
    Options.hxToken := FToken.Handle;

  if CheckBoxBreakaway.Checked then
    Include(Options.Flags, poBreakawayFromJob);

  if CheckBoxForceBreakaway.Checked then
    Include(Options.Flags, poForceBreakaway);

  if CheckBoxSuspended.Checked then
    Include(Options.Flags, poSuspended);

  if CheckBoxInherit.Checked then
    Include(Options.Flags, poInheritHandles);

  if CheckBoxNewConsole.Checked then
    Include(Options.Flags, poNewConsole);

  if ComboBoxShowMode.ItemIndex <> Integer(SW_SHOW_NORMAL) then
    Include(Options.Flags, poUseWindowMode);

  if CheckBoxRunAsInvoker.State <> cbGrayed then
  begin
    if CheckBoxRunAsInvoker.Checked then
      Include(Options.Flags, poRunAsInvokerOn)
    else
      Include(Options.Flags, poRunAsInvokerOff);
  end;

  if CheckBoxRunas.Checked then
    Include(Options.Flags, poRequireElevation);

  // Prompt for credentials if necessary
  if @ExecMethod = @AdvxCreateProcessWithLogon then
    PromptCredentialsUI(Handle,
      procedure (Domain, User, Password: String)
      begin
        Options.Domain := Domain;
        Options.Username := User;
        Options.Password := Password;
      end
    );

  // TODO: check that the process didn't crash immediately

  if Assigned(ExecMethod) then
    ExecMethod(Options, ProcInfo).RaiseOnError
  else
    raise Exception.Create('No exec method available');

  // Suggest opening the token since we have a handle anyway
  if Assigned(ProcInfo.hxProcess) and cbxOpenToken.Checked and
    NtxOpenProcessToken(hxToken, ProcInfo.hxProcess.Handle,
    MAXIMUM_ALLOWED).IsSuccess then
    FormMain.TokenView.Add(TToken.Create(hxToken,
      Format('%s [%d]', [ExtractFileName(EditExe.Text),
        ProcInfo.ClientId.UniqueProcess])));
end;

procedure TDialogRun.ChangedExecMethod(Sender: TObject);
var
  OldParentAccessMask: TProcessAccessMask;
begin
  case ComboMethod.ItemIndex of
    0: ExecMethod := AdvxCreateProcess;
    1: ExecMethod := AdvxCreateProcessWithToken;
    2: ExecMethod := AdvxCreateProcessWithLogon;
    3: ExecMethod := AdvxCreateProcessRemote;
    4: ExecMethod := RtlxCreateUserProcess;
    5: ExecMethod := RtlxCreateUserProcessEx;
    6: ExecMethod := NtxCreateUserProcess;
    7: ExecMethod := NtxCreateProcessEx;
    8: ExecMethod := ShlxExecute;
    9: ExecMethod := ComxShellExecute;
    10: ExecMethod := WdcxCreateProcess;
    11: ExecMethod := WmixCreateProcess;
  else
    ExecMethod := nil;
  end;

  if ppParentProcess in ExecSupports(ExecMethod) then
  begin
    OldParentAccessMask := ParentAccessMask;

    // Determine required access to the parent process
    if @ExecMethod = @AdvxCreateProcessRemote then
      ParentAccessMask := PROCESS_CREATE_PROCESS_REMOTE
    else
      ParentAccessMask := PROCESS_CREATE_PROCESS;

    // Try to reopen the parent if necessary; clear silently on failure
    if (ParentAccessMask <> OldParentAccessMask) and Assigned(hxParentProcess)
      and not NtxReopenHandle(hxParentProcess, ParentAccessMask).IsSuccess then
        MenuClearParentClick(Sender);
  end;

  UpdateEnabledState;
end;

constructor TDialogRun.Create(AOwner: TComponent);
begin
  inherited CreateChild(AOwner, cfmDesktop);
  ParentAccessMask := PROCESS_CREATE_PROCESS;
end;

procedure TDialogRun.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UseToken := nil;
  MenuClearParentClick(Sender);
end;

procedure TDialogRun.FormCreate(Sender: TObject);
begin
  EditExe.Text := GetEnvironmentVariable('ComSpec');
  SHAutoComplete(EditExe.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditDir.Handle, SHACF_FILESYS_DIRS);
  ChangedExecMethod(Sender);
  UpdateDesktopList;
end;

procedure TDialogRun.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Ctrl+N to switch between tabs
  if (Shift = [ssCtrl]) and (Key >= Ord('1')) and (Key <= Ord('4')) then
    PageControl.ActivePageIndex := Key - Ord('1');

  // Ctrl+O to choose a file
  if (Shift = [ssCtrl]) and (Key = Ord('O')) then
    ButtonBrowse.Click;
end;

procedure TDialogRun.LinkLabelTokenLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  if Assigned(FToken) then
    TInfoDialog.CreateFromToken(Self, FToken);
end;

procedure TDialogRun.MenuClearACClick(Sender: TObject);
begin
  AppContainerSid := nil;
  EditAppContainer.Text := 'No';
end;

procedure TDialogRun.MenuClearParentClick(Sender: TObject);
begin
  hxParentProcess := nil;
  EditParent.Text := '<not specified>';
end;

procedure TDialogRun.MenuCmdClick(Sender: TObject);
begin
  EditExe.Text := GetEnvironmentVariable('ComSpec');
  ButtonRun.SetFocus;
end;

procedure TDialogRun.MenuSelfClick(Sender: TObject);
begin
  EditExe.Text := ParamStr(0);
  ButtonRun.SetFocus;
end;

procedure TDialogRun.OnCaptionChange;
begin
  LinkLabelToken.Caption := 'Using token: <a>' + NewCaption + '</a>';
end;

procedure TDialogRun.SetToken(const Value: IToken);
begin
  FToken := Value;

  if not Assigned(Value) then
    LinkLabelToken.Caption := 'Using token: <not specified>'
  else
    CaptionSubscription := (Value as IToken3).ObserveString(tsCaption, OnCaptionChange);
end;

procedure TDialogRun.UpdateDesktopList;
var
  Current: string;
  Desktops: TArray<String>;
  Found: Boolean;
  i: Integer;
begin
  Desktops := UsrxEnumAllDesktops;
  Current := UsrxCurrentDesktopName.ToLower;

  with ComboBoxDesktop do
  begin
    Items.BeginUpdate;
    Items.Clear;

    Found := False;

    // Add all enumerated desktops
    for i := 0 to High(Desktops) do
    begin
      Items.Add(Desktops[i]);

      if Desktops[i].ToLower = Current then
      begin
        // Select the current one
        ItemIndex := i;
        Found := True;
      end;
    end;

    // Make sure the current desktop is in the list
    if not Found then
      ItemIndex := Items.Add(Current);

    Items.EndUpdate;
  end;
end;

procedure TDialogRun.UpdateEnabledState;
var
  SupportedOptions: TExecParamSet;
begin
  SupportedOptions := ExecSupports(ExecMethod);

  EditDir.Enabled := ppCurrentDirectory in SupportedOptions;
  ComboBoxDesktop.Enabled := ppDesktop in SupportedOptions;
  ComboBoxLogonFlags.Enabled := ppLogonFlags in SupportedOptions;
  CheckBoxInherit.Enabled := ppInheritHandles in SupportedOptions;
  CheckBoxSuspended.Enabled := ppCreateSuspended in SupportedOptions;
  CheckBoxBreakaway.Enabled := ppBreakaway in SupportedOptions;
  CheckBoxForceBreakaway.Enabled := ppForceBreakaway in SupportedOptions;
  CheckBoxNewConsole.Enabled := ppNewConsole in SupportedOptions;
  CheckBoxRunas.Enabled := ppRequireElevation in SupportedOptions;
  ComboBoxShowMode.Enabled := ppShowWindowMode in SupportedOptions;
  CheckBoxRunAsInvoker.Enabled := ppRunAsInvoker in SupportedOptions;
  EditParent.Enabled := ppParentProcess in SupportedOptions;
  ButtonChooseParent.Enabled := ppParentProcess in SupportedOptions;
  EditAppContainer.Enabled := ppAppContainer in SupportedOptions;
  ButtonAC.Enabled := ppAppContainer in SupportedOptions;
end;

end.
