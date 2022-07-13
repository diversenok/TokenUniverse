unit UI.Process.Run;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, UI.Prototypes.Forms,
  Vcl.ExtCtrls, Vcl.Menus, TU.Tokens, NtUtils.Environment,
  NtUtils.Objects, Ntapi.WinUser, NtUtils, Ntapi.ProcessThreadsApi,
  NtUtils.Processes.Create, Ntapi.ntpsapi;

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
    CheckBoxIgnoreElevation: TCheckBox;
    Isolation: TTabSheet;
    CheckBoxLPAC: TCheckBox;
    GroupBoxChildFlags: TGroupBox;
    CheckBoxChildRestricted: TCheckBox;
    CheckBoxChildUnlessSecure: TCheckBox;
    CheckBoxChildOverride: TCheckBox;
    Manifest: TTabSheet;
    RadioButtonManifestNone: TRadioButton;
    RadioButtonManifestEmbedded: TRadioButton;
    RadioButtonManifestExternalExe: TRadioButton;
    EditManifestExecutable: TEdit;
    RadioButtonManifestExternal: TRadioButton;
    EditManifestFile: TEdit;
    RadioButtonManifestCustom: TRadioButton;
    CheckBoxManifestThemes: TCheckBox;
    LabelManifestDpi: TLabel;
    ComboBoxManifestDpi: TComboBox;
    CheckBoxManifestGdiScaling: TCheckBox;
    CheckBoxManifestLongPaths: TCheckBox;
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
    procedure EditManifestExecutableEnter(Sender: TObject);
    procedure EditManifestFileEnter(Sender: TObject);
    procedure CheckBoxManifestThemesEnter(Sender: TObject);
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
    function TryOpenToken(const Info: TProcessInfo): TNtxStatus;
  public
    property UseToken: IToken read FToken write SetToken;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.Shlwapi, NtUtils.WinUser, Ntapi.ntseapi, Ntapi.ntstatus,
  Ntapi.ntcsrapi, NtUtils.Processes, NtUiLib.Errors, NtUtils.Sections,
  NtUtils.Tokens.Info, NtUtils.Processes.Create.Win32, NtUtils.Profiles,
  NtUtils.Processes.Create.Shell, NtUtils.Processes.Create.Native,
  NtUtils.Processes.Create.Com, NtUtils.Processes.Create.Remote,
  NtUtils.Processes.Create.Manual, NtUtils.Tokens, NtUtils.Csr,
  NtUiLib.TaskDialog, NtUtils.SysUtils, NtUtils.Threads, NtUtils.Manifests,
  NtUtils.Processes.Info, NtUtils.Files.Open, TU.Exec, UI.Information,
  UI.ProcessList, UI.AppContainer.List, UI.MainForm, TU.Credentials,
  TU.Tokens.Open;

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
  AutoCancel: IAutoReleasable;
  ManifestBuilfer: IManifestBuilder;
  ManifestRva: TMemory;
  hxManifestSection: IHandle;
begin
  if (@ExecMethod = @AdvxCreateProcessRemote) and
    not Assigned(hxParentProcess) then
  begin
    UsrxShowTaskDialog(Handle, 'Error', 'Invalid Parent Process',
      'The selected method requires explicitly specifying a parent process.',
      diError, dbOk);
    Exit;
  end;

  Options := Default(TCreateProcessOptions);
  Options.Application := EditExe.Text;
  Options.Parameters := EditParams.Text;
  Options.CurrentDirectory := EditDir.Text;
  Options.Desktop := ComboBoxDesktop.Text;
  Options.hxParentProcess := hxParentProcess;
  Options.AppContainer := AppContainerSid;
  Options.LogonFlags := TProcessLogonFlags(ComboBoxLogonFlags.ItemIndex);
  Options.WindowMode := TShowMode32(ComboBoxShowMode.ItemIndex);

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

  if CheckBoxIgnoreElevation.Checked then
    Include(Options.Flags, poIgnoreElevation);

  if ComboBoxShowMode.ItemIndex <> Integer(TShowMode32.SW_SHOW_NORMAL) then
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

  if CheckBoxLPAC.Checked then
    Include(Options.Flags, poLPAC);

  if CheckBoxChildRestricted.Checked then
    Options.ChildPolicy := Options.ChildPolicy or
      PROCESS_CREATION_CHILD_PROCESS_RESTRICTED;

  if CheckBoxChildUnlessSecure.Checked then
    Options.ChildPolicy := Options.ChildPolicy or
      PROCESS_CREATION_CHILD_PROCESS_RESTRICTED_UNLESS_SECURE;

  if CheckBoxChildOverride.Checked then
    Options.ChildPolicy := Options.ChildPolicy or
      PROCESS_CREATION_CHILD_PROCESS_OVERRIDE;

  if (spoDetectManifest in ExecSupports(ExecMethod)) and
    not RadioButtonManifestNone.Checked then
  begin
    Include(Options.Flags, poDetectManifest);

    // Also suspend while we register with SxS
    Include(Options.Flags, poSuspended);
  end;

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
    raise Exception.Create('No exec method selected');

  // Manually register with SxS if necessary
  if (spoDetectManifest in ExecSupports(ExecMethod)) and
    not RadioButtonManifestNone.Checked then
  begin
    // Terminate on failure
    AutoCancel := NtxDelayedTerminateProcess(ProcInfo.hxProcess,
      STATUS_CANCELLED);

    // Embedded
    if RadioButtonManifestEmbedded.Checked and
      (@ExecMethod <> @AdvxCreateProcess) then
      CsrxRegisterProcessCreation(Options, ProcInfo).RaiseOnError

    // External PE file
    else if RadioButtonManifestExternalExe.Checked then
    begin
      RtlxCreateFileSection(
        hxManifestSection,
        FileOpenParameters.UseFileName(EditManifestExecutable.Text, fnWin32),
        RtlxSecImageNoExecute
      ).RaiseOnError;

      RtlxFindManifestInSection(
        hxManifestSection.Handle,
        ManifestRva
      ).RaiseOnError;

      CsrxRegisterProcessManifest(
        ProcInfo.hxProcess.Handle,
        ProcInfo.hxThread.Handle,
        ProcInfo.ClientId,
        hxManifestSection.Handle,
        BASE_MSG_HANDLETYPE_SECTION,
        ManifestRva,
        Options.ApplicationWin32
      ).RaiseOnError
    end

    // External XML file
    else if RadioButtonManifestExternal.Checked then
      CsrxRegisterProcessManifestFromFile(
        ProcInfo.hxProcess.Handle,
        ProcInfo.hxThread.Handle,
        ProcInfo.ClientId,
        EditManifestFile.Text,
        Options.ApplicationWin32
      ).RaiseOnError

    // Custom
    else if RadioButtonManifestCustom.Checked then
    begin
      ManifestBuilfer := NewManifestBuilder
        .UseRuntimeThemes(CheckBoxManifestThemes.Checked)
        .UseGdiScaling(CheckBoxManifestGdiScaling.Checked)
        .UseLongPathAware(CheckBoxManifestLongPaths.Checked);

      case ComboBoxManifestDpi.ItemIndex of
        1: ManifestBuilfer := ManifestBuilfer
            .UseDpiAware(dpiAwareFalse)
            .UseDpiAwareness(dpiUnaware);

        2: ManifestBuilfer := ManifestBuilfer
            .UseDpiAware(dpiAwareTrue)
            .UseDpiAwareness(dpiSystem);

        3: ManifestBuilfer := ManifestBuilfer
            .UseDpiAware(dpiAwareTruePerMonitor)
            .UseDpiAwareness(dpiPerMonitor);

        4: ManifestBuilfer := ManifestBuilfer
            .UseDpiAware(dpiAwareTruePerMonitor)
            .UseDpiAwareness(dpiPerMonitorV2);
      end;

      CsrxRegisterProcessManifestFromString(
        ProcInfo.hxProcess.Handle,
        ProcInfo.hxThread.Handle,
        ProcInfo.ClientId,
        ManifestBuilfer.Build,
        Options.ApplicationWin32
      ).RaiseOnError
    end;

    // Prevent termination on failure
    AutoCancel.AutoRelease := False;

    if not CheckBoxSuspended.Checked then
      NtxResumeThread(ProcInfo.hxThread.Handle);
  end;

  // Check if we need to open the token since we might have a process handle
  if cbxOpenToken.Checked then
    TryOpenToken(ProcInfo);
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

  if spoParentProcess in ExecSupports(ExecMethod) then
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

procedure TDialogRun.CheckBoxManifestThemesEnter;
begin
  RadioButtonManifestCustom.Checked := True;
end;

constructor TDialogRun.Create(AOwner: TComponent);
begin
  inherited CreateChild(AOwner, cfmDesktop);
  ParentAccessMask := PROCESS_CREATE_PROCESS;
end;

procedure TDialogRun.EditManifestExecutableEnter;
begin
  RadioButtonManifestExternalExe.Checked := True;
end;

procedure TDialogRun.EditManifestFileEnter;
begin
  RadioButtonManifestExternal.Checked := True;
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
  SHAutoComplete(EditManifestExecutable.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditManifestFile.Handle, SHACF_FILESYS_ONLY);
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

procedure TDialogRun.SetToken;
begin
  FToken := Value;

  if not Assigned(Value) then
    LinkLabelToken.Caption := 'Using token: <not specified>'
  else
    CaptionSubscription := Value.ObserveString(tsCaption, OnCaptionChange);
end;

function TDialogRun.TryOpenToken;
var
  Token: IToken;
  PID: TProcessId;
begin
  if not (piProcessHandle in Info.ValidFields) then
  begin
    Result.Location := 'TDialogRun.TryOpenToken';
    Result.Status := STATUS_INVALID_HANDLE;
    Exit;
  end;

  if piProcessID in Info.ValidFields then
    PID := Info.ClientId.UniqueProcess
  else
    PID := 0;

  Result := MakeOpenProcessToken(Token, Info.hxProcess, PID);

  if Result.IsSuccess then
    FormMain.TokenView.Add(Token);
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

  ComboBoxDesktop.Enabled := spoDesktop in SupportedOptions;
  ComboBoxLogonFlags.Enabled := spoCredentials in SupportedOptions;
  CheckBoxInherit.Enabled := spoInheritHandles in SupportedOptions;
  CheckBoxSuspended.Enabled := spoSuspended in SupportedOptions;
  CheckBoxBreakaway.Enabled := spoBreakawayFromJob in SupportedOptions;
  CheckBoxForceBreakaway.Enabled := spoForceBreakaway in SupportedOptions;
  CheckBoxNewConsole.Enabled := spoNewConsole in SupportedOptions;
  CheckBoxRunas.Enabled := spoRequireElevation in SupportedOptions;
  CheckBoxIgnoreElevation.Enabled := spoIgnoreElevation in SupportedOptions;
  ComboBoxShowMode.Enabled := spoWindowMode in SupportedOptions;
  CheckBoxRunAsInvoker.Enabled := spoRunAsInvoker in SupportedOptions;
  EditParent.Enabled := spoParentProcess in SupportedOptions;
  ButtonChooseParent.Enabled := spoParentProcess in SupportedOptions;
  EditAppContainer.Enabled := spoAppContainer in SupportedOptions;
  ButtonAC.Enabled := spoAppContainer in SupportedOptions;
  CheckBoxLPAC.Enabled := spoLPAC in SupportedOptions;
  CheckBoxChildRestricted.Enabled := spoChildPolicy in SupportedOptions;
  CheckBoxChildUnlessSecure.Enabled := spoChildPolicy in SupportedOptions;
  CheckBoxChildOverride.Enabled := spoChildPolicy in SupportedOptions;
  RadioButtonManifestNone.Enabled := spoDetectManifest in SupportedOptions;
  RadioButtonManifestEmbedded.Enabled := spoDetectManifest in SupportedOptions;
  RadioButtonManifestExternalExe.Enabled := spoDetectManifest in SupportedOptions;
  EditManifestExecutable.Enabled := spoDetectManifest in SupportedOptions;
  RadioButtonManifestExternal.Enabled := spoDetectManifest in SupportedOptions;
  EditManifestFile.Enabled := spoDetectManifest in SupportedOptions;
  RadioButtonManifestCustom.Enabled := spoDetectManifest in SupportedOptions;
  CheckBoxManifestThemes.Enabled := spoDetectManifest in SupportedOptions;
  CheckBoxManifestGdiScaling.Enabled := spoDetectManifest in SupportedOptions;
  CheckBoxManifestLongPaths.Enabled := spoDetectManifest in SupportedOptions;
  LabelManifestDpi.Enabled := spoDetectManifest in SupportedOptions;
  ComboBoxManifestDpi.Enabled := spoDetectManifest in SupportedOptions;
end;

end.
