unit UI.Process.Run;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, UI.Prototypes.Forms,
  Vcl.ExtCtrls, Vcl.Menus, TU.Tokens, NtUtils.Environment, NtUtils,
  Ntapi.ProcessThreadsApi, NtUtils.Processes.Create, Ntapi.ntpsapi,
  TU.Processes.Create, Ntapi.WinNt, NtUiFrame.AppContainer.Edit;

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
    CheckBoxInheritConsole: TCheckBox;
    CheckBoxRunAsInvoker: TCheckBox;
    LabelAppContainer: TLabel;
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
    LabelProtection: TLabel;
    ComboBoxProtection: TComboBox;
    EditAppId: TEdit;
    LabelAppId: TLabel;
    AppContainerField: TAppContainerFieldFrame;
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
    procedure EditManifestExecutableEnter(Sender: TObject);
    procedure EditManifestFileEnter(Sender: TObject);
    procedure CheckBoxManifestThemesEnter(Sender: TObject);
  private
    Method: TKnownCreateMethod;
    FToken: IToken;
    ParentAccessMask: TProcessAccessMask;
    ParentProcessId: TProcessId;
    hxParentProcess: IHandle;
    CaptionSubscription: IAutoReleasable;
    procedure UpdateEnabledState;
    procedure OnCaptionChange(const InfoClass: TTokenStringClass; const NewCaption: String);
    procedure SetToken(const Value: IToken);
    procedure UpdateDesktopList;
    function TryOpenToken(const Info: TProcessInfo): TNtxStatus;
    function GetProtection(out Value: TProtectionLevel): Boolean;
  public
    property UseToken: IToken read FToken write SetToken;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Ntapi.ntstatus, Ntapi.WinError, Ntapi.ntseapi, Ntapi.WinUser,
  Ntapi.wincred, Ntapi.Shlwapi,
  NtUtils.Processes, NtUtils.Processes.Create.Remote, NtUtils.Objects,
  NtUtils.WinUser, NtUtils.Tokens, NtUtils.Tokens.Info, NtUtils.Profiles,
  NtUiLib.Errors, NtUiLib.TaskDialog, NtUiLib.WinCred,
  UI.Information, UI.ProcessList, UI.MainForm,
  TU.Tokens.Open, UI.Settings, System.UITypes, NtUtils.Security.Sid;

{$R *.dfm}

procedure TDialogRun.ButtonBrowseClick;
begin
  if OpenDlg.Execute(Handle) then
  begin
    EditExe.Text := OpenDlg.FileName;
    ButtonRun.SetFocus;
  end;
end;

procedure TDialogRun.ButtonChooseParentClick;
var
  ClientIdEx: TClientIdEx;
begin
  ClientIdEx := TProcessListDialog.Execute(Self, False);
  NtxOpenProcess(hxParentProcess, ClientIdEx.ProcessID,
    ParentAccessMask).RaiseOnError;

  ParentProcessId := ClientIdEx.ProcessID;
  EditParent.Text := Format('%s [%d]', [ClientIdEx.ImageName,
    ClientIdEx.ProcessID]);
end;

procedure TDialogRun.ButtonCloseClick;
begin
  Close;
end;

procedure TDialogRun.ButtonRunClick;
var
  Options: TCreateProcessOptions;
  OptionsEx: TTuCreateProcessOptions;
  ProcInfo: TProcessInfo;
  Credentials: TLogonCredentials;
  PromptFlags: TCredUiWinFlags;
  Status: TNtxStatus;
begin
  if (Method = cmCreateProcessViaInjection) and not Assigned(hxParentProcess) then
  begin
    UsrxShowTaskDialog(Handle, 'Error', 'Invalid Parent Process',
      'The selected method requires explicitly specifying a parent process.',
      diError, dbOk);
    Exit;
  end;

  if (Method = cmIDesktopAppxActivator) and (EditAppId.Text = '') then
  begin
    UsrxShowTaskDialog(Handle, 'Error', 'Invalid AppUserModeId',
      'The selected method requires explicitly specifying package information.',
      diError, dbOk);
    Exit;
  end;

  Options := Default(TCreateProcessOptions);
  Options.Application := EditExe.Text;
  Options.Parameters := EditParams.Text;
  Options.CurrentDirectory := EditDir.Text;
  Options.Desktop := ComboBoxDesktop.Text;
  Options.ParentProcessId := ParentProcessId;
  Options.hxParentProcess := hxParentProcess;
  Options.AppContainer := AppContainerField.Sid;
  Options.LogonFlags := TProcessLogonFlags(ComboBoxLogonFlags.ItemIndex);
  Options.WindowMode := TShowMode32(ComboBoxShowMode.ItemIndex);
  Options.AppUserModeId := EditAppId.Text;

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

  if CheckBoxInheritConsole.Checked then
    Include(Options.Flags, poInheritConsole);

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

  if GetProtection(Options.Protection) then
    Include(Options.Flags, poUseProtection);

  OptionsEx := Default(TTuCreateProcessOptions);

  if RadioButtonManifestNone.Checked then
    OptionsEx.ManifestMode := mmNoRegistration
  else if RadioButtonManifestEmbedded.Checked then
    OptionsEx.ManifestMode := mmUseEmbedded
  else if RadioButtonManifestExternalExe.Checked then
  begin
    OptionsEx.ManifestMode := mmUseFromPE;
    OptionsEx.ManifestFilename := EditManifestExecutable.Text;
  end
  else if RadioButtonManifestExternal.Checked then
  begin
    OptionsEx.ManifestMode := mmUseFromXML;
    OptionsEx.ManifestFilename := EditManifestFile.Text;
  end
  else if RadioButtonManifestCustom.Checked then
  begin
    OptionsEx.ManifestMode := mmCustom;
    OptionsEx.UseRuntimeThemes := CheckBoxManifestThemes.Checked;
    OptionsEx.UseGdiScaling := CheckBoxManifestGdiScaling.Checked;
    OptionsEx.UseLongPathAware := CheckBoxManifestLongPaths.Checked;
    Integer(OptionsEx.DpiAwareness) := ComboBoxManifestDpi.ItemIndex;
  end;

  // Prompt for credentials when using logon
  if Method = cmCreateProcessWithLogon then
  begin
    if TSettings.PromtOnSecureDesktop then
      PromptFlags := CREDUIWIN_SECURE_PROMPT
    else
      PromptFlags := 0;

    Status := CredxPromptForWindowsCredentials(Handle, 'Token Universe',
      'Credentials for the new process:', Credentials,
      PromptFlags);

    if Status.Win32Error = ERROR_CANCELLED then
      Abort;

    Status.RaiseOnError;

    Options.Domain := Credentials.Domain;
    Options.Username := Credentials.Username;
    Options.Password := Credentials.Password;
  end;

  TuCreateProcess(Handle, Options, OptionsEx, Method, ProcInfo).RaiseOnError;

  // TODO: check that the process didn't crash immediately

  // Check if we need to open the token since we might have a process handle
  if cbxOpenToken.Checked then
    TryOpenToken(ProcInfo);
end;

procedure TDialogRun.ChangedExecMethod;
var
  OldParentAccessMask: TProcessAccessMask;
begin
  Method := TKnownCreateMethod(ComboMethod.ItemIndex + 1);

  if spoParentProcess in TuPsMethodSupports(Method) then
  begin
    OldParentAccessMask := ParentAccessMask;

    // Determine required access to the parent process
    if Method = cmCreateProcessViaInjection then
      ParentAccessMask := PROCESS_CREATE_PROCESS_REMOTE
    else if (Method = cmCreateProcessWithToken) or
      (Method = cmCreateProcessWithLogon) then
      ParentAccessMask := PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_DUP_HANDLE
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

constructor TDialogRun.Create;
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

procedure TDialogRun.FormClose;
begin
  UseToken := nil;
  MenuClearParentClick(Sender);
end;

procedure TDialogRun.FormCreate;
begin
  EditExe.Text := GetEnvironmentVariable('ComSpec');
  SHAutoComplete(EditExe.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditDir.Handle, SHACF_FILESYS_DIRS);
  SHAutoComplete(EditManifestExecutable.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditManifestFile.Handle, SHACF_FILESYS_ONLY);
  ChangedExecMethod(Sender);
  UpdateDesktopList;
end;

procedure TDialogRun.FormKeyDown;
begin
  // Ctrl+N to switch between tabs
  if (Shift = [ssCtrl]) and (Key >= Ord('1')) and (Key <= Ord('3')) then
    PageControl.ActivePageIndex := Key - Ord('1');

  // Ctrl+O to choose a file
  if (Shift = [ssCtrl]) and (Key = Ord('O')) then
    ButtonBrowse.Click;
end;

function TDialogRun.GetProtection;
begin
  Result := True;

  case ComboBoxProtection.ItemIndex of
    1: Value := PROTECTION_LEVEL_CODEGEN_LIGHT;
    2: Value := PROTECTION_LEVEL_ANTIMALWARE_LIGHT;
    3: Value := PROTECTION_LEVEL_PPL_APP;
    4: Value := PROTECTION_LEVEL_LSA_LIGHT;
    5: Value := PROTECTION_LEVEL_WINDOWS_LIGHT;
    6: Value := PROTECTION_LEVEL_WINTCB_LIGHT;
    7: Value := PROTECTION_LEVEL_AUTHENTICODE;
    8: Value := PROTECTION_LEVEL_WINDOWS;
    9: Value := PROTECTION_LEVEL_WINTCB;
  else
    Result := False;
  end;
end;

procedure TDialogRun.LinkLabelTokenLinkClick;
begin
  if Assigned(FToken) then
    TInfoDialog.CreateFromToken(Self, FToken);
end;

procedure TDialogRun.MenuClearParentClick;
begin
  ParentProcessId := 0;
  hxParentProcess := nil;
  EditParent.Text := '<not specified>';
end;

procedure TDialogRun.MenuCmdClick;
begin
  EditExe.Text := GetEnvironmentVariable('ComSpec');
  ButtonRun.SetFocus;
end;

procedure TDialogRun.MenuSelfClick;
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
  begin
    CaptionSubscription := Value.ObserveString(tsCaption, OnCaptionChange);
    AppContainerField.TrySetUserFromToken(Value.Handle);
  end;
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
  Desktops := UsrxEnumerateAllDesktops;
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
  SupportedOptions: TSupportedCreateParameters;
begin
  SupportedOptions := TuPsMethodSupports(Method);

  EditDir.Enabled := spoCurrentDirectory in SupportedOptions;
  ComboBoxDesktop.Enabled := spoDesktop in SupportedOptions;
  ComboBoxLogonFlags.Enabled := spoLogonFlags in SupportedOptions;
  CheckBoxInherit.Enabled := spoInheritHandles in SupportedOptions;
  CheckBoxSuspended.Enabled := spoSuspended in SupportedOptions;
  CheckBoxBreakaway.Enabled := spoBreakawayFromJob in SupportedOptions;
  CheckBoxForceBreakaway.Enabled := spoForceBreakaway in SupportedOptions;
  CheckBoxInheritConsole.Enabled := spoInheritConsole in SupportedOptions;
  CheckBoxRunas.Enabled := spoRequireElevation in SupportedOptions;
  CheckBoxIgnoreElevation.Enabled := spoIgnoreElevation in SupportedOptions;
  ComboBoxShowMode.Enabled := spoWindowMode in SupportedOptions;
  CheckBoxRunAsInvoker.Enabled := spoRunAsInvoker in SupportedOptions;
  EditParent.Enabled := [spoParentProcess, spoParentProcessId] * SupportedOptions <> [];
  ButtonChooseParent.Enabled := [spoParentProcess, spoParentProcessId] * SupportedOptions <> [];
  AppContainerField.Enabled := spoAppContainer in SupportedOptions;
  CheckBoxLPAC.Enabled := spoLPAC in SupportedOptions;
  CheckBoxChildRestricted.Enabled := spoChildPolicy in SupportedOptions;
  CheckBoxChildUnlessSecure.Enabled := spoChildPolicy in SupportedOptions;
  CheckBoxChildOverride.Enabled := spoChildPolicy in SupportedOptions;
  ComboBoxProtection.Enabled := spoProtection in SupportedOptions;
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
  EditAppId.Enabled := spoAppUserModeId in SupportedOptions;

  if spoToken in SupportedOptions then
    LinkLabelToken.Font.Style := LinkLabelToken.Font.Style - [fsStrikeOut]
  else
    LinkLabelToken.Font.Style := LinkLabelToken.Font.Style + [fsStrikeOut];
end;

end.
