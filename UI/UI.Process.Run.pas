unit UI.Process.Run;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, UI.Prototypes.ChildForm,
  Vcl.ExtCtrls, Vcl.Menus, NtUtils.Exec, TU.Tokens;

type
  TDialogRun = class(TChildTaskbarForm, IExecProvider)
    PageControl: TPageControl;
    TabMethod: TTabSheet;
    TabEnv: TTabSheet;
    TabParent: TTabSheet;
    RadioButtonRtl: TRadioButton;
    LabelOther: TLabel;
    RadioButtonShell: TRadioButton;
    RadioButtonWdc: TRadioButton;
    RadioButtonWmi: TRadioButton;
    RadioButtonAsUser: TRadioButton;
    RadioButtonWithToken: TRadioButton;
    LabelCred: TLabel;
    RadioButtonWithLogon: TRadioButton;
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
  private
    // IExecProvider implementation
    function Provides(Parameter: TExecParam): Boolean;
    function Application: String;
    function Parameters: String;
    function CurrentDircetory: String;
    function Desktop: String;
    function Token: THandle;
    function ParentProcess: THandle;
    function LogonFlags: Cardinal;
    function InheritHandles: Boolean;
    function CreateSuspended: Boolean;
    function Breakaway: Boolean;
    function RequireElevation: Boolean;
    function ShowWindowMode: Word;
  private
    ExecMethod: IExecMethod;
    FToken: TToken;
    hParentProcess: THandle;
    procedure UpdateEnabledState;
    procedure OnCaptionChange(NewCaption: String);
    procedure SetToken(const Value: TToken);
    procedure UpdateDesktopList;
  public
    property UseToken: TToken read FToken write SetToken;
  end;

implementation

uses
  Winapi.Shlwapi, NtUtils.Exec.Win32, NtUtils.Exec.Shell, NtUtils.Exec.Wdc,
  NtUtils.Exec.Wmi, NtUtils.Exec.Nt, UI.Information, UI.ProcessList,
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntpsapi, NtUtils.Exceptions,
  NtUtils.Objects, NtUtils.WinUser, NtUtils.Processes;

{$R *.dfm}

function TDialogRun.Application: String;
begin
  Result := EditExe.Text;
end;

function TDialogRun.Breakaway: Boolean;
begin
  Result := CheckBoxBreakaway.Checked;
end;

procedure TDialogRun.ButtonBrowseClick(Sender: TObject);
begin
  if OpenDlg.Execute(Handle) then
    EditExe.Text := OpenDlg.FileName;
end;

procedure TDialogRun.ButtonChooseParentClick(Sender: TObject);
var
  ClientIdEx: TClientIdEx;
begin
  MenuClearParentClick(Sender);

  ClientIdEx := TProcessListDialog.Execute(Self, False);
  NtxOpenProcess(hParentProcess, PROCESS_DUP_HANDLE,
    ClientIdEx.ProcessID).RaiseOnError;

  EditParent.Text := Format('%s [%d]', [ClientIdEx.ImageName,
    ClientIdEx.ProcessID]);
end;

procedure TDialogRun.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogRun.ButtonRunClick(Sender: TObject);
begin
  if Assigned(ExecMethod) then
    ExecMethod.Execute(Self)
  else
    raise Exception.Create('No exec method available');
end;

procedure TDialogRun.ChangedExecMethod(Sender: TObject);
begin
  if Sender = RadioButtonAsUser then
    ExecMethod := TExecCreateProcessAsUser.Create
  else if Sender = RadioButtonWithToken then
    ExecMethod := TExecCreateProcessWithToken.Create
  else if Sender = RadioButtonRtl then
    ExecMethod := TExecRtlCreateUserProcess.Create
  else if Sender = RadioButtonShell then
    ExecMethod := TExecShellExecute.Create
  else if Sender = RadioButtonWdc then
    ExecMethod := TExecCallWdc.Create
  else if Sender = RadioButtonWMI then
    ExecMethod := TExecCallWmi.Create
  else
    ExecMethod := nil;
  UpdateEnabledState;
end;

function TDialogRun.CreateSuspended: Boolean;
begin
  Result := CheckBoxSuspended.Checked;
end;

function TDialogRun.CurrentDircetory: String;
begin
  Result := EditDir.Text;
end;

function TDialogRun.Desktop: String;
begin
  Result := ComboBoxDesktop.Text;
end;

procedure TDialogRun.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UseToken := nil;
  MenuClearParentClick(Sender);
end;

procedure TDialogRun.FormCreate(Sender: TObject);
begin
  MenuCmdClick(Sender);
  SHAutoComplete(EditExe.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditDir.Handle, SHACF_FILESYS_DIRS);
  ChangedExecMethod(RadioButtonAsUser);
  UpdateDesktopList;
end;

function TDialogRun.InheritHandles: Boolean;
begin
  Result := CheckBoxInherit.Checked;
end;

procedure TDialogRun.LinkLabelTokenLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  if Assigned(FToken) then
    TInfoDialog.CreateFromToken(Self, FToken);
end;

function TDialogRun.LogonFlags: Cardinal;
begin
  // 0, LOGON_WITH_PROFILE, LOGON_NETCREDENTIALS_ONLY
  Result := ComboBoxLogonFlags.ItemIndex;
end;

procedure TDialogRun.MenuClearParentClick(Sender: TObject);
begin
  if hParentProcess <> 0 then
  begin
    NtxSafeClose(hParentProcess);
    hParentProcess := 0;
    EditParent.Text := '<not specified>';
  end;
end;

procedure TDialogRun.MenuCmdClick(Sender: TObject);
begin
  EditExe.Text := GetEnvironmentVariable('ComSpec');
end;

procedure TDialogRun.MenuSelfClick(Sender: TObject);
begin
  EditExe.Text := ParamStr(0);
end;

procedure TDialogRun.OnCaptionChange(NewCaption: String);
begin
  LinkLabelToken.Caption := 'Using token: <a>' + NewCaption + '</a>';
end;

function TDialogRun.Parameters: String;
begin
  Result := EditParams.Text;
end;

function TDialogRun.ParentProcess: THandle;
begin
  Result := hParentProcess;
end;

function TDialogRun.Provides(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppDesktop, ppLogonFlags, ppInheritHandles, ppCreateSuspended, ppBreakaway,
    ppRequireElevation, ppShowWindowMode:
      Result := True;

    ppParameters:
      Result := EditParams.Text <> '';

    ppCurrentDirectory:
      Result := EditDir.Text <> '';

    ppToken:
      Result := Assigned(FToken);

    ppParentProcess:
      Result := hParentProcess <> 0;
  else
    Result := False;
  end;
end;

function TDialogRun.RequireElevation: Boolean;
begin
  Result := CheckBoxRunas.Checked;
end;

procedure TDialogRun.SetToken(const Value: TToken);
begin
  if Assigned(FToken) then
  begin
    FToken.OnCaptionChange.Unsubscribe(OnCaptionChange);
    UnsubscribeTokenCanClose(FToken);
  end;

  FToken := Value;

  if not Assigned(Value) then
    LinkLabelToken.Caption := 'Using token: <not specified>'
  else
  begin
    SubscribeTokenCanClose(Value, 'Run dialod');
    Value.OnCaptionChange.Subscribe(OnCaptionChange);
    OnCaptionChange(Value.Caption);
  end;
end;

function TDialogRun.ShowWindowMode: Word;
begin
  // SW_HIDE..SW_SHOWMAXIMIZED
  Result := ComboBoxShowMode.ItemIndex;
end;

function TDialogRun.Token: THandle;
begin
  if Assigned(FToken) then
    Result := FToken.Handle
  else
    Result := 0;
end;

procedure TDialogRun.UpdateDesktopList;
var
  Desktops: TStringArray;
  Current: string;
  Found: Boolean;
  i: Integer;
begin
  Desktops := UsrxEnumAllDesktops;
  Current := UsrxCurrentDesktopName;

  with ComboBoxDesktop do
  begin
    Items.BeginUpdate;
    Items.Clear;

    Found := False;

    // Add all enumerated desktops
    for i := 0 to High(Desktops) do
    begin
      Items.Add(Desktops[i]);

      if Desktops[i] = Current then
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
begin
  EditParams.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppParameters);

  EditDir.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppCurrentDirectory);

  ComboBoxDesktop.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppDesktop);

  ComboBoxLogonFlags.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppLogonFlags);

  CheckBoxInherit.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppInheritHandles);

  CheckBoxSuspended.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppCreateSuspended);

  CheckBoxBreakaway.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppBreakaway);

  CheckBoxRunas.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppRequireElevation);

  ComboBoxShowMode.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppShowWindowMode);

  EditParent.Enabled := Assigned(ExecMethod) and
    ExecMethod.Supports(ppParentProcess);
  ButtonChooseParent.Enabled := EditParent.Enabled;
end;

end.
