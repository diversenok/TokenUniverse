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
    RadioButtonUsual: TRadioButton;
    RadioButtonRtl: TRadioButton;
    LabelOther: TLabel;
    RadioButtonShell: TRadioButton;
    RadioButtonWdc: TRadioButton;
    RadioButtonWMI: TRadioButton;
    LabelToken: TLabel;
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
    LabelUsual: TLabel;
    ButtonClose: TButton;
    ButtonRun: TButton;
    LabelDesktop: TLabel;
    LabelLogonFlags: TLabel;
    PopupMenuExe: TPopupMenu;
    MenuCmd: TMenuItem;
    MenuSelf: TMenuItem;
    CheckBoxRunas: TCheckBox;
    RadioButtonWmiImp: TRadioButton;
    OpenDlg: TOpenDialog;
    LabelShowMode: TLabel;
    ComboBoxShowMode: TComboBox;
    LinkLabelToken: TLinkLabel;
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
  private
    // IExecProvider implementation
    function Provides(Parameter: TExecParam): Boolean;
    function Application: String;
    function Parameters: String;
    function CurrentDircetory: String;
    function Desktop: String;
    function Token: THandle;
    function LogonFlags: Cardinal;
    function InheritHandles: Boolean;
    function CreateSuspended: Boolean;
    function Breakaway: Boolean;
    function RequireElevation: Boolean;
    function ShowWindowMode: Word;
  private
    ExecMethod: IExecMethod;
    FToken: TToken;
    procedure UpdateEnabledState;
    procedure OnCaptionChange(NewCaption: String);
    procedure SetToken(const Value: TToken);
  public
    property UseToken: TToken read FToken write SetToken;
  end;

implementation

uses
  Winapi.Shlwapi, NtUtils.Exec.Win32, NtUtils.Exec.Shell, NtUtils.Exec.Wdc,
  NtUtils.Exec.Wmi, UI.Information;

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
  if Sender = RadioButtonUsual then
    ExecMethod := TExecCreateProcess.Create
  else if Sender = RadioButtonAsUser then
    ExecMethod := TExecCreateProcessAsUser.Create
  else if Sender = RadioButtonWithToken then
    ExecMethod := TExecCreateProcessWithToken.Create
  else if Sender = RadioButtonWmiImp then
    ExecMethod := TExecCallWmiImpersonated.Create
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
end;

procedure TDialogRun.FormCreate(Sender: TObject);
begin
  MenuCmdClick(Sender);
  SHAutoComplete(EditExe.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditDir.Handle, SHACF_FILESYS_DIRS);
  ChangedExecMethod(RadioButtonUsual);
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
  LinkLabelToken.Caption := 'Token: <a>' + NewCaption + '</a>';
end;

function TDialogRun.Parameters: String;
begin
  Result := EditParams.Text;
end;

function TDialogRun.Provides(Parameter: TExecParam): Boolean;
begin
  // TODO: Token
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
    LinkLabelToken.Caption := 'Token: None'
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

procedure TDialogRun.UpdateEnabledState;
begin
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
end;

end.
