unit UI.Run;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  TU.Tokens;

type
  TRunDialog = class(TForm)
    ButtonCancel: TButton;
    GroupBoxAppName: TGroupBox;
    CheckBoxAppName: TCheckBox;
    EditAppName: TEdit;
    ButtonBrowseAppName: TButton;
    GroupBoxCmd: TGroupBox;
    EditCmd: TEdit;
    ButtonBrowseCmd: TButton;
    CheckBoxCmd: TCheckBox;
    GroupBoxFlags: TGroupBox;
    GroupBoxDirectory: TGroupBox;
    EditDirectory: TEdit;
    ButtonBrowseDirectory: TButton;
    CheckBoxDirectory: TCheckBox;
    CheckBoxInherit: TCheckBox;
    CheckBoxSuspended: TCheckBox;
    CheckBoxNewConsole: TCheckBox;
    CheckBoxBreakaway: TCheckBox;
    GroupBoxDesktop: TGroupBox;
    ComboBoxDesktop: TComboBox;
    ButtonAsUser: TButton;
    ButtonWithToken: TButton;
    RadioButtonLogonZero: TRadioButton;
    RadioButtonLogonProfile: TRadioButton;
    RadioButtonLogonNet: TRadioButton;
    GroupBoxLogon: TGroupBox;
    OpenDialog1: TOpenDialog;
    procedure CheckBoxAppNameClick(Sender: TObject);
    procedure CheckBoxCmdClick(Sender: TObject);
    procedure CheckBoxDirectoryClick(Sender: TObject);
    class procedure Execute(AOwner: TComponent; Token: TToken);
    procedure ButtonAsUserClick(Sender: TObject);
    procedure ButtonWithTokenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    constructor Create(AOwner: TComponent; Token: TToken); reintroduce;
  private
    Token: TToken;
    function GetAppName: PWideChar;
    function GetCmd: PWideChar;
    function GetDir: PWideChar;
    function GetStartupInfo: TStartupInfoW;
    function GetFlags: Cardinal;
    function GetLogonFlags: Cardinal;
  end;

var
  RunDialog: TRunDialog;

implementation

uses
  UI.ProcessList;

{$R *.dfm}

const
  SHACF_FILESYS_ONLY = $10;
  SHACF_FILESYS_DIRS = $20;

function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HRESULT; stdcall;
  external 'shlwapi.dll';

function CreateProcessWithTokenW(hToken: THandle; dwLogonFlags: Cardinal;
  lpApplicationName: PWideChar; lpCommandLine: PWideChar;
  dwCreationFlags: Cardinal; lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfoW;
  out lpProcessInfo: TProcessInformation): LongBool; stdcall;
  external advapi32;

procedure TRunDialog.ButtonAsUserClick(Sender: TObject);
var
  PI: TProcessInformation;
begin
  FillChar(PI, SizeOf(PI), 0);
  Win32Check(CreateProcessAsUser(Token.Handle, GetAppName, GetCmd, nil, nil,
    CheckBoxInherit.Checked, GetFlags, nil, GetDir, GetStartupInfo, PI));

  CloseHandle(PI.hProcess);
  CloseHandle(PI.hThread);
end;

procedure TRunDialog.ButtonWithTokenClick(Sender: TObject);
var
  PI: TProcessInformation;
begin
  FillChar(PI, SizeOf(PI), 0);
  Win32Check(CreateProcessWithTokenW(Token.Handle, GetLogonFlags, GetAppName,
    GetCmd, GetFlags, nil, GetDir, GetStartupInfo, PI));

  CloseHandle(PI.hProcess);
  CloseHandle(PI.hThread);
end;

procedure TRunDialog.CheckBoxAppNameClick(Sender: TObject);
begin
  EditAppName.Enabled := CheckBoxAppName.Checked;
  ButtonBrowseAppName.Enabled := CheckBoxAppName.Checked;
end;

procedure TRunDialog.CheckBoxCmdClick(Sender: TObject);
begin
  EditCmd.Enabled := CheckBoxCmd.Checked;
  ButtonBrowseCmd.Enabled := CheckBoxCmd.Checked;
end;

procedure TRunDialog.CheckBoxDirectoryClick(Sender: TObject);
begin
  EditDirectory.Enabled := CheckBoxDirectory.Checked;
  ButtonBrowseDirectory.Enabled := CheckBoxDirectory.Checked;
end;

constructor TRunDialog.Create(AOwner: TComponent; Token: TToken);
begin
  inherited Create(AOwner);
  Self.Token := Token;
end;

class procedure TRunDialog.Execute(AOwner: TComponent; Token: TToken);
begin
  with TRunDialog.Create(AOwner, Token) do
  begin
    ShowModal;
  end;
end;

procedure TRunDialog.FormCreate(Sender: TObject);
begin
  SHAutoComplete(EditAppName.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditCmd.Handle, SHACF_FILESYS_ONLY);
  SHAutoComplete(EditDirectory.Handle, SHACF_FILESYS_DIRS);
end;

function TRunDialog.GetAppName: PWideChar;
begin
  if CheckBoxAppName.Checked then
    Result := PWideChar(EditAppName.Text)
  else
    Result := nil;
end;

function TRunDialog.GetCmd: PWideChar;
begin
  if CheckBoxCmd.Checked then
    Result := PWideChar(EditCmd.Text)
  else
    Result := nil;
end;

function TRunDialog.GetDir: PWideChar;
begin
  if CheckBoxDirectory.Checked then
    Result := PWideChar(EditDirectory.Text)
  else
    Result := nil;
end;

function TRunDialog.GetFlags: Cardinal;
const
  CREATE_BREAKAWAY_FROM_JOB = $01000000;
begin
  Result := 0;
  if CheckBoxSuspended.Checked then
    Result := Result or CREATE_SUSPENDED;
  if CheckBoxNewConsole.Checked then
    Result := Result or CREATE_NEW_CONSOLE;
  if CheckBoxBreakaway.Checked then
    Result := Result or CREATE_BREAKAWAY_FROM_JOB;
end;

function TRunDialog.GetLogonFlags: Cardinal;
const
  LOGON_WITH_PROFILE = $00000001;
  LOGON_NETCREDENTIALS_ONLY = $00000002;
begin
  if RadioButtonLogonProfile.Checked then
    Result := LOGON_WITH_PROFILE
  else if RadioButtonLogonNet.Checked then
    Result := LOGON_NETCREDENTIALS_ONLY
  else
    Result := 0;
end;

function TRunDialog.GetStartupInfo: TStartupInfoW;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cb := SizeOf(Result);
  Result.lpDesktop := PWideChar(ComboBoxDesktop.Text);
end;

// TODO: Also add Cmd and Self buttons for app name
// TODO: CreateProcessWithToken ignores the session in the token

end.
