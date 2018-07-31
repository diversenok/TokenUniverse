unit UI.Logon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TLogonDialog = class(TForm)
    ComboLogonType: TComboBox;
    ComboLogonProvider: TComboBox;
    LabelType: TLabel;
    LabelProvider: TLabel;
    ButtonCancel: TButton;
    ButtonContinue: TButton;
    procedure ButtonContinueClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure TokenCreationCallback(Domain, User: String; Password: PWideChar);
    function GetLogonType: Cardinal;
    function GetLogonProvider: Cardinal;
  public
    { Public declarations }
  end;

implementation

uses
  TU.Common, TU.Credentials, TU.Tokens, UI.MainForm;

{$R *.dfm}

procedure TLogonDialog.ButtonContinueClick(Sender: TObject);
begin
  PromptCredentialsUI(Handle, TokenCreationCallback);
  Close;
end;

procedure TLogonDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TLogonDialog.GetLogonProvider: Cardinal;
const
  LogonProvider: array [0 .. 3] of Cardinal = (LOGON32_PROVIDER_DEFAULT,
    LOGON32_PROVIDER_WINNT50, LOGON32_PROVIDER_WINNT40,
    LOGON32_PROVIDER_WINNT35);
begin
  Result := LogonProvider[ComboLogonProvider.ItemIndex];
end;

function TLogonDialog.GetLogonType: Cardinal;
const
  LOGON32_LOGON_NETWORK_CLEARTEXT = 8;
  LOGON32_LOGON_NEW_CREDENTIALS = 9;
  LogonType: array [0 .. 5] of Cardinal = (LOGON32_LOGON_INTERACTIVE,
    LOGON32_LOGON_BATCH, LOGON32_LOGON_NETWORK,
    LOGON32_LOGON_NETWORK_CLEARTEXT, LOGON32_LOGON_NEW_CREDENTIALS,
    LOGON32_LOGON_SERVICE);
begin
  Result := LogonType[ComboLogonType.ItemIndex];
end;

procedure TLogonDialog.TokenCreationCallback(Domain, User: String;
  Password: PWideChar);
begin
  FormMain.Frame.AddToken(TToken.CreateWithLogon(GetLogonType, GetLogonProvider,
    Domain, User, Password));
end;

end.
