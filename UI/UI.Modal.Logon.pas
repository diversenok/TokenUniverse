unit UI.Modal.Logon;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.ChildForm, Vcl.ComCtrls,
  UI.ListViewEx, UI.Prototypes, TU.LsaApi, UI.Prototypes.Groups,
  Winapi.WinBase, Winapi.NtSecApi;

type
  TLogonDialog = class(TChildForm)
    ComboLogonType: TComboBox;
    ComboLogonProvider: TComboBox;
    LabelType: TLabel;
    LabelProvider: TLabel;
    ButtonCancel: TButton;
    ButtonContinue: TButton;
    ButtonAddSID: TButton;
    LabelGroups: TLabel;
    PopupMenu: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    GroupBoxSource: TGroupBox;
    EditSourceName: TEdit;
    StaticSourceName: TStaticText;
    StaticSourceLuid: TStaticText;
    EditSourceLuid: TEdit;
    ButtonAllocLuid: TButton;
    FrameGroups: TFrameGroups;
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboLogonProviderChange(Sender: TObject);
    procedure ButtonAllocLuidClick(Sender: TObject);
  private
    procedure TokenCreationCallback(Domain, User: String; Password: PWideChar);
    function GetLogonType: TSecurityLogonType;
    function GetLogonProvider: TLogonProvider;
  end;

implementation

uses
  TU.Credentials, TU.Tokens, UI.MainForm, UI.Modal.PickUser,
  TU.Tokens.Types, DelphiUtils.Strings,
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi;

{$R *.dfm}

const
  S4U_INDEX = 5; // Make sure to be consisten with the combobox

procedure TLogonDialog.ButtonAddSIDClick(Sender: TObject);
begin
  FrameGroups.AddGroup(TDialogPickUser.PickNew(Self));
  ButtonContinue.SetFocus;
end;

procedure TLogonDialog.ButtonAllocLuidClick(Sender: TObject);
var
  NewLuid: TLuid;
begin
  if NT_SUCCESS(NtAllocateLocallyUniqueId(NewLuid)) then
    EditSourceLuid.Text := IntToHexEx(NewLuid);
end;

procedure TLogonDialog.ButtonContinueClick(Sender: TObject);
begin
  Enabled := False;
  try
    PromptCredentialsUI(Handle, TokenCreationCallback,
      ComboLogonProvider.ItemIndex = S4U_INDEX);
  finally
    Enabled := True;
  end;
  ModalResult := mrOk;
end;

procedure TLogonDialog.ComboLogonProviderChange(Sender: TObject);
begin
  EditSourceName.Enabled := (ComboLogonProvider.ItemIndex = S4U_INDEX);
  EditSourceLuid.Enabled := EditSourceName.Enabled;
  ButtonAllocLuid.Enabled := EditSourceName.Enabled;
end;

procedure TLogonDialog.FormCreate(Sender: TObject);
begin
  ButtonAllocLuidClick(Sender);
end;

function TLogonDialog.GetLogonProvider: TLogonProvider;
begin
  Result := TLogonProvider(ComboLogonProvider.ItemIndex);
end;

function TLogonDialog.GetLogonType: TSecurityLogonType;
const
  LogonTypeMapping: array [0 .. 6] of TSecurityLogonType = (ltInteractive,
    ltNetwork, ltNetworkCleartext, ltNewCredentials, ltUnlock, ltBatch,
    ltService);
begin
  Result := LogonTypeMapping[ComboLogonType.ItemIndex];
end;

procedure TLogonDialog.MenuEditClick(Sender: TObject);
begin
  FrameGroups.UiEditSelected(Self);
end;

procedure TLogonDialog.MenuRemoveClick(Sender: TObject);
begin
  if Assigned(FrameGroups.ListView.Selected) then
    FrameGroups.RemoveGroup(FrameGroups.ListView.Selected.Index);
end;

procedure TLogonDialog.TokenCreationCallback(Domain, User: String;
  Password: PWideChar);
var
  Source: TTokenSource;
begin
  if ComboLogonProvider.ItemIndex = S4U_INDEX then
  begin
    // Use Services 4 Users logon
    Source := CreateTokenSource(EditSourceName.Text, StrToUInt64Ex(
      EditSourceLuid.Text, 'Source LUID'));

    FormMain.TokenView.Add(TToken.CreateS4ULogon(GetLogonType, Domain, User,
      Source, FrameGroups.Groups));
  end
  else
    FormMain.TokenView.Add(TToken.CreateWithLogon(GetLogonType,
      GetLogonProvider, Domain, User, Password, FrameGroups.Groups));
end;

end.
