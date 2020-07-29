unit UI.Modal.Logon;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.ChildForm, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes, UI.Prototypes.Groups,
  Winapi.WinBase, Winapi.NtSecApi;

type
  TLogonDialog = class(TChildForm)
    ComboLogonType: TComboBox;
    LabelType: TLabel;
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
    procedure ButtonAllocLuidClick(Sender: TObject);
    procedure ComboLogonTypeChange(Sender: TObject);
  private
    procedure TokenCreationCallback(Domain, User: String; Password: PWideChar);
    function GetLogonType: TSecurityLogonType;
    procedure SuggestCurrentLogonGroup;
  end;

implementation

uses
  TU.Credentials, TU.Tokens, UI.MainForm, UI.Modal.PickUser, NtUtils,
  Winapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi, Ntapi.ntseapi, Ntapi.ntrtl,
  NtUtils.Security.Sid, Winapi.WinUser, NtUtils.WinUser, System.UITypes,
  NtUiLib.Exceptions, DelphiUiLib.Strings;

{$R *.dfm}

const
  S4U_INDEX = 0; // Make sure to be consisten with the combobox

function IsLogonSid(Sid: ISid): Boolean;
begin
  Result := (RtlIdentifierAuthoritySid(Sid.Data).ToInt64 = SECURITY_NT_AUTHORITY_ID)
    and (RtlSubAuthorityCountSid(Sid.Data)^ = SECURITY_LOGON_IDS_RID_COUNT) and
    (RtlSubAuthoritySid(Sid.Data, 0)^ = SECURITY_LOGON_IDS_RID);
end;

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
  // When logging users with additional groups at least one of them shoud be a
  // logon group
  if FrameGroups.Count > 0 then
    SuggestCurrentLogonGroup;

  Enabled := False;
  try
    PromptCredentialsUI(Handle, TokenCreationCallback,
      ComboLogonType.ItemIndex = S4U_INDEX);
  finally
    Enabled := True;
  end;
  ModalResult := mrOk;
end;

procedure TLogonDialog.ComboLogonTypeChange(Sender: TObject);
begin
  EditSourceName.Enabled := (ComboLogonType.ItemIndex = S4U_INDEX);
  EditSourceLuid.Enabled := EditSourceName.Enabled;
  ButtonAllocLuid.Enabled := EditSourceName.Enabled;
end;

procedure TLogonDialog.FormCreate(Sender: TObject);
begin
  ButtonAllocLuidClick(Sender);
end;

function TLogonDialog.GetLogonType: TSecurityLogonType;
const
  LogonTypeMapping: array [1 .. 7] of TSecurityLogonType = (
    LogonTypeInteractive, LogonTypeNetwork, LogonTypeNetworkCleartext,
    LogonTypeNewCredentials, LogonTypeUnlock, LogonTypeBatch, LogonTypeService
  );
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

procedure TLogonDialog.SuggestCurrentLogonGroup;
const
  TITLE = 'Add current logon group?';
  MSG = 'Do you also want to add a logon group that allows full access to ' +
        'the current window station? Note, that when providing additional ' +
        'groups, at least one of them must be a logon group.';
var
  i: Integer;
  LogonGroup: TGroup;
begin
  // Check for existing logon groups
  for i := 0 to FrameGroups.Count - 1 do
    if IsLogonSid(FrameGroups.Group[i].Sid) then
      Exit;

  case TaskMessageDlg(TITLE, MSG, mtConfirmation, [mbYes, mbIgnore, mbCancel],
    -1) of
    IDYES:
    begin
      // Query window station SID
      UsrxQuerySid(GetProcessWindowStation, LogonGroup.Sid).RaiseOnError;

      LogonGroup.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or
        SE_GROUP_ENABLED or SE_GROUP_LOGON_ID;

      // Add it
      FrameGroups.AddGroup(LogonGroup);
    end;

    IDCANCEL:
      Abort;
  end;
end;

procedure TLogonDialog.TokenCreationCallback(Domain, User: String;
  Password: PWideChar);
var
  Source: TTokenSource;
begin
  if ComboLogonType.ItemIndex = S4U_INDEX then
  begin
    // Use Services 4 Users logon
    Source.FromString(EditSourceName.Text);
    Source.SourceIdentifier := StrToUInt64Ex(EditSourceLuid.Text,
      'Source LUID');

    FormMain.TokenView.Add(TToken.CreateS4ULogon(Domain, User, Source,
      FrameGroups.Groups));
  end
  else
    FormMain.TokenView.Add(TToken.CreateWithLogon(GetLogonType, Domain, User,
      Password, FrameGroups.Groups));
end;

end.
