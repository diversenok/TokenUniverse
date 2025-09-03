unit UI.Modal.Logon;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, NtUiCommon.Forms, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes, UI.Prototypes.Groups, Ntapi.NtSecApi,
  Vcl.ExtCtrls, NtUtils;

type
  TLogonDialog = class(TChildForm)
    cbxLogonType: TComboBox;
    lblLogonType: TLabel;
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
    GroupsPanel: TPanel;
    GroupsFrame: TFrameGroups;
    lblAuthPackage: TLabel;
    cbxAuthPackage: TComboBox;
    lblMessageType: TLabel;
    cbxMessageType: TComboBox;
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAllocLuidClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    function GetAuthPackage: AnsiString;
    function GetMessageType: TLogonSubmitType;
    function GetLogonType: TSecurityLogonType;
    procedure EditSingleGroup(const Value: TGroup);
    procedure SuggestCurrentLogonGroup;
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.ntseapi, Ntapi.ntrtl, Ntapi.ntexapi, Ntapi.ntpsapi,
  Ntapi.wincred, NtUiLib.WinCred, Ntapi.WinError, UI.MainForm,
  UI.Modal.PickUser, NtUtils.Security.Sid, Ntapi.WinUser,
  NtUtils.WinUser, NtUtils.Errors, System.UITypes, NtUiLib.Errors,
  DelphiUiLib.Strings, DelphiUiLib.Reflection.Strings, NtUiCommon.Exceptions,
  TU.Tokens, TU.Tokens.Open, UI.Settings;

{$R *.dfm}

function IsLogonSid(Sid: ISid): Boolean;
begin
  Result := (RtlxIdentifierAuthoritySid(Sid) = SECURITY_NT_AUTHORITY)
    and (RtlSubAuthorityCountSid(Sid.Data)^ = SECURITY_LOGON_IDS_RID_COUNT) and
    (RtlSubAuthoritySid(Sid.Data, 0)^ = SECURITY_LOGON_IDS_RID);
end;

procedure TLogonDialog.ButtonAddSIDClick;
begin
  try
    if GroupsFrame.VST.RootNodeCount = 0 then
      SuggestCurrentLogonGroup;
  except
    on E: Exception do
      ReportException(E);
  end;

  GroupsFrame.Add([TDialogPickUser.PickNew(Self)]);
  ButtonContinue.SetFocus;
end;

procedure TLogonDialog.ButtonAllocLuidClick;
var
  NewLuid: TLuid;
begin
  if NtAllocateLocallyUniqueId(NewLuid).IsSuccess then
    EditSourceLuid.Text := UiLibUIntToHex(NewLuid);
end;

procedure TLogonDialog.ButtonCancelClick;
begin
  Close;
end;

procedure TLogonDialog.ButtonContinueClick;
const
  DIALOG_CAPTION = 'Token Universe';
var
  Status: TNtxStatus;
  Credentials: TLogonCredentials;
  Source: TTokenSource;
  Token: IToken;
begin
  if GetMessageType in [TLogonSubmitType.S4ULogon,
    TLogonSubmitType.VirtualLogon] then
    // No need to use the secure desktop just for usernames
    Status := CredxPromptForWindowsCredentials(Handle, DIALOG_CAPTION,
      'Note: password is not required', Credentials, 0, 0, GetAuthPackage)
  else
    // Prefer the secure desktop but allow fallback
    Status := CredxPromptForWindowsCredentialsPreferSecure(Handle,
      DIALOG_CAPTION, 'Logon a user:', Credentials, 0, 0, GetAuthPackage);

  if Status.Win32Error = ERROR_CANCELLED then
    Abort;

  Status.RaiseOnError;
  Source.Name := EditSourceName.Text;
  Source.SourceIdentifier := UiLibStringToUInt64RaiseOnError(
    EditSourceLuid.Text, 'Source LUID');

  MakeLogonToken(Token, GetMessageType, GetLogonType, Credentials, Source,
  GroupsFrame.All, GetAuthPackage).RaiseOnError;

  FormMain.TokenView.Add(Token);

  // TODO: no-close setting
  ModalResult := mrOk;
  Close;
end;

procedure TLogonDialog.EditSingleGroup;
begin
  GroupsFrame.EditSelectedGroup(
    procedure (var Group: TGroup)
    begin
      Group := TDialogPickUser.PickEditOne(Self, Group);
    end
  );
end;

procedure TLogonDialog.FormCreate;
begin
  ButtonAllocLuidClick(Sender);
  GroupsFrame.OnDefaultAction := EditSingleGroup;
end;

function TLogonDialog.GetAuthPackage;
begin
  case cbxAuthPackage.ItemIndex of
    1: Result := MSV1_0_PACKAGE_NAME;
    2: Result := MICROSOFT_KERBEROS_NAME_A;
  else
    Result := NEGOSSP_NAME_A;
  end;
end;

function TLogonDialog.GetLogonType;
const
  LOGON_TYPES: array [0..11] of TSecurityLogonType = (
    TSecurityLogonType.Interactive, TSecurityLogonType.Network,
    TSecurityLogonType.Batch, TSecurityLogonType.Service,
    TSecurityLogonType.Proxy, TSecurityLogonType.Unlock,
    TSecurityLogonType.NetworkCleartext, TSecurityLogonType.NewCredentials,
    TSecurityLogonType.RemoteInteractive, TSecurityLogonType.CachedInteractive,
    TSecurityLogonType.CachedRemoteInteractive, TSecurityLogonType.CachedUnlock
  );
begin
  if (cbxLogonType.ItemIndex < 0) or
    (cbxLogonType.ItemIndex > High(LOGON_TYPES)) then
    Abort;

  Result := LOGON_TYPES[cbxLogonType.ItemIndex];
end;

function TLogonDialog.GetMessageType;
const
  MESSAGE_TYPES: array [0..3] of TLogonSubmitType = (
    TLogonSubmitType.InteractiveLogon, TLogonSubmitType.S4ULogon,
    TLogonSubmitType.VirtualLogon, TLogonSubmitType.NoElevationLogon
  );
begin
  if (cbxMessageType.ItemIndex < 0) or
    (cbxMessageType.ItemIndex > High(MESSAGE_TYPES)) then
    Abort;

  Result := MESSAGE_TYPES[cbxMessageType.ItemIndex];
end;

procedure TLogonDialog.MenuEditClick;
begin
  // Single edit
  if GroupsFrame.VST.SelectedCount = 1 then
    EditSingleGroup(Default(TGroup))

  // Multiple edit
  else if GroupsFrame.VST.SelectedCount > 1 then
    GroupsFrame.EditSelectedGroups(
      procedure (
        const Groups: TArray<TGroup>;
        var AttributesToClear: TGroupAttributes;
        var AttributesToSet: TGroupAttributes
      )
      begin
        TDialogPickUser.PickEditMultiple(Self, Groups, AttributesToSet,
          AttributesToClear);
      end
    );
end;

procedure TLogonDialog.MenuRemoveClick;
begin
  GroupsFrame.VST.DeleteSelectedNodes;
end;

procedure TLogonDialog.SuggestCurrentLogonGroup;
const
  TITLE = 'Add current logon SID?';
  MSG = 'Adding groups during logon often requires explicitly specifying the ' +
    'logon SID for the new token. Do you want to copy it from the current ' +
    'desktop? This operation will also allow using the token for starting ' +
    'interactive processes.';
var
  Group: TGroup;
begin
  case TaskMessageDlg(TITLE, MSG, mtConfirmation, [mbYes, mbIgnore, mbCancel],
    -1) of
    IDYES:
    begin
      UsrxQuerySid(UsrxCurrentDesktop, Group.Sid).RaiseOnError;

      if not Assigned(Group.Sid) then
        raise Exception.Create('The current desktop does not have a logon SID.');

      Group.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or
        SE_GROUP_ENABLED or SE_GROUP_LOGON_ID;

      GroupsFrame.Add([Group]);
    end;
  end;
end;

end.
