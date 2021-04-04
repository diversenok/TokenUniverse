unit UI.Modal.Logon;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.Forms, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes, UI.Prototypes.Groups,
  Winapi.WinBase, Winapi.NtSecApi, Vcl.ExtCtrls;

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
    GroupsFrame: TGroupsFrame;
    GroupsPanel: TPanel;
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
  NtUiLib.Exceptions, DelphiUiLib.Strings, DelphiUiLib.Reflection.Strings;

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
  GroupsFrame.Add([TDialogPickUser.PickNew(Self)]);
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
  if GroupsFrame.ListViewEx.Items.Count > 0 then
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
var
  AddAttributes, RemoveAttriutes: TGroupAttributes;
begin
  with GroupsFrame.ListViewEx do
  begin
    // Edit one group: SID and attributes
    if (SelCount = 1) and Assigned(Selected) then
      GroupsFrame[Selected.Index] := TDialogPickUser.PickEditOne(Self,
        GroupsFrame[Selected.Index]);

    // Edit multiple groups: only attributes
    if SelCount > 1 then
    begin
      TDialogPickUser.PickEditMultiple(Self, GroupsFrame.Selected, AddAttributes,
        RemoveAttriutes);

      GroupsFrame.UpdateSelected(AddAttributes, RemoveAttriutes);
    end;
  end;
end;

procedure TLogonDialog.MenuRemoveClick(Sender: TObject);
begin
  GroupsFrame.RemoveSelected;
end;

procedure TLogonDialog.SuggestCurrentLogonGroup;
const
  TITLE = 'Add current logon group?';
  MSG = 'Do you also want to add a logon group that allows full access to ' +
        'the current window station? Note, that when providing additional ' +
        'groups, at least one of them must be a logon group.';
var
  Group: TGroup;
begin
  // Check for existing logon groups
  for Group in GroupsFrame.All do
    if IsLogonSid(Group.Sid) then
      Exit;

  case TaskMessageDlg(TITLE, MSG, mtConfirmation, [mbYes, mbIgnore, mbCancel],
    -1) of
    IDYES:
    begin
      // Query window station SID
      UsrxQuerySid(GetProcessWindowStation, Group.Sid).RaiseOnError;

      Group.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or
        SE_GROUP_ENABLED or SE_GROUP_LOGON_ID;

      // Add it
      GroupsFrame.Add([Group]);
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
    Source.Name := EditSourceName.Text;
    Source.SourceIdentifier := StrToUInt64Ex(EditSourceLuid.Text,
      'Source LUID');

    FormMain.TokenView.Add(TToken.CreateS4ULogon(Domain, User, Source,
      GroupsFrame.All));
  end
  else
    FormMain.TokenView.Add(TToken.CreateWithLogon(GetLogonType, Domain, User,
      Password, GroupsFrame.All));
end;

end.
