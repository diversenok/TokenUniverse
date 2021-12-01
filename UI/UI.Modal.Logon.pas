unit UI.Modal.Logon;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Menus, UI.Prototypes.Forms, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes, UI.Prototypes.Groups,
  Ntapi.WinBase, Ntapi.NtSecApi, Vcl.ExtCtrls, NtUtils;

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
    GroupsPanel: TPanel;
    GroupsFrame: TFrameGroups;
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAllocLuidClick(Sender: TObject);
    procedure ComboLogonTypeChange(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    function GetLogonType: TSecurityLogonType;
    procedure EditSingleGroup(const Value: TGroup);
    procedure SuggestCurrentLogonGroup;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  TU.Credentials, TU.Tokens, UI.MainForm, UI.Modal.PickUser,
  Ntapi.WinNt, Ntapi.ntdef, Ntapi.ntexapi, Ntapi.ntseapi, Ntapi.ntrtl,
  NtUtils.Security.Sid, Ntapi.WinUser, NtUtils.WinUser, System.UITypes,
  NtUiLib.Errors, DelphiUiLib.Strings, DelphiUiLib.Reflection.Strings;

{$R *.dfm}

const
  S4U_INDEX = 0; // Make sure to be consisten with the combobox

function IsLogonSid(Sid: ISid): Boolean;
begin
  Result := (RtlxIdentifierAuthoritySid(Sid) = SECURITY_NT_AUTHORITY)
    and (RtlSubAuthorityCountSid(Sid.Data)^ = SECURITY_LOGON_IDS_RID_COUNT) and
    (RtlSubAuthoritySid(Sid.Data, 0)^ = SECURITY_LOGON_IDS_RID);
end;

procedure TLogonDialog.ButtonAddSIDClick;
begin
  GroupsFrame.Add([TDialogPickUser.PickNew(Self)]);
  ButtonContinue.SetFocus;
end;

procedure TLogonDialog.ButtonAllocLuidClick;
var
  NewLuid: TLuid;
begin
  if NT_SUCCESS(NtAllocateLocallyUniqueId(NewLuid)) then
    EditSourceLuid.Text := IntToHexEx(NewLuid);
end;

procedure TLogonDialog.ButtonCancelClick;
begin
  Close;
end;

procedure TLogonDialog.ButtonContinueClick;
var
  Handle: THwnd;
begin
  // When logging users with additional groups at least one of them shoud be a
  // logon group
  if GroupsFrame.VST.RootNodeCount > 0 then
    SuggestCurrentLogonGroup;

  Handle := FormMain.Handle;
  Enabled := False;

  try
    PromptCredentialsUI(Handle,
      procedure (Domain, User, Password: String)
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
      end,
      ComboLogonType.ItemIndex = S4U_INDEX
    );
  finally
    Enabled := True;
  end;
  ModalResult := mrOk;
  Close;
end;

procedure TLogonDialog.ComboLogonTypeChange;
begin
  EditSourceName.Enabled := (ComboLogonType.ItemIndex = S4U_INDEX);
  EditSourceLuid.Enabled := EditSourceName.Enabled;
  ButtonAllocLuid.Enabled := EditSourceName.Enabled;
end;

constructor TLogonDialog.Create;
begin
  inherited CreateChild(AOwner, cfmDesktop);
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

function TLogonDialog.GetLogonType;
const
  LogonTypeMapping: array [1 .. 7] of TSecurityLogonType = (
    LogonTypeInteractive, LogonTypeNetwork, LogonTypeNetworkCleartext,
    LogonTypeNewCredentials, LogonTypeUnlock, LogonTypeBatch, LogonTypeService
  );
begin
  Result := LogonTypeMapping[ComboLogonType.ItemIndex];
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

end.
