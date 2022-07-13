unit UI.Restrict;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes.Forms, UI.Prototypes.Privileges,
  UI.Prototypes.Groups, NtUtils.Security.Sid, Ntapi.WinNt, Ntapi.ntseapi,
  NtUtils, TU.Tokens;

type
  TDialogRestrictToken = class(TChildForm)
    CheckBoxDisableMaxPriv: TCheckBox;
    CheckBoxSandboxInert: TCheckBox;
    CheckBoxLUA: TCheckBox;
    CheckBoxWriteOnly: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl1: TPageControl;
    TabSheetSidDisable: TTabSheet;
    TabSheetSidRestict: TTabSheet;
    TabSheetPrivDelete: TTabSheet;
    ButtonAddSID: TButton;
    CheckBoxUsual: TCheckBox;
    GroupsRestrictFrame: TFrameGroups;
    GroupsDisableFrame: TFrameGroups;
    PrivilegesFrame: TFramePrivileges;
    procedure FormCreate(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
  private
    Token: IToken;
    ManuallyAdded: TArray<TGroup>;
    CaptionSubscription: IAutoReleasable;
    PrivilegesSubscription: IAutoReleasable;
    GroupsSubscription: IAutoReleasable;
    function GetFlags: Cardinal;
    procedure ChangedCaption(const InfoClass: TTokenStringClass; const NewCaption: String);
    procedure ChangedPrivileges(const Status: TNtxStatus; const Privileges: TArray<TPrivilege>);
    procedure ChangedGroups(const Status: TNtxStatus; const NewGroups: TArray<TGroup>);
    procedure InspectGroup(const Group: TGroup);
  public
    constructor CreateFromToken(AOwner: TComponent; const SrcToken: IToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes, UI.Modal.PickUser, UI.Settings, TU.Suggestions,
  Ntapi.WinBase, UI.Sid.View, DelphiUtils.Arrays, Ntapi.ntrtl,
  TU.Tokens.Open, NtUiLib.Errors;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonAddSIDClick;
var
  Group: TGroup;
begin
  Group := TDialogPickUser.PickNew(Self, True);

  ManuallyAdded := ManuallyAdded + [Group];
  GroupsRestrictFrame.Add([Group]);
  GroupsRestrictFrame.IsChecked[Group] := True;
end;

procedure TDialogRestrictToken.ButtonOKClick;
var
  NewToken: IToken;
begin
  MakeFilteredToken(
    NewToken,
    Token,
    GetFlags,
    GroupsToSids(GroupsDisableFrame.Checked),
    PrivilegesToIDs(PrivilegesFrame.Checked),
    GroupsToSids(GroupsRestrictFrame.Checked)
  ).RaiseOnError;

  FormMain.TokenView.Add(NewToken);

  if CheckBoxSandboxInert.Checked then
    CheckSandboxInert(Handle, NewToken);

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogRestrictToken.ChangedCaption;
begin
  Caption := Format('Create Restricted Token for "%s"', [NewCaption]);
end;

procedure TDialogRestrictToken.ChangedGroups;
var
  User: TGroup;
  Groups, AlreadyRestricted: TArray<TGroup>;
begin
  if not Status.IsSuccess then
    Exit;

  Groups := Copy(NewGroups, 0, Length(NewGroups));

  // User can be both disabled and restricted
  if Token.QueryUser(User).IsSuccess then
  begin
    // Zero is a default state here that indicated the enabled one
    if User.Attributes = 0 then
      User.Attributes := SE_GROUP_ENABLED or SE_GROUP_ENABLED_BY_DEFAULT;

    Groups := Groups + [User];
  end;

  // Populate disable list
  GroupsDisableFrame.Load(Groups);

  // Hide disabled groups from the restricted view to avoid confusing outcomes
  TArray.FilterInline<TGroup>(Groups,
    function (const Entry: TGroup): Boolean
    begin
      Result := BitTest(Entry.Attributes and SE_GROUP_ENABLED);
    end
  );

  // Include manually added groups
  Groups := Groups + ManuallyAdded;

  // Restricting SIDs can include arbitrary groups
  if not Token.QueryRestrictedSids(AlreadyRestricted).IsSuccess then
    AlreadyRestricted := nil;

  // Include already restricted groups as well
  Groups := Groups + AlreadyRestricted;

  // Exclude all duplicates
  Groups := TArray.RemoveDuplicates<TGroup>(Groups,
    function (const A, B: TGroup): Boolean
    begin
      Result := RtlEqualSid(A.Sid.Data, B.Sid.Data);
    end
  );

  // Populate resricted view
  GroupsRestrictFrame.Load(Groups);
  GroupsRestrictFrame.Checked := AlreadyRestricted;
end;

procedure TDialogRestrictToken.ChangedPrivileges;
begin
  if Status.IsSuccess then
    PrivilegesFrame.Load(Privileges);
end;

constructor TDialogRestrictToken.CreateFromToken;
begin
  Token := SrcToken;
  inherited CreateChild(AOwner, cfmDesktop);
  GroupsDisableFrame.OnDefaultAction := InspectGroup;
  GroupsRestrictFrame.OnDefaultAction := InspectGroup;
  Show;
end;

procedure TDialogRestrictToken.DoCloseForm;
begin
  Close;
end;

procedure TDialogRestrictToken.FormCreate;
var
  Group: TGroup;
  SabdoxInert: LongBool;
begin
  Assert(Assigned(Token));

  CaptionSubscription := Token.ObserveString(tsCaption, ChangedCaption);

  CheckBoxSandboxInert.Checked := Token.QuerySandboxInert(
    SabdoxInert).IsSuccess and SabdoxInert;

  // Privileges
  PrivilegesFrame.ColoringUnChecked := pcStateBased;
  PrivilegesFrame.ColoringChecked := pcRemoved;
  PrivilegesSubscription := Token.ObservePrivileges(ChangedPrivileges);

  // Craft additional suggestions for restricting list
  SetLength(ManuallyAdded, 0);
  Group.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or SE_GROUP_ENABLED;

  // RESTRICTED is useful to provide access to WinSta0 and Default desktop
  if SddlxCreateWellKnownSid(TWellKnownSidType.WinRestrictedCodeSid,
    Group.Sid).IsSuccess then
    ManuallyAdded := ManuallyAdded + [Group];

  // WRITE RESTRICTED can also be useful
  if SddlxCreateWellKnownSid(TWellKnownSidType.WinWriteRestrictedCodeSid,
    Group.Sid).IsSuccess then
    ManuallyAdded := ManuallyAdded + [Group];

  // Populare groups
  GroupsSubscription := Token.ObserveGroups(ChangedGroups);
end;

function TDialogRestrictToken.GetFlags;
begin
  Result := 0;

  if CheckBoxDisableMaxPriv.Checked then
    Result := Result or DISABLE_MAX_PRIVILEGE;

  if CheckBoxSandboxInert.Checked then
    Result := Result or SANDBOX_INERT;

  if CheckBoxLUA.Checked then
    Result := Result or LUA_TOKEN;

  if not CheckBoxWriteOnly.Checked then
    Result := Result or WRITE_RESTRICTED;
end;

procedure TDialogRestrictToken.InspectGroup;
begin
  TDialogSidView.CreateView(Self, Group.Sid);
end;

end.
