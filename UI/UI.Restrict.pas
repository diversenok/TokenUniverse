unit UI.Restrict;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls, TU.Tokens,
  VclEx.ListView, UI.Prototypes.Forms, UI.Prototypes.Privileges,
  UI.Prototypes.Groups, NtUtils.Security.Sid, Winapi.WinNt, Ntapi.ntseapi,
  NtUtils;

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
    PrivilegesFrame: TPrivilegesFrame;
    GroupsDisableFrame: TGroupsFrame;
    GroupsRestrictFrame: TGroupsFrame;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure GroupsDisableFrameListViewExDblClick(Sender: TObject);
    procedure GroupsRestrictFrameListViewExDblClick(Sender: TObject);
  private
    Token: IToken;
    ManuallyAdded: TArray<TGroup>;
    function GetFlags: Cardinal;
    procedure ChangedCaption(const NewCaption: String);
    procedure ChangedGroups(const NewGroups: TArray<TGroup>);
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: IToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes, UI.Modal.PickUser, UI.Settings, TU.Suggestions,
  Winapi.securitybaseapi, UI.Sid.View, DelphiUtils.Arrays, Ntapi.ntrtl;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonAddSIDClick(Sender: TObject);
var
  Group: TGroup;
begin
  Group := TDialogPickUser.PickNew(Self, True);

  ManuallyAdded := ManuallyAdded + [Group];
  GroupsRestrictFrame.Add([Group]);

  // Check the newly added item
  with GroupsRestrictFrame.ListViewEx do
    Items[Pred(Items.Count)].Checked := True;
end;

procedure TDialogRestrictToken.ButtonOKClick(Sender: TObject);
var
  NewToken: IToken;
begin
  NewToken := TToken.CreateRestricted(Token, GetFlags,
    GroupsDisableFrame.Checked,
    GroupsRestrictFrame.Checked,
    PrivilegesFrame.Checked);

  FormMain.TokenView.Add(NewToken);

  // Check whether SandboxInert was actually enabled
  if CheckBoxSandboxInert.Checked then
    if NewToken.InfoClass.Query(tdTokenSandBoxInert) and
      not NewToken.InfoClass.SandboxInert then
      begin
        if not TSettings.NoCloseCreationDialogs then
          Hide;
        MessageDlg(NO_SANBOX_INERT, mtWarning, [mbOK], 0);
      end;

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogRestrictToken.ChangedCaption(const NewCaption: String);
begin
  Caption := Format('Create Restricted Token for "%s"', [NewCaption]);
end;

procedure TDialogRestrictToken.ChangedGroups(const NewGroups: TArray<TGroup>);
var
  Groups, AlreadyRestricted: TArray<TGroup>;
  i, j: Integer;
begin
  Groups := Copy(NewGroups, 0, Length(NewGroups));

  // User can be both disabled and restricted
  if Token.InfoClass.Query(tdTokenUser) then
    Groups := Groups + [Token.InfoClass.User];

  // Populate disable list
  GroupsDisableFrame.Load(Groups);

  // Hide disabled groups from the restricted view to avoid confusing outcomes
  TArray.FilterInline<TGroup>(Groups,
    function (const Entry: TGroup): Boolean
    begin
      Result := Entry.Attributes and SE_GROUP_ENABLED <> 0;
    end
  );

  // Include manually added groups
  Groups := Groups + ManuallyAdded;

  // Restricting SIDs can include arbitrary groups
  if Token.InfoClass.Query(tdTokenRestrictedSids) then
    AlreadyRestricted := Token.InfoClass.RestrictedSids
  else
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

  // Check already restricted items
  for i := 0 to High(AlreadyRestricted) do
  begin
    j := GroupsRestrictFrame.Find(AlreadyRestricted[i].Sid);
    if j >= 0 then
      GroupsRestrictFrame.ListViewEx.Items[j].Checked := True;
  end;
end;

constructor TDialogRestrictToken.CreateFromToken(AOwner: TComponent;
  SrcToken: IToken);
begin
  Token := SrcToken;
  inherited CreateChild(AOwner, True);
  Show;
end;

procedure TDialogRestrictToken.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TDialogRestrictToken.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Token.OnCaptionChange.Unsubscribe(ChangedCaption);
  Token.Events.OnPrivilegesChange.Unsubscribe(PrivilegesFrame.Load);
  Token.Events.OnGroupsChange.Unsubscribe(ChangedGroups);
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
var
  Group: TGroup;
  RestrInd, ItemInd: Integer;
begin
  Assert(Assigned(Token));

  Token.OnCaptionChange.Subscribe(ChangedCaption);
  ChangedCaption(Token.Caption);

  if Token.InfoClass.Query(tdTokenSandBoxInert) then
    CheckBoxSandboxInert.Checked := Token.InfoClass.SandboxInert;

  // Privileges
  PrivilegesFrame.ColoringUnChecked := pcStateBased;
  PrivilegesFrame.ColoringChecked := pcRemoved;
  Token.InfoClass.Query(tdTokenPrivileges);
  Token.Events.OnPrivilegesChange.Subscribe(PrivilegesFrame.Load, True);

  // Craft additional suggestions for restricting list
  SetLength(ManuallyAdded, 0);
  Group.Attributes := SE_GROUP_ENABLED_BY_DEFAULT or SE_GROUP_ENABLED;

  // RESTRICTED is useful to provide access to WinSta0 and Default desktop
  if SddlxGetWellKnownSid(Group.Sid, WinRestrictedCodeSid).IsSuccess then
    ManuallyAdded := ManuallyAdded + [Group];

  // WRITE RESTRICTED can also be useful
  if SddlxGetWellKnownSid(Group.Sid, WinWriteRestrictedCodeSid).IsSuccess then
    ManuallyAdded := ManuallyAdded + [Group];

  // Populare groups
  Token.InfoClass.Query(tdTokenGroups);
  Token.Events.OnGroupsChange.Subscribe(ChangedGroups, True);
end;

function TDialogRestrictToken.GetFlags: Cardinal;
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

procedure TDialogRestrictToken.GroupsDisableFrameListViewExDblClick(
  Sender: TObject);
begin
  with GroupsDisableFrame.ListViewEx do
    if Assigned(Selected) then
      TDialogSidView.CreateView(GroupsDisableFrame[Selected.Index].Sid);
end;

procedure TDialogRestrictToken.GroupsRestrictFrameListViewExDblClick(
  Sender: TObject);
begin
  with GroupsRestrictFrame.ListViewEx do
    if Assigned(Selected) then
      TDialogSidView.CreateView(GroupsRestrictFrame[Selected.Index].Sid);
end;

end.
