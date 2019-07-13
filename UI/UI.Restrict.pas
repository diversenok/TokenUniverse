unit UI.Restrict;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls, TU.Tokens,
  UI.ListViewEx, UI.Prototypes.ChildForm, UI.Prototypes.Privileges,
  UI.Prototypes.Groups, NtUtils.Security.Sid, Winapi.WinNt;

type
  TDialogRestrictToken = class(TChildTaskbarForm)
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
    PopupMenu: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    CheckBoxUsual: TCheckBox;
    FramePrivileges: TFramePrivileges;
    FrameGroupsDisable: TFrameGroups;
    FrameGroupsRestrict: TFrameGroups;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonAddSIDClick(Sender: TObject);
    procedure ListViewRestrictSIDContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure MenuRemoveClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
  private
    Token: TToken;
    FirstEditableItem: Integer; // in list of restricting SIDs
    function GetFlags: Cardinal;
    procedure ChangedCaption(NewCaption: String);
    procedure ChangedPrivileges(NewPrivileges: TArray<TPrivilege>);
    procedure ChangedGroups(NewGroups: TArray<TGroup>);
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes, UI.Modal.PickUser, UI.Settings, TU.Suggestions,
  Ntapi.ntseapi, Winapi.securitybaseapi, DelphiUtils.Strings,
  NtUtils.Exceptions;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonAddSIDClick(Sender: TObject);
begin
  FrameGroupsRestrict.AddGroup(TDialogPickUser.PickNew(Self, True)).Checked :=
    True;
end;

procedure TDialogRestrictToken.ButtonOKClick(Sender: TObject);
var
  NewToken: TToken;
begin
  NewToken := TToken.CreateRestricted(Token, GetFlags,
    FrameGroupsDisable.CheckedGroups,
    FrameGroupsRestrict.CheckedGroups,
    FramePrivileges.CheckedPrivileges);

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

procedure TDialogRestrictToken.ChangedCaption(NewCaption: String);
begin
  Caption := Format('Create Restricted Token for "%s"', [NewCaption]);
end;

procedure TDialogRestrictToken.ChangedGroups(NewGroups: TArray<TGroup>);
var
  ManuallyAddes: TArray<TGroup>;
  i: Integer;
begin
  // Disabled SIDs: add all groups
  with FrameGroupsDisable do
  begin
    ListView.Items.BeginUpdate(True);

    Clear;
    AddGroups(NewGroups);

    ListView.Items.EndUpdate(True);
  end;

  // Restricting SIDs: add only enabled once to avoid confusion
  with FrameGroupsRestrict do
  begin
    ListView.Items.BeginUpdate(True);

    // If there are any manually added SIDs backup them
    if FirstEditableItem < ListView.Items.Count then
    begin
      SetLength(ManuallyAddes, ListView.Items.Count - FirstEditableItem);

      for i := FirstEditableItem to ListView.Items.Count - 1 do
        ManuallyAddes[i - FirstEditableItem] := Group[i];
    end
    else
      SetLength(ManuallyAddes, 0);

    // Delete everything
    Clear;

    // Add enabled groups
    for i := 0 to High(NewGroups) do
      if Contains(NewGroups[i].Attributes, SE_GROUP_ENABLED) then
        AddGroup(NewGroups[i]);

    // Starting from here manually items only
    FirstEditableItem := ListView.Items.Count;

    // Restore manually added ones
    for i := 0 to High(ManuallyAddes) do
      AddGroup(ManuallyAddes[i]);

    ListView.Items.EndUpdate(True);
  end;
end;

procedure TDialogRestrictToken.ChangedPrivileges(
  NewPrivileges: TArray<TPrivilege>);
begin
  FramePrivileges.ListView.Items.BeginUpdate(True);

  FramePrivileges.Clear;
  FramePrivileges.AddPrivileges(NewPrivileges);

  FramePrivileges.ListView.Items.EndUpdate(True);
end;

constructor TDialogRestrictToken.CreateFromToken(AOwner: TComponent;
  SrcToken: TToken);
begin
  Token := SrcToken;
  inherited Create(AOwner);
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
  Token.Events.OnPrivilegesChange.Unsubscribe(ChangedPrivileges);
  Token.Events.OnGroupsChange.Unsubscribe(ChangedGroups);
  UnsubscribeTokenCanClose(Token);
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
var
  Sid: ISid;
  Group: TGroup;
  RestrInd, ItemInd: Integer;
begin
  Assert(Assigned(Token));

  SubscribeTokenCanClose(Token, Caption);

  Token.OnCaptionChange.Subscribe(ChangedCaption);
  ChangedCaption(Token.Caption);

  if Token.InfoClass.Query(tdTokenSandBoxInert) then
    CheckBoxSandboxInert.Checked := Token.InfoClass.SandboxInert;

  // Privileges
  FramePrivileges.ColorMode := pcGrayChecked;
  Token.InfoClass.Query(tdTokenPrivileges);
  Token.Events.OnPrivilegesChange.Subscribe(ChangedPrivileges, True);

  // Disabled SIDs and Restricting SIDs
  begin
    FrameGroupsDisable.ListView.Items.BeginUpdate;
    FrameGroupsRestrict.ListView.Items.BeginUpdate;

    // Subscribe
    Token.InfoClass.Query(tdTokenGroups);
    Token.Events.OnGroupsChange.Subscribe(ChangedGroups, True);

     // Add the user since it can also be disabled and restricted
    if Token.InfoClass.Query(tdTokenUser) then
    begin
      FrameGroupsDisable.AddGroup(Token.InfoClass.User);
      FrameGroupsRestrict.AddGroup(Token.InfoClass.User);
    end;

    // RESTRICTED is useful to provide access to WinSta0 and Default desktop
    if TSid.GetWellKnownSid(WinRestrictedCodeSid, Sid).IsSuccess then
    begin
      Group.SecurityIdentifier := Sid;
      Group.Attributes := SE_GROUP_USER_DEFAULT;
      FrameGroupsRestrict.AddGroup(Group);
    end;

    // And WRITE RESTRICTED
    if TSid.GetWellKnownSid(WinWriteRestrictedCodeSid, Sid).IsSuccess then
    begin
      Group.SecurityIdentifier := Sid;
      Group.Attributes := SE_GROUP_USER_DEFAULT;
      FrameGroupsRestrict.AddGroup(Group);
    end;

    // If the token has restricting SIDs then check them. It can also contain
    // manually added items that are not part of the group list. Add them here.
    if Token.InfoClass.Query(tdTokenRestrictedSids) then
      with Token.InfoClass do
        for RestrInd := 0 to High(RestrictedSids) do
        begin
          ItemInd := FrameGroupsRestrict.Find(
            RestrictedSids[RestrInd].SecurityIdentifier);

          // Check the item if it's in the list. If not, add it.
          if ItemInd <> -1 then
            FrameGroupsRestrict.ListView.Items[ItemInd].Checked := True
          else
            FrameGroupsRestrict.AddGroup(
              RestrictedSids[RestrInd]).Checked := True;
      end;

    FrameGroupsRestrict.ListView.Items.EndUpdate;
    FrameGroupsDisable.ListView.Items.EndUpdate;
  end;
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

procedure TDialogRestrictToken.ListViewRestrictSIDContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
begin
  with FrameGroupsRestrict.ListView do
  begin
    // Only one item can be edited at a time
    MenuEdit.Enabled := (SelCount = 1);

    // Show context menu only if selection contains removable/editable items
    Handled := True;
    for i := FirstEditableItem to Items.Count - 1 do
      if Items[i].Selected then
      begin
        Handled := False;
        Exit;
      end;
  end;
end;

procedure TDialogRestrictToken.MenuEditClick(Sender: TObject);
begin
  with FrameGroupsRestrict do
    if (ListView.SelCount = 1) and Assigned(ListView.Selected) and
      (ListView.Selected.Index >= FirstEditableItem) then
        UiEditSelected(Self, True);
end;

procedure TDialogRestrictToken.MenuRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  with FrameGroupsRestrict.ListView do
  begin
    // deletion changes indexes, go downwards
    for i := Items.Count - 1 downto FirstEditableItem do
      if Items[i].Selected then
        FrameGroupsRestrict.RemoveGroup(i);
  end;
end;

end.
