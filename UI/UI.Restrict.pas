unit UI.Restrict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, TU.Tokens, UI.ListViewEx, UI.Prototypes,
  UI.Prototypes.ChildForm, System.ImageList, Vcl.ImgList, Vcl.Menus,
  UI.Prototypes.Privileges, NtUtils.Types;

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
    ListViewRestrictSID: TListViewEx;
    ListViewDisableSID: TListViewEx;
    ButtonAddSID: TButton;
    PopupMenu: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
    CheckBoxUsual: TCheckBox;
    FramePrivileges: TFramePrivileges;
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
    DisableGoupsSource, RestrictGroupsSource: TGroupsSource;
    function GetFlags: Cardinal;
    procedure ChangedCaption(NewCaption: String);
    procedure ChangedPrivileges(NewPrivileges: TPrivilegeArray);
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes, UI.Modal.PickUser, TU.Tokens.Types, UI.Settings,
  TU.Suggestions, Ntapi.ntseapi, Winapi.securitybaseapi;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonAddSIDClick(Sender: TObject);
begin
  RestrictGroupsSource.AddGroup(TDialogPickUser.PickNew(Self, True)).Checked :=
    True;
end;

procedure TDialogRestrictToken.ButtonOKClick(Sender: TObject);
var
  NewToken: TToken;
begin
  NewToken := TToken.CreateRestricted(Token, GetFlags,
    DisableGoupsSource.CheckedGroups,
    RestrictGroupsSource.CheckedGroups,
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

procedure TDialogRestrictToken.ChangedPrivileges(
  NewPrivileges: TPrivilegeArray);
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
  RestrictGroupsSource.Free;
  DisableGoupsSource.Free;
  Token.Events.OnPrivilegesChange.Unsubscribe(ChangedPrivileges);
  UnsubscribeTokenCanClose(Token);
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
var
  Sid: ISid;
  Group: TGroup;
  Found: Boolean;
  RestrInd, ItemInd: Integer;
begin
  Assert(Assigned(Token));

  SubscribeTokenCanClose(Token, Caption);

  DisableGoupsSource := TGroupsSource.Create(ListViewDisableSID);
  RestrictGroupsSource := TGroupsSource.Create(ListViewRestrictSID);

  DisableGoupsSource.SubscribeToken(Token, gsGroups);

  FramePrivileges.ColorMode := pmGrayChecked;
  Token.InfoClass.Query(tdTokenPrivileges);
  Token.Events.OnPrivilegesChange.Subscribe(ChangedPrivileges, True);

  // Show only enabled groups to make things clear when using restictions
  RestrictGroupsSource.SubscribeToken(Token, gsGroupsEnabledOnly);

  Token.OnCaptionChange.Subscribe(ChangedCaption);
  ChangedCaption(Token.Caption);

  // The user can also be disabled and restricted
  if Token.InfoClass.Query(tdTokenUser) then
  begin
    DisableGoupsSource.AddGroup(Token.InfoClass.User);
    RestrictGroupsSource.AddGroup(Token.InfoClass.User);
  end;

  // RESTRICTED also is useful to provide access to WinSta0 and Default desktop
  if TSid.GetWellKnownSid(WinRestrictedCodeSid, Sid) then
  begin
    Group.SecurityIdentifier := Sid;
    Group.Attributes := SE_GROUP_USER_DEFAULT;
    RestrictGroupsSource.AddGroup(Group);
  end;

  // And WRITE RESTRICTED
  if TSid.GetWellKnownSid(WinWriteRestrictedCodeSid, Sid) then
  begin
    Group.SecurityIdentifier := Sid;
    Group.Attributes := SE_GROUP_USER_DEFAULT;
    RestrictGroupsSource.AddGroup(Group);
  end;

  // If the token has restricting SIDs then check them. It can also contain
  // manually added items that are not part of the group list. Add them here.
  if Token.InfoClass.Query(tdTokenRestrictedSids) then
    with Token.InfoClass do
      for RestrInd := 0 to High(RestrictedSids) do
      begin
        Found := False;

        // Find and check it
        for ItemInd := 0 to ListViewRestrictSID.Items.Count - 1 do
          if RestrictedSids[RestrInd].SecurityIdentifier.Lookup.SDDL =
            RestrictGroupsSource.Group[ItemInd].SecurityIdentifier.Lookup.SDDL
            then
          begin
            ListViewRestrictSID.Items[ItemInd].Checked := True;
            Found := True;
            Break;
          end;

      // The restricting SID was not found in the list, add and check it
      if not Found then
        RestrictGroupsSource.AddGroup(RestrictedSids[RestrInd]).Checked := True;
    end;

  if Token.InfoClass.Query(tdTokenSandBoxInert) then
    CheckBoxSandboxInert.Checked := Token.InfoClass.SandboxInert
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
  with ListViewRestrictSID do
  begin
    // Only one item can be edited at a time
    MenuEdit.Enabled := (SelCount = 1);

    // Show context menu only if selection contains removable/editable items
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        Handled := Handled or RestrictGroupsSource.IsAdditional(i);

    Handled := not Handled;
  end;
end;

procedure TDialogRestrictToken.MenuEditClick(Sender: TObject);
begin
  RestrictGroupsSource.UiEditSelected(Self, True);
end;

procedure TDialogRestrictToken.MenuRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  with ListViewRestrictSID do
  begin
    // deletion changes indexes, go downwards
    for i := Items.Count - 1 downto 0 do
      if Items[i].Selected and RestrictGroupsSource.IsAdditional(i) then
        RestrictGroupsSource.RemoveGroup(i);
  end;
end;

end.
