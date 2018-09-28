unit UI.Restrict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, TU.Common, TU.Tokens, UI.ListViewEx, UI.Prototypes,
  UI.Prototypes.ChildForm, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TDialogRestrictToken = class(TChildForm)
    CheckBoxDisableMaxPriv: TCheckBox;
    CheckBoxSandboxInert: TCheckBox;
    CheckBoxLUA: TCheckBox;
    CheckBoxWriteRestrict: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl1: TPageControl;
    TabSheetSidDisable: TTabSheet;
    TabSheetSidRestict: TTabSheet;
    TabSheetPrivDelete: TTabSheet;
    ListViewPrivileges: TPrivilegesListViewEx;
    ListViewRestrictSID: TGroupListViewEx;
    ListViewDisableSID: TGroupListViewEx;
    ButtonAddSID: TButton;
    PopupMenu: TPopupMenu;
    MenuEdit: TMenuItem;
    MenuRemove: TMenuItem;
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
    function GetFlags: Cardinal;
    procedure ChangedCaption(NewCaption: String);
  public
    procedure Refresh;
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes, UI.Modal.PickUser;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonAddSIDClick(Sender: TObject);
begin
  ListViewRestrictSID.AddGroup(TDialogPickUser.Execute(Self, True));
end;

procedure TDialogRestrictToken.ButtonOKClick(Sender: TObject);
const
  NO_SANBOX_INERT = 'The resulting token doesn''t contain SandboxInert flag ' +
    'despite you tried to enable it. Looks like this action requires ' +
    'SeTcbPrivilege on your system.';
var
  NewToken: TToken;
begin
  NewToken := TToken.CreateRestricted(Token, GetFlags,
    ListViewDisableSID.CheckedGroups,
    ListViewRestrictSID.CheckedGroups,
    ListViewPrivileges.CheckedPrivileges);

  FormMain.Frame.AddToken(NewToken);

  // Check whether SandboxInert was actually enabled
  if CheckBoxSandboxInert.Checked then
    with NewToken.SandboxInert do
      if IsValid and not Value then
      begin
        Hide;
        MessageDlg(NO_SANBOX_INERT, mtWarning, [mbOK], 0);
      end;

  Close;
end;

procedure TDialogRestrictToken.ChangedCaption(NewCaption: String);
begin
  Caption := Format('Create restricted token for "%s"', [NewCaption]);
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
  Token.OnCaptionChange.Delete(ChangedCaption);
  UnsubscribeTokenCanClose(Token);
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
var
  UserItem: TGroup;
begin
  SubscribeTokenCanClose(Token, Caption);
  Refresh;

  Token.OnCaptionChange.Add(ChangedCaption);
  Token.OnCaptionChange.Invoke(Token.Caption);
  ListViewDisableSID.Token := Token;
  ListViewRestrictSID.Token := Token;
  ListViewPrivileges.Token := Token;

  with Token.User do // It can also be disabled and restricted
    if IsValid then
    begin
      UserItem.SecurityIdentifier := Value.SecurityIdentifier;
      UserItem.Attributes := Value.Attributes;
      ListViewDisableSID.AddGroup(UserItem);
      ListViewRestrictSID.AddGroup(UserItem);
    end;
end;

function TDialogRestrictToken.GetFlags: Cardinal;
const
  DISABLE_MAX_PRIVILEGE = 1;
  SANDBOX_INERT = 2;
  LUA_TOKEN = 4;
  WRITE_RESTRICTED = 8; // TODO: Check windows version, this value depends on it
begin
  Result := 0;
  if CheckBoxDisableMaxPriv.Checked then
    Result := Result or DISABLE_MAX_PRIVILEGE;
  if CheckBoxSandboxInert.Checked then
    Result := Result or SANDBOX_INERT;
  if CheckBoxLUA.Checked then
    Result := Result or LUA_TOKEN;
  if CheckBoxWriteRestrict.Checked then
    Result := Result or WRITE_RESTRICTED;
end;

procedure TDialogRestrictToken.ListViewRestrictSIDContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
begin
  with ListViewRestrictSID do
  begin
    MenuEdit.Enabled := (SelCount = 1);

    // Show context menu only if selection contains removable items
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        Handled := Handled or IsAdditional(i);

    Handled := not Handled;
  end;
end;

procedure TDialogRestrictToken.MenuEditClick(Sender: TObject);
begin
  with ListViewRestrictSID do
    if Assigned(Selected) then
      Groups[Selected.Index] := TDialogPickUser.Execute(Self,
        Groups[Selected.Index]);
end;

procedure TDialogRestrictToken.MenuRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  with ListViewRestrictSID do
  begin
    // deletion changes indexes, go downwards
    for i := Items.Count - 1 downto 0 do
      if Items[i].Selected and IsAdditional(i) then
        RemoveGroup(i);
  end;
end;

procedure TDialogRestrictToken.Refresh;
begin
  Token.Groups;
  Token.Privileges;
end;

end.
