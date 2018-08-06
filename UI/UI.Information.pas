unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens, System.ImageList, Vcl.ImgList,
  UI.ListViewEx, UI.Prototypes, TU.Common, TU.WtsApi;

type
  TInfoDialog = class(TForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    StaticUser: TStaticText;
    EditUser: TEdit;
    ButtonClose: TButton;
    ListViewGroups: TGroupListViewEx;
    ListViewPrivileges: TPrivilegesListViewEx;
    TabRestricted: TTabSheet;
    ListViewRestricted: TGroupListViewEx;
    StaticObjAddr: TStaticText;
    EditObjAddr: TEdit;
    StaticSession: TStaticText;
    StaticElevation: TStaticText;
    StaticVirtualization: TStaticText;
    StaticIntegrity: TStaticText;
    StaticUIAccess: TStaticText;
    StaticType: TStaticText;
    ComboSession: TSessionComboBox;
    ComboIntegrity: TIntegrityComboBox;
    StaticHandle: TStaticText;
    EditHandle: TEdit;
    EditType: TEdit;
    ImageList: TImageList;
    ComboBoxView: TComboBox;
    BtnSetIntegrity: TSpeedButton;
    BtnSetSession: TSpeedButton;
    StaticAccess: TStaticText;
    EditAccess: TEdit;
    EditElevation: TEdit;
    PrivilegePopup: TPopupMenu;
    MenuPrivEnable: TMenuItem;
    MenuPrivDisable: TMenuItem;
    MenuPrivRemove: TMenuItem;
    GroupPopup: TPopupMenu;
    MenuGroupEnable: TMenuItem;
    MenuGroupDisable: TMenuItem;
    MenuGroupReset: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnSetIntegrityClick(Sender: TObject);
    procedure ChangedView(Sender: TObject);
    procedure BtnSetSessionClick(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetStaleColor(Sender: TObject);
    procedure ActionPrivilegeEnable(Sender: TObject);
    procedure ActionPrivilegeDisable(Sender: TObject);
    procedure ActionPrivilegeRemove(Sender: TObject);
    procedure ListViewPrivilegesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ActionGroupEnable(Sender: TObject);
    procedure ActionGroupDisable(Sender: TObject);
    procedure ActionGroupReset(Sender: TObject);
    procedure ListViewGroupsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    Token: TToken;
    procedure ConfirmTokenClose(Sender: TToken);
    procedure ChangedCaption(NewCaption: String);
    procedure ChangedIntegrity(NewIntegrity: CanFail<TTokenIntegrity>);
    procedure ChangedSession(NewSession: CanFail<Cardinal>);
    procedure ChangedPrivileges(NewPrivileges: CanFail<TPrivilegeArray>);
    procedure Refresh;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Colors;

{$R *.dfm}

procedure TInfoDialog.ActionGroupDisable(Sender: TObject);
begin
  if ListViewGroups.SelCount <> 0 then
    Token.GroupAdjust(ListViewGroups.SelectedGroups, gaDisable);
end;

procedure TInfoDialog.ActionGroupEnable(Sender: TObject);
begin
  if ListViewGroups.SelCount <> 0 then
    Token.GroupAdjust(ListViewGroups.SelectedGroups, gaEnable);
end;

procedure TInfoDialog.ActionGroupReset(Sender: TObject);
begin
  if ListViewGroups.SelCount <> 0 then
    Token.GroupAdjust(ListViewGroups.SelectedGroups, gaResetDefault);
end;

procedure TInfoDialog.ActionPrivilegeDisable(Sender: TObject);
begin
  if ListViewPrivileges.SelCount <> 0 then
    Token.PrivilegeAdjust(ListViewPrivileges.SelectedPrivileges, paDisable);
end;

procedure TInfoDialog.ActionPrivilegeEnable(Sender: TObject);
begin
  if ListViewPrivileges.SelCount <> 0 then
    Token.PrivilegeAdjust(ListViewPrivileges.SelectedPrivileges, paEnable);
end;

procedure TInfoDialog.ActionPrivilegeRemove(Sender: TObject);
begin
  if ListViewPrivileges.SelCount <> 0 then
    Token.PrivilegeAdjust(ListViewPrivileges.SelectedPrivileges, paRemove);
end;

procedure TInfoDialog.BtnSetIntegrityClick(Sender: TObject);
begin
  try
    Token.Integrity := ComboIntegrity.SelectedIntegrity;
  except
    ChangedIntegrity(Token.TryGetIntegrity);
    raise;
  end;
end;

procedure TInfoDialog.BtnSetSessionClick(Sender: TObject);
begin
  try
    Token.Session := ComboSession.SelectedSession;
  except
    ChangedSession(Token.TryGetSession);
    raise;
  end;
end;

procedure TInfoDialog.ChangedCaption(NewCaption: String);
begin
  Caption := Format('Token Information for "%s"', [NewCaption]);
end;

procedure TInfoDialog.ChangedIntegrity(NewIntegrity: CanFail<TTokenIntegrity>);
begin
  ComboIntegrity.Color := clWindow;
  ComboIntegrity.SetIntegrity(NewIntegrity);
end;

procedure TInfoDialog.ChangedPrivileges(
  NewPrivileges: CanFail<TPrivilegeArray>);
begin
  with NewPrivileges do
    if IsValid then
      TabPrivileges.Caption := Format('Privileges (%d)', [Length(Value)]);
end;

procedure TInfoDialog.ChangedSession(NewSession: CanFail<Cardinal>);
begin
  ComboSession.Color := clWindow;
  ComboSession.Items.BeginUpdate;

  with NewSession do
    if IsValid then
      ComboSession.SelectedSession := Value
    else
    begin
      ComboSession.ItemIndex := -1;
      ComboSession.Text := 'Unknown session';
    end;

  ComboSession.Items.EndUpdate;
end;

procedure TInfoDialog.ChangedView(Sender: TObject);
begin
  with Token.User do
    if IsValid then
    begin
      if ComboBoxView.ItemIndex = 0 then
        EditUser.Text := Value.ToString
      else
        EditUser.Text := Value.SID;
    end;
  ListViewGroups.ViewAs := TGroupViewAs(ComboBoxView.ItemIndex);
  ListViewRestricted.ViewAs := TGroupViewAs(ComboBoxView.ItemIndex);
end;

procedure TInfoDialog.ConfirmTokenClose(Sender: TToken);
const
  CONFIRM_CLOSE = 'This token has an opened information window. Do you want ' +
    'close it?';
begin
  // The main window should not close the token until any information windows
  // are opened for it.
  if MessageDlg(CONFIRM_CLOSE, mtConfirmation, mbYesNoCancel, 0) = IDYES then
    Close
  else
    Abort;
end;

constructor TInfoDialog.CreateFromToken(AOwner: TComponent; SrcToken: TToken);
begin
  Token := SrcToken;
  inherited Create(AOwner);
  Show;
end;

procedure TInfoDialog.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TInfoDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Token.Events.OnPrivilegesChange.Delete(ChangedPrivileges);
  Token.Events.OnIntegrityChange.Delete(ChangedIntegrity);
  Token.Events.OnSessionChange.Delete(ChangedSession);
  Token.OnCaptionChange.Delete(ChangedCaption);
  Token.OnCanClose.Delete(ConfirmTokenClose);
  FormMain.OnMainFormClose.Delete(DoCloseForm);
  Action := caFree;
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
begin
  FormMain.OnMainFormClose.Add(DoCloseForm);

  ListViewGroups.Token := Token;
  ListViewPrivileges.Token := Token;
  ListViewRestricted.Token := Token;

  Token.OnCanClose.Add(ConfirmTokenClose);
  Token.OnCaptionChange.Add(ChangedCaption);
  Token.Events.OnSessionChange.Add(ChangedSession);
  Token.Events.OnIntegrityChange.Add(ChangedIntegrity);
  Token.Events.OnPrivilegesChange.Add(ChangedPrivileges);

  Refresh;
end;

procedure TInfoDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    Refresh;
end;

procedure TInfoDialog.ListViewGroupsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  MenuGroupEnable.Visible := ListViewGroups.SelCount <> 0;
  MenuGroupDisable.Visible := ListViewGroups.SelCount <> 0;
end;

procedure TInfoDialog.ListViewPrivilegesContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := ListViewPrivileges.SelCount = 0;
end;

procedure TInfoDialog.Refresh;
begin
  ComboSession.RefreshSessionList;

  EditObjAddr.Text := '0x' + IntToHex(Token.ObjAddress, 8);
  EditHandle.Text := '0x' + IntToHex(Token.Handle, -1);

  with Token.Access do
    if IsValid then
      EditAccess.Text := AccessToDetailedString(Value);

  with Token.TokenTypeInfo do
    if IsValid then
      EditType.Text := Value.ToString;

  with Token.Elevation do
    if IsValid then
      EditElevation.Text := Value.ToString;

  // TODO: Should we share the obtained information with other event listeners?
  ChangedCaption(Token.Caption);
  ChangedIntegrity(Token.TryGetIntegrity);
  ChangedSession(Token.TryGetSession);
  ChangedView(Token);
  //TODO: It is now broken for groups, privileges, and restricted SIDs

  // TODO: Doesn't show zero count if we can't obtain it
  TabGroups.Caption := Format('Groups (%d)', [ListViewGroups.Items.Count]);
  TabPrivileges.Caption := Format('Privileges (%d)',
    [ListViewPrivileges.Items.Count]);
  TabRestricted.Caption := Format('Restricted SIDs (%d)',
    [ListViewRestricted.Items.Count]);
end;

procedure TInfoDialog.SetStaleColor(Sender: TObject);
begin
  (Sender as TComboBox).Color := clStale;
end;

end.
