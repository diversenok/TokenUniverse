unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens, System.ImageList, Vcl.ImgList,
  UI.ListViewEx, UI.Prototypes, UI.Prototypes.ChildForm, TU.Common, TU.WtsApi;

type
  TInfoDialog = class(TChildForm)
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
    StaticSession: TStaticText;
    StaticIntegrity: TStaticText;
    ComboSession: TSessionComboBox;
    ComboIntegrity: TIntegrityComboBox;
    ImageList: TImageList;
    ComboBoxView: TComboBox;
    BtnSetIntegrity: TSpeedButton;
    BtnSetSession: TSpeedButton;
    PrivilegePopup: TPopupMenu;
    MenuPrivEnable: TMenuItem;
    MenuPrivDisable: TMenuItem;
    MenuPrivRemove: TMenuItem;
    GroupPopup: TPopupMenu;
    MenuGroupEnable: TMenuItem;
    MenuGroupDisable: TMenuItem;
    MenuGroupReset: TMenuItem;
    TabAdvanced: TTabSheet;
    ListViewAdvanced: TListViewEx;
    StaticOwner: TStaticText;
    ComboOwner: TComboBox;
    BtnSetOwner: TSpeedButton;
    BtnSetPrimary: TSpeedButton;
    ComboPrimary: TComboBox;
    StaticPrimary: TStaticText;
    StaticUIAccess: TStaticText;
    ComboUIAccess: TComboBox;
    BtnSetUIAccess: TSpeedButton;
    StaticText1: TStaticText;
    ComboPolicy: TComboBox;
    SpeedButton1: TSpeedButton;
    ListViewGeneral: TListViewEx;
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
    procedure BtnSetUIAccessClick(Sender: TObject);
    procedure BtnSetMandatoryPolicy(Sender: TObject);
    procedure ListViewAdvancedResize(Sender: TObject);
  private
    Token: TToken;
    procedure ChangedCaption(NewCaption: String);
    procedure ChangedIntegrity(NewIntegrity: CanFail<TTokenIntegrity>);
    procedure ChangedSession(NewSession: CanFail<Cardinal>);
    procedure ChangedUIAccess(NewUIAccess: CanFail<Cardinal>);
    procedure ChangedPolicy(NewPolicy: CanFail<TMandatoryPolicy>);
    procedure ChangedPrivileges(NewPrivileges: CanFail<TPrivilegeArray>);
    procedure Refresh;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Colors, TU.LsaApi;

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

procedure TInfoDialog.BtnSetMandatoryPolicy(Sender: TObject);
begin
  try
    if ComboPolicy.ItemIndex = -1 then
      Token.MandatoryPolicy := TMandatoryPolicy(StrToIntEx(ComboPolicy.Text,
        'mandatory policy flag'))
    else
      Token.MandatoryPolicy := TMandatoryPolicy(ComboPolicy.ItemIndex);
  except
    ChangedPolicy(Token.TryGetMandatoryPolicy);
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

procedure TInfoDialog.BtnSetUIAccessClick(Sender: TObject);
begin
  ComboUIAccess.Color := clWindow;
  try
    if ComboUIAccess.ItemIndex = -1 then
      Token.UIAccess := StrToIntEx(ComboUIAccess.Text, 'UIAccess value')
    else
      Token.UIAccess := ComboUIAccess.ItemIndex;
  except
    ChangedUIAccess(Token.TryGetUIAccess);
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

procedure TInfoDialog.ChangedPolicy(NewPolicy: CanFail<TMandatoryPolicy>);
begin
  ComboPolicy.Color := clWindow;

  with NewPolicy do
    if IsValid then
    begin
      if (Value >= TokenMandatoryPolicyOff) and
       (Value <= TokenMandatoryPolicyValidMask) then
       ComboPolicy.ItemIndex := Integer(Value)
      else
      begin
        ComboPolicy.ItemIndex := -1;
        ComboPolicy.Text := IntToStr(Integer(Value));
      end;
    end
    else
    begin
      ComboPolicy.ItemIndex := -1;
      ComboPolicy.Text := 'Unknown policy';
    end;
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

procedure TInfoDialog.ChangedUIAccess(NewUIAccess: CanFail<Cardinal>);
begin
  with NewUIAccess do
  begin
    if IsValid then
      ComboUIAccess.ItemIndex := Integer(Value <> 0)
    else
    begin
      ComboUIAccess.ItemIndex := -1;
      ComboUIAccess.Text := 'Unknown UIAccess';
    end;
  end;
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

  with Token.Owner do
    if IsValid then
    begin
      if ComboBoxView.ItemIndex = 0 then
        ComboOwner.Text := Value.ToString
      else
        ComboOwner.Text := Value.SID;
    end;

  with Token.PrimaryGroup do
    if IsValid then
    begin
      if ComboBoxView.ItemIndex = 0 then
        ComboPrimary.Text := Value.ToString
      else
        ComboPrimary.Text := Value.SID;
    end;

  ListViewGroups.ViewAs := TGroupViewAs(ComboBoxView.ItemIndex);
  ListViewRestricted.ViewAs := TGroupViewAs(ComboBoxView.ItemIndex);
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
  Token.Events.OnPolicyChange.Delete(ChangedPolicy);
  Token.Events.OnIntegrityChange.Delete(ChangedIntegrity);
  Token.Events.OnUIAccessChange.Delete(ChangedUIAccess);
  Token.Events.OnSessionChange.Delete(ChangedSession);
  Token.OnCaptionChange.Delete(ChangedCaption);
  UnsubscribeTokenCanClose(Token);
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
begin
  SubscribeTokenCanClose(Token, Caption);

  ListViewGroups.Token := Token;
  ListViewPrivileges.Token := Token;
  ListViewRestricted.Token := Token;

  Token.OnCaptionChange.Add(ChangedCaption);
  Token.Events.OnSessionChange.Add(ChangedSession);
  Token.Events.OnUIAccessChange.Add(ChangedUIAccess);
  Token.Events.OnIntegrityChange.Add(ChangedIntegrity);
  Token.Events.OnPolicyChange.Add(ChangedPolicy);
  Token.Events.OnPrivilegesChange.Add(ChangedPrivileges);

  Refresh;
end;

procedure TInfoDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F5 then
    Refresh;
end;

procedure TInfoDialog.ListViewAdvancedResize(Sender: TObject);
begin
  // HACK: designs-time AutoSize causes horizontal scrollbar to appear
  ListViewAdvanced.Columns[1].AutoSize := True;
  ListViewRestricted.OnResize := nil;
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
var
  i: Integer;
begin
  ComboSession.RefreshSessionList;

  ListViewGeneral.Items.BeginUpdate;
  with ListViewGeneral do
  begin
    Items[0].SubItems[0] := Format('0x%.8x', [Token.ObjAddress]);
    Items[1].SubItems[0] := Format('0x%x', [Token.Handle]);

    with Token.Access do
      if IsValid then
        Items[2].SubItems[0] := AccessToDetailedString(Value);

    with Token.TokenTypeInfo do
      if IsValid then
        Items[3].SubItems[0] := Value.ToString;

    with Token.Elevation do
    if IsValid then
        Items[4].SubItems[0] := Value.ToString;
  end;
  ListViewGeneral.Items.EndUpdate;

  ListViewAdvanced.Items.BeginUpdate;
  with ListViewAdvanced do
  begin
    with Token.Source do
      if IsValid then
      begin
        Items[0].SubItems[0] := TokeSourceNameToString(Value);
        Items[1].SubItems[0] := Value.SourceIdentifier.ToString;
      end;

    with Token.Statistics do
      if IsValid then
      begin
        Items[2].SubItems[0] := Value.TokenId.ToString;
        Items[3].SubItems[0] := Value.AuthenticationId.ToString;
        Items[4].SubItems[0] := NativeTimeToString(Value.ExpirationTime.QuadPart);
        Items[5].SubItems[0] := BytesToString(Value.DynamicCharged);
        Items[6].SubItems[0] := BytesToString(Value.DynamicAvailable);
        Items[7].SubItems[0] := Value.GroupCount.ToString;
        Items[8].SubItems[0] := Value.PrivilegeCount.ToString;
        Items[9].SubItems[0] := Value.ModifiedId.ToString;

        Items[12].SubItems[0] := Value.AuthenticationId.ToString;
        with GetLogonSessionInformation(Value.AuthenticationId) do
          if IsValid then
          begin
            if Value.HasUser then
            begin
              Items[13].SubItems[0] := Value.User.ToString;
              Items[13].Hint := TGroupListViewEx.BuildHint(Value.User,
                TGroupAttributes(0), False);
            end
            else
              Items[13].SubItems[0] := 'No linked user';
            Items[14].SubItems[0] := Value.AuthPackage;
            Items[15].SubItems[0] := Value.LogonServer;
            Items[16].SubItems[0] := Value.LogonType.ToString;
            Items[17].SubItems[0] := Value.Session.ToString;
            Items[18].SubItems[0] := DateTimeToStr(Value.LogonTime);
          end
          else
          begin
            for i := 13 to 18 do
              Items[i].Hint := GetErrorMessage;
          end;
      end;

    with Token.SandboxInert do
      if IsValid then
        Items[10].SubItems[0] := YesNoToString(Value);

    with Token.HasRestrictions do
      if IsValid then
        Items[11].SubItems[0] := YesNoToString(Value);
  end;
  ListViewAdvanced.Items.EndUpdate;

  // TODO: Should we share the obtained information with other event listeners?
  ChangedCaption(Token.Caption);
  ChangedIntegrity(Token.TryGetIntegrity);
  ChangedSession(Token.TryGetSession);
  ChangedUIAccess(Token.TryGetUIAccess);
  ChangedPolicy(Token.TryGetMandatoryPolicy);
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
