unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens, System.ImageList, Vcl.ImgList;

type
  TInfoDialog = class(TForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    StaticUser: TStaticText;
    EditUser: TEdit;
    StaticSID: TStaticText;
    ButtonClose: TButton;
    ListViewGroups: TListView;
    EditSID: TEdit;
    ListViewPrivileges: TListView;
    TabRestricted: TTabSheet;
    ListViewRestricted: TListView;
    StaticObjAddr: TStaticText;
    EditObjAddr: TEdit;
    StaticSession: TStaticText;
    StaticElevation: TStaticText;
    StaticVirtualization: TStaticText;
    StaticIntegrity: TStaticText;
    StaticUIAccess: TStaticText;
    StaticType: TStaticText;
    ComboSession: TComboBox;
    ComboIntegrity: TComboBox;
    StaticHandle: TStaticText;
    EditHandle: TEdit;
    EditType: TEdit;
    ImageList: TImageList;
    ComboBoxView: TComboBox;
    BtnSetIntegrity: TSpeedButton;
    BtnSetSession: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnSetIntegrityClick(Sender: TObject);
    procedure ChangedView(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
  private
    Token: TToken; // TODO: What if we delete it from the list?
    procedure ChangedCaption(Sender: TObject);
    procedure ChangedIntegrity(Sender: TObject);
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  TU.Suggestions;

{$R *.dfm}

procedure TInfoDialog.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TInfoDialog.BtnSetIntegrityClick(Sender: TObject);
const
  IndexToIntegrity: array [0 .. 5] of TTokenIntegrityLevel = (ilUntrusted,
    ilLow, ilMedium, ilMediumPlus, ilHigh, ilSystem);
var
  IL: Integer;
begin
  try
    if ComboIntegrity.ItemIndex <> -1 then
      Token.Integrity := IndexToIntegrity[ComboIntegrity.ItemIndex]
    else
    begin
      if String(ComboIntegrity.Text).StartsWith('0x') then
      ComboIntegrity.Text := String(ComboIntegrity.Text).Replace('0x', '$', []);

      if TryStrToInt(ComboIntegrity.Text, IL) then
        Token.Integrity := TTokenIntegrityLevel(IL)
      else
        raise EConvertError.Create(E_CONV_INTEGRITY);
    end;
  except
    on Exception do
    begin
      ChangedIntegrity(Token);
      raise;
    end;
  end;
end;

procedure TInfoDialog.ChangedCaption(Sender: TObject);
const
  Title = 'Token Information for "%s"';
begin
  Caption := Format(Title, [Token.Caption]);
end;

procedure TInfoDialog.ChangedIntegrity(Sender: TObject);
var
  IL: TTokenIntegrityLevel;
  index: integer;
begin
  with Token.TryGetIntegrity do
    if IsValid then
      IL := Value.Level
    else
      Exit;

  if not IL.IsWellKnown then
  begin
    if IL < ilLow then
      index := 1
    else if IL < ilMedium then
      index := 2
    else if IL < ilMediumPlus then
      index := 3
    else if IL < ilHigh then
      index := 4
    else if IL < ilSystem then
      index := 5
    else
      index := 6;

    ComboIntegrity.Items.Insert(index, Format('Itermediate (0x%.4x)',
      [Cardinal(IL)]));
  end;

  if IL = ilUntrusted then
    ComboIntegrity.ItemIndex := 0
  else if IL <= ilLow then
    ComboIntegrity.ItemIndex := 1
  else if IL <= ilMedium then
    ComboIntegrity.ItemIndex := 2
  else if IL <= ilMediumPlus then
    ComboIntegrity.ItemIndex := 3
  else if IL <= ilHigh then
    ComboIntegrity.ItemIndex := 4
  else if IL <= ilSystem then
    ComboIntegrity.ItemIndex := 5
  else
    ComboIntegrity.ItemIndex := 6;
end;

procedure TInfoDialog.ChangedView(Sender: TObject);
var
  i: integer;
begin
  ListViewGroups.Items.BeginUpdate;
  ListViewGroups.Clear;
  with Token.Groups do
    if IsValid then
    begin
      TabGroups.Caption := Format('Groups (%d)', [Length(Value)]);

      for i := 0 to High(Value) do
      with Value[i], ListViewGroups.Items.Add do
      begin
        if ComboBoxView.ItemIndex = 0 then
          Caption := SecurityIdentifier.ToString
        else
          Caption := SecurityIdentifier.SID;
        SubItems.Add(Attributes.ToString);
      end;
    end;
  ListViewGroups.Items.EndUpdate;

  ListViewRestricted.Items.BeginUpdate;
  ListViewRestricted.Clear;
  with Token.RestrictedSids do
    if IsValid then
    begin
      TabRestricted.Caption := Format('Restricted SIDs (%d)', [Length(Value)]);

      for i := 0 to High(Value) do
      with Value[i], ListViewRestricted.Items.Add do
      begin
        if ComboBoxView.ItemIndex = 0 then
          Caption := SecurityIdentifier.ToString
        else
          Caption := SecurityIdentifier.SID;
        SubItems.Add(Attributes.ToString);
      end;
    end;
  ListViewRestricted.Items.EndUpdate;
end;

procedure TInfoDialog.ConfirmTokenClose(Sender: TObject);
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

procedure TInfoDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Token.OnIntegrityChange.Delete(ChangedIntegrity);
  Token.OnCaptionChange.Delete(ChangedCaption);
  Token.OnClose.Delete(ConfirmTokenClose);
  FormMain.OnMainFormClose.Delete(DoCloseForm);
  Action := caFree;
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  FormMain.OnMainFormClose.Add(DoCloseForm);
  Token.OnClose.Add(ConfirmTokenClose);
  Token.OnCaptionChange.Add(ChangedCaption);
  Token.OnIntegrityChange.Add(ChangedIntegrity);

  ChangedCaption(Token);
  ChangedIntegrity(Token);
  ChangedView(Token);

  EditObjAddr.Text := '0x' + IntToHex(Token.ObjAddress, 8);
  EditHandle.Text := '0x' + IntToHex(Token.Handle, -1);

  with Token.TokenTypeInfo do
    if IsValid then
      EditType.Text := Value.ToString;

  with Token.User do
    if IsValid then
    begin
      EditUser.Text := Value.Domain + '\' + Value.User;
      EditSID.Text := Value.SID;
    end;

  ListViewPrivileges.Clear;
  ListViewPrivileges.Items.BeginUpdate;
  with Token.Privileges do
    if IsValid then
    begin
      TabPrivileges.Caption := Format('Privileges (%d)', [Length(Value)]);

      for i := 0 to High(Value) do
      with Value[i], ListViewPrivileges.Items.Add do
      begin
        Caption := Name;
        SubItems.Add(Value[i].AttributesToString);
        SubItems.Add(Value[i].Description);
      end;
    end;
  ListViewPrivileges.Items.EndUpdate;
end;

end.
