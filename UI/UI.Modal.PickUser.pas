unit UI.Modal.PickUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, TU.Common, TU.Tokens,
  UI.MainForm, UI.Prototypes, UI.Prototypes.ChildForm;

type
  TDialogPickUser = class(TChildForm)
    ComboBoxSID: TComboBox;
    ButtonFilter: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonPick: TButton;
    CheckBoxMandatory: TCheckBox;
    CheckBoxDenyOnly: TCheckBox;
    CheckBoxResource: TCheckBox;
    CheckBoxEnabled: TCheckBox;
    CheckBoxEnabledByDafault: TCheckBox;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabLogon: TTabSheet;
    TabIntegrity: TTabSheet;
    CheckBoxLogon: TCheckBox;
    CheckBoxIntegrity: TCheckBox;
    CheckBoxIntegrityEnabled: TCheckBox;
    CheckBoxOwner: TCheckBox;
    ComboBoxLogonId: TComboBox;
    ComboBoxIntegrity: TComboBox;
    procedure ButtonPickClick(Sender: TObject);
    procedure ComboBoxSIDChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControlChange(Sender: TObject);
    procedure ComboBoxLogonIdChange(Sender: TObject);
    procedure ComboBoxIntegrityChange(Sender: TObject);
  private
    FSelectedGroup: TSecurityIdentifier;
    FValidGroup: Boolean;
    LogonIDSource: TLogonSessionSource;
    procedure ObjPickerCallback(UserName: String);
    function GetAttributes: TGroupAttributes;
    procedure SetAttributes(const Value: TGroupAttributes);
    procedure SetSID(const Value: TSecurityIdentifier);
    procedure DoDisableAttributes;
    function LuidToLogonIdString(Value: LUID): String;
  public
    class function Execute(AOwner: TComponent;
      DisableAttributes: Boolean = False): TGroup; overload;
    class function Execute(AOwner: TComponent; Group: TGroup;
      DisableAttributes: Boolean = False): TGroup; overload;
    property Attributes: TGroupAttributes read GetAttributes write SetAttributes;
  end;

implementation

uses
  TU.ObjPicker;

{$R *.dfm}

procedure TDialogPickUser.ButtonOKClick(Sender: TObject);
begin
  if not FValidGroup then
    FSelectedGroup := TSecurityIdentifier.CreateFromString(ComboBoxSID.Text);
  ModalResult := mrOk;
end;

procedure TDialogPickUser.ButtonPickClick(Sender: TObject);
begin
  CallObjectPicker(Handle, ObjPickerCallback);
end;

procedure TDialogPickUser.ComboBoxLogonIdChange(Sender: TObject);
var
  Value: UInt64;
begin
  CheckBoxLogon.Checked := True;
  if ComboBoxLogonId.ItemIndex <> -1 then
    ComboBoxSID.Text := LuidToLogonIdString(LogonIDSource.SelectedLogonSession)
  else if TryStrToUInt64Ex(ComboBoxLogonId.Text, Value) then
    ComboBoxSID.Text := LuidToLogonIdString(PLUID(@Value)^);
end;

procedure TDialogPickUser.ComboBoxSIDChange(Sender: TObject);
begin
  FValidGroup := False;
end;

procedure TDialogPickUser.DoDisableAttributes;
begin
  CheckBoxMandatory.Enabled := False;
  CheckBoxEnabledByDafault.Enabled := False;
  CheckBoxEnabled.Enabled := False;
  CheckBoxOwner.Enabled := False;
  CheckBoxDenyOnly.Enabled := False;
  CheckBoxIntegrity.Enabled := False;
  CheckBoxIntegrityEnabled.Enabled := False;
  CheckBoxResource.Enabled := False;
  CheckBoxLogon.Enabled := False;
end;

class function TDialogPickUser.Execute(AOwner: TComponent;
  Group: TGroup; DisableAttributes: Boolean): TGroup;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    SetSID(Group.SecurityIdentifier);
    SetAttributes(Group.Attributes);

    if DisableAttributes then
      DoDisableAttributes;

    ShowModal;
    Result.SecurityIdentifier := FSelectedGroup;
    Result.Attributes := GetAttributes;
  end;
end;

class function TDialogPickUser.Execute(AOwner: TComponent;
  DisableAttributes: Boolean): TGroup;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    if DisableAttributes then
    begin
      SetAttributes(GroupEnabled);
      DoDisableAttributes;
    end;

    ShowModal;
    Result.SecurityIdentifier := FSelectedGroup;
    Result.Attributes := GetAttributes;
  end;
end;

procedure TDialogPickUser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LogonIDSource.Free;
end;

procedure TDialogPickUser.FormCreate(Sender: TObject);
begin
  LogonIDSource := TLogonSessionSource.Create(ComboBoxLogonId);
end;

function TDialogPickUser.GetAttributes: TGroupAttributes;
var
  BitwiseResult: Integer;
begin
  BitwiseResult := 0;

  if CheckBoxMandatory.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupMandatory);
  if CheckBoxEnabledByDafault.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupEnabledByDefault);
  if CheckBoxEnabled.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupEnabled);
  if CheckBoxOwner.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupOwner);
  if CheckBoxDenyOnly.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupUforDenyOnly);
  if CheckBoxIntegrity.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupIntegrity);
  if CheckBoxIntegrityEnabled.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupIntegrityEnabled);
  if CheckBoxResource.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupResource);
  if CheckBoxLogon.Checked then
    BitwiseResult := BitwiseResult or Integer(GroupLogonId);

  Result := TGroupAttributes(BitwiseResult);
end;

procedure TDialogPickUser.ComboBoxIntegrityChange(Sender: TObject);
begin
  CheckBoxIntegrity.Checked := True;
  CheckBoxIntegrityEnabled.Checked := True;
end;

function TDialogPickUser.LuidToLogonIdString(Value: LUID): String;
begin
  {$R-}
  Result := 'S-1-5-5-' + UIntToStr(Value.HighPart) +'-' + UIntToStr(Value.LowPart);
  {$R+}
end;

procedure TDialogPickUser.ObjPickerCallback(UserName: String);
begin
  SetSID(TSecurityIdentifier.CreateFromString(UserName));
end;

procedure TDialogPickUser.PageControlChange(Sender: TObject);
begin
  ComboBoxSID.Enabled := PageControl.ActivePageIndex = 0;
  ButtonFilter.Enabled := ComboBoxSID.Enabled;
  ButtonPick.Enabled := ComboBoxSID.Enabled;
end;

procedure TDialogPickUser.SetAttributes(const Value: TGroupAttributes);
begin
  CheckBoxMandatory.Checked := Value.Contain(GroupMandatory);
  CheckBoxEnabledByDafault.Checked := Value.Contain(GroupEnabledByDefault);
  CheckBoxEnabled.Checked := Value.Contain(GroupEnabled);
  CheckBoxOwner.Checked := Value.Contain(GroupOwner);
  CheckBoxDenyOnly.Checked := Value.Contain(GroupUforDenyOnly);
  CheckBoxIntegrity.Checked := Value.Contain(GroupIntegrity);
  CheckBoxIntegrityEnabled.Checked := Value.Contain(GroupIntegrityEnabled);
  CheckBoxResource.Checked := Value.Contain(GroupResource);
  CheckBoxLogon.Checked := Value.Contain(GroupLogonId);
end;

procedure TDialogPickUser.SetSID(const Value: TSecurityIdentifier);
begin
  FValidGroup := True;
  FSelectedGroup := Value;
  ComboBoxSID.Text := FSelectedGroup.ToString;
end;

end.
