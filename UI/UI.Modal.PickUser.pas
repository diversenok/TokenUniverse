unit UI.Modal.PickUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, TU.Tokens, TU.Tokens.Types,
  UI.MainForm, UI.Prototypes, UI.Prototypes.ChildForm, NtUtils.Security.Sid;

type
  TCheckBoxMapping = record
    CheckBox: TCheckBox;
    Attribute: Cardinal;
    procedure Create(CheckBox: TCheckBox; Attribute: Cardinal);
  end;

  // HACK: Change of state should not issue OnClick event
  TCheckBoxHack = class helper for TCheckBox
    procedure SetStateEx(Value: TCheckBoxState);
    procedure SetCheckedEx(Value: Boolean);
  end;

  TDialogPickUser = class(TChildForm)
    ComboBoxSID: TComboBox;
    ButtonFilter: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonPick: TButton;
    GroupBoxMain: TGroupBox;
    CheckBoxEnabled: TCheckBox;
    CheckBoxEnabledByDafault: TCheckBox;
    CheckBoxMandatory: TCheckBox;
    CheckBoxDenyOnly: TCheckBox;
    CheckBoxOwner: TCheckBox;
    GroupBoxAdditional: TGroupBox;
    CheckBoxIntegrityEnabled: TCheckBox;
    CheckBoxIntegrity: TCheckBox;
    CheckBoxResource: TCheckBox;
    CheckBoxLogon: TCheckBox;
    ButtonIntegrity: TButton;
    ButtonLogonID: TButton;
    procedure ButtonPickClick(Sender: TObject);
    procedure ComboBoxSIDChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonIntegrityClick(Sender: TObject);
    procedure CheckBoxDenyOnlyClick(Sender: TObject);
    procedure CheckBoxMandatoryClick(Sender: TObject);
    procedure CheckBoxEnabledClick(Sender: TObject);
    procedure CheckBoxEnabledByDafaultClick(Sender: TObject);
  private
    SelectedGroup: ISid;
    IsValidGroup: Boolean;
    Mapping: array of TCheckBoxMapping;
    procedure ObjPickerCallback(UserName: String);
    function GetAttributes: Cardinal;
    procedure SetAttributes(Value: Cardinal);
    procedure SetSelectedGroup(Value: ISid);
    procedure DoDisableAttributes;
  public
    class function PickNew(AOwner: TComponent;
      DisableAttributes: Boolean = False): TGroup;
    class function PickEditOne(AOwner: TComponent; const Group: TGroup;
      DisableAttributes: Boolean = False): TGroup;
    class procedure PickEditMultiple(AOwner: TComponent; Groups: TGroupArray;
      out AttributesToAdd, AttributesToDelete: Cardinal);
  end;

implementation

uses
  TU.ObjPicker, UI.Modal.ComboDlg, DelphiUtils.Strings, Winapi.WinNt;

{$R *.dfm}

{ TCheckBoxMapping }

procedure TCheckBoxMapping.Create(CheckBox: TCheckBox;
  Attribute: Cardinal);
begin
  Self.CheckBox := CheckBox;
  Self.Attribute := Attribute;
end;

{ TCheckBoxHack }

procedure TCheckBoxHack.SetCheckedEx(Value: Boolean);
begin
  ClicksDisabled := True;
  Checked := Value;
  ClicksDisabled := False;
end;

procedure TCheckBoxHack.SetStateEx(Value: TCheckBoxState);
begin
  ClicksDisabled := True;
  State := Value;
  ClicksDisabled := False;
end;

{ TDialogPickUser }

procedure TDialogPickUser.ButtonIntegrityClick(Sender: TObject);
begin
  SetSelectedGroup(TSid.CreateFromString(Format('S-1-16-%d',
    [Cardinal(TComboDialog.PickIntegrity(Self))])));

  SetAttributes(SE_GROUP_INTEGRITY or SE_GROUP_INTEGRITY_ENABLED);
end;

procedure TDialogPickUser.ButtonOKClick(Sender: TObject);
begin
  if ComboBoxSID.Enabled and not IsValidGroup then
    SelectedGroup := TSid.CreateFromString(ComboBoxSID.Text);
  ModalResult := mrOk;
end;

procedure TDialogPickUser.ButtonPickClick(Sender: TObject);
begin
  CallObjectPicker(Handle, ObjPickerCallback);
end;

procedure TDialogPickUser.CheckBoxDenyOnlyClick(Sender: TObject);
begin
  if CheckBoxDenyOnly.Checked then
  begin
    CheckBoxMandatory.SetCheckedEx(False);
    CheckBoxEnabled.SetCheckedEx(False);
    CheckBoxEnabledByDafault.SetCheckedEx(False);
  end;
end;

procedure TDialogPickUser.CheckBoxEnabledByDafaultClick(Sender: TObject);
begin
  if CheckBoxEnabledByDafault.Checked then
    CheckBoxDenyOnly.SetCheckedEx(False)
  else
    CheckBoxMandatory.SetCheckedEx(False);
end;

procedure TDialogPickUser.CheckBoxEnabledClick(Sender: TObject);
begin
  if CheckBoxEnabled.Checked then
    CheckBoxDenyOnly.SetCheckedEx(False)
  else
    CheckBoxMandatory.SetCheckedEx(False);
end;

procedure TDialogPickUser.CheckBoxMandatoryClick(Sender: TObject);
begin
  if CheckBoxMandatory.Checked then
  begin
    CheckBoxEnabled.SetCheckedEx(True);
    CheckBoxEnabledByDafault.SetCheckedEx(True);
    CheckBoxDenyOnly.SetCheckedEx(False);
  end;
end;

procedure TDialogPickUser.ComboBoxSIDChange(Sender: TObject);
begin
  IsValidGroup := False;
end;

procedure TDialogPickUser.DoDisableAttributes;
var
  i: Integer;
begin
  for i := 0 to High(Mapping) do
    Mapping[i].CheckBox.Enabled := False;
end;

procedure TDialogPickUser.FormCreate(Sender: TObject);
begin
  SetLength(Mapping, 9);
  Mapping[0].Create(CheckBoxMandatory, SE_GROUP_MANDATORY);
  Mapping[1].Create(CheckBoxEnabledByDafault, SE_GROUP_ENABLED_BY_DEFAULT);
  Mapping[2].Create(CheckBoxEnabled, SE_GROUP_ENABLED);
  Mapping[3].Create(CheckBoxOwner, SE_GROUP_OWNER);
  Mapping[4].Create(CheckBoxDenyOnly, SE_GROUP_USE_FOR_DENY_ONLY);
  Mapping[5].Create(CheckBoxIntegrity, SE_GROUP_INTEGRITY);
  Mapping[6].Create(CheckBoxIntegrityEnabled, SE_GROUP_INTEGRITY_ENABLED);
  Mapping[7].Create(CheckBoxResource, SE_GROUP_RESOURCE);
  Mapping[8].Create(CheckBoxLogon, SE_GROUP_LOGON_ID);
end;

function TDialogPickUser.GetAttributes: Cardinal;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to High(Mapping) do
    if Mapping[i].CheckBox.Checked then
      Result := Result or Mapping[i].Attribute;
end;

procedure TDialogPickUser.ObjPickerCallback(UserName: String);
begin
  SetSelectedGroup(TSid.CreateFromString(UserName));
end;

class procedure TDialogPickUser.PickEditMultiple(AOwner: TComponent;
  Groups: TGroupArray; out AttributesToAdd, AttributesToDelete: Cardinal);
var
  BitwiseAnd, BitwiseOr: Cardinal;
  i: Integer;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    ComboBoxSID.Enabled := False;
    ButtonFilter.Enabled := False;
    ButtonPick.Enabled := False;
    ButtonIntegrity.Enabled := False;
    ComboBoxSID.Text := '< Multiple values >';

    BitwiseOr := 0;
    BitwiseAnd := Cardinal(not 0);

    // Find out which flags present in all of the groups and which are only in
    // some of them
    for i := 0 to High(Groups) do
    begin
      BitwiseOr := BitwiseOr or Groups[i].Attributes;
      BitwiseAnd := BitwiseAnd and Groups[i].Attributes;
    end;

    // Set appropriate checkbox states
    for i := 0 to High(Mapping) do
      if Contains(BitwiseAnd, Mapping[i].Attribute) then
        Mapping[i].CheckBox.SetStateEx(cbChecked) // All groups contain it
      else if Contains(BitwiseOr, Mapping[i].Attribute) then
      begin
        Mapping[i].CheckBox.AllowGrayed := True;
        Mapping[i].CheckBox.SetStateEx(cbGrayed); // Only some of them
      end
      else
        Mapping[i].CheckBox.SetStateEx(cbUnchecked); // None of them

    // Show the dialog and wait
    ShowModal;

    AttributesToAdd := 0;
    AttributesToDelete := 0;

    // Collect the attributes
    for i := 0 to High(Mapping) do
      case Mapping[i].CheckBox.State of
        cbUnchecked:
          AttributesToDelete := AttributesToDelete or Mapping[i].Attribute;
        cbChecked:
          AttributesToAdd := AttributesToAdd or Mapping[i].Attribute;
      end;
  end;
end;

class function TDialogPickUser.PickEditOne(AOwner: TComponent;
  const Group: TGroup; DisableAttributes: Boolean): TGroup;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    SetSelectedGroup(Group.SecurityIdentifier);
    SetAttributes(Group.Attributes);

    if DisableAttributes then
      DoDisableAttributes;

    ShowModal;
    Result.SecurityIdentifier := SelectedGroup;
    Result.Attributes := GetAttributes;
  end;
end;

class function TDialogPickUser.PickNew(AOwner: TComponent;
  DisableAttributes: Boolean): TGroup;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    if DisableAttributes then
    begin
      SetAttributes(SE_GROUP_ENABLED);
      DoDisableAttributes;
    end;

    ShowModal;
    Result.SecurityIdentifier := SelectedGroup;
    Result.Attributes := GetAttributes;
  end;
end;

procedure TDialogPickUser.SetAttributes(Value: Cardinal);
var
  i: Integer;
begin
  for i := 0 to High(Mapping) do
    Mapping[i].CheckBox.SetCheckedEx(Contains(Value, Mapping[i].Attribute));
end;

procedure TDialogPickUser.SetSelectedGroup(Value: ISid);
begin
  IsValidGroup := True;
  SelectedGroup := Value;
  ComboBoxSID.Text := SelectedGroup.Lookup.FullName;
end;

end.
