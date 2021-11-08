unit UI.Modal.PickUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, TU.Tokens, TU.Tokens.Types, NtUtils,
  UI.MainForm, UI.Prototypes, UI.Prototypes.Forms, NtUtils.Security.Sid,
  Ntapi.ntseapi;

type
  TCheckBoxMapping = record
    CheckBox: TCheckBox;
    Attribute: Cardinal;
    procedure Create(CheckBox: TCheckBox; Attribute: Cardinal);
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
    class procedure PickEditMultiple(AOwner: TComponent; Groups: TArray<TGroup>;
      out AttributesToAdd, AttributesToDelete: TGroupAttributes);
  end;

implementation

uses
  TU.ObjPicker, UI.Modal.ComboDlg, Ntapi.WinNt, UI.Helper,
  NtUtils.Lsa.Sid, NtUiLib.Errors;

{$R *.dfm}

{ TCheckBoxMapping }

procedure TCheckBoxMapping.Create;
begin
  Self.CheckBox := CheckBox;
  Self.Attribute := Attribute;
end;

{ TDialogPickUser }

procedure TDialogPickUser.ButtonIntegrityClick(Sender: TObject);
var
  Sid: ISid;
begin
  RtlxCreateSid(Sid, SECURITY_MANDATORY_LABEL_AUTHORITY,
    [TComboDialog.PickIntegrity(Self)]).RaiseOnError;

  SetSelectedGroup(Sid);
  SetAttributes(SE_GROUP_INTEGRITY or SE_GROUP_INTEGRITY_ENABLED);
end;

procedure TDialogPickUser.ButtonOKClick(Sender: TObject);
begin
  if ComboBoxSID.Enabled and not IsValidGroup then
    LsaxLookupNameOrSddl(ComboBoxSID.Text, SelectedGroup).RaiseOnError;

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
var
  Sid: ISid;
begin
  LsaxLookupNameOrSddl(UserName, Sid).RaiseOnError;
  SetSelectedGroup(Sid);
end;

class procedure TDialogPickUser.PickEditMultiple(AOwner: TComponent;
  Groups: TArray<TGroup>; out AttributesToAdd, AttributesToDelete:
  TGroupAttributes);
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
      if BitwiseAnd and Mapping[i].Attribute <> 0 then
        Mapping[i].CheckBox.SetStateEx(cbChecked) // All groups contain it
      else if BitwiseOr and Mapping[i].Attribute <> 0 then
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
    SetSelectedGroup(Group.Sid);
    SetAttributes(Group.Attributes);

    if DisableAttributes then
      DoDisableAttributes;

    ShowModal;
    Result.Sid := SelectedGroup;
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
    Result.Sid := SelectedGroup;
    Result.Attributes := GetAttributes;
  end;
end;

procedure TDialogPickUser.SetAttributes(Value: Cardinal);
var
  i: Integer;
begin
  for i := 0 to High(Mapping) do
    Mapping[i].CheckBox.SetCheckedEx(Value and Mapping[i].Attribute <> 0);
end;

procedure TDialogPickUser.SetSelectedGroup(Value: ISid);
begin
  IsValidGroup := True;
  SelectedGroup := Value;
  ComboBoxSID.Text := LsaxSidToString(SelectedGroup.Data);
end;

end.
