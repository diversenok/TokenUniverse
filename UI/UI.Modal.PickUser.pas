unit UI.Modal.PickUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, TU.Common, TU.Tokens, TU.Tokens.Types,
  UI.MainForm, UI.Prototypes, UI.Prototypes.ChildForm;

type
  TCheckBoxMapping = record
    CheckBox: TCheckBox;
    Attribute: TGroupAttributes;
    procedure Create(CheckBox: TCheckBox; Attribute: TGroupAttributes);
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
  private
    SelectedGroup: TSecurityIdentifier;
    IsValidGroup: Boolean;
    Mapping: array of TCheckBoxMapping;
    procedure ObjPickerCallback(UserName: String);
    function GetAttributes: TGroupAttributes;
    procedure SetAttributes(const Value: TGroupAttributes);
    procedure SetSelectedGroup(const Value: TSecurityIdentifier);
    procedure DoDisableAttributes;
  public
    class function PickNew(AOwner: TComponent;
      DisableAttributes: Boolean = False): TGroup;
    class function PickEditOne(AOwner: TComponent; Group: TGroup;
      DisableAttributes: Boolean = False): TGroup;
    class procedure PickEditMultiple(AOwner: TComponent; Groups: TGroupArray;
      out AttributesToAdd, AttributesToDelete: Cardinal);
  end;

implementation

uses
  TU.ObjPicker, UI.Modal.ComboDlg;

{$R *.dfm}

{ TCheckBoxMapping }

procedure TCheckBoxMapping.Create(CheckBox: TCheckBox;
  Attribute: TGroupAttributes);
begin
  Self.CheckBox := CheckBox;
  Self.Attribute := Attribute;
end;

{ TDialogPickUser }

procedure TDialogPickUser.ButtonIntegrityClick(Sender: TObject);
begin
  SetSelectedGroup(TSecurityIdentifier.CreateFromString(Format('S-1-16-%d',
    [Cardinal(TComboDialog.PickIntegrity(Self))])));

  SetAttributes(TGroupAttributes(Cardinal(GroupIntegrity) or
    Cardinal(GroupIntegrityEnabled)));
end;

procedure TDialogPickUser.ButtonOKClick(Sender: TObject);
begin
  if ComboBoxSID.Enabled and not IsValidGroup then
    SelectedGroup := TSecurityIdentifier.CreateFromString(ComboBoxSID.Text);
  ModalResult := mrOk;
end;

procedure TDialogPickUser.ButtonPickClick(Sender: TObject);
begin
  CallObjectPicker(Handle, ObjPickerCallback);
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
  Mapping[0].Create(CheckBoxMandatory, GroupMandatory);
  Mapping[1].Create(CheckBoxEnabledByDafault, GroupEnabledByDefault);
  Mapping[2].Create(CheckBoxEnabled, GroupEnabled);
  Mapping[3].Create(CheckBoxOwner, GroupOwner);
  Mapping[4].Create(CheckBoxDenyOnly, GroupUforDenyOnly);
  Mapping[5].Create(CheckBoxIntegrity, GroupIntegrity);
  Mapping[6].Create(CheckBoxIntegrityEnabled, GroupIntegrityEnabled);
  Mapping[7].Create(CheckBoxResource, GroupResource);
  Mapping[8].Create(CheckBoxLogon, GroupLogonId);
end;

function TDialogPickUser.GetAttributes: TGroupAttributes;
var
  BitwiseResult, i: Integer;
begin
  BitwiseResult := 0;

  for i := 0 to High(Mapping) do
    if Mapping[i].CheckBox.Checked then
      BitwiseResult := BitwiseResult or Integer(Mapping[i].Attribute);

  Result := TGroupAttributes(BitwiseResult);
end;

procedure TDialogPickUser.ObjPickerCallback(UserName: String);
begin
  SetSelectedGroup(TSecurityIdentifier.CreateFromString(UserName));
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
      BitwiseOr := BitwiseOr or Cardinal(Groups[i].Attributes);
      BitwiseAnd := BitwiseAnd and Cardinal(Groups[i].Attributes);
    end;

    // Set appropriate checkbox states
    for i := 0 to High(Mapping) do
      if TGroupAttributes(BitwiseAnd).Contain(Mapping[i].Attribute) then
        Mapping[i].CheckBox.State := cbChecked // All groups contain it
      else if TGroupAttributes(BitwiseOr).Contain(Mapping[i].Attribute) then
      begin
        Mapping[i].CheckBox.AllowGrayed := True;
        Mapping[i].CheckBox.State :=  cbGrayed; // Only some of then
      end
      else
        Mapping[i].CheckBox.State := cbUnchecked; // None of them

    // Show the dialog and wait
    ShowModal;

    AttributesToAdd := 0;
    AttributesToDelete := 0;

    // Collect the attributes
    for i := 0 to High(Mapping) do
      case Mapping[i].CheckBox.State of
        cbUnchecked:
          AttributesToDelete := AttributesToDelete or
            Cardinal(Mapping[i].Attribute);
        cbChecked:
          AttributesToAdd := AttributesToAdd or
            Cardinal(Mapping[i].Attribute);
      end;
  end;
end;

class function TDialogPickUser.PickEditOne(AOwner: TComponent; Group: TGroup;
  DisableAttributes: Boolean): TGroup;
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
      SetAttributes(GroupEnabled);
      DoDisableAttributes;
    end;

    ShowModal;
    Result.SecurityIdentifier := SelectedGroup;
    Result.Attributes := GetAttributes;
  end;
end;

procedure TDialogPickUser.SetAttributes(const Value: TGroupAttributes);
var
  i: Integer;
begin
  for i := 0 to High(Mapping) do
    Mapping[i].CheckBox.Checked := Value.Contain(Mapping[i].Attribute);
end;

procedure TDialogPickUser.SetSelectedGroup(const Value: TSecurityIdentifier);
begin
  IsValidGroup := True;
  SelectedGroup := Value;
  ComboBoxSID.Text := SelectedGroup.ToString;
end;

// TODO: Checkbox OnClick events to enable dependent attributes

end.
