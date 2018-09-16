unit UI.Modal.PickUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, UI.MainForm, TU.Common, TU.Tokens, UI.Prototypes.ChildForm;

type
  TDialogPickUser = class(TChildForm)
    ComboBoxSID: TComboBox;
    ButtonFilter: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonPick: TButton;
    GroupBoxAttributes: TGroupBox;
    CheckBoxMandatory: TCheckBox;
    CheckBoxDenyOnly: TCheckBox;
    CheckBoxOwner: TCheckBox;
    CheckBoxResource: TCheckBox;
    CheckBoxEnabled: TCheckBox;
    CheckBoxEnabledByDafault: TCheckBox;
    CheckBoxIntegrity: TCheckBox;
    CheckBoxIntegrityEnabled: TCheckBox;
    CheckBoxLogon: TCheckBox;
    procedure ButtonPickClick(Sender: TObject);
    procedure ComboBoxSIDChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FSelectedGroup: TSecurityIdentifier;
    FValidGroup: Boolean;
    procedure ObjPickerCallback(UserName: String);
    function GetAttributes: TGroupAttributes;
  public
    class function ExecuteNew(AOwner: TComponent): TGroup;
    property Attributes: TGroupAttributes read GetAttributes;
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

procedure TDialogPickUser.ComboBoxSIDChange(Sender: TObject);
begin
  FValidGroup := False;
end;

class function TDialogPickUser.ExecuteNew(AOwner: TComponent): TGroup;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    ShowModal;
    Result.SecurityIdentifier := FSelectedGroup;
    Result.Attributes := GetAttributes;
  end;
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

procedure TDialogPickUser.ObjPickerCallback(UserName: String);
begin
  FSelectedGroup := TSecurityIdentifier.CreateFromString(UserName);
  FValidGroup := True;
  ComboBoxSID.Text := FSelectedGroup.ToString;
end;

end.
