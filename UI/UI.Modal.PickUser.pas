unit UI.Modal.PickUser;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, NtUtils, UI.Prototypes.Forms,
  Ntapi.ntseapi, UI.Prototypes.Sid.Edit;

type
  TCheckBoxMapping = record
    CheckBox: TCheckBox;
    Attribute: Cardinal;
    procedure Create(CheckBox: TCheckBox; Attribute: Cardinal);
  end;

  TDialogPickUser = class(TChildForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
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
    ButtonLogonSID: TButton;
    SidEditor: TSidEditor;
    procedure FormCreate(Sender: TObject);
    procedure ButtonIntegrityClick(Sender: TObject);
    procedure CheckBoxDenyOnlyClick(Sender: TObject);
    procedure CheckBoxMandatoryClick(Sender: TObject);
    procedure CheckBoxEnabledClick(Sender: TObject);
    procedure CheckBoxEnabledByDafaultClick(Sender: TObject);
    procedure ButtonLogonSIDClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    Mapping: array of TCheckBoxMapping;
    function GetAttributes: TGroupAttributes;
    procedure SetAttributes(Value: TGroupAttributes);
    procedure DoDisableAttributes;
  public
    property Attributes: TGroupAttributes read GetAttributes write SetAttributes;
    class function PickNew(AOwner: TComponent;
      DisableAttributes: Boolean = False): TGroup;
    class function PickEditOne(AOwner: TComponent; const Group: TGroup;
      DisableAttributes: Boolean = False): TGroup;
    class procedure PickEditMultiple(AOwner: TComponent; const Groups: TArray<TGroup>;
      out AttributesToAdd, AttributesToDelete: TGroupAttributes);
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.ntpsapi, NtUtils.Security.Sid, NtUtils.WinUser, UI.Helper,
  UI.Modal.Integrity, NtUiLib.Errors;

{$R *.dfm}

{ TCheckBoxMapping }

procedure TCheckBoxMapping.Create;
begin
  Self.CheckBox := CheckBox;
  Self.Attribute := Attribute;
end;

{ TDialogPickUser }

procedure TDialogPickUser.ButtonIntegrityClick;
var
  CurrentRID: Cardinal;
  Sid: ISid;
begin
  CurrentRID := SECURITY_MANDATORY_MEDIUM_RID;

  // Set the slider to the current integrity when selected
  if SidEditor.TryGetSid(Sid).IsSuccess and
    (RtlxIdentifierAuthoritySid(Sid) =
    SECURITY_MANDATORY_LABEL_AUTHORITY) then
    CurrentRID := RtlxRidSid(Sid);

  RtlxCreateSid(Sid, SECURITY_MANDATORY_LABEL_AUTHORITY,
    [TIntegrityPicker.Choose(Self, CurrentRID)]).RaiseOnError;

  SidEditor.Sid := Sid;
  Attributes := SE_GROUP_INTEGRITY or SE_GROUP_INTEGRITY_ENABLED;
  ButtonOK.SetFocus;
end;

procedure TDialogPickUser.ButtonLogonSIDClick;
var
  Sid: ISid;
begin
  UsrxQuerySid(GetThreadDesktop(NtCurrentThreadId), Sid).RaiseOnError;

  if Assigned(Sid) then
    SidEditor.Sid := Sid
  else
    raise Exception.Create('The current desktop does not have a logon SID.');

  Attributes := SE_GROUP_LOGON_ID or SE_GROUP_ENABLED or
    SE_GROUP_ENABLED_BY_DEFAULT;

  ButtonOK.SetFocus;
end;

procedure TDialogPickUser.ButtonOKClick;
var
  Sid: ISid;
begin
  // Prevent closing unless the SID is valid
  if SidEditor.Enabled then
    SidEditor.TryGetSid(Sid).RaiseOnError;

  ModalResult := mrOk;
end;

procedure TDialogPickUser.CheckBoxDenyOnlyClick;
begin
  if CheckBoxDenyOnly.Checked then
  begin
    CheckBoxMandatory.SetCheckedEx(False);
    CheckBoxEnabled.SetCheckedEx(False);
    CheckBoxEnabledByDafault.SetCheckedEx(False);
  end;
end;

procedure TDialogPickUser.CheckBoxEnabledByDafaultClick;
begin
  if CheckBoxEnabledByDafault.Checked then
    CheckBoxDenyOnly.SetCheckedEx(False)
  else
    CheckBoxMandatory.SetCheckedEx(False);
end;

procedure TDialogPickUser.CheckBoxEnabledClick;
begin
  if CheckBoxEnabled.Checked then
    CheckBoxDenyOnly.SetCheckedEx(False)
  else
    CheckBoxMandatory.SetCheckedEx(False);
end;

procedure TDialogPickUser.CheckBoxMandatoryClick;
begin
  if CheckBoxMandatory.Checked then
  begin
    CheckBoxEnabled.SetCheckedEx(True);
    CheckBoxEnabledByDafault.SetCheckedEx(True);
    CheckBoxDenyOnly.SetCheckedEx(False);
  end;
end;

procedure TDialogPickUser.DoDisableAttributes;
var
  i: Integer;
begin
  for i := 0 to High(Mapping) do
    Mapping[i].CheckBox.Enabled := False;
end;

procedure TDialogPickUser.FormCreate;
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

function TDialogPickUser.GetAttributes;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to High(Mapping) do
    if Mapping[i].CheckBox.Checked then
      Result := Result or Mapping[i].Attribute;
end;

class procedure TDialogPickUser.PickEditMultiple;
var
  BitwiseAnd, BitwiseOr: Cardinal;
  i: Integer;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    SidEditor.Enabled := False;
    SidEditor.tbxSid.Enabled := False;
    SidEditor.btnDsPicker.Enabled := False;
    SidEditor.btnCheatsheet.Enabled := False;
    ButtonIntegrity.Enabled := False;
    ButtonLogonSID.Enabled := False;
    SidEditor.tbxSid.Text := '< Multiple values >';

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

class function TDialogPickUser.PickEditOne;
begin
  with TDialogPickUser.Create(AOwner) do
  begin
    SidEditor.Sid := Group.Sid;
    Attributes := Group.Attributes;

    if DisableAttributes then
      DoDisableAttributes;

    ShowModal;
    Result.Sid := SidEditor.Sid;
    Result.Attributes := GetAttributes;
  end;
end;

class function TDialogPickUser.PickNew;
begin
  with TDialogPickUser.CreateChild(AOwner, cfmApplication) do
  begin
    if DisableAttributes then
    begin
      Attributes := SE_GROUP_ENABLED;
      DoDisableAttributes;
    end;

    ShowModal;
    Result.Sid := SidEditor.Sid;
    Result.Attributes := GetAttributes;
  end;
end;

procedure TDialogPickUser.SetAttributes;
var
  i: Integer;
begin
  for i := 0 to High(Mapping) do
    Mapping[i].CheckBox.SetCheckedEx(Value and Mapping[i].Attribute <> 0);
end;

end.
