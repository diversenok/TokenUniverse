unit UI.Modal.Integrity;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Ntapi.WinNt, UI.Prototypes.Forms;

type
  TIntegrityPicker = class(TChildForm)
    TrackBar: TTrackBar;
    ComboBox: TComboBox;
    btnCancel: TButton;
    btnOk: TButton;
    lblUntrusted: TLabel;
    lblSystem: TLabel;
    lblMedium: TLabel;
    lblLow: TLabel;
    lblHigh: TLabel;
    procedure TrackBarChange(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
  private
    Value: Cardinal;
    procedure UpdateComboBoxValue;
    procedure UpdateTrackBarValue;
  public
    class function Choose(
      AOwner: TComponent;
      InitialValue: Cardinal = SECURITY_MANDATORY_MEDIUM_RID
    ): Cardinal; static;
  end;

implementation

uses
  NtUtils.SysUtils;

{$R *.dfm}

class function TIntegrityPicker.Choose;
begin
  with TIntegrityPicker.CreateChild(AOwner, cfmApplication) do
  begin
    Value := InitialValue;
    UpdateComboBoxValue;
    UpdateTrackBarValue;

    ShowModal;
    Result := Value;
  end;
end;

procedure TIntegrityPicker.ComboBoxChange;
begin
  case ComboBox.ItemIndex of
    0: Value := SECURITY_MANDATORY_UNTRUSTED_RID;
    1: Value := SECURITY_MANDATORY_LOW_RID;
    2: Value := SECURITY_MANDATORY_MEDIUM_RID;
    3: Value := SECURITY_MANDATORY_MEDIUM_PLUS_RID;
    4: Value := SECURITY_MANDATORY_HIGH_RID;
    5: Value := SECURITY_MANDATORY_SYSTEM_RID;
    6: Value := SECURITY_MANDATORY_PROTECTED_PROCESS_RID;
  else
    if not RtlxStrToUInt(ComboBox.Text, Value) then
      Exit;
  end;

  UpdateTrackBarValue;
end;

procedure TIntegrityPicker.TrackBarChange;
begin
  Value := TrackBar.Position;

  // Make known values slightly sticky
  if $800 - Abs(Integer(Value and $FFF) - $800) < $1C000 / TrackBar.Width then
    Value := Round(Value / $1000) * $1000;

  UpdateTrackBarValue;
  UpdateComboBoxValue;
end;

procedure TIntegrityPicker.UpdateComboBoxValue;
begin
  ComboBox.OnChange := nil;
  try
    ComboBox.ItemIndex := -1;
    case Value of
      SECURITY_MANDATORY_UNTRUSTED_RID:         ComboBox.ItemIndex := 0;
      SECURITY_MANDATORY_LOW_RID:               ComboBox.ItemIndex := 1;
      SECURITY_MANDATORY_MEDIUM_RID:            ComboBox.ItemIndex := 2;
      SECURITY_MANDATORY_MEDIUM_PLUS_RID:       ComboBox.ItemIndex := 3;
      SECURITY_MANDATORY_HIGH_RID:              ComboBox.ItemIndex := 4;
      SECURITY_MANDATORY_SYSTEM_RID:            ComboBox.ItemIndex := 5;
      SECURITY_MANDATORY_PROTECTED_PROCESS_RID: ComboBox.ItemIndex := 6;
    else
      ComboBox.Text := RtlxUIntToStr(Value, 16, 4);
    end;
  finally
    ComboBox.OnChange := ComboBoxChange;
  end;
end;

procedure TIntegrityPicker.UpdateTrackBarValue;
begin
  TrackBar.OnChange := nil;
  try
    TrackBar.Position := Value;
  finally
    TrackBar.OnChange := TrackBarChange;
  end;
end;

end.
