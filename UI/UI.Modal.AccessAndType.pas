unit UI.Modal.AccessAndType;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls, TU.Tokens,
  UI.Prototypes.Forms, VclEx.ListView, TU.Tokens.Types,
  UI.Prototypes.AccessMask;

type
  TDialogAccessAndType = class(TChildForm)
    RadioButtonPrimary: TRadioButton;
    GroupBoxType: TGroupBox;
    RadioButtonAnonymous: TRadioButton;
    RadioButtonIdentification: TRadioButton;
    RadioButtonImpersonation: TRadioButton;
    RadioButtonDelegation: TRadioButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxEffective: TCheckBox;
    GroupBoxAccess: TGroupBox;
    AccessMaskFrame: TAccessMaskFrame;
    procedure RadioButtonClick(Sender: TObject);
  private
    FSelectedType: TTokenTypeEx;
    procedure SetSelectedType(const Value: TTokenTypeEx);
  protected
    procedure DoCreate; override;
  public
    property SelectedTokenType: TTokenTypeEx read FSelectedType write
      SetSelectedType;
    class function ExecuteDuplication(AOwner: TComponent; Source: IToken):
      IToken;
  end;

implementation

uses
  Ntapi.ntseapi;

{$R *.dfm}

procedure TDialogAccessAndType.DoCreate;
begin
  inherited;

  AccessMaskFrame.LoadType(TypeInfo(TTokenAccessMask), TokenGenericMapping);
  AccessMaskFrame.AccessMask := TOKEN_ALL_ACCESS;

  FSelectedType := ttPrimary;
end;

class function TDialogAccessAndType.ExecuteDuplication(AOwner: TComponent;
  Source: IToken): IToken;
begin
  with TDialogAccessAndType.CreateChild(AOwner, cfmApplication) do
  begin
    if Source.InfoClass.Query(tdTokenType) then
      SelectedTokenType := Source.InfoClass.TokenTypeInfo;

    ShowModal;

    Result := TToken.CreateDuplicateToken(Source, AccessMaskFrame.AccessMask,
      SelectedTokenType, CheckBoxEffective.Checked);
  end;
end;

procedure TDialogAccessAndType.SetSelectedType(const Value: TTokenTypeEx);
begin
  RadioButtonAnonymous.Checked := (Value = ttAnonymous);
  RadioButtonIdentification.Checked := (Value = ttIdentification);
  RadioButtonImpersonation.Checked := (Value = ttImpersonation);
  RadioButtonDelegation.Checked := (Value = ttDelegation);
  RadioButtonPrimary.Checked := (Value = ttPrimary);
end;

procedure TDialogAccessAndType.RadioButtonClick(Sender: TObject);
begin
  FSelectedType := TTokenTypeEx((Sender as TRadioButton).Tag);
end;

end.
