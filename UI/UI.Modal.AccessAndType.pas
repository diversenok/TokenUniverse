unit UI.Modal.AccessAndType;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls, TU.Tokens,
  UI.Prototypes.Forms, VclEx.ListView, TU.Tokens.Old.Types,
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
    class function ExecuteDuplication(AOwner: TComponent; const Source: IToken):
      IToken;
  end;

implementation

uses
  Ntapi.ntseapi, NtUtils, TU.Tokens.Open, NtUiLib.Errors;

{$R *.dfm}

procedure TDialogAccessAndType.DoCreate;
begin
  inherited;

  AccessMaskFrame.LoadType(TypeInfo(TTokenAccessMask), TokenGenericMapping);
  AccessMaskFrame.AccessMask := TOKEN_ALL_ACCESS;

  FSelectedType := ttPrimary;
end;

class function TDialogAccessAndType.ExecuteDuplication;
var
  Statistics: TTokenStatistics;
begin
  with TDialogAccessAndType.CreateChild(AOwner, cfmApplication) do
  begin
    if Source.QueryStatistics(Statistics).IsSuccess then
    begin
      if Statistics.TokenType = TokenPrimary then
        SelectedTokenType := ttPrimary
      else
        SelectedTokenType := TTokenTypeEx(Statistics.ImpersonationLevel);
    end;

    ShowModal;

    MakeDuplicateToken(Result, Source, SelectedTokenType,
      AccessMaskFrame.AccessMask, CheckBoxEffective.Checked).RaiseOnError;
  end;
end;

procedure TDialogAccessAndType.SetSelectedType;
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
