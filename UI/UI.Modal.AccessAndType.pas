unit UI.Modal.AccessAndType;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Graphics,
  Vcl.ComCtrls, TU.Tokens, UI.Prototypes, UI.Prototypes.ChildForm,
  UI.ListViewEx;

type
  TDialogAccessAndType = class(TChildForm)
    RadioButtonPrimary: TRadioButton;
    GroupBoxType: TGroupBox;
    RadioButtonAnonymous: TRadioButton;
    RadioButtonIdentification: TRadioButton;
    RadioButtonImpersonation: TRadioButton;
    RadioButtonDelegation: TRadioButton;
    ButtonOK: TButton;
    StaticTextAccess: TStaticText;
    ButtonCancel: TButton;
    ListViewAccess: TListViewEx;
    procedure RadioButtonClick(Sender: TObject);
  private
    FSelectedType: TTokenTypeEx;
    function GetAccess: ACCESS_MASK;
    procedure SetSelectedType(const Value: TTokenTypeEx);
  protected
    procedure DoCreate; override;
  public
    property SelectedAccess: ACCESS_MASK read GetAccess;
    property SelectedTokenType: TTokenTypeEx read FSelectedType write
      SetSelectedType;
    class function ExecuteDuplication(AOwner: TComponent; Source: TToken):
      TToken;
  end;

var
  DialogAccessAndType: TDialogAccessAndType;

implementation

{$R *.dfm}

procedure TDialogAccessAndType.DoCreate;
begin
  inherited;
  TAccessMaskSource.InitAccessEntries(ListViewAccess, TOKEN_ALL_ACCESS);
  FSelectedType := ttPrimary;
end;

class function TDialogAccessAndType.ExecuteDuplication(AOwner: TComponent;
  Source: TToken): TToken;
begin
  with TDialogAccessAndType.Create(AOwner) do
  begin
    with Source.TokenTypeInfo do
      if IsValid then
        SelectedTokenType := Value;

    ShowModal;

    Result := TToken.CreateDuplicate(Source, SelectedAccess, SelectedTokenType);
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

function TDialogAccessAndType.GetAccess: ACCESS_MASK;
begin
  Result := TAccessMaskSource.GetAccessMask(ListViewAccess);
end;

procedure TDialogAccessAndType.RadioButtonClick(Sender: TObject);
begin
  FSelectedType := TTokenTypeEx((Sender as TRadioButton).Tag);
end;

end.
