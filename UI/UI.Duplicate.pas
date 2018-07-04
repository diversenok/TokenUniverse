unit UI.Duplicate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Graphics,
  TU.Tokens;

type
  TDuplicateDialog = class(TForm)
    RadioButtonPrimary: TRadioButton;
    GroupBoxType: TGroupBox;
    RadioButtonAnonymous: TRadioButton;
    RadioButtonIdentification: TRadioButton;
    RadioButtonImpersonation: TRadioButton;
    RadioButtonDelegation: TRadioButton;
    ButtonOK: TButton;
    StaticTextAccess: TStaticText;
    ListBoxAccess: TCheckListBox;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    class function Execute(AOwner: TComponent; Source: TToken): TToken;
    procedure RadioButtonClick(Sender: TObject);
    procedure SetDefaults(Source: TToken);
  private
    SelectedType: Integer;
  public
    { Public declarations }
  end;

var
  DuplicateDialog: TDuplicateDialog;

implementation

{$R *.dfm}

class function TDuplicateDialog.Execute(AOwner: TComponent;
  Source: TToken): TToken;
var
  TokenTypeInfo: TTokenTypeInfo;
  AccessMask: ACCESS_MASK;
  i: integer;
begin
  with TDuplicateDialog.Create(AOwner) do
  begin
    SetDefaults(Source);
    ShowModal; // And wait until the dialog closes

    // The form wouldn't be actually destroyed until Application.ProcessMessages

    if ModalResult <> mrOk then
      raise EAbort.Create('');

    if RadioButtonPrimary.Checked then
    begin
      TokenTypeInfo.TokenType := TokenPrimary;
      TokenTypeInfo.Impersonation := SecurityImpersonation;
    end
    else
    begin
      TokenTypeInfo.TokenType := TokenImpersonation;
      TokenTypeInfo.Impersonation := TSecurityImpersonationLevel(SelectedType);
    end;

    AccessMask := 0;
    for i := 0 to ACCESS_COUNT - 1 do
      if ListBoxAccess.Checked[i] then
        AccessMask := AccessMask or AccessValues[i];

    Result := TToken.CreateDuplicate(Source, AccessMask,
      TokenTypeInfo.Impersonation, TokenTypeInfo.TokenType);
  end;
end;

procedure TDuplicateDialog.SetDefaults(Source: TToken);
begin
  with Source.TokenTypeInfo do
    if IsValid then
    begin
      if Value.TokenType = TokenImpersonation then
      begin
        RadioButtonAnonymous.Checked := Value.Impersonation = SecurityAnonymous;
        RadioButtonIdentification.Checked := Value.Impersonation = SecurityIdentification;
        RadioButtonImpersonation.Checked := Value.Impersonation = SecurityImpersonation;
        RadioButtonDelegation.Checked := Value.Impersonation = SecurityDelegation;
      end
      else
        RadioButtonPrimary.Checked := True;
    end;
end;

procedure TDuplicateDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TDuplicateDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ACCESS_COUNT - 1 do
  begin
    ListBoxAccess.Items.Add(AccessStrings[i]);
    ListBoxAccess.Checked[i] := True;
  end;
end;

procedure TDuplicateDialog.RadioButtonClick(Sender: TObject);
begin
  SelectedType := (Sender as TRadioButton).Tag;
end;

end.
