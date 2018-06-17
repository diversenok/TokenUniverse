unit UI.Duplicate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Graphics,
  TU.TokenUtils;

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

    if ModalResult <> mrOk then
      raise EAbort.Create('Canceled');

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
      TokenTypeInfo);
  end;
end;

procedure TDuplicateDialog.SetDefaults(Source: TToken);
begin
  try
    with Source.TokenTypeAndImpersonation do
      if TokenType = TokenImpersonation then
      begin
        RadioButtonAnonymous.Checked := Impersonation = SecurityAnonymous;
        RadioButtonIdentification.Checked := Impersonation = SecurityIdentification;
        RadioButtonImpersonation.Checked := Impersonation = SecurityImpersonation;
        RadioButtonDelegation.Checked := Impersonation = SecurityDelegation;
        if Impersonation = SecurityAnonymous then
          RadioButtonIdentification.Font.Style := [fsStrikeOut];
        if Impersonation <= SecurityIdentification then
        begin
          RadioButtonImpersonation.Font.Style := [fsStrikeOut];
          RadioButtonDelegation.Font.Style := [fsStrikeOut];
          RadioButtonPrimary.Font.Style := [fsStrikeOut];
        end;
      end
      else
        RadioButtonPrimary.Checked := True;
  except
    on EOSError do;
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
