unit UI.Modal.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics, Vcl.ComCtrls, TU.Tokens,
  UI.Prototypes.Forms, VclEx.ListView, UI.Prototypes.AccessMask;

type
  TDialogAccess = class(TChildForm)
    RadioButtonSame: TRadioButton;
    RadioButtonSpecial: TRadioButton;
    GroupBoxMode: TGroupBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    GroupBoxAccess: TGroupBox;
    AccessMaskFrame: TAccessMaskFrame;
  public
    class function ExecuteDuplication(AOwner: TComponent; const Source: IToken):
      IToken;
  end;

implementation

uses
   TU.Tokens.Old.Types, Ntapi.ntseapi, Ntapi.ntobapi, NtUtils,
   TU.Tokens.Open, NtUiLib.Errors;

{$R *.dfm}

class function TDialogAccess.ExecuteDuplication;
var
  BasicInfo: TObjectBasicInformation;
begin
  with TDialogAccess.CreateChild(AOwner, cfmApplication) do
  begin
    AccessMaskFrame.LoadType(TypeInfo(TTokenAccessMask), TokenGenericMapping);

    if Source.QueryBasicInfo(BasicInfo).IsSuccess then
      AccessMaskFrame.AccessMask := BasicInfo.GrantedAccess;

    ShowModal;

    MakeDuplicateHandle(Result, Source, AccessMaskFrame.AccessMask,
      RadioButtonSame.Checked).RaiseOnError;
  end;
end;

end.
