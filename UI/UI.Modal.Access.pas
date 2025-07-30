unit UI.Modal.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics, Vcl.ComCtrls, TU.Tokens,
  NtUiCommon.Forms, VclEx.ListView, NtUiFrame.Bits;

type
  TDialogAccess = class(TChildForm)
    RadioButtonSame: TRadioButton;
    RadioButtonSpecial: TRadioButton;
    GroupBoxMode: TGroupBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    GroupBoxAccess: TGroupBox;
    AccessMaskFrame: TBitsFrame;
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
  with TDialogAccess.Create(AOwner, cfmApplication) do
  begin
    AccessMaskFrame.LoadAccessMaskType(TypeInfo(TTokenAccessMask),
      TokenGenericMapping, True, True);

    if Source.QueryBasicInfo(BasicInfo).IsSuccess then
      AccessMaskFrame.Value := BasicInfo.GrantedAccess;

    ShowModal;

    MakeDuplicateHandle(Result, Source, AccessMaskFrame.Value,
      RadioButtonSame.Checked).RaiseOnError;
  end;
end;

end.
