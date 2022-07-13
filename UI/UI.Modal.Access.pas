unit UI.Modal.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics, Vcl.ComCtrls, TU.Tokens3,
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
    class function ExecuteDuplication(AOwner: TComponent; Source: IToken3):
      IToken3;
  end;

implementation

uses
   TU.Tokens.Old.Types, Ntapi.ntseapi, Ntapi.ntobapi, NtUtils,
   TU.Tokens3.Open, NtUiLib.Errors;

{$R *.dfm}

class function TDialogAccess.ExecuteDuplication(AOwner: TComponent;
  Source: IToken3): IToken3;
var
  BasicInfo: TObjectBasicInformation;
begin
  with TDialogAccess.CreateChild(AOwner, cfmApplication) do
  begin
    AccessMaskFrame.LoadType(TypeInfo(TTokenAccessMask), TokenGenericMapping);

    if (Source as IToken3).QueryBasicInfo(BasicInfo).IsSuccess then
      AccessMaskFrame.AccessMask := BasicInfo.GrantedAccess;

    ShowModal;

    MakeDuplicateHandle(Result, Source, AccessMaskFrame.AccessMask,
      RadioButtonSame.Checked).RaiseOnError;
  end;
end;

end.
