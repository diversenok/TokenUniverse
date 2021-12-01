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
    class function ExecuteDuplication(AOwner: TComponent; Source: IToken):
      IToken;
  end;

implementation

uses
   TU.Tokens.Types, Ntapi.ntseapi;

{$R *.dfm}

class function TDialogAccess.ExecuteDuplication(AOwner: TComponent;
  Source: IToken): IToken;
begin
  with TDialogAccess.CreateChild(AOwner, cfmApplication) do
  begin
    AccessMaskFrame.LoadType(TypeInfo(TTokenAccessMask), TokenGenericMapping);

    if Source.InfoClass.Query(tdObjectInfo) then
      AccessMaskFrame.AccessMask :=
        Source.InfoClass.ObjectInformation.GrantedAccess;

    ShowModal;

    Result := TToken.CreateDuplicateHandle(Source, AccessMaskFrame.AccessMask,
      RadioButtonSame.Checked);
  end;
end;

end.
