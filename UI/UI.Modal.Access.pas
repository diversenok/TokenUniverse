unit UI.Modal.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics,
  Vcl.ComCtrls, TU.Tokens, UI.Prototypes, UI.Prototypes.ChildForm,
  VclEx.ListView, TU.Tokens.Types;

type
  TDialogAccess = class(TChildForm)
    RadioButtonSame: TRadioButton;
    RadioButtonMaximum: TRadioButton;
    RadioButtonSpecial: TRadioButton;
    GroupBox1: TGroupBox;
    StaticTextAccess: TStaticText;
    ListViewAccess: TListViewEx;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure ListViewAccessChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  protected
    function GetAccess: ACCESS_MASK;
  public
    class function ExecuteDuplication(AOwner: TComponent; Source: TToken):
      TToken;
  end;

implementation

{$R *.dfm}

class function TDialogAccess.ExecuteDuplication(AOwner: TComponent;
  Source: TToken): TToken;
begin
  with TDialogAccess.Create(AOwner) do
  begin
    if Source.InfoClass.Query(tdObjectInfo) then
      TAccessMaskSource.InitAccessEntries(ListViewAccess,
        Source.InfoClass.ObjectInformation.GrantedAccess)
    else
      TAccessMaskSource.InitAccessEntries(ListViewAccess, 0);

    ShowModal;

    Result := TToken.CreateDuplicateHandle(Source, GetAccess,
      RadioButtonSame.Checked);
  end;
end;

function TDialogAccess.GetAccess: ACCESS_MASK;
begin
  if RadioButtonMaximum.Checked then
    Result := MAXIMUM_ALLOWED
  else
    Result := TAccessMaskSource.GetAccessMask(ListViewAccess);
end;

procedure TDialogAccess.ListViewAccessChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  RadioButtonSpecial.Checked := True;
end;

end.
