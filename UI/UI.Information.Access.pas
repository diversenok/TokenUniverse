unit UI.Information.Access;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls,
  UI.Prototypes, UI.Prototypes.ChildForm, UI.ListViewEx, Winapi.WinNt;

type
  TDialogGrantedAccess = class(TChildForm)
    ListViewAccess: TListViewEx;
    ButtonClose: TButton;
    procedure DisableItemChecking(Sender: TObject; Item: TListItem);
  public
    class procedure Execute(AOwner: TComponent; Access: TAccessMask);
  end;

implementation

{$R *.dfm}

{ TDialogGrantedAccess }

procedure TDialogGrantedAccess.DisableItemChecking(Sender: TObject;
  Item: TListItem);
begin
  ListViewAccess.OnItemChecked := nil; // Prevent deadly recursion
  Item.Checked := not Item.Checked;
  ListViewAccess.OnItemChecked := DisableItemChecking;
end;

class procedure TDialogGrantedAccess.Execute(AOwner: TComponent;
  Access: TAccessMask);
begin
  with TDialogGrantedAccess.Create(AOwner) do
  begin
    TAccessMaskSource.InitAccessEntries(ListViewAccess, Access);
    ListViewAccess.OnItemChecked := DisableItemChecking;
    ShowModal;
  end;
end;

end.
