unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons;

type
  TInfoDialog = class(TForm)
    PageControl: TPageControl;
    TabMain: TTabSheet;
    TabSheet1: TTabSheet;
    Privileges: TTabSheet;
    StaticTextUser: TStaticText;
    EditUser: TEdit;
    StaticTextSid: TStaticText;
    EditUserSID: TEdit;
    ButtonUser: TSpeedButton;
    Edit1: TEdit;
    ButtonUserSID: TSpeedButton;
    ButtonCancel: TButton;
    procedure EditColorOutdated(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InfoDialog: TInfoDialog;

implementation

{$R *.dfm}

procedure TInfoDialog.EditColorOutdated(Sender: TObject);
begin
  (Sender as TEdit).Color := $D2D5EE;
end;

end.
