unit UI.Information;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, TU.Tokens;

type
  TInfoDialog = class(TForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabGroups: TTabSheet;
    TabPrivileges: TTabSheet;
    StaticUser: TStaticText;
    EditUser: TEdit;
    StaticSID: TStaticText;
    ButtonClose: TButton;
    ListViewGroups: TListView;
    EditSID: TEdit;
    ListViewPrivileges: TListView;
    TabRestricted: TTabSheet;
    ListViewRestricted: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Token: TToken;
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

{$R *.dfm}

procedure TInfoDialog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TInfoDialog.CreateFromToken(AOwner: TComponent; SrcToken: TToken);
begin
  Token := SrcToken;
  inherited Create(AOwner);
  Show;
end;

procedure TInfoDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TInfoDialog.FormCreate(Sender: TObject);
const
  Title = 'Token Information for "%s"';
var
  i: integer;
begin
  Caption := Format(Title, [Token.Caption]);

  with Token.User do
    if IsValid then
    begin
      EditUser.Text := Value.Domain + '\' + Value.User;
      EditSID.Text := Value.SID;
    end;

  with Token.Groups do
    if IsValid then
    begin
      TabGroups.Caption := Format('%s (%d)', [TabGroups.Caption,
        Length(Value)]);

      for i := 0 to High(Value) do
      with Value[i], ListViewGroups.Items.Add do
      begin
        Caption := SecurityIdentifier.ToString;
        SubItems.Add(Attributes.ToString);
      end;
    end;

  with Token.Privileges do
    if IsValid then
    begin
      TabPrivileges.Caption := Format('%s (%d)', [TabPrivileges.Caption,
        Length(Value)]);

      for i := 0 to High(Value) do
      with Value[i], ListViewPrivileges.Items.Add do
      begin
        Caption := Name;
        SubItems.Add(Value[i].AttributesToString);
        SubItems.Add(Value[i].Description);
      end;
    end;

  with Token.RestrictedSids do
    if IsValid then
    begin
      TabRestricted.Caption := Format('%s (%d)', [TabRestricted.Caption,
        Length(Value)]);

      for i := 0 to High(Value) do
      with Value[i], ListViewRestricted.Items.Add do
      begin
        Caption := SecurityIdentifier.ToString;
        SubItems.Add(Attributes.ToString);
      end;
    end;
end;

end.
