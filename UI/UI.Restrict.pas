unit UI.Restrict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, TU.Common, TU.Tokens, UI.ListViewEx, UI.Prototypes,
  UI.Prototypes.ChildForm, System.ImageList, Vcl.ImgList;

type
  TDialogRestrictToken = class(TChildForm)
    CheckBoxDisableMaxPriv: TCheckBox;
    CheckBoxSandboxInert: TCheckBox;
    CheckBoxLUA: TCheckBox;
    CheckBoxWriteRestrict: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PageControl1: TPageControl;
    TabSheetSidDisable: TTabSheet;
    TabSheetSidRestict: TTabSheet;
    TabSheetPrivDelete: TTabSheet;
    ListViewPrivileges: TPrivilegesListViewEx;
    ListViewRestrictSID: TGroupListViewEx;
    ListViewDisableSID: TGroupListViewEx;
    ButtonAddSID: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DoCloseForm(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    Token: TToken;
    function GetFlags: Cardinal;
    procedure ChangedCaption(NewCaption: String);
  public
    procedure Refresh;
    constructor CreateFromToken(AOwner: TComponent; SrcToken: TToken);
  end;

implementation

uses
  UI.MainForm, System.UITypes;

{$R *.dfm}

{ TDialogRestrictToken }

procedure TDialogRestrictToken.ButtonOKClick(Sender: TObject);
const
  NO_SANBOX_INERT = 'The resulting token doesn''t contain SandboxInert flag ' +
    'despite you tried to enable it. Looks like this action requires ' +
    'SeTcbPrivilege on your system.';
var
  NewToken: TToken;
begin
  NewToken := TToken.CreateRestricted(Token, GetFlags,
    ListViewDisableSID.CheckedGroups,
    ListViewRestrictSID.CheckedGroups,
    ListViewPrivileges.CheckedPrivileges);

  FormMain.Frame.AddToken(NewToken);

  // Check whether SandboxInert was actually enabled
  if CheckBoxSandboxInert.Checked then
    with NewToken.SandboxInert do
      if IsValid and not Value then
      begin
        Hide;
        MessageDlg(NO_SANBOX_INERT, mtWarning, [mbOK], 0);
      end;

  Close;
end;

procedure TDialogRestrictToken.ChangedCaption(NewCaption: String);
begin
  Caption := Format('Create restricted token for "%s"', [NewCaption]);
end;

constructor TDialogRestrictToken.CreateFromToken(AOwner: TComponent;
  SrcToken: TToken);
begin
  Token := SrcToken;
  inherited Create(AOwner);
  Show;
end;

procedure TDialogRestrictToken.DoCloseForm(Sender: TObject);
begin
  Close;
end;

procedure TDialogRestrictToken.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Token.OnCaptionChange.Delete(ChangedCaption);
  UnsubscribeTokenCanClose(Token);
end;

procedure TDialogRestrictToken.FormCreate(Sender: TObject);
begin
  SubscribeTokenCanClose(Token, Caption);
  Refresh;

  Token.OnCaptionChange.Add(ChangedCaption);
  Token.OnCaptionChange.Invoke(Token.Caption);
  ListViewDisableSID.Token := Token;
  ListViewRestrictSID.Token := Token;
  ListViewPrivileges.Token := Token;
end;

function TDialogRestrictToken.GetFlags: Cardinal;
const
  DISABLE_MAX_PRIVILEGE = 1;
  SANDBOX_INERT = 2;
  LUA_TOKEN = 4;
  WRITE_RESTRICTED = 8; // TODO: Check windows version, this value depends on it
begin
  Result := 0;
  if CheckBoxDisableMaxPriv.Checked then
    Result := Result or DISABLE_MAX_PRIVILEGE;
  if CheckBoxSandboxInert.Checked then
    Result := Result or SANDBOX_INERT;
  if CheckBoxLUA.Checked then
    Result := Result or LUA_TOKEN;
  if CheckBoxWriteRestrict.Checked then
    Result := Result or WRITE_RESTRICTED;
end;

procedure TDialogRestrictToken.Refresh;
begin
  Token.Groups;
  Token.Privileges;
end;

end.
