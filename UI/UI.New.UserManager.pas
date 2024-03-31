unit UI.New.UserManager;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, UI.Prototypes.Forms, NtUiFrame,
  UI.Prototypes.Sid.Edit, UI.Prototypes, Ntapi.WinNt;

type
  TUserManagerTokens = class(TChildForm)
    rbxDefault: TRadioButton;
    rbxSession: TRadioButton;
    rbxShell: TRadioButton;
    rbxContext: TRadioButton;
    rbxSid: TRadioButton;
    rbxName: TRadioButton;
    cbxSessionId: TComboBox;
    BevelSession: TBevel;
    cbxContext: TComboBox;
    cbxName: TEdit;
    btnOpen: TButton;
    btnClose: TButton;
    SidEditor: TSidEditor;
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure cbxContextEnter(Sender: TObject);
    procedure cbxNameEnter(Sender: TObject);
    procedure cbxSessionIdEnter(Sender: TObject);
    procedure SidEditorEnter(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    SessionSource: TSessionSource;
    ContextsSource: TUmgrContextSource;
  public
    { Public declarations }
  end;

implementation

uses
  NtUtils, TU.Tokens, TU.Tokens.Open, UI.MainForm, UI.Settings;

{$R *.dfm}

procedure TUserManagerTokens.btnCloseClick;
begin
  Close;
end;

procedure TUserManagerTokens.btnOpenClick;
var
  Token: IToken;
begin
  if rbxDefault.Checked then
    MakeUmgrDefaultAccountToken(Token).RaiseOnError
  else if rbxSession.Checked then
    MakeUmgrSessionUserToken(Token, SessionSource.SelectedSession).RaiseOnError
  else if rbxShell.Checked then
    MakeUmgrActiveShellToken(Token, SessionSource.SelectedSession).RaiseOnError
  else if rbxContext.Checked then
    MakeUmgrTokenByContext(Token, ContextsSource.SelectedContext).RaiseOnError
  else if rbxSid.Checked then
    MakeUmgrTokenBySid(Token, SidEditor.Sid).RaiseOnError
  else if rbxName.Checked then
    MakeUmgrTokenByName(Token, cbxName.Text).RaiseOnError
  else
    Abort;

  FormMain.TokenView.Add(Token);

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TUserManagerTokens.cbxContextEnter;
begin
  rbxContext.Checked := True;
end;

procedure TUserManagerTokens.cbxNameEnter;
begin
  rbxName.Checked := True;
end;

procedure TUserManagerTokens.cbxSessionIdEnter;
begin
  if not rbxSession.Checked and not rbxShell.Checked then
    rbxShell.Checked := True;
end;

procedure TUserManagerTokens.FormClose;
begin
  SessionSource.Free;
  ContextsSource.Free;
end;

procedure TUserManagerTokens.FormCreate;
begin
  SessionSource := TSessionSource.Create(cbxSessionId, True);
  ContextsSource := TUmgrContextSource.Create(cbxContext);
end;

procedure TUserManagerTokens.FormResize(Sender: TObject);
begin
  // Fix selection on resizing
  cbxSessionId.SelLength := 0;
  cbxContext.SelLength := 0;
end;

procedure TUserManagerTokens.SidEditorEnter;
begin
  rbxSid.Checked := True;
end;

end.
