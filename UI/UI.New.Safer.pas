unit UI.New.Safer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes.Forms, TU.Tokens, Winapi.WinSafer;

type
  TDialogSafer = class(TChildForm)
    ComboBoxScope: TComboBox;
    ComboBoxLevel: TComboBox;
    LabelScope: TLabel;
    LabelLevel: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxSandboxInert: TCheckBox;
    LabelDesc: TLabel;
    LabelDescription: TLabel;
    LabelName: TLabel;
    LabelFriendlyName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ComboBoxLevelChange(Sender: TObject);
  private
    Token: IToken;
    function GetScopeId: TSaferScopeId;
    function GetLevelId: TSaferLevelId;
    procedure ChangedCaption(const NewCaption: String);
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: IToken);
  end;

implementation

uses
  UI.Settings, UI.MainForm, TU.Suggestions, System.UITypes,
  NtUtils.WinSafer, NtUtils;

{$R *.dfm}

{ TDialogSafer }

procedure TDialogSafer.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogSafer.ButtonOKClick(Sender: TObject);
var
  NewToken: IToken;
begin
  NewToken := TToken.CreateSaferToken(Token, GetScopeId, GetLevelId,
    CheckBoxSandboxInert.Checked);

  FormMain.TokenView.Add(NewToken);

  // Check whether SandboxInert was actually enabled
  if CheckBoxSandboxInert.Checked then
    if NewToken.InfoClass.Query(tdTokenSandBoxInert) and
      not NewToken.InfoClass.SandboxInert then
      begin
        if not TSettings.NoCloseCreationDialogs then
          Hide;
        MessageDlg(NO_SANBOX_INERT, mtWarning, [mbOK], 0);
      end;

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogSafer.ChangedCaption(const NewCaption: String);
begin
  Caption := Format('Create Safer Token for "%s"', [NewCaption]);
end;

procedure TDialogSafer.ComboBoxLevelChange(Sender: TObject);
var
  hxLevel: IHandle;
  Name, Description: string;
begin
  Name := '';
  Description := '';

  if SafexOpenLevel(hxLevel, GetScopeId, GetLevelId).IsSuccess then
  begin
    SafexQueryNameLevel(hxLevel.Handle, Name);
    SafexQueryDescriptionLevel(hxLevel.Handle, Description);
  end;

  LabelFriendlyName.Caption := Name;
  LabelDescription.Caption := Description;
end;

constructor TDialogSafer.CreateFromToken(AOwner: TComponent; SrcToken: IToken);
begin
  Token := SrcToken;
  inherited CreateChild(AOwner, True);
  Show;
end;

procedure TDialogSafer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Token.OnCaptionChange.Unsubscribe(ChangedCaption);
end;

procedure TDialogSafer.FormCreate(Sender: TObject);
begin
  Assert(Assigned(Token));

  Token.OnCaptionChange.Subscribe(ChangedCaption);
  ChangedCaption(Token.Caption);

  if Token.InfoClass.Query(tdTokenSandBoxInert) then
    CheckBoxSandboxInert.Checked := Token.InfoClass.SandboxInert;

  ComboBoxLevelChange(Sender);
end;

function TDialogSafer.GetLevelId: TSaferLevelId;
begin
  case ComboBoxLevel.ItemIndex of
    0: Result := SAFER_LEVELID_FULLYTRUSTED;
    1: Result := SAFER_LEVELID_NORMALUSER;
    2: Result := SAFER_LEVELID_CONSTRAINED;
    3: Result := SAFER_LEVELID_UNTRUSTED;
    4: Result := SAFER_LEVELID_DISALLOWED;
  else
    Result := SAFER_LEVELID_NORMALUSER;
  end;
end;

function TDialogSafer.GetScopeId: TSaferScopeId;
begin
  if ComboBoxScope.ItemIndex = 0 then
    Result := SAFER_SCOPEID_MACHINE
  else
    Result := SAFER_SCOPEID_USER;
end;

end.
