unit UI.New.Safer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes.Forms, TU.Tokens, Ntapi.WinSafer, NtUtils,
  TU.Tokens3;

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
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ComboBoxLevelChange(Sender: TObject);
  private
    Token: IToken;
    CaptionSubscripion: IAutoReleasable;
    function GetScopeId: TSaferScopeId;
    function GetLevelId: TSaferLevelId;
    procedure ChangedCaption(const InfoClass: TTokenStringClass; const NewCaption: String);
  public
    constructor CreateFromToken(AOwner: TComponent; SrcToken: IToken);
  end;

implementation

uses
  UI.Settings, UI.MainForm, TU.Suggestions, System.UITypes,
  NtUtils.WinSafer;

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

procedure TDialogSafer.ChangedCaption;
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
  inherited CreateChild(AOwner, cfmDesktop);
  Show;
end;

procedure TDialogSafer.FormCreate(Sender: TObject);
begin
  Assert(Assigned(Token));

  CaptionSubscripion := (Token as IToken3).ObserveString(tsCaption,
    ChangedCaption);

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
