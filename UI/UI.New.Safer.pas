unit UI.New.Safer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UI.Prototypes.Forms, Ntapi.WinSafer, NtUtils,
  TU.Tokens;

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
    CaptionSubscription: IAutoReleasable;
    function GetScopeId: TSaferScopeId;
    function GetLevelId: TSaferLevelId;
    procedure ChangedCaption(const InfoClass: TTokenStringClass; const NewCaption: String);
  public
    constructor CreateFromToken(AOwner: TComponent; const SrcToken: IToken);
  end;

implementation

uses
  UI.Settings, UI.MainForm, TU.Suggestions, System.UITypes,
  NtUtils.WinSafer, NtUiLib.Errors;

{$R *.dfm}

{ TDialogSafer }

procedure TDialogSafer.ButtonCancelClick;
begin
  Close;
end;

procedure TDialogSafer.ButtonOKClick;
var
  hxNewToken: IHandle;
  NewToken: IToken;
  LevelName: String;
begin
  SafexComputeSaferTokenById(hxNewToken, Token.Handle, GetScopeId, GetLevelId,
    CheckBoxSandboxInert.Checked).RaiseOnError;

  case GetLevelId of
    SAFER_LEVELID_FULLYTRUSTED:
      LevelName := 'Unrestricted';

    SAFER_LEVELID_NORMALUSER:
      LevelName := 'Normal';

    SAFER_LEVELID_CONSTRAINED:
      LevelName := 'Constrained';

    SAFER_LEVELID_UNTRUSTED:
      LevelName := 'Untrusted';

    SAFER_LEVELID_DISALLOWED:
      LevelName := 'Disallowed'
  else
    LevelName := 'Unknown';
  end;

  NewToken := CaptureTokenHandle(hxNewToken, LevelName + ' Safer for ' +
    Token.Caption);

  FormMain.TokenView.Add(NewToken);

  if CheckBoxSandboxInert.Checked then
    CheckSandboxInert(Handle, NewToken);

  if not TSettings.NoCloseCreationDialogs then
    Close;
end;

procedure TDialogSafer.ChangedCaption;
begin
  Caption := Format('Create Safer Token for "%s"', [NewCaption]);
end;

procedure TDialogSafer.ComboBoxLevelChange;
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

constructor TDialogSafer.CreateFromToken;
begin
  Token := SrcToken;
  inherited CreateChild(AOwner, cfmDesktop);
  Show;
end;

procedure TDialogSafer.FormCreate;
var
  SandboxInert: LongBool;
begin
  if not Assigned(Token) then
    raise EAccessViolation.Create('Token is not set');

  CaptionSubscription := Token.ObserveString(tsCaption, ChangedCaption);

  CheckBoxSandboxInert.Checked := Token.QuerySandboxInert(
    SandboxInert).IsSuccess and SandboxInert;

  ComboBoxLevelChange(Sender);
end;

function TDialogSafer.GetLevelId;
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

function TDialogSafer.GetScopeId;
begin
  if ComboBoxScope.ItemIndex = 0 then
    Result := SAFER_SCOPEID_MACHINE
  else
    Result := SAFER_SCOPEID_USER;
end;

end.
