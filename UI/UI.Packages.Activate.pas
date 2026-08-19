unit UI.Packages.Activate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NtUtilsUI.StdCtrls,
  NtUiLib.AutoCompletion, NtUtilsUI, NtUtilsUI.Base, NtUtilsUI.SessionID,
  NtUtilsUI.UmgrContext, NtUiFrame.Bits;

type
  TFormActivatePackage = class (TUiLibChildForm)
    tbxAumid: TUiLibEdit;
    lblAumid: TLabel;
    lblDisplayName: TLabel;
    tbxDisplayName: TUiLibEdit;
    lblArguments: TLabel;
    tbxArguments: TUiLibEdit;
    btnActivate: TButton;
    btnClose: TButton;
    tbxResult: TUiLibEdit;
    lblResult: TLabel;
    cbxMethod: TComboBox;
    lblMethod: TLabel;
    cbxSession: TUiLibSessionIdBox;
    chkSession: TCheckBox;
    cbxContext: TUiLibUmgrContextBox;
    fmxOptions: TBitsFrame;
    lblOptions: TLabel;
    chkContext: TCheckBox;
    procedure btnActivateClick(Sender: TObject);
    procedure tbxAumidEnter(Sender: TObject);
    procedure tbxAumidChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbxMethodChange(Sender: TObject);
    procedure UiLibChildFormCreate(Sender: TObject);
  private
    FAumidSuggestions: IAutoCompletionSuggestions;
  public
    { Public declarations }
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.appmodel, NtUtils, NtUtils.Processes.Create.Package,
  NtUtils.Packages, NtUtils.Packages.SRCache, NtUtils.Packages.Mrm,
  NtUtils.SysUtils, DelphiUiLib.LiteReflection, NtUiLib.Errors;

{$R *.dfm}

{ Functions }

function SuggestAumids(
  const Root: String;
  out Suggestions: TArray<String>
): TNtxStatus;
var
  AppId: TSRCacheApplicationId;
  hxAppIdKey: IHandle;
  Aumid: String;
begin
  if Root <> '' then
  begin
    Suggestions := nil;
    Result := NtxSuccess;
    Exit;
  end;

  Suggestions := nil;

  // Collect package applications from the state repository cache
  for AppId in PkgxSRCacheIterateApplicationIDs(@Result) do
  begin
    Result := PkgxSRCacheOpenApplication(AppId, hxAppIdKey);

    if Result.IsSuccess then
      Result := PkgxSRCacheQueryApplicationAumid(hxAppIdKey, Aumid);

    if Result.IsSuccess then
    begin
      SetLength(Suggestions, Succ(Length(Suggestions)));
      Suggestions[High(Suggestions)] := Aumid;
    end;
  end;

  Result := NtxSuccess;
end;

function LookupDisplayName(
  const Aumid: String;
  out DisplayName: String
): TNtxStatus;
var
  AppId: TSRCacheApplicationId;
  FamilyName, RelativeAppId: String;
  hxAppKey: IHandle;
begin
  // Find the state repository cache ID for the application
  Result := PkgxSRCacheFindApplicationId(AppId, Aumid);

  if not Result.IsSuccess then
    Exit;

  // Open the application key in the state repository cache
  Result := PkgxSRCacheOpenApplication(AppId, hxAppKey);

  if not Result.IsSuccess then
    Exit;

  // Read the display name from it
  Result := PkgxSRCacheQueryApplicationDisplayName(hxAppKey, DisplayName);

  if not Result.IsSuccess then
    Exit;

  // Non-resource strings appear as-is
  if PkgxMrmResourceReferenceType(DisplayName) = rkUnknown then
  begin
    Result := NtxSuccess;
    Exit;
  end;

  // Determine the package family for resource lookup
  Result := PkgxDeriveFamilyNameFromAppUserModelId(Aumid, FamilyName,
    RelativeAppId);

  if not Result.IsSuccess then
    Exit;

  // Resolve the packaged resource
  Result := PkgxMrmResolveStringVar(DisplayName, FamilyName);
end;

{ TFormActivatePackage }

procedure TFormActivatePackage.btnActivateClick;
var
  Options: TPkgxActivatePackageOptions;
  ProcessId: TProcessId32;
  ExtendedMethod: Boolean;
begin
  tbxResult.Text := '';
  ExtendedMethod := cbxMethod.ItemIndex = 1;

  // Collection activation settings
  Options := Default(TPkgxActivatePackageOptions);
  Options.Aumid := tbxAumid.Text;
  Options.Arguments := tbxArguments.Text;
  Options.Options := Cardinal(fmxOptions.Value);

  if ExtendedMethod and chkSession.Checked then
  begin
    Include(Options.Flags, apUseSessionId);
    Options.SessionId := cbxSession.SessionID;
  end;

  if ExtendedMethod and chkContext.Checked then
    Options.UserContext := cbxContext.UserContext;

  // Request package activation
  if ExtendedMethod then
    PkgxActivateApplicationEx(Options, ProcessId).RaiseOnError
  else
    PkgxActivateApplication(Options.Aumid, Options.Arguments, Options.Options,
      @ProcessId).RaiseOnError;

  // Report the returned PID
  tbxResult.Text := Rttix.Format(ProcessId);
end;

procedure TFormActivatePackage.btnCloseClick;
begin
  Close;
end;

procedure TFormActivatePackage.cbxMethodChange;
var
  Extended: Boolean;
begin
  Extended := cbxMethod.ItemIndex = 1;
  chkSession.Enabled := Extended;
  cbxSession.Enabled := Extended and chkSession.Checked;
  chkContext.Enabled := Extended;
  cbxContext.Enabled := Extended and chkContext.Checked;
end;

procedure TFormActivatePackage.tbxAumidChange;
var
  Status: TNtxStatus;
  DisplayName: String;
begin
  tbxResult.Text := '';
  Status := LookupDisplayName(tbxAumid.Text, DisplayName);

  if Status.IsSuccess then
  begin
    tbxDisplayName.Text := RtlxStringOrDefault(DisplayName, '(Empty)');
    tbxDisplayName.Hint := '';
  end
  else
  begin
    tbxDisplayName.Text := '(Unknown)';
    tbxDisplayName.Hint := Status.ToString;
  end;
end;

procedure TFormActivatePackage.tbxAumidEnter;
begin
  if Assigned(FAumidSuggestions) then
    Exit;

  FAumidSuggestions := ShlxPrepareDynamicSuggestions(SuggestAumids);
  ShlxEnableSuggestions(tbxAumid.Handle, FAumidSuggestions);
end;

procedure TFormActivatePackage.UiLibChildFormCreate;
begin
  fmxOptions.LoadType(TypeInfo(TActivateOptionsInternal))
end;

end.
