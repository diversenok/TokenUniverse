unit UI.Packages.Activate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NtUtilsUI.StdCtrls,
  NtUiLib.AutoCompletion, NtUtilsUI;

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
    lblHost: TLabel;
    procedure btnActivateClick(Sender: TObject);
    procedure tbxAumidEnter(Sender: TObject);
    procedure tbxAumidChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    FSuggestions: IAutoCompletionSuggestions;
  public
    { Public declarations }
  end;

implementation

uses
  Ntapi.WinNt, NtUtils, NtUtils.Processes.Create.Package, NtUtils.Packages,
  NtUtils.Packages.SRCache, NtUtils.Packages.Mrm, NtUtils.SysUtils,
  DelphiUiLib.LiteReflection, NtUiLib.Errors;

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
  ProcessId: TProcessId32;
begin
  tbxResult.Text := '';

  // Request package activation
  PkgxActivateApplication(tbxAumid.Text, tbxArguments.Text, 0,
    @ProcessId).RaiseOnError;

  // Report the returned PID
  tbxResult.Text := Rttix.Format(ProcessId);
end;

procedure TFormActivatePackage.btnCloseClick;
begin
  Close;
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
  if Assigned(FSuggestions) then
    Exit;

  FSuggestions := ShlxPrepareDynamicSuggestions(SuggestAumids);
  ShlxEnableSuggestions(tbxAumid.Handle, FSuggestions);
end;

end.
