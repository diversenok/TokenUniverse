unit UI.WinRT.Activate;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Ntapi.winrt, NtUtilsUI,
  NtUtilsUI.StdCtrls, VirtualTrees, NtUtilsUI.Tree, NtUtilsUI.Base,
  NtUtilsUI.Guid, NtUtilsUI.UmgrContext, NtUiLib.AutoCompletion;

type
  TActivationMode = (
    amInstance,
    amInstanceInPackage,
    amInstanceAsUser,
    amClass,
    amClassAsUser,
    amBrokerInstance,
    amBrokerClass
  );

  TFormActivateWinRTClass = class (TUiLibChildForm)
    lblMethod: TLabel;
    cbxMethod: TComboBox;
    lblResult: TLabel;
    btnRelease: TButton;
    btnActivate: TButton;
    btnClose: TButton;
    tbxResult: TUiLibEdit;
    lblCatalog: TLabel;
    uiCatalog: TUiLibTree;
    lblAcid: TLabel;
    tbxAcid: TUiLibEdit;
    lblPackage: TLabel;
    tbxPackage: TUiLibEdit;
    uiUserContext: TUiLibUmgrContextBox;
    chkxUserContext: TCheckBox;
    uiResult: TUiLibTree;
    lblBrokerType: TLabel;
    cbxBrokerType: TComboBox;
    tbxIid: TUiLibGuidBox;
    btnQueryIntf: TButton;
    procedure btnActivateClick(Sender: TObject);
    procedure cbxMethodChange(Sender: TObject);
    procedure UiLibChildFormCreate(Sender: TObject);
    procedure tbxAcidChange(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure chkxUserContextClick(Sender: TObject);
    procedure tbxPackageEnter(Sender: TObject);
    procedure btnQueryIntfClick(Sender: TObject);
  private
    FPackageSuggestions: IAutoCompletionSuggestions;
    FAcidSuggestions: IAutoCompletionSuggestions;
    FWinRTObject: IUnknown;
    FMode: TActivationMode;
    procedure SetWinRTObject(const Value: IUnknown);
  public
    { Public declarations }
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.ObjBase, Ntapi.ntregapi, Ntapi.comcatalog, NtUtils,
  NtUtils.SysUtils, NtUtils.Com, NtUtils.Registry, NtUtils.Packages.SRCache,
  NtUiLib.Errors, DelphiUiLib.Strings, DelphiUiLib.LiteReflection,
  UI.Com.Activate;

{$R *.dfm}

const
  colProperty = 0;
  colValue = 1;
  colMax = 2;

function CollectPackageSuggestions: TArray<String>;
var
  Status: TNtxStatus;
  PackageName: String;
begin
  Result := nil;

  for PackageName in PkgxSRCacheIteratePackageNames(@Status) do
  begin
    SetLength(Result, Succ(Length(Result)));
    Result[High(Result)] := PackageName;
  end;
end;

function CollectMachineAcidSuggestions(
  out Suggestions: TArray<String>
): TNtxStatus;
var
  hxKey: IHandle;
  MachineKeys: TArray<TNtxRegKey>;
  i: Integer;
begin
  Result := NtxOpenKey(hxKey, REG_PATH_MACHINE +
    '\Software\Microsoft\WindowsRuntime\ActivatableClassId',
    KEY_ENUMERATE_SUB_KEYS);

  if not Result.IsSuccess then
    Exit;

  Result := NtxEnumerateKeys(hxKey, MachineKeys);

  if not Result.IsSuccess then
    Exit;

  SetLength(Suggestions, Length(MachineKeys));

  for i := 0 to High(Suggestions) do
    Suggestions[i] := MachineKeys[i].Name;
end;

function CollectPackageAcidSuggestions(
  const PackageName: String;
  out Suggestions: TArray<String>
): TNtxStatus;
var
  hxAppxKey: IHandle;
  RepositoryRoot: String;
  hxHive, hxAcidKey: IHandle;
  AcidSubkeys: TArray<TNtxRegKey>;
  i: Integer;
begin
  // First, we need to know where the package repository root is; open the key
  // that stores its path
  Result := NtxOpenKey(hxAppxKey, REG_PATH_MACHINE +
    '\SOFTWARE\Microsoft\Windows\CurrentVersion\Appx', KEY_QUERY_VALUE);

  if not Result.IsSuccess then
    Exit;

  Result := NtxQueryValueKeyString(hxAppxKey, 'PackageRepositoryRoot',
    RepositoryRoot);

  if not Result.IsSuccess then
    Exit;

  // Load the package activation store app hive
  Result := NtxLoadKeyEx(hxHive, '\??\' + RepositoryRoot + '\Packages\' +
    PackageName + '\ActivationStore.dat', REG_PATH_APPKEY + '\' +
    RtlxGuidToString(RtlxRandomGuid), REG_APP_HIVE or REG_OPEN_READ_ONLY, nil,
    nil, AttributeBuilder.UseDesiredAccess(KEY_ENUMERATE_SUB_KEYS));

  if not Result.IsSuccess then
    Exit;

  // Localte the package ACID storage
  Result := NtxOpenKey(hxAcidKey, 'ActivatableClassId', KEY_ENUMERATE_SUB_KEYS,
    0, AttributeBuilder.UseRoot(hxHive));

  if not Result.IsSuccess then
    Exit;

  // Collect ACIDs
  Result := NtxEnumerateKeys(hxAcidKey, AcidSubkeys);

  if not Result.IsSuccess then
    Exit;

  SetLength(Suggestions, Length(AcidSubkeys));

  for i := 0 to High(AcidSubkeys) do
    Suggestions[i] := AcidSubkeys[i].Name;
end;

{ TFormActivateWinRTClass }

procedure TFormActivateWinRTClass.btnActivateClick;
var
  pv: IInspectable;
  UserContext: TUmgrContext;
  BrokerClsid: TClsid;
  BrokerClsidHint: String;
  Broker: IRuntimeBroker;
  Status: TNtxStatus;
begin
  SetWinRTObject(nil);

  if (FMode in [amInstanceAsUser, amClassAsUser]) and
    chkxUserContext.Checked then
    UserContext := uiUserContext.UserContext
  else
    UserContext := 0;

  case FMode of
    amInstance:
      RoxActivateInstance(tbxAcid.Text, pv).RaiseOnError;

    amInstanceInPackage:
      RoxActivateInstanceInPackage(tbxAcid.Text, tbxPackage.Text,
        pv).RaiseOnError;

    amInstanceAsUser:
      RoxActivateInstanceAsUser(tbxAcid.Text, UserContext, pv).RaiseOnError;

    amClass:
      RoxGetActivationFactory(tbxAcid.Text,
        IActivationFactory(pv)).RaiseOnError;

    amClassAsUser:
      RoxGetActivationFactoryAsUser(tbxAcid.Text, UserContext,
        IActivationFactory(pv)).RaiseOnError;

    amBrokerInstance, amBrokerClass:
    begin
      if cbxBrokerType.ItemIndex = 0 then
      begin
        BrokerClsid := CLSID_RuntimeBroker;
        BrokerClsidHint := 'CLSID_RuntimeBroker';
      end
      else
      begin
        BrokerClsid := CLSID_PerAppRuntimeBroker;
        BrokerClsidHint := 'CLSID_PerAppRuntimeBroker';
      end;

      ComxCreateInstance(BrokerClsid, IRuntimeBroker, Broker,
        BrokerClsidHint).RaiseOnError;

      if FMode = amBrokerInstance then
      begin
        Status.Location := 'IRuntimeBroker::ActivateInstance';
        Status.HResult := Broker.ActivateInstance(PWideChar(tbxAcid.Text), pv);
      end
      else
      begin
        Status.Location := 'IRuntimeBroker::GetActivationFactory';
        Status.HResult := Broker.GetActivationFactory(PWideChar(tbxAcid.Text),
          IActivationFactory, pv);
      end;

      Status.RaiseOnError;
    end
  else
    Abort;
  end;

  SetWinRTObject(pv);
end;

procedure TFormActivateWinRTClass.btnQueryIntfClick;
var
  pv: IUnknown;
  Status: TNtxStatus;
begin
  if Assigned(FWinRTObject) then
  begin
    Status.Location := 'IUnknown::QueryInterface';
    Status.HResult := FWinRTObject.QueryInterface(tbxIid.Guid, pv);
    Status.RaiseOnError;
    SetWinRTObject(pv);
  end;
end;

procedure TFormActivateWinRTClass.btnReleaseClick;
begin
  SetWinRTObject(nil);
end;

procedure TFormActivateWinRTClass.cbxMethodChange;
begin
  FMode := TActivationMode(cbxMethod.ItemIndex);
  lblPackage.Enabled := FMode in [amInstanceInPackage];
  tbxPackage.Enabled := FMode in [amInstanceInPackage];
  chkxUserContext.Enabled := FMode in [amInstanceAsUser, amClassAsUser];
  chkxUserContextClick(Sender);
  lblBrokerType.Enabled := FMode in [amBrokerInstance, amBrokerClass];
  cbxBrokerType.Enabled := FMode in [amBrokerInstance, amBrokerClass];
end;

procedure TFormActivateWinRTClass.chkxUserContextClick;
begin
  uiUserContext.Enabled := chkxUserContext.Enabled and chkxUserContext.Checked;
end;

procedure TFormActivateWinRTClass.SetWinRTObject;
var
  Inspectable: IInspectable;
  ParentNode, Node: IEditableNodeProvider;
  ClassName: String;
  Iids: TArray<TIid>;
  TrustLevel: TRoTrustLevel;
  i: Integer;
begin
  FWinRTObject := Value;
  btnRelease.Enabled := Assigned(Value);
  tbxIid.Enabled := Assigned(Value);
  btnQueryIntf.Enabled := Assigned(Value);
  tbxResult.Text := FormatInterfacePointer(Value);

  uiResult.Clear;
  if Assigned(Value) and Comx.QueryInterface(Value, Inspectable).IsSuccess then
  begin
    if RoxInspectableGetClassName(Inspectable, ClassName).IsSuccess then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'Class Name';
      Node.ColumnText[colValue] := ClassName;
      uiResult.AddChild(Node);
    end;

    if Inspectable.GetTrustLevel(TrustLevel) = S_OK then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'Trust Level';
      Node.ColumnText[colValue] := Rttix.Format(TrustLevel);
      uiResult.AddChild(Node);
    end;

    if RoxInspectableGetIids(Inspectable, Iids).IsSuccess then
    begin
      ParentNode := TEditableNodeProvider.Create(colMax);
      ParentNode.ColumnText[colProperty] := 'IIDs';
      ParentNode.ColumnText[colValue] := '(' + IntToStr(Length(Iids)) +
        ' entries)';
      uiResult.AddChild(ParentNode);
    end;

    for i := 0 to High(Iids) do
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := '[' + IntToStr(i) + ']';
      Node.ColumnText[colValue] := RtlxGuidToString(Iids[i]);
      uiResult.AddChild(Node, ParentNode);
    end;
  end;
end;

procedure TFormActivateWinRTClass.tbxAcidChange(Sender: TObject);
var
  Status: TNtxStatus;
  Info: IWinRTRuntimeClassInfo;
  Node: IEditableNodeProvider;
  ActivationType: TRoActivationType;
  ThreadingModel: TRoThreadingType;
  TrustLevel: TRoTrustLevel;
  RegistrationScope: TRoRegistrationScope;
  ActivateAsUser: TRoActivateAsUser;
  B: LongBool;
  S: PWideChar;
begin
  uiCatalog.Clear;

  if tbxAcid.Text <> '' then
  begin
    Status := ComxQueryAcid(Info, tbxAcid.Text, tbxPackage.Text);

    if not Status.IsSuccess then
    begin
      uiCatalog.EmptyListMessage := Status.ToString;
      Exit;
    end;
  end;

  uiCatalog.EmptyListMessage := 'No items to display';

  if tbxAcid.Text = '' then
    Exit;

  if Info.GetActivationType(ActivationType) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Activation Type';
    Node.ColumnText[colValue] := Rttix.Format(ActivationType);
    uiCatalog.AddChild(Node);
  end;

  if (Info.GetDllPath(S) = S_OK) and Assigned(S) then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'DLL Path';
    Node.ColumnText[colValue] := String(S);
    uiCatalog.AddChild(Node);
  end;

  if (Info.GetServerName(S) = S_OK) and Assigned(S) then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Server Name';
    Node.ColumnText[colValue] := String(S);
    uiCatalog.AddChild(Node);
  end;

  if Info.GetThreadingModel(ThreadingModel) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Threading Model';
    Node.ColumnText[colValue] := Rttix.Format(ThreadingModel);
    uiCatalog.AddChild(Node);
  end;

  if Info.GetTrustLevel(TrustLevel) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Trust Level';
    Node.ColumnText[colValue] := Rttix.Format(TrustLevel);
    uiCatalog.AddChild(Node);
  end;

  if Info.GetRegistrationScope(RegistrationScope) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Registration Scope';
    Node.ColumnText[colValue] := Rttix.Format(RegistrationScope);
    uiCatalog.AddChild(Node);
  end;

  if Info.GetActivateAsUser(ActivateAsUser) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Activate As User';
    Node.ColumnText[colValue] := Rttix.Format(ActivateAsUser);
    uiCatalog.AddChild(Node);
  end;

  if Info.GetActivateInSharedBroker(B) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Activate In Shared Broker';
    Node.ColumnText[colValue] := BooleanToString(B);
    uiCatalog.AddChild(Node);
  end;

  if Info.GetActivateInBrokerForMediumILContainer(B) = S_OK then
  begin
    Node := TEditableNodeProvider.Create(colMax);
    Node.ColumnText[colProperty] := 'Broker For Medium IL Container';
    Node.ColumnText[colValue] := BooleanToString(B);
    uiCatalog.AddChild(Node);
  end;
end;

procedure TFormActivateWinRTClass.tbxPackageEnter;
begin
  if not Assigned(FPackageSuggestions) then
  begin
    FPackageSuggestions := ShlxPrepareStatisSuggestions(
      CollectPackageSuggestions);
    ShlxEnableSuggestions(tbxPackage.Handle, FPackageSuggestions);
  end;
end;

procedure TFormActivateWinRTClass.UiLibChildFormCreate;
begin
  FAcidSuggestions := ShlxPrepareDynamicSuggestions(
    function (
      const Root: String;
      out Suggestions: TArray<String>
    ): TNtxStatus
    begin
      if Root <> '' then
      begin
        Suggestions := nil;
        Exit(NtxSuccess);
      end;

      if tbxPackage.Enabled and (tbxPackage.Text <> '') then
        Result := CollectPackageAcidSuggestions(tbxPackage.Text, Suggestions)
      else
        Result := CollectMachineAcidSuggestions(Suggestions);
    end
  );
  ShlxEnableSuggestions(tbxAcid.Handle, FAcidSuggestions);
  tbxIid.Guid := IInspectable;
end;

end.
