unit UI.Com.Activate;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NtUtilsUI.StdCtrls,
  NtUtilsUI, NtUtilsUI.Base, NtUtilsUI.Guid, VirtualTrees, NtUtilsUI.Tree,
  NtUiFrame.Bits, NtUtils, NtUiLib.AutoCompletion, NtUtilsUI.Number,
  NtUtilsUI.SessionID, NtUtilsUI.UmgrContext;

type
  TActivationMode = (
    amInstance,
    amInstanceEx,
    amInstanceAsUser,
    amClass,
    amStandard,
    amStanadardClass
  );

  TFormActivateComClass = class (TUiLibChildForm)
    lblMethod: TLabel;
    cbxMethod: TComboBox;
    lblClsid: TLabel;
    tbxClsid: TUiLibGuidBox;
    uiCatalog: TUiLibTree;
    Label1: TLabel;
    uiClsCtx: TBitsFrame;
    Label2: TLabel;
    chkxSessionId: TCheckBox;
    uiSessionId: TUiLibSessionIdBox;
    cbxLuaLevel: TComboBox;
    lblLuaLevel: TLabel;
    tbxIid: TUiLibGuidBox;
    lblIid: TLabel;
    btnRelease: TButton;
    lblResult: TLabel;
    btnActivate: TButton;
    btnClose: TButton;
    tbxResult: TUiLibEdit;
    uiUserContext: TUiLibUmgrContextBox;
    chkxUserContext: TCheckBox;
    btnQueryIntf: TButton;
    procedure tbxClsidChange(Sender: TObject);
    procedure UiLibChildFormCreate(Sender: TObject);
    procedure cbxMethodChange(Sender: TObject);
    procedure btnActivateClick(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure chkxSessionIdClick(Sender: TObject);
    procedure chkxUserContextClick(Sender: TObject);
    procedure btnQueryIntfClick(Sender: TObject);
  private
    FClsidSuggestions: IAutoCompletionSuggestions;
    FComObject: IUnknown;
    FMode: TActivationMode;
    procedure SetComObject(const Value: IUnknown);
  public
    { Public declarations }
  end;

implementation

uses
  Ntapi.WinNt, Ntapi.ObjBase, Ntapi.ObjIdl, Ntapi.comcatalog, Ntapi.ntregapi,
  Ntapi.ntseapi, NtUtils.SysUtils, DelphiUtils.Arrays, NtUtils.Com,
  NtUtils.Registry, NtUtils.Tokens.Info, NtUtils.Security.Sid, NtUiLib.Errors,
  DelphiUiLib.LiteReflection;

{$R *.dfm}

const
  colProperty = 0;
  colValue = 1;
  colMax = 2;

function CollectClsidsSuggestions: TArray<String>;
var
  hxKey: IHandle;
  UserSid: ISid;
  MachineClsidKeys, UserClsidKey: TArray<TNtxRegKey>;
begin
  // Collect machine-wide CLSIDs
  if NtxOpenKey(hxKey, REG_PATH_MACHINE + '\Software\Classes\CLSID',
    KEY_ENUMERATE_SUB_KEYS).IsSuccess then
    NtxEnumerateKeys(hxKey, MachineClsidKeys);

  // Collect user-wide CLSIDs
  if NtxQuerySidToken(NtxCurrentEffectiveToken, TokenUser, UserSid).IsSuccess
    and NtxOpenKey(hxKey, REG_PATH_USER + '\' + RtlxSidToStringNoError(UserSid)
    + '_Classes\CLSID', KEY_ENUMERATE_SUB_KEYS).IsSuccess then
    NtxEnumerateKeys(hxKey, UserClsidKey);

  // Convert into GUID strings
  Result := TArray.Convert<TNtxRegKey, String>(MachineClsidKeys + UserClsidKey,
    function (const Key: TNtxRegKey; out Clsid: String): Boolean
    var
      Dummy: TGuid;
    begin
      Clsid := Key.Name;
      Result := RtlxStringToGuid(Key.Name, Dummy).IsSuccess;
    end
  );
end;

procedure TFormActivateComClass.btnActivateClick;
var
  Activator: IStandardActivator;
  Properties: ISpecialSystemProperties;
  pv: IUnknown;
  Status: TNtxStatus;
  Clsid, Iid: TIid;
  UserContext: TUmgrContext;
  ClsCtx: TClsCtx;
begin
  SetComObject(nil);
  Clsid := tbxClsid.Guid;
  Iid := tbxIid.Guid;
  ClsCtx := TClsCtx(uiClsCtx.Value);

  case FMode of
    amInstance:
      ComxCreateInstance(Clsid, Iid, pv, '', ClsCtx).RaiseOnError;

    amInstanceEx:
      ComxCreateInstanceEx(Clsid, Iid, pv, '', ClsCtx).RaiseOnError;

    amInstanceAsUser:
    begin
      if chkxUserContext.Checked then
        UserContext := uiUserContext.UserContext
      else
        UserContext := 0;

      ComxCreateInstanceAsUser(Clsid, UserContext, Iid, pv, '',
        ClsCtx).RaiseOnError;
    end;

    amClass:
      ComxGetClassObject(Clsid, Iid, pv, '', ClsCtx).RaiseOnError;

    amStandard, amStanadardClass:
    begin
      ComxCreateCustomActivator(Activator, Properties).RaiseOnError;

      if chkxSessionId.Checked then
      begin
        Status.Location := 'ISpecialSystemProperties::SetSessionId';
        Status.HResult := Properties.SetSessionId(uiSessionId.SessionID, False,
          True);
        Status.RaiseOnError;
      end;

      if TRunLevel(cbxLuaLevel.ItemIndex) in [RUNLEVEL_HIGHEST,
        RUNLEVEL_ADMIN] then
      begin
        Status.Location := 'ISpecialSystemProperties::SetLUARunLevel';
        Status.HResult := Properties.SetLUARunLevel(TRunLevel(
          cbxLuaLevel.ItemIndex), Handle);
        Status.RaiseOnError;
      end;

      if FMode = amStandard then
        ComxStandardActivatorCreateInstance(Activator, Clsid, Iid, pv, '',
          ClsCtx).RaiseOnError
      else
        ComxStandardActivatorGetClassObject(Activator, Clsid, Iid, pv, '',
          ClsCtx).RaiseOnError;
    end;
  else
    Abort;
  end;

  SetComObject(pv);
end;

procedure TFormActivateComClass.btnQueryIntfClick;
var
  pv: IUnknown;
  Status: TNtxStatus;
begin
  if Assigned(FComObject) then
  begin
    Status.Location := 'IUnknown::QueryInterface';
    Status.HResult := FComObject.QueryInterface(tbxIid.Guid, pv);
    Status.RaiseOnError;
    SetComObject(pv);
  end;
end;

procedure TFormActivateComClass.btnReleaseClick;
begin
  SetComObject(nil);
end;

procedure TFormActivateComClass.cbxMethodChange;
begin
  FMode := TActivationMode(cbxMethod.ItemIndex);
  chkxSessionId.Enabled := FMode in [amStandard, amStanadardClass];
  chkxSessionIdClick(Sender);
  lblLuaLevel.Enabled := FMode in [amStandard, amStanadardClass];
  cbxLuaLevel.Enabled := FMode in [amStandard, amStanadardClass];
  chkxUserContext.Enabled := FMode in [amInstanceAsUser];
  chkxUserContextClick(Sender);
end;

procedure TFormActivateComClass.chkxSessionIdClick;
begin
  uiSessionId.Enabled := chkxSessionId.Checked;
end;

procedure TFormActivateComClass.chkxUserContextClick;
begin
  uiUserContext.Enabled := chkxUserContext.Checked;
end;

procedure TFormActivateComClass.SetComObject;
var
  Locality: TRpcOptServerLocalityValues;
  ServerPid: TProcessId32;
  ResultInfo: String;
begin
  FComObject := Value;
  btnRelease.Enabled := Assigned(Value);
  btnQueryIntf.Enabled := Assigned(Value);

  if Assigned(Value) then
  begin
    ResultInfo := 'Pointer @ ' + RtlxIntToHex(UIntPtr(Value),
      NUMERIC_WIDTH_ROUND_TO_GROUP);

    if ComxQueryServerLocality(Value, Locality).IsSuccess then
      ResultInfo := ResultInfo + ' - ' + Rttix.Format(Locality) + ' Proxy';

    if ComxQueryServerProcessId(Value, ServerPid).IsSuccess then
      ResultInfo := ResultInfo + ' - ' + Rttix.Format(ServerPid);
  end
  else
    ResultInfo := '';

  tbxResult.Text := ResultInfo;
end;

procedure TFormActivateComClass.tbxClsidChange;
var
  Status: TNtxStatus;
  Info: IUnknown;
  ClassInfo: IComClassInfo;
  ClassicInfo: IClassClassicInfo;
  Node: IEditableNodeProvider;
  ThreadModel: TThreadingModel;
  S: PWideChar;
begin
  uiCatalog.Clear;
  Status := ComxQueryClsid(tbxClsid.Guid, Info);

  if not Status.IsSuccess then
  begin
    uiCatalog.EmptyListMessage := Status.ToString;
    Exit;
  end
  else
    uiCatalog.EmptyListMessage := 'No items to display';

  if Comx.QueryInterface(Info, ClassInfo).IsSuccess then
  begin
    if (ClassInfo.GetClassNameW(S) = S_OK) and Assigned(S) then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'Class Name';
      Node.ColumnText[colValue] := String(S);
      uiCatalog.AddChild(Node);
    end;
  end;

  if Comx.QueryInterface(Info, ClassicInfo).IsSuccess then
  begin
    if ClassicInfo.GetThreadingModel(ThreadModel) = S_OK then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'Threading Model';
      Node.ColumnText[colValue] := Rttix.Format(ThreadModel);
      uiCatalog.AddChild(Node);
    end;

    if ClassicInfo.GetModulePath(CLSCTX_INPROC_SERVER, S) = S_OK then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'In-process Server';
      Node.ColumnText[colValue] := String(S);
      uiCatalog.AddChild(Node);
    end;

    if ClassicInfo.GetModulePath(CLSCTX_INPROC_HANDLER, S) = S_OK then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'In-process Handler';
      Node.ColumnText[colValue] := String(S);
      uiCatalog.AddChild(Node);
    end;

    if ClassicInfo.GetModulePath(CLSCTX_LOCAL_SERVER, S) = S_OK then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'Local Server';
      Node.ColumnText[colValue] := String(S);
      uiCatalog.AddChild(Node);
    end;

    if (ClassicInfo.GetSurrogateCommandLine(S) = S_OK) and Assigned(S) then
    begin
      Node := TEditableNodeProvider.Create(colMax);
      Node.ColumnText[colProperty] := 'Surrogate Command';
      Node.ColumnText[colValue] := String(S);
      uiCatalog.AddChild(Node);
    end;
  end;
end;

procedure TFormActivateComClass.UiLibChildFormCreate;
begin
  FClsidSuggestions := ShlxPrepareStatisSuggestions(CollectClsidsSuggestions);
  ShlxEnableSuggestions(tbxClsid.EditHandle, FClsidSuggestions);
  uiClsCtx.LoadType(TypeInfo(TClsCtx));
  uiClsCtx.Value := CLSCTX_ALL;
  tbxIid.Guid := IUnknown;
end;

end.
