unit TU.ObjPicker;

interface

uses
  Winapi.Windows;

type
  TSelectedUserCallback = procedure(UserName: String) of object;

procedure CallObjectPicker(Parent: HWND; UserCallback: TSelectedUserCallback);

implementation

uses
  Winapi.ActiveX;

const
  CLSID_DsObjectPicker: TGUID = (D1: $17D6CCD8; D2: $3B7B; D3: $11D2;
    D4: ($B9, $E0, $00, $C0, $4F, $D8, $DB, $F7));

  IID_IDsObjectPicker: TGUID = (D1: $0C87E64E; D2: $3B7A; D3: $11D2;
    D4: ($B9, $E0, $00, $C0, $4F, $D8, $DB, $F7));

{$MINENUMSIZE 4}

type
  TDsScopeTypes = (
    DsOpScopeTypeTargetComputer = $1,
    DsOpScopeTypeUplevelJoinedDomain = $2,
    DsOpScopeTypeDownlevelJoinedDomain = $4,
    DsOpScopeTypeEnterpriseDomain = $8,
    DsOpScopeTypeGlobalCatalog = $10,
    DsOpScopeTypeExternalUplevelDomain = $20,
    DsOpScopeTypeExternalDownlevelDomain = $40,
    DsOpScopeTypeWorkgroup = $80,
    DsOpScopeTypeUserEnteredUplevelScope = $100,
    DsOpScopeTypeUserEnteredDownlevelScope = $200
  );

  TDsScopeFlags = (
    DsOpScopeFlagStartingScope = $1,
    DsOpScopeFlagWantProviderWinnt = $2,
    DsOpScopeFlagWantProviderLdap = $4,
    DsOpScopeFlagWantProviderGc = $8,
    DsOpScopeFlagWantSidPath = $10,
    DsOpScopeFlagWantDownlevelBuiltinPath = $20,
    DsOpScopeFlagDefaultFilterUsers = $40,
    DsOpScopeFlagDefaultFilterGroups = $80,
    DsOpScopeFlagDefaultFilterComputers = $100,
    DsOpScopeFlagDefaultFilterContacts = $200
  );

  TDsDownlevelFilters = (
    DsOpDownlevelFilterUsers = Integer($80000001),
    DsOpDownlevelFilterLocalGroups = Integer($80000002),
    DsOpDownlevelFilterGlobalGroups = Integer($80000004),
    DsOpDownlevelFilterComputers = Integer($80000008),
    DsOpDownlevelFilterWorld = Integer($80000010),
    DsOpDownlevelFilterAuthenticatedUser = Integer($80000020),
    DsOpDownlevelFilterAnonymous = Integer($80000040),
    DsOpDownlevelFilterBatch = Integer($80000080),
    DsOpDownlevelFilterCreatorOwner = Integer($80000100),
    DsOpDownlevelFilterCreatorGroup = Integer($80000200),
    DsOpDownlevelFilterDialup = Integer($80000400),
    DsOpDownlevelFilterInteractive = Integer($80000800),
    DsOpDownlevelFilterNetwork = Integer($80001000),
    DsOpDownlevelFilterService = Integer($80002000),
    DsOpDownlevelFilterSystem = Integer($80004000),
    DsOpDownlevelFilterExcludeBuiltinGroups = Integer($80008000),
    DsOpDownlevelFilterTerminalServer = Integer($80010000),
    DsOpDownlevelFilterAllWellknownSids = Integer($80020000),
    DsOpDownlevelFilterLocalService = Integer($80040000),
    DsOpDownlevelFilterNetworkService = Integer($80080000),
    DsOpDownlevelFilterRemoteLogon = Integer($80100000),
    DsOpDownlevelFilterInternetUser = Integer($80200000),
    DsOpDownlevelFilterOwnerRights = Integer($80400000),
    DsOpDownlevelFilterServices = Integer($80800000)
  );

  TDsFilters = (
    DsOpFilterIncludeAdvancedView = $1,
    DsOpFilterUsers = $2,
    DsOpFilterBuiltinGroups = $4,
    DsOpFilterWellKnownPrincipals = $8,
    DsOpFilterUniversalGroupsDl = $10,
    DsOpFilterUniversalGroupsSe = $20,
    DsOpFilterGlobalGroupsDl = $40,
    DsOpFilterGlobalGroupsSe = $80,
    DsOpFilterDomainLocalGroupsDl = $100,
    DsOpFilterDomainLocalGroupsSe = $200,
    DsOpFilterContacts = $400,
    DsOpFilterComputers = $800,
    DsOpFilterServiceAccounts = $1000,
    DsOpFilterPasswordsettingsObjects = $2000
  );

  TDsOpUpLevelFilterFlags = record
    flBothModes: TDsFilters;
    flMixedModeOnly: TDsFilters;
    flNativeModeOnly: TDsFilters;
  end;

  TDsOpFilterFlags = record
    Uplevel: TDsOpUpLevelFilterFlags;
    flDownlevel: TDsDownlevelFilters;
  end;

  TDsOpScopeInitInfo = record
    cbSize: Cardinal;
    flType: TDsScopeTypes;
    flScope: TDsScopeFlags;
    FilterFlags: TDsOpFilterFlags;
    pwzDcName: PWideChar;
    pwzADsPath: PWideChar;
    hr: HRESULT;
  end;

  TDsFlags = (
    DsOpFlagMultiselect = $1,
    DsOpFlagSkipTargetComputerDcCheck = $2
  );

  TDsOpInitInfo = record
    cbSize: Cardinal;
    pwzTargetComputer: PWideChar;
    cDsScopeInfos: Cardinal;
    aDsScopeInfos: array of TDsOpScopeInitInfo;
    flOptions: TDsFlags;
    cAttributesToFetch: Cardinal;
    apwzAttributeNames: array of PWideChar;
  end;

  IDsObjectPicker = interface(IUnknown)
    ['{0C87E64E-3B7A-11D2-B9E0-00C04FD8DBF7}']
    function Initialize(const pInitInfo: TDsOpInitInfo): HRESULT; stdcall;
    function InvokeDialog(hwndParent: HWND; out ppdoSelections: IDataObject)
      : HRESULT; stdcall;
  end;

  TDsSelection = record
    pwzName: PWideChar;
    pwzADsPath: PWideChar;
    pwzClass: PWideChar;
    pwzUPN: PWideChar;
    pvarFetchedAttributes: POleVariant;
    flScopeType: Cardinal;
  end;

  TDsSelectionList = record
    cItems: Integer;
    cFetchedAttributes: Cardinal;
    aDsSelection: array [Word] of TDsSelection;
  end;
  PDSSelectionList = ^TDsSelectionList;

function InitObjectPicker(Picker: IDsObjectPicker): HRESULT;
var
  InitInfo: TDsOpInitInfo;
begin
  if not Assigned(Picker) then
    Exit(E_INVALIDARG);

  FillChar(InitInfo, SizeOf(InitInfo), 0);
  with InitInfo do
  begin
    cbSize := SizeOf(InitInfo);

    cDsScopeInfos := 1;
    SetLength(aDsScopeInfos, cDsScopeInfos);

    with aDsScopeInfos[0] do
    begin
      cbSize := SizeOf(TDsOpScopeInitInfo);
      flType := TDsScopeTypes($3FF);
      flScope := TDsScopeFlags($1C1);
      FilterFlags.Uplevel.flBothModes := TDsFilters($BFF);
      FilterFlags.flDownlevel := TDsDownlevelFilters($8002200F);
    end;

    flOptions := DsOpFlagSkipTargetComputerDcCheck;
  end;

  Result := Picker.Initialize(InitInfo);
end;

function ProcessSelectedObjects(DatObj: IDataObject;
  UserCallback: TSelectedUserCallback): HRESULT;
var
  Medium: TStgMedium;
  FormatEtc: TFormatEtc;
  SelLst: PDSSelectionList;
  i: Integer;
begin
  if not Assigned(DatObj) then
    Exit(E_INVALIDARG);

  with FormatEtc do
  begin
    cfFormat := RegisterClipboardFormatW('CFSTR_DSOP_DS_SELECTION_LIST');
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;

  Result := DatObj.GetData(FormatEtc, Medium);

  if not Succeeded(Result) then
    Exit;

  SelLst := PDSSelectionList(GlobalLock(Medium.hGlobal));
  try
    for i := 0 to SelLst.cItems - 1 do
      UserCallback(WideCharToString(SelLst.aDsSelection[i].pwzName));
  finally
    GlobalUnlock(Medium.hGlobal);
    ReleaseStgMedium(Medium);
  end;
end;

procedure CallObjectPicker(Parent: HWND; UserCallback: TSelectedUserCallback);
var
  Picker: IDsObjectPicker;
  DatObj: IDataObject;
begin
  if Succeeded(CoInitialize(nil)) then
    try
      if Succeeded(CoCreateInstance(CLSID_DsObjectPicker, nil,
        CLSCTX_INPROC_SERVER, IID_IDsObjectPicker, Picker)) then
        if Succeeded(InitObjectPicker(Picker)) then
          case Picker.InvokeDialog(Parent, DatObj) of
            S_OK: ProcessSelectedObjects(DatObj, UserCallback);
            S_FALSE: ;
          end;
    finally
      CoUninitialize;
    end;
end;

end.
