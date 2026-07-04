unit UI.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, NtUtils, Ntapi.WinNt, NtUtilsUI, NtUtilsUI.StdCtrls,
  UI.Prototypes.Sid.Edit, NtUiFrame.Bits, TU.Access,
  NtUiLib.AutoCompletion;

type
  TAccessCheckForm = class(TUiLibChildForm)
    PageControlModes: TPageControl;
    TabByName: TTabSheet;
    TabByPid: TTabSheet;
    lblNameType: TLabel;
    tbxName: TUiLibEdit;
    lblName: TLabel;
    tbxNameType: TUiLibEdit;
    TabBySid: TTabSheet;
    ButtonClose: TButton;
    lblPidType: TLabel;
    lblPid: TLabel;
    tbxPid: TUiLibEdit;
    cbxPidType: TUiLibComboBox;
    btnSelectPid: TButton;
    lblSidType: TLabel;
    cbxSidType: TUiLibComboBox;
    lblSid: TLabel;
    SidEditor: TSidEditor;
    lblSidLookupType: TLabel;
    tbxSidLookupType: TUiLibEdit;
    AccessMaskFrame: TBitsFrame;
    ButtonSecurity: TButton;
    TabByService: TTabSheet;
    lblServiceName: TLabel;
    tbxServiceName: TUiLibEdit;
    lblServiceType: TLabel;
    tbxServiceType: TUiLibEdit;
    TabSingleton: TTabSheet;
    lblSingleton: TLabel;
    cbxSingleton: TUiLibComboBox;
    TabByTid: TTabSheet;
    lblTid: TLabel;
    tbxTid: TUiLibEdit;
    btnSelectTid: TButton;
    lblTidType: TLabel;
    cbxTidType: TUiLibComboBox;
    procedure FormCreate(Sender: TObject);
    procedure tbxNameChange(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure tbxPidChange(Sender: TObject);
    procedure tbxSidChange(Sender: TObject);
    procedure btnSelectPidClick(Sender: TObject);
    procedure PageControlModesChange(Sender: TObject);
    procedure ButtonSecurityClick(Sender: TObject);
    procedure tbxServiceNameChange(Sender: TObject);
    procedure tbxServiceNameEnter(Sender: TObject);
    procedure cbxSingletonChange(Sender: TObject);
    procedure tbxNameEnter(Sender: TObject);
    procedure btnSelectTidClick(Sender: TObject);
    procedure tbxTidChange(Sender: TObject);
    procedure UiLibChildFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FContext: TAccessContext;
    FNamespaceSuggestions: IAutoCompletionSuggestions;
    FServicesSuggestions: IAutoCompletionSuggestions;
    procedure ShowAccessMask(const Context: TAccessContext);
    procedure ResetAccessMask;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Ntapi.ntdef, NtUtils.Objects, NtUtils.Objects.Snapshots,
  NtUiLib.AutoCompletion.Namespace, NtUtils.Lsa.Sid, NtUiLib.Errors,
  DelphiUiLib.LiteReflection, NtUiCommon.Prototypes, DelphiUiLib.Strings,
  NtUtilsUI.Components;

procedure TAccessCheckForm.btnSelectPidClick;
begin
  tbxPid.Text := UiLibUIntToDec(UiLibPickProcess(Self));
end;

procedure TAccessCheckForm.btnSelectTidClick;
begin
  tbxTid.Text := UiLibUIntToDec(UiLibPickThread(Self).UniqueThread)
end;

procedure TAccessCheckForm.ButtonCloseClick;
begin
  Close;
end;

procedure TAccessCheckForm.ButtonSecurityClick;
begin
  if not Assigned(NtUiLibShowSecurity) or
    not Assigned(FContext.Security.HandleProvider) or
    not Assigned(FContext.Security.QueryFunction) or
    not Assigned(FContext.Security.SetFunction) then
    Exit;

  NtUiLibShowSecurity(FContext.Security);
end;

procedure TAccessCheckForm.cbxSingletonChange;
begin
  if (cbxSingleton.ItemIndex > Ord(ltInvalid)) and (cbxSingleton.ItemIndex <=
    Ord(High(TSingletonType))) then
    ShowAccessMask(TuGetAccessSingletonObject(TSingletonType(
      cbxSingleton.ItemIndex)))
  else
    ResetAccessMask;
end;

procedure TAccessCheckForm.FormCreate;
begin
  AccessMaskFrame.IsReadOnly := True;
  ResetAccessMask;

  SidEditor.OnSidChanged := tbxSidChange;
end;

procedure TAccessCheckForm.PageControlModesChange;
begin
  cbxSingleton.ItemIndex := 0;
  ResetAccessMask;
end;

procedure TAccessCheckForm.ResetAccessMask;
begin
  ShowAccessMask(Default(TAccessContext));
end;

procedure TAccessCheckForm.ShowAccessMask;
var
  MaskType: Pointer;
begin
  FContext := Context;

  if Assigned(Context.Security.AccessMaskType) then
    MaskType := Context.Security.AccessMaskType
  else
    MaskType := TypeInfo(TAccessMask);

  ButtonSecurity.Enabled := Assigned(NtUiLibShowSecurity) and
    Assigned(Context.Security.HandleProvider) and
    Assigned(Context.Security.QueryFunction) and
    Assigned(Context.Security.SetFunction);

  AccessMaskFrame.LoadAccessMaskType(MaskType, Context.Security.GenericMapping,
    False, False);
  AccessMaskFrame.Value := Context.MaximumAccess;
end;

procedure TAccessCheckForm.tbxPidChange;
var
  CidType: TCidType;
  Pid: NativeUInt;
begin
  case cbxPidType.ItemIndex of
    0: CidType := ctProcess;
    1: CidType := ctProcessToken;
    2: CidType := ctProcessDebugObject;
  else
    CidType := ctInvalid;
  end;

  if (CidType = ctInvalid) or not UiLibStringToUIntPtr(tbxPid.Text, Pid) then
  begin
    ResetAccessMask;
    Exit;
  end;

  ShowAccessMask(TuGetAccessCidObject(Pid, CidType));
end;

procedure TAccessCheckForm.tbxNameChange;
var
  Entry: TNamespaceEntry;
  EntryType: TNtxObjectTypeInfo;
begin
  Entry := RtlxQueryNamespaceEntry(tbxName.Text);
  tbxNameType.Text := Rttix.Format(Entry.KnownType);

  if RtlxFindKernelType(Entry.TypeName, EntryType).IsSuccess then
    ShowAccessMask(TuGetAccessNamedObject(Entry))
  else
    ResetAccessMask;
end;

procedure TAccessCheckForm.tbxNameEnter;
begin
  if not Assigned(FNamespaceSuggestions) then
  begin
    FNamespaceSuggestions := ShlxPrepareNamespaceSuggestions(
      NT_NAMESPACE_KNOWN_TYPES);
    ShlxEnableSuggestions(tbxName.Handle, FNamespaceSuggestions);
  end;
end;

procedure TAccessCheckForm.tbxServiceNameChange;
begin
  if tbxServiceName.Text <> '' then
    ShowAccessMask(TuGetAccessServiceObject(tbxServiceName.Text))
  else
    ResetAccessMask;
end;

procedure TAccessCheckForm.tbxServiceNameEnter;
begin
  if not Assigned(FServicesSuggestions) then
  begin
    FServicesSuggestions := ShlxPrepareStatisSuggestions(TuCollectServiceNames);
    ShlxEnableSuggestions(tbxServiceName.Handle, FServicesSuggestions);
  end;
end;

procedure TAccessCheckForm.tbxSidChange;
var
  Sid: ISid;
  SidType: TSidType;
  Lookup: TTranslatedName;
begin
  if (SidEditor.tbxSid.Text = '') or not SidEditor.TryGetSid(Sid).IsSuccess then
  begin
    ResetAccessMask;
    Exit;
  end;

  SidType := stInvalid;
  LsaxLookupSid(Sid, Lookup);
  tbxSidLookupType.Text := Rttix.Format(Lookup.SidType);

  if cbxSidType.ItemIndex = 0 then
    SidType := stLsaAccount
  else if cbxSidType.ItemIndex = 1 then
    case Lookup.SidType of
      SidTypeUser:   SidType := stSamUser;
      SidTypeGroup:  SidType := stSamGroup;
      SidTypeDomain: SidType := stSamDomain;
      SidTypeAlias:  SidType := stSamAlias;
    end;

  if SidType = stInvalid then
  begin
    ResetAccessMask;
    Exit;
  end;

  ShowAccessMask(TuGetAccessSidObject(Sid, SidType));
end;

procedure TAccessCheckForm.tbxTidChange;
var
  CidType: TCidType;
  Tid: NativeUInt;
begin
  case cbxTidType.ItemIndex of
    0: CidType := ctThread;
    1: CidType := ctThreadToken;
  else
    CidType := ctInvalid;
  end;

  if (CidType = ctInvalid) or not UiLibStringToUIntPtr(tbxTid.Text, Tid) then
  begin
    ResetAccessMask;
    Exit;
  end;

  ShowAccessMask(TuGetAccessCidObject(Tid, CidType));
end;

procedure TAccessCheckForm.UiLibChildFormKeyDown;
begin
  // Ctrl+Digit to switch between tabs
  if (Shift = [ssCtrl]) and (Key >= Ord('1')) and (Key <= Ord('6')) then
    PageControlModes.ActivePageIndex := Key - Ord('1');
end;

end.
