unit UI.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, NtUtils, Ntapi.WinNt, NtUiCommon.Forms,
  UI.Prototypes.Sid.Edit, NtUiFrame.Bits, NtUiFrame, TU.Access,
  NtUiLib.AutoCompletion;

type
  TAccessCheckForm = class(TChildForm)
    PageControlModes: TPageControl;
    TabByName: TTabSheet;
    TabByCID: TTabSheet;
    lblNameType: TLabel;
    tbxName: TEdit;
    lblName: TLabel;
    tbxNameType: TEdit;
    TabBySid: TTabSheet;
    ButtonClose: TButton;
    lblCidtType: TLabel;
    lblCid: TLabel;
    tbxCid: TEdit;
    cbxCidType: TComboBox;
    btnSelectCid: TButton;
    lblSidType: TLabel;
    cbxSidType: TComboBox;
    lblSid: TLabel;
    lblCidSubType: TLabel;
    cbxCidSubType: TComboBox;
    SidEditor: TSidEditor;
    lblSidLookupType: TLabel;
    tbxSidLookupType: TEdit;
    AccessMaskFrame: TBitsFrame;
    ButtonSecurity: TButton;
    TabByService: TTabSheet;
    lblServiceName: TLabel;
    tbxServiceName: TEdit;
    lblServiceType: TLabel;
    tbxServiceType: TEdit;
    TabSingleton: TTabSheet;
    lblSingleton: TLabel;
    cbxSingleton: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure tbxNameChange(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure tbxCidChange(Sender: TObject);
    procedure tbxSidChange(Sender: TObject);
    procedure btnSelectCidClick(Sender: TObject);
    procedure PageControlModesChange(Sender: TObject);
    procedure ButtonSecurityClick(Sender: TObject);
    procedure tbxServiceNameChange(Sender: TObject);
    procedure tbxServiceNameEnter(Sender: TObject);
    procedure cbxSingletonChange(Sender: TObject);
    procedure tbxNameEnter(Sender: TObject);
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
  NtUtils.Objects, NtUtils.Objects.Snapshots, NtUiLib.AutoCompletion.Namespace,
  NtUtils.Lsa.Sid, NtUiLib.Errors, DelphiUiLib.Reflection, UI.ProcessList,
  NtUiCommon.Prototypes, DelphiUiLib.Strings, DelphiUiLib.Reflection.Strings;

procedure TAccessCheckForm.btnSelectCidClick;
var
  IsThread: Boolean;
  ClientIdEx: TClientIdEx;
begin
  IsThread := cbxCidType.ItemIndex = 1;
  ClientIdEx := TProcessListDialog.Execute(Self, IsThread);

  if IsThread then
    tbxCid.Text := UiLibUIntToDec(ClientIdEx.ThreadID)
  else
    tbxCid.Text := UiLibUIntToDec(ClientIdEx.ProcessID);
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

procedure TAccessCheckForm.tbxCidChange;
var
  CidType: TCidType;
  Cid: NativeUInt;
begin
  if (cbxCidType.ItemIndex = 0) and (cbxCidSubType.ItemIndex = 0) then
    CidType := ctProcess
  else if (cbxCidType.ItemIndex = 0) and (cbxCidSubType.ItemIndex = 1) then
    CidType := ctProcessToken
  else if (cbxCidType.ItemIndex = 0) and (cbxCidSubType.ItemIndex = 2) then
    CidType := ctProcessDebugObject
  else if (cbxCidType.ItemIndex = 1) and (cbxCidSubType.ItemIndex = 0) then
    CidType := ctThread
  else if (cbxCidType.ItemIndex = 1) and (cbxCidSubType.ItemIndex = 1) then
    CidType := ctThreadToken
  else
    CidType := ctInvalid;

  if not (CidType in [ctProcess..ctThreadToken]) or
    not UiLibStringToUIntPtr(tbxCid.Text, Cid) then
  begin
    ResetAccessMask;
    Exit;
  end;

  ShowAccessMask(TuGetAccessCidObject(Cid, CidType));
end;

procedure TAccessCheckForm.tbxNameChange;
var
  Entry: TNamespaceEntry;
  EntryType: TObjectTypeInfo;
begin
  Entry := RtlxQueryNamespaceEntry(tbxName.Text);
  tbxNameType.Text := TType.Represent(Entry.KnownType).Text;

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
  tbxSidLookupType.Text := TType.Represent(Lookup.SidType).Text;

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

end.
