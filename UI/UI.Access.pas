unit UI.Access;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, NtUtils, Ntapi.WinNt, UI.Prototypes.Forms,
  UI.Prototypes.AccessMask, UI.Prototypes.Sid.Edit;

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
    AccessMaskFrame: TAccessMaskFrame;
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
    procedure FormCreate(Sender: TObject);
    procedure tbxNameChange(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure tbxCidChange(Sender: TObject);
    procedure tbxSidChange(Sender: TObject);
    procedure btnSelectCidClick(Sender: TObject);
    procedure PageControlModesChange(Sender: TObject);
  private
    procedure ShowAccessMask(
      Value: TAccessMask;
      MaskType: Pointer;
      const GenericMapping: TGenericMapping
    );
    procedure ResetAccessMask;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  NtUtils.Objects, NtUtils.Objects.Snapshots, NtUiLib.AutoCompletion.Namespace,
  NtUtils.SysUtils, NtUtils.Lsa.Sid, NtUiLib.Errors, NtUiLib.AutoCompletion,
  DelphiUiLib.Reflection.Numeric, TU.Access, UI.ProcessList;

procedure TAccessCheckForm.btnSelectCidClick;
var
  IsThread: Boolean;
  ClientIdEx: TClientIdEx;
begin
  IsThread := cbxCidType.ItemIndex = 1;
  ClientIdEx := TProcessListDialog.Execute(Self, IsThread);

  if IsThread then
    tbxCid.Text := RtlxUIntToStr(ClientIdEx.ThreadID)
  else
    tbxCid.Text := RtlxUIntToStr(ClientIdEx.ProcessID);
end;

procedure TAccessCheckForm.ButtonCloseClick;
begin
  Close;
end;

procedure TAccessCheckForm.FormCreate;
begin
  AccessMaskFrame.IsReadOnly := True;
  ResetAccessMask;

  SidEditor.OnSidChanged := tbxSidChange;
  ShlxEnableNamespaceSuggestions(tbxName.Handle, NT_NAMESPACE_KNOWN_TYPES);
end;

procedure TAccessCheckForm.PageControlModesChange;
begin
  ResetAccessMask;
end;

procedure TAccessCheckForm.ResetAccessMask;
begin
  ShowAccessMask(0, nil, Default(TGenericMapping));
end;

procedure TAccessCheckForm.ShowAccessMask;
begin
  AccessMaskFrame.LoadType(MaskType, GenericMapping, True);
  AccessMaskFrame.AccessMask := Value;
end;

procedure TAccessCheckForm.tbxCidChange;
var
  CidType: TCidType;
  Cid: Cardinal;
  ObjectInfo: TObjectTypeInfo;
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
    not RtlxStrToUInt(tbxCid.Text, Cid) then
  begin
    ResetAccessMask;
    Exit;
  end;

  if not RtlxFindKernelType(TuCidTypeName(CidType), ObjectInfo).IsSuccess then
    ObjectInfo := Default(TObjectTypeInfo);

  ShowAccessMask(
    TuGetMaximumAccessCidObject(Cid, CidType),
    TuCidAccessMaskType(CidType),
    ObjectInfo.Other.GenericMapping
  );
end;

procedure TAccessCheckForm.tbxNameChange;
var
  Entry: TNamespaceEntry;
  EntryType: TObjectTypeInfo;
begin
  Entry := RtlxQueryNamespaceEntry(tbxName.Text);
  tbxNameType.Text := TNumeric.Represent(Entry.KnownType).Text;

  if not RtlxFindKernelType(Entry.TypeName, EntryType).IsSuccess then
    EntryType := Default(TObjectTypeInfo);

  ShowAccessMask(
    TuGetMaximumAccessNamedObject(Entry),
    RtlxGetNamespaceAccessMaskType(Entry.KnownType),
    EntryType.Other.GenericMapping
  );
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
  tbxSidLookupType.Text := TNumeric.Represent(Lookup.SidType).Text;

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

  ShowAccessMask(
    TuGetMaximumAccessSidObject(Sid, SidType),
    TuSidAccessMaskType(SidType),
    TuGetGenericMappingSid(SidType)
  );
end;

end.
