unit UI.Sid.View;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, UI.Prototypes.Forms,
  NtUtils.Security.Sid, UI.Prototypes.Lsa.Privileges, UI.Prototypes.AuditFrame,
  NtUtils.Lsa.Audit, NtUtils, NtUiFrame.Bits;

type
  TDialogSidView = class(TChildForm)
    Pages: TPageControl;
    TabSid: TTabSheet;
    TabDomain: TTabSheet;
    TabGroup: TTabSheet;
    TabAlias: TTabSheet;
    TabUser: TTabSheet;
    TabLsaPrivileges: TTabSheet;
    LinkLabelDomain: TLinkLabel;
    LabelSid: TStaticText;
    LabelType: TStaticText;
    LabelFullName: TStaticText;
    EditFullName: TEdit;
    EditSID: TEdit;
    EditType: TEdit;
    ButtonClose: TButton;
    LabelSubAuthorities: TStaticText;
    EditSubAuthorities: TEdit;
    LinkLabelMinusOne: TLinkLabel;
    StaticTextDomain: TStaticText;
    TabLsaRights: TTabSheet;
    TabLsaAudit: TTabSheet;
    TabLsaQuotas: TTabSheet;
    FrameLsaPrivileges: TFrameLsaPrivileges;
    FrameLsaAudit: TFrameAudit;
    LabelStatus: TLabel;
    ButtonApply: TButton;
    LogonMaskFrame: TBitsFrame;
    procedure LinkLabelDomainLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonCloseClick(Sender: TObject);
    procedure LinkLabelMinusOneLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Sid: ISid;
    procedure SetUserAudit(const Audit: TArray<TAuditPolicyEntry>);
    procedure LoadLogonRights;
  public
    class procedure CreateView(AOwner: TComponent; SrcSid: ISid); static;
  end;

implementation

uses
  Ntapi.WinNt, NtUiLib.Errors, NtUtils.Lsa.Sid, Ntapi.ntrtl, Ntapi.ntlsa,
  Ntapi.ntstatus, DelphiApi.Reflection, NtUtils.Lsa, Ntapi.ntdef,
  DelphiUiLib.Reflection, DelphiUiLib.Reflection.Strings, System.Rtti;

{$R *.dfm}

function EnumerateFlagAttributes(
  AType: Pointer
): TArray<TFlagName>;
var
  i: Integer;
  Flags: TArray<FlagNameAttribute>;
begin
  RttixFilterAttributes(TRttiContext.Create.GetType(AType).GetAttributes,
    FlagNameAttribute, TCustomAttributeArray(Flags));

  SetLength(Result, Length(Flags));

  for i := 0 to High(Result) do
    Result[i] := Flags[i].Flag;
end;

{ TDialogSidView }

procedure TDialogSidView.ButtonApplyClick(Sender: TObject);
begin
  LsaxSetRightsAccountBySid(Sid, LogonMaskFrame.Value).RaiseOnError;
  LogonMaskFrame.Value := LogonMaskFrame.Value;
end;

procedure TDialogSidView.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

class procedure TDialogSidView.CreateView(AOwner: TComponent; SrcSid: ISid);
var
  Lookup: TTranslatedName;
begin
  if not Assigned(SrcSid) then
    Exit;

  with TDialogSidView.CreateChild(AOwner, cfmDesktop) do
  begin
    Sid := SrcSid;

    if not LsaxLookupSid(Sid, Lookup).IsSuccess then
    begin
      Lookup.SidType := SidTypeUndefined;
      Lookup.DomainName := '';
      Lookup.UserName := '';
    end;

    Caption := Caption + ' for "' + LsaxSidToString(Sid) +'"';

    if not (Lookup.SidType in [SidTypeUndefined, SidTypeInvalid,
      SidTypeUnknown]) then
      EditFullName.Text := Lookup.FullName;

    EditSID.Text := RtlxSidToString(Sid);
    EditType.Text := PrettifyCamelCaseEnum(TypeInfo(TSidNameUse),
        Integer(Lookup.SidType), 'SidType');
    EditSubAuthorities.Text := IntToStr(RtlSubAuthorityCountSid(
      Sid.Data)^);

    if RtlSubAuthorityCountSid(Sid.Data)^ = 0 then
      LinkLabelMinusOne.Visible := False; // Hide parent SID link

    if Lookup.DomainName <> '' then
    begin
      LinkLabelDomain.Caption := Lookup.DomainName;

      // When viewing anything but domains make it a link
      if Lookup.SidType <> SidTypeDomain then
        LinkLabelDomain.Caption := '<a>' + LinkLabelDomain.Caption + '</a>';
    end;

    // Manage tab visibility
    TabDomain.TabVisible := (Lookup.SidType = SidTypeDomain);
    TabGroup.TabVisible := (Lookup.SidType = SidTypeGroup);
    TabAlias.TabVisible := (Lookup.SidType = SidTypeAlias);
    TabUser.TabVisible := (Lookup.SidType = SidTypeUser);
    Pages.ActivePage := TabSid;

    // Initialize logon bitmask frame
    LogonMaskFrame.LoadType(TypeInfo(TSystemAccess));
    LoadLogonRights;

    FrameLsaPrivileges.DelayedCreate;
    FrameLsaPrivileges.LoadForSid(Sid);

    FrameLsaAudit.OnApplyClick := SetUserAudit;
    FrameLsaAudit.LoadForSid(Sid);

    Show;
  end;
end;

procedure TDialogSidView.LinkLabelDomainLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  DomainSid: ISid;
  Lookup: TTranslatedName;
begin
  if LsaxLookupSid(Sid, Lookup).IsSuccess and (Lookup.DomainName <> '') then
  begin
    LsaxLookupName(Lookup.DomainName, DomainSid).RaiseOnError;
    TDialogSidView.CreateView(Owner, DomainSid);
  end;
end;

procedure TDialogSidView.LinkLabelMinusOneLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  Parent: ISid;
begin
  if RtlxMakeParentSid(Parent, Sid).IsSuccess then
    TDialogSidView.CreateView(Owner, Parent);
end;

procedure TDialogSidView.LoadLogonRights;
var
  xStatus: TNtxStatus;
  Rights: TSystemAccess;
begin
  xStatus := LsaxQueryRightsAccountBySid(Sid, Rights);

  if xStatus.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsaOpenAccount') then
  begin
    LabelStatus.Caption := 'No policies are assigned to the account';
    LabelStatus.Hint := '';
  end
  else if not xStatus.IsSuccess then
  begin
    LabelStatus.Caption := xStatus.ToString;
    LabelStatus.Hint := xStatus.Description;
    Exit;
  end;

  LabelStatus.Caption := '';
  LabelStatus.Hint := '';
  LogonMaskFrame.Value := Rights;
end;

procedure TDialogSidView.SetUserAudit;
begin
  FrameLsaAudit.LabelStatus.Caption := '';
  FrameLsaAudit.LabelStatus.Hint := '';

  try
    LsaxSetUserAudit(Sid, Audit).RaiseOnError;
  finally
    FrameLsaAudit.LoadForSid(Sid);
  end;
end;

end.
