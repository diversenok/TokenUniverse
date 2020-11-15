unit UI.Sid.View;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, UI.Prototypes.Forms,
  NtUtils.Security.Sid, UI.Prototypes.Lsa.Privileges, UI.Prototypes.AuditFrame,
  NtUtils.Lsa.Audit, NtUtils, UI.Prototypes.BitMask;

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
    LabelSubAuthrities: TStaticText;
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
    LogonMaskFrame: TBitMaskFrame;
    procedure LinkLabelDomainLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonCloseClick(Sender: TObject);
    procedure LinkLabelMinusOneLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Sid: ISid;
    procedure SetUserAudit(NewAudit: IAudit);
    procedure LoadLogonRights;
  public
    class procedure CreateView(AOwner: TComponent; SrcSid: ISid); static;
  end;

implementation

uses
  Winapi.WinNt, NtUiLib.Exceptions, NtUtils.Lsa.Sid, Ntapi.ntrtl, Winapi.ntlsa,
  Ntapi.ntstatus, DelphiApi.Reflection, NtUtils.Lsa,
  DelphiUiLib.Reflection.Strings, DelphiUiLib.Reflection.Numeric;

{$R *.dfm}

{ TDialogSidView }

procedure TDialogSidView.ButtonApplyClick(Sender: TObject);
begin
  LsaxSetRightsAccountBySid(Sid.Data, LogonMaskFrame.Value).RaiseOnError;
  LogonMaskFrame.Value := LogonMaskFrame.Value;
end;

procedure TDialogSidView.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

class procedure TDialogSidView.CreateView(AOwner: TComponent; SrcSid: ISid);
var
  Lookup: TTranslatedName;
  LogonGroups: TArray<TFlagName>;
begin
  if not Assigned(SrcSid) then
    Exit;

  with TDialogSidView.CreateChild(AOwner, True) do
  begin
    Sid := SrcSid;

    if not LsaxLookupSid(Sid.Data, Lookup).IsSuccess then
    begin
      Lookup.SidType := SidTypeUndefined;
      Lookup.DomainName := '';
      Lookup.UserName := '';
    end;

    Caption := Caption + ' for "' + LsaxSidToString(Sid.Data) +'"';

    if not (Lookup.SidType in [SidTypeUndefined, SidTypeInvalid,
      SidTypeUnknown]) then
      EditFullName.Text := Lookup.FullName;

    EditSID.Text := RtlxSidToString(Sid.Data);
    EditType.Text := PrettifyCamelCaseEnum(TypeInfo(TSidNameUse),
        Integer(Lookup.SidType), 'SidType');
    EditSubAuthorities.Text := IntToStr(RtlSubAuthorityCountSid(
      Sid.Data)^);

    if RtlSubAuthorityCountSid(Sid.Data)^ = 0 then
      LinkLabelMinusOne.Visible := False; // Hide parant SID link

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
    SetLength(LogonGroups, 2);
    LogonGroups[0].Value := SECURITY_ACCESS_ALLOWED_MASK;
    LogonGroups[0].Name := 'Allowing';
    LogonGroups[1].Value := SECURITY_ACCESS_DENIED_MASK;
    LogonGroups[1].Name := 'Denying';
    LogonMaskFrame.Initialize(EnumerateFlagAttributes(TypeInfo(TSystemAccess)),
      LogonGroups);
    LoadLogonRights;

    FrameLsaPrivileges.DeleyedCreate;
    FrameLsaPrivileges.LoadForSid(Sid);

    FrameLsaAudit.OnApplyClick := SetUserAudit;
    FrameLsaAudit.LoadForSid(Sid.Data);

    Show;
  end;
end;

procedure TDialogSidView.LinkLabelDomainLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  DomainSid: ISid;
  Lookup: TTranslatedName;
begin
  if LsaxLookupSid(Sid.Data, Lookup).IsSuccess and (Lookup.DomainName <> '') then
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
  if RtlxParentSid(Parent, Sid).IsSuccess then
    TDialogSidView.CreateView(Owner, Parent);
end;

procedure TDialogSidView.LoadLogonRights;
var
  xStatus: TNtxStatus;
  Rights: TSystemAccess;
begin
  xStatus := LsaxQueryRightsAccountBySid(Sid.Data, Rights);

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

procedure TDialogSidView.SetUserAudit(NewAudit: IAudit);
begin
  FrameLsaAudit.LabelStatus.Caption := '';
  FrameLsaAudit.LabelStatus.Hint := '';

  try
    (NewAudit as IPerUserAudit).AssignToUser(Sid.Data).RaiseOnError;
  finally
    FrameLsaAudit.LoadForSid(Sid.Data);
  end;
end;

end.
