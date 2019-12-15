unit UI.Sid.View;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  UI.Prototypes.ChildForm, NtUtils.Security.Sid, UI.Prototypes.Lsa.Rights,
  UI.Prototypes.Lsa.Privileges, UI.Prototypes.AuditFrame, NtUtils.Lsa.Audit;

type
  TDialogSidView = class(TChildTaskbarForm)
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
    FrameLsaRights: TFrameLsaRights;
    TabLsaQuotas: TTabSheet;
    FrameLsaPrivileges: TFrameLsaPrivileges;
    FrameLsaAudit: TFrameAudit;
    procedure LinkLabelDomainLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonCloseClick(Sender: TObject);
    procedure LinkLabelMinusOneLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    Sid: ISid;
    procedure SetUserAudit(NewAudit: IAudit);
  public
    class procedure CreateView(SrcSid: ISid); static;
  end;

implementation

uses
  DelphiUtils.Strings, Winapi.WinNt, NtUtils.Exceptions, NtUtils.Lsa.Sid;

{$R *.dfm}

{ TDialogSidView }

procedure TDialogSidView.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

class procedure TDialogSidView.CreateView(SrcSid: ISid);
var
  Lookup: TTranslatedName;
begin
  if not Assigned(SrcSid) then
    Exit;

  with TDialogSidView.Create(nil) do
  begin
    Sid := SrcSid;

    if not LsaxLookupSid(Sid.Sid, Lookup).IsSuccess then
    begin
      Lookup.SidType := SidTypeUndefined;
      Lookup.DomainName := '';
      Lookup.UserName := '';
    end;

    Caption := Caption + ' for "' + LsaxSidToString(Sid.Sid) +'"';

    if not (Lookup.SidType in [SidTypeUndefined, SidTypeInvalid,
      SidTypeUnknown]) then
      EditFullName.Text := Lookup.FullName;

    EditSID.Text := Sid.SDDL;
    EditType.Text := PrettifyCamelCaseEnum(TypeInfo(TSidNameUse),
        Integer(Lookup.SidType), 'SidType');
    EditSubAuthorities.Text := IntToStr(Sid.SubAuthorities);

    if Sid.SubAuthorities = 0 then
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

    FrameLsaRights.DeleyedCreate;
    FrameLsaRights.LoadForSid(Sid);

    FrameLsaPrivileges.DeleyedCreate;
    FrameLsaPrivileges.LoadForSid(Sid);

    FrameLsaAudit.OnApplyClick := SetUserAudit;
    FrameLsaAudit.LoadForSid(Sid.Sid);

    Show;
  end;
end;

procedure TDialogSidView.LinkLabelDomainLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  DomainSid: ISid;
  Lookup: TTranslatedName;
begin
  if LsaxLookupSid(Sid.Sid, Lookup).IsSuccess and (Lookup.DomainName <> '') then
  begin
    LsaxLookupName(Lookup.DomainName, DomainSid).RaiseOnError;
    TDialogSidView.CreateView(DomainSid);
  end;
end;

procedure TDialogSidView.LinkLabelMinusOneLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  if Sid.SubAuthorities > 0 then
    TDialogSidView.CreateView(Sid.Parent);
end;

procedure TDialogSidView.SetUserAudit(NewAudit: IAudit);
begin
  FrameLsaAudit.LabelStatus.Caption := '';
  FrameLsaAudit.LabelStatus.Hint := '';

  try
    (NewAudit as IPerUserAudit).AssignToUser(Sid.Sid).RaiseOnError;
  finally
    FrameLsaAudit.LoadForSid(Sid.Sid);
  end;
end;

end.
