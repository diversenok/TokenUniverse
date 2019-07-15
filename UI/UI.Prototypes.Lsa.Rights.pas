unit UI.Prototypes.Lsa.Rights;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, UI.ListViewEx, Vcl.StdCtrls,
  NtUtils.Security.Sid, NtUtils.Lsa;

type
  TFrameLsaRights = class(TFrame)
    ButtonApply: TButton;
    LabelStatus: TLabel;
    ListView: TListViewEx;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ListViewItemChecked(Sender: TObject; Item: TListItem);
  private
    Sid: ISid;
    AllRights: TArray<TLogonRightRec>;
    CurrentRights: Cardinal;
    Updating: Boolean;
    function CheckedRights: Cardinal;
  public
    procedure DeleyedCreate;
    procedure LoadForSid(Sid: ISid);
  end;

implementation

uses
  Ntapi.ntstatus, DelphiUtils.Strings, UI.Colors, NtUtils.Exceptions;

{$R *.dfm}

const
  // Be consistent with ListView's groups
  GROUP_ID_ALLOW = 0;
  GROUP_ID_DENY = 1;

function FormatHint(const Right: TLogonRightRec): String;
var
  Sections: array of THintSection;
begin
  SetLength(Sections, 2);

  Sections[0].Title := 'Name';
  Sections[0].Enabled := True;
  Sections[0].Content := Right.Name;

  Sections[1].Title := 'Value';
  Sections[1].Enabled := True;
  Sections[1].Content := IntToHexEx(Right.Value);

  Result := BuildHint(Sections);
end;

{ TFrameLsaRights }

procedure TFrameLsaRights.ButtonApplyClick(Sender: TObject);
begin
  LsaxSetRightsAccount(Sid.Sid, CheckedRights).RaiseOnError;
  LoadForSid(Sid);
end;

function TFrameLsaRights.CheckedRights: Cardinal;
var
  i: Integer;
begin
  Assert(Length(AllRights) = ListView.Items.Count);

  Result := CurrentRights;
  for i := 0 to High(AllRights) do
    if ListView.Items[i].Checked then
      Result := Result or AllRights[i].Value
    else
      Result := Result and not AllRights[i].Value;
end;

procedure TFrameLsaRights.DeleyedCreate;
var
  i: Integer;
begin
  Updating := True;
  ListView.Items.BeginUpdate;

  // Add all AllRights
  AllRights := LsaxEnumerateLogonRights;
  for i := 0 to High(AllRights) do
    with ListView.Items.Add do
    begin
      if AllRights[i].AllowedType then
        GroupID := GROUP_ID_ALLOW
      else
        GroupID := GROUP_ID_DENY;

      Caption := AllRights[i].Description;
      Hint := FormatHint(AllRights[i]);
    end;

  ListView.Items.EndUpdate;
  Updating := False;
end;

procedure TFrameLsaRights.ListViewItemChecked(Sender: TObject; Item: TListItem);
begin
  if Updating then
    Exit;

  if Assigned(Item) and (Item is TListItemEx) then
  begin
    if Item.Checked xor Contains(CurrentRights,
      AllRights[Item.Index].Value) then
      TListItemEx(Item).Color := clStale
    else
      TListItemEx(Item).ColorEnabled := False;
  end;
end;

procedure TFrameLsaRights.LoadForSid(Sid: ISid);
var
  StatusEx: TNtxStatus;
  i: Integer;
begin
  Self.Sid := Sid;

  StatusEx := LsaxQueryRightsAccount(Sid.Sid, CurrentRights);

  if StatusEx.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsaOpenAccount') then
  begin
    LabelStatus.Caption := 'No policies are assigned to the account';
    LabelStatus.Hint := '';
  end
  else if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.ToString;
    LabelStatus.Hint := StatusEx.MessageHint;
  end
  else
  begin
    LabelStatus.Caption := '';
    LabelStatus.Hint := '';
  end;

  // Check assigned logon rights
  begin
    Updating := True;
    ListView.Items.BeginUpdate;

    for i := 0 to High(AllRights) do
    begin
      ListView.Items[i].Checked := Contains(CurrentRights, AllRights[i].Value);
      ListView.Items[i].ColorEnabled := False;
    end;

    ListView.Items.EndUpdate;
    Updating := False;
  end;
end;

end.
