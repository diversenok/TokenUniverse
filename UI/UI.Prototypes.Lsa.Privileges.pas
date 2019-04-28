unit UI.Prototypes.Lsa.Privileges;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, UI.ListViewEx, Vcl.StdCtrls, NtUtils.Types,
  Winapi.WinNt, NtUtils.Lsa, UI.Prototypes.Privileges, Vcl.Menus;

type
  TFrameLsaPrivileges = class(TFrame)
    ButtonApply: TButton;
    LabelStatus: TLabel;
    FramePrivileges: TFramePrivileges;
    PopupMenu: TPopupMenu;
    MenuEnable: TMenuItem;
    MenuDisable: TMenuItem;
    procedure MenuEnableClick(Sender: TObject);
    procedure MenuDisableClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Sid: ISid;
    CurrentlyAssigned: TPrivilegeArray;
    procedure SetSelectedPrivState(Attributes: Cardinal);
  public
    procedure DeleyedCreate;
    procedure LoadForSid(Sid: ISid);
  end;

implementation

uses
  NtUtils.ErrorMsg, Ntapi.ntstatus;

{$R *.dfm}

{ TFrameLsaPolicy }

function FindPrivInArray(PrivArray: TPrivilegeArray; Value: TLuid): PPrivilege;
var
  i: Integer;
begin
  for i := 0 to High(PrivArray) do
    if PrivArray[i].Luid = Value then
      Exit(@PrivArray[i]);

  Result := nil;
end;

procedure TFrameLsaPrivileges.ButtonApplyClick(Sender: TObject);
var
  PrivToAdd, PrivToRemove: TPrivilegeArray;
  i: Integer;
  Current: PPrivilege;
begin
  SetLength(PrivToAdd, 0);
  SetLength(PrivToRemove, 0);

  for i := 0 to FramePrivileges.PrivilegeCount - 1 do
  with FramePrivileges.ListView.Items[i] do
    begin
      Current := FindPrivInArray(CurrentlyAssigned,
        FramePrivileges.Privilege[i].Luid);

      if Checked and (not Assigned(Current) or (Current.Attributes <>
        FramePrivileges.Privilege[i].Attributes)) then
      begin
        // It was enabled or modified, add
        SetLength(PrivToAdd, Length(PrivToAdd) + 1);
        PrivToAdd[High(PrivToAdd)] := FramePrivileges.Privilege[i];
      end;

      if not Checked and Assigned(Current) then
      begin
        // It was disabled, remove
        SetLength(PrivToRemove, Length(PrivToRemove) + 1);
        PrivToRemove[High(PrivToRemove)] := FramePrivileges.Privilege[i];
      end;
    end;

  try
    LsaxManagePrivilegesAccount(Sid.Sid, False, PrivToAdd,
      PrivToRemove).RaiseOnError;
  finally
    LoadForSid(Sid);
  end;

  FramePrivileges.ListView.SetFocus;
end;

procedure TFrameLsaPrivileges.DeleyedCreate;
begin
  FramePrivileges.ColorMode := pcColorChecked;
  FramePrivileges.NamingMode := pnFriendlyName;
  FramePrivileges.AddAllPrivileges;
end;

procedure TFrameLsaPrivileges.LoadForSid(Sid: ISid);
var
  StatusEx: TNtxStatus;
  i, j: Integer;
begin
  Self.Sid := Sid;
  StatusEx := LsaxEnumerateAccountPrivileges(Sid.Sid, CurrentlyAssigned);

  if StatusEx.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsaOpenAccount') then
  begin
    LabelStatus.Caption := 'No policies are assigned to the account';
    LabelStatus.Hint := '';
  end
  else if not StatusEx.IsSuccess then
  begin
    LabelStatus.Caption := StatusEx.Location + ': ' +
      StatusToString(StatusEx.Status);
    LabelStatus.Hint := SysNativeErrorMessage(StatusEx.Status);
  end
  else
  begin
    LabelStatus.Caption := '';
    LabelStatus.Hint := '';
  end;

  // Check assigned privileges
  begin
    FramePrivileges.ListView.Items.BeginUpdate;

    for i := 0 to FramePrivileges.PrivilegeCount - 1 do
      FramePrivileges.ListView.Items[i].Checked := False;

    for j := 0 to High(CurrentlyAssigned) do
      for i := 0 to FramePrivileges.PrivilegeCount - 1 do
        if FramePrivileges.Privilege[i].Luid = CurrentlyAssigned[j].Luid then
        begin
          FramePrivileges.PrivAttributes[i] := CurrentlyAssigned[j].Attributes;
          FramePrivileges.ListView.Items[i].Checked := True;
          Break;
        end;

    FramePrivileges.ListView.Items.EndUpdate;
  end;
end;

procedure TFrameLsaPrivileges.MenuDisableClick(Sender: TObject);
begin
  SetSelectedPrivState(0);
end;

procedure TFrameLsaPrivileges.MenuEnableClick(Sender: TObject);
begin
  SetSelectedPrivState(SE_PRIVILEGE_ENABLED or SE_PRIVILEGE_ENABLED_BY_DEFAULT);
end;

procedure TFrameLsaPrivileges.SetSelectedPrivState(Attributes: Cardinal);
var
  i: Integer;
begin
  if FramePrivileges.ListView.SelCount > 0 then
    for i := 0 to FramePrivileges.ListView.Items.Count - 1 do
      if FramePrivileges.ListView.Items[i].Selected then
        FramePrivileges.PrivAttributes[i] := Attributes
end;


end.
