unit UI.Prototypes.Lsa.Privileges;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, VclEx.ListView, Vcl.StdCtrls, NtUtils.Security.Sid,
  Ntapi.WinNt, NtUtils.Lsa, UI.Prototypes.Privileges, Vcl.Menus, Ntapi.ntseapi,
  NtUtils;

type
  TFrameLsaPrivileges = class(TFrame)
    ButtonApply: TButton;
    LabelStatus: TLabel;
    PopupMenu: TPopupMenu;
    MenuEnable: TMenuItem;
    MenuDisable: TMenuItem;
    PrivilegesFrame: TPrivilegesFrame;
    procedure MenuEnableClick(Sender: TObject);
    procedure MenuDisableClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Sid: ISid;
    CurrentlyAssigned: TArray<TPrivilege>;
    procedure SetSelectedPrivState(Attributes: Cardinal);
  public
    procedure DeleyedCreate;
    procedure LoadForSid(Sid: ISid);
  end;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntdef, NtUiLib.Errors;

{$R *.dfm}

{ TFrameLsaPolicy }

function FindPrivInArray(PrivArray: TArray<TPrivilege>; Value: TLuid):
  PPrivilege;
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
  PrivToAdd, PrivToRemove: TArray<TPrivilege>;
  i: Integer;
  Current: PPrivilege;
  New: TPrivilege;
begin
  SetLength(PrivToAdd, 0);
  SetLength(PrivToRemove, 0);

  for i := 0 to Pred(PrivilegesFrame.ListViewEx.Items.Count) do
  with PrivilegesFrame.ListViewEx.Items[i] do
    begin
      Current := FindPrivInArray(CurrentlyAssigned,
        PrivilegesFrame.Privilege[i].Luid);

      New := PrivilegesFrame.Privilege[i];

      if Checked and (not Assigned(Current) or
        (Current.Attributes <> New.Attributes)) then
      begin
        // It was enabled or modified, add
        SetLength(PrivToAdd, Length(PrivToAdd) + 1);
        PrivToAdd[High(PrivToAdd)] := New;
      end;

      if not Checked and Assigned(Current) then
      begin
        // It was disabled, remove
        SetLength(PrivToRemove, Length(PrivToRemove) + 1);
        PrivToRemove[High(PrivToRemove)] := New;
      end;
    end;

  try
    LsaxManagePrivilegesAccount(Sid.Data, False, PrivToAdd,
      PrivToRemove).RaiseOnError;
  finally
    LoadForSid(Sid);
    PrivilegesFrame.ListViewEx.SetFocus;
  end;
end;

procedure TFrameLsaPrivileges.DeleyedCreate;
begin
  PrivilegesFrame.ColoringUnChecked := pcNone;
  PrivilegesFrame.ColoringChecked := pcStateBased;
  PrivilegesFrame.LoadEvery;
end;

procedure TFrameLsaPrivileges.LoadForSid(Sid: ISid);
var
  Status: TNtxStatus;
begin
  Self.Sid := Sid;
  Status := LsaxEnumeratePrivilegesAccountBySid(Sid.Data, CurrentlyAssigned);

  if Status.Matches(STATUS_OBJECT_NAME_NOT_FOUND, 'LsaOpenAccount') then
  begin
    LabelStatus.Caption := 'No policies are assigned to the account';
    LabelStatus.Hint := '';
  end
  else if not Status.IsSuccess then
  begin
    LabelStatus.Caption := Status.ToString;
    LabelStatus.Hint := Status.Description;
  end
  else
  begin
    LabelStatus.Caption := '';
    LabelStatus.Hint := '';
  end;

  // Check assigned privileges
  PrivilegesFrame.Checked := CurrentlyAssigned;
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
  if PrivilegesFrame.ListViewEx.SelCount > 0 then
    for i := 0 to Pred(PrivilegesFrame.ListViewEx.Items.Count) do
      if PrivilegesFrame.ListViewEx.Items[i].Selected then
        PrivilegesFrame.UpdateState(i, Attributes);
end;


end.
