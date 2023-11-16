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
    PrivilegesFrame: TFramePrivileges;
    procedure MenuEnableClick(Sender: TObject);
    procedure MenuDisableClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    Sid: ISid;
    CurrentlyAssigned: TArray<TPrivilege>;
  public
    procedure DelayedCreate;
    procedure LoadForSid(const Sid: ISid);
  end;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntdef, NtUiLib.Errors, DelphiUtils.Arrays;

{$R *.dfm}

{ TFrameLsaPolicy }

procedure TFrameLsaPrivileges.ButtonApplyClick;
var
  Added, Removed: TArray<TPrivilege>;
begin
  Added := PrivilegesFrame.Checked;

  Removed := TArray.Filter<TPrivilege>(CurrentlyAssigned,
    function (const Privilege: TPrivilege): Boolean
    begin
      Result := not TArray.Contains<TPrivilege>(Added, Privilege,
        function (const A, B: TPrivilege): Boolean
        begin
          Result := (A.Luid = B.Luid);
        end
      );
    end
  );

  try
    LsaxManagePrivilegesAccount(Sid, False, Added,
      Removed).RaiseOnError;
  finally
    LoadForSid(Sid);
    PrivilegesFrame.VST.SetFocus;
  end;
end;

procedure TFrameLsaPrivileges.DelayedCreate;
begin
  PrivilegesFrame.ColoringUnChecked := pcNone;
  PrivilegesFrame.ColoringChecked := pcStateBased;
  PrivilegesFrame.LoadEvery;
end;

procedure TFrameLsaPrivileges.LoadForSid;
var
  Status: TNtxStatus;
begin
  Self.Sid := Sid;
  Status := LsaxEnumeratePrivilegesAccountBySid(Sid, CurrentlyAssigned);

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

procedure TFrameLsaPrivileges.MenuDisableClick;
begin
  PrivilegesFrame.AdjustSelected(0);
end;

procedure TFrameLsaPrivileges.MenuEnableClick;
begin
  PrivilegesFrame.AdjustSelected(SE_PRIVILEGE_ENABLED or
    SE_PRIVILEGE_ENABLED_BY_DEFAULT);
end;

end.
