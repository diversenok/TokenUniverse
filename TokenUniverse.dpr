program TokenUniverse;

uses
  Vcl.Forms,
  NtUtils,
  NtUtils.Svc.SingleTaskSvc,
  NtUiLib.Exceptions,
  TU.Tokens in 'Core\TU.Tokens.pas',
  UI.TokenListFrame in 'UI\UI.TokenListFrame.pas' {FrameTokenList: TFrame},
  UI.MainForm in 'UI\UI.MainForm.pas' {FormMain},
  UI.Modal.AccessAndType in 'UI\UI.Modal.AccessAndType.pas' {DialogAccessAndType},
  UI.HandleSearch in 'UI\UI.HandleSearch.pas' {FormHandleSearch},
  UI.Information in 'UI\UI.Information.pas' {InfoDialog},
  UI.ProcessList in 'UI\UI.ProcessList.pas' {ProcessListDialog},
  TU.RestartSvc in 'Core\TU.RestartSvc.pas',
  TU.Suggestions in 'Core\TU.Suggestions.pas',
  UI.Restrict in 'UI\UI.Restrict.pas' {DialogRestrictToken},
  UI.Prototypes in 'UI\UI.Prototypes.pas',
  TU.Credentials in 'Core\TU.Credentials.pas',
  UI.Modal.Logon in 'UI\UI.Modal.Logon.pas' {LogonDialog},
  UI.Modal.PickUser in 'UI\UI.Modal.PickUser.pas' {DialogPickUser},
  TU.ObjPicker in 'Core\TU.ObjPicker.pas',
  UI.CreateToken in 'UI\UI.CreateToken.pas' {DialogCreateToken},
  TU.Tokens.Types in 'Core\TU.Tokens.Types.pas',
  UI.Modal.Columns in 'UI\UI.Modal.Columns.pas' {DialogColumns},
  UI.Settings in 'UI\UI.Settings.pas',
  UI.Modal.Access in 'UI\UI.Modal.Access.pas' {DialogAccess},
  UI.Modal.ComboDlg in 'UI\UI.Modal.ComboDlg.pas' {ComboDialog},
  TU.Winapi in 'Core\TU.Winapi.pas',
  UI.Modal.ThreadList in 'UI\UI.Modal.ThreadList.pas' {ThreadListDialog},
  UI.Information.Access in 'UI\UI.Information.Access.pas' {DialogGrantedAccess},
  UI.Modal.PickToken in 'UI\UI.Modal.PickToken.pas' {DialogPickToken},
  UI.New.Safer in 'UI\UI.New.Safer.pas' {DialogSafer},
  UI.Prototypes.AuditFrame in 'UI\UI.Prototypes.AuditFrame.pas' {FrameAudit: TFrame},
  UI.Prototypes.Logon in 'UI\UI.Prototypes.Logon.pas' {FrameLogon: TFrame},
  UI.Sid.View in 'UI\UI.Sid.View.pas' {DialogSidView},
  UI.Prototypes.Lsa.Privileges in 'UI\UI.Prototypes.Lsa.Privileges.pas' {FrameLsaPrivileges: TFrame},
  UI.Audit.System in 'UI\UI.Audit.System.pas' {DialogSystemAudit},
  UI.Process.Run in 'UI\UI.Process.Run.pas' {DialogRun},
  VclEx.ListView in 'NtUtilsUI\VclEx\VclEx.ListView.pas',
  UI.Prototypes.Forms in 'NtUtilsUI\Common\UI.Prototypes.Forms.pas',
  VclEx.Form in 'NtUtilsUI\VclEx\VclEx.Form.pas',
  UI.Prototypes.Privileges in 'NtUtilsUI\Prototypes\UI.Prototypes.Privileges.pas' {PrivilegesFrame: TFrame},
  UI.Colors in 'NtUtilsUI\Common\UI.Colors.pas',
  UI.ProcessIcons in 'NtUtilsUI\Common\UI.ProcessIcons.pas',
  UI.Prototypes.AccessMask in 'NtUtilsUI\Prototypes\UI.Prototypes.AccessMask.pas' {AccessMaskFrame: TFrame},
  UI.AppContainer.View in 'UI\UI.AppContainer.View.pas' {DialogAppContainer},
  TU.Exec in 'Core\TU.Exec.pas',
  UI.AppContainer.List in 'UI\UI.AppContainer.List.pas' {DialogACProfiles},
  UI.Prototypes.BitMask in 'NtUtilsUI\Prototypes\UI.Prototypes.BitMask.pas' {BitMaskFrame: TFrame},
  UI.Prototypes.Groups in 'NtUtilsUI\Prototypes\UI.Prototypes.Groups.pas' {FrameGroups: TFrame},
  UI.Helper in 'NtUtilsUI\Common\UI.Helper.pas';

{$R *.res}

begin
  // Running as a service
  if ParamStr(1) = RESVC_PARAM then
  begin
    SvcxMain(RESVC_NAME, ReSvcRunInSession);
    Exit;
  end;

  // The user delegated us to create a service
  if ParamStr(1) = DELEGATE_PARAM then
  begin
    ReSvcCreateService(ParamStr(2) = RESVC_SYSPLUS_PARAM).RaiseOnError;
    Exit;
  end;

  // Normal mode
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Token Universe';
  Application.HintHidePause := 20000;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
