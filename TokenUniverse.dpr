program TokenUniverse;

uses
  Vcl.Forms,
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
  UI.ListViewEx in 'UI\UI.ListViewEx.pas',
  UI.Colors in 'UI\UI.Colors.pas',
  UI.Prototypes in 'UI\UI.Prototypes.pas',
  TU.Credentials in 'Core\TU.Credentials.pas',
  UI.Modal.Logon in 'UI\UI.Modal.Logon.pas' {LogonDialog},
  UI.Prototypes.ChildForm in 'UI\UI.Prototypes.ChildForm.pas',
  UI.Modal.PickUser in 'UI\UI.Modal.PickUser.pas' {DialogPickUser},
  TU.ObjPicker in 'Core\TU.ObjPicker.pas',
  UI.CreateToken in 'UI\UI.CreateToken.pas' {DialogCreateToken},
  TU.Tokens.Types in 'Core\TU.Tokens.Types.pas',
  UI.Modal.Columns in 'UI\UI.Modal.Columns.pas' {DialogColumns},
  UI.Settings in 'UI\UI.Settings.pas',
  UI.Modal.Access in 'UI\UI.Modal.Access.pas' {DialogAccess},
  UI.Modal.ComboDlg in 'UI\UI.Modal.ComboDlg.pas' {ComboDialog},
  Ntapi.ntdef in 'Headers\Ntapi.ntdef.pas',
  Ntapi.ntstatus in 'Headers\Ntapi.ntstatus.pas',
  Ntapi.ntexapi in 'Headers\Ntapi.ntexapi.pas',
  Ntapi.ntobapi in 'Headers\Ntapi.ntobapi.pas',
  Ntapi.ntpsapi in 'Headers\Ntapi.ntpsapi.pas',
  Ntapi.ntseapi in 'Headers\Ntapi.ntseapi.pas',
  Ntapi.ntrtl in 'Headers\Ntapi.ntrtl.pas',
  Ntapi.ntpebteb in 'Headers\Ntapi.ntpebteb.pas',
  Ntapi.ntkeapi in 'Headers\Ntapi.ntkeapi.pas',
  TU.Winapi in 'Core\TU.Winapi.pas',
  UI.Modal.ThreadList in 'UI\UI.Modal.ThreadList.pas' {ThreadListDialog},
  NtUtils.Exceptions in 'NtUtils\NtUtils.Exceptions.pas',
  Winapi.WinError in 'Headers\Winapi.WinError.pas',
  Winapi.WinNt in 'Headers\Winapi.WinNt.pas',
  Winapi.WinBase in 'Headers\Winapi.WinBase.pas',
  Winapi.Sddl in 'Headers\Winapi.Sddl.pas',
  UI.Information.Access in 'UI\UI.Information.Access.pas' {DialogGrantedAccess},
  UI.Modal.PickToken in 'UI\UI.Modal.PickToken.pas' {DialogPickToken},
  Winapi.WinSafer in 'Headers\Winapi.WinSafer.pas',
  UI.New.Safer in 'UI\UI.New.Safer.pas' {DialogSafer},
  Winapi.NtSecApi in 'Headers\Winapi.NtSecApi.pas',
  UI.Prototypes.AuditFrame in 'UI\UI.Prototypes.AuditFrame.pas' {FrameAudit: TFrame},
  Winapi.ntlsa in 'Headers\Winapi.ntlsa.pas',
  UI.Prototypes.Logon in 'UI\UI.Prototypes.Logon.pas' {FrameLogon: TFrame},
  NtUtils.Lsa in 'NtUtils\NtUtils.Lsa.pas',
  NtUtils.ErrorMsg in 'NtUtils\NtUtils.ErrorMsg.pas',
  Winapi.winsta in 'Headers\Winapi.winsta.pas',
  NtUtils.WinStation in 'NtUtils\NtUtils.WinStation.pas',
  DelphiUtils.Events in 'NtUtils\DelphiUtils.Events.pas',
  DelphiUtils.Strings in 'NtUtils\DelphiUtils.Strings.pas',
  NtUtils.Ldr in 'NtUtils\NtUtils.Ldr.pas',
  Ntapi.ntldr in 'Headers\Ntapi.ntldr.pas',
  Winapi.securitybaseapi in 'Headers\Winapi.securitybaseapi.pas',
  NtUtils.Strings in 'NtUtils\NtUtils.Strings.pas',
  NtUtils.Security.Sid in 'NtUtils\NtUtils.Security.Sid.pas',
  UI.Prototypes.Privileges in 'UI\UI.Prototypes.Privileges.pas' {FramePrivileges: TFrame},
  UI.Prototypes.Groups in 'UI\UI.Prototypes.Groups.pas' {FrameGroups: TFrame},
  Ntapi.ntsam in 'Headers\Ntapi.ntsam.pas',
  UI.Sid.View in 'UI\UI.Sid.View.pas' {DialogSidView},
  UI.Prototypes.Lsa.Rights in 'UI\UI.Prototypes.Lsa.Rights.pas' {FrameLsaRights: TFrame},
  UI.Prototypes.Lsa.Privileges in 'UI\UI.Prototypes.Lsa.Privileges.pas' {FrameLsaPrivileges: TFrame},
  NtUtils.Lsa.Audit in 'NtUtils\NtUtils.Lsa.Audit.pas',
  UI.Audit.System in 'UI\UI.Audit.System.pas' {DialogSystemAudit},
  Winapi.WinUser in 'Headers\Winapi.WinUser.pas',
  NtUtils.Exec in 'NtUtils\NtUtils.Exec.pas',
  UI.Process.Run in 'UI\UI.Process.Run.pas' {DialogRun},
  Winapi.Shlwapi in 'Headers\Winapi.Shlwapi.pas',
  NtUtils.Exec.Win32 in 'NtUtils\NtUtils.Exec.Win32.pas',
  Winapi.ProcessThreadsApi in 'Headers\Winapi.ProcessThreadsApi.pas',
  NtUtils.Exec.Shell in 'NtUtils\NtUtils.Exec.Shell.pas',
  Winapi.Shell in 'Headers\Winapi.Shell.pas',
  NtUtils.Exec.Wdc in 'NtUtils\NtUtils.Exec.Wdc.pas',
  Winapi.Wdc in 'Headers\Winapi.Wdc.pas',
  NtUtils.Exec.Wmi in 'NtUtils\NtUtils.Exec.Wmi.pas',
  Ntapi.ntmmapi in 'Headers\Ntapi.ntmmapi.pas',
  NtUtils.Exec.Nt in 'NtUtils\NtUtils.Exec.Nt.pas',
  NtUtils.WinUser in 'NtUtils\NtUtils.WinUser.pas',
  NtUtils.Access in 'NtUtils\NtUtils.Access.pas',
  NtUtils.Processes in 'NtUtils\NtUtils.Processes.pas',
  NtUtils.Objects in 'NtUtils\NtUtils.Objects.pas',
  NtUtils.Tokens in 'NtUtils\NtUtils.Tokens.pas',
  NtUtils.Tokens.Impersonate in 'NtUtils\NtUtils.Tokens.Impersonate.pas',
  NtUtils.Security.Acl in 'NtUtils\NtUtils.Security.Acl.pas',
  Winapi.Svc in 'Headers\Winapi.Svc.pas',
  NtUtils.Tokens.Logon in 'NtUtils\NtUtils.Tokens.Logon.pas',
  NtUtils.Tokens.Misc in 'NtUtils\NtUtils.Tokens.Misc.pas',
  NtUtils.Svc in 'NtUtils\NtUtils.Svc.pas',
  NtUtils.Svc.SingleTaskSvc in 'NtUtils\NtUtils.Svc.SingleTaskSvc.pas',
  NtUtils.Svc.Security in 'NtUtils\NtUtils.Svc.Security.pas',
  NtUtils.Lsa.Security in 'NtUtils\NtUtils.Lsa.Security.pas',
  NtUtils.Objects.Security in 'NtUtils\NtUtils.Objects.Security.pas',
  NtUtils.Sam.Security in 'NtUtils\NtUtils.Sam.Security.pas',
  NtUtils.Sam in 'NtUtils\NtUtils.Sam.pas',
  NtUtils.Job in 'NtUtils\NtUtils.Job.pas',
  NtUtils.Lsa.Logon in 'NtUtils\NtUtils.Lsa.Logon.pas',
  NtUtils.Environment in 'NtUtils\NtUtils.Environment.pas',
  Winapi.UserEnv in 'Headers\Winapi.UserEnv.pas',
  NtUtils.Objects.Snapshots in 'NtUtils\NtUtils.Objects.Snapshots.pas',
  NtUtils.Processes.Snapshots in 'NtUtils\NtUtils.Processes.Snapshots.pas',
  Ntapi.ntregapi in 'Headers\Ntapi.ntregapi.pas',
  NtUtils.Registry in 'NtUtils\NtUtils.Registry.pas',
  NtUtils.Registry.HKCU in 'NtUtils\NtUtils.Registry.HKCU.pas',
  NtUtils.WinSafer in 'NtUtils\NtUtils.WinSafer.pas',
  NtUtils.Threads in 'NtUtils\NtUtils.Threads.pas',
  NtUtils.Access.Expected in 'NtUtils\NtUtils.Access.Expected.pas',
  NtUtils.Objects.Compare in 'NtUtils\NtUtils.Objects.Compare.pas',
  NtUtils.Objects.Namespace in 'NtUtils\NtUtils.Objects.Namespace.pas',
  NtUtils.Processes.Memory in 'NtUtils\NtUtils.Processes.Memory.pas',
  NtUtils.Shellcode in 'NtUtils\NtUtils.Shellcode.pas',
  NtUtils.Sections in 'NtUtils\NtUtils.Sections.pas',
  Ntapi.ntioapi in 'Headers\Ntapi.ntioapi.pas';

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
