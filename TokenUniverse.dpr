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
  UI.Run in 'UI\UI.Run.pas' {RunDialog},
  TU.RestartSvc in 'Core\TU.RestartSvc.pas',
  TU.Suggestions in 'Core\TU.Suggestions.pas',
  UI.Restrict in 'UI\UI.Restrict.pas' {DialogRestrictToken},
  UI.ListViewEx in 'UI\UI.ListViewEx.pas',
  UI.Colors in 'UI\UI.Colors.pas',
  UI.Prototypes in 'UI\UI.Prototypes.pas',
  TU.Credentials in 'Core\TU.Credentials.pas',
  UI.Modal.Logon in 'UI\UI.Modal.Logon.pas' {LogonDialog},
  TU.LsaApi in 'Core\TU.LsaApi.pas',
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
  NtUtils.Handles in 'NtUtils\NtUtils.Handles.pas',
  NtUtils.Processes in 'NtUtils\NtUtils.Processes.pas',
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
  NtUtils.ApiExtension in 'NtUtils\NtUtils.ApiExtension.pas',
  Winapi.winsta in 'Headers\Winapi.winsta.pas',
  NtUtils.WinStation in 'NtUtils\NtUtils.WinStation.pas',
  NtUtils.Audit in 'NtUtils\NtUtils.Audit.pas',
  DelphiUtils.Events in 'DelphiUtils\DelphiUtils.Events.pas',
  DelphiUtils.Strings in 'DelphiUtils\DelphiUtils.Strings.pas',
  NtUtils.DelayedImport in 'NtUtils\NtUtils.DelayedImport.pas',
  Ntapi.ntldr in 'Headers\Ntapi.ntldr.pas',
  Winapi.securitybaseapi in 'Headers\Winapi.securitybaseapi.pas';

{$R *.res}

begin
  // Running as a service
  if ParamStr(1) = RESVC_PARAM then
  begin
    ReSvcMain;
    Exit;
  end;

  // The user delegated us to create a service
  if ParamStr(1) = DELEGATE_PARAM then
  begin
    ReSvcCreateService(ParamStr(2) = RESVC_SYSPLUS_PARAM);
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
