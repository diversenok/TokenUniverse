program TokenUniverse;

uses
  Vcl.Forms,
  Ntapi.ntpebteb,
  NtUtils,
  NtUtils.Svc.SingleTaskSvc,
  NtUiLib.Errors,
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
  UI.Modal.Logon in 'UI\UI.Modal.Logon.pas' {LogonDialog},
  UI.Modal.PickUser in 'UI\UI.Modal.PickUser.pas' {DialogPickUser},
  UI.CreateToken in 'UI\UI.CreateToken.pas' {DialogCreateToken},
  TU.Tokens.Old.Types in 'Core\TU.Tokens.Old.Types.pas',
  UI.Modal.Columns in 'UI\UI.Modal.Columns.pas' {DialogColumns},
  UI.Settings in 'UI\UI.Settings.pas',
  UI.Modal.Access in 'UI\UI.Modal.Access.pas' {DialogAccess},
  UI.Modal.ComboDlg in 'UI\UI.Modal.ComboDlg.pas' {ComboDialog},
  TU.Winapi in 'Core\TU.Winapi.pas',
  UI.Modal.ThreadList in 'UI\UI.Modal.ThreadList.pas' {ThreadListDialog},
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
  UI.Colors in 'NtUtilsUI\Common\UI.Colors.pas',
  UI.ProcessIcons in 'NtUtilsUI\Common\UI.ProcessIcons.pas',
  TU.Processes.Create in 'Core\TU.Processes.Create.pas',
  UI.Prototypes.Groups in 'NtUtilsUI\Prototypes\UI.Prototypes.Groups.pas' {FrameGroups: TFrame},
  UI.Helper in 'NtUtilsUI\Common\UI.Helper.pas',
  VirtualTreesEx in 'NtUtilsUI\Components\VirtualTreesEx.pas',
  UI.Prototypes.Privileges in 'NtUtilsUI\Prototypes\UI.Prototypes.Privileges.pas' {FramePrivileges: TFrame},
  UI.Builtin.DsObjectPicker in 'NtUtilsUI\Prototypes\UI.Builtin.DsObjectPicker.pas',
  UI.Prototypes.Sid.Cheatsheet in 'NtUtilsUI\Prototypes\UI.Prototypes.Sid.Cheatsheet.pas' {SidCheatsheet},
  UI.Prototypes.Sid.Edit in 'NtUtilsUI\Prototypes\UI.Prototypes.Sid.Edit.pas' {SidEditor: TFrame},
  UI.Prototypes.Acl in 'NtUtilsUI\Prototypes\UI.Prototypes.Acl.pas' {FrameAcl: TFrame},
  TU.Observers in 'Core\TU.Observers.pas',
  TU.Tokens.Events in 'Core\TU.Tokens.Events.pas',
  TU.Tokens in 'Core\TU.Tokens.pas',
  UI.Exceptions in 'NtUtilsUI\Common\UI.Exceptions.pas',
  TU.Events in 'Core\TU.Events.pas',
  VirtualTreesEx.DefaultMenu in 'NtUtilsUI\Components\VirtualTreesEx.DefaultMenu.pas',
  DevirtualizedTree.Provider in 'NtUtilsUI\Components\DevirtualizedTree.Provider.pas',
  DevirtualizedTree in 'NtUtilsUI\Components\DevirtualizedTree.pas',
  TU.Tokens.Open in 'Core\TU.Tokens.Open.pas',
  UI.New.TokenFrame in 'UI\UI.New.TokenFrame.pas' {FrameTokens: TFrame},
  TU.AccountRights in 'Core\TU.AccountRights.pas',
  UI.Access in 'UI\UI.Access.pas' {AccessCheckForm},
  TU.Access in 'Core\TU.Access.pas',
  TU.DesktopAccess in 'Core\TU.DesktopAccess.pas',
  TU.Startup in 'Core\TU.Startup.pas',
  NtUiBackend.AppContainers in 'NtUtilsUI\Common\NtUiBackend.AppContainers.pas',
  NtUiBackend.UserProfiles in 'NtUtilsUI\Common\NtUiBackend.UserProfiles.pas',
  NtUiCommon.Interfaces in 'NtUtilsUI\Common\NtUiCommon.Interfaces.pas',
  NtUiFrame.Search in 'NtUtilsUI\Prototypes\NtUiFrame.Search.pas' {SearchFrame: TFrame},
  NtUiFrame.UserProfiles in 'NtUtilsUI\Prototypes\NtUiFrame.UserProfiles.pas' {UserProfilesFrame: TFrame},
  NtUiFrame.AppContainer.List in 'NtUtilsUI\Prototypes\NtUiFrame.AppContainer.List.pas' {AppContainerListFrame: TFrame},
  NtUiFrame.AppContainer.ListAllUsers in 'NtUtilsUI\Prototypes\NtUiFrame.AppContainer.ListAllUsers.pas' {AppContainerListAllUsersFrame: TFrame},
  NtUiFrame.AppContainer.Edit in 'NtUtilsUI\Prototypes\NtUiFrame.AppContainer.Edit.pas' {AppContainerFieldFrame: TFrame},
  NtUiCommon.Prototypes in 'NtUtilsUI\Common\NtUiCommon.Prototypes.pas',
  NtUiDialog.FrameHost in 'NtUtilsUI\Prototypes\NtUiDialog.FrameHost.pas' {FrameHostDialog},
  NtUiFrame.AppContainer.View in 'NtUtilsUI\Prototypes\NtUiFrame.AppContainer.View.pas' {AppContainerViewFrame: TFrame},
  NtUiBackend.Bits in 'NtUtilsUI\Common\NtUiBackend.Bits.pas',
  NtUiFrame.Bits in 'NtUtilsUI\Prototypes\NtUiFrame.Bits.pas' {BitsFrame: TFrame},
  NtUiFrame in 'NtUtilsUI\Prototypes\NtUiFrame.pas' {BaseFrame: TFrame},
  NtUiBackend.Acl in 'NtUtilsUI\Common\NtUiBackend.Acl.pas',
  NtUiCommon.PageHost in 'NtUtilsUI\Prototypes\NtUiCommon.PageHost.pas' {FramePages: TFrame},
  NtUiFrame.Ace in 'NtUtilsUI\Prototypes\NtUiFrame.Ace.pas',
  NtUiFrame.Acl in 'NtUtilsUI\Prototypes\NtUiFrame.Acl.pas' {AclFrame: TFrame},
  NtUiFrame.Security.Acl in 'NtUtilsUI\Prototypes\NtUiFrame.Security.Acl.pas' {AclSecurityFrame: TFrame},
  NtUiFrame.Security.OwnerGroup in 'NtUtilsUI\Prototypes\NtUiFrame.Security.OwnerGroup.pas' {OwnerGroupSecurityFrame: TFrame},
  NtUiFrame.Security in 'NtUtilsUI\Prototypes\NtUiFrame.Security.pas',
  NtUiFrame.Ace.Condition in 'NtUtilsUI\Prototypes\NtUiFrame.Ace.Condition.pas' {AceConditionFrame: TFrame},
  NtUiFrame.Hex.Edit in 'NtUtilsUI\Prototypes\NtUiFrame.Hex.Edit.pas' {HexEditFrame: TFrame},
  NtUiBackend.HexView in 'NtUtilsUI\Common\NtUiBackend.HexView.pas',
  NtUiFrame.Guid in 'NtUtilsUI\Prototypes\NtUiFrame.Guid.pas' {GuidFrame: TFrame},
  NtUiFrame.Sid.Integrity in 'NtUtilsUI\Prototypes\NtUiFrame.Sid.Integrity.pas' {FrameIntegrity: TFrame};

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

  if RtlGetCurrentPeb.ImageBaseAddress <> @ImageBase then
    IsLibrary := True;

  // Normal mode
  EnableNtUiLibExceptionHandling;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Token Universe';
  Application.HintHidePause := 20000;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
