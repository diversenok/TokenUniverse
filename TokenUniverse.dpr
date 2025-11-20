program TokenUniverse;

uses
  Vcl.Forms,
  Ntapi.ntpebteb,
  NtUtils,
  NtUtils.Svc.SingleTaskSvc,
  NtUiLib.Errors,
  NtUtils.Com,
  NtUtils.Threads,
  DelphiUiLib.LiteReflection.Types,
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
  NtUiCommon.Forms in 'NtUtilsUI\Common\NtUiCommon.Forms.pas',
  VclEx.Form in 'NtUtilsUI\VclEx\VclEx.Form.pas',
  NtUiCommon.Colors in 'NtUtilsUI\Common\NtUiCommon.Colors.pas',
  NtUiCommon.Icons in 'NtUtilsUI\Common\NtUiCommon.Icons.pas',
  TU.Processes.Create in 'Core\TU.Processes.Create.pas',
  UI.Prototypes.Groups in 'NtUtilsUI\Prototypes\UI.Prototypes.Groups.pas' {FrameGroups: TFrame},
  NtUiCommon.Helpers in 'NtUtilsUI\Common\NtUiCommon.Helpers.pas',
  VirtualTreesEx in 'NtUtilsUI\Components\VirtualTreesEx.pas',
  UI.Prototypes.Privileges in 'NtUtilsUI\Prototypes\UI.Prototypes.Privileges.pas' {FramePrivileges: TFrame},
  UI.Prototypes.Sid.Edit in 'NtUtilsUI\Prototypes\UI.Prototypes.Sid.Edit.pas' {SidEditor: TFrame},
  TU.Observers in 'Core\TU.Observers.pas',
  TU.Tokens.Events in 'Core\TU.Tokens.Events.pas',
  TU.Tokens in 'Core\TU.Tokens.pas',
  NtUiCommon.Exceptions in 'NtUtilsUI\Common\NtUiCommon.Exceptions.pas',
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
  NtUiFrame.Sid.Integrity in 'NtUtilsUI\Prototypes\NtUiFrame.Sid.Integrity.pas' {FrameIntegrity: TFrame},
  NtUiBuiltin.DsObjectPicker in 'NtUtilsUI\Prototypes\NtUiBuiltin.DsObjectPicker.pas',
  NtUiFrame.Sid.Trust in 'NtUtilsUI\Prototypes\NtUiFrame.Sid.Trust.pas' {FrameTrustSid: TFrame},
  UI.New.UserManager in 'UI\UI.New.UserManager.pas' {UserManagerTokens},
  TU.UserManager in 'Core\TU.UserManager.pas',
  NtUiBackend.Sids.Abbreviations in 'NtUtilsUI\Common\NtUiBackend.Sids.Abbreviations.pas',
  NtUiFrame.Sids.Abbreviations in 'NtUtilsUI\Prototypes\NtUiFrame.Sids.Abbreviations.pas' {SidAbbreviationFrame: TFrame},
  NtUiBackend.Sids in 'NtUtilsUI\Common\NtUiBackend.Sids.pas',
  VclEx.Edit in 'NtUtilsUI\VclEx\VclEx.Edit.pas',
  NtUiFrame.Sids.Capabilities in 'NtUtilsUI\Prototypes\NtUiFrame.Sids.Capabilities.pas' {CapabilityListFrame: TFrame},
  NtUiBackend.Sids.Capabilities in 'NtUtilsUI\Common\NtUiBackend.Sids.Capabilities.pas';

{$R *.res}
{$WEAKLINKRTTI ON}

begin
  NtxSetNameThread(NtxCurrentThread, 'TU Main thread');

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

  // Enable our excption reporting
  EnableNtUiLibExceptionHandling;
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  // Help COM initialization under LPAC + use at least implicit MTA
  ComxSuppressCapabilityCheck;
  ComxInitializeImplicitOnce;

  // Enable lite reflection for custom types
  RttixRegisterAllFormatter;

  // Proceed to VCL initialization
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Token Universe';
  Application.HintHidePause := 20000;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
