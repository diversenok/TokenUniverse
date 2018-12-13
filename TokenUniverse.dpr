program TokenUniverse;

uses
  Vcl.Forms,
  TU.Common in 'Core\TU.Common.pas',
  TU.Handles in 'Core\TU.Handles.pas',
  TU.NativeAPI in 'Core\TU.NativeAPI.pas',
  TU.Processes in 'Core\TU.Processes.pas',
  TU.Tokens in 'Core\TU.Tokens.pas',
  UI.TokenListFrame in 'UI\UI.TokenListFrame.pas' {FrameTokenList: TFrame},
  UI.MainForm in 'UI\UI.MainForm.pas' {FormMain},
  UI.Modal.AccessAndType in 'UI\UI.Modal.AccessAndType.pas' {DialogAccessAndType},
  UI.HandleSearch in 'UI\UI.HandleSearch.pas' {FormHandleSearch},
  UI.Information in 'UI\UI.Information.pas' {InfoDialog},
  UI.ProcessList in 'UI\UI.ProcessList.pas' {ProcessListDialog},
  UI.Run in 'UI\UI.Run.pas' {RunDialog},
  TU.Winapi in 'Core\TU.Winapi.pas',
  TU.RestartSvc in 'Core\TU.RestartSvc.pas',
  TU.WtsApi in 'Core\TU.WtsApi.pas',
  TU.Suggestions in 'Core\TU.Suggestions.pas',
  UI.Modal.Session in 'UI\UI.Modal.Session.pas' {SessionDialog},
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
  UI.Modal.Access in 'UI\UI.Modal.Access.pas' {DialogAccess};

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
