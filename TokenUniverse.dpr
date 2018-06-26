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
  UI.Duplicate in 'UI\UI.Duplicate.pas' {DuplicateDialog},
  UI.HandleSearch in 'UI\UI.HandleSearch.pas' {FormHandleSearch},
  UI.Information in 'UI\UI.Information.pas' {InfoDialog},
  UI.ProcessList in 'UI\UI.ProcessList.pas' {ProcessListDialog},
  UI.Run in 'UI\UI.Run.pas' {RunDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
