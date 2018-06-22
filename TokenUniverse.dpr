program TokenUniverse;

uses
  Vcl.Forms,
  UI.TokenListForm in 'UI.TokenListForm.pas' {FormMain},
  TU.TokenUtils in 'TU.TokenUtils.pas',
  TU.Common in 'TU.Common.pas',
  UI.Information in 'UI.Information.pas' {InfoDialog},
  UI.Duplicate in 'UI.Duplicate.pas' {DuplicateDialog},
  TU.EnumProcesses in 'TU.EnumProcesses.pas',
  UI.ProcessList in 'UI.ProcessList.pas' {ProcessListDialog},
  UI.Run in 'UI.Run.pas' {RunDialog},
  UI.TokenListFrame in 'UI.TokenListFrame.pas' {FrameTokenList: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
