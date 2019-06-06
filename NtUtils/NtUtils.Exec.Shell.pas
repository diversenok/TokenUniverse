unit NtUtils.Exec.Shell;

interface

uses
  NtUtils.Exec;

type
  TExecShellExecute = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    procedure Execute(ParamSet: IExecProvider);
  end;

implementation

uses
  Winapi.Shell, Winapi.WinUser, NtUtils.Exceptions;

{ TExecShellExecute }

procedure TExecShellExecute.Execute(ParamSet: IExecProvider);
var
  ShellExecInfo: TShellExecuteInfoW;
begin
  FillChar(ShellExecInfo, SizeOf(ShellExecInfo), 0);
  ShellExecInfo.cbSize := SizeOf(ShellExecInfo);
  ShellExecInfo.fMask := SEE_MASK_NOASYNC or SEE_MASK_UNICODE;

  ShellExecInfo.lpFile := PWideChar(ParamSet.Application);

  if ParamSet.Provides(ppParameters) then
    ShellExecInfo.lpParameters := PWideChar(ParamSet.Parameters);

  if ParamSet.Provides(ppCurrentDirectory) then
    ShellExecInfo.lpDirectory := PWideChar(ParamSet.CurrentDircetory);

  if ParamSet.Provides(ppRequireElevation) and ParamSet.RequireElevation then
    ShellExecInfo.lpVerb := 'runas';

  if ParamSet.Provides(ppShowWindowMode) then
    ShellExecInfo.nShow := ParamSet.ShowWindowMode
  else
    ShellExecInfo.nShow := SW_SHOWNORMAL;

  WinCheck(ShellExecuteExW(ShellExecInfo), 'ShellExecuteExW');
end;

function TExecShellExecute.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory, ppRequireElevation, ppShowWindowMode:
      Result := True;
  else
    Result := False;
  end;
end;

end.
