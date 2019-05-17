unit NtUtils.Exec.Wdc;

interface

uses
  NtUtils.Exec;

type
  TExecCallWdc = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    procedure Execute(ParamSet: IExecProvider);
  end;

implementation

uses
  Winapi.Wdc, NtUtils.Exceptions, Winapi.WinError;

{ TExecCallWdc }

procedure TExecCallWdc.Execute(ParamSet: IExecProvider);
var
  CommandLine: String;
  CurrentDir: PWideChar;
  Result: HRESULT;
begin
  CommandLine := PrepareCommandLine(ParamSet);

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  // TODO: check delayed import
  Result := WdcRunTaskAsInteractiveUser(PWideChar(CommandLine), CurrentDir, 0);

  if not Succeeded(Result) then
    raise EWinError.Create(Result, 'WdcRunTaskAsInteractiveUser');
end;

function TExecCallWdc.Supports(Parameter: TExecParam): Boolean;
begin
  case Parameter of
    ppParameters, ppCurrentDirectory:
      Result := True;
  else
    Result := False;
  end;
end;

end.
