unit NtUtils.Exec.Wdc;

interface

uses
  NtUtils.Exec;

type
  TExecCallWdc = class(TInterfacedObject, IExecMethod)
    function Supports(Parameter: TExecParam): Boolean;
    function Execute(ParamSet: IExecProvider): TProcessInfo;
  end;

implementation

uses
  Winapi.Wdc, NtUtils.Exceptions, Winapi.WinError, NtUtils.Ldr;

{ TExecCallWdc }

function TExecCallWdc.Execute(ParamSet: IExecProvider): TProcessInfo;
var
  CommandLine: String;
  CurrentDir: PWideChar;
  ResultCode: HRESULT;
begin
  CommandLine := PrepareCommandLine(ParamSet);

  if ParamSet.Provides(ppCurrentDirectory) then
    CurrentDir := PWideChar(ParamSet.CurrentDircetory)
  else
    CurrentDir := nil;

  LdrxCheckModuleDelayedImport(wdc, 'WdcRunTaskAsInteractiveUser').RaiseOnError;

  ResultCode := WdcRunTaskAsInteractiveUser(PWideChar(CommandLine), CurrentDir,
    0);

  if not Succeeded(ResultCode) then
    raise ENtError.Create(Cardinal(ResultCode), 'WdcRunTaskAsInteractiveUser');

  // The method does not provide any information about the newly created process
  FillChar(Result, SizeOf(Result), 0);
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
