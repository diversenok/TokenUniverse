unit Winapi.Wdc;

interface
{$WARN SYMBOL_PLATFORM OFF}

// rev
function WdcRunTaskAsInteractiveUser(CommandLine: PWideChar;
  CurrentDirectory: PWideChar; dwReserved: Cardinal): HRESULT; stdcall;
  external 'wdc.dll' delayed;

implementation

end.
