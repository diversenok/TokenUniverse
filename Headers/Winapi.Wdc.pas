unit Winapi.Wdc;

interface
{$WARN SYMBOL_PLATFORM OFF}

const
  wdc = 'wdc.dll';

// rev
function WdcRunTaskAsInteractiveUser(CommandLine: PWideChar;
  CurrentDirectory: PWideChar; dwReserved: Cardinal): HRESULT; stdcall;
  external wdc delayed;

implementation

end.
