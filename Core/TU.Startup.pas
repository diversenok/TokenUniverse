unit TU.Startup;

{
  This module provides helpers to execute at startup.
}

interface

// Load the most widely used delayed modules
procedure TuPreloadDelayModules;

implementation

uses
  Ntapi.ntsam, Ntapi.winsta, Ntapi.xmllite, NtUtils.Ldr;

procedure TuPreloadDelayModules;
begin
  // SAM - for SID suggestions
  LdrxCheckDelayedModule(delayed_samlib);

  // WinSta - for session lookup
  LdrxCheckDelayedModule(delayed_winsta);

  // XmlLite - for the task dialog
  LdrxCheckDelayedModule(delayed_xmllite);
end;

end.
