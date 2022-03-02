unit TU.Exceptions;

{
  This module defines an abstraction for handling exceptions.
}

interface

uses
  System.SysUtils;

// Show the default exception dialog
procedure ReportException(E: Exception);

implementation

uses
  NtUiLib.Exceptions.Dialog, Vcl.Forms;

procedure ReportException;
begin
  ShowNtxException(Application.Handle, E)
end;

end.
