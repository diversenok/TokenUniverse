unit Winapi.Sddl;

interface

uses
  Winapi.WinNt, Winapi.WinBase;

function ConvertSidToStringSidW(Sid: PSid; var StringSid: PWideChar): LongBool;
  stdcall; external advapi32;

function ConvertStringSidToSidW(StringSid: PWideChar; var Sid: PSid): LongBool;
  stdcall; external advapi32;

implementation

end.
