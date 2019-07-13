unit Winapi.Sddl;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt;

function ConvertSidToStringSidW(Sid: PSid; var StringSid: PWideChar): LongBool;
  stdcall; external advapi32;

function ConvertStringSidToSidW(StringSid: PWideChar; var Sid: PSid): LongBool;
  stdcall; external advapi32;

function ConvertSecurityDescriptorToStringSecurityDescriptorW(
  SecurityDescriptor: PSecurityDescriptor; RequestedStringSDRevision: Cardinal;
  SecurityInformation: TSecurityInformation;
  out StringSecurityDescriptor: PWideChar;
  StringSecurityDescriptorLen: PCardinal): LongBool; stdcall; external advapi32;

implementation

end.
