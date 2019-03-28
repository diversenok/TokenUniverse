unit DelphiUtils.Strings;

interface

function EnabledDisabledToString(Value: LongBool): String;
function YesNoToString(Value: LongBool): String;
function CheckboxToString(Value: LongBool): String;
function BytesToString(Size: Cardinal): String;

function IntToHexEx(Value: Int64; Digits: Integer = 0): String; overload;
function IntToHexEx(Value: UInt64; Digits: Integer = 0): String; overload;

function TryStrToUInt64Ex(S: String; out Value: UInt64): Boolean;
function StrToUIntEx(S: String; Comment: String): Cardinal; inline;
function StrToUInt64Ex(S: String; Comment: String): UInt64; inline;

implementation

uses
  System.SysUtils;

function EnabledDisabledToString(Value: LongBool): String;
begin
  if Value then
    Result := 'Enabled'
  else
    Result := 'Disabled';
end;

function YesNoToString(Value: LongBool): String;
begin
  if Value then
    Result := 'Yes'
  else
    Result := 'No';
end;

function CheckboxToString(Value: LongBool): String;
begin
  if Value then
    Result := '☑'
  else
    Result := '☐';
end;

function BytesToString(Size: Cardinal): String;
begin
  if Size mod 1024 = 0 then
    Result := (Size div 1024).ToString + ' kB'
  else
    Result := Size.ToString + ' B';
end;

function IntToHexEx(Value: UInt64; Digits: Integer): String;
begin
  Result := '0x' + IntToHex(Value, Digits);
end;

function IntToHexEx(Value: Int64; Digits: Integer): String;
begin
  Result := '0x' + IntToHex(Value, Digits);
end;

function TryStrToUInt64Ex(S: String; out Value: UInt64): Boolean;
var
  E: Integer;
begin
  if S.StartsWith('0x') then
    S := S.Replace('0x', '$', []);

  Val(S, Value, E);
  Result := (E = 0);
end;

function StrToUInt64Ex(S: String; Comment: String): UInt64;
const
  E_DECHEX = 'Invalid %s. Please specify a decimal or a hexadecimal value.';
begin
  if not TryStrToUInt64Ex(S, Result) then
    raise EConvertError.Create(Format(E_DECHEX, [Comment]));
end;

function StrToUIntEx(S: String; Comment: String): Cardinal;
begin
  {$R-}
  Result := StrToUInt64Ex(S, Comment);
  {$R+}
end;

end.
