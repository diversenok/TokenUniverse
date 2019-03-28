unit TU.Common;

interface

/// <symmary>
///  Converts a string that contains a decimal or a hexadecimal number to an
///  integer.
/// </summary>
/// <exception cref="EConvertError"> Can raise EConvertError. </exception>
function TryStrToUInt64Ex(S: String; out Value: UInt64): Boolean;
function StrToUIntEx(S: String; Comment: String): Cardinal; inline;
function StrToUInt64Ex(S: String; Comment: String): UInt64; inline;

/// <symmary>
///  Converts a number of 100ns intervals from 01.01.1601 to Delphi's
///  <c>TDateTime</c> type.
/// </summary>
function NativeTimeToLocalDateTime(NativeTime: Int64): TDateTime;

/// <symmary>
///  Converts Delphi's <c>TDateTime</c> to a number of 100ns intervals from
///  01.01.1601.
/// </summary>
function DateTimeToNative(LocalTime: TDateTime): Int64;

implementation

uses
  System.SysUtils, System.DateUtils;

{ Conversion functions }

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

const
  DAYS_FROM_1601 = 109205;
  NATIVE_TIME_SCALE = 864000000000; // 100ns in 1 day

function NativeTimeToLocalDateTime(NativeTime: Int64): TDateTime;
begin
  Result := NativeTime / NATIVE_TIME_SCALE - DAYS_FROM_1601;
  Result := TTimeZone.Local.ToLocalTime(Result);
end;

function DateTimeToNative(LocalTime: TDateTime): Int64;
begin
  Result := Trunc(NATIVE_TIME_SCALE * (DAYS_FROM_1601 +
    TTimeZone.Local.ToUniversalTime(LocalTime)));
end;

end.
