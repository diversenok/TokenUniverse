unit NtUtils.Security.Sid;

interface

uses
  Winapi.WinNt, Winapi.securitybaseapi, NtUtils.Lsa;

const
  SE_GROUP_USER_DEFAULT = SE_GROUP_ENABLED or SE_GROUP_ENABLED_BY_DEFAULT;

  SE_GROUP_ALL_FLAGS = SE_GROUP_MANDATORY or SE_GROUP_OWNER or
                       SE_GROUP_USE_FOR_DENY_ONLY or SE_GROUP_INTEGRITY or
                       SE_GROUP_RESOURCE or SE_GROUP_LOGON_ID;

type
  TTranslatedName = NtUtils.Lsa.TTranslatedName;

  ISid = interface
    function Sid: PSid;
    function Lookup: TTranslatedName;
    function NewLookup: TTranslatedName;
    function EqualsTo(Sid2: ISid): Boolean;
    function SDDL: String;
    function SubAuthorities: Byte;
    function ParentSid: ISid;
  end;

  TSid = class(TInterfacedObject, ISid)
  protected
    FSid: PSid;
    FLookupCached: Boolean;
    FLookup: TTranslatedName;
  public
    constructor CreateCopy(SourceSid: PSid);
    constructor CreateFromString(AccountOrSID: String);
    class function GetWellKnownSid(WellKnownSidType: TWellKnownSidType;
      out Sid: ISid): Boolean;
    destructor Destroy; override;
    function Sid: PSid;
    function Lookup: TTranslatedName;
    function NewLookup: TTranslatedName;
    function EqualsTo(Sid2: ISid): Boolean;
    function SDDL: String;
    function SubAuthorities: Byte;
    function ParentSid: ISid;
  end;

  TGroup = record
    SecurityIdentifier: ISid;
    Attributes: Cardinal; // SE_GROUP_*
  end;
  TGroupArray = array of TGroup;

// Convert an SID to its SDDL representation
function RtlxConvertSidToString(SID: PSid): String;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Winapi.WinBase, Winapi.Sddl,
  NtUtils.Exceptions, DelphiUtils.Strings, System.SysUtils;

{ TSid }

constructor TSid.CreateCopy(SourceSid: PSid);
var
  BufferSize: Cardinal;
begin
  Assert(Assigned(SourceSid) and RtlValidSid(SourceSid));

  BufferSize := RtlLengthSid(SourceSid);
  FSid := AllocMem(BufferSize);
  RtlCopySid(BufferSize, FSid, SourceSid);
end;

constructor TSid.CreateFromString(AccountOrSID: String);
var
  Status: TNtxStatus;
  Buffer: PSid;
  IdentifierAuthorityUInt64: UInt64;
  IdentifierAuthority: TSidIdentifierAuthority;
begin
  // Since someone might create an account which name is a valid SDDL string,
  // lookup the account name first. Parse it as SDDL only if this lookup failed.

  Status := LsaxLookupUserName(AccountOrSID, FSid);

  if not Status.IsSuccess and AccountOrSID.StartsWith('S-1-', True) then
  begin
    // Despite the fact that RtlConvertSidToUnicodeString can convert SIDs with
    // zero sub authorities to SDDL, ConvertStringSidToSidW (for some reason)
    // can't convert them back. Fix this behaviour by parsing them manually.

    // Expected formats for an SID with 0 sub authorities:
    //        S-1-(\d+)     |     S-1-(0x[A-F\d]+)
    // where the value fits into a 6-byte (48-bit) buffer

    if TryStrToUInt64Ex(Copy(AccountOrSID, Length('S-1-') + 1,
      Length(AccountOrSID)), IdentifierAuthorityUInt64) and
      (IdentifierAuthorityUInt64 < UInt64(1) shl 48) then
    begin
      IdentifierAuthority.Value[0] := Byte(IdentifierAuthorityUInt64 shr 40);
      IdentifierAuthority.Value[1] := Byte(IdentifierAuthorityUInt64 shr 32);
      IdentifierAuthority.Value[2] := Byte(IdentifierAuthorityUInt64 shr 24);
      IdentifierAuthority.Value[3] := Byte(IdentifierAuthorityUInt64 shr 16);
      IdentifierAuthority.Value[4] := Byte(IdentifierAuthorityUInt64 shr 8);
      IdentifierAuthority.Value[5] := Byte(IdentifierAuthorityUInt64 shr 0);

      Buffer := AllocMem(RtlLengthRequiredSid(0));
      try
        RtlInitializeSid(Buffer, @IdentifierAuthority, 0);
        CreateCopy(Buffer);
      finally
        FreeMem(Buffer);
      end;
    end
    else
    begin
      WinCheck(ConvertStringSidToSidW(PWideChar(AccountOrSID), Buffer),
        'ConvertStringSidToSidW');

      try
        CreateCopy(Buffer);
      finally
        LocalFree(Buffer);
      end;
    end;
  end
  else
    Status.RaiseOnError;
end;

destructor TSid.Destroy;
begin
  FreeMem(FSid);
  inherited;
end;

function TSid.EqualsTo(Sid2: ISid): Boolean;
begin
  Result := RtlEqualSid(FSid, Sid2.Sid)
end;

class function TSid.GetWellKnownSid(WellKnownSidType: TWellKnownSidType;
  out Sid: ISid): Boolean;
var
  Buffer: PSid;
  BufferSize: Cardinal;
begin
  BufferSize := 0;
  CreateWellKnownSid(WellKnownSidType, nil, nil, BufferSize);

  if not WinTryCheckBuffer(BufferSize) then
    Exit(False);

  Buffer := AllocMem(BufferSize);
  try
    Result := CreateWellKnownSid(WellKnownSidType, nil, Buffer, BufferSize);

    if Result then
      Sid := TSid.CreateCopy(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

function TSid.Lookup: TTranslatedName;
begin
  // TODO: Optimize multiple queries with LsaLookupSids / LsaLookupNames
  if FLookupCached then
    Result := FLookup
  else
    Result := NewLookup;
end;

function TSid.NewLookup: TTranslatedName;
begin
  FLookup := LsaxLookupSid(FSid);
  FLookupCached := True;
  Result := FLookup;
end;

function TSid.ParentSid: ISid;
var
  Buffer: PSid;
  i: Integer;
begin
  // The rule is simple: we drop the last sub-authority and create a new SID.

  if SubAuthorities = 0 then
    Result := nil
  else
  begin
    Buffer := AllocMem(RtlLengthRequiredSid(SubAuthorities - 1));
    try
      // Copy identifier authority
      if not NT_SUCCESS(RtlInitializeSid(Buffer,
        RtlIdentifierAuthoritySid(FSid), SubAuthorities - 1)) then
        Exit(nil);

      // Copy sub authorities
      for i := 0 to RtlSubAuthorityCountSid(Buffer)^ - 1 do
        RtlSubAuthoritySid(Buffer, i)^ := RtlSubAuthoritySid(FSid, i)^;

      Result := TSid.CreateCopy(Buffer);
    finally
      FreeMem(Buffer);
    end;
  end;
end;

function TSid.SDDL: String;
begin
  Result := RtlxConvertSidToString(FSid);
end;

function TSid.Sid: PSid;
begin
  Result := FSid;
end;

function TSid.SubAuthorities: Byte;
begin
  Result := RtlSubAuthorityCountSid(FSid)^;
end;

{ Functions }

function RtlxConvertSidToString(SID: PSid): String;
var
  SDDL: UNICODE_STRING;
  Buffer: array [0 .. SECURITY_MAX_SID_STRING_CHARACTERS] of WideChar;
begin
  SDDL.Length := 0;
  SDDL.MaximumLength := SECURITY_MAX_SID_STRING_CHARACTERS;
  SDDL.Buffer := PWideChar(@Buffer);

  if NT_SUCCESS(RtlConvertSidToUnicodeString(SDDL, SID, False)) then
    Result := SDDL.ToString
  else
    Result := '';
end;

end.
