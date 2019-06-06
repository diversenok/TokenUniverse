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
    function ChildSid(Rid: Cardinal): ISid;
  end;

  TSid = class(TInterfacedObject, ISid)
  protected
    FSid: PSid;
    FLookupCached: Boolean;
    FLookup: TTranslatedName;
    constructor CreateOwned(OwnedSid: PSid; Dummy: Integer = 0);
  public
    constructor CreateCopy(SourceSid: PSid);
    constructor CreateFromString(AccountOrSID: String);
    class function GetWellKnownSid(WellKnownSidType: TWellKnownSidType;
      out Sid: ISid): TNtxStatus;
    destructor Destroy; override;
    function Sid: PSid;
    function Lookup: TTranslatedName;
    function NewLookup: TTranslatedName;
    function EqualsTo(Sid2: ISid): Boolean;
    function SDDL: String;
    function SubAuthorities: Byte;
    function ParentSid: ISid;
    function ChildSid(Rid: Cardinal): ISid;
  end;

  TGroup = record
    SecurityIdentifier: ISid;
    Attributes: Cardinal; // SE_GROUP_*
  end;
  TGroupArray = array of TGroup;

// Convert an SID to its SDDL representation
function RtlxConvertSidToString(SID: PSid): String;

// Convert SDDL string to SID
function RtlxConvertStringToSid(SDDL: String; out SID: PSid): TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Ntapi.ntstatus, Winapi.WinBase, Winapi.Sddl,
  NtUtils.Exceptions, DelphiUtils.Strings, System.SysUtils;

{ TSid }

function TSid.ChildSid(Rid: Cardinal): ISid;
var
  Buffer: PSid;
  Status: NTSTATUS;
  i: Integer;
begin
  Buffer := AllocMem(RtlLengthRequiredSid(SubAuthorities + 1));

  // Copy identifier authority
  Status := RtlInitializeSid(Buffer, RtlIdentifierAuthoritySid(FSid),
    SubAuthorities + 1);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(Buffer);
    raise ENtError.Create(Status, 'RtlInitializeSid');
  end;

  // Copy existing sub authorities
  for i := 0 to SubAuthorities - 1 do
    RtlSubAuthoritySid(Buffer, i)^ := RtlSubAuthoritySid(FSid, i)^;

  // Set the last sub authority to the RID
  RtlSubAuthoritySid(Buffer, SubAuthorities)^ := Rid;

  Result := TSid.CreateOwned(Buffer);
end;

constructor TSid.CreateCopy(SourceSid: PSid);
var
  Status: NTSTATUS;
begin
  if not RtlValidSid(SourceSid) then
    raise ENtError.Create(STATUS_INVALID_SID, 'RtlValidSid');

  FSid := AllocMem(RtlLengthSid(SourceSid));
  Status := RtlCopySid(RtlLengthSid(SourceSid), FSid, SourceSid);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(FSid);
    raise ENtError.Create(Status, 'RtlCopySid');
  end;
end;

constructor TSid.CreateFromString(AccountOrSID: String);
var
  Status: TNtxStatus;
begin
  // Since someone might create an account which name is a valid SDDL string,
  // lookup the account name first. Parse it as SDDL only if this lookup failed.

  Status := LsaxLookupUserName(AccountOrSID, FSid);

  if not Status.IsSuccess and AccountOrSID.StartsWith('S-1-', True) then
    Status := RtlxConvertStringToSid(AccountOrSID, FSid);

  Status.RaiseOnError;
end;

constructor TSid.CreateOwned(OwnedSid: PSid; Dummy: Integer);
begin
  FSid := OwnedSid;
end;

destructor TSid.Destroy;
begin
  FreeMem(FSid);
  inherited;
end;

function TSid.EqualsTo(Sid2: ISid): Boolean;
begin
  Result := RtlEqualSid(FSid, Sid2.Sid);
end;

class function TSid.GetWellKnownSid(WellKnownSidType: TWellKnownSidType;
  out Sid: ISid): TNtxStatus;
var
  Buffer: PSid;
  BufferSize: Cardinal;
begin
  BufferSize := 0;

  Result.Location := 'CreateWellKnownSid';
  Result.Win32Result := CreateWellKnownSid(WellKnownSidType, nil, nil,
    BufferSize);

  if not NtxTryCheckBuffer(Result.Status, BufferSize) then
    Exit;

  Buffer := AllocMem(BufferSize);

  Result.Win32Result := CreateWellKnownSid(WellKnownSidType, nil, Buffer,
    BufferSize);

  if Result.IsSuccess then
    Sid := TSid.CreateOwned(Buffer)
  else
    FreeMem(Buffer);
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
  Status: NTSTATUS;
  Buffer: PSid;
  i: Integer;
begin
  // The rule is simple: we drop the last sub-authority and create a new SID.

  Assert(SubAuthorities > 0);

  Buffer := AllocMem(RtlLengthRequiredSid(SubAuthorities - 1));

  // Copy identifier authority
  Status := RtlInitializeSid(Buffer, RtlIdentifierAuthoritySid(FSid),
    SubAuthorities - 1);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(Buffer);
    raise ENtError.Create(Status, 'RtlInitializeSid');
  end;

  // Copy sub authorities
  for i := 0 to RtlSubAuthorityCountSid(Buffer)^ - 1 do
    RtlSubAuthoritySid(Buffer, i)^ := RtlSubAuthoritySid(FSid, i)^;

  Result := TSid.CreateOwned(Buffer);
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

function RtlxConvertStringToSid(SDDL: String; out SID: PSid): TNtxStatus;
var
  Buffer: PSid;
  IdAuthorityUInt64: UInt64;
  IdAuthority: TSidIdentifierAuthority;
begin
  // Despite the fact that RtlConvertSidToUnicodeString can convert SIDs with
  // zero sub authorities to SDDL, ConvertStringSidToSidW (for some reason)
  // can't convert them back. Fix this behaviour by parsing them manually.

  // Expected formats for an SID with 0 sub authorities:
  //        S-1-(\d+)     |     S-1-(0x[A-F\d]+)
  // where the value fits into a 6-byte (48-bit) buffer

  if TryStrToUInt64Ex(Copy(SDDL, Length('S-1-') + 1, Length(SDDL)),
    IdAuthorityUInt64) and (IdAuthorityUInt64 < UInt64(1) shl 48) then
  begin
    IdAuthority.Value[0] := Byte(IdAuthorityUInt64 shr 40);
    IdAuthority.Value[1] := Byte(IdAuthorityUInt64 shr 32);
    IdAuthority.Value[2] := Byte(IdAuthorityUInt64 shr 24);
    IdAuthority.Value[3] := Byte(IdAuthorityUInt64 shr 16);
    IdAuthority.Value[4] := Byte(IdAuthorityUInt64 shr 8);
    IdAuthority.Value[5] := Byte(IdAuthorityUInt64 shr 0);

    Buffer := AllocMem(RtlLengthRequiredSid(0));

    Result.Location := 'RtlInitializeSid';
    Result.Status := RtlInitializeSid(Buffer, @IdAuthority, 0);

    if Result.IsSuccess then
      SID := Buffer
    else
      FreeMem(Buffer);
  end
  else
  begin
    // Usual SDDLs

    Result.Location := 'ConvertStringSidToSidW';
    Result.Win32Result := ConvertStringSidToSidW(PWideChar(SDDL), Buffer);

    if not Result.IsSuccess then
      Exit;

    SID := AllocMem(RtlLengthSid(Buffer));

    Result.Location := 'RtlCopySid';
    Result.Status := RtlCopySid(RtlLengthSid(Buffer), SID, Buffer);

    if not Result.IsSuccess then
    begin
      FreeMem(SID);
      SID := nil;
    end;

    LocalFree(Buffer);
  end;
end;

end.
