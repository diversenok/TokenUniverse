unit NtUtils.Security.Sid;

interface

uses
  Winapi.WinNt, Winapi.securitybaseapi, NtUtils.Exceptions;

type
  TTranslatedName = record
    DomainName, UserName: String;
    SidType: TSidNameUse;
    function FullName: String;
  end;

  ISid = interface
    function Sid: PSid;
    function SidLength: Cardinal;
    function EqualsTo(Sid2: PSid): Boolean;
    function RefreshLookup: TNtxStatus;
    function ParentSid: ISid;
    function ChildSid(Rid: Cardinal): ISid;
    function SDDL: String;
    function AsString: String;
    function DomainName: String;
    function UserName: String;
    function SidType: TSidNameUse;
    function IdentifyerAuthority: PSidIdentifierAuthority;
    function Rid: Cardinal;
    function SubAuthorities: Byte;
    function SubAuthority(Index: Integer): Cardinal;
    procedure SetSubAuthority(Index: Integer; NewValue: Cardinal);
  end;

  TSid = class(TInterfacedObject, ISid)
  protected
    FSid: PSid;
    FLookupCached: Boolean;
    FLookup: TTranslatedName;
    constructor CreateOwned(OwnedSid: PSid; Dummy: Integer = 0);
    procedure ValidateLookup;
  public
    constructor CreateCopy(SourceSid: PSid);
    constructor CreateNew(const IdentifyerAuthority: TSidIdentifierAuthority;
      SubAuthorities: Byte; SubAuthourity0: Cardinal = 0;
      SubAuthourity1: Cardinal = 0; SubAuthourity2: Cardinal = 0;
      SubAuthourity3: Cardinal = 0; SubAuthourity4: Cardinal = 0);
    constructor CreateFromString(AccountOrSID: String); // May raise exceptions
    destructor Destroy; override;
    function Sid: PSid;
    function SidLength: Cardinal;
    function EqualsTo(Sid2: PSid): Boolean;
    function RefreshLookup: TNtxStatus;
    function ParentSid: ISid;
    function ChildSid(Rid: Cardinal): ISid;
    function SDDL: String;
    function AsString: String;
    function DomainName: String;
    function UserName: String;
    function SidType: TSidNameUse;
    function IdentifyerAuthority: PSidIdentifierAuthority;
    function Rid: Cardinal;
    function SubAuthorities: Byte;
    function SubAuthority(Index: Integer): Cardinal;
    procedure SetSubAuthority(Index: Integer; NewValue: Cardinal);
  end;

  TGroup = record
    SecurityIdentifier: ISid;
    Attributes: Cardinal; // SE_GROUP_*
  end;

function RtlxpApplySddlOverrides(SID: PSid; var SDDL: String): Boolean;

// Convert an SID to its SDDL representation
function RtlxConvertSidToString(SID: PSid): String;

// Convert SDDL string to SID
function RtlxConvertStringToSid(SDDL: String; out SID: PSid): TNtxStatus;

// Construct a well-known SID
function SddlxGetWellKnownSid(WellKnownSidType: TWellKnownSidType;
  out Sid: ISid): TNtxStatus;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Ntapi.ntstatus, Winapi.WinBase, Winapi.Sddl,
  NtUtils.Lsa, DelphiUtils.Strings, System.SysUtils;

{ TTranslatedName }

function TTranslatedName.FullName: String;
begin
  if SidType = SidTypeDomain then
    Result := DomainName
  else if (UserName <> '') and (DomainName <> '') then
    Result := DomainName + '\' + UserName
  else if (UserName <> '') then
    Result := UserName
  else
    Result := '';
end;

{ TSid }

function TSid.AsString: String;
begin
  // Return most suitable name we have
  ValidateLookup;
  Result := FLookup.FullName;
  if Result = '' then
    Result := SDDL;
end;

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
    NtxAssert(Status, 'RtlInitializeSid');
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
    NtxAssert(STATUS_INVALID_SID, 'RtlValidSid');

  FSid := AllocMem(RtlLengthSid(SourceSid));
  Status := RtlCopySid(RtlLengthSid(SourceSid), FSid, SourceSid);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(FSid);
    NtxAssert(Status, 'RtlCopySid');
  end;
end;

constructor TSid.CreateFromString(AccountOrSID: String);
var
  Status: TNtxStatus;
  LookupSid: ISid;
begin
  // Since someone might create an account which name is a valid SDDL string,
  // lookup the account name first. Parse it as SDDL only if this lookup failed.

  Status := LsaxLookupUserName(AccountOrSID, LookupSid);

  if Status.IsSuccess then
  begin
    CreateCopy(LookupSid.Sid);
    Exit;
  end;

  // The string can start with "S-1-" and represent an arbitrary SID or can be
  // one of ~40 double-letter abbreviations. See [MS-DTYP] for SDDL definition.
  if (Length(AccountOrSID) = 2) or AccountOrSID.StartsWith('S-1-', True) then
    Status := RtlxConvertStringToSid(AccountOrSID, FSid);

  Status.RaiseOnError;
end;

constructor TSid.CreateNew(const IdentifyerAuthority: TSidIdentifierAuthority;
  SubAuthorities: Byte; SubAuthourity0, SubAuthourity1, SubAuthourity2,
  SubAuthourity3, SubAuthourity4: Cardinal);
var
  Status: NTSTATUS;
begin
  FSid := AllocMem(RtlLengthRequiredSid(SubAuthorities));

  Status := RtlInitializeSid(FSid, @IdentifyerAuthority, SubAuthorities);

  if not NT_SUCCESS(Status) then
  begin
    FreeMem(FSid);
    NtxAssert(Status, 'RtlInitializeSid');
  end;

  if SubAuthorities > 0 then
    RtlSubAuthoritySid(FSid, 0)^ := SubAuthourity0;

  if SubAuthorities > 1 then
    RtlSubAuthoritySid(FSid, 1)^ := SubAuthourity1;

  if SubAuthorities > 2 then
    RtlSubAuthoritySid(FSid, 2)^ := SubAuthourity2;

  if SubAuthorities > 3 then
    RtlSubAuthoritySid(FSid, 3)^ := SubAuthourity3;

  if SubAuthorities > 4 then
    RtlSubAuthoritySid(FSid, 4)^ := SubAuthourity4;
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

function TSid.DomainName: String;
begin
  ValidateLookup;
  Result := FLookup.DomainName;
end;

function TSid.EqualsTo(Sid2: PSid): Boolean;
begin
  Result := RtlEqualSid(FSid, Sid2);
end;

function TSid.IdentifyerAuthority: PSidIdentifierAuthority;
begin
  Result := RtlIdentifierAuthoritySid(FSid);
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
    NtxAssert(Status, 'RtlInitializeSid');
  end;

  // Copy sub authorities
  for i := 0 to RtlSubAuthorityCountSid(Buffer)^ - 1 do
    RtlSubAuthoritySid(Buffer, i)^ := RtlSubAuthoritySid(FSid, i)^;

  Result := TSid.CreateOwned(Buffer);
end;

function TSid.RefreshLookup: TNtxStatus;
begin
  // TODO: Optimize multiple queries with LsaLookupSids / LsaLookupNames
  Result := LsaxLookupSid(FSid, FLookup);
  FLookupCached := FLookupCached or Result.IsSuccess;
end;

function TSid.Rid: Cardinal;
begin
  if SubAuthorities > 0 then
    Result := SubAuthority(SubAuthorities - 1)
  else
    Result := 0;
end;

function TSid.SDDL: String;
begin
  Result := RtlxConvertSidToString(FSid);
end;

procedure TSid.SetSubAuthority(Index: Integer; NewValue: Cardinal);
begin
  RtlSubAuthoritySid(FSid, Index)^ := NewValue;
end;

function TSid.Sid: PSid;
begin
  Result := FSid;
end;

function TSid.SidLength: Cardinal;
begin
  Result := RtlLengthSid(FSid);
end;

function TSid.SidType: TSidNameUse;
begin
  ValidateLookup;
  Result := FLookup.SidType;
end;

function TSid.SubAuthorities: Byte;
begin
  Result := RtlSubAuthorityCountSid(FSid)^;
end;

function TSid.SubAuthority(Index: Integer): Cardinal;
begin
  if (Index >= 0) and (Index < SubAuthorities) then
    Result := RtlSubAuthoritySid(FSid, Index)^
  else
    Result := 0;
end;

function TSid.UserName: String;
begin
  ValidateLookup;
  Result := FLookup.UserName;
end;

procedure TSid.ValidateLookup;
begin
  if not FLookupCached then
    RefreshLookup;
end;

{ Functions }

function RtlxpApplySddlOverrides(SID: PSid; var SDDL: String): Boolean;
begin
  Result := False;

  // We override convertion of some SIDs to strings for the sake of readability.
  // The result is still a parsable SDDL string.

  case RtlIdentifierAuthoritySid(SID).ToInt64 of

    // Integrity: S-1-16-x
    SECURITY_MANDATORY_LABEL_AUTHORITY_ID:
      if RtlSubAuthorityCountSid(SID)^ = 1 then
      begin
        SDDL := 'S-1-16-' + IntToHexEx(RtlSubAuthoritySid(SID, 0)^, 4);
        Result := True;
      end;

  end;
end;

function RtlxConvertSidToString(SID: PSid): String;
var
  SDDL: UNICODE_STRING;
  Buffer: array [0 .. SECURITY_MAX_SID_STRING_CHARACTERS - 1] of WideChar;
begin
  Result := '';

  if RtlxpApplySddlOverrides(SID, Result) then
    Exit;

  SDDL.Length := 0;
  SDDL.MaximumLength := SizeOf(Buffer);
  SDDL.Buffer := @Buffer;

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
    IdAuthority.FromInt64(IdAuthorityUInt64);

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

function SddlxGetWellKnownSid(WellKnownSidType: TWellKnownSidType;
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

end.
