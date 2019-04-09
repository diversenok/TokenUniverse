unit NtUtils.Types;

interface

uses
  Winapi.WinNt, Winapi.securitybaseapi, NtUtils.Lsa;

const
  SE_GROUP_USER_DEFAULT = SE_GROUP_ENABLED or SE_GROUP_ENABLED_BY_DEFAULT;
  SE_GROUP_ALL_FLAGS = SE_GROUP_MANDATORY or SE_GROUP_OWNER or
    SE_GROUP_USE_FOR_DENY_ONLY or SE_GROUP_INTEGRITY or SE_GROUP_RESOURCE or
    SE_GROUP_LOGON_ID;

type
  TTranslatedName = NtUtils.Lsa.TTranslatedName;

  ISid = interface
    function Sid: PSid;
    function Lookup: TTranslatedName;
    function NewLookup: TTranslatedName;
    function EqualsTo(Sid2: ISid): Boolean;
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
  end;

  TGroup = record
    SecurityIdentifier: ISid;
    Attributes: Cardinal; // SE_GROUP_*
  end;
  TGroupArray = array of TGroup;

  TPrivilege = TLuidAndAttributes;
  TPrivilegeArray = array of TPrivilege;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Winapi.WinBase, Winapi.Sddl,
  NtUtils.Exceptions, System.SysUtils;

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
  Status: NTSTATUS;
  Buffer: PSid;
begin
  // Check if it's a valid account name first. If not, try to parse it as SDDL.
  // It should be done in such order since someone might create an account
  // which name can be parsed as SDDL.

  Status := LsaxLookupUserName(AccountOrSID, FSid);

  if not NT_SUCCESS(Status) and AccountOrSID.StartsWith('S-1-', True) then
  begin
    WinCheck(ConvertStringSidToSidW(PWideChar(AccountOrSID), Buffer),
      'ConvertStringSidToSidW');

    try
      CreateCopy(Buffer);
    finally
      LocalFree(Buffer);
    end;
  end
  else
    NativeCheck(Status, 'LsaLookupNames2');
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

function TSid.Sid: PSid;
begin
  Result := FSid;
end;

end.
