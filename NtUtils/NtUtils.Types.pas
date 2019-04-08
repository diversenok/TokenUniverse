unit NtUtils.Types;

interface

uses
  Winapi.WinNt, NtUtils.Lsa;

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
  end;

  TSid = class(TInterfacedObject, ISid)
  protected
    FSid: PSid;
  public
    constructor CreateCopy(SourceSid: PSid);
    constructor CreateFromString(AccountOrSID: String);
    destructor Destroy; override;
    function Sid: PSid;
    function Lookup: TTranslatedName; virtual;
  end;

  TCachedLookupSid = class(TSid, ISid)
  protected
    FLookup: TTranslatedName;
  public
    procedure AfterConstruction; override;
    function Lookup: TTranslatedName; override;
  end;

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
  end;

  NativeCheck(Status, 'LsaLookupNames2');
end;

destructor TSid.Destroy;
begin
  FreeMem(FSid);
  inherited;
end;

function TSid.Lookup: TTranslatedName;
begin
  Result := LsaxLookupSid(FSid);
end;

function TSid.Sid: PSid;
begin
  Result := FSid;
end;

{ TCachedLookupSid }

procedure TCachedLookupSid.AfterConstruction;
begin
  inherited;
  FLookup := inherited Lookup; // Cache to query only once
end;

function TCachedLookupSid.Lookup: TTranslatedName;
begin
  Result := FLookup;
end;

end.
