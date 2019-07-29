unit NtUtils.Tokens.Misc;

interface

uses
  Winapi.WinNt, NtUtils.Security.Sid;

function NtxpAllocPrivileges(Privileges: TArray<TLuid>;
  Attribute: Cardinal): PTokenPrivileges;
function NtxpAllocPrivileges2(Privileges: TArray<TPrivilege>): PTokenPrivileges;

function NtxpAllocPrivilegeSet(Privileges: TArray<TPrivilege>): PPrivilegeSet;

function NtxpAllocGroups(Sids: TArray<ISid>; Attribute: Cardinal): PTokenGroups;
function NtxpAllocGroups2(Groups: TArray<TGroup>): PTokenGroups;

implementation

function NtxpAllocPrivileges(Privileges: TArray<TLuid>;
  Attribute: Cardinal): PTokenPrivileges;
var
  i: Integer;
begin
  Result := AllocMem(SizeOf(Integer) +
    Length(Privileges) * SizeOf(TLUIDAndAttributes));

  Result.PrivilegeCount := Length(Privileges);

  for i := 0 to High(Privileges) do
  begin
    Result.Privileges{$R-}[i]{$R+}.Luid := Privileges[i];
    Result.Privileges{$R-}[i]{$R+}.Attributes := Attribute;
  end;
end;

function NtxpAllocPrivileges2(Privileges: TArray<TPrivilege>): PTokenPrivileges;
var
  i: Integer;
begin
  Result := AllocMem(SizeOf(Integer) +
    Length(Privileges) * SizeOf(TLUIDAndAttributes));

  Result.PrivilegeCount := Length(Privileges);

  for i := 0 to High(Privileges) do
    Result.Privileges{$R-}[i]{$R+} := Privileges[i];
end;

function NtxpAllocPrivilegeSet(Privileges: TArray<TPrivilege>): PPrivilegeSet;
var
  i: Integer;
begin
  Result := AllocMem(SizeOf(Cardinal) + SizeOf(Cardinal) +
    SizeOf(TLuidAndAttributes) * Length(Privileges));

  Result.PrivilegeCount := Length(Privileges);
  Result.Control := 0;

  for i := 0 to High(Privileges) do
    Result.Privilege{$R-}[i]{$R+} := Privileges[i];
end;

function NtxpAllocGroups(Sids: TArray<ISid>; Attribute: Cardinal): PTokenGroups;
var
  i: Integer;
begin
  Result := AllocMem(SizeOf(Integer) + Length(Sids) * SizeOf(TSIDAndAttributes));

  Result.GroupCount := Length(Sids);

  for i := 0 to High(Sids) do
  begin
    Result.Groups{$R-}[i]{$R+}.Sid := Sids[i].Sid;
    Result.Groups{$R-}[i]{$R+}.Attributes := Attribute;
  end;
end;

function NtxpAllocGroups2(Groups: TArray<TGroup>): PTokenGroups;
var
  i: Integer;
begin
  Result := AllocMem(SizeOf(Integer) +
    Length(Groups) * SizeOf(TSIDAndAttributes));

  Result.GroupCount := Length(Groups);

  for i := 0 to High(Groups) do
  begin
    Result.Groups{$R-}[i]{$R+}.Sid := Groups[i].SecurityIdentifier.Sid;
    Result.Groups{$R-}[i]{$R+}.Attributes := Groups[i].Attributes;
  end;
end;

end.
