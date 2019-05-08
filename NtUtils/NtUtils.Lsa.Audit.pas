unit NtUtils.Lsa.Audit;

interface

uses
  Winapi.WinNt, Winapi.NtSecApi, NtUtils.Exceptions, NtUtils.Types;

type
  TNtxStatus = NtUtils.Exceptions.TNtxStatus;
  IPerUserAudit = NtUtils.Types.IPerUserAudit;

  TTokenPerUserAudit = class(TInterfacedObject, IPerUserAudit)
  protected
    AuditPolicySize: Integer;
    Data: PTokenAuditPolicy;
    function GetSubCatogory(Index: Integer): Byte;
    procedure SetSubCatogory(Index: Integer; Value: Byte);
  public
    constructor CreateCopy(Buffer: PTokenAuditPolicy; BufferSize: Integer);
    destructor Destroy; override;

    function RawBuffer: PTokenAuditPolicy;
    function RawBufferSize: Integer;
    procedure FreeRawBuffer(Buffer: PTokenAuditPolicy);

    function AssignToUser(Sid: PSid): TNtxStatus;

    function ContainsFlag(Index: Integer; Flag: Integer): Boolean;
    procedure SetFlag(Index: Integer; Flag: Integer; Enabled: Boolean);

    property SuccessInclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_SUCCESS_INCLUDE read ContainsFlag write SetFlag;
    property SuccessExclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_SUCCESS_EXCLUDE read ContainsFlag write SetFlag;
    property FailureInclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_FAILURE_INCLUDE read ContainsFlag write SetFlag;
    property FailureExclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_FAILURE_EXCLUDE read ContainsFlag write SetFlag;
  end;

  TPerUserAudit = class(TInterfacedObject, IPerUserAudit)
  private
    Count: Integer;
    Data: TAuditPolicyInformationDynArray;
  public
    class function CreateEmpty(out Self: IPerUserAudit): TNtxStatus; static;
    class function CreateLoadForUser(Sid: PSid;
      out Self: IPerUserAudit): TNtxStatus; static;

    function RawBuffer: PTokenAuditPolicy;
    function RawBufferSize: Integer;
    procedure FreeRawBuffer(Buffer: PTokenAuditPolicy);

    function AssignToUser(Sid: PSid): TNtxStatus;

    function ContainsFlag(Index: Integer; Flag: Integer): Boolean;
    procedure SetFlag(Index: Integer; Flag: Integer; Enabled: Boolean);

    property SuccessInclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_SUCCESS_INCLUDE read ContainsFlag write SetFlag;
    property SuccessExclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_SUCCESS_EXCLUDE read ContainsFlag write SetFlag;
    property FailureInclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_FAILURE_INCLUDE read ContainsFlag write SetFlag;
    property FailureExclude[SubCatogiry: Integer]: Boolean index PER_USER_AUDIT_FAILURE_EXCLUDE read ContainsFlag write SetFlag;
  end;

  TGuidDynArray = Winapi.NtSecApi.TGuidDynArray;
  TGuidDynArray2 = array of TGuidDynArray;

// LsarEnumerateAuditCategories & LsarEnumerateAuditSubCategories
function LsaxEnumerateAuditCategiries(out Categories: TGuidDynArray;
  out SubCategories: TGuidDynArray2): TNtxStatus;

// LsarEnumerateAuditSubCategories
function LsaxEnumerateAuditSubCategiries(out SubCategories: TGuidDynArray):
  TNtxStatus;

// LsarLookupAuditCategoryName
function LsaxLookupAuditCategoryName(const Category: TGuid): String;

// LsarLookupAuditSubCategoryName
function LsaxLookupAuditSubCategoryName(const SubCategory: TGuid): String;

implementation

uses
  Ntapi.ntstatus, NtUtils.ApiExtension, DelphiUtils.Strings, System.SysUtils;

{ TTokenPerUserAudit }

function TTokenPerUserAudit.AssignToUser(Sid: PSid): TNtxStatus;
var
  i: Integer;
  SubCategories: TGuidDynArray;
  Policies: TAuditPolicyInformationDynArray;
begin
  Result := LsaxEnumerateAuditSubCategiries(SubCategories);

  if not Result.IsSuccess then
    Exit;

  SetLength(Policies, Length(SubCategories));

  if Length(Policies) > AuditPolicySize shl 1 then
  begin
    // The amoun of audit subcategories on the system should always
    // correlate with the amount of entries in TokenAuditPolicy
    Result.Status := STATUS_INFO_LENGTH_MISMATCH;
    Result.Location := '[Assertion]';
    Exit;
  end;

  for i := 0 to High(SubCategories) do
  begin
    Policies[i].AuditSubCategoryGuid := SubCategories[i];
    Policies[i].AuditingInformation := GetSubCatogory(i);

    // Explicitly convert PER_USER_POLICY_UNCHANGED to PER_USER_AUDIT_NONE
    if Policies[i].AuditingInformation = PER_USER_POLICY_UNCHANGED then
      Policies[i].AuditingInformation := PER_USER_AUDIT_NONE;
  end;

  Result := TNtxStatus.FromWin32(AuditSetPerUserPolicy(Sid, Policies,
    Length(Policies)), 'LsarSetAuditPolicy');
end;

function TTokenPerUserAudit.ContainsFlag(Index: Integer; Flag: Integer): Boolean;
begin
  Result := Contains(GetSubCatogory(Index), Flag);
end;

constructor TTokenPerUserAudit.CreateCopy(Buffer: PTokenAuditPolicy;
  BufferSize: Integer);
var
  i: Integer;
begin
  Self.AuditPolicySize := BufferSize;

  Self.Data := AllocMem(AuditPolicySize);
  for i := 0 to AuditPolicySize - 1 do
    Self.Data.PerUserPolicy[i] := Buffer.PerUserPolicy[i];
end;

destructor TTokenPerUserAudit.Destroy;
begin
  FreeMem(Data);
  Data := nil;
  inherited;
end;

procedure TTokenPerUserAudit.FreeRawBuffer(Buffer: PTokenAuditPolicy);
begin
  ; // We own the buffer, no need to free it
end;

function TTokenPerUserAudit.GetSubCatogory(Index: Integer): Byte;
begin
  if (Index < 0) or (Index > AuditPolicySize shl 1) then
    Exit(0);

  // Each bytes stores policies for two subcategories, extract the byte
  Result := Data.PerUserPolicy[Index shr 1];

  // Extract the required half of it
  if Index and 1 = 0 then
    Result := Result and $0F
  else
    Result := Result shr 4;
end;

function TTokenPerUserAudit.RawBuffer: PTokenAuditPolicy;
begin
  Result := Data;
end;

function TTokenPerUserAudit.RawBufferSize: Integer;
begin
  Result := AuditPolicySize;
end;

procedure TTokenPerUserAudit.SetFlag(Index: Integer; Flag: Integer;
  Enabled: Boolean);
begin
  if Enabled then
    SetSubCatogory(Index, GetSubCatogory(Index) or Flag)
  else
    SetSubCatogory(Index, GetSubCatogory(Index) and not Flag);
end;

procedure TTokenPerUserAudit.SetSubCatogory(Index: Integer; Value: Byte);
var
  PolicyByte: Byte;
begin
  if (Index < 0) or (Index > AuditPolicySize shl 1) then
    Exit;

  // We need only half a byte
  Value := Value and $0F;

  // Since each byte stores policies for two subcategories we should modify
  // only half of the byte preserving another half unchanged.

  PolicyByte := Data.PerUserPolicy[Index shr 1];

  if Index and 1 = 0 then
  begin
    PolicyByte := PolicyByte and $F0; // one half
    PolicyByte := PolicyByte or Value;
  end
  else
  begin
    Value := Value shl 4;
    PolicyByte := PolicyByte and $0F; // another half
    PolicyByte := PolicyByte or Value;
  end;

  Data.PerUserPolicy[Index shr 1] := PolicyByte;
end;

{ TPerUserAudit }

function TPerUserAudit.AssignToUser(Sid: PSid): TNtxStatus;
var
  i: Integer;
begin
  // Although on read PER_USER_POLICY_UNCHANGED means that the audit is
  // disabled, we need to explicitly convert it to PER_USER_AUDIT_NONE on write.

  for i := 0 to Count - 1 do
    if Data[i].AuditingInformation = PER_USER_POLICY_UNCHANGED then
      Data[i].AuditingInformation := PER_USER_AUDIT_NONE;

  Result := TNtxStatus.FromWin32(AuditSetPerUserPolicy(Sid, Data, Count),
    'LsarSetAuditPolicy');
end;

function TPerUserAudit.ContainsFlag(Index, Flag: Integer): Boolean;
begin
  if (Index < 0) or (Index >= Count) then
    Exit(False);

  Result := Contains(Data[Index].AuditingInformation, Flag);
end;

class function TPerUserAudit.CreateEmpty(out Self: IPerUserAudit): TNtxStatus;
var
  Obj: TPerUserAudit;
  SubCategories: TGuidDynArray;
  i: Integer;
begin
  Self := nil;
  Result := LsaxEnumerateAuditSubCategiries(SubCategories);

  if not Result.IsSuccess then
    Exit;

  Obj := TPerUserAudit.Create;

  Obj.Count := Length(SubCategories);
  SetLength(Obj.Data, Obj.Count);

  for i := 0 to High(SubCategories) do
  begin
    Obj.Data[i].AuditSubCategoryGuid := SubCategories[i];
    Obj.Data[i].AuditingInformation := PER_USER_POLICY_UNCHANGED;
  end;

  Self := Obj;
end;

class function TPerUserAudit.CreateLoadForUser(Sid: PSid;
  out Self: IPerUserAudit): TNtxStatus;
var
  Obj: TPerUserAudit;
  SubCategories: TGuidDynArray;
  Buffer: PAuditPolicyInformationArray;
  i: Integer;
begin
  Self := nil;
  Result := LsaxEnumerateAuditSubCategiries(SubCategories);

  if not Result.IsSuccess then
    Exit;

  Result := TNtxStatus.FromWin32(AuditQueryPerUserPolicy(Sid, SubCategories,
    Length(SubCategories), Buffer), 'LsarQueryAuditPolicy');

  if not Result.IsSuccess then
    Exit;

  Obj := TPerUserAudit.Create;
  Self := Obj;

  Obj.Count := Length(SubCategories);
  SetLength(Obj.Data, Obj.Count);

  for i := 0 to Obj.Count - 1 do
    Obj.Data[i] := Buffer[i];

  AuditFree(Buffer);
end;

procedure TPerUserAudit.FreeRawBuffer(Buffer: PTokenAuditPolicy);
begin
  FreeMem(Buffer);
end;

function TPerUserAudit.RawBuffer: PTokenAuditPolicy;
var
  i: Integer;
begin
  Result := AllocMem(RawBufferSize);

  // TokenAuditPolicy stores policies for two subcategories in each byte

  for i := 0 to Count - 1 do
    if i and 1 = 0 then
      Result.PerUserPolicy[i shr 1] := Result.PerUserPolicy[i shr 1] or
        Byte(Data[i].AuditingInformation and $0F)
    else
      Result.PerUserPolicy[i shr 1] := Result.PerUserPolicy[i shr 1] or
        (Byte(Data[i].AuditingInformation and $0F) shl 4);
end;

function TPerUserAudit.RawBufferSize: Integer;
begin
  // In accordance with Winapi's definition of TOKEN_AUDIT_POLICY
  Result := (Count shr 1) + 1;
end;

procedure TPerUserAudit.SetFlag(Index, Flag: Integer; Enabled: Boolean);
var
  PolicyByte: Byte;
begin
  if Index >= Count then
    Exit;

  // Replace PER_USER_AUDIT_NONE with PER_USER_POLICY_UNCHANGED
  // which means the same until we assign the policy to a user
  PolicyByte := Data[Index].AuditingInformation and $0F;

  if Enabled then
    Data[Index].AuditingInformation := PolicyByte or Flag
  else
    Data[Index].AuditingInformation := PolicyByte and not Flag;
end;

{ Functions }

function LsaxEnumerateAuditCategiries(out Categories: TGuidDynArray;
  out SubCategories: TGuidDynArray2): TNtxStatus;
var
  Guids, SubGuids: PGuidArray;
  Count, SubCount: Cardinal;
  Ind, SubInd: Integer;
begin
  Result.Status := STATUS_SUCCESS;

  SetLength(Categories, 0);
  SetLength(SubCategories, 0, 0);

  // Query categories

  Result.Location := 'LsarEnumerateAuditCategories';
  if not AuditEnumerateCategories(Guids, Count) then
  begin
    Result.Status := RtlxGetLastNtStatus;
    Exit;
  end;

  SetLength(Categories, Count);
  SetLength(SubCategories, Count, 0);

  // Go through all categories
  for Ind := 0 to High(Categories) do
  begin
    Categories[Ind] := Guids[Ind];

    // Query subcategories of this category

    Result.Location := 'LsarEnumerateAuditSubCategories';
    if not AuditEnumerateSubCategories(Guids[Ind], False, SubGuids,
      SubCount) then
    begin
      Result.Status := RtlxGetLastNtStatus;
      Exit;
    end;

    SetLength(SubCategories[Ind], SubCount);

    // Go through all subcategories
    for SubInd := 0 to High(SubCategories[Ind]) do
      SubCategories[Ind, SubInd] := SubGuids[SubInd];

    AuditFree(SubGuids);
  end;

  AuditFree(Guids);
end;

function LsaxEnumerateAuditSubCategiries(out SubCategories: TGuidDynArray):
  TNtxStatus;
var
  Buffer: PGuidArray;
  Count, i: Integer;
begin
  Result.Status := STATUS_SUCCESS;
  SetLength(SubCategories, 0);

  Result.Location := 'LsarEnumerateAuditSubCategories';
  if not AuditEnumerateSubCategories(nil, True, Buffer, Count) then
  begin
    Result.Status := RtlxGetLastNtStatus;
    Exit;
  end;

  SetLength(SubCategories, Count);

  for i := 0 to Count - 1 do
    SubCategories[i] := Buffer[i];

  AuditFree(Buffer);
end;

function LsaxLookupAuditCategoryName(const Category: TGuid): String;
var
  Buffer: PWideChar;
begin
  if AuditLookupCategoryNameW(Category, Buffer) then
  begin
    Result := String(Buffer);
    AuditFree(Buffer);
  end
  else
    Result := GUIDToString(Category);
end;

function LsaxLookupAuditSubCategoryName(const SubCategory: TGuid): String;
var
  Buffer: PWideChar;
begin
  if AuditLookupSubCategoryNameW(SubCategory, Buffer) then
  begin
    Result := String(Buffer);
    AuditFree(Buffer);
  end
  else
    Result := GUIDToString(SubCategory);
end;

end.
