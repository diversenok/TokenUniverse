unit TU.Access;

interface

uses
  Ntapi.WinNt, NtUtils, NtUiLib.AutoCompletion.Namespace,
  DelphiApi.Reflection;

type
  TCidType = (
    ctInvalid,
    ctProcess,
    ctProcessToken,
    ctProcessDebugObject,
    ctThread,
    ctThreadToken
  );

  TSidType = (
    stInvalid,
    stLsaAccount,
    stSamDomain,
    stSamAlias,
    stSamUser,
    stSamGroup
  );

// Determine maximum access we can get to a named (namespace) object
function TuGetMaximumAccessNamedObject(
  const Entry: TNamespaceEntry
): TAccessMask;

// Determine maximum access we can get to a client ID-defined object
function TuGetMaximumAccessCidObject(
  Cid: NativeUInt;
  CidType: TCidType
): TAccessMask;

// Get TypeInfo of an access mask corresponding to a CID type
[Result: MayReturnNil]
function TuCidAccessMaskType(
  CidType: TCidType
): Pointer;

// Get kerenel type name of corresponding to a CID type
[Result: MayReturnNil]
function TuCidTypeName(
  CidType: TCidType
): String;

// Determine maximum access we can get to a SID-defined object
function TuGetMaximumAccessSidObject(
  const Sid: ISid;
  SidType: TSidType
): TAccessMask;

// Get generic access mapping for a SID-defined objects
function TuGetGenericMappingSid(
  SidType: TSidType
): TGenericMapping;

// Get TypeInfo of an access mask corresponding to a CID type
[Result: MayReturnNil]
function TuSidAccessMaskType(
  SidType: TSidType
): Pointer;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntioapi, Ntapi.ntregapi, Ntapi.ntobapi, Ntapi.nttmapi,
  Ntapi.ntmmapi, Ntapi.ntexapi, Ntapi.ntpsapi, Ntapi.ntdbg, Ntapi.ntseapi,
  Ntapi.ntlsa, Ntapi.ntsam, Ntapi.Versions,
  NtUtils.Objects, NtUtils.Objects.Namespace, NtUtils.Registry,
  NtUtils.Objects.Snapshots, NtUtils.Files, NtUtils.Files.Open,
  NtUtils.Files.Operations, NtUtils.Sections, NtUtils.Processes,
  NtUtils.Jobs, NtUtils.Synchronization, NtUtils.Memory, NtUtils.Transactions,
  NtUtils.Threads, NtUtils.Tokens, NtUtils.Debug, NtUtils.Lsa, NtUtils.Sam,
  NtUtils.Security.Sid;

function TrimLastBackslash(
  const Path: String
): String;
begin
  if (Length(Path) > 0) and (Path[High(Path)] = '\') then
    Result := Copy(Path, 1, Length(Path) - 1)
  else
    Result := Path;
end;

function TuMakeNamespaceOpener(
  const FullPath: String;
  KnownType: TNamespaceObjectType
): TObjectOpener;
var
  TrimmedPath: String;
begin
  if FullPath <> '\' then
    TrimmedPath := TrimLastBackslash(FullPath)
  else
    TrimmedPath := FullPath;

  Result := function (
      out hxObject: IHandle;
      AccessMask: TAccessMask
    ): TNtxStatus
    var
      Parameters: IFileParameters;
    begin
      case KnownType of
        otSymlink:
          Result := NtxOpenSymlink(hxObject, AccessMask, TrimmedPath);

        otDirectory:
          Result := NtxOpenDirectory(hxObject, AccessMask, TrimmedPath);

        otDevice, otFileDirectory, otFile, otNamedPipe:
        begin
          Parameters := FileParameters
            .UseFileName(FullPath)
            .UseAccess(AccessMask)
            .UseSyncMode(fsAsynchronous)
            .UseOptions(FILE_COMPLETE_IF_OPLOCKED or FILE_OPEN_NO_RECALL)
          ;

          // Prefer opening reparse points but let the caller overwrite this
          // behavior by using names ending with "\"
          if (FullPath <> '') and (FullPath[High(FullPath)] <> '\') then
            Parameters := Parameters.UseOptions(Parameters.Options or
              FILE_OPEN_REPARSE_POINT);

          // Don't use backup intent with pipes because it causes the maximum
          // allowed check to fail, forcing us to check bits one by one,
          // introcuding self-inflicted race conditions that randomly fail some
          // of them with STATUS_PIPE_NOT_AVAILABLE.
          if KnownType <> otNamedPipe then
            Parameters := Parameters.UseOptions(Parameters.Options or
              FILE_OPEN_FOR_BACKUP_INTENT);

          Result := NtxOpenFile(hxObject, Parameters);
        end;

        otRegistryKey:
        begin
          // Try the possible maximum with backup/restore privileges
          Result := NtxOpenKey(hxObject, FullPath, AccessMask,
            REG_OPTION_BACKUP_RESTORE);

          // Maybe we got access denied because we don't have the privileges?
          if Result.Status = STATUS_ACCESS_DENIED then
            Result := NtxOpenKey(hxObject, FullPath, AccessMask);
        end;

        otSection:
          Result := NtxOpenSection(hxObject, AccessMask, TrimmedPath);

        otEvent:
          Result := NtxOpenEvent(hxObject, AccessMask, TrimmedPath);

        otSemaphore:
          Result := NtxOpenSemaphore(hxObject, AccessMask, TrimmedPath);

        otMutex:
          Result := NtxOpenMutant(hxObject, AccessMask, TrimmedPath);

        otTimer:
          Result := NtxOpenTimer(hxObject, AccessMask, TrimmedPath);

        otJob:
          Result := NtxOpenJob(hxObject, AccessMask, TrimmedPath);

        otSession:
          Result := NtxOpenSession(hxObject, AccessMask, TrimmedPath);

        otKeyedEvent:
          Result := NtxOpenKeyedEvent(hxObject, AccessMask, TrimmedPath);

        otIoCompletion:
          Result := NtxOpenIoCompletion(hxObject, AccessMask, TrimmedPath);

        otPartition:
          Result := NtxOpenPartition(hxObject, AccessMask, TrimmedPath);

        otRegistryTransaction:
          Result := NtxOpenRegistryTransaction(hxObject, AccessMask,
            TrimmedPath);
      else
        Result.Location := 'TuMakeNamespaceOpener';
        Result.Status := STATUS_INVALID_PARAMETER;
      end;
    end;
end;

function TuGetMaximumAccessNamedObject;
var
  ObjectInfo: TObjectTypeInfo;
  Status: TNtxStatus;
begin
  Status := RtlxFindKernelType(Entry.TypeName, ObjectInfo);

  if not Status.IsSuccess then
    Exit(0);

  Status := RtlxComputeMaximumAccess(
    Result,
    TuMakeNamespaceOpener(Entry.FullPath, Entry.KnownType),
    True,
    ObjectInfo.Other.ValidAccessMask,
    ObjectInfo.Other.GenericMapping.GenericRead
  );

  if not Status.IsSuccess then
    Exit(0);
end;

function TuMakeCidOpener(
  Cid: NativeUInt;
  KnownType: TCidType
): TObjectOpener;
begin
  Result := function (
      out hxObject: IHandle;
      AccessMask: TAccessMask
    ): TNtxStatus
    begin
      case KnownType of
        ctProcess:
          Result := NtxOpenProcess(hxObject, Cid, AccessMask);

        ctProcessToken:
          Result := NtxOpenProcessTokenById(hxObject, Cid, AccessMask);

        ctThread:
          Result := NtxOpenThread(hxObject, Cid, AccessMask);

        ctThreadToken:
          Result := NtxOpenThreadTokenById(hxObject, Cid, AccessMask);
      else
        Result.Location := 'TuMakeCidOpener';
        Result.Status := STATUS_INVALID_PARAMETER;
      end;
    end;
end;

function TuGetMaximumAccessDebugObject(
  out MaximumAccess: TAccessMask;
  PID: TProcessId
): TNtxStatus;
var
  hxProcess, hxDebugObj: IHandle;
  Info: TObjectBasicInformation;
begin
  Result := NtxOpenProcess(hxProcess, PID, PROCESS_QUERY_INFORMATION);

  if not Result.IsSuccess then
    Exit;

  Result := NtxOpenDebugObjectProcess(hxDebugObj, hxProcess.Handle);

  if not Result.IsSuccess then
    Exit;

  Result := NtxObject.Query(hxDebugObj.Handle, ObjectBasicInformation, Info);

  if not Result.IsSuccess then
    Exit;

  MaximumAccess := Info.GrantedAccess;
end;

function TuGetMaximumAccessCidObject;
var
  Status: TNtxStatus;
  TypeName: String;
  ObjectInfo: TObjectTypeInfo;
begin
  case CidType of
    ctProcessDebugObject:
    begin
      // Opening debug object doesn't allow specifiying access
      if not TuGetMaximumAccessDebugObject(Result, Cid).IsSuccess then
        Result := 0;

      Exit;
    end;

    ctProcess:      TypeName := 'Process';
    ctThread:       TypeName := 'Thread';
    ctThreadToken,
    ctProcessToken: TypeName := 'Token';
  end;

  Status := RtlxFindKernelType(TypeName, ObjectInfo);

  if not Status.IsSuccess then
    Exit(0);

  Status := RtlxComputeMaximumAccess(
    Result,
    TuMakeCidOpener(Cid, CidType),
    True,
    ObjectInfo.Other.ValidAccessMask,
    ObjectInfo.Other.GenericMapping.GenericRead
  );

  if not Status.IsSuccess then
    Exit(0);
end;

function TuCidAccessMaskType;
begin
  case CidType of
    ctProcess:            Result := TypeInfo(TProcessAccessMask);
    ctThread:             Result := TypeInfo(TThreadAccessMask);
    ctProcessToken,
    ctThreadToken:        Result := TypeInfo(TTokenAccessMask);
    ctProcessDebugObject: Result := TypeInfo(TDebugObjectAccessMask);
  else
    Result := nil;
  end;
end;

function TuCidTypeName;
begin
  case CidType of
    ctProcess:            Result := 'Process';
    ctThread:             Result := 'Thread';
    ctProcessDebugObject: Result := 'DebugObject';
    ctProcessToken,
    ctThreadToken:        Result := 'Token';
  else
    Result := '';
  end;
end;

function TuMakeSidOpener(
  const Sid: ISid;
  KnownType: TSidType
): TObjectOpener;
begin
  Result := function (
      out hxObject: IHandle;
      AccessMask: TAccessMask
    ): TNtxStatus
    begin
      case KnownType of
        stLsaAccount:
          Result := LsaxOpenAccount(hxObject, Sid, AccessMask);

        stSamDomain:
          Result := SamxOpenDomain(hxObject, Sid, AccessMask);

        stSamAlias:
          Result := SamxOpenAliasBySid(hxObject, Sid, AccessMask);

        stSamUser:
          Result := SamxOpenUserBySid(hxObject, Sid, AccessMask);

        stSamGroup:
          Result := SamxOpenGroupBySid(hxObject, Sid, AccessMask);
      else
        Result.Location := 'TuMakeSidOpener';
        Result.Status := STATUS_INVALID_PARAMETER;
      end;
    end;
end;

function TuGetValidAccessMaskSid(
  SidType: TSidType
): TAccessMask;
begin
  case SidType of
    stLsaAccount: Result := ACCOUNT_ALL_ACCESS;
    stSamDomain:  Result := DOMAIN_ALL_ACCESS;
    stSamAlias:   Result := ALIAS_ALL_ACCESS;
    stSamUser:    Result := USER_ALL_ACCESS;
    stSamGroup:   Result := GROUP_ALL_ACCESS;
  else
    Result := 0;
  end;
end;

function TuGetGenericMappingSid(
  SidType: TSidType
): TGenericMapping;
begin
  case SidType of
    stLsaAccount:
    begin
      Result.GenericRead := ACCOUNT_READ;
      Result.GenericWrite := ACCOUNT_WRITE;
      Result.GenericExecute := ACCOUNT_EXECUTE;
      Result.GenericAll := ACCOUNT_ALL_ACCESS;
    end;

    stSamDomain:
    begin
      Result.GenericRead := DOMAIN_READ;
      Result.GenericWrite := DOMAIN_WRITE;
      Result.GenericExecute := DOMAIN_EXECUTE;
      Result.GenericAll := DOMAIN_ALL_ACCESS;
    end;

    stSamAlias:
    begin
      Result.GenericRead := ALIAS_READ;
      Result.GenericWrite := ALIAS_WRITE;
      Result.GenericExecute := ALIAS_EXECUTE;
      Result.GenericAll := ALIAS_ALL_ACCESS;
    end;

    stSamUser:
    begin
      Result.GenericRead := USER_READ;
      Result.GenericWrite := USER_WRITE;
      Result.GenericExecute := USER_EXECUTE;
      Result.GenericAll := USER_ALL_ACCESS;
    end;

    stSamGroup:
    begin
      Result.GenericRead := GROUP_READ;
      Result.GenericWrite := GROUP_WRITE;
      Result.GenericExecute := GROUP_EXECUTE;
      Result.GenericAll := GROUP_ALL_ACCESS;
    end;
  else
    Result := Default(TGenericMapping);
  end;
end;

function TuSidAccessMaskType;
begin
  case SidType of
    stLsaAccount: Result := TypeInfo(TLsaAccountAccessMask);
    stSamDomain:  Result := TypeInfo(TDomainAccessMask);
    stSamAlias:   Result := TypeInfo(TAliasAccessMask);
    stSamUser:    Result := TypeInfo(TUserAccessMask);
    stSamGroup:   Result := TypeInfo(TGroupAccessMask);
  else
    Result := nil;
  end;
end;

function TuGetMaximumAccessSidObject;
var
  Status: TNtxStatus;
begin
  Status := RtlxComputeMaximumAccess(
    Result,
    TuMakeSidOpener(Sid, SidType),
    False,
    TuGetValidAccessMaskSid(SidType),
    TuGetGenericMappingSid(SidType).GenericRead
  );
end;

end.
