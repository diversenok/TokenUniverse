unit TU.Access;

interface

uses
  Ntapi.WinNt, NtUtils, NtUiLib.AutoCompletion.Namespace,
  DelphiApi.Reflection, NtUiCommon.Prototypes;

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

  TSingletonType = (
    ltInvalid,
    ltLsaPolicy,
    ltSamServer,
    ltScmDatabase,
    ltCurrentWinSta,
    ltCurrentDesktop
  );

  TAccessContext = record
    Security: TNtUiLibSecurityContext;
    MaximumAccess: TAccessMask;
  end;

// Determine maximum access we can get to a named (namespace) object
function TuGetAccessNamedObject(
  const Entry: TNamespaceEntry
): TAccessContext;

// Determine maximum access we can get to a client ID-defined object
function TuGetAccessCidObject(
  Cid: NativeUInt;
  CidType: TCidType
): TAccessContext;

// Determine maximum access we can get to a SID-defined object
function TuGetAccessSidObject(
  const Sid: ISid;
  SidType: TSidType
): TAccessContext;

// Determine maximum access we can get to an SCM service object
function TuGetAccessServiceObject(
  const Name: String
): TAccessContext;

// Enumerate all service names for suggestions
function TuCollectServiceNames: TArray<String>;

// Determine maximum access to a singleton object
function TuGetAccessSingletonObject(
  const ObjectType: TSingletonType
): TAccessContext;

implementation

uses
  Ntapi.ntstatus, Ntapi.ntioapi, Ntapi.ntregapi, Ntapi.ntobapi, Ntapi.nttmapi,
  Ntapi.ntmmapi, Ntapi.ntexapi, Ntapi.ntpsapi, Ntapi.ntdbg, Ntapi.ntseapi,
  Ntapi.ntlsa, Ntapi.ntsam, Ntapi.Versions, Ntapi.WinSvc, Ntapi.WinUser,
  NtUtils.Objects, NtUtils.Objects.Namespace, NtUtils.Registry,
  NtUtils.Objects.Snapshots, NtUtils.Files, NtUtils.Files.Open,
  NtUtils.Files.Operations, NtUtils.Sections, NtUtils.Processes,
  NtUtils.Jobs, NtUtils.Synchronization, NtUtils.Memory, NtUtils.Transactions,
  NtUtils.Threads, NtUtils.Tokens, NtUtils.Debug, NtUtils.Lsa, NtUtils.Sam,
  NtUtils.Security.Sid, NtUtils.Svc, NtUtils.WinUser;

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
          // introducing self-inflicted race conditions that randomly fail some
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

function TuGetAccessNamedObject;
var
  ObjectInfo: TObjectTypeInfo;
begin
  Result := Default(TAccessContext);
  Result.Security.AccessMaskType := RtlxGetNamespaceAccessMaskType(Entry.KnownType);
  Result.Security.QueryFunction := NtxQuerySecurityObject;
  Result.Security.SetFunction := NtxSetSecurityObject;
  Result.Security.HandleProvider := TuMakeNamespaceOpener(Entry.FullPath,
    Entry.KnownType);

  if RtlxFindKernelType(Entry.TypeName, ObjectInfo).IsSuccess then
    Result.Security.GenericMapping := ObjectInfo.Native.GenericMapping
  else
    ObjectInfo.Native.ValidAccessMask := SPECIFIC_RIGHTS_ALL or STANDARD_RIGHTS_ALL;

  RtlxComputeMaximumAccess(
    Result.MaximumAccess,
    Result.Security.HandleProvider,
    True,
    ObjectInfo.Native.ValidAccessMask,
    ObjectInfo.Native.GenericMapping.GenericRead
  );
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
    var
      hxProcess: IHandle;
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

        ctProcessDebugObject:
        begin
          Result := NtxOpenProcess(hxProcess, Cid, PROCESS_QUERY_INFORMATION);

          if not Result.IsSuccess then
            Exit;

          Result := NtxOpenDebugObjectProcess(hxObject, hxProcess);
        end
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

  Result := NtxOpenDebugObjectProcess(hxDebugObj, hxProcess);

  if not Result.IsSuccess then
    Exit;

  Result := NtxObject.Query(hxDebugObj, ObjectBasicInformation, Info);

  if not Result.IsSuccess then
    Exit;

  MaximumAccess := Info.GrantedAccess;
end;

[Result: MayReturnNil]
function TuCidAccessMaskType(
  CidType: TCidType
): Pointer;
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

function TuGetAccessCidObject;
var
  TypeName: String;
  ObjectInfo: TObjectTypeInfo;
begin
  Result := Default(TAccessContext);
  Result.Security.AccessMaskType := TuCidAccessMaskType(CidType);
  Result.Security.QueryFunction := NtxQuerySecurityObject;
  Result.Security.SetFunction := NtxSetSecurityObject;
  Result.Security.HandleProvider := TuMakeCidOpener(Cid, CidType);

  case CidType of
    ctProcess:            TypeName := 'Process';
    ctThread:             TypeName := 'Thread';
    ctProcessDebugObject: TypeName := 'DebugObject';
    ctProcessToken,
    ctThreadToken:        TypeName := 'Token';
  end;

  if RtlxFindKernelType(TypeName, ObjectInfo).IsSuccess then
    Result.Security.GenericMapping := ObjectInfo.Native.GenericMapping
  else
    ObjectInfo.Native.ValidAccessMask := SPECIFIC_RIGHTS_ALL or STANDARD_RIGHTS_ALL;

  if CidType = ctProcessDebugObject then
    // Opening debug object doesn't allow specifying access
    TuGetMaximumAccessDebugObject(Result.MaximumAccess, Cid)
  else
    RtlxComputeMaximumAccess(
      Result.MaximumAccess,
      Result.Security.HandleProvider,
      True,
      ObjectInfo.Native.ValidAccessMask,
      ObjectInfo.Native.GenericMapping.GenericRead
    );
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

[Result: MayReturnNil]
function TuSidAccessMaskType(
  SidType: TSidType
): Pointer;
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

function TuGetAccessSidObject;
begin
  Result := Default(TAccessContext);
  Result.Security.AccessMaskType := TuSidAccessMaskType(SidType);;
  Result.Security.GenericMapping := TuGetGenericMappingSid(SidType);
  Result.Security.HandleProvider := TuMakeSidOpener(Sid, SidType);

  if SidType = stLsaAccount then
  begin
    Result.Security.QueryFunction := LsaxQuerySecurityObject;
    Result.Security.SetFunction := LsaxSetSecurityObject;
  end
  else
  begin
    Result.Security.QueryFunction := SamxQuerySecurityObject;
    Result.Security.SetFunction := SamxSetSecurityObject;
  end;

  RtlxComputeMaximumAccess(
    Result.MaximumAccess,
    Result.Security.HandleProvider,
    False,
    TuGetValidAccessMaskSid(SidType),
    Result.Security.GenericMapping.GenericRead
  );
end;

function TuMakeServiceOpener(
  const Name: String
): TObjectOpener;
begin
  Result := function (
    out hxObject: IHandle;
    DesiredAccess: TAccessMask
    ): TNtxStatus
    begin
      Result := ScmxOpenService(hxObject, Name, DesiredAccess);
    end;
end;

function TuServiceSecurityQueryMaskLookup(
  Info: TSecurityInformation
): TAccessMask;
begin
  Result := SecurityReadAccess(Info);

  // Workaround SCM requiting more rights to read trust information
  if BitTest(Info and PROCESS_TRUST_LABEL_SECURITY_INFORMATION) then
    Result := Result or ACCESS_SYSTEM_SECURITY;
end;

function TuGetAccessServiceObject;
begin
  Result := Default(TAccessContext);
  Result.Security.HandleProvider := TuMakeServiceOpener(Name);
  Result.Security.AccessMaskType := TypeInfo(TServiceAccessMask);
  Result.Security.QueryFunction := ScmxQuerySecurityObject;
  Result.Security.SetFunction := ScmxSetSecurityObject;
  Result.Security.GenericMapping.GenericRead := SERVICE_READ;
  Result.Security.GenericMapping.GenericWrite := SERVICE_WRITE;
  Result.Security.GenericMapping.GenericExecute := SERVICE_EXECUTE;
  Result.Security.GenericMapping.GenericAll := SERVICE_ALL_ACCESS;

  // SCM requires custom rights for viewing trust labels
  Result.Security.CustomQueryAccessLookup := TuServiceSecurityQueryMaskLookup;

  RtlxComputeMaximumAccess(
    Result.MaximumAccess,
    Result.Security.HandleProvider,
    False,
    SERVICE_ALL_ACCESS,
    Result.Security.GenericMapping.GenericRead
  );
end;

function TuCollectServiceNames;
var
  Services: TArray<TServiceEntry>;
  i: Integer;
begin
  if not ScmxEnumerateServices(Services).IsSuccess then
    Services := nil;

  SetLength(Result, Length(Services));

  for i := 0 to High(Services) do
    Result[i] := Services[i].ServiceName;
end;

function TuMakeSingletonOpener(
  const ObjectType: TSingletonType
): TObjectOpener;
begin
  Result := function (
    out hxObject: IHandle;
    DesiredAccess: TAccessMask
    ): TNtxStatus
    begin
      case ObjectType of
        ltLsaPolicy:   Result := LsaxOpenPolicy(hxObject, DesiredAccess);
        ltSamServer:   Result := SamxConnect(hxObject, DesiredAccess);
        ltScmDatabase: Result := ScmxConnect(hxObject, DesiredAccess);

        ltCurrentWinSta:
          Result := NtxDuplicateHandleLocal(UsrxCurrentWindowStation,
            hxObject, DesiredAccess, 0, 0);

        ltCurrentDesktop:
          Result := NtxDuplicateHandleLocal(UsrxCurrentDesktop, hxObject,
            DesiredAccess, 0, 0);
      else
        Result.Location := 'TuMakeSingletonOpener';
        Result.Status := STATUS_INVALID_PARAMETER;
      end;
    end;
end;

function TuGetAccessSingletonObject;
var
  Info: TObjectTypeInfo;
begin
  Result := Default(TAccessContext);
  Result.Security.HandleProvider := TuMakeSingletonOpener(ObjectType);

  case ObjectType of
    ltLsaPolicy:
    begin
      Result.Security.AccessMaskType := TypeInfo(TLsaPolicyAccessMask);
      Result.Security.QueryFunction := LsaxQuerySecurityObject;
      Result.Security.SetFunction := LsaxSetSecurityObject;
      Result.Security.GenericMapping.GenericRead := POLICY_READ;
      Result.Security.GenericMapping.GenericWrite := POLICY_WRITE;
      Result.Security.GenericMapping.GenericExecute := POLICY_EXECUTE;
      Result.Security.GenericMapping.GenericAll := POLICY_ALL_ACCESS_EX;
    end;

    ltSamServer:
    begin
      Result.Security.AccessMaskType := TypeInfo(TSamAccessMask);
      Result.Security.QueryFunction := SamxQuerySecurityObject;
      Result.Security.SetFunction := SamxSetSecurityObject;
      Result.Security.GenericMapping.GenericRead := SAM_SERVER_READ;
      Result.Security.GenericMapping.GenericWrite := SAM_SERVER_WRITE;
      Result.Security.GenericMapping.GenericExecute := SAM_SERVER_EXECUTE;
      Result.Security.GenericMapping.GenericAll := SAM_SERVER_ALL_ACCESS;
    end;

    ltScmDatabase:
    begin
      Result.Security.AccessMaskType := TypeInfo(TScmAccessMask);
      Result.Security.QueryFunction := ScmxQuerySecurityObject;
      Result.Security.SetFunction := ScmxSetSecurityObject;
      Result.Security.GenericMapping.GenericRead := SC_MANAGER_READ;
      Result.Security.GenericMapping.GenericWrite := SC_MANAGER_WRITE;
      Result.Security.GenericMapping.GenericExecute := SC_MANAGER_EXECUTE;
      Result.Security.GenericMapping.GenericAll := SC_MANAGER_ALL_ACCESS;

      // SCM requires custom rights for viewing trust labels
      Result.Security.CustomQueryAccessLookup :=
        TuServiceSecurityQueryMaskLookup;
    end;

    ltCurrentWinSta:
    begin
      Result.Security.AccessMaskType := TypeInfo(TWinStaAccessMask);
      Result.Security.QueryFunction := NtxQuerySecurityObject;
      Result.Security.SetFunction := NtxSetSecurityObject;

      if RtlxFindKernelType('WindowStation', Info).IsSuccess then
        Result.Security.GenericMapping := Info.Native.GenericMapping;
    end;

    ltCurrentDesktop:
    begin
      Result.Security.AccessMaskType := TypeInfo(TDesktopAccessMask);
      Result.Security.QueryFunction := NtxQuerySecurityObject;
      Result.Security.SetFunction := NtxSetSecurityObject;

      if RtlxFindKernelType('Desktop', Info).IsSuccess then
        Result.Security.GenericMapping := Info.Native.GenericMapping;
    end;
  else
    Exit;
  end;

  RtlxComputeMaximumAccess(
    Result.MaximumAccess,
    Result.Security.HandleProvider,
    False,
    Result.Security.GenericMapping.GenericAll,
    Result.Security.GenericMapping.GenericRead
  );
end;

end.
