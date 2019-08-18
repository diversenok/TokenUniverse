unit NtUtils.Access.Expected;

interface

uses
  Ntapi.ntpsapi, Ntapi.ntseapi, Winapi.ntlsa, Ntapi.ntsam, Winapi.Svc,
  NtUtils.Exceptions;

{ Process }

procedure RtlxComputeProcessQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TProcessInfoClass);

procedure RtlxComputeProcessSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TProcessInfoClass);

{ Thread }

procedure RtlxComputeThreadQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TThreadInfoClass);

procedure RtlxComputeThreadSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TThreadInfoClass);

{ Token }

procedure RtlxComputeTokenQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TTokenInformationClass);

procedure RtlxComputeTokenSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TTokenInformationClass);

{ LSA policy }

procedure RtlxComputePolicyQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TPolicyInformationClass);

procedure RtlxComputePolicySetAccess(var LastCall: TLastCallInfo;
  InfoClass: TPolicyInformationClass);

{ SAM domain }

procedure RtlxComputeDomainQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TDomainInformationClass);

procedure RtlxComputeDomainSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TDomainInformationClass);

{ SAM user }

procedure RtlxComputeUserQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TUserInformationClass);

procedure RtlxComputeUserSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TUserInformationClass);

{ Service }

procedure RtlxComputeServiceControlAccess(var LastCall: TLastCallInfo;
  Control: TServiceControl);

{ Section }

procedure RtlxComputeSectionFileAccess(var LastCall: TLastCallInfo;
  Win32Protect: Cardinal);

procedure RtlxComputeSectionMapAccess(var LastCall: TLastCallInfo;
  Win32Protect: Cardinal);

implementation

uses
  Ntapi.ntmmapi, Ntapi.ntioapi;

{ Process }

procedure RtlxComputeProcessQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TProcessInfoClass);
begin
  case InfoClass of
    ProcessBasicInformation, ProcessQuotaLimits, ProcessIoCounters,
    ProcessVmCounters, ProcessTimes, ProcessDefaultHardErrorMode,
    ProcessPooledUsageAndLimits, ProcessPriorityClass, ProcessHandleCount,
    ProcessPriorityBoost, ProcessSessionInformation, ProcessWow64Information,
    ProcessImageFileName, ProcessLUIDDeviceMapsEnabled, ProcessIoPriority,
    ProcessImageInformation, ProcessCycleTime, ProcessPagePriority,
    ProcessImageFileNameWin32, ProcessAffinityUpdateMode,
    ProcessMemoryAllocationMode, ProcessGroupInformation,
    ProcessConsoleHostProcess, ProcessWindowInformation:
      LastCall.Expects(PROCESS_QUERY_LIMITED_INFORMATION, objNtProcess);

    ProcessDebugPort, ProcessWorkingSetWatch, ProcessWx86Information,
    ProcessDeviceMap, ProcessBreakOnTermination, ProcessDebugObjectHandle,
    ProcessDebugFlags, ProcessHandleTracing, ProcessExecuteFlags,
    ProcessWorkingSetWatchEx, ProcessImageFileMapping:
      LastCall.Expects(PROCESS_QUERY_INFORMATION, objNtProcess);

    ProcessCookie:
      LastCall.Expects(PROCESS_VM_WRITE, objNtProcess);

    ProcessLdtInformation:
      LastCall.Expects(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
        objNtProcess);
  end;
end;

procedure RtlxComputeProcessSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TProcessInfoClass);
begin
  // Privileges
  case InfoClass of
    ProcessQuotaLimits:
      LastCall.ExpectedPrivilege := SE_INCREASE_QUOTA_PRIVILEGE;

    ProcessBasePriority, ProcessIoPriority:
      LastCall.ExpectedPrivilege := SE_INCREASE_BASE_PRIORITY_PRIVILEGE;

    ProcessExceptionPort, ProcessUserModeIOPL, ProcessWx86Information,
    ProcessSessionInformation:
      LastCall.ExpectedPrivilege := SE_TCB_PRIVILEGE;

    ProcessAccessToken:
      LastCall.ExpectedPrivilege := SE_ASSIGN_PRIMARY_TOKEN_PRIVILEGE;

    ProcessBreakOnTermination:
      LastCall.ExpectedPrivilege := SE_DEBUG_PRIVILEGE;
  end;

  // Access
  case InfoClass of
    ProcessBasePriority, ProcessRaisePriority, ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers, ProcessWorkingSetWatch, ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup, ProcessPriorityClass,
    ProcessWx86Information, ProcessAffinityMask, ProcessPriorityBoost,
    ProcessDeviceMap, ProcessForegroundInformation, ProcessBreakOnTermination,
    ProcessDebugFlags, ProcessHandleTracing, ProcessIoPriority,
    ProcessPagePriority, ProcessWorkingSetWatchEx, ProcessMemoryAllocationMode,
    ProcessTokenVirtualizationEnabled:
      LastCall.Expects(PROCESS_SET_INFORMATION, objNtProcess);

    ProcessSessionInformation:
      LastCall.Expects(PROCESS_SET_INFORMATION or PROCESS_SET_SESSIONID,
       objNtProcess);

    ProcessExceptionPort:
      LastCall.Expects(PROCESS_SUSPEND_RESUME, objNtProcess);

    ProcessQuotaLimits:
      LastCall.Expects(PROCESS_SET_QUOTA, objNtProcess);

    ProcessAccessToken:
      begin
        LastCall.Expects(PROCESS_SET_INFORMATION, objNtProcess);
        LastCall.Expects(TOKEN_ASSIGN_PRIMARY, objNtToken);
      end;

    ProcessLdtInformation, ProcessLdtSize:
      LastCall.Expects(PROCESS_SET_INFORMATION or PROCESS_VM_WRITE,
        objNtProcess);
  end;
end;

{ Thread }

procedure RtlxComputeThreadQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TThreadInfoClass);
begin
  case InfoClass of
    ThreadBasicInformation, ThreadTimes, ThreadAmILastThread,
    ThreadPriorityBoost, ThreadIsTerminated, ThreadIoPriority, ThreadCycleTime,
    ThreadPagePriority, ThreadGroupInformation, ThreadIdealProcessorEx:
      LastCall.Expects(THREAD_QUERY_LIMITED_INFORMATION, objNtThread);

    ThreadDescriptorTableEntry, ThreadQuerySetWin32StartAddress,
    ThreadPerformanceCount, ThreadIsIoPending, ThreadHideFromDebugger,
    ThreadBreakOnTermination, ThreadUmsInformation, ThreadCounterProfiling:
      LastCall.Expects(THREAD_QUERY_INFORMATION, objNtThread);

    ThreadLastSystemCall, ThreadWow64Context:
      LastCall.Expects(THREAD_GET_CONTEXT, objNtThread);

    ThreadTebInformation:
      LastCall.Expects(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT, objNtThread);
  end;
end;

procedure RtlxComputeThreadSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TThreadInfoClass);
begin
  // Privileges
  case InfoClass of
    ThreadBreakOnTermination:
      LastCall.ExpectedPrivilege := SE_DEBUG_PRIVILEGE;

    ThreadPriority, ThreadIoPriority:
      LastCall.ExpectedPrivilege := SE_INCREASE_BASE_PRIORITY_PRIVILEGE;
  end;

  // Access
  case InfoClass of
    ThreadPriority, ThreadBasePriority, ThreadAffinityMask, ThreadPriorityBoost,
    ThreadActualBasePriority:
      LastCall.Expects(THREAD_SET_LIMITED_INFORMATION, objNtThread);

    ThreadEnableAlignmentFaultFixup, ThreadZeroTlsCell,
    ThreadIdealProcessor, ThreadHideFromDebugger, ThreadBreakOnTermination,
    ThreadIoPriority, ThreadPagePriority, ThreadGroupInformation,
    ThreadCounterProfiling, ThreadIdealProcessorEx:
      LastCall.Expects(THREAD_SET_INFORMATION, objNtThread);

    ThreadWow64Context:
      LastCall.Expects(THREAD_SET_CONTEXT, objNtThread);

    ThreadImpersonationToken:
    begin
      LastCall.Expects(THREAD_SET_THREAD_TOKEN, objNtThread);
      LastCall.Expects(TOKEN_IMPERSONATE, objNtToken);
    end;
  end;
end;

{ Token }

procedure RtlxComputeTokenQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TTokenInformationClass);
begin
  // Privileges
  case InfoClass of
    TokenAuditPolicy:
      LastCall.ExpectedPrivilege := SE_SECURITY_PRIVILEGE;
  end;

  // Access
  case InfoClass of
    TokenSource:
      LastCall.Expects(TOKEN_QUERY_SOURCE, objNtToken);
  else
    LastCall.Expects(TOKEN_QUERY, objNtToken);
  end;
end;

procedure RtlxComputeTokenSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TTokenInformationClass);
begin
  // Privileges
  case InfoClass of
    TokenSessionId, TokenSessionReference, TokenAuditPolicy, TokenOrigin,
    TokenIntegrityLevel, TokenUIAccess, TokenMandatoryPolicy:
      LastCall.ExpectedPrivilege := SE_TCB_PRIVILEGE;

    TokenLinkedToken, TokenVirtualizationAllowed:
      LastCall.ExpectedPrivilege := SE_CREATE_TOKEN_PRIVILEGE;
  end;

  // Access
  case InfoClass of
    TokenSessionId:
      LastCall.Expects(TOKEN_ADJUST_DEFAULT or TOKEN_ADJUST_SESSIONID,
        objNtToken);

    TokenLinkedToken:
      LastCall.Expects(TOKEN_ADJUST_DEFAULT or TOKEN_QUERY, objNtToken);

  else
    LastCall.Expects(TOKEN_ADJUST_DEFAULT, objNtToken);
  end;
end;

{ Policy }

procedure RtlxComputePolicyQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TPolicyInformationClass);
begin
  // See [MS-LSAD] & LsapDbRequiredAccessQueryPolicy
  case InfoClass of
    PolicyAuditLogInformation, PolicyAuditEventsInformation,
    PolicyAuditFullQueryInformation:
      LastCall.Expects(POLICY_VIEW_AUDIT_INFORMATION, objLsaPolicy);

    PolicyPrimaryDomainInformation, PolicyAccountDomainInformation,
    PolicyLsaServerRoleInformation, PolicyReplicaSourceInformation,
    PolicyDefaultQuotaInformation, PolicyDnsDomainInformation,
    PolicyDnsDomainInformationInt, PolicyLocalAccountDomainInformation:
      LastCall.Expects(POLICY_VIEW_LOCAL_INFORMATION, objLsaPolicy);

    PolicyPdAccountInformation:
      LastCall.Expects(POLICY_GET_PRIVATE_INFORMATION, objLsaPolicy);
  end;
end;

procedure RtlxComputePolicySetAccess(var LastCall: TLastCallInfo;
  InfoClass: TPolicyInformationClass);
begin
  // See [MS-LSAD] & LsapDbRequiredAccessSetPolicy
  case InfoClass of
    PolicyPrimaryDomainInformation, PolicyAccountDomainInformation,
    PolicyDnsDomainInformation, PolicyDnsDomainInformationInt,
    PolicyLocalAccountDomainInformation:
      LastCall.Expects(POLICY_TRUST_ADMIN, objLsaPolicy);

    PolicyAuditLogInformation, PolicyAuditFullSetInformation:
      LastCall.Expects(POLICY_AUDIT_LOG_ADMIN, objLsaPolicy);

    PolicyAuditEventsInformation:
      LastCall.Expects(POLICY_SET_AUDIT_REQUIREMENTS, objLsaPolicy);

    PolicyLsaServerRoleInformation, PolicyReplicaSourceInformation:
      LastCall.Expects(POLICY_SERVER_ADMIN, objLsaPolicy);

    PolicyDefaultQuotaInformation:
      LastCall.Expects(POLICY_SET_DEFAULT_QUOTA_LIMITS, objLsaPolicy);
  end;
end;

{ Domain }

procedure RtlxComputeDomainQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TDomainInformationClass);
begin
  // See [MS-SAMR]
  case InfoClass of
    DomainGeneralInformation, DomainLogoffInformation, DomainOemInformation,
    DomainNameInformation, DomainReplicationInformation,
    DomainServerRoleInformation, DomainModifiedInformation,
    DomainStateInformation, DomainUasInformation, DomainModifiedInformation2:
      LastCall.Expects(DOMAIN_READ_OTHER_PARAMETERS, objSamDomain);

    DomainPasswordInformation, DomainLockoutInformation:
      LastCall.Expects(DOMAIN_READ_PASSWORD_PARAMETERS, objSamDomain);

    DomainGeneralInformation2:
      LastCall.Expects(DOMAIN_READ_PASSWORD_PARAMETERS or
        DOMAIN_READ_OTHER_PARAMETERS, objSamDomain);
  end;
end;

procedure RtlxComputeDomainSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TDomainInformationClass);
begin
  // See [MS-SAMR]
  case InfoClass of
    DomainPasswordInformation, DomainLockoutInformation:
      LastCall.Expects(DOMAIN_WRITE_PASSWORD_PARAMS, objSamDomain);

    DomainLogoffInformation, DomainOemInformation, DomainUasInformation:
      LastCall.Expects(DOMAIN_WRITE_OTHER_PARAMETERS, objSamDomain);

    DomainReplicationInformation, DomainServerRoleInformation,
    DomainStateInformation:
      LastCall.Expects(DOMAIN_ADMINISTER_SERVER, objSamDomain);
  end;
end;

{ User }

procedure RtlxComputeUserQueryAccess(var LastCall: TLastCallInfo;
  InfoClass: TUserInformationClass);
begin
  // See [MS-SAMR]
  case InfoClass of
    UserGeneralInformation, UserNameInformation, UserAccountNameInformation,
    UserFullNameInformation, UserPrimaryGroupInformation,
    UserAdminCommentInformation:
      LastCall.Expects(USER_READ_GENERAL, objSamUser);

    UserLogonHoursInformation, UserHomeInformation, UserScriptInformation,
    UserProfileInformation, UserWorkStationsInformation:
      LastCall.Expects(USER_READ_LOGON, objSamUser);

    UserControlInformation, UserExpiresInformation, UserInternal1Information,
    UserParametersInformation:
      LastCall.Expects(USER_READ_ACCOUNT, objSamUser);

    UserPreferencesInformation:
      LastCall.Expects(USER_READ_PREFERENCES or USER_READ_GENERAL, objSamUser);

    UserLogonInformation, UserAccountInformation:
      LastCall.Expects(USER_READ_GENERAL or USER_READ_PREFERENCES or
        USER_READ_LOGON or USER_READ_ACCOUNT, objSamUser);

    UserLogonUIInformation: ; // requires administrator and whatever access
  end;
end;

procedure RtlxComputeUserSetAccess(var LastCall: TLastCallInfo;
  InfoClass: TUserInformationClass);
begin
  // See [MS-SAMR]
  case InfoClass of
    UserLogonHoursInformation, UserNameInformation, UserAccountNameInformation,
    UserFullNameInformation, UserPrimaryGroupInformation, UserHomeInformation,
    UserScriptInformation, UserProfileInformation, UserAdminCommentInformation,
    UserWorkStationsInformation, UserControlInformation, UserExpiresInformation,
    UserParametersInformation:
      LastCall.Expects(USER_WRITE_ACCOUNT, objSamUser);

    UserPreferencesInformation:
      LastCall.Expects(USER_WRITE_PREFERENCES, objSamUser);

    UserSetPasswordInformation:
      LastCall.Expects(USER_FORCE_PASSWORD_CHANGE, objSamUser);
  end;
end;

procedure RtlxComputeServiceControlAccess(var LastCall: TLastCallInfo;
  Control: TServiceControl);
begin
  // MSDN
  case Control of
    ServiceControlPause, ServiceControlContinue,
    ServiceControlParamChange, ServiceControlNetbindAdd,
    ServiceControlNetbindRemove, ServiceControlNetbindEnable,
    ServiceControlNetbindDisable:
      LastCall.Expects(SERVICE_PAUSE_CONTINUE, objScmService);

    ServiceControlStop:
      LastCall.Expects(SERVICE_STOP, objScmService);

    ServiceControlInterrogate:
      LastCall.Expects(SERVICE_INTERROGATE, objScmService);
  else
    if (Cardinal(Control) >= 128) and (Cardinal(Control) < 255) then
      LastCall.Expects(SERVICE_USER_DEFINED_CONTROL, objScmService);
  end;
end;

procedure RtlxComputeSectionFileAccess(var LastCall: TLastCallInfo;
  Win32Protect: Cardinal);
begin
  case Win32Protect and $FF of
    PAGE_NOACCESS, PAGE_READONLY, PAGE_WRITECOPY:
      LastCall.Expects(FILE_READ_DATA, objIoFile);

    PAGE_READWRITE:
      LastCall.Expects(FILE_WRITE_DATA or FILE_READ_DATA, objIoFile);

    PAGE_EXECUTE:
      LastCall.Expects(FILE_EXECUTE, objIoFile);

    PAGE_EXECUTE_READ, PAGE_EXECUTE_WRITECOPY:
      LastCall.Expects(FILE_EXECUTE or FILE_READ_DATA, objIoFile);

    PAGE_EXECUTE_READWRITE:
      LastCall.Expects(FILE_EXECUTE or FILE_WRITE_DATA or FILE_READ_DATA,
        objIoFile);
  end;
end;

procedure RtlxComputeSectionMapAccess(var LastCall: TLastCallInfo;
  Win32Protect: Cardinal);
begin
  case Win32Protect and $FF of
    PAGE_NOACCESS, PAGE_READONLY, PAGE_WRITECOPY:
      LastCall.Expects(SECTION_MAP_READ, objNtSection);

    PAGE_READWRITE:
      LastCall.Expects(SECTION_MAP_WRITE, objNtSection);

    PAGE_EXECUTE:
      LastCall.Expects(SECTION_MAP_EXECUTE, objNtSection);

    PAGE_EXECUTE_READ, PAGE_EXECUTE_WRITECOPY:
      LastCall.Expects(SECTION_MAP_EXECUTE or SECTION_MAP_READ, objNtSection);

    PAGE_EXECUTE_READWRITE:
      LastCall.Expects(SECTION_MAP_EXECUTE or SECTION_MAP_WRITE, objNtSection);
  end;
end;

end.
