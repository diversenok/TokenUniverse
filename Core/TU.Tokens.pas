unit TU.Tokens;

interface

{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}
uses
  Ntapi.WinNt, Ntapi.ntseapi, Ntapi.NtSecApi, NtUtils.Objects, NtUtils,
  TU.Tokens.Types, TU.Tokens3;

type
  {-------------------  TToken object definition  ---------------------------}

  IToken = interface
    ['{A5F087C7-53FE-4C6F-B4E9-2AF5CD270117}']
    procedure SetCaption(const Value: String);
    function GetCaption: String;
    function GetHandle: IHandle;
    property Handle: IHandle read GetHandle;
    property Caption: String read GetCaption write SetCaption;
    function SendHandleToProcess(PID: NativeUInt): NativeUInt;
    function OpenLinkedToken(out Token: IToken): TNtxStatus;
  end;

  /// <summary>
  ///  Token Universe representation of an opend token handle.
  /// </summary>
  TToken = class(TInterfacedObject, IToken, IToken3)
  private
    procedure SetCaption(const Value: String);
    function GetCaption: String;
    function GetHandle: IHandle;
  protected
    hxToken: IHandle;
    FCaption: String;

    // Migration to the new IToken interface
    FTokenV3: IToken3;
    property TokenV3: IToken3 read FTokenV3 implements IToken3;
  public
    {--------------------  TToken public section ---------------------------}

    property Handle: IHandle read GetHandle;
    property Caption: String read GetCaption write SetCaption;
    function SendHandleToProcess(PID: NativeUInt): NativeUInt;

    /// <summary> Removes the thread impersonation token. </summary>
    class procedure RevertThreadToken(TID: NativeUInt);
  public

    {--------------------  TToken constructors  ----------------------------}

    /// <summary>
    ///  Registers in the factory and initializes cache.
    /// </summary>
    procedure AfterConstruction; override;

    /// <summary> General purpuse constructor. </summary>
    /// <exception> This constructor doesn't raise any exceptions. </exception>
    constructor Create(Handle: IHandle; Caption: String);

    /// <summary>
    ///  Create a TToken object using inherited handle.
    /// </summary>
    constructor CreateByHandle(Handle: THandle);

    /// <summary> Opens a token of current process. </summary>
    constructor CreateOpenCurrent(Access: TAccessMask = MAXIMUM_ALLOWED);

    /// <summary> Opens a token of a process. </summary>
    constructor CreateOpenProcess(PID: NativeUInt; ImageName: String;
      Access: TAccessMask = MAXIMUM_ALLOWED; Attributes: Cardinal = 0);

      /// <summary> Opens a token of a thread. </summary>
    constructor CreateOpenThread(TID: NativeUInt; ImageName: String;
      Access: TAccessMask = MAXIMUM_ALLOWED; Attributes: Cardinal = 0;
      Dummy: Integer = 0);

    constructor CreateOpenEffective(TID: NativeUInt; ImageName: String;
      ImpersonationLevel: TSecurityImpersonationLevel = SecurityImpersonation;
      Access: TAccessMask = MAXIMUM_ALLOWED;
      Attributes: Cardinal = 0; EffectiveOnly: Boolean = False);

    /// <summary> Duplicates a token. </summary>
    constructor CreateDuplicateToken(SrcToken: IToken; Access: TAccessMask;
      TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);

    /// <summary>
    ///  Duplicates a handle. The result references for the same kernel object.
    /// </summary>
    constructor CreateDuplicateHandle(SrcToken: IToken; Access: TAccessMask;
      SameAccess: Boolean; HandleAttributes: Cardinal = 0);

    /// <summary>
    ///  Queries a token of the specified Windows Terminal Session.
    /// </summary>
    constructor CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);

    /// <summary> Creates a restricted version of the token. </summary>
    constructor CreateRestricted(SrcToken: IToken; Flags: Cardinal;
      SIDsToDisabe, SIDsToRestrict: TArray<TGroup>;
      PrivilegesToDelete: TArray<TPrivilege>);

    /// <summary> Logons a user with the specified credentials. </summary>
    constructor CreateWithLogon(LogonType: TSecurityLogonType;
      Domain, User, Password: String; AddGroups: TArray<TGroup>);

    /// <summary> Logon a user using Services 4 Users. </summary>
    constructor CreateS4ULogon(Domain, User: String; const Source: TTokenSource;
      AddGroups: TArray<TGroup>);

    /// <summary> Creates a new token from the scratch. </summary>
    /// <remarks> This action requires SeCreateTokenPrivilege. </remarks>
    constructor CreateNtCreateToken(User: ISid; DisableUser: Boolean;
      Groups: TArray<TGroup>; Privileges: TArray<TPrivilege>;
      LogonID: TLuid; Owner: ISid; PrimaryGroup: ISid;
      const Source: TTokenSource; Expires: TLargeInteger);

    /// <summary>
    ///  Create a token using <see cref="NtImpersonateAnonymousToken">.
    /// </summary>
    constructor CreateAnonymous(Access: TAccessMask = MAXIMUM_ALLOWED;
      HandleAttributes: Cardinal = 0);

    /// <summary>
    ///  Opens a linked token for the current token.
    ///  Requires SeTcbPrivilege to open a primary token.
    /// </summary>
    function OpenLinkedToken(out Token: IToken): TNtxStatus;
  end;

{----------------------  End of interface section  ----------------------------}

implementation

uses
  Ntapi.ntpsapi, NtUtils.Tokens.Info, NtUtils.Processes, NtUtils.WinStation,
  NtUtils.Tokens, NtUiLib.Errors, NtUtils.Tokens.Impersonate, System.SysUtils,
  NtUtils.Tokens.Logon, DelphiUtils.Arrays, NtUtils.Lsa.Sid;

{ TToken }

procedure TToken.AfterConstruction;
begin
  inherited;
  FTokenV3 := CaptureTokenHandle(hxToken, FCaption);
end;

constructor TToken.Create(Handle: IHandle; Caption: String);
begin
  hxToken := Handle;
  FCaption := Caption;;
end;

constructor TToken.CreateAnonymous(Access: TAccessMask;
  HandleAttributes: Cardinal);
begin
  NtxOpenAnonymousToken(hxToken, Access, HandleAttributes).RaiseOnError;
  FCaption := 'Anonymous token';
end;

constructor TToken.CreateByHandle(Handle: THandle);
begin
  hxToken := NtxObject.Capture(Handle);
  FCaption := Format('Inherited %d [0x%x]', [hxToken.Handle, hxToken.Handle]);
end;

constructor TToken.CreateDuplicateHandle(SrcToken: IToken; Access: TAccessMask;
  SameAccess: Boolean; HandleAttributes: Cardinal = 0);
begin
  NtxDuplicateHandleLocal(SrcToken.Handle.Handle, hxToken, Access,
    HandleAttributes).RaiseOnError;

  FCaption := SrcToken.Caption + ' (ref)'
  // TODO: No need to snapshot handles, object address is already known
end;

constructor TToken.CreateDuplicateToken(SrcToken: IToken; Access: TAccessMask;
  TokenTypeEx: TTokenTypeEx; EffectiveOnly: Boolean);
var
  TokenType: TTokenType;
  ImpersonationLvl: TSecurityImpersonationLevel;
begin
  if TokenTypeEx = ttPrimary then
  begin
    TokenType := TokenPrimary;
    ImpersonationLvl := SecurityImpersonation;
  end
  else
  begin
    TokenType := TokenImpersonation;
    ImpersonationLvl := TSecurityImpersonationLevel(TokenTypeEx);
  end;

  NtxDuplicateToken(hxToken, SrcToken.Handle, TokenType,
    ImpersonationLvl, AttributeBuilder.UseEffectiveOnly(EffectiveOnly).
    UseDesiredAccess(Access)).RaiseOnError;

  if EffectiveOnly then
    FCaption := SrcToken.Caption + ' (eff. copy)'
  else
    FCaption := SrcToken.Caption + ' (copy)'
end;

constructor TToken.CreateNtCreateToken(User: ISid; DisableUser: Boolean;
  Groups: TArray<TGroup>; Privileges: TArray<TPrivilege>; LogonID: TLuid;
  Owner: ISid; PrimaryGroup: ISid; const Source: TTokenSource;
  Expires: TLargeInteger);
var
  TokenUser: TGroup;
begin
  TokenUser.Sid := User;

  // Fill user attributes. Zero value is default here and means "Enabled"
  if DisableUser then
    TokenUser.Attributes := SE_GROUP_USE_FOR_DENY_ONLY
  else
    TokenUser.Attributes := 0;

  NtxCreateToken(hxToken, TokenPrimary, SecurityImpersonation,
    Source, LogonID, TokenUser, PrimaryGroup, Groups, Privileges, Owner, nil,
    Expires).RaiseOnError;

  FCaption := 'New token: ' + LsaxSidToString(User);
end;

constructor TToken.CreateOpenCurrent(Access: TAccessMask);
begin
  CreateOpenProcess(NtCurrentProcessId, 'Current process');
end;

constructor TToken.CreateOpenEffective(TID: NativeUInt; ImageName: String;
  ImpersonationLevel: TSecurityImpersonationLevel; Access: TAccessMask;
  Attributes: Cardinal; EffectiveOnly: Boolean);
begin
  NtxCopyEffectiveTokenById(hxToken, TID, ImpersonationLevel, Access,
    Attributes, EffectiveOnly).RaiseOnError;

  FCaption := Format('Eff. thread %d of %s', [TID, ImageName]);
  if EffectiveOnly then
    FCaption := FCaption + ' (eff.)';
end;

constructor TToken.CreateOpenProcess(PID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal);
begin
  NtxOpenProcessTokenById(hxToken, PID, Access, Attributes).RaiseOnError;
  FCaption := Format('%s [%d]', [ImageName, PID]);
end;

constructor TToken.CreateOpenThread(TID: NativeUInt; ImageName: String;
  Access: TAccessMask; Attributes: Cardinal; Dummy: Integer);
begin
  NtxOpenThreadTokenById(hxToken, TID, Access, Attributes).RaiseOnError;
  FCaption := Format('Thread %d of %s', [TID, ImageName]);
end;

constructor TToken.CreateQueryWts(SessionID: Cardinal; Dummy: Boolean = True);
begin
  WsxQueryToken(hxToken, SessionID).RaiseOnError;
  FCaption := Format('Session %d token', [SessionID]);
end;

constructor TToken.CreateRestricted(SrcToken: IToken; Flags: Cardinal;
  SIDsToDisabe, SIDsToRestrict: TArray<TGroup>;
  PrivilegesToDelete: TArray<TPrivilege>);
var
  Disable, Restrict: TArray<ISid>;
  Remove: TArray<TLuid>;
  i: Integer;
begin
  SetLength(Disable, Length(SIDsToDisabe));
  for i := 0 to High(SIDsToDisabe) do
    Disable[i] := SIDsToDisabe[i].Sid;

  SetLength(Restrict, Length(SIDsToRestrict));
  for i := 0 to High(SIDsToRestrict) do
    Restrict[i] := SIDsToRestrict[i].Sid;

  SetLength(Remove, Length(PrivilegesToDelete));
  for i := 0 to High(PrivilegesToDelete) do
    Remove[i] := PrivilegesToDelete[i].Luid;

  NtxFilterToken(hxToken, SrcToken.Handle, Flags, Disable, Remove,
    Restrict).RaiseOnError;

  FCaption := 'Restricted ' + SrcToken.Caption;
end;

constructor TToken.CreateS4ULogon(Domain, User: String;
  const Source: TTokenSource; AddGroups: TArray<TGroup>);
begin
  LsaxLogonS4U(hxToken, Domain, User, Source, AddGroups).RaiseOnError;

  FCaption := 'S4U logon of ' + User;
end;

constructor TToken.CreateWithLogon(LogonType: TSecurityLogonType;
  Domain, User, Password: String; AddGroups: TArray<TGroup>);
begin
  LsaxLogonUser(hxToken, Domain, User, PWideChar(Password), LogonType,
    AddGroups).RaiseOnError;

  FCaption := 'Logon of ' + User;
end;

function TToken.GetCaption: String;
begin
  Result := FCaption;
end;

function TToken.GetHandle: IHandle;
begin
  Result := hxToken;
end;

function TToken.OpenLinkedToken(out Token: IToken): TNtxStatus;
var
  Handle: THandle;
begin
  Result := NtxToken.Query(hxToken, TokenLinkedToken,
    Handle);

  if Result.IsSuccess then
    Token := TToken.Create(NtxObject.Capture(Handle),
      'Linked token for ' + Caption);
end;

class procedure TToken.RevertThreadToken(TID: NativeUInt);
begin
  NtxSetThreadTokenById(TID, nil).RaiseOnError;
end;

function TToken.SendHandleToProcess(PID: NativeUInt): NativeUInt;
var
  hxTargetProcess: IHandle;
begin
  NtxOpenProcess(hxTargetProcess, PID, PROCESS_DUP_HANDLE).RaiseOnError;

  // Send the handle
  NtxDuplicateHandleTo(hxTargetProcess.Handle, hxToken.Handle,
    Result).RaiseOnError;
end;

procedure TToken.SetCaption(const Value: String);
begin
  FCaption := Value;

  if Assigned(TokenV3) then
    TokenV3.Caption := Value;
end;

end.

