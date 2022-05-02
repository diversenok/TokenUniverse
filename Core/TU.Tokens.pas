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

constructor TToken.CreateByHandle(Handle: THandle);
begin
  hxToken := Auto.CaptureHandle(Handle);
  FCaption := Format('Inherited %d [0x%x]', [hxToken.Handle, hxToken.Handle]);
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
    Token := TToken.Create(Auto.CaptureHandle(Handle),
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

