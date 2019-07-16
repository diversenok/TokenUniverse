unit NtUtils.Environment;

interface

uses
  NtUtils.Exceptions;

type
  TEnvVariable = record
    Name, Value: String;
  end;

  IEnvironment = interface
    function Environment: Pointer;
    function Size: NativeUInt;
    function IsCurrent: Boolean;
    function SetAsCurrent: TNtxStatus;
    function SetAsCurrentExchange(out Old: IEnvironment): TNtxStatus;
    function Enumerate: TArray<TEnvVariable>;
    function SetVariable(Name, Value: String): TNtxStatus;
    function QueryVariable(Name: String): String;
    function QueryVariableWithStatus(Name: String; out Value: String):
      TNtxStatus;
  end;

  TEnvironment = class (TInterfacedObject, IEnvironment)
  private
    FBlock: Pointer;
  public
    constructor OpenCurrent;
    constructor CreateNew(CloneCurrent: Boolean);
    destructor Destroy; override;
    function Environment: Pointer;
    function Size: NativeUInt;
    function IsCurrent: Boolean;
    function SetAsCurrent: TNtxStatus;
    function SetAsCurrentExchange(out Old: IEnvironment): TNtxStatus;
    function Enumerate: TArray<TEnvVariable>;
    function SetVariable(Name, Value: String): TNtxStatus;
    function QueryVariable(Name: String): String;
    function QueryVariableWithStatus(Name: String; out Value: String):
      TNtxStatus;
  end;

// Environmental block parsing routine
function RtlxEnumerateEnvironment(Environment: PWideChar;
  EnvironmentLength: Cardinal; var CurrentIndex: Cardinal;
  out Name: String; out Value: String): Boolean;

implementation

uses
  Ntapi.ntdef, Ntapi.ntrtl, Ntapi.ntstatus, Ntapi.ntpebteb, Ntapi.ntmmapi,
  Ntapi.ntpsapi;

function RtlxEnumerateEnvironment(Environment: PWideChar;
  EnvironmentLength: Cardinal; var CurrentIndex: Cardinal;
  out Name: String; out Value: String): Boolean;
var
  pCurrentChar, pName, pValue: PWideChar;
  StartIndex: Cardinal;
begin
  pCurrentChar := Environment + CurrentIndex;

  // Start parsing the name
  StartIndex := CurrentIndex;
  pName := pCurrentChar;

  // Find the end of the name
  repeat
    if CurrentIndex >= EnvironmentLength then
      Exit(False);

    // The equality sign is considered as a delimiter between the name and the
    // value unless it is the first character
    if (pCurrentChar^ = '=') and (StartIndex <> CurrentIndex) then
      Break;

    if pCurrentChar^ = #0 then
      Exit(False); // no more variables

    Inc(CurrentIndex);
    Inc(pCurrentChar);
  until False;

  SetString(Name, pName, CurrentIndex - StartIndex);

  // Skip the equality sign
  Inc(CurrentIndex);
  Inc(pCurrentChar);

  // Start parsing the value
  StartIndex := CurrentIndex;
  pValue := pCurrentChar;

  // Find the end of the value
  repeat
    if CurrentIndex >= EnvironmentLength then
      Exit(False);

    // The value is zero-terminated
    if pCurrentChar^ = #0 then
      Break;

    Inc(CurrentIndex);
    Inc(pCurrentChar);
  until False;

  SetString(Value, pValue, CurrentIndex - StartIndex);

  // Skip the #0 character
  Inc(CurrentIndex);

  Result := True;
end;

{ TEnvironment }

function TEnvironment.Enumerate: TArray<TEnvVariable>;
var
  Ind: Cardinal;
  BlockLength: Cardinal;
  Name, Value: String;
begin
  SetLength(Result, 0);

  Ind := 0;
  BlockLength := Self.Size div SizeOf(WideChar);

  while RtlxEnumerateEnvironment(Environment, BlockLength, Ind, Name, Value) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Name := Name;
    Result[High(Result)].Value := Value;
  end;
end;

function TEnvironment.Environment: Pointer;
begin
  // Always return a non-null pointer

  if Assigned(FBlock) then
    Result := FBlock
  else
    Result := RtlGetCurrentPeb.ProcessParameters.Environment;
end;

function TEnvironment.IsCurrent: Boolean;
begin
  // Referencing null means referencing current environment
  Result := not Assigned(FBlock);
end;

constructor TEnvironment.CreateNew(CloneCurrent: Boolean);
begin
  NtxAssert(RtlCreateEnvironment(CloneCurrent, FBlock), 'RtlCreateEnvironment');
end;

destructor TEnvironment.Destroy;
begin
  if Assigned(FBlock) then
    RtlDestroyEnvironment(FBlock);
  inherited;
end;

constructor TEnvironment.OpenCurrent;
begin
  FBlock := nil;
end;

function TEnvironment.QueryVariable(Name: String): String;
begin
  if not QueryVariableWithStatus(Name, Result).IsSuccess then
    Result := '';
end;

function TEnvironment.QueryVariableWithStatus(Name: String; out Value: String):
  TNtxStatus;
var
  NameStr, ValueStr: UNICODE_STRING;
begin
  NameStr.FromString(Name);

  ValueStr.Length := 0;
  ValueStr.MaximumLength := 0;
  ValueStr.Buffer := nil;

  // Query the required length
  Result.Location := 'RtlQueryEnvironmentVariable_U';
  Result.Status := RtlQueryEnvironmentVariable_U(FBlock, NameStr, ValueStr);

  if Result.Status = STATUS_BUFFER_TOO_SMALL then
  begin
    // Include zero-termination character and allocate the buffer
    ValueStr.MaximumLength := ValueStr.Length + SizeOf(WideChar);
    ValueStr.Buffer := AllocMem(ValueStr.MaximumLength);

    Result.Status := RtlQueryEnvironmentVariable_U(FBlock, NameStr, ValueStr);

    if Result.IsSuccess then
      Value := ValueStr.ToString;

    FreeMem(ValueStr.Buffer);
  end;
end;

function TEnvironment.SetAsCurrent: TNtxStatus;
begin
  if Assigned(FBlock) then
  begin
    Result.Location := 'RtlSetCurrentEnvironment';
    Result.Status := RtlSetCurrentEnvironment(FBlock, nil);

    // Make the object point to the current environment
    if Result.IsSuccess then
      FBlock := nil;
  end
  else
  begin
    // We are already pointing to the current environment, nothing to do
    Result.Status := STATUS_SUCCESS;
  end;
end;

function TEnvironment.SetAsCurrentExchange(out Old: IEnvironment): TNtxStatus;
var
  OldEnv: Pointer;
  OldEnvObj: TEnvironment;
begin
  if Assigned(FBlock) then
  begin
    Result.Location := 'RtlSetCurrentEnvironment';
    Result.Status := RtlSetCurrentEnvironment(FBlock, @OldEnv);

    if Result.IsSuccess then
    begin
      // Store the returned pointer into a new IEnvironmnent
      OldEnvObj := TEnvironment.Create;
      OldEnvObj.FBlock := OldEnv;
      Old := OldEnvObj;

      // Make this object point to the current environment
      FBlock := nil;
    end;
  end
  else
  begin
    // The caller tries to exchange the current environment with itself
    Result.Status := STATUS_SUCCESS;
    Old := Self;
  end;
end;

function TEnvironment.SetVariable(Name, Value: String): TNtxStatus;
var
  NameStr, ValueStr: UNICODE_STRING;
begin
  NameStr.FromString(Name);
  ValueStr.FromString(Value);
  Result.Location := 'RtlSetEnvironmentVariable';
  Result.Status := RtlSetEnvironmentVariable(FBlock, NameStr, ValueStr);
  // Note: this operation might re-allocate the environment
end;

function TEnvironment.Size: NativeUInt;
begin
  // This is the same way as RtlSetEnvironmentVariable determines the size.
  // Make sure to pass a valid pointer for the call.
  Result := RtlSizeHeap(NtCurrentTeb.ProcessEnvironmentBlock.ProcessHeap, 0,
    Environment);
end;

end.
