unit UI.Prototypes.Logon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes, NtUtils.Lsa.Logon,
  TU.Tokens, Winapi.WinNt, Vcl.StdCtrls;

type
  TFrameLogon = class(TFrame)
    ListView: TListViewEx;
    ComboOrigin: TComboBox;
    StaticOrigin: TStaticText;
    BtnSetOrigin: TButton;
    CheckBoxReference: TCheckBox;
    BtnSetRef: TButton;
    procedure ComboOriginChange(Sender: TObject);
    procedure BtnSetOriginClick(Sender: TObject);
    procedure BtnSetRefClick(Sender: TObject);
    procedure CheckBoxReferenceClick(Sender: TObject);
  private
    Token: IToken;
    LogonSource: TLogonSessionSource;
    procedure OnOriginChange(const NewOrigin: TLuid);
    procedure OnFlagsChange(const NewFlags: Cardinal);
    function GetSubscribed: Boolean;
  public
    property Subscribed: Boolean read GetSubscribed;
    procedure SubscribeToken(const Token: IToken);
    procedure UnsubscribeToken(const Dummy: IToken = nil);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Graphics, UI.Colors.Old, DelphiUiLib.Strings, NtUtils.Security.Sid,
  UI.Prototypes.Groups, Ntapi.ntseapi, Winapi.NtSecApi,
  DelphiUiLib.Reflection.Records, DelphiUtils.AutoObject,
  DelphiUiLib.Reflection;

{$R *.dfm}

const
  // Be consistent with ListView
  GROUP_IND_LOGON = 0;
  GROUP_IND_ORIGIN = 1;

{ TFrameLogon }

procedure TFrameLogon.BtnSetOriginClick(Sender: TObject);
begin
  Assert(Assigned(Token));
  try
    Token.InfoClass.Origin := LogonSource.SelectedLogonSession;
  finally
    // TODO: think how to fix duplicate events
    ComboOrigin.Color := clWindow;
    if Token.InfoClass.Query(tdTokenOrigin) then
      OnOriginChange(Token.InfoClass.Origin);
  end;
end;

procedure TFrameLogon.BtnSetRefClick(Sender: TObject);
begin
  Assert(Assigned(Token));
  try
    Token.InfoClass.SessionReference := CheckBoxReference.Checked;
  finally
    CheckBoxReference.Font.Style := [];
    if Token.InfoClass.Query(tdTokenFlags) then
      OnFlagsChange(Token.InfoClass.Flags);
  end;
end;

procedure TFrameLogon.CheckBoxReferenceClick(Sender: TObject);
begin
  CheckBoxReference.Font.Style := [fsBold];
end;

procedure TFrameLogon.ComboOriginChange(Sender: TObject);
begin
  ComboOrigin.Color := clStale;
end;

constructor TFrameLogon.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: TLogonSessionSource triggers enumeration, postpone it until
  // the user actually switches to the tab
  LogonSource := TLogonSessionSource.Create(ComboOrigin);
end;

destructor TFrameLogon.Destroy;
begin
  UnsubscribeToken;
  LogonSource.Free;
  inherited;
end;

function TFrameLogon.GetSubscribed: Boolean;
begin
  Result := Assigned(Token);
end;

procedure TFrameLogon.OnFlagsChange(const NewFlags: Cardinal);
begin
  CheckBoxReference.Checked := NewFlags and TOKEN_SESSION_NOT_REFERENCED = 0;
  CheckBoxReference.Font.Style := [];
end;

procedure TFrameLogon.OnOriginChange(const NewOrigin: TLuid);
begin
  ComboOrigin.Color := clWindow;
  LogonSource.SelectedLogonSession := NewOrigin;

  with ListView.Items[ListView.Items.Count - 1] do
    if NewOrigin = 0 then
      Cell[1] := '0 (value not set)'
    else if Token.InfoClass.Query(tdTokenStatistics) and
      (Token.InfoClass.Statistics.AuthenticationId = NewOrigin) then
      Cell[1] := 'Same as current'
    else
      Cell[1] := IntToHexEx(NewOrigin);
end;

procedure TFrameLogon.SubscribeToken(const Token: IToken);
begin
  UnsubscribeToken;

  Self.Token := Token;

  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  with ListView.Items.Add do
    begin
      Cell[0] := 'Logon ID';
      Cell[1] := 'Unknown';
      GroupId := GROUP_IND_LOGON;
    end;

  if Token.InfoClass.Query(tdLogonInfo) then
    with Token.InfoClass.LogonSessionInfo do
    begin
      ListView.Items[0].Cell[1] := IntToHexEx(LogonId);

      TRecord.Traverse(Ptr.RefOrNil<PSecurityLogonSessionData>(Detailed),
        procedure (const Field: TFieldReflection)
        var
          SidReflection: TRepresentation;
        begin
          // Skip the logon ID, we already processed it
          if Field.Offset = UIntPtr(@PSecurityLogonSessionData(nil).LogonID) then
            Exit;

          with ListView.Items.Add do
          begin
            Cell[0] := PrettifyCamelCase(Field.FieldName);
            GroupId := GROUP_IND_LOGON;

            if (Field.Offset = UIntPtr(@PSecurityLogonSessionData(nil).SID)) and
              not Assigned(Detailed) and Assigned(WellKnownSid) then
            begin
              // Fallback to well-known SIDs if necessary
              SidReflection := TType.Represent(WellKnownSid);
              Cell[1] := SidReflection.Text;
              Hint := SidReflection.Hint;
            end
            else
            begin
              Cell[1] := Field.Reflection.Text;
              Hint := Field.Reflection.Hint;
            end;
          end;
        end
      );
    end;

  // Add an item for the originating logon ID
  with ListView.Items.Add do
  begin
    Cell[0] := 'Logon ID';
    GroupId := GROUP_IND_ORIGIN;
  end;

  Token.Events.OnOriginChange.Subscribe(OnOriginChange);
  Token.Events.OnFlagsChange.Subscribe(OnFlagsChange);

  ListView.Items.EndUpdate;
end;

procedure TFrameLogon.UnsubscribeToken(const Dummy: IToken);
begin
  if Assigned(Token) then
  begin
    Token.Events.OnFlagsChange.Unsubscribe(OnFlagsChange);
    Token.Events.OnOriginChange.Unsubscribe(OnOriginChange);
    Token := nil;
  end;
end;

end.
