unit UI.Prototypes.Logon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VclEx.ListView, UI.Prototypes, NtUtils.Lsa.Logon,
  TU.Tokens, Ntapi.WinNt, Vcl.StdCtrls, Ntapi.ntseapi, NtUtils;

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
    OriginSubscription: IAutoReleasable;
    FlagsSubscription: IAutoReleasable;
    procedure OnOriginChange(const Status: TNtxStatus; const NewOrigin: TLogonId);
    procedure OnFlagsChange(const Status: TNtxStatus; const NewFlags: TTokenFlags);
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
  Vcl.Graphics, NtUiCommon.Colors, DelphiUiLib.Strings, NtUtils.Security.Sid,
  Ntapi.NtSecApi, DelphiUiLib.Reflection.Records, DelphiUiLib.Reflection,
  NtUiLib.Errors;

{$R *.dfm}

const
  // Be consistent with ListView
  GROUP_IND_LOGON = 0;
  GROUP_IND_ORIGIN = 1;

{ TFrameLogon }

procedure TFrameLogon.BtnSetOriginClick;
var
  Status: TNtxStatus;
begin
  Assert(Assigned(Token));
  Status := Token.SetOrigin(LogonSource.SelectedLogonSession);

  if not Status.IsSuccess then
  begin
    OriginSubscription := nil;
    OriginSubscription := Token.ObserveOrigin(OnOriginChange);
  end;

  Status.RaiseOnError;
end;

procedure TFrameLogon.BtnSetRefClick;
begin
  Assert(Assigned(Token));
  Token.SetSessionReference(CheckBoxReference.Checked);
end;

procedure TFrameLogon.CheckBoxReferenceClick;
begin
  CheckBoxReference.Font.Style := [fsBold];
end;

procedure TFrameLogon.ComboOriginChange;
begin
  ComboOrigin.Color := ColorSettings.clBackgroundUnsaved;
end;

constructor TFrameLogon.Create;
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

function TFrameLogon.GetSubscribed;
begin
  Result := Assigned(Token);
end;

procedure TFrameLogon.OnFlagsChange;
begin
  if Status.IsSuccess then
  begin
    CheckBoxReference.Checked := NewFlags and TOKEN_SESSION_NOT_REFERENCED = 0;
    CheckBoxReference.Font.Style := [];
  end;
end;

procedure TFrameLogon.OnOriginChange;
var
  Statistics: TTokenStatistics;
begin
  if not Status.IsSuccess then
    Exit;

  ComboOrigin.Color := clWindow;
  LogonSource.SelectedLogonSession := NewOrigin;

  with ListView.Items[ListView.Items.Count - 1] do
    if NewOrigin = 0 then
      Cell[1] := '0 (value not set)'
    else if Token.QueryStatistics(Statistics).IsSuccess and
      (Statistics.AuthenticationId = NewOrigin) then
      Cell[1] := 'Same as current'
    else
      Cell[1] := UIntToHexEx(NewOrigin);
end;

procedure TFrameLogon.SubscribeToken;
var
  Statistics: TTokenStatistics;
  WellKnownSid: ISid;
  Detailed: ILogonSession;
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

  if Token.QueryStatistics(Statistics).IsSuccess then
  begin
    ListView.Items[0].Cell[1] := UIntToHexEx(Statistics.AuthenticationId);
    WellKnownSid := LsaxLookupKnownLogonSessionSid(Statistics.AuthenticationId);
    if not LsaxQueryLogonSession(Statistics.AuthenticationId, Detailed).IsSuccess then
      Detailed := nil;

    TRecord.Traverse(Auto.DataOrNil<PSecurityLogonSessionData>(Detailed),
      procedure (const Field: TFieldReflection)
      var
        SidReflection: TRepresentation;
      begin
        // Skip the logon ID, we already processed it
        if Field.Offset = IntPtr(@PSecurityLogonSessionData(nil).LogonID) then
          Exit;

        with ListView.Items.Add do
        begin
          Cell[0] := PrettifyCamelCase(Field.FieldName);
          GroupId := GROUP_IND_LOGON;

          if (Field.Offset = IntPtr(@PSecurityLogonSessionData(nil).SID)) and
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

  OriginSubscription := Token.ObserveOrigin(OnOriginChange);
  FlagsSubscription := Token.ObserveFlags(OnFlagsChange);

  ListView.Items.EndUpdate;
end;

procedure TFrameLogon.UnsubscribeToken;
begin
  if Assigned(Token) then
    Token := nil;
end;

end.
