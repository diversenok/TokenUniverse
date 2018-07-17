unit UI.SessionComboBox;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, TU.WtsApi;

type
  TSessionComboBox = class(TComboBox)
  private
    Sessions: TSessionList;
    function GetSession: Cardinal;
    procedure SetSession(const Value: Cardinal);
  protected
    { Protected declarations }
  public
    destructor Destroy; override;
    procedure RefreshSessionList;
    property SelectedSession: Cardinal read GetSession write SetSession;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
  TU.Common;

procedure Register;
begin
  RegisterComponents('Token Universe', [TSessionComboBox]);
end;

{ TSessionComboBox }

destructor TSessionComboBox.Destroy;
begin
  Sessions.Free;
  inherited;
end;

function TSessionComboBox.GetSession: Cardinal;
begin
  if ItemIndex = -1 then
    Result := StrToIntEx(Text, 'session')
  else
    Result := Sessions[ItemIndex].SessionId;
end;

procedure TSessionComboBox.RefreshSessionList;
var
  i: integer;
begin
  Sessions.Free;
  Sessions := TSessionList.CreateCurrentServer;
  Items.BeginUpdate;
  Items.Clear;

  for i := 0 to Sessions.Count - 1 do
    Items.Add(Sessions[i].ToString);

  if Sessions.Count > 0 then
    ItemIndex := 0;

  Items.EndUpdate;
end;

procedure TSessionComboBox.SetSession(const Value: Cardinal);
begin
  ItemIndex := Sessions.Find(Value);
  if ItemIndex = -1 then
    Text := IntToStr(Value);
end;

end.
