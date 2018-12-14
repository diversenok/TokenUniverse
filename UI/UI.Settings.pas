unit UI.Settings;

interface

uses
  TU.Tokens;

type
  TColumns = set of TTokenStringClass;

  TSettings = class
  class var
    SelectedColumns: TColumns;
    PromptOnHandleClose: Boolean;
    NoCloseCreationDialogs: Boolean;
    class constructor Create;
  end;

implementation

{ TSettings }

class constructor TSettings.Create;
begin
  SelectedColumns := [tsTokenType, tsAccess, tsUserName,
    tsSession, tsElevation, tsIntegrity];
end;

end.
