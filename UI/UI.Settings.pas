unit UI.Settings;

interface

uses
  TU.Tokens, TU.Tokens3, System.Classes;

type
  TColumns = set of TTokenStringClass;

  TSettings = class
  class var
    SelectedColumns: TColumns;
    PromptOnHandleClose: Boolean;
    NoCloseCreationDialogs: Boolean;
    UseSafeImpersonation: Boolean;
    class constructor Create;
  end;

type
  TColumnInfo = record
    Caption: String;
    Width: Integer;
    Alignment: TAlignment;
  end;

const
  ColumsInfo: array [TTokenStringClass] of TColumnInfo = (
    (Caption: 'Caption';                  Width: 100; Alignment: taLeftJustify),
    (Caption: 'Handle';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Access';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Kernel Address';           Width: 100; Alignment: taLeftJustify),
    (Caption: 'User Name';                Width: 170; Alignment: taLeftJustify),
    (Caption: 'Groups';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Privileges';               Width: 100; Alignment: taLeftJustify),
    (Caption: 'Owner';                    Width: 100; Alignment: taLeftJustify),
    (Caption: 'Primary Group';            Width: 100; Alignment: taLeftJustify),
    (Caption: 'Source';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Source ID';                Width: 100; Alignment: taLeftJustify),
    (Caption: 'Token Type';               Width: 85; Alignment: taLeftJustify),
    (Caption: 'Token ID';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon ID';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'Modified ID';              Width: 100; Alignment: taLeftJustify),
    (Caption: 'Exprires';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'Dynamic Charged';          Width: 100; Alignment: taLeftJustify),
    (Caption: 'Dynamic Available';        Width: 100; Alignment: taLeftJustify),
    (Caption: 'Restricted SIDs';          Width: 100; Alignment: taLeftJustify),
    (Caption: 'Session';                  Width: 50; Alignment: taCenter),
    (Caption: 'Sandbox Inert';            Width: 100; Alignment: taLeftJustify),
    (Caption: 'Origin';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Elevation';                Width: 90; Alignment: taCenter),
    (Caption: 'Flags';                    Width: 100; Alignment: taLeftJustify),
    (Caption: 'Restricted';               Width: 100; Alignment: taLeftJustify),
    (Caption: 'SessionReference';         Width: 100; Alignment: taLeftJustify),
    (Caption: 'Virtualization';           Width: 100; Alignment: taLeftJustify),
    (Caption: 'Filtered';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'UIAccess';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'LowBox';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Private Namespace';        Width: 100; Alignment: taLeftJustify),
    (Caption: 'Child Flags';              Width: 100; Alignment: taLeftJustify),
    (Caption: 'Permissive Learning';      Width: 100; Alignment: taLeftJustify),
    (Caption: 'Redirection Trust';        Width: 100; Alignment: taLeftJustify),
    (Caption: 'Integrity';                Width: 70; Alignment: taCenter),
    (Caption: 'Mandatory Policy';         Width: 100; Alignment: taLeftJustify),
    (Caption: 'Capabilities';             Width: 100; Alignment: taLeftJustify),
    (Caption: 'AppContainer Number';      Width: 100; Alignment: taLeftJustify),
    (Caption: 'AppContainer Name';        Width: 100; Alignment: taLeftJustify),
    (Caption: 'AppContainer Description'; Width: 100; Alignment: taLeftJustify),
    (Caption: 'User Claims';              Width: 100; Alignment: taLeftJustify),
    (Caption: 'Device Claims';            Width: 100; Alignment: taLeftJustify),
    (Caption: 'Restricted User Claims';   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Restricted Device Claims'; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Device Groups';            Width: 100; Alignment: taLeftJustify),
    (Caption: 'Restricted Device Groups'; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Security Attributes';      Width: 100; Alignment: taLeftJustify),
    (Caption: 'LPAC';                     Width: 100; Alignment: taLeftJustify),
    (Caption: 'Is Restricted';            Width: 100; Alignment: taLeftJustify),
    (Caption: 'Trust Level';              Width: 100; Alignment: taLeftJustify),
    (Caption: 'Singleton Attributes';     Width: 100; Alignment: taLeftJustify),
    (Caption: 'BNO Isolation';            Width: 100; Alignment: taLeftJustify),
    (Caption: 'Sandboxed';                Width: 100; Alignment: taLeftJustify),
    (Caption: 'Originating Trust';        Width: 100; Alignment: taLeftJustify)
  );

implementation

{ TSettings }

class constructor TSettings.Create;
begin
  SelectedColumns := [tsType, tsAccess, tsUser, tsSessionId,
    tsElevation, tsIntegrity];

  UseSafeImpersonation := True;
end;

end.
