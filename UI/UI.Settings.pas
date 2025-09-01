unit UI.Settings;

interface

uses
  TU.Tokens, System.Classes;

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
  ColumnsInfo: array [TTokenStringClass] of TColumnInfo = (
    (Caption: 'Caption';                  Width: 180; Alignment: taLeftJustify),
    (Caption: 'Handle';                   Width: 70;  Alignment: taLeftJustify),
    (Caption: 'Handle (detailed)';        Width: 100; Alignment: taLeftJustify),
    (Caption: 'Granted Access';           Width: 100; Alignment: taLeftJustify),
    (Caption: 'Granted Access (numeric)'; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Handle Count';             Width: 80;  Alignment: taCenter),
    (Caption: 'Paged Pool Charge';        Width: 110; Alignment: taCenter),
    (Caption: 'Non-Paged Pool Charge';    Width: 140; Alignment: taCenter),
    (Caption: 'Kernel Address';           Width: 150; Alignment: taLeftJustify),
    (Caption: 'Creator';                  Width: 150; Alignment: taLeftJustify),
    (Caption: 'User Name';                Width: 200; Alignment: taLeftJustify),
    (Caption: 'Groups';                   Width: 50;  Alignment: taCenter),
    (Caption: 'Enabled Groups';           Width: 100; Alignment: taCenter),
    (Caption: 'Privileges';               Width: 60;  Alignment: taCenter),
    (Caption: 'Enabled Privileges';       Width: 110; Alignment: taCenter),
    (Caption: 'Owner';                    Width: 200; Alignment: taLeftJustify),
    (Caption: 'Primary Group';            Width: 200; Alignment: taLeftJustify),
    (Caption: 'Source';                   Width: 70;  Alignment: taCenter),
    (Caption: 'Source ID';                Width: 100; Alignment: taLeftJustify),
    (Caption: 'Token Type';               Width: 90;  Alignment: taCenter),
    (Caption: 'Token ID';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon ID';                 Width: 100; Alignment: taLeftJustify),
    (Caption: 'Auth. Package';            Width: 90;  Alignment: taCenter),
    (Caption: 'Logon Type';               Width: 80;  Alignment: taCenter),
    (Caption: 'Logon Time';               Width: 130; Alignment: taCenter),
    (Caption: 'Modified ID';              Width: 100; Alignment: taLeftJustify),
    (Caption: 'Expires';                  Width: 120; Alignment: taCenter),
    (Caption: 'Dynamic Charged';          Width: 100; Alignment: taCenter),
    (Caption: 'Dynamic Available';        Width: 100; Alignment: taCenter),
    (Caption: 'Restricted SIDs';          Width: 100; Alignment: taCenter),
    (Caption: 'Session';                  Width: 50;  Alignment: taCenter),
    (Caption: 'Session (detailed)';       Width: 240; Alignment: taLeftJustify),
    (Caption: 'Sandbox Inert';            Width: 100; Alignment: taCenter),
    (Caption: 'Origin';                   Width: 100; Alignment: taLeftJustify),
    (Caption: 'Elevation';                Width: 90;  Alignment: taCenter),
    (Caption: 'Flags';                    Width: 180; Alignment: taLeftJustify),
    (Caption: 'Restricted';               Width: 70;  Alignment: taCenter),
    (Caption: 'Session Reference';        Width: 110; Alignment: taCenter),
    (Caption: 'Virtualization';           Width: 80;  Alignment: taCenter),
    (Caption: 'Filtered';                 Width: 60;  Alignment: taCenter),
    (Caption: 'UIAccess';                 Width: 70;  Alignment: taCenter),
    (Caption: 'LowBox';                   Width: 60;  Alignment: taCenter),
    (Caption: 'Private Namespace';        Width: 110; Alignment: taCenter),
    (Caption: 'Child Processes';          Width: 100; Alignment: taCenter),
    (Caption: 'Permissive Learning';      Width: 120; Alignment: taCenter),
    (Caption: 'Redirection Trust';        Width: 100; Alignment: taCenter),
    (Caption: 'Integrity';                Width: 80;  Alignment: taCenter),
    (Caption: 'Mandatory Policy';         Width: 170; Alignment: taLeftJustify),
    (Caption: 'Logon SID';                Width: 240; Alignment: taLeftJustify),
    (Caption: 'Capabilities';             Width: 70;  Alignment: taCenter),
    (Caption: 'AC Number';                Width: 80;  Alignment: taCenter),
    (Caption: 'AppContainer';             Width: 180; Alignment: taLeftJustify),
    (Caption: 'AppContainer Description'; Width: 240; Alignment: taLeftJustify),
    (Caption: 'User Claims';              Width: 80;  Alignment: taCenter),
    (Caption: 'Device Claims';            Width: 80;  Alignment: taCenter),
    (Caption: 'Restricted User Claims';   Width: 130; Alignment: taCenter),
    (Caption: 'Restricted Device Claims'; Width: 140; Alignment: taCenter),
    (Caption: 'Device Groups';            Width: 90;  Alignment: taCenter),
    (Caption: 'Restricted Device Groups'; Width: 150; Alignment: taCenter),
    (Caption: 'Security Attributes';      Width: 110; Alignment: taCenter),
    (Caption: 'Security Attribute Names'; Width: 240; Alignment: taLeftJustify),
    (Caption: 'LPAC';                     Width: 50;  Alignment: taCenter),
    (Caption: 'Package Flags';            Width: 140; Alignment: taLeftJustify),
    (Caption: 'Package Origin';           Width: 90;  Alignment: taCenter),
    (Caption: 'Is Restricted';            Width: 80;  Alignment: taCenter),
    (Caption: 'Trust Level';              Width: 100; Alignment: taCenter),
    (Caption: 'Singleton Attributes';     Width: 120; Alignment: taCenter),
    (Caption: 'BNO Isolation';            Width: 100; Alignment: taCenter),
    (Caption: 'BNO Prefix';               Width: 100; Alignment: taLeftJustify),
    (Caption: 'Sandboxed';                Width: 80;  Alignment: taCenter),
    (Caption: 'Is AppSilo';               Width: 80;  Alignment: taCenter)
  );

implementation

{ TSettings }

class constructor TSettings.Create;
begin
  SelectedColumns := [tsCaption, tsType, tsAccess, tsUser, tsSessionId,
    tsElevation, tsIntegrity];

  UseSafeImpersonation := True;
end;

end.
