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
    ForceUpdateOnInfoDialog: Boolean;
    class constructor Create;
  end;

type
  TColumnCategory = (uicGeneral, uicAdvanced, uicStatistics, uicLogon,
    uicSource);

  TColumnInfo = record
    Caption: String;
    Category: TColumnCategory;
    Width: Integer;
    Alignment: TAlignment;
  end;

const
  ColumsInfo: array [TTokenStringClass] of TColumnInfo = (
    (Caption: 'Token Type';         Category: uicGeneral; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Granted Access';     Category: uicGeneral; Width: 100; Alignment: taLeftJustify),
    (Caption: 'User Name';          Category: uicGeneral; Width: 160; Alignment: taLeftJustify),
    (Caption: 'User State';         Category: uicGeneral; Width: 70; Alignment: taCenter),
    (Caption: 'Session';            Category: uicGeneral; Width: 50; Alignment: taCenter),
    (Caption: 'Elevated';           Category: uicGeneral; Width: 65; Alignment: taCenter),
    (Caption: 'Integrity';          Category: uicGeneral; Width: 70; Alignment: taCenter),
    (Caption: 'Object Address';     Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Handle';             Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'No-write-up';        Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'New-process-min';    Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'UIAccess';           Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Owner';              Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Primary Group';      Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Sandbox Inert';      Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Has Restrictions';   Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Virtualization';     Category: uicAdvanced; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Token ID';           Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Exprires';           Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Dynamic Charged';    Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Dynamic Available';  Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Group Count';        Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Privilege Count';    Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Modified ID';        Category: uicStatistics; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon/Auth ID';      Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon Auth Package'; Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon Server';       Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon WTS Session';  Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon Time';         Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon Type';         Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Logon User Name';    Category: uicLogon; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Source LUID';        Category: uicSource; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Source Name';        Category: uicSource; Width: 100; Alignment: taLeftJustify),
    (Caption: 'Origin';             Category: uicAdvanced; Width: 100; Alignment: taLeftJustify)
  );

implementation

{ TSettings }

class constructor TSettings.Create;
begin
  SelectedColumns := [tsTokenType, tsAccess, tsUserName, tsSession, tsElevation,
    tsIntegrity];
end;

end.
