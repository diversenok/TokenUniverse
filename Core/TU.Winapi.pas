unit TU.Winapi;

interface

uses
  Winapi.WinNt, Ntapi.ntseapi;

type
  TAccessGroup = (agRead, agWrite, agExecute, agStandard);

const
  ACCESS_COUNT = 13;
  AccessValues: array [0 .. ACCESS_COUNT - 1] of Cardinal = (
    TOKEN_ASSIGN_PRIMARY, TOKEN_DUPLICATE, TOKEN_IMPERSONATE, TOKEN_QUERY,
    TOKEN_QUERY_SOURCE, TOKEN_ADJUST_DEFAULT, TOKEN_ADJUST_PRIVILEGES,
     TOKEN_ADJUST_GROUPS, TOKEN_ADJUST_SESSIONID, _DELETE, READ_CONTROL,
    WRITE_DAC, WRITE_OWNER);
  AccessStrings: array [0 .. ACCESS_COUNT - 1] of String = ('Assign primary',
    'Duplicate', 'Impersonate', 'Query', 'Query source', 'Adjust default',
    'Adjust privileges', 'Adjust groups', 'Adjust session', 'Delete',
    'Read control', 'Write DAC', 'Write owner');

  AccessGroupValues: array [0 .. ACCESS_COUNT - 1] of TAccessGroup = (
    agExecute, agRead, agExecute, agRead, agRead, agWrite, agWrite, agWrite,
    agWrite, agStandard, agStandard, agStandard, agStandard);
  AccessGroupStrings: array [TAccessGroup] of String = ('Generic Read',
    'Generic Write', 'Generic Execute', 'Standard');

implementation

end.
