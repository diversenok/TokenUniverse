unit Ntapi.ntioapi;

{$MINENUMSIZE 4}

interface

uses
  Winapi.WinNt;

const
  // WinNt.12936
  FILE_READ_DATA = $0001;            // file & pipe
  FILE_LIST_DIRECTORY = $0001;       // directory
  FILE_WRITE_DATA = $0002;           // file & pipe
  FILE_ADD_FILE = $0002;             // directory
  FILE_APPEND_DATA = $0004;          // file
  FILE_ADD_SUBDIRECTORY = $0004;     // directory
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe
  FILE_READ_EA = $0008;              // file & directory
  FILE_WRITE_EA = $0010;             // file & directory
  FILE_EXECUTE = $0020;              // file
  FILE_TRAVERSE = $0020;             // directory
  FILE_DELETE_CHILD = $0040;         // directory
  FILE_READ_ATTRIBUTES = $0080;      // all
  FILE_WRITE_ATTRIBUTES = $0100;     // all

  FILE_ALL_ACCESS = STANDARD_RIGHTS_ALL or $1FF;

  // WinNt.12983
  FILE_SHARE_READ = $00000001;
  FILE_SHARE_WRITE = $00000002;
  FILE_SHARE_DELETE = $00000004;

implementation

end.
