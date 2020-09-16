unit UI.HandleSearch;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls,
  UI.TokenListFrame, VclEx.ListView, UI.Prototypes.Forms;

type
  TFormHandleSearch = class(TChildForm)
    Frame: TFrameTokenList;
    ButtonClose: TButton;
    LabelStatistics: TLabel;
    ButtonRefresh: TButton;
    PopupMenu: TPopupMenu;
    cmSave: TMenuItem;
    cmInspect: TMenuItem;
    procedure ButtonRefreshClick(Sender: TObject);
    procedure cmInspectClick(Sender: TObject);
    procedure cmSaveClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

implementation

uses
  System.UITypes, UI.MainForm, UI.Information, TU.Tokens,
  Winapi.WinNt, Ntapi.ntpebteb, NtUtils, NtUtils.Objects.Snapshots,
  NtUiLib.Exceptions, DelphiUtils.Arrays, NtUtils.Processes,
  NtUtils.Processes.Query, NtUtils.Objects;

{$R *.dfm}

procedure TFormHandleSearch.ButtonRefreshClick(Sender: TObject);
var
  Handles: TArray<TSystemHandleEntry>;
  PerProcess: TArray<TArrayGroup<TProcessId, TSystemHandleEntry>>;
  TokenIndex: Integer;
  hxProcess, hxToken: IHandle;
  ImageName: String;
  i, j: Integer;
begin
  NtxEnumerateHandles(Handles).RaiseOnError;
  NtxFindType('Token', TokenIndex).RaiseOnError;

  // Include only tokens from other processes
  TArray.FilterInline<TSystemHandleEntry>(Handles,
    function (const Entry: TSystemHandleEntry): Boolean
    begin
      Result := (Entry.ObjectTypeIndex = TokenIndex) and
        (Entry.UniqueProcessId <> NtCurrentTeb.ClientID.UniqueProcess);
    end
  );

  // Group tokens by process
  PerProcess := TArray.GroupBy<TSystemHandleEntry, TProcessId>(Handles,
    function (const Entry: TSystemHandleEntry): TProcessId
    begin
      Result := Entry.UniqueProcessId;
    end,
    function (const PID1, PID2: TProcessId): Boolean
    begin
      Result := PID1 = PID2;
    end
  );

  LabelStatistics.Caption := Format('Found %d opened handles in %d processes',
    [Length(Handles), Length(PerProcess)]);

  // Start adding the tokens to the view
  Frame.ListViewTokens.Groups.BeginUpdate;
  Frame.ListViewTokens.Items.BeginUpdate;
  Frame.ListViewTokens.Groups.Clear;
  Frame.ListViewTokens.Items.Clear;
  Frame.ListViewTokens.Groups.Add.Header := 'Search Results';

  for i := 0 to High(PerProcess) do
  begin
    NtxOpenProcess(hxProcess, PerProcess[i].Key, PROCESS_DUP_HANDLE);

    // Format process' name
    if not NtxQueryNameProcessId(PerProcess[i].Key, ImageName).IsSuccess
      then ImageName := 'Unknown Process';

    ImageName := Format('%s [%d]', [ImageName, PerProcess[i].Key]);

    // Each process has a ListView group
    with Frame.ListViewTokens.Groups.Add do
    begin
      Header := ImageName;
      State := State + [lgsCollapsible];

      // Add all tokens from the process
      for j := 0 to High(PerProcess[i].Values) do
      begin
        // Try to get a copy
        if Assigned(hxProcess) then
          NtxDuplicateHandleFrom(hxProcess.Handle,
            PerProcess[i].Values[j].HandleValue, hxToken)
        else
          hxToken := nil;

        Frame.AddToken(TToken.CreatePseudo(PerProcess[i].Values[j], ImageName,
          hxToken), Index);
      end;
    end;
  end;

  Frame.ListViewTokens.Groups.EndUpdate;
  Frame.ListViewTokens.Items.EndUpdate;
end;

procedure TFormHandleSearch.cmInspectClick(Sender: TObject);
begin
  if Assigned(Frame.GetSelectedToken()) then
    TInfoDialog.CreateFromToken(FormMain, Frame.GetSelectedToken.ConvertToReal);
end;

procedure TFormHandleSearch.cmSaveClick(Sender: TObject);
begin
  if Assigned(Frame.GetSelectedToken()) then
    FormMain.TokenView.Add(Frame.GetSelectedToken.ConvertToReal);
end;

end.
