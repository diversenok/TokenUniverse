unit UI.AppContainer.List;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, VclEx.ListView, UI.Prototypes.Forms, NtUtils, Vcl.Menus;

type
  TDialogACProfiles = class(TChildForm)
    lblProfiles: TLabel;
    cbProfile: TComboBox;
    lvAppContainers: TListViewEx;
    SearchBox: TButtonedEdit;
    ButtonClose: TButton;
    ButtonOK: TButton;
    PopupMenu: TPopupMenu;
    cmInspect: TMenuItem;
    cmSelect: TMenuItem;
    procedure ButtonCloseClick(Sender: TObject);
    procedure cmInspectClick(Sender: TObject);
    procedure cmSelectClick(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
    procedure SearchBoxRightButtonClick(Sender: TObject);
  private
    Profiles: TArray<ISid>;
    procedure UpdateProfileList(LockToUser: ISid = nil);
    procedure UpdateAppContainers;
    function User: ISid;
  public
    class procedure Execute(AOwner: TComponent; LockToUser: ISid = nil); static;
    class function ExecuteSelect(AOwner: TComponent; LockToUser: ISid): ISid;
      static;
  end;

implementation

uses
  NtUtils.Profiles, NtUtils.Security.Sid, NtUtils.Lsa.Sid,
  UI.AppContainer.View, UI.MainForm;

{$R *.dfm}

{ TDialogACProfiles }

procedure TDialogACProfiles.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogACProfiles.cmInspectClick(Sender: TObject);
begin
  if Assigned(lvAppContainers.Selected) then
    TDialogAppContainer.Execute(FormMain, User,
      ISid(lvAppContainers.Selected.OwnedIData));
end;

procedure TDialogACProfiles.cmSelectClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class procedure TDialogACProfiles.Execute(AOwner: TComponent; LockToUser: ISid);
begin
  with TDialogACProfiles.CreateChild(AOwner, True) do
  begin
    UpdateProfileList(LockToUser);
    UpdateAppContainers;
    Show;
  end;
end;

class function TDialogACProfiles.ExecuteSelect(AOwner: TComponent;
  LockToUser: ISid): ISid;
begin
  with TDialogACProfiles.CreateChild(AOwner, True) do
  begin
    UpdateProfileList(LockToUser);
    UpdateAppContainers;
    ButtonOK.Visible := True;

    ShowModal;

    if not Assigned(lvAppContainers.Selected) then
      Abort;

    Result := ISid(lvAppContainers.Selected.OwnedIData);
  end;
end;

procedure TDialogACProfiles.SearchBoxChange(Sender: TObject);
var
  i: Integer;
begin
  lvAppContainers.Items.BeginUpdate;
  lvAppContainers.GroupView := SearchBox.Text <> '';

  for i := 0 to Pred(lvAppContainers.Items.Count) do
    with lvAppContainers.Items[i] do
      if (SearchBox.Text <> '') and Matches(SearchBox.Text) then
        GroupID := 0
      else
        GroupID := -1;

  lvAppContainers.Items.EndUpdate;
end;

procedure TDialogACProfiles.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

procedure TDialogACProfiles.UpdateAppContainers;
var
  AppContainers: TArray<ISid>;
  Info: TAppContainerInfo;
  i: Integer;
begin
  if not UnvxEnumerateAppContainers(AppContainers, User).IsSuccess then
    Exit;

  lvAppContainers.Items.BeginUpdate;
  lvAppContainers.Items.Clear;

  for i := 0 to High(AppContainers) do
    with lvAppContainers.Items.Add do
    begin
      Cell[2] := RtlxSidToString(AppContainers[i]);

      if UnvxQueryAppContainer(Info, AppContainers[i],
        User).IsSuccess then
      begin
        Cell[0] := Info.DisplayName;
        Cell[1] := Info.Name;
      end;

      OwnedIData := AppContainers[i];
    end;

  lvAppContainers.Items.EndUpdate;
end;

procedure TDialogACProfiles.UpdateProfileList(LockToUser: ISid = nil);
var
  i: Integer;
begin
  // No need to snapshot profiles if the choise is locked
  if Assigned(LockToUser) then
  begin
    Profiles := [LockToUser];
    cbProfile.Items.Add(LsaxSidToString(LockToUser));
    cbProfile.ItemIndex := 0;
    cbProfile.Enabled := False;
    Exit;
  end;

  // Include loaded profiles only since we read them to enumerate AppContainers
  if UnvxEnumerateLoadedProfiles(Profiles).IsSuccess then
  begin
    cbProfile.Items.BeginUpdate;

    for i := 0 to High(Profiles) do
      cbProfile.Items.Add(LsaxSidToString(Profiles[i]));

    cbProfile.Items.EndUpdate;
  end;
end;

function TDialogACProfiles.User: ISid;
begin
  Result := Profiles[cbProfile.ItemIndex];
end;

end.
