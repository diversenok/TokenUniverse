unit UI.AppContainer.View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Prototypes.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, NtUtils, Vcl.ComCtrls, VclEx.ListView,
  NtUtils.Security.AppContainer, Vcl.Menus;

type
  TDialogAppContainer = class(TChildForm)
    Pages: TPageControl;
    TabGeneral: TTabSheet;
    lblDispName: TLabel;
    tbxDispName: TEdit;
    lnkUser: TLinkLabel;
    lblName: TLabel;
    tbxName: TEdit;
    lblSid: TLabel;
    tbxSid: TEdit;
    ButtonClose: TButton;
    TabChildren: TTabSheet;
    lvChildren: TListViewEx;
    PopupMenu: TPopupMenu;
    cmInspect: TMenuItem;
    lnkParent: TLinkLabel;
    procedure lnkUserLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure tbxSidDblClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure lnkParentLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure PagesChange(Sender: TObject);
    procedure cmInspectClick(Sender: TObject);
  private
    User, AppContainer: ISid;
    Info: TAppContainerInfo;
    ChildrenInitialized: Boolean;
  public
    procedure Load(const UserSid, AppContainerSid: ISid);
    class procedure Execute(AOwner: TComponent; const UserSid, AppContainerSid: ISid);
      static;
  end;

implementation

uses
  NtUtils.Security.Sid, NtUtils.Lsa.Sid, DelphiUiLib.Strings, NtUiLib.Errors,
  UI.MainForm, UI.Sid.View;

{$R *.dfm}

{ TFormAppContainer }

procedure TDialogAppContainer.ButtonCloseClick;
begin
  Close;
end;

procedure TDialogAppContainer.tbxSidDblClick;
begin
  TDialogSidView.CreateView(Owner, AppContainer);
end;

procedure TDialogAppContainer.cmInspectClick;
begin
  if Assigned(lvChildren.Selected) then
    TDialogAppContainer.Execute(FormMain, User,
      ISid(lvChildren.Selected.OwnedIData));
end;

class procedure TDialogAppContainer.Execute;
begin
  with TDialogAppContainer.CreateChild(AOwner, cfmDesktop) do
  begin
    Load(UserSid, AppContainerSid);
    Show;
  end;
end;

procedure TDialogAppContainer.lnkParentLinkClick;
var
  ParentAC: ISid;
begin
  RtlxGetAppContainerParent(AppContainer, ParentAC).RaiseOnError;
  TDialogAppContainer.Execute(FormMain, User, ParentAC);
end;

procedure TDialogAppContainer.lnkUserLinkClick;
begin
  TDialogSidView.CreateView(Owner, User);
end;

procedure TDialogAppContainer.Load;
begin
  User := UserSid;
  AppContainer := AppContainerSid;

  lnkUser.Caption := 'User: <a>' + LsaxSidToString(User) + '</a>';
  tbxSid.Text := RtlxSidToString(AppContainer);

  if RtlxQueryAppContainer(Info, AppContainer, User).IsSuccess then
  begin
    tbxName.Text := Info.FullMoniker;
    tbxDispName.Text := Info.DisplayName;

    if Info.IsChild then
      lnkParent.Caption := 'Parent: <a>Show</a>'
    else
      lnkParent.Caption := 'Parent: None';
  end;
end;

procedure TDialogAppContainer.PagesChange;
var
  Children: TArray<ISid>;
  ChildInfo: TAppContainerInfo;
  i: Integer;
begin
  if Pages.ActivePage = TabChildren then
  begin
    if ChildrenInitialized or Info.IsChild then
      Exit;

    // Delayed loading of child AppContainers
    if RtlxEnumerateAppContainerSIDs(Children, AppContainer,
      User).IsSuccess then
    begin
      lvChildren.Items.BeginUpdate;

      for i := 0 to High(Children) do
        with lvChildren.Items.Add do
        begin
          if RtlxQueryAppContainer(ChildInfo, Children[i],
            User).IsSuccess then
            Caption := ChildInfo.Moniker
          else
            Caption := RtlxSidToString(Children[i]);

          Hint := ChildInfo.FullMoniker;
          OwnedIData := Children[i];
        end;

      lvChildren.Items.EndUpdate;
    end;

    ChildrenInitialized := True;
  end;
end;

end.
