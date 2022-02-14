unit UI.AppContainer.View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Prototypes.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, NtUtils, Vcl.ComCtrls, VclEx.ListView, NtUtils.Profiles,
  Vcl.Menus;

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
    procedure Load(UserSid, AppContainerSid: ISid);
    class procedure Execute(AOwner: TComponent; UserSid, AppContainerSid: ISid);
      static;
  end;

implementation

uses
  NtUtils.Security.Sid, NtUtils.Lsa.Sid, NtUtils.Security.AppContainer,
  DelphiUiLib.Strings, NtUiLib.Errors, UI.MainForm, UI.Sid.View;

{$R *.dfm}

{ TFormAppContainer }

procedure TDialogAppContainer.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDialogAppContainer.tbxSidDblClick(Sender: TObject);
begin
  TDialogSidView.CreateView(Owner, AppContainer);
end;

procedure TDialogAppContainer.cmInspectClick(Sender: TObject);
begin
  if Assigned(lvChildren.Selected) then
    TDialogAppContainer.Execute(FormMain, User,
      ISid(lvChildren.Selected.OwnedIData));
end;

class procedure TDialogAppContainer.Execute(AOwner: TComponent; UserSid,
  AppContainerSid: ISid);
begin
  with TDialogAppContainer.CreateChild(AOwner, cfmDesktop) do
  begin
    Load(UserSid, AppContainerSid);
    Show;
  end;
end;

procedure TDialogAppContainer.lnkParentLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  ParentAC: ISid;
begin
  RtlxAppContainerParent(AppContainer, ParentAC).RaiseOnError;
  TDialogAppContainer.Execute(FormMain, User, ParentAC);
end;

procedure TDialogAppContainer.lnkUserLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  TDialogSidView.CreateView(Owner, User);
end;

procedure TDialogAppContainer.Load(UserSid, AppContainerSid: ISid);
begin
  User := UserSid;
  AppContainer := AppContainerSid;

  lnkUser.Caption := 'User: <a>' + LsaxSidToString(User) + '</a>';
  tbxSid.Text := RtlxSidToString(AppContainer);

  if UnvxQueryAppContainer(Info, AppContainer, User).IsSuccess then
  begin
    tbxName.Text := Info.FullName;
    tbxDispName.Text := Info.DisplayName;

    if Assigned(Info.ParentPackage) then
      lnkParent.Caption := 'Parent: <a>Show</a>'
    else
      lnkParent.Caption := 'Parent: None';
  end;
end;

procedure TDialogAppContainer.PagesChange(Sender: TObject);
var
  Children: TArray<ISid>;
  ChildInfo: TAppContainerInfo;
  i: Integer;
begin
  if Pages.ActivePage = TabChildren then
  begin
    if ChildrenInitialized or Assigned(Info.ParentPackage) then
      Exit;

    // Delayed loading of child AppContainers
    if UnvxEnumerateChildrenAppContainer(Children, AppContainer,
      User).IsSuccess then
    begin
      lvChildren.Items.BeginUpdate;

      for i := 0 to High(Children) do
        with lvChildren.Items.Add do
        begin
          if UnvxQueryAppContainer(ChildInfo, Children[i],
            User).IsSuccess then
            Caption := ChildInfo.Name
          else
            Caption := RtlxSidToString(Children[i]);

          Hint := Info.Name + '/' + Caption;
          OwnedIData := Children[i];
        end;

      lvChildren.Items.EndUpdate;
    end;

    ChildrenInitialized := True;
  end;
end;

end.
