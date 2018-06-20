unit UI.Run;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  TU.TokenUtils;

type
  TRunDialog = class(TForm)
    ButtonCancel: TButton;
    GroupBoxAppName: TGroupBox;
    CheckBoxAppName: TCheckBox;
    EditAppName: TEdit;
    ButtonBrowseAppName: TButton;
    GroupBoxCmd: TGroupBox;
    EditCmd: TEdit;
    ButtonBrowseCmd: TButton;
    CheckBoxCmd: TCheckBox;
    GroupBoxFlags: TGroupBox;
    GroupBoxDirectory: TGroupBox;
    EditDirectory: TEdit;
    ButtonBrowseDirectory: TButton;
    CheckBoxDirectory: TCheckBox;
    CheckBoxInherit: TCheckBox;
    CheckBoxSuspended: TCheckBox;
    CheckBoxNewConsole: TCheckBox;
    CheckBoxBreakaway: TCheckBox;
    GroupBoxParent: TGroupBox;
    EditParent: TEdit;
    ButtonChooseParent: TButton;
    CheckBoxParent: TCheckBox;
    GroupBoxDesktop: TGroupBox;
    ComboBoxDesktop: TComboBox;
    ButtonAsUser: TButton;
    ButtonWithToken: TButton;
    procedure CheckBoxAppNameClick(Sender: TObject);
    procedure CheckBoxCmdClick(Sender: TObject);
    procedure CheckBoxDirectoryClick(Sender: TObject);
    procedure CheckBoxParentClick(Sender: TObject);
    class procedure Execute(AOwner: TComponent; Token: TToken);
    procedure ButtonChooseParentClick(Sender: TObject);
  private
    ParentPID: Cardinal;
  end;

var
  RunDialog: TRunDialog;

implementation

uses
  UI.ProcessList;

{$R *.dfm}

procedure TRunDialog.ButtonChooseParentClick(Sender: TObject);
begin
  ParentPID := TProcessListDialog.Execute(Self);
  EditParent.Text := IntToStr(ParentPID);
end;

procedure TRunDialog.CheckBoxAppNameClick(Sender: TObject);
begin
  EditAppName.Enabled := CheckBoxAppName.Checked;
  ButtonBrowseAppName.Enabled := CheckBoxAppName.Checked;
end;

procedure TRunDialog.CheckBoxCmdClick(Sender: TObject);
begin
  EditCmd.Enabled := CheckBoxCmd.Checked;
  ButtonBrowseCmd.Enabled := CheckBoxCmd.Checked;
end;

procedure TRunDialog.CheckBoxDirectoryClick(Sender: TObject);
begin
  EditDirectory.Enabled := CheckBoxDirectory.Checked;
  ButtonBrowseDirectory.Enabled := CheckBoxDirectory.Checked;
end;

procedure TRunDialog.CheckBoxParentClick(Sender: TObject);
begin
  EditParent.Enabled := CheckBoxParent.Checked;
  ButtonChooseParent.Enabled := CheckBoxParent.Checked;
end;

class procedure TRunDialog.Execute(AOwner: TComponent; Token: TToken);
begin
  with TRunDialog.Create(AOwner) do
  begin
    ShowModal;
  end;
end;

end.
