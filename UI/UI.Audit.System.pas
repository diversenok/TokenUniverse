unit UI.Audit.System;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,  Vcl.Menus,
  Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, UI.ListViewEx,
  UI.Prototypes.ChildForm, UI.Prototypes.AuditFrame, NtUtils.Lsa.Audit;

type
  TDialogSystemAudit = class(TChildTaskbarForm)
    FrameAudit: TFrameAudit;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetSystemAudit(NewAudit: IAudit);
  public
  end;

implementation

uses
  NtUtils.Exceptions;

{$R *.dfm}

{ TDialogSystemAudit }

procedure TDialogSystemAudit.FormCreate(Sender: TObject);
begin
  FrameAudit.OnApplyClick := SetSystemAudit;
  FrameAudit.LoadForSystem;
end;

procedure TDialogSystemAudit.SetSystemAudit(NewAudit: IAudit);
begin
  try
    (NewAudit as ISystemAudit).AssignToSystem.RaiseOnError;
  finally
    FrameAudit.LoadForSystem;
  end;
end;

end.
