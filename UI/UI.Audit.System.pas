unit UI.Audit.System;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,  Vcl.Menus,
  Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, VclEx.ListView,
  UI.Prototypes.Forms, UI.Prototypes.AuditFrame, NtUtils.Lsa.Audit;

type
  TDialogSystemAudit = class(TChildForm)
    FrameAudit: TFrameAudit;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetSystemAudit(NewAudit: IAudit);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  NtUtils, NtUiLib.Exceptions;

{$R *.dfm}

{ TDialogSystemAudit }

constructor TDialogSystemAudit.Create(AOwner: TComponent);
begin
  inherited CreateChild(AOwner, True);
end;

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
