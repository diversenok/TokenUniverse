object DialogSystemAudit: TDialogSystemAudit
  Left = 0
  Top = 0
  Caption = 'System Audit Policy'
  ClientHeight = 350
  ClientWidth = 356
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  inline FrameAudit: TFrameAudit
    Left = 0
    Top = 0
    Width = 356
    Height = 350
    Align = alClient
    TabOrder = 0
    inherited LabelStatus: TLabel
      Top = 328
      Width = 247
    end
    inherited ListView: TListViewEx
      Width = 350
      Height = 318
      Columns = <
        item
          Width = 180
        end
        item
          Alignment = taCenter
          Caption = 'Success'
          Width = 70
        end
        item
          Alignment = taCenter
          Caption = 'Failure'
          Width = 70
        end>
      PopupMenu = FrameAudit.PopupSystem
    end
    inherited ButtonApply: TButton
      Left = 256
      Top = 323
    end
  end
end
