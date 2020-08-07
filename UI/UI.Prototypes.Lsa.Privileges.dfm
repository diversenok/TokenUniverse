object FrameLsaPrivileges: TFrameLsaPrivileges
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object LabelStatus: TLabel
    Left = 87
    Top = 217
    Width = 230
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    EllipsisPosition = epEndEllipsis
  end
  object ButtonApply: TButton
    Left = 3
    Top = 212
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Apply'
    TabOrder = 0
    OnClick = ButtonApplyClick
  end
  inline PrivilegesFrame: TPrivilegesFrame
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 314
    Height = 205
    Margins.Bottom = 32
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    inherited ListViewEx: TListViewEx
      Width = 314
      Height = 205
      Checkboxes = True
      GridLines = False
      GroupView = True
      PopupMenu = PopupMenu
    end
  end
  object PopupMenu: TPopupMenu
    Left = 248
    Top = 128
    object MenuEnable: TMenuItem
      Caption = 'Enable'
      ShortCut = 16453
      OnClick = MenuEnableClick
    end
    object MenuDisable: TMenuItem
      Caption = 'Disable'
      ShortCut = 16452
      OnClick = MenuDisableClick
    end
  end
end
