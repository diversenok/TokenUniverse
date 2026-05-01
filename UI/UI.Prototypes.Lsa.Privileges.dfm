object FrameLsaPrivileges: TFrameLsaPrivileges
  Left = 0
  Top = 0
  Width = 400
  Height = 400
  TabOrder = 0
  object LabelStatus: TLabel
    Left = 87
    Top = 377
    Width = 310
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    EllipsisPosition = epEndEllipsis
  end
  object ButtonApply: TButton
    Left = 3
    Top = 372
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Apply'
    TabOrder = 0
    OnClick = ButtonApplyClick
  end
  object PrivilegeList: TUiLibPrivilegeList
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 394
    Height = 364
    Margins.Bottom = 33
    Align = alClient
    TabOrder = 1
    Mode = pmAdding
    PopupMenu = PopupMenu
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
