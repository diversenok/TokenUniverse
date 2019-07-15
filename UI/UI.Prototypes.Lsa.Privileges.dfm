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
  inline FramePrivileges: TFramePrivileges
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 320
    Height = 211
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 29
    Align = alClient
    TabOrder = 1
    inherited ListView: TListViewEx
      Width = 314
      Height = 205
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end
        item
          Caption = 'State'
          Width = 90
        end>
      GridLines = False
      Groups = <
        item
          Header = 'Sensitive (high integrity)'
          GroupID = 0
          State = [lgsNormal, lgsCollapsible]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end
        item
          Header = 'Usual (medium integrity)'
          GroupID = 1
          State = [lgsNormal, lgsCollapsible]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end
        item
          Header = 'Non-sensitive (no integrity)'
          GroupID = 2
          State = [lgsNormal, lgsCollapsible]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end>
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
