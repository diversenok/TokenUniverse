object FrameAudit: TFrameAudit
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object LabelStatus: TLabel
    Left = 3
    Top = 282
    Width = 342
    Height = 19
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    EllipsisPosition = epEndEllipsis
  end
  object ListView: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 445
    Height = 272
    Margins.Bottom = 29
    Align = alClient
    Columns = <
      item
        Width = 140
      end
      item
        Alignment = taCenter
        Caption = 'Succ Inc'
        Width = 55
      end
      item
        Alignment = taCenter
        Caption = 'Succ Exc'
        Width = 55
      end
      item
        Alignment = taCenter
        Caption = 'Fail Inc'
        Width = 55
      end
      item
        Alignment = taCenter
        Caption = 'Fail Exc'
        Width = 55
      end>
    MultiSelect = True
    GroupView = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupPerUser
    TabOrder = 0
    ViewStyle = vsReport
    OnContextPopup = ListViewContextPopup
    ClipboardSourceColumn = 0
    ColoringItems = True
    PopupOnItemsOnly = True
  end
  object ButtonApply: TButton
    Left = 351
    Top = 277
    Width = 97
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply changes'
    Enabled = False
    TabOrder = 1
    OnClick = ButtonApplyClick
  end
  object PopupPerUser: TPopupMenu
    Left = 167
    Top = 205
    object MenuIncSucc: TMenuItem
      Caption = 'Include success events'
      OnClick = MenuIncSuccClick
    end
    object MenuExcSucc: TMenuItem
      Caption = 'Exclude inherited success events'
      OnClick = MenuExcSuccClick
    end
    object MenuIncFail: TMenuItem
      Caption = 'Include failure events'
      OnClick = MenuIncFailClick
    end
    object MenuExcFail: TMenuItem
      Caption = 'Exclude inherited failure events'
      OnClick = MenuExcFailClick
    end
  end
  object PopupSystem: TPopupMenu
    Left = 248
    Top = 208
    object MenuSuccess: TMenuItem
      Caption = 'Audit success'
      OnClick = MenuSuccessClick
    end
    object MenuFailure: TMenuItem
      Caption = 'Audit faillure'
      OnClick = MenuFailureClick
    end
  end
end
