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
    PopupMenu = AuditPopup
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
  object AuditPopup: TPopupMenu
    Left = 231
    Top = 213
    object AuditIncSucc: TMenuItem
      Caption = 'Include success events'
      OnClick = AuditIncSuccClick
    end
    object AuditExcSucc: TMenuItem
      Caption = 'Exclude inherited success events'
      OnClick = AuditExcSuccClick
    end
    object AuditIncFail: TMenuItem
      Caption = 'Include failure events'
      OnClick = AuditIncFailClick
    end
    object AuditExcFail: TMenuItem
      Caption = 'Exclude inherited failure events'
      OnClick = AuditExcFailClick
    end
  end
end
