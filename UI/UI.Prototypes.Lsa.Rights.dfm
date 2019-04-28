object FrameLsaRights: TFrameLsaRights
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
  object ListView: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 314
    Height = 206
    Margins.Bottom = 31
    Align = alClient
    AllocBy = 36
    Checkboxes = True
    Columns = <
      item
        Width = 280
      end>
    FullDrag = True
    Groups = <
      item
        Header = 'Allowing rights'
        GroupID = 0
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Denying rights'
        GroupID = 1
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end>
    MultiSelect = True
    GroupView = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 1
    ViewStyle = vsReport
    OnItemChecked = ListViewItemChecked
    ClipboardSourceColumn = 0
    ColoringItems = True
    PopupOnItemsOnly = True
  end
end
