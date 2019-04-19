object FrameGroups: TFrameGroups
  Left = 0
  Top = 0
  Width = 388
  Height = 369
  TabOrder = 0
  object ListView: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 382
    Height = 363
    Align = alClient
    Columns = <
      item
        Caption = 'Group name'
        Width = 220
      end
      item
        Caption = 'State'
        Width = 110
      end
      item
        Caption = 'Flags'
        Width = 120
      end>
    FullDrag = True
    GridLines = True
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListViewDblClick
    ClipboardSourceColumn = 0
    ColoringItems = True
  end
end
