object FramePrivileges: TFramePrivileges
  Left = 0
  Top = 0
  Width = 388
  Height = 369
  Align = alClient
  TabOrder = 0
  object ListView: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 382
    Height = 363
    Align = alClient
    AllocBy = 36
    Columns = <
      item
        Caption = 'Name'
        Width = 180
      end
      item
        Caption = 'State'
        Width = 110
      end
      item
        Caption = 'Description'
        Width = 220
      end
      item
        Alignment = taCenter
        Caption = 'LUID'
        Width = 40
      end>
    FullDrag = True
    GridLines = True
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnItemChecked = ListViewItemChecked
    ClipboardSourceColumn = 0
    ColoringItems = True
    PopupOnItemsOnly = True
  end
end
