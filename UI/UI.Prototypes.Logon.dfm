object FrameLogon: TFrameLogon
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
    Height = 332
    Margins.Bottom = 34
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Width = 160
      end
      item
        Width = 200
      end>
    Groups = <
      item
        Header = 'Logon Session'
        GroupID = 0
        State = [lgsNormal, lgsCollapsible]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Originating Logon'
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
    TabOrder = 0
    ViewStyle = vsReport
    ClipboardSourceColumn = 1
  end
  object ComboOrigin: TComboBox
    Left = 112
    Top = 344
    Width = 244
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'Unknown Originating logon session'
    OnChange = ComboOriginChange
  end
  object StaticOrigin: TStaticText
    Left = 3
    Top = 347
    Width = 89
    Height = 17
    Caption = 'Originating logon:'
    TabOrder = 2
  end
  object BtnSetOrigin: TButton
    Left = 361
    Top = 343
    Width = 24
    Height = 23
    Anchors = [akTop, akRight]
    ImageAlignment = iaCenter
    ImageIndex = 5
    Images = FormMain.SmallIcons
    TabOrder = 3
    OnClick = BtnSetOriginClick
  end
end
