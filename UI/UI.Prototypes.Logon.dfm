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
    Height = 298
    Margins.Bottom = 68
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
    Top = 312
    Width = 244
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    Text = 'Unknown Originating logon session'
    OnChange = ComboOriginChange
  end
  object StaticOrigin: TStaticText
    Left = 3
    Top = 315
    Width = 92
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Originating Logon:'
    TabOrder = 2
  end
  object BtnSetOrigin: TButton
    Left = 361
    Top = 311
    Width = 24
    Height = 23
    Anchors = [akRight, akBottom]
    ImageAlignment = iaCenter
    ImageIndex = 5
    Images = FormMain.SmallIcons
    TabOrder = 3
    OnClick = BtnSetOriginClick
  end
  object CheckBoxReference: TCheckBox
    Left = 112
    Top = 342
    Width = 244
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Reference logon session'
    State = cbGrayed
    TabOrder = 4
    OnClick = CheckBoxReferenceClick
  end
  object BtnSetRef: TButton
    Left = 361
    Top = 339
    Width = 24
    Height = 23
    Anchors = [akTop, akRight]
    ImageAlignment = iaCenter
    ImageIndex = 5
    Images = FormMain.SmallIcons
    TabOrder = 5
    OnClick = BtnSetRefClick
  end
end
