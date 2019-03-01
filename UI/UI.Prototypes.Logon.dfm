object FrameLogon: TFrameLogon
  Left = 0
  Top = 0
  Width = 388
  Height = 369
  TabOrder = 0
  object ListView: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 40
    Width = 382
    Height = 326
    Margins.Top = 40
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Width = 140
      end
      item
        Width = 220
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
    Items.ItemData = {
      05A40200000E00000000000000FFFFFFFFFFFFFFFF0000000000000000000000
      00084C006F0067006F006E0020004900440000000000FFFFFFFFFFFFFFFF0000
      0000000000000000000009550073006500720020004E0061006D006500000000
      00FFFFFFFFFFFFFFFF0000000000000000000000001641007500740068006500
      6E007400690063006100740069006F006E0020005000610063006B0061006700
      650000000000FFFFFFFFFFFFFFFF0000000000000000000000000C4C006F0067
      006F006E00200053006500720076006500720000000000FFFFFFFFFFFFFFFF00
      00000000000000000000000A4C006F0067006F006E0020005400790070006500
      00000000FFFFFFFFFFFFFFFF0000000000000000000000000753006500730073
      0069006F006E0000000000FFFFFFFFFFFFFFFF0000000000000000000000000A
      4C006F0067006F006E002000540069006D00650000000000FFFFFFFFFFFFFFFF
      000000000100000000000000084C006F0067006F006E00200049004400000000
      00FFFFFFFFFFFFFFFF0000000001000000000000000955007300650072002000
      4E0061006D00650000000000FFFFFFFFFFFFFFFF000000000100000000000000
      16410075007400680065006E007400690063006100740069006F006E00200050
      00610063006B0061006700650000000000FFFFFFFFFFFFFFFF00000000010000
      00000000000C4C006F0067006F006E0020005300650072007600650072000000
      0000FFFFFFFFFFFFFFFF0000000001000000000000000A4C006F0067006F006E
      002000540079007000650000000000FFFFFFFFFFFFFFFF000000000100000000
      00000007530065007300730069006F006E0000000000FFFFFFFFFFFFFFFF0000
      000001000000000000000A4C006F0067006F006E002000540069006D006500}
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
    Top = 9
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
    Top = 12
    Width = 89
    Height = 17
    Caption = 'Originating logon:'
    TabOrder = 2
  end
  object BtnSetOrigin: TButton
    Left = 361
    Top = 8
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
