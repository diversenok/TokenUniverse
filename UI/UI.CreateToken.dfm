object DialogCreateToken: TDialogCreateToken
  Left = 0
  Top = 0
  Caption = 'Create new token'
  ClientHeight = 403
  ClientWidth = 328
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 344
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    328
    403)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 169
    Top = 372
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 250
    Top = 373
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 322
    Height = 367
    Margins.Bottom = 33
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 2
    object TabGeneral: TTabSheet
      Caption = 'General'
      DesignSize = (
        314
        339)
      object StaticLogonID: TStaticText
        Left = 12
        Top = 88
        Width = 76
        Height = 17
        Caption = 'Logon Session:'
        TabOrder = 0
      end
      object StaticOwner: TStaticText
        Left = 12
        Top = 115
        Width = 40
        Height = 17
        Caption = 'Owner:'
        TabOrder = 1
      end
      object StaticPrimaryGroup: TStaticText
        Left = 12
        Top = 142
        Width = 75
        Height = 17
        Caption = 'Primary group:'
        TabOrder = 2
      end
      object StaticDacl: TStaticText
        Left = 12
        Top = 200
        Width = 72
        Height = 17
        Caption = 'Default DACL:'
        TabOrder = 3
      end
      object StaticSource: TStaticText
        Left = 12
        Top = 169
        Width = 41
        Height = 17
        Caption = 'Source:'
        TabOrder = 4
      end
      object EditSource: TEdit
        Left = 114
        Top = 165
        Width = 84
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxLength = 8
        ParentFont = False
        TabOrder = 5
        Text = 'TOK_UNIV'
      end
      object ComboLogonSession: TComboBox
        Left = 114
        Top = 84
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        Text = 'Choose a logon session'
      end
      object ButtonLoad: TButton
        Left = 3
        Top = 309
        Width = 118
        Height = 27
        Anchors = [akLeft, akBottom]
        Caption = 'Load entries from...'
        TabOrder = 7
      end
      object ComboOwner: TComboBox
        Left = 114
        Top = 111
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
      end
      object ComboPrimary: TComboBox
        Left = 114
        Top = 138
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 9
      end
      object GroupBoxUser: TGroupBox
        Left = 3
        Top = 3
        Width = 308
        Height = 70
        Anchors = [akLeft, akTop, akRight]
        Caption = 'User '
        TabOrder = 10
        DesignSize = (
          308
          70)
        object ButtonPickUser: TButton
          Left = 276
          Top = 15
          Width = 25
          Height = 25
          Hint = 'Use default user selection dialog'
          Anchors = [akTop, akRight]
          ImageIndex = 2
          ImageMargins.Left = 2
          ImageMargins.Top = 1
          Images = FormMain.SmallIcons
          TabOrder = 0
          OnClick = ButtonPickUserClick
        end
        object ComboUser: TComboBox
          Left = 9
          Top = 17
          Width = 261
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
        object CheckBoxUserState: TCheckBox
          Left = 9
          Top = 44
          Width = 261
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Use for deny only'
          TabOrder = 2
        end
      end
    end
    object TabGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      DesignSize = (
        314
        339)
      object ListViewGroups: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 308
        Height = 306
        Margins.Bottom = 30
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
        PopupMenu = PopupMenuGroups
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
        PopupOnItemsOnly = True
      end
      object ButtonAddSID: TButton
        Left = 117
        Top = 312
        Width = 78
        Height = 25
        Anchors = [akBottom]
        Caption = 'Add SID'
        ImageIndex = 1
        ImageMargins.Left = 3
        ImageMargins.Top = 1
        Images = FormMain.SmallIcons
        TabOrder = 1
        OnClick = ButtonAddSIDClick
      end
    end
    object TabPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 2
      object ListViewPrivileges: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 308
        Height = 333
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'Privilege name'
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
        ColoringItems = True
      end
    end
    object TabAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 3
      DesignSize = (
        314
        339)
      object GroupBoxExpires: TGroupBox
        Left = 3
        Top = 16
        Width = 308
        Height = 82
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Expiration Time '
        TabOrder = 0
        DesignSize = (
          308
          82)
        object CheckBoxInfinite: TCheckBox
          Left = 9
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Infinite'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CheckBoxInfiniteClick
        end
        object DateExpires: TDateTimePicker
          Left = 9
          Top = 47
          Width = 198
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Date = 401769.000706018600000000
          Time = 401769.000706018600000000
          Enabled = False
          TabOrder = 1
        end
        object TimeExpires: TDateTimePicker
          Left = 213
          Top = 47
          Width = 88
          Height = 21
          Anchors = [akTop, akRight]
          Date = 43444.500000000000000000
          Time = 43444.500000000000000000
          Enabled = False
          Kind = dtkTime
          TabOrder = 2
        end
      end
    end
  end
  object PopupMenuGroups: TPopupMenu
    Left = 256
    Top = 248
    object MenuEdit: TMenuItem
      Caption = 'Edit'
      OnClick = MenuEditClick
    end
    object MenuRemove: TMenuItem
      Caption = 'Remove'
      OnClick = MenuRemoveClick
    end
  end
end
