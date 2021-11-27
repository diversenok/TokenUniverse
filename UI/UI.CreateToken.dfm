object DialogCreateToken: TDialogCreateToken
  Left = 0
  Top = 0
  Caption = 'Create new token'
  ClientHeight = 406
  ClientWidth = 400
  Color = clBtnFace
  Constraints.MinHeight = 370
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
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 241
    Top = 376
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
    Left = 322
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 394
    Height = 370
    Margins.Bottom = 33
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 2
    object TabGeneral: TTabSheet
      Caption = 'General'
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
      object ComboLogonSession: TComboBox
        Left = 114
        Top = 84
        Width = 267
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'Choose a logon session'
      end
      object ComboOwner: TComboBox
        Left = 114
        Top = 111
        Width = 267
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 4
        Text = '< Same as user >'
        Items.Strings = (
          '< Same as user >')
      end
      object ComboPrimary: TComboBox
        Left = 114
        Top = 138
        Width = 267
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 5
        Text = '< Same as user >'
        Items.Strings = (
          '< Same as user >')
      end
      object GroupBoxUser: TGroupBox
        Left = 3
        Top = 3
        Width = 378
        Height = 70
        Anchors = [akLeft, akTop, akRight]
        Caption = 'User '
        TabOrder = 6
        object ButtonPickUser: TButton
          Left = 346
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
          Width = 331
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = ComboUserChange
        end
        object CheckBoxUserState: TCheckBox
          Left = 9
          Top = 44
          Width = 331
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Use for deny only'
          TabOrder = 2
        end
      end
      object GroupBoxPostCreation: TGroupBox
        Left = 5
        Top = 239
        Width = 378
        Height = 100
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'After creation: '
        TabOrder = 7
        object CheckBoxNoWriteUp: TCheckBox
          Left = 7
          Top = 24
          Width = 331
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable No Write Up policy'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object CheckBoxNewProcMin: TCheckBox
          Left = 7
          Top = 47
          Width = 331
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable New Process Min policy'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object CheckBoxSession: TCheckBox
          Left = 7
          Top = 70
          Width = 331
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Change session to current'
          TabOrder = 2
        end
      end
    end
    object TabGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      object ButtonAddSID: TButton
        Left = 152
        Top = 315
        Width = 78
        Height = 25
        Anchors = [akBottom]
        Caption = 'Add SID'
        ImageIndex = 1
        ImageMargins.Left = 3
        ImageMargins.Top = 1
        Images = FormMain.SmallIcons
        TabOrder = 0
        OnClick = ButtonAddSIDClick
      end
      inline GroupsFrame: TFrameGroups
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 380
        Height = 307
        Margins.Bottom = 32
        Align = alClient
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        inherited VST: TVirtualStringTreeEx
          Width = 380
          Height = 307
          NodePopupMenu = PopupMenuGroups
        end
      end
    end
    object TabPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 2
      inline PrivilegesFrame: TPrivilegesFrame
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 380
        Height = 336
        Align = alClient
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        inherited ListViewEx: TListViewEx
          Width = 380
          Height = 336
          Checkboxes = True
          PopupMenu = PopupMenuPrivileges
        end
      end
    end
    object TabAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 3
      object GroupBoxExpires: TGroupBox
        Left = 3
        Top = 97
        Width = 378
        Height = 82
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Expiration Time '
        TabOrder = 0
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
          Width = 253
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Date = 401769.000000000000000000
          Time = 401769.000000000000000000
          Enabled = False
          TabOrder = 1
        end
        object TimeExpires: TDateTimePicker
          Left = 268
          Top = 47
          Width = 103
          Height = 21
          Anchors = [akTop, akRight]
          Time = 0.500000000000000000
          ShowCheckbox = True
          Enabled = False
          Kind = dtkTime
          TabOrder = 2
        end
      end
      object GroupBoxSource: TGroupBox
        Left = 3
        Top = 15
        Width = 190
        Height = 76
        Caption = 'Token Source '
        TabOrder = 1
        object EditSourceName: TEdit
          Left = 69
          Top = 21
          Width = 84
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 8
          TabOrder = 0
          Text = 'TOK_UNIV'
        end
        object StaticSourceName: TStaticText
          Left = 12
          Top = 25
          Width = 35
          Height = 17
          Caption = 'Name:'
          TabOrder = 1
        end
        object StaticSourceLuid: TStaticText
          Left = 12
          Top = 52
          Width = 31
          Height = 17
          Caption = 'LUID:'
          TabOrder = 2
        end
        object EditSourceLuid: TEdit
          Left = 69
          Top = 48
          Width = 84
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          Text = '0'
        end
        object ButtonAllocLuid: TButton
          Left = 159
          Top = 46
          Width = 25
          Height = 23
          Hint = 'Allocate new Locally Unique Identifier'
          Anchors = [akLeft, akTop, akRight]
          ImageIndex = 4
          ImageMargins.Left = 2
          ImageMargins.Top = 1
          Images = FormMain.SmallIcons
          TabOrder = 4
          OnClick = ButtonAllocLuidClick
        end
      end
    end
    object TabDefaltDacl: TTabSheet
      Caption = 'Default DACL'
      ImageIndex = 4
    end
  end
  object ButtonLoad: TButton
    Left = 3
    Top = 376
    Width = 84
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load from...'
    TabOrder = 3
    OnClick = ButtonLoadClick
  end
  object PopupMenuGroups: TPopupMenu
    Left = 256
    Top = 248
    object MenuEdit: TMenuItem
      Caption = 'Edit'
      ShortCut = 113
      OnClick = MenuEditClick
    end
    object MenuRemove: TMenuItem
      Caption = 'Remove'
      ShortCut = 46
      OnClick = MenuRemoveClick
    end
  end
  object PopupMenuPrivileges: TPopupMenu
    Left = 143
    Top = 251
    object MenuDisabled: TMenuItem
      Caption = 'Disabled'
      ShortCut = 16452
      OnClick = MenuDisabledClick
    end
    object MenuDisabledModif: TMenuItem
      Caption = 'Disabled (modified)'
      ShortCut = 24644
      OnClick = MenuDisabledModifClick
    end
    object MenuEnabled: TMenuItem
      Caption = 'Enabled'
      ShortCut = 16453
      OnClick = MenuEnabledClick
    end
    object MenuEnabledModif: TMenuItem
      Caption = 'Enabled (modified)'
      ShortCut = 24645
      OnClick = MenuEnabledModifClick
    end
  end
end
