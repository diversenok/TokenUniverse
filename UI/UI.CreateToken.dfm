object DialogCreateToken: TDialogCreateToken
  Left = 0
  Top = 0
  Caption = 'Create new token'
  ClientHeight = 428
  ClientWidth = 572
  Color = clBtnFace
  Constraints.MinHeight = 467
  Constraints.MinWidth = 520
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
    Left = 413
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 494
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 566
    Height = 392
    Margins.Bottom = 33
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 0
    object TabGeneral: TTabSheet
      Caption = 'General'
      object StaticLogonID: TStaticText
        Left = 12
        Top = 92
        Width = 76
        Height = 17
        Caption = 'Logon Session:'
        TabOrder = 7
      end
      object StaticOwner: TStaticText
        Left = 12
        Top = 119
        Width = 40
        Height = 17
        Caption = 'Owner:'
        TabOrder = 8
      end
      object StaticPrimaryGroup: TStaticText
        Left = 12
        Top = 146
        Width = 75
        Height = 17
        Caption = 'Primary group:'
        TabOrder = 9
      end
      object ComboLogonSession: TComboBox
        Left = 114
        Top = 88
        Width = 439
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'Choose a logon session'
      end
      object ComboOwner: TComboBox
        Left = 114
        Top = 115
        Width = 439
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DoubleBuffered = False
        ItemIndex = 0
        ParentDoubleBuffered = False
        TabOrder = 2
        Text = '< Same as user >'
        Items.Strings = (
          '< Same as user >')
      end
      object ComboPrimary: TComboBox
        Left = 114
        Top = 142
        Width = 439
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DoubleBuffered = False
        ItemIndex = 0
        ParentDoubleBuffered = False
        TabOrder = 3
        Text = '< Same as user >'
        Items.Strings = (
          '< Same as user >')
      end
      object GroupBoxUser: TGroupBox
        Left = 3
        Top = 8
        Width = 550
        Height = 70
        Anchors = [akLeft, akTop, akRight]
        Caption = 'User '
        TabOrder = 0
        object CheckBoxUserState: TCheckBox
          Left = 9
          Top = 45
          Width = 532
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Use for deny only'
          TabOrder = 1
        end
        object ButtonPickUser: TButton
          Left = 372
          Top = 15
          Width = 25
          Height = 25
          Hint = 'Use default user selection dialog'
          Anchors = [akTop, akRight]
          ImageIndex = 2
          ImageMargins.Left = 2
          ImageMargins.Top = 1
          Images = FormMain.SmallIcons
          TabOrder = 2
        end
        inline SidEditor: TSidEditor
          Left = 9
          Top = 15
          Width = 532
          Height = 27
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          inherited tbxSid: TEdit
            Width = 475
          end
          inherited btnDsPicker: TButton
            Left = 507
          end
          inherited btnCheatsheet: TButton
            Left = 479
          end
        end
      end
      object GroupBoxPostCreation: TGroupBox
        Left = 5
        Top = 261
        Width = 550
        Height = 100
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'After creation: '
        TabOrder = 6
        object CheckBoxNoWriteUp: TCheckBox
          Left = 7
          Top = 24
          Width = 503
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
          Width = 503
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
          Width = 503
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Change session to current'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
      object GroupBoxSource: TGroupBox
        Left = 5
        Top = 173
        Width = 253
        Height = 82
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Token Source '
        TabOrder = 4
        object EditSourceName: TEdit
          Left = 69
          Top = 21
          Width = 147
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
          TabOrder = 3
        end
        object StaticSourceLuid: TStaticText
          Left = 12
          Top = 52
          Width = 31
          Height = 17
          Caption = 'LUID:'
          TabOrder = 4
        end
        object EditSourceLuid: TEdit
          Left = 69
          Top = 48
          Width = 147
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = '0'
        end
        object ButtonAllocLuid: TButton
          Left = 222
          Top = 46
          Width = 25
          Height = 23
          Hint = 'Allocate new Locally Unique Identifier'
          Anchors = [akTop, akRight]
          ImageIndex = 4
          ImageMargins.Left = 2
          ImageMargins.Top = 1
          Images = FormMain.SmallIcons
          TabOrder = 2
          OnClick = ButtonAllocLuidClick
        end
      end
      object GroupBoxExpires: TGroupBox
        Left = 264
        Top = 173
        Width = 291
        Height = 82
        Anchors = [akTop, akRight]
        Caption = 'Expiration Time '
        TabOrder = 5
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
          Width = 166
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Date = 401769.000000000000000000
          Time = 401769.000000000000000000
          Enabled = False
          TabOrder = 1
        end
        object TimeExpires: TDateTimePicker
          Left = 181
          Top = 47
          Width = 103
          Height = 21
          Anchors = [akTop, akRight]
          Date = 44533.000000000000000000
          Time = 0.500000000000000000
          ShowCheckbox = True
          Enabled = False
          Kind = dtkTime
          TabOrder = 2
        end
      end
    end
    object TabGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      object ButtonAddSID: TButton
        Left = 237
        Top = 337
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
        Left = 0
        Top = 0
        Width = 558
        Height = 332
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 32
        Align = alClient
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        inherited VST: TDevirtualizedTree
          Width = 558
          Height = 332
          PopupMenuEx = PopupMenuGroups
        end
      end
    end
    object TabPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 2
      inline PrivilegesFrame: TFramePrivileges
        Left = 0
        Top = 0
        Width = 558
        Height = 364
        Align = alClient
        TabOrder = 0
        inherited VST: TDevirtualizedTree
          Width = 558
          Height = 364
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          PopupMenuEx = PopupMenuPrivileges
        end
      end
    end
  end
  object ButtonLoad: TButton
    Left = 3
    Top = 398
    Width = 84
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load from...'
    TabOrder = 1
    OnClick = ButtonLoadClick
  end
  object PopupMenuGroups: TPopupMenu
    Left = 496
    Top = 312
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
    Left = 391
    Top = 307
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
  end
end
