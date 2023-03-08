object LogonDialog: TLogonDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Logon user'
  ClientHeight = 558
  ClientWidth = 386
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 266
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblLogonType: TLabel
    Left = 8
    Top = 112
    Width = 60
    Height = 13
    Caption = 'Logon Type:'
  end
  object LabelGroups: TLabel
    Left = 8
    Top = 225
    Width = 370
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Additional groups (requires Tcb privilege)'
  end
  object lblAuthPackage: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Authentication Package:'
  end
  object lblMessageType: TLabel
    Left = 8
    Top = 58
    Width = 73
    Height = 13
    Caption = 'Message Type:'
  end
  object cbxLogonType: TComboBox
    Left = 8
    Top = 131
    Width = 370
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 1
    TabOrder = 2
    Text = 'Network'
    Items.Strings = (
      'Interactive'
      'Network'
      'Batch'
      'Service'
      'Proxy'
      'Unlock'
      'Network Cleartext'
      'New Credentials'
      'Remote Interactive'
      'Cached Interactive'
      'Cached Remote Interactive'
      'Cached Unlock')
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 525
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
    OnClick = ButtonCancelClick
  end
  object ButtonContinue: TButton
    Left = 303
    Top = 525
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Continue'
    Default = True
    TabOrder = 5
    OnClick = ButtonContinueClick
  end
  object ButtonAddSID: TButton
    Left = 155
    Top = 525
    Width = 78
    Height = 25
    Anchors = [akBottom]
    Caption = 'Add SID'
    ImageIndex = 1
    ImageMargins.Left = 3
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 4
    OnClick = ButtonAddSIDClick
  end
  object GroupBoxSource: TGroupBox
    Left = 8
    Top = 165
    Width = 370
    Height = 54
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Token Source : '
    TabOrder = 3
    object EditSourceName: TEdit
      Left = 45
      Top = 21
      Width = 83
      Height = 21
      Hint = 'To mimic the LogonUser API, use "Advapi  ".'
      MaxLength = 8
      TabOrder = 0
      Text = 'TokUniv.'
    end
    object StaticSourceName: TStaticText
      Left = 8
      Top = 24
      Width = 35
      Height = 17
      Caption = 'Name:'
      TabOrder = 1
    end
    object StaticSourceLuid: TStaticText
      Left = 135
      Top = 24
      Width = 31
      Height = 17
      Caption = 'LUID:'
      TabOrder = 2
    end
    object EditSourceLuid: TEdit
      Left = 167
      Top = 21
      Width = 163
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = '0'
    end
    object ButtonAllocLuid: TButton
      Left = 336
      Top = 20
      Width = 25
      Height = 23
      Hint = 'Allocate new Locally Unique Identifier'
      Anchors = [akTop, akRight]
      ImageIndex = 4
      ImageMargins.Left = 2
      ImageMargins.Top = 1
      Images = FormMain.SmallIcons
      TabOrder = 4
      OnClick = ButtonAllocLuidClick
    end
  end
  object GroupsPanel: TPanel
    Left = 8
    Top = 248
    Width = 370
    Height = 271
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 7
    inline GroupsFrame: TFrameGroups
      Left = 0
      Top = 0
      Width = 370
      Height = 271
      Align = alClient
      DoubleBuffered = True
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      inherited VST: TDevirtualizedTree
        Width = 370
        Height = 271
        AccessibleName = '1'
        PopupMenuEx = PopupMenu
        NoItemsText = 'No additional groups'
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 0
            Text = 'Friendly Name'
            Width = 150
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 1
            Text = 'SID'
            Width = 280
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 2
            Text = 'SID Type'
            Width = 110
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 3
            Text = 'State'
            Width = 90
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 4
            Text = 'Flags'
            Width = 90
          end>
      end
    end
  end
  object cbxAuthPackage: TComboBox
    Left = 8
    Top = 27
    Width = 370
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 0
    Text = 'Negotiate'
    Items.Strings = (
      'Negotiate'
      'Microsoft Authentication Package V1.0'
      'Kerberos')
  end
  object cbxMessageType: TComboBox
    Left = 8
    Top = 77
    Width = 370
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 1
    Text = 'Interactive'
    Items.Strings = (
      'Interactive'
      'S4U'
      'Virtual'
      'No Elevation')
  end
  object PopupMenu: TPopupMenu
    Left = 208
    Top = 64
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
end
