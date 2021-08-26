object LogonDialog: TLogonDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Logon user'
  ClientHeight = 388
  ClientWidth = 350
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 266
  DoubleBuffered = True
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
  object LabelType: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Logon type:'
  end
  object LabelGroups: TLabel
    Left = 8
    Top = 116
    Width = 334
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Additional groups (requires Tcb privilege)'
  end
  object ComboLogonType: TComboBox
    Left = 8
    Top = 27
    Width = 334
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 1
    TabOrder = 0
    Text = 'Interactive'
    OnChange = ComboLogonTypeChange
    Items.Strings = (
      'S4U (without a password)'
      'Interactive'
      'Network'
      'Network clear text'
      'New credentials'
      'Unlock'
      'Batch'
      'Service')
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
  object ButtonContinue: TButton
    Left = 267
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Continue'
    Default = True
    TabOrder = 3
    OnClick = ButtonContinueClick
  end
  object ButtonAddSID: TButton
    Left = 137
    Top = 355
    Width = 78
    Height = 25
    Anchors = [akBottom]
    Caption = 'Add SID'
    ImageIndex = 1
    ImageMargins.Left = 3
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 2
    OnClick = ButtonAddSIDClick
  end
  object GroupBoxSource: TGroupBox
    Left = 8
    Top = 54
    Width = 334
    Height = 54
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Token Source '
    TabOrder = 1
    object EditSourceName: TEdit
      Left = 45
      Top = 21
      Width = 83
      Height = 21
      Enabled = False
      MaxLength = 8
      TabOrder = 0
      Text = 'TOK_UNIV'
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
      Width = 127
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 3
      Text = '0'
    end
    object ButtonAllocLuid: TButton
      Left = 300
      Top = 20
      Width = 25
      Height = 23
      Hint = 'Allocate new Locally Unique Identifier'
      Anchors = [akTop, akRight]
      Enabled = False
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
    Top = 137
    Width = 334
    Height = 212
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 5
    inline GroupsFrame: TFrameGroups
      Left = 0
      Top = 0
      Width = 334
      Height = 212
      Align = alClient
      DoubleBuffered = True
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      inherited VST: TVirtualStringTree
        Width = 334
        Height = 212
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
