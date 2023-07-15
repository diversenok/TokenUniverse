object DialogAccessAndType: TDialogAccessAndType
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select access rights and token type'
  ClientHeight = 316
  ClientWidth = 395
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 370
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxType: TGroupBox
    Left = 249
    Top = 3
    Width = 140
    Height = 251
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Token Type '
    TabOrder = 0
    object RadioButtonPrimary: TRadioButton
      Tag = 4
      Left = 16
      Top = 118
      Width = 113
      Height = 17
      Caption = '&Primary Token'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonClick
    end
    object RadioButtonAnonymous: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = '&Anonymous'
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object RadioButtonIdentification: TRadioButton
      Tag = 1
      Left = 16
      Top = 48
      Width = 113
      Height = 17
      Caption = 'I&dentification'
      TabOrder = 2
      OnClick = RadioButtonClick
    end
    object RadioButtonImpersonation: TRadioButton
      Tag = 2
      Left = 16
      Top = 71
      Width = 113
      Height = 17
      Caption = '&Impersonation'
      TabOrder = 3
      OnClick = RadioButtonClick
    end
    object RadioButtonDelegation: TRadioButton
      Tag = 3
      Left = 16
      Top = 95
      Width = 113
      Height = 17
      Caption = 'Dele&gation'
      DoubleBuffered = False
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = RadioButtonClick
    end
  end
  object ButtonOK: TButton
    Left = 249
    Top = 283
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 323
    Top = 283
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object CheckBoxEffective: TCheckBox
    Left = 257
    Top = 260
    Width = 132
    Height = 17
    Hint = 'Duplicate only currently enabled parts of the token.'
    Anchors = [akRight, akBottom]
    Caption = 'Copy &effective only'
    TabOrder = 1
  end
  object GroupBoxAccess: TGroupBox
    Left = 4
    Top = 3
    Width = 239
    Height = 309
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Access Mask '
    TabOrder = 4
    inline AccessMaskFrame: TBitsFrame
      AlignWithMargins = True
      Left = 2
      Top = 18
      Width = 235
      Height = 289
      Margins.Left = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      inherited Tree: TDevirtualizedTree
        Width = 235
        Height = 261
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 0
            Text = 'Name'
            Width = 231
          end>
      end
      inherited BottomPanel: TPanel
        Top = 261
        Width = 235
        inherited tbxValue: TEdit
          Width = 89
        end
        inherited btnAll: TButton
          Left = 165
        end
      end
    end
  end
end
