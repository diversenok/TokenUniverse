object DialogAccess: TDialogAccess
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select desired access'
  ClientHeight = 307
  ClientWidth = 409
  Color = clBtnFace
  Constraints.MinHeight = 180
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
  object GroupBoxMode: TGroupBox
    Left = 262
    Top = 3
    Width = 140
    Height = 267
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Access Mode '
    TabOrder = 0
    object RadioButtonSpecial: TRadioButton
      Left = 11
      Top = 47
      Width = 126
      Height = 17
      Caption = 'The speci&fied'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonSame: TRadioButton
      Left = 11
      Top = 24
      Width = 126
      Height = 17
      Caption = 'Same as &source'
      TabOrder = 1
      TabStop = True
    end
  end
  object ButtonOK: TButton
    Left = 262
    Top = 275
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 335
    Top = 275
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBoxAccess: TGroupBox
    Left = 4
    Top = 3
    Width = 252
    Height = 301
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Access Mask '
    TabOrder = 3
    inline AccessMaskFrame: TBitsFrame
      AlignWithMargins = True
      Left = 2
      Top = 18
      Width = 248
      Height = 281
      Margins.Left = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      inherited Tree: TDevirtualizedTree
        Width = 248
        Height = 253
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
            Position = 0
            Text = 'Name'
            Width = 244
          end>
      end
      inherited BottomPanel: TPanel
        Top = 253
        Width = 248
        inherited tbxValue: TEdit
          Width = 102
        end
        inherited btnAll: TButton
          Left = 178
        end
      end
    end
  end
end
