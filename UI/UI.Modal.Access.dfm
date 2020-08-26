object DialogAccess: TDialogAccess
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select desired access'
  ClientHeight = 248
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
    Height = 208
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
    Top = 216
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
    Top = 216
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
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Access Mask '
    TabOrder = 3
    inline AccessMaskFrame: TAccessMaskFrame
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 242
      Height = 219
      Align = alClient
      Constraints.MinHeight = 200
      Constraints.MinWidth = 180
      DoubleBuffered = True
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      inherited ListViewEx: TListViewEx
        Width = 242
        Height = 190
      end
      inherited Panel: TPanel
        Top = 190
        Width = 242
        inherited ButtonFull: TButton
          Left = 172
        end
        inherited EditMask: TEdit
          Width = 94
          Constraints.MinWidth = 32
        end
      end
    end
  end
end
