object IntegrityPicker: TIntegrityPicker
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Integrity Level Picker'
  ClientHeight = 159
  ClientWidth = 284
  Color = clBtnFace
  Constraints.MinHeight = 198
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblUntrusted: TLabel
    Left = 8
    Top = 103
    Width = 48
    Height = 13
    Caption = 'Untrusted'
  end
  object lblSystem: TLabel
    Left = 241
    Top = 103
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'System'
  end
  object lblMedium: TLabel
    Left = 124
    Top = 103
    Width = 36
    Height = 13
    Alignment = taCenter
    Anchors = [akTop]
    Caption = 'Medium'
  end
  object lblHigh: TLabel
    Left = 194
    Top = 43
    Width = 21
    Height = 13
    Alignment = taCenter
    Anchors = [akTop]
    Caption = 'High'
  end
  object lblLow: TLabel
    Left = 72
    Top = 43
    Width = 19
    Height = 13
    Alignment = taCenter
    Anchors = [akTop]
    Caption = 'Low'
  end
  object TrackBar: TTrackBar
    Left = 8
    Top = 62
    Width = 268
    Height = 35
    Anchors = [akLeft, akTop, akRight]
    LineSize = 512
    Max = 16384
    PageSize = 4096
    Frequency = 4096
    Position = 8192
    ShowSelRange = False
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = TrackBarChange
  end
  object ComboBox: TComboBox
    Left = 8
    Top = 8
    Width = 268
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 2
    TabOrder = 0
    Text = 'Medium (0x2000)'
    OnChange = ComboBoxChange
    Items.Strings = (
      'Untrusted (0x0000)'
      'Low (0x1000)'
      'Medium (0x2000)'
      'Medium Plus (0x2100)'
      'High (0x3000)'
      'System (0x4000)'
      'Protected (0x5000)')
  end
  object btnCancel: TButton
    Left = 8
    Top = 126
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnOk: TButton
    Left = 201
    Top = 126
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
