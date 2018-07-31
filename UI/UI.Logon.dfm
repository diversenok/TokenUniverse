object LogonDialog: TLogonDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Logon user'
  ClientHeight = 139
  ClientWidth = 196
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  DesignSize = (
    196
    139)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelType: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Logon type:'
  end
  object LabelProvider: TLabel
    Left = 8
    Top = 57
    Width = 76
    Height = 13
    Caption = 'Logon provider:'
  end
  object ComboLogonType: TComboBox
    Left = 8
    Top = 27
    Width = 180
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 0
    Text = 'Interactive'
    Items.Strings = (
      'Interactive'
      'Batch'
      'Network'
      'Network clear text'
      'New credentials'
      'Service')
  end
  object ComboLogonProvider: TComboBox
    Left = 8
    Top = 76
    Width = 180
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 1
    Text = 'Default'
    Items.Strings = (
      'Default'
      'Negotiate'
      'NTLM'
      'Windows NT 3.5')
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 107
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonContinue: TButton
    Left = 113
    Top = 107
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Continue'
    Default = True
    TabOrder = 3
    OnClick = ButtonContinueClick
  end
end
