object SessionDialog: TSessionDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Choose session'
  ClientHeight = 66
  ClientWidth = 255
  Color = clBtnFace
  Constraints.MinHeight = 104
  Constraints.MinWidth = 185
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    255
    66)
  PixelsPerInch = 96
  TextHeight = 13
  object SessionCombo: TSessionComboBox
    Left = 8
    Top = 7
    Width = 239
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'Unable to query. Insert it manually'
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 35
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 174
    Top = 35
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
