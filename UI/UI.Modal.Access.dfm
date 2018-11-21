object DialogAccess: TDialogAccess
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select desired access'
  ClientHeight = 212
  ClientWidth = 354
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
  object GroupBox1: TGroupBox
    Left = 207
    Top = 3
    Width = 140
    Height = 173
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Access mode '
    TabOrder = 0
    object RadioButtonSpecial: TRadioButton
      Left = 16
      Top = 70
      Width = 130
      Height = 17
      Caption = 'The speci&fied'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object RadioButtonMaximum: TRadioButton
      Left = 16
      Top = 47
      Width = 130
      Height = 17
      Caption = '&Maximum allowed'
      TabOrder = 1
      TabStop = True
    end
    object RadioButtonSame: TRadioButton
      Left = 16
      Top = 24
      Width = 130
      Height = 17
      Caption = 'Same as &source'
      TabOrder = 0
      TabStop = True
    end
  end
  object StaticTextAccess: TStaticText
    Left = 5
    Top = 3
    Width = 196
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Access rights'
    TabOrder = 4
  end
  object ListViewAccess: TListViewEx
    Left = 5
    Top = 24
    Width = 196
    Height = 182
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Width = 160
      end>
    MultiSelect = True
    GroupView = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListViewAccessChange
  end
  object ButtonOK: TButton
    Left = 207
    Top = 182
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 281
    Top = 182
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
