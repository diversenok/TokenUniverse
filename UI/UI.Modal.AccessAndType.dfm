object DialogAccessAndType: TDialogAccessAndType
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select access rights and token type'
  ClientHeight = 196
  ClientWidth = 351
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    351
    196)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxType: TGroupBox
    Left = 205
    Top = 3
    Width = 140
    Height = 156
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Token Type '
    TabOrder = 0
    DesignSize = (
      140
      156)
    object RadioButtonPrimary: TRadioButton
      Tag = 4
      Left = 16
      Top = 124
      Width = 113
      Height = 17
      Anchors = [akLeft]
      Caption = 'Primary Token'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonClick
    end
    object RadioButtonAnonymous: TRadioButton
      Left = 16
      Top = 25
      Width = 113
      Height = 17
      Anchors = [akLeft]
      Caption = 'Anonymous'
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object RadioButtonIdentification: TRadioButton
      Tag = 1
      Left = 16
      Top = 50
      Width = 113
      Height = 17
      Anchors = [akLeft]
      Caption = 'Identification'
      TabOrder = 2
      OnClick = RadioButtonClick
    end
    object RadioButtonImpersonation: TRadioButton
      Tag = 2
      Left = 16
      Top = 75
      Width = 113
      Height = 17
      Anchors = [akLeft]
      Caption = 'Impersonation'
      TabOrder = 3
      OnClick = RadioButtonClick
    end
    object RadioButtonDelegation: TRadioButton
      Tag = 3
      Left = 16
      Top = 100
      Width = 113
      Height = 17
      Anchors = [akLeft]
      Caption = 'Delegation'
      DoubleBuffered = False
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = RadioButtonClick
    end
  end
  object ButtonOK: TButton
    Left = 205
    Top = 165
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object StaticTextAccess: TStaticText
    Left = 5
    Top = 3
    Width = 194
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Access rights'
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 279
    Top = 165
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ListViewAccess: TListViewEx
    Left = 5
    Top = 24
    Width = 194
    Height = 164
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
    TabOrder = 4
    ViewStyle = vsReport
  end
end
