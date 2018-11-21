object DialogAccessAndType: TDialogAccessAndType
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select access rights and token type'
  ClientHeight = 212
  ClientWidth = 354
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
  DesignSize = (
    354
    212)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxType: TGroupBox
    Left = 208
    Top = 3
    Width = 140
    Height = 149
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Token Type '
    TabOrder = 0
    DesignSize = (
      140
      149)
    object RadioButtonPrimary: TRadioButton
      Tag = 4
      Left = 16
      Top = 118
      Width = 113
      Height = 17
      Anchors = [akLeft]
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
      Anchors = [akLeft]
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
      Anchors = [akLeft]
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
      Anchors = [akLeft]
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
      Anchors = [akLeft]
      Caption = 'Dele&gation'
      DoubleBuffered = False
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = RadioButtonClick
    end
  end
  object ButtonOK: TButton
    Left = 208
    Top = 181
    Width = 66
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object StaticTextAccess: TStaticText
    Left = 5
    Top = 3
    Width = 197
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Access rights'
    TabOrder = 5
  end
  object ButtonCancel: TButton
    Left = 282
    Top = 181
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
    Width = 197
    Height = 180
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
  object CheckBoxEffective: TCheckBox
    Left = 216
    Top = 158
    Width = 132
    Height = 17
    Hint = 'Duplicate only currently enabled parts of the token.'
    Anchors = [akRight, akBottom]
    Caption = 'Copy &effective only'
    TabOrder = 1
  end
end
