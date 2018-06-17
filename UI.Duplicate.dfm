object DuplicateDialog: TDuplicateDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Duplication options'
  ClientHeight = 183
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    360
    183)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxType: TGroupBox
    Left = 214
    Top = 2
    Width = 140
    Height = 145
    Anchors = [akTop, akRight]
    Caption = 'Token Type '
    TabOrder = 0
    ExplicitLeft = 228
    object RadioButtonPrimary: TRadioButton
      Tag = 4
      Left = 16
      Top = 115
      Width = 113
      Height = 17
      Caption = 'Primary token'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonClick
    end
    object RadioButtonAnonymous: TRadioButton
      Left = 16
      Top = 23
      Width = 113
      Height = 17
      Caption = 'Anonymous'
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object RadioButtonIdentification: TRadioButton
      Tag = 1
      Left = 16
      Top = 46
      Width = 113
      Height = 17
      Caption = 'Identification'
      TabOrder = 2
      OnClick = RadioButtonClick
    end
    object RadioButtonImpersonation: TRadioButton
      Tag = 2
      Left = 16
      Top = 69
      Width = 113
      Height = 17
      Caption = 'Impersonation'
      TabOrder = 3
      OnClick = RadioButtonClick
    end
    object RadioButtonDelegation: TRadioButton
      Tag = 3
      Left = 16
      Top = 92
      Width = 113
      Height = 17
      Caption = 'Delegation'
      DoubleBuffered = False
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = RadioButtonClick
    end
  end
  object ButtonOK: TButton
    Left = 288
    Top = 153
    Width = 66
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 302
  end
  object StaticTextAccess: TStaticText
    Left = 5
    Top = 3
    Width = 203
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Access rights'
    TabOrder = 2
    ExplicitWidth = 209
  end
  object ListBoxAccess: TCheckListBox
    Left = 5
    Top = 20
    Width = 203
    Height = 158
    Anchors = [akLeft, akTop, akRight, akBottom]
    Flat = False
    ItemHeight = 18
    Style = lbOwnerDrawFixed
    TabOrder = 3
    ExplicitWidth = 209
    ExplicitHeight = 157
  end
  object ButtonCancel: TButton
    Left = 214
    Top = 153
    Width = 66
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ExplicitLeft = 228
  end
end
