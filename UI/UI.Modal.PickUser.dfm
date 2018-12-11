object DialogPickUser: TDialogPickUser
  Left = 0
  Top = 0
  Anchors = [akTop]
  BorderIcons = [biSystemMenu]
  Caption = 'Choose user or group'
  ClientHeight = 262
  ClientWidth = 294
  Color = clBtnFace
  Constraints.MinHeight = 225
  Constraints.MinWidth = 310
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    294
    262)
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBoxSID: TComboBox
    Left = 8
    Top = 10
    Width = 218
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ComboBoxSIDChange
  end
  object ButtonFilter: TButton
    Left = 232
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Filter suggestions'
    Anchors = [akTop, akRight]
    ImageIndex = 3
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 5
  end
  object ButtonOK: TButton
    Left = 213
    Top = 231
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 231
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ButtonPick: TButton
    Left = 263
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Use default user selection dialog'
    Anchors = [akTop, akRight]
    ImageIndex = 2
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 6
    OnClick = ButtonPickClick
  end
  object GroupBoxMain: TGroupBox
    Left = 8
    Top = 37
    Width = 278
    Height = 100
    Caption = 'Main attributes '
    TabOrder = 1
    DesignSize = (
      278
      100)
    object CheckBoxEnabled: TCheckBox
      Left = 14
      Top = 26
      Width = 130
      Height = 17
      Anchors = [akTop]
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxEnabledByDafault: TCheckBox
      Left = 14
      Top = 49
      Width = 130
      Height = 17
      Anchors = [akTop]
      Caption = 'Enabled by default'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxMandatory: TCheckBox
      Left = 150
      Top = 26
      Width = 120
      Height = 17
      Anchors = [akTop]
      Caption = 'Mandatory'
      TabOrder = 3
    end
    object CheckBoxDenyOnly: TCheckBox
      Left = 150
      Top = 49
      Width = 120
      Height = 17
      Anchors = [akTop]
      Caption = 'Use for deny only'
      TabOrder = 4
    end
    object CheckBoxOwner: TCheckBox
      Left = 14
      Top = 72
      Width = 130
      Height = 17
      Anchors = [akTop]
      Caption = 'Owner'
      TabOrder = 2
    end
  end
  object GroupBoxAdditional: TGroupBox
    Left = 8
    Top = 143
    Width = 278
    Height = 82
    Caption = 'Additional attributes '
    TabOrder = 2
    DesignSize = (
      278
      82)
    object CheckBoxIntegrityEnabled: TCheckBox
      Left = 14
      Top = 48
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Integrity Enabled'
      TabOrder = 1
    end
    object CheckBoxIntegrity: TCheckBox
      Left = 14
      Top = 25
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Integrity'
      TabOrder = 0
    end
    object CheckBoxResource: TCheckBox
      Left = 142
      Top = 48
      Width = 120
      Height = 17
      Anchors = [akTop]
      Caption = 'Resource'
      TabOrder = 3
    end
    object CheckBoxLogon: TCheckBox
      Left = 142
      Top = 25
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Logon ID'
      TabOrder = 2
    end
  end
end
