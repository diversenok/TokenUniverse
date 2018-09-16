object DialogPickUser: TDialogPickUser
  Left = 0
  Top = 0
  Anchors = [akTop]
  BorderIcons = [biSystemMenu]
  Caption = 'Choose group or user'
  ClientHeight = 219
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  DesignSize = (
    375
    219)
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBoxSID: TComboBox
    Left = 8
    Top = 8
    Width = 299
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ComboBoxSIDChange
  end
  object ButtonFilter: TButton
    Left = 313
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Filter suggestions'
    Anchors = [akTop, akRight]
    ImageIndex = 3
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 294
    Top = 188
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 188
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ButtonPick: TButton
    Left = 344
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Use default user selection dialog'
    Anchors = [akTop, akRight]
    ImageIndex = 2
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 4
    OnClick = ButtonPickClick
  end
  object GroupBoxAttributes: TGroupBox
    Left = 8
    Top = 35
    Width = 359
    Height = 147
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Group attributes '
    TabOrder = 5
    DesignSize = (
      359
      147)
    object CheckBoxMandatory: TCheckBox
      Left = 198
      Top = 24
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Mandatory'
      TabOrder = 0
    end
    object CheckBoxDenyOnly: TCheckBox
      Left = 198
      Top = 47
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Use for deny only'
      TabOrder = 1
    end
    object CheckBoxOwner: TCheckBox
      Left = 198
      Top = 70
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Owner'
      TabOrder = 2
    end
    object CheckBoxResource: TCheckBox
      Left = 198
      Top = 116
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Resource'
      TabOrder = 3
    end
    object CheckBoxEnabled: TCheckBox
      Left = 16
      Top = 24
      Width = 150
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxEnabledByDafault: TCheckBox
      Left = 16
      Top = 47
      Width = 150
      Height = 17
      Caption = 'Enabled by default'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxIntegrity: TCheckBox
      Left = 16
      Top = 70
      Width = 150
      Height = 17
      Caption = 'Integrity'
      TabOrder = 6
    end
    object CheckBoxIntegrityEnabled: TCheckBox
      Left = 16
      Top = 93
      Width = 150
      Height = 17
      Caption = 'Integrity Enabled'
      TabOrder = 7
    end
    object CheckBoxLogon: TCheckBox
      Left = 198
      Top = 93
      Width = 150
      Height = 17
      Anchors = [akTop]
      Caption = 'Logon ID'
      TabOrder = 8
    end
  end
end
