object DialogPickUser: TDialogPickUser
  Left = 0
  Top = 0
  Anchors = [akTop]
  BorderIcons = [biSystemMenu]
  Caption = 'Choose user or group'
  ClientHeight = 181
  ClientWidth = 554
  Color = clBtnFace
  Constraints.MinHeight = 220
  Constraints.MinWidth = 570
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
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 473
    Top = 150
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
    Top = 150
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ButtonPick: TButton
    Left = 523
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Use default user selection dialog'
    Anchors = [akTop, akRight]
    ImageIndex = 2
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 5
    OnClick = ButtonPickClick
  end
  object GroupBoxMain: TGroupBox
    Left = 8
    Top = 37
    Width = 278
    Height = 106
    Caption = 'Main attributes '
    TabOrder = 1
    object CheckBoxEnabled: TCheckBox
      Left = 14
      Top = 26
      Width = 130
      Height = 17
      Anchors = [akTop]
      Caption = '&Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBoxEnabledClick
    end
    object CheckBoxEnabledByDafault: TCheckBox
      Left = 14
      Top = 49
      Width = 130
      Height = 17
      Anchors = [akTop]
      Caption = 'Enabled by &default'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBoxEnabledByDafaultClick
    end
    object CheckBoxMandatory: TCheckBox
      Left = 150
      Top = 26
      Width = 120
      Height = 17
      Anchors = [akTop]
      Caption = '&Mandatory'
      TabOrder = 3
      OnClick = CheckBoxMandatoryClick
    end
    object CheckBoxDenyOnly: TCheckBox
      Left = 150
      Top = 49
      Width = 120
      Height = 17
      Anchors = [akTop]
      Caption = '&Use for deny only'
      TabOrder = 4
      OnClick = CheckBoxDenyOnlyClick
    end
    object CheckBoxOwner: TCheckBox
      Left = 14
      Top = 72
      Width = 130
      Height = 17
      Anchors = [akTop]
      Caption = '&Owner'
      TabOrder = 2
    end
  end
  object GroupBoxAdditional: TGroupBox
    Left = 292
    Top = 37
    Width = 253
    Height = 106
    Caption = 'Additional attributes '
    TabOrder = 2
    object CheckBoxIntegrityEnabled: TCheckBox
      Left = 16
      Top = 48
      Width = 114
      Height = 17
      Anchors = [akTop]
      Caption = 'Integrity Enabled'
      TabOrder = 1
    end
    object CheckBoxIntegrity: TCheckBox
      Left = 16
      Top = 25
      Width = 114
      Height = 17
      Anchors = [akTop]
      Caption = 'Integrity'
      TabOrder = 0
    end
    object CheckBoxResource: TCheckBox
      Left = 134
      Top = 25
      Width = 104
      Height = 17
      Anchors = [akTop]
      Caption = 'Resource'
      TabOrder = 3
    end
    object CheckBoxLogon: TCheckBox
      Left = 134
      Top = 48
      Width = 104
      Height = 17
      Anchors = [akTop]
      Caption = 'Logon ID'
      TabOrder = 4
    end
    object ButtonIntegrity: TButton
      Left = 14
      Top = 71
      Width = 105
      Height = 25
      Anchors = [akTop]
      Caption = 'Choose &Intrgirty'
      TabOrder = 2
      OnClick = ButtonIntegrityClick
    end
    object ButtonLogonSID: TButton
      Left = 134
      Top = 71
      Width = 107
      Height = 25
      Hint = 'Copy the logon SID from the current desktop'
      Anchors = [akTop]
      Caption = 'Current &Logon SID'
      TabOrder = 5
      OnClick = ButtonLogonSIDClick
    end
  end
  object ComboBoxSID: TEdit
    Left = 8
    Top = 8
    Width = 505
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
