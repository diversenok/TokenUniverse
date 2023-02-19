object DialogPickUser: TDialogPickUser
  Left = 0
  Top = 0
  Anchors = [akTop]
  BorderIcons = [biSystemMenu]
  Caption = 'Choose user or group'
  ClientHeight = 181
  ClientWidth = 567
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
    Left = 486
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
  object GroupBoxMain: TGroupBox
    Left = 8
    Top = 37
    Width = 278
    Height = 106
    Caption = 'Primary attributes '
    TabOrder = 1
    object CheckBoxEnabled: TCheckBox
      Left = 14
      Top = 26
      Width = 130
      Height = 17
      Hint = 'Makes the group valid for access checks'
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
      Hint = 'Denotes the default state; does NOT affect access checks'
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
      Hint = 'Mandatory groups cannot be disabled'
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
      Hint = 'The system will consider this group only for denying access'
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
      Hint = 'Marks the group as a valid token owner'
      Anchors = [akTop]
      Caption = '&Owner'
      TabOrder = 2
    end
  end
  object GroupBoxAdditional: TGroupBox
    Left = 292
    Top = 37
    Width = 267
    Height = 106
    Hint = 'The system will consider this group only for denying access'
    Caption = 'Additional attributes '
    TabOrder = 2
    object CheckBoxIntegrityEnabled: TCheckBox
      Left = 18
      Top = 48
      Width = 114
      Height = 17
      Hint = 'Denotes a group as a valid source of token'#39's integrity level'
      Anchors = [akTop]
      Caption = 'Integrity Enabled'
      TabOrder = 1
    end
    object CheckBoxIntegrity: TCheckBox
      Left = 18
      Top = 25
      Width = 114
      Height = 17
      Hint = 'Does not seem to have any effects'
      Anchors = [akTop]
      Caption = 'I&ntegrity'
      TabOrder = 0
    end
    object CheckBoxResource: TCheckBox
      Left = 144
      Top = 25
      Width = 104
      Height = 17
      Hint = 'Identifies a domain-local group'
      Anchors = [akTop]
      Caption = '&Resource'
      TabOrder = 3
    end
    object CheckBoxLogon: TCheckBox
      Left = 144
      Top = 48
      Width = 104
      Height = 17
      Hint = 'Marks a group as a Logon SID'
      Anchors = [akTop]
      Caption = 'Lo&gon ID'
      TabOrder = 4
    end
    object ButtonIntegrity: TButton
      Left = 18
      Top = 71
      Width = 105
      Height = 25
      Anchors = [akTop]
      Caption = 'Choose &Intrgirty'
      TabOrder = 2
      OnClick = ButtonIntegrityClick
    end
    object ButtonLogonSID: TButton
      Left = 144
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
  inline SidEditor: TSidEditor
    Left = 8
    Top = 5
    Width = 553
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    inherited tbxSid: TEdit
      Width = 496
    end
    inherited btnDsPicker: TButton
      Left = 528
    end
    inherited btnCheatsheet: TButton
      Left = 500
    end
  end
end
