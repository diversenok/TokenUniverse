object DialogSafer: TDialogSafer
  Left = 0
  Top = 0
  Caption = 'Create Safer Token'
  ClientHeight = 211
  ClientWidth = 274
  Color = clBtnFace
  Constraints.MinHeight = 220
  Constraints.MinWidth = 290
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelScope: TLabel
    Left = 8
    Top = 11
    Width = 33
    Height = 13
    Caption = 'Scope:'
  end
  object LabelLevel: TLabel
    Left = 8
    Top = 38
    Width = 29
    Height = 13
    Caption = 'Level:'
  end
  object LabelDesc: TLabel
    Left = 8
    Top = 92
    Width = 57
    Height = 13
    Caption = 'Description:'
  end
  object LabelDescription: TLabel
    Left = 81
    Top = 92
    Width = 188
    Height = 80
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    EllipsisPosition = epEndEllipsis
    WordWrap = True
  end
  object LabelName: TLabel
    Left = 8
    Top = 65
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object LabelFriendlyName: TLabel
    Left = 81
    Top = 65
    Width = 188
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    EllipsisPosition = epEndEllipsis
  end
  object ComboBoxScope: TComboBox
    Left = 81
    Top = 8
    Width = 188
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 1
    Text = 'Machine'
    OnChange = ComboBoxLevelChange
    Items.Strings = (
      'Machine'
      'User')
  end
  object ComboBoxLevel: TComboBox
    Left = 81
    Top = 35
    Width = 188
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 1
    TabOrder = 0
    Text = 'Normal User'
    OnChange = ComboBoxLevelChange
    Items.Strings = (
      'Fully Trusted'
      'Normal User'
      'Constrained'
      'Untrusted'
      'Disallowed')
  end
  object ButtonOK: TButton
    Left = 115
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 194
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
  object CheckBoxSandboxInert: TCheckBox
    Left = 8
    Top = 186
    Width = 97
    Height = 17
    Hint = 
      'Does not check AppLocker rules or apply Software Restriction Pol' +
      'icies for the process with this token.'#13#10#13#10'This action might requ' +
      'ire SeTcbPrivilege to take effect.'
    Anchors = [akLeft, akBottom]
    Caption = 'Sandbox Inert'
    TabOrder = 2
  end
end
