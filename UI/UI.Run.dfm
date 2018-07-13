object RunDialog: TRunDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Run program with token'
  ClientHeight = 406
  ClientWidth = 374
  Color = clBtnFace
  Constraints.MinHeight = 444
  Constraints.MinWidth = 390
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    374
    406)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonCancel: TButton
    Left = 292
    Top = 375
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
  end
  object GroupBoxAppName: TGroupBox
    Left = 8
    Top = 16
    Width = 358
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    DesignSize = (
      358
      49)
    object EditAppName: TEdit
      Left = 11
      Top = 15
      Width = 271
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'C:\Windows\System32\cmd.exe'
    end
    object ButtonBrowseAppName: TButton
      Left = 288
      Top = 13
      Width = 62
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browse'
      TabOrder = 1
    end
  end
  object CheckBoxAppName: TCheckBox
    Left = 19
    Top = 8
    Width = 102
    Height = 17
    Caption = 'Application name'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBoxAppNameClick
  end
  object GroupBoxCmd: TGroupBox
    Left = 8
    Top = 80
    Width = 358
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    DesignSize = (
      358
      49)
    object EditCmd: TEdit
      Left = 11
      Top = 15
      Width = 271
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
    end
    object ButtonBrowseCmd: TButton
      Left = 288
      Top = 13
      Width = 62
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browse'
      Enabled = False
      TabOrder = 1
    end
  end
  object CheckBoxCmd: TCheckBox
    Left = 19
    Top = 72
    Width = 86
    Height = 17
    Caption = 'Command line'
    TabOrder = 2
    OnClick = CheckBoxCmdClick
  end
  object GroupBoxFlags: TGroupBox
    Left = 8
    Top = 199
    Width = 358
    Height = 74
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Flags '
    TabOrder = 6
    object CheckBoxInherit: TCheckBox
      Left = 11
      Top = 24
      Width = 150
      Height = 17
      Caption = 'Inherit handles'
      TabOrder = 0
    end
    object CheckBoxSuspended: TCheckBox
      Left = 11
      Top = 47
      Width = 150
      Height = 17
      Caption = 'Create suspended'
      TabOrder = 1
    end
    object CheckBoxNewConsole: TCheckBox
      Left = 179
      Top = 22
      Width = 150
      Height = 17
      Caption = 'Create new console'
      TabOrder = 2
    end
    object CheckBoxBreakaway: TCheckBox
      Left = 179
      Top = 45
      Width = 150
      Height = 17
      Caption = 'Breakaway from job'
      TabOrder = 3
    end
  end
  object GroupBoxDirectory: TGroupBox
    Left = 8
    Top = 144
    Width = 358
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    DesignSize = (
      358
      49)
    object EditDirectory: TEdit
      Left = 11
      Top = 15
      Width = 271
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
    end
    object ButtonBrowseDirectory: TButton
      Left = 288
      Top = 13
      Width = 62
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browse'
      Enabled = False
      TabOrder = 1
    end
  end
  object CheckBoxDirectory: TCheckBox
    Left = 19
    Top = 135
    Width = 104
    Height = 17
    Caption = 'Current directory'
    TabOrder = 4
    OnClick = CheckBoxDirectoryClick
  end
  object GroupBoxDesktop: TGroupBox
    Left = 8
    Top = 279
    Width = 175
    Height = 59
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Desktop '
    TabOrder = 7
    DesignSize = (
      175
      59)
    object ComboBoxDesktop: TComboBox
      Left = 12
      Top = 24
      Width = 148
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'WinSta0\Default'
      Items.Strings = (
        'WinSta0\Default'
        'WinSta0\Winlogon')
    end
  end
  object ButtonAsUser: TButton
    Left = 8
    Top = 344
    Width = 173
    Height = 25
    Caption = 'CreateProcessAsUser #1'
    TabOrder = 9
    OnClick = ButtonAsUserClick
  end
  object ButtonWithToken: TButton
    Left = 8
    Top = 375
    Width = 173
    Height = 25
    Caption = 'CreateProcessWithTokenW #2'
    TabOrder = 10
    OnClick = ButtonWithTokenClick
  end
  object GroupBoxLogon: TGroupBox
    Left = 189
    Top = 279
    Width = 177
    Height = 90
    Anchors = [akTop, akRight]
    Caption = 'Logon flags for method #2'
    TabOrder = 8
    object RadioButtonLogonZero: TRadioButton
      Left = 11
      Top = 20
      Width = 151
      Height = 17
      Caption = 'Default'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonLogonProfile: TRadioButton
      Left = 11
      Top = 43
      Width = 151
      Height = 17
      Caption = 'Logon with profile'
      TabOrder = 1
    end
    object RadioButtonLogonNet: TRadioButton
      Left = 11
      Top = 66
      Width = 151
      Height = 17
      Caption = 'Network only credentials'
      TabOrder = 2
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Executable files|(*.exe, *.com, *.scr)|All files|*'
    Left = 184
    Top = 64
  end
end
