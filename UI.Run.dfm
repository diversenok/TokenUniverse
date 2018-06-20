object RunDialog: TRunDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Run program with token'
  ClientHeight = 362
  ClientWidth = 399
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 415
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    399
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonCancel: TButton
    Left = 317
    Top = 331
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    ExplicitTop = 329
  end
  object GroupBoxAppName: TGroupBox
    Left = 8
    Top = 16
    Width = 383
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    DesignSize = (
      383
      49)
    object EditAppName: TEdit
      Left = 11
      Top = 15
      Width = 296
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
    end
    object ButtonBrowseAppName: TButton
      Left = 313
      Top = 13
      Width = 62
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browse'
      Enabled = False
      TabOrder = 1
    end
  end
  object CheckBoxAppName: TCheckBox
    Left = 19
    Top = 8
    Width = 102
    Height = 17
    Caption = 'Application name'
    TabOrder = 2
    OnClick = CheckBoxAppNameClick
  end
  object GroupBoxCmd: TGroupBox
    Left = 8
    Top = 80
    Width = 383
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    DesignSize = (
      383
      49)
    object EditCmd: TEdit
      Left = 11
      Top = 15
      Width = 296
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
    end
    object ButtonBrowseCmd: TButton
      Left = 313
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
    TabOrder = 4
    OnClick = CheckBoxCmdClick
  end
  object GroupBoxFlags: TGroupBox
    Left = 8
    Top = 199
    Width = 173
    Height = 122
    Caption = 'Flags '
    TabOrder = 5
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
      Left = 11
      Top = 70
      Width = 150
      Height = 17
      Caption = 'Create new console'
      TabOrder = 2
    end
    object CheckBoxBreakaway: TCheckBox
      Left = 11
      Top = 93
      Width = 150
      Height = 17
      Caption = 'Breakaway from job'
      TabOrder = 3
    end
  end
  object GroupBoxDirectory: TGroupBox
    Left = 8
    Top = 144
    Width = 383
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    DesignSize = (
      383
      49)
    object EditDirectory: TEdit
      Left = 11
      Top = 15
      Width = 296
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 0
    end
    object ButtonBrowseDirectory: TButton
      Left = 313
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
    TabOrder = 7
    OnClick = CheckBoxDirectoryClick
  end
  object GroupBoxParent: TGroupBox
    Left = 187
    Top = 204
    Width = 204
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    DesignSize = (
      204
      50)
    object EditParent: TEdit
      Left = 12
      Top = 17
      Width = 115
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      NumbersOnly = True
      TabOrder = 0
    end
    object ButtonChooseParent: TButton
      Left = 133
      Top = 15
      Width = 62
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Choose'
      Enabled = False
      TabOrder = 1
      OnClick = ButtonChooseParentClick
    end
  end
  object CheckBoxParent: TCheckBox
    Left = 199
    Top = 197
    Width = 134
    Height = 17
    Caption = 'Replace parent process'
    TabOrder = 9
    OnClick = CheckBoxParentClick
  end
  object GroupBoxDesktop: TGroupBox
    Left = 187
    Top = 259
    Width = 204
    Height = 62
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Desktop '
    TabOrder = 10
    DesignSize = (
      204
      62)
    object ComboBoxDesktop: TComboBox
      Left = 12
      Top = 24
      Width = 177
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
    Top = 331
    Width = 137
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'CreateProcessAsUser'
    TabOrder = 11
    ExplicitTop = 329
  end
  object ButtonWithToken: TButton
    Left = 151
    Top = 331
    Width = 160
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'CreateProcessWithTokenW'
    TabOrder = 12
    ExplicitTop = 329
  end
end
