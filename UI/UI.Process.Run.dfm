object DialogRun: TDialogRun
  Left = 0
  Top = 0
  Caption = 'Run program...'
  ClientHeight = 378
  ClientWidth = 331
  Color = clBtnFace
  DoubleBuffered = True
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
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 325
    Height = 344
    Margins.Bottom = 31
    ActivePage = TabMethod
    Align = alClient
    TabOrder = 0
    object TabMethod: TTabSheet
      Caption = 'Method'
      object LabelOther: TLabel
        Left = 8
        Top = 209
        Width = 76
        Height = 13
        Caption = 'Other methods:'
      end
      object LabelToken: TLabel
        Left = 8
        Top = 73
        Width = 60
        Height = 13
        Caption = 'Using token:'
      end
      object LabelCred: TLabel
        Left = 8
        Top = 162
        Width = 121
        Height = 13
        Caption = 'Using explicit credentials:'
      end
      object LabelUsual: TLabel
        Left = 8
        Top = 8
        Width = 106
        Height = 13
        Caption = 'Inherit process token:'
      end
      object RadioButtonUsual: TRadioButton
        Left = 24
        Top = 27
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CreateProcess'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = ChangedExecMethod
      end
      object RadioButtonRtl: TRadioButton
        Left = 24
        Top = 50
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RtlCreateUserProcess'
        Enabled = False
        TabOrder = 1
        OnClick = ChangedExecMethod
      end
      object RadioButtonShell: TRadioButton
        Left = 24
        Top = 228
        Width = 113
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'ShellExecuteEx'
        Enabled = False
        TabOrder = 6
        OnClick = ChangedExecMethod
      end
      object RadioButtonWdc: TRadioButton
        Left = 24
        Top = 251
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'WdcRunTaskAsInteractiveUser'
        Enabled = False
        TabOrder = 7
        OnClick = ChangedExecMethod
      end
      object RadioButtonWMI: TRadioButton
        Left = 24
        Top = 274
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'WMI'
        Enabled = False
        TabOrder = 8
        OnClick = ChangedExecMethod
      end
      object RadioButtonAsUser: TRadioButton
        Left = 24
        Top = 92
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CreateProcessAsUser'
        TabOrder = 2
        OnClick = ChangedExecMethod
      end
      object RadioButtonWithToken: TRadioButton
        Left = 24
        Top = 115
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CreateProcessWithToken'
        TabOrder = 3
        OnClick = ChangedExecMethod
      end
      object RadioButtonWithLogon: TRadioButton
        Left = 24
        Top = 181
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'CreateProcessWithLogon'
        Enabled = False
        TabOrder = 5
        OnClick = ChangedExecMethod
      end
      object CheckBoxRunas: TCheckBox
        Left = 165
        Top = 229
        Width = 135
        Height = 15
        Anchors = [akTop]
        Caption = 'Request elevation'
        Enabled = False
        TabOrder = 9
      end
      object RadioButtonWmiImp: TRadioButton
        Left = 24
        Top = 138
        Width = 276
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'WMI under impersonation'
        Enabled = False
        TabOrder = 4
        OnClick = ChangedExecMethod
      end
    end
    object TabParams: TTabSheet
      Caption = 'Parameters'
      ImageIndex = 4
      object LabelDesktop: TLabel
        Left = 3
        Top = 190
        Width = 43
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Desktop:'
      end
      object LabelLogonFlags: TLabel
        Left = 161
        Top = 190
        Width = 59
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Logon flags:'
      end
      object LabelShowMode: TLabel
        Left = 162
        Top = 146
        Width = 71
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Window mode:'
      end
      object EditExe: TLabeledEdit
        Left = 3
        Top = 24
        Width = 230
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'Executable:'
        TabOrder = 0
      end
      object ButtonBrowse: TButton
        Left = 239
        Top = 22
        Width = 71
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        DropDownMenu = PopupMenuExe
        Style = bsSplitButton
        TabOrder = 1
        OnClick = ButtonBrowseClick
      end
      object EditParams: TLabeledEdit
        Left = 3
        Top = 68
        Width = 307
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 59
        EditLabel.Height = 13
        EditLabel.Caption = 'Parameters:'
        TabOrder = 2
      end
      object GroupBoxFlags: TGroupBox
        Left = 3
        Top = 234
        Width = 307
        Height = 79
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Flags: '
        TabOrder = 3
        object CheckBoxInherit: TCheckBox
          Left = 11
          Top = 24
          Width = 115
          Height = 17
          Caption = 'Inherit handles'
          TabOrder = 0
        end
        object CheckBoxSuspended: TCheckBox
          Left = 11
          Top = 47
          Width = 115
          Height = 17
          Caption = 'Create suspended'
          TabOrder = 1
        end
        object CheckBoxBreakaway: TCheckBox
          Left = 158
          Top = 24
          Width = 134
          Height = 17
          Caption = 'Breakaway from job'
          TabOrder = 2
        end
      end
      object ComboBoxLogonFlags: TComboBox
        Left = 161
        Top = 207
        Width = 149
        Height = 21
        Style = csDropDownList
        Anchors = [akRight, akBottom]
        ItemIndex = 0
        TabOrder = 4
        Text = 'Default'
        Items.Strings = (
          'Default'
          'Logon with profile'
          'Network only credentials')
      end
      object EditDir: TLabeledEdit
        Left = 3
        Top = 117
        Width = 307
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 87
        EditLabel.Height = 13
        EditLabel.Caption = 'Current directory:'
        TabOrder = 5
      end
      object ComboBoxDesktop: TComboBox
        Left = 3
        Top = 207
        Width = 144
        Height = 21
        Anchors = [akLeft, akBottom]
        ItemIndex = 0
        TabOrder = 6
        Text = 'WinSta0\Default'
        Items.Strings = (
          'WinSta0\Default'
          'WinSta0\Winlogon')
      end
      object ComboBoxShowMode: TComboBox
        Left = 161
        Top = 163
        Width = 149
        Height = 21
        Style = csDropDownList
        Anchors = [akRight, akBottom]
        ItemIndex = 1
        TabOrder = 7
        Text = 'Show normal'
        Items.Strings = (
          'Hide'
          'Show normal'
          'Show minimized'
          'Show maximized')
      end
    end
    object TabEnv: TTabSheet
      Caption = 'Environment'
      ImageIndex = 1
    end
    object TabParent: TTabSheet
      Caption = 'Parent process'
      ImageIndex = 2
    end
  end
  object ButtonClose: TButton
    Left = 253
    Top = 349
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
    OnClick = ButtonCloseClick
  end
  object ButtonRun: TButton
    Left = 172
    Top = 349
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Run'
    Default = True
    TabOrder = 2
    OnClick = ButtonRunClick
  end
  object PopupMenuExe: TPopupMenu
    Left = 215
    Top = 43
    object MenuCmd: TMenuItem
      Caption = 'Command Prompt'
      OnClick = MenuCmdClick
    end
    object MenuSelf: TMenuItem
      Caption = 'Token Universe'
      OnClick = MenuSelfClick
    end
  end
  object OpenDlg: TOpenDialog
    Filter = 
      'Executable files (*.exe;*.com;*.scr)|*.exe;*.com;*.scr|All files' +
      ' (*.*)|*'
    Options = [ofEnableSizing]
    Left = 223
    Top = 91
  end
end
