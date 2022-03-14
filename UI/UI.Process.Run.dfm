object DialogRun: TDialogRun
  Left = 0
  Top = 0
  Caption = 'Run program...'
  ClientHeight = 589
  ClientWidth = 331
  Color = clBtnFace
  Constraints.MinHeight = 455
  Constraints.MinWidth = 347
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 52
    Width = 325
    Height = 506
    Margins.Top = 52
    Margins.Bottom = 31
    ActivePage = TabParams
    Align = alClient
    TabOrder = 2
    object TabParams: TTabSheet
      Caption = 'Parameters'
      ImageIndex = 4
      object LabelDesktop: TLabel
        Left = 3
        Top = 235
        Width = 43
        Height = 13
        Caption = 'Desktop:'
      end
      object LabelLogonFlags: TLabel
        Left = 161
        Top = 284
        Width = 59
        Height = 13
        Caption = 'Logon flags:'
      end
      object LabelShowMode: TLabel
        Left = 4
        Top = 284
        Width = 71
        Height = 13
        Caption = 'Window mode:'
      end
      object LabelAppContainer: TLabel
        Left = 3
        Top = 336
        Width = 70
        Height = 13
        Caption = 'AppContainer:'
      end
      object LabelMethod: TLabel
        Left = 3
        Top = 7
        Width = 40
        Height = 13
        Caption = 'Method:'
      end
      object Label1: TLabel
        Left = 3
        Top = 85
        Width = 76
        Height = 13
        Caption = 'Parent process:'
      end
      object EditParams: TLabeledEdit
        Left = 4
        Top = 156
        Width = 307
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 59
        EditLabel.Height = 13
        EditLabel.Caption = 'Parameters:'
        TabOrder = 0
      end
      object GroupBoxFlags: TGroupBox
        Left = 3
        Top = 361
        Width = 307
        Height = 113
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Flags: '
        TabOrder = 5
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
        object CheckBoxNewConsole: TCheckBox
          Left = 11
          Top = 93
          Width = 134
          Height = 17
          Caption = 'Create new console'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object CheckBoxRunas: TCheckBox
          Left = 11
          Top = 70
          Width = 116
          Height = 15
          Caption = 'Request elevation'
          Enabled = False
          TabOrder = 4
        end
        object CheckBoxRunAsInvoker: TCheckBox
          Left = 158
          Top = 68
          Width = 116
          Height = 17
          Hint = 
            'Use this compatibility mechanism if you want to start a program ' +
            'that requires elevation as a limited user.'
          AllowGrayed = True
          Caption = 'Run as invoker'
          State = cbGrayed
          TabOrder = 5
        end
        object CheckBoxForceBreakaway: TCheckBox
          Left = 158
          Top = 47
          Width = 116
          Height = 15
          Hint = 
            'Allows the new process to breakaway from all parent jobs regardl' +
            'ess of their flags. Requires the Tcb privilege.'
          Caption = 'Force breakaway'
          Enabled = False
          TabOrder = 6
        end
      end
      object ComboBoxLogonFlags: TComboBox
        Left = 162
        Top = 300
        Width = 149
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'Default'
        Items.Strings = (
          'Default'
          'Logon with profile'
          'Network only credentials')
      end
      object EditDir: TLabeledEdit
        Left = 4
        Top = 204
        Width = 307
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 87
        EditLabel.Height = 13
        EditLabel.Caption = 'Current directory:'
        TabOrder = 1
      end
      object ComboBoxDesktop: TComboBox
        Left = 4
        Top = 254
        Width = 307
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'WinSta0\Default'
      end
      object ComboBoxShowMode: TComboBox
        Left = 4
        Top = 300
        Width = 149
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 3
        Text = 'Show normal'
        Items.Strings = (
          'Hide'
          'Show normal'
          'Show minimized'
          'Show maximized')
      end
      object ButtonAC: TButton
        Left = 226
        Top = 331
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Choose'
        DropDownMenu = PopupClearAC
        Style = bsSplitButton
        TabOrder = 6
        OnClick = ButtonACClick
      end
      object EditAppContainer: TEdit
        Left = 79
        Top = 333
        Width = 141
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 7
        Text = 'No'
      end
      object ComboMethod: TComboBox
        Left = 3
        Top = 24
        Width = 306
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 8
        Text = 'CreateProcessAsUser'
        OnChange = ChangedExecMethod
        Items.Strings = (
          'CreateProcessAsUser'
          'CreateProcessWithToken'
          'CreateProcessWithLogon'
          'CreateProcess via code injection'
          'RtlCreateUserProcess'
          'RtlCreateUserProcessEx'
          'NtCreateUserProcess'
          'NtCreateProcessEx'
          'ShellExecuteEx'
          'ShellExecute via IShellDispatch'
          'WdcRunTaskAsInteractiveUser'
          'WMI')
      end
      object ButtonChooseParent: TButton
        Left = 226
        Top = 102
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Choose'
        DropDownMenu = PopupClearParent
        Style = bsSplitButton
        TabOrder = 9
        OnClick = ButtonChooseParentClick
      end
      object EditParent: TEdit
        Left = 3
        Top = 104
        Width = 217
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 10
        Text = '<not specified>'
      end
      object LinkLabelToken: TLinkLabel
        Left = 3
        Top = 59
        Width = 292
        Height = 14
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Using token: <not specified>'
        TabOrder = 11
        OnLinkClick = LinkLabelTokenLinkClick
      end
    end
  end
  object ButtonClose: TButton
    Left = 253
    Top = 560
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 4
    OnClick = ButtonCloseClick
  end
  object ButtonRun: TButton
    Left = 172
    Top = 560
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Run'
    Default = True
    TabOrder = 3
    OnClick = ButtonRunClick
  end
  object EditExe: TLabeledEdit
    Left = 8
    Top = 23
    Width = 239
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 57
    EditLabel.Height = 13
    EditLabel.Caption = 'Executable:'
    TabOrder = 0
  end
  object ButtonBrowse: TButton
    Left = 253
    Top = 21
    Width = 71
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    DropDownMenu = PopupMenuExe
    Style = bsSplitButton
    TabOrder = 1
    OnClick = ButtonBrowseClick
  end
  object cbxOpenToken: TCheckBox
    Left = 8
    Top = 564
    Width = 158
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Open token afterwards'
    TabOrder = 5
  end
  object PopupMenuExe: TPopupMenu
    Left = 271
    Top = 195
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
    Left = 199
    Top = 195
  end
  object PopupClearParent: TPopupMenu
    Left = 271
    Top = 147
    object MenuClearParent: TMenuItem
      Caption = 'Clear'
      OnClick = MenuClearParentClick
    end
  end
  object PopupClearAC: TPopupMenu
    Left = 263
    Top = 251
    object MenuClearAC: TMenuItem
      Caption = 'Clear'
      OnClick = MenuClearACClick
    end
  end
end
