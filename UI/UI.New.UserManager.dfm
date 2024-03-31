object UserManagerTokens: TUserManagerTokens
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'User Manager Tokens'
  ClientHeight = 217
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 256
  Constraints.MinWidth = 540
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object BevelSession: TBevel
    Left = 4
    Top = 36
    Width = 526
    Height = 56
    Anchors = [akLeft, akTop, akRight]
    Shape = bsFrame
  end
  object rbxDefault: TRadioButton
    Left = 12
    Top = 12
    Width = 518
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Default Account Token'
    TabOrder = 0
  end
  object rbxSession: TRadioButton
    Left = 12
    Top = 43
    Width = 137
    Height = 17
    Caption = 'Session User Token'
    TabOrder = 1
  end
  object rbxShell: TRadioButton
    Left = 12
    Top = 66
    Width = 137
    Height = 17
    Caption = 'Active Shell Token'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object rbxContext: TRadioButton
    Left = 12
    Top = 101
    Width = 161
    Height = 17
    Caption = 'User Token by Context'
    TabOrder = 4
  end
  object rbxSid: TRadioButton
    Left = 12
    Top = 130
    Width = 161
    Height = 17
    Caption = 'User Token by SID'
    TabOrder = 6
  end
  object rbxName: TRadioButton
    Left = 12
    Top = 159
    Width = 161
    Height = 17
    Caption = 'User Token by Name'
    TabOrder = 8
  end
  object cbxSessionId: TComboBox
    Left = 179
    Top = 52
    Width = 342
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnEnter = cbxSessionIdEnter
  end
  object cbxContext: TComboBox
    Left = 179
    Top = 98
    Width = 342
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnEnter = cbxContextEnter
  end
  object cbxName: TEdit
    Left = 179
    Top = 156
    Width = 342
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
    TextHint = 'User name without domain'
    OnEnter = cbxNameEnter
  end
  object btnOpen: TButton
    Left = 374
    Top = 185
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open'
    Default = True
    TabOrder = 10
    OnClick = btnOpenClick
  end
  object btnClose: TButton
    Left = 455
    Top = 185
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
    OnClick = btnCloseClick
  end
  inline SidEditor: TSidEditor
    Left = 179
    Top = 126
    Width = 342
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnEnter = SidEditorEnter
    inherited tbxSid: TEdit
      Width = 255
    end
    inherited btnDsPicker: TButton
      Left = 317
    end
    inherited btnCheatsheet: TButton
      Left = 288
    end
    inherited btnChoice: TButton
      Left = 259
    end
  end
end
