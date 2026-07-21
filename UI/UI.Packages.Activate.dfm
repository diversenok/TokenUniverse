object FormActivatePackage: TFormActivatePackage
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Activate a package'
  ClientHeight = 246
  ClientWidth = 600
  Color = clBtnFace
  Constraints.MinHeight = 285
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  TextHeight = 15
  object lblAumid: TLabel
    Left = 5
    Top = 58
    Width = 92
    Height = 15
    Caption = 'AppUserModelId:'
  end
  object lblDisplayName: TLabel
    Left = 5
    Top = 8
    Width = 74
    Height = 15
    Caption = 'Display name:'
  end
  object lblArguments: TLabel
    Left = 5
    Top = 108
    Width = 62
    Height = 15
    Caption = 'Arguments:'
  end
  object lblHost: TLabel
    Left = 5
    Top = 158
    Width = 35
    Height = 15
    Caption = 'Result:'
  end
  object tbxAumid: TUiLibEdit
    Left = 5
    Top = 78
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 
      'Enter {PackageFamilyName}!{RelativeAppId} or press DOWN for sugg' +
      'estions'
    OnChange = tbxAumidChange
    OnEnter = tbxAumidEnter
  end
  object tbxDisplayName: TUiLibEdit
    Left = 5
    Top = 26
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object tbxArguments: TUiLibEdit
    Left = 5
    Top = 128
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    TextHint = 'Optional string'
  end
  object btnActivate: TButton
    Left = 520
    Top = 213
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Activate'
    Default = True
    TabOrder = 3
    OnClick = btnActivateClick
  end
  object btnClose: TButton
    Left = 5
    Top = 213
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object tbxResult: TUiLibEdit
    Left = 5
    Top = 178
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
end
