object FormActivatePackage: TFormActivatePackage
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Activate a package'
  ClientHeight = 640
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
  OnCreate = UiLibChildFormCreate
  TextHeight = 15
  object lblAumid: TLabel
    Left = 5
    Top = 108
    Width = 92
    Height = 15
    Caption = 'AppUserModelId:'
  end
  object lblDisplayName: TLabel
    Left = 5
    Top = 58
    Width = 74
    Height = 15
    Caption = 'Display name:'
  end
  object lblArguments: TLabel
    Left = 5
    Top = 158
    Width = 62
    Height = 15
    Caption = 'Arguments:'
  end
  object lblResult: TLabel
    Left = 5
    Top = 558
    Width = 35
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Result:'
  end
  object lblMethod: TLabel
    Left = 5
    Top = 8
    Width = 45
    Height = 15
    Caption = 'Method:'
  end
  object lblOptions: TLabel
    Left = 5
    Top = 308
    Width = 45
    Height = 15
    Caption = 'Options:'
  end
  object tbxAumid: TUiLibEdit
    Left = 5
    Top = 128
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
    Top = 78
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object tbxArguments: TUiLibEdit
    Left = 5
    Top = 178
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    TextHint = 'Optional string'
  end
  object btnActivate: TButton
    Left = 520
    Top = 607
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
    Top = 607
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
    Top = 578
    Width = 590
    Height = 23
    Anchors = [akLeft, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object cbxMethod: TComboBox
    Left = 5
    Top = 28
    Width = 590
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 6
    Text = 'IApplicationActivationManager::ActivateApplication'
    OnChange = cbxMethodChange
    Items.Strings = (
      'IApplicationActivationManager::ActivateApplication'
      'IApplicationActivationBroker::ActivateApplication')
  end
  object cbxSession: TUiLibSessionIdBox
    Left = 5
    Top = 230
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 7
  end
  object chkSession: TCheckBox
    Left = 5
    Top = 209
    Width = 97
    Height = 17
    Caption = 'Session:'
    Enabled = False
    TabOrder = 8
    OnClick = cbxMethodChange
  end
  object cbxContext: TUiLibUmgrContextBox
    Left = 5
    Top = 278
    Width = 590
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 9
  end
  inline fmxOptions: TBitsFrame
    Left = 5
    Top = 327
    Width = 590
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 100
    Constraints.MinWidth = 224
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    inherited Tree: TUiLibTree
      Width = 590
      Height = 197
      Columns = <
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
          Position = 0
          Text = 'Name'
          Width = 586
        end>
    end
    inherited BottomPanel: TPanel
      Top = 197
      Width = 590
      inherited tbxValue: TUiLibEdit
        Width = 444
      end
      inherited btnAll: TButton
        Left = 520
      end
    end
  end
  object chkContext: TCheckBox
    Left = 5
    Top = 258
    Width = 188
    Height = 17
    Caption = 'User manager context:'
    Enabled = False
    TabOrder = 11
    OnClick = cbxMethodChange
  end
end
