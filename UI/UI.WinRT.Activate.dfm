object FormActivateWinRTClass: TFormActivateWinRTClass
  Left = 0
  Top = 0
  Caption = 'Activate a WinRT class'
  ClientHeight = 670
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 620
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = UiLibChildFormCreate
  CloseOnEscape = True
  TextHeight = 15
  object lblMethod: TLabel
    Left = 5
    Top = 8
    Width = 45
    Height = 15
    Caption = 'Method:'
  end
  object lblResult: TLabel
    Left = 5
    Top = 393
    Width = 35
    Height = 15
    Caption = 'Result:'
  end
  object lblCatalog: TLabel
    Left = 5
    Top = 56
    Width = 54
    Height = 15
    Caption = 'Class info:'
  end
  object lblAcid: TLabel
    Left = 5
    Top = 299
    Width = 30
    Height = 15
    Caption = 'ACID:'
  end
  object lblPackage: TLabel
    Left = 5
    Top = 252
    Width = 104
    Height = 15
    Caption = 'Package Full Name:'
    Enabled = False
  end
  object lblBrokerType: TLabel
    Left = 471
    Top = 345
    Width = 63
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Broker type:'
    Enabled = False
  end
  object cbxMethod: TComboBox
    Left = 5
    Top = 26
    Width = 611
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 9
    Text = 'RoActivateInstance'
    OnChange = cbxMethodChange
    Items.Strings = (
      'RoActivateInstance'
      'RoActivateInstanceInPackage'
      'RoActivateInstanceAsUser'
      'RoGetActivationFactory'
      'RoGetActivationFactoryAsUser'
      'IRuntimeBroker::ActivateInstance'
      'IRuntimeBroker::GetActivationFactory')
  end
  object btnRelease: TButton
    Left = 541
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Release'
    Enabled = False
    TabOrder = 5
    OnClick = btnReleaseClick
  end
  object btnActivate: TButton
    Left = 541
    Top = 637
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Activate'
    Default = True
    TabOrder = 7
    OnClick = btnActivateClick
  end
  object btnClose: TButton
    Left = 5
    Top = 637
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 8
  end
  object tbxResult: TUiLibEdit
    Left = 5
    Top = 409
    Width = 530
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
  end
  object uiCatalog: TUiLibTree
    Left = 5
    Top = 74
    Width = 611
    Height = 172
    Anchors = [akLeft, akTop, akRight]
    EmptyListMessage = 'No items to display'
    Header.AutoSizeIndex = 0
    TabOrder = 10
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Property'
        Width = 200
      end
      item
        Position = 1
        Text = 'Value'
        Width = 380
      end>
  end
  object tbxAcid: TUiLibEdit
    Left = 5
    Top = 315
    Width = 611
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = tbxAcidChange
  end
  object tbxPackage: TUiLibEdit
    Left = 5
    Top = 269
    Width = 611
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 11
    OnChange = tbxAcidChange
    OnEnter = tbxPackageEnter
  end
  object uiUserContext: TUiLibUmgrContextBox
    Left = 5
    Top = 365
    Width = 460
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 2
  end
  object chkxUserContext: TCheckBox
    Left = 5
    Top = 345
    Width = 460
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'User Context:'
    Enabled = False
    TabOrder = 1
    OnClick = chkxUserContextClick
  end
  object uiResult: TUiLibTree
    Left = 5
    Top = 439
    Width = 611
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    EmptyListMessage = 'No items to display'
    Header.AutoSizeIndex = 0
    TabOrder = 6
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Property'
        Width = 150
      end
      item
        Position = 1
        Text = 'Value'
        Width = 450
      end>
  end
  object cbxBrokerType: TComboBox
    Left = 471
    Top = 365
    Width = 145
    Height = 23
    Style = csDropDownList
    Anchors = [akTop, akRight]
    Enabled = False
    ItemIndex = 0
    TabOrder = 3
    Text = 'Shared'
    Items.Strings = (
      'Shared'
      'Per-app')
  end
  object tbxIid: TUiLibGuidBox
    Left = 5
    Top = 610
    Width = 502
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    Enabled = False
    TabOrder = 12
  end
  object btnQueryIntf: TButton
    Left = 513
    Top = 609
    Width = 103
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Query Interface'
    Enabled = False
    TabOrder = 13
    OnClick = btnQueryIntfClick
  end
end
