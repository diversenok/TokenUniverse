object FormActivateComClass: TFormActivateComClass
  Left = 0
  Top = 0
  Caption = 'Activate a COM class'
  ClientHeight = 546
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 560
  Constraints.MinWidth = 620
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
  object lblClsid: TLabel
    Left = 5
    Top = 196
    Width = 34
    Height = 15
    Caption = 'CLSID:'
  end
  object lblCatalog: TLabel
    Left = 5
    Top = 56
    Width = 54
    Height = 15
    Caption = 'Class info:'
  end
  object Label2: TLabel
    Left = 5
    Top = 242
    Width = 44
    Height = 15
    Caption = 'Context:'
  end
  object lblLuaLevel: TLabel
    Left = 316
    Top = 340
    Width = 55
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'LUA Level:'
    Enabled = False
  end
  object lblIid: TLabel
    Left = 316
    Top = 242
    Width = 17
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'IID:'
  end
  object lblResult: TLabel
    Left = 8
    Top = 466
    Width = 35
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Result:'
  end
  object cbxMethod: TComboBox
    Left = 5
    Top = 26
    Width = 611
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 13
    Text = 'CoCreateInstance'
    OnChange = cbxMethodChange
    Items.Strings = (
      'CoCreateInstance'
      'CoCreateInstanceEx'
      'CoCreateInstanceAsUser'
      'CoGetClassObject'
      'IStandardActivator::StandardCreateInstance'
      'IStandardActivator::StandardGetClassObject')
  end
  object tbxClsid: TUiLibGuidBox
    Left = 5
    Top = 214
    Width = 611
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = tbxClsidChange
  end
  object uiCatalog: TUiLibTree
    Left = 5
    Top = 74
    Width = 611
    Height = 117
    Anchors = [akLeft, akTop, akRight]
    EmptyListMessage = 'No items to display'
    Header.AutoSizeIndex = 0
    TabOrder = 14
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
        Width = 420
      end>
  end
  inline uiClsCtx: TBitsFrame
    Left = 5
    Top = 260
    Width = 305
    Height = 203
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 100
    Constraints.MinWidth = 224
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    inherited Tree: TUiLibTree
      Width = 305
      Height = 175
      Columns = <
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
          Position = 0
          Text = 'Name'
          Width = 301
        end>
    end
    inherited BottomPanel: TPanel
      Top = 175
      Width = 305
      inherited tbxValue: TUiLibEdit
        Width = 159
      end
      inherited btnAll: TButton
        Left = 235
      end
    end
  end
  object chkxSessionId: TCheckBox
    Left = 316
    Top = 288
    Width = 300
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Session ID:'
    Enabled = False
    TabOrder = 3
    OnClick = chkxSessionIdClick
  end
  object uiSessionId: TUiLibSessionIdBox
    Left = 316
    Top = 311
    Width = 300
    Height = 23
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 4
  end
  object cbxLuaLevel: TComboBox
    Left = 316
    Top = 361
    Width = 300
    Height = 23
    Style = csDropDownList
    Anchors = [akTop, akRight]
    Enabled = False
    ItemIndex = 0
    TabOrder = 5
    Text = 'Not set'
    Items.Strings = (
      'Not set'
      'Highest Available'
      'Require Administrator')
  end
  object tbxIid: TUiLibGuidBox
    Left = 316
    Top = 260
    Width = 300
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 2
    OnChange = tbxClsidChange
  end
  object btnRelease: TButton
    Left = 541
    Top = 483
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Release'
    Enabled = False
    TabOrder = 10
    OnClick = btnReleaseClick
  end
  object btnActivate: TButton
    Left = 541
    Top = 513
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Activate'
    Default = True
    TabOrder = 11
    OnClick = btnActivateClick
  end
  object btnClose: TButton
    Left = 8
    Top = 513
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 12
  end
  object tbxResult: TUiLibEdit
    Left = 8
    Top = 484
    Width = 420
    Height = 23
    Anchors = [akLeft, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
  end
  object uiUserContext: TUiLibUmgrContextBox
    Left = 316
    Top = 411
    Width = 300
    Height = 23
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 7
  end
  object chkxUserContext: TCheckBox
    Left = 316
    Top = 390
    Width = 300
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'User Context:'
    Enabled = False
    TabOrder = 6
    OnClick = chkxUserContextClick
  end
  object btnQueryIntf: TButton
    Left = 434
    Top = 483
    Width = 103
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Query Interface'
    Enabled = False
    TabOrder = 9
    OnClick = btnQueryIntfClick
  end
end
