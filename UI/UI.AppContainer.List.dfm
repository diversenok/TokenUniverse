object DialogACProfiles: TDialogACProfiles
  Left = 0
  Top = 0
  Caption = 'App Container Profiles'
  ClientHeight = 307
  ClientWidth = 552
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    552
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProfiles: TLabel
    Left = 8
    Top = 14
    Width = 34
    Height = 13
    Caption = 'Profile:'
  end
  object cbProfile: TComboBox
    Left = 56
    Top = 11
    Width = 488
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object lvAppContainers: TListViewEx
    Left = 8
    Top = 65
    Width = 536
    Height = 203
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Display Name'
        Width = 240
      end
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Caption = 'SID'
        Width = 200
      end>
    GridLines = True
    Groups = <
      item
        Header = 'Search Results:'
        GroupID = 0
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = cmSelectClick
    PopupOnItemsOnly = True
  end
  object SearchBox: TButtonedEdit
    Left = 8
    Top = 38
    Width = 536
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Images = FormMain.SearchButtons
    LeftButton.ImageIndex = 0
    LeftButton.Visible = True
    RightButton.HotImageIndex = 2
    RightButton.ImageIndex = 1
    RightButton.PressedImageIndex = 3
    TabOrder = 2
    TextHint = 'Search'
    OnChange = SearchBoxChange
    OnRightButtonClick = SearchBoxRightButtonClick
  end
  object ButtonClose: TButton
    Left = 469
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = ButtonCloseClick
  end
  object ButtonOK: TButton
    Left = 388
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    Visible = False
  end
  object PopupMenu: TPopupMenu
    Left = 184
    Top = 168
    object cmSelect: TMenuItem
      Caption = 'Select'
      Default = True
      ShortCut = 13
      OnClick = cmSelectClick
    end
    object cmInspect: TMenuItem
      Caption = 'Inspect'
      ShortCut = 16397
      OnClick = cmInspectClick
    end
  end
end
