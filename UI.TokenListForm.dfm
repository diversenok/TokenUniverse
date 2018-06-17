object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Token Universe :: Main Window'
  ClientHeight = 360
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TokenListView: TListView
    AlignWithMargins = True
    Left = 3
    Top = 41
    Width = 758
    Height = 316
    Align = alClient
    Columns = <
      item
        Caption = 'Token name'
        Width = 180
      end
      item
        Caption = 'Type'
        Width = 80
      end
      item
        Caption = 'Access'
        Width = 120
      end
      item
        Caption = 'User'
        Width = 180
      end
      item
        Alignment = taCenter
        Caption = 'Session'
      end
      item
        Alignment = taCenter
        Caption = 'Elevated'
        Width = 55
      end
      item
        Alignment = taCenter
        Caption = 'Integrity'
        Width = 58
      end>
    GridLines = True
    RowSelect = True
    PopupMenu = PopupMenuNothing
    TabOrder = 0
    ViewStyle = vsReport
    OnEdited = TokenListViewEdited
    OnSelectItem = TokenListViewSelectItem
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 764
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 104
      Top = 12
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Button1: TButton
      Left = 3
      Top = 7
      Width = 86
      Height = 25
      Caption = 'Open Process'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object PopupMenuItem: TPopupMenu
    Left = 176
    Top = 112
    object ActionDuplicate: TMenuItem
      Caption = 'Duplicate'
      OnClick = ActionDuplicateClick
    end
    object ActionClose: TMenuItem
      Caption = 'Close'
      OnClick = ActionCloseClick
    end
  end
  object PopupMenuNothing: TPopupMenu
    Left = 176
    Top = 168
    object New1: TMenuItem
      Caption = 'New'
      object Opencurrentprocess2: TMenuItem
        Caption = 'Open current process'
      end
      object Otherprocess1: TMenuItem
        Caption = 'Open other process'
      end
      object hread1: TMenuItem
        Caption = 'Open other thread'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object LogonUser1: TMenuItem
        Caption = 'Logon user'
      end
      object WTSQueryUserToken1: TMenuItem
        Caption = 'WTSQueryUserToken'
      end
      object CreatefromSaferAPI1: TMenuItem
        Caption = 'Create using Safer API'
      end
      object Createnewtoken2: TMenuItem
        Caption = 'Create using NtCreateToken'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Fromotherprocess1: TMenuItem
        Caption = 'Copy handle from other process'
      end
      object Searchforhandles1: TMenuItem
        Caption = 'Search for token handles'
      end
    end
  end
end
