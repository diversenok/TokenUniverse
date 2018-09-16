object FormHandleSearch: TFormHandleSearch
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Search for token handles'
  ClientHeight = 354
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  DesignSize = (
    673
    354)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStatistics: TLabel
    Left = 90
    Top = 331
    Width = 189
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Found 0 opened handles in 0 processes'
  end
  inline Frame: TFrameTokenList
    Left = 0
    Top = 0
    Width = 673
    Height = 325
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    inherited ListViewTokens: TListViewEx
      Width = 667
      Columns = <
        item
          Caption = 'Description'
          Width = 100
        end
        item
          Caption = 'Type'
          Width = 100
        end
        item
          Caption = 'Access'
          Width = 100
        end
        item
          Caption = 'User'
          Width = 170
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
          Width = 70
        end>
      MultiSelect = True
      GroupView = True
      ReadOnly = True
      PopupMenu = PopupMenu
      OnContextPopup = FrameListViewTokensContextPopup
    end
    inherited SearchBox: TButtonedEdit
      Width = 520
    end
    inherited ComboBoxColumn: TComboBox
      Left = 527
    end
  end
  object ButtonClose: TButton
    Left = 595
    Top = 326
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonRefresh: TButton
    Left = 3
    Top = 326
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = ButtonRefreshClick
  end
  object PopupMenu: TPopupMenu
    Left = 336
    Top = 264
    object TokenObtain: TMenuItem
      Caption = 'Get this handle'
      ShortCut = 24644
      OnClick = ActionObtain
    end
  end
end
