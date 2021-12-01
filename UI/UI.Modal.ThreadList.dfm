object ThreadListDialog: TThreadListDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Threads'
  ClientHeight = 225
  ClientWidth = 275
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListViewThreads: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 269
    Height = 189
    Margins.Bottom = 33
    Align = alClient
    Columns = <
      item
        Caption = 'Thread ID'
        Width = 90
      end
      item
        Alignment = taCenter
        Caption = 'Created'
        Width = 140
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListViewThreadsDblClick
    OnSelectItem = ListViewThreadsSelectItem
    ColoringItems = True
    PopupOnItemsOnly = True
  end
  object ButtonOk: TButton
    Left = 197
    Top = 196
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 3
    Top = 196
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PopupMenu: TPopupMenu
    Left = 56
    Top = 104
    object cmSuspend: TMenuItem
      Caption = '&Suspend'
      OnClick = cmActionClick
    end
    object cmResume: TMenuItem
      Caption = '&Resume'
      OnClick = cmActionClick
    end
    object cmTerminate: TMenuItem
      Caption = '&Terminate'
      ShortCut = 46
      OnClick = cmActionClick
    end
  end
end
