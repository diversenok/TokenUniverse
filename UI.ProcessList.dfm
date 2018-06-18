object ProcessListDialog: TProcessListDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select a process'
  ClientHeight = 306
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = ButtonRefreshClick
  DesignSize = (
    332
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOk: TButton
    Left = 172
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 253
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonRefresh: TButton
    Left = 3
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = ButtonRefreshClick
  end
  object SearchBox: TButtonedEdit
    Left = 3
    Top = 8
    Width = 325
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    TextHint = 'Search'
  end
  object ListView: TListView
    Left = 3
    Top = 33
    Width = 325
    Height = 234
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Process name'
        Width = 220
      end
      item
        Alignment = taCenter
        Caption = 'PID'
        Width = 60
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    SmallImages = ImageList
    TabOrder = 4
    ViewStyle = vsReport
  end
  object ImageList: TImageList
    ColorDepth = cd32Bit
    Left = 80
    Top = 112
  end
end
