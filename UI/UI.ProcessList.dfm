object ProcessListDialog: TProcessListDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select a process'
  ClientHeight = 312
  ClientWidth = 334
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 280
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = ReloadProcessList
  OnKeyDown = FormKeyDown
  DesignSize = (
    334
    312)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOk: TButton
    Left = 174
    Top = 279
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
    Left = 255
    Top = 279
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonRefresh: TButton
    Left = 3
    Top = 279
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    TabOrder = 4
    OnClick = ReloadProcessList
  end
  object SearchBox: TButtonedEdit
    Left = 3
    Top = 8
    Width = 327
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Images = FormMain.SearchButtons
    LeftButton.ImageIndex = 0
    LeftButton.Visible = True
    RightButton.HotImageIndex = 2
    RightButton.ImageIndex = 1
    RightButton.PressedImageIndex = 3
    TabOrder = 3
    TextHint = 'Search'
    OnChange = SearchBoxChange
    OnRightButtonClick = SearchBoxRightButtonClick
  end
  object ListView: TListViewEx
    Left = 3
    Top = 33
    Width = 327
    Height = 240
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Process name'
        Width = 240
      end
      item
        Alignment = taCenter
        Caption = 'PID'
        Width = 60
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = ListViewSelectItem
  end
end
