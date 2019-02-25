object DialogPickToken: TDialogPickToken
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select Token'
  ClientHeight = 202
  ClientWidth = 256
  Color = clBtnFace
  Constraints.MinHeight = 230
  Constraints.MinWidth = 250
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object ListViewTokens: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 250
    Height = 170
    Margins.Bottom = 29
    Align = alClient
    Columns = <
      item
        Width = 200
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = ListViewTokensSelectItem
    ClipboardSourceColumn = 0
  end
  object ButtonCancel: TButton
    Left = 178
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 97
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
end
