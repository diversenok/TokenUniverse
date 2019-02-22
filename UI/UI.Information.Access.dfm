object DialogGrantedAccess: TDialogGrantedAccess
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Granted Access Rights'
  ClientHeight = 383
  ClientWidth = 206
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 222
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
  object ListViewAccess: TListViewEx
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 200
    Height = 349
    Margins.Bottom = 31
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Width = 178
      end>
    MultiSelect = True
    GroupView = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
  end
  object ButtonClose: TButton
    Left = 128
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 8
    TabOrder = 1
  end
end
