object LogonDialog: TLogonDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Logon user'
  ClientHeight = 324
  ClientWidth = 304
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 266
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    304
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelType: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Logon type:'
  end
  object LabelProvider: TLabel
    Left = 8
    Top = 57
    Width = 76
    Height = 13
    Caption = 'Logon provider:'
  end
  object LabelGroups: TLabel
    Left = 8
    Top = 110
    Width = 288
    Height = 17
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Additional groups (requires SeTcbPrivilege)'
  end
  object ComboLogonType: TComboBox
    Left = 8
    Top = 27
    Width = 288
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 0
    Text = 'Interactive'
    Items.Strings = (
      'Interactive'
      'Network'
      'Network clear text'
      'New credentials'
      'Unlock'
      'Batch'
      'Service')
  end
  object ComboLogonProvider: TComboBox
    Left = 8
    Top = 76
    Width = 288
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 1
    Text = 'Default'
    Items.Strings = (
      'Default'
      'Windows NT 3.5'
      'NTLM'
      'Negotiate')
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 291
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonContinue: TButton
    Left = 221
    Top = 291
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Continue'
    Default = True
    TabOrder = 3
    OnClick = ButtonContinueClick
  end
  object ListViewGroups: TListViewEx
    Left = 8
    Top = 128
    Width = 288
    Height = 157
    Margins.Top = 26
    Margins.Bottom = 31
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'SID'
        Width = 160
      end
      item
        Caption = 'State'
        Width = 90
      end
      item
        Caption = 'Flags'
        Width = 120
      end>
    FullDrag = True
    GridLines = True
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu
    TabOrder = 4
    ViewStyle = vsReport
    OnDblClick = MenuEditClick
    ClipboardSourceColumn = 0
    ColoringItems = True
    PopupOnItemsOnly = True
  end
  object ButtonAddSID: TButton
    Left = 114
    Top = 291
    Width = 78
    Height = 25
    Anchors = [akBottom]
    Caption = 'Add SID'
    ImageIndex = 1
    ImageMargins.Left = 3
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 5
    OnClick = ButtonAddSIDClick
  end
  object PopupMenu: TPopupMenu
    Left = 136
    Top = 168
    object MenuEdit: TMenuItem
      Caption = 'Edit'
      ShortCut = 113
      OnClick = MenuEditClick
    end
    object MenuRemove: TMenuItem
      Caption = 'Remove'
      ShortCut = 46
      OnClick = MenuRemoveClick
    end
  end
end
