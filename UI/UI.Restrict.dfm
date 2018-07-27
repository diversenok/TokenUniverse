object DialogRestrictToken: TDialogRestrictToken
  Left = 0
  Top = 0
  Caption = 'Create restricted token'
  ClientHeight = 311
  ClientWidth = 337
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    337
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 258
    Top = 281
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 177
    Top = 281
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = DoCloseForm
  end
  object PageControl1: TPageControl
    Left = 4
    Top = 6
    Width = 329
    Height = 252
    ActivePage = TabSheetSidDisable
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheetSidDisable: TTabSheet
      Caption = 'SIDs to disable'
      object ListViewDisableSID: TGroupListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 315
        Height = 218
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'SID'
            Width = 220
          end
          item
            Caption = 'State'
            Width = 110
          end
          item
            Caption = 'Flags'
            Width = 120
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
      end
    end
    object TabSheetSidRestict: TTabSheet
      Caption = 'SIDs to restrict'
      ImageIndex = 1
      DesignSize = (
        321
        224)
      object CheckBoxWriteRestrict: TCheckBox
        Left = 6
        Top = 4
        Width = 312
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Write restricted'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object ListViewRestrictSID: TGroupListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 315
        Height = 195
        Margins.Top = 26
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'SID'
            Width = 220
          end
          item
            Caption = 'State'
            Width = 110
          end
          item
            Caption = 'Flags'
            Width = 120
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
        ColoringItems = True
      end
    end
    object TabSheetPrivDelete: TTabSheet
      Caption = 'Privileges to delete'
      ImageIndex = 2
      DesignSize = (
        321
        224)
      object CheckBoxDisableMaxPriv: TCheckBox
        Left = 6
        Top = 4
        Width = 312
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Disable maximum privileges'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object ListViewPrivileges: TPrivilegesListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 315
        Height = 195
        Margins.Top = 26
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'Privilege name'
            Width = 180
          end
          item
            Caption = 'State'
            Width = 110
          end
          item
            Caption = 'Description'
            Width = 220
          end
          item
            Alignment = taCenter
            Caption = 'LUID'
            Width = 40
          end>
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
        ColoringItems = True
      end
    end
  end
  object CheckBoxLUA: TCheckBox
    Left = 14
    Top = 266
    Width = 123
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'LUA token'
    TabOrder = 3
  end
  object CheckBoxSandboxInert: TCheckBox
    Left = 14
    Top = 285
    Width = 120
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Sandbox inert'
    TabOrder = 4
  end
end
