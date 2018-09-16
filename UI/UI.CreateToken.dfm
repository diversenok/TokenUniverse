object DialogCreateToken: TDialogCreateToken
  Left = 0
  Top = 0
  Caption = 'Create new token'
  ClientHeight = 363
  ClientWidth = 348
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 344
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    348
    363)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 189
    Top = 333
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 270
    Top = 333
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 342
    Height = 327
    Margins.Bottom = 33
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 2
    object TabGeneral: TTabSheet
      Caption = 'General'
      DesignSize = (
        334
        299)
      object StaticLogonID: TStaticText
        Left = 7
        Top = 72
        Width = 76
        Height = 17
        Caption = 'Logon Session:'
        TabOrder = 0
      end
      object StaticOwner: TStaticText
        Left = 7
        Top = 123
        Width = 40
        Height = 17
        Caption = 'Owner:'
        TabOrder = 1
      end
      object StaticPrimaryGroup: TStaticText
        Left = 7
        Top = 146
        Width = 75
        Height = 17
        Caption = 'Primary group:'
        TabOrder = 2
      end
      object StaticDacl: TStaticText
        Left = 7
        Top = 169
        Width = 72
        Height = 17
        Caption = 'Default DACL:'
        TabOrder = 3
      end
      object StaticSource: TStaticText
        Left = 7
        Top = 99
        Width = 41
        Height = 17
        Caption = 'Source:'
        TabOrder = 4
      end
      object EditSource: TEdit
        Left = 109
        Top = 95
        Width = 84
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxLength = 8
        ParentFont = False
        TabOrder = 5
        Text = 'TOK_UNIV'
      end
      object ComboLogonSession: TComboBox
        Left = 109
        Top = 68
        Width = 217
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        Text = 'Choose a logon session'
      end
      object StaticTokenType: TStaticText
        Left = 7
        Top = 18
        Width = 62
        Height = 17
        Caption = 'Token type:'
        TabOrder = 7
      end
      object ComboTokenType: TComboBox
        Left = 109
        Top = 14
        Width = 217
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 4
        TabOrder = 8
        Text = 'Primary token'
        Items.Strings = (
          'Anonymous'
          'Identification'
          'Impersonation'
          'Delegation'
          'Primary token')
      end
      object ComboUser: TComboBox
        Left = 109
        Top = 41
        Width = 186
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 9
      end
      object StaticUser: TStaticText
        Left = 7
        Top = 45
        Width = 30
        Height = 17
        Caption = 'User:'
        TabOrder = 10
      end
      object ButtonPickUser: TButton
        Left = 301
        Top = 38
        Width = 25
        Height = 25
        Hint = 'Use default user selection dialog'
        Anchors = [akTop, akRight]
        ImageIndex = 2
        ImageMargins.Left = 2
        ImageMargins.Top = 1
        Images = FormMain.SmallIcons
        TabOrder = 11
      end
      object ButtonLoad: TButton
        Left = 3
        Top = 271
        Width = 118
        Height = 25
        Caption = 'Load entries from...'
        TabOrder = 12
      end
    end
    object TabGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      DesignSize = (
        334
        299)
      object ListViewGroups: TGroupListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 328
        Height = 266
        Margins.Bottom = 30
        Align = alClient
        Columns = <
          item
            Caption = 'Group name'
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
        FullDrag = True
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
      end
      object ButtonAddSID: TButton
        Left = 127
        Top = 271
        Width = 78
        Height = 25
        Anchors = [akBottom]
        Caption = 'Add SID'
        ImageIndex = 1
        ImageMargins.Left = 3
        ImageMargins.Top = 1
        Images = FormMain.SmallIcons
        TabOrder = 1
      end
    end
    object TabPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 2
      object ListViewPrivileges: TPrivilegesListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 328
        Height = 293
        Align = alClient
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
        FullDrag = True
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
      end
    end
  end
  object ComboBoxView: TComboBox
    Left = 3
    Top = 335
    Width = 158
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 0
    TabOrder = 3
    Text = 'Resolve users and groups'
    Items.Strings = (
      'Resolve users and groups'
      'Show SIDs')
  end
end
