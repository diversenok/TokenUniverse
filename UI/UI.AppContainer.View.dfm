object DialogAppContainer: TDialogAppContainer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'AppContainer Information'
  ClientHeight = 274
  ClientWidth = 332
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  TextHeight = 13
  object Pages: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 326
    Height = 238
    Margins.Bottom = 33
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 0
    OnChange = PagesChange
    object TabGeneral: TTabSheet
      Caption = 'General'
      object lblDispName: TLabel
        Left = 6
        Top = 161
        Width = 68
        Height = 13
        Caption = 'Display Name:'
      end
      object lblName: TLabel
        Left = 6
        Top = 105
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblSid: TLabel
        Left = 6
        Top = 52
        Width = 21
        Height = 13
        Caption = 'SID:'
      end
      object tbxDispName: TEdit
        Left = 6
        Top = 180
        Width = 306
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 0
        Text = 'Unknown'
      end
      object lnkUser: TLinkLabel
        Left = 6
        Top = 8
        Width = 306
        Height = 14
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'User: <a>Link</a>'
        TabOrder = 1
        OnLinkClick = lnkUserLinkClick
      end
      object tbxName: TEdit
        Left = 6
        Top = 124
        Width = 306
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 2
        Text = 'Unknown'
      end
      object tbxSid: TEdit
        Left = 6
        Top = 71
        Width = 306
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 3
        Text = 'Unknown'
        OnDblClick = tbxSidDblClick
      end
      object lnkParent: TLinkLabel
        Left = 6
        Top = 30
        Width = 306
        Height = 14
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Parent: Unknown'
        TabOrder = 4
        OnLinkClick = lnkParentLinkClick
      end
    end
    object TabChildren: TTabSheet
      Caption = 'Children'
      ImageIndex = 1
      object lvChildren: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 312
        Height = 204
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end>
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = cmInspectClick
        ClipboardSourceColumn = 0
        PopupOnItemsOnly = True
      end
    end
  end
  object ButtonClose: TButton
    Left = 254
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = ButtonCloseClick
  end
  object PopupMenu: TPopupMenu
    Left = 111
    Top = 147
    object cmInspect: TMenuItem
      Caption = 'Inspect'
      Default = True
      ShortCut = 13
      OnClick = cmInspectClick
    end
  end
end
