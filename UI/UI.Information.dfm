object InfoDialog: TInfoDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Information about'
  ClientHeight = 339
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    404
    339)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 398
    Height = 306
    ActivePage = TabGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabGeneral: TTabSheet
      Caption = 'General'
      DesignSize = (
        390
        278)
      object StaticUser: TStaticText
        Left = 7
        Top = 17
        Width = 30
        Height = 17
        Caption = 'User:'
        TabOrder = 0
      end
      object EditUser: TEdit
        Left = 91
        Top = 13
        Width = 293
        Height = 21
        Margins.Right = 6
        Anchors = [akLeft, akTop, akRight]
        AutoSelect = False
        AutoSize = False
        ReadOnly = True
        TabOrder = 1
        Text = 'Unknown user'
      end
      object StaticSID: TStaticText
        Left = 7
        Top = 44
        Width = 50
        Height = 17
        Caption = 'User SID:'
        TabOrder = 2
      end
      object EditSID: TEdit
        Left = 91
        Top = 40
        Width = 293
        Height = 21
        Margins.Right = 6
        Anchors = [akLeft, akTop, akRight]
        AutoSelect = False
        AutoSize = False
        ReadOnly = True
        TabOrder = 3
        Text = 'Unknown SID'
      end
    end
    object TabGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 389
      ExplicitHeight = 0
      object ListViewGroups: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 384
        Height = 272
        Align = alClient
        Columns = <
          item
            Caption = 'Group name'
            Width = 220
          end
          item
            Caption = 'Flags'
            Width = 140
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 2
      object ListViewPrivileges: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 384
        Height = 272
        Align = alClient
        Columns = <
          item
            Caption = 'Privilege name'
            Width = 180
          end
          item
            Caption = 'Flags'
            Width = 140
          end
          item
            Caption = 'Description'
            Width = 230
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabRestricted: TTabSheet
      Caption = 'Restricted SIDs'
      ImageIndex = 3
      object ListViewRestricted: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 384
        Height = 272
        Align = alClient
        Columns = <
          item
            Caption = 'User or Group'
            Width = 220
          end
          item
            Caption = 'Flags'
            Width = 140
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object ButtonClose: TButton
    Left = 325
    Top = 311
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
    OnClick = ButtonCloseClick
  end
end
