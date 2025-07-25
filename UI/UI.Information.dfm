object InfoDialog: TInfoDialog
  Left = 0
  Top = 0
  Caption = 'Token Information'
  ClientHeight = 448
  ClientWidth = 402
  Color = clBtnFace
  Constraints.MinHeight = 484
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 396
    Height = 415
    Margins.Bottom = 30
    ActivePage = TabGeneral
    Align = alClient
    DoubleBuffered = True
    MultiLine = True
    ParentDoubleBuffered = False
    TabOrder = 0
    OnChange = PageControlChange
    object TabGeneral: TTabSheet
      Caption = 'General'
      object StaticUser: TStaticText
        Left = 7
        Top = 128
        Width = 30
        Height = 17
        Caption = 'User:'
        TabOrder = 18
      end
      object EditUser: TEdit
        Left = 112
        Top = 124
        Width = 270
        Height = 21
        Margins.Right = 6
        Anchors = [akLeft, akTop, akRight]
        AutoSelect = False
        AutoSize = False
        ReadOnly = True
        TabOrder = 0
        Text = 'Unknown User'
        OnDblClick = EditUserDblClick
      end
      object StaticSession: TStaticText
        Left = 7
        Top = 182
        Width = 44
        Height = 17
        Caption = 'Session:'
        TabOrder = 19
      end
      object StaticIntegrity: TStaticText
        Left = 7
        Top = 209
        Width = 75
        Height = 17
        Caption = 'Integrity level:'
        TabOrder = 20
      end
      object ComboSession: TComboBox
        Left = 112
        Top = 178
        Width = 244
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'Unknown Session'
        OnChange = SetStaleColor
      end
      object ComboIntegrity: TComboBox
        Left = 112
        Top = 205
        Width = 244
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'Unknown Integrity Level'
        OnChange = SetStaleColor
      end
      object StaticOwner: TStaticText
        Left = 9
        Top = 317
        Width = 40
        Height = 17
        Caption = 'Owner:'
        TabOrder = 21
      end
      object ComboOwner: TComboBox
        Left = 112
        Top = 313
        Width = 244
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 14
        Text = 'Unknown Owner'
        OnChange = SetStaleColor
      end
      object ComboPrimary: TComboBox
        Left = 112
        Top = 340
        Width = 244
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 16
        Text = 'Unknown Primary group'
        OnChange = SetStaleColor
      end
      object StaticPrimary: TStaticText
        Left = 9
        Top = 344
        Width = 75
        Height = 17
        Caption = 'Primary group:'
        TabOrder = 22
      end
      object StaticUIAccess: TStaticText
        Left = 7
        Top = 236
        Width = 52
        Height = 17
        Caption = 'UIAccess:'
        TabOrder = 23
      end
      object ComboUIAccess: TComboBox
        Left = 112
        Top = 232
        Width = 244
        Height = 21
        Hint = 
          'UIAccess flag is used by accessibility applications and allows t' +
          'hem to bypass some UIPI (User Interface Privilege Isolation) res' +
          'trictions like sending messages to windows with higher integrity' +
          ' levels or installing global hooks.'
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        Text = 'Unknown UIAccess'
        OnChange = SetStaleColor
        Items.Strings = (
          'Disabled'
          'Enabled')
      end
      object StaticText1: TStaticText
        Left = 7
        Top = 263
        Width = 90
        Height = 17
        Caption = 'Mandatory policy:'
        TabOrder = 24
      end
      object CheckBoxNoWriteUp: TCheckBox
        Left = 112
        Top = 261
        Width = 97
        Height = 17
        Hint = 
          'A process associated with the token cannot write to objects that' +
          ' have a greater mandatory integrity label.'
        Caption = 'No Write Up'
        State = cbGrayed
        TabOrder = 7
        OnClick = CheckBoxClick
      end
      object CheckBoxNewProcessMin: TCheckBox
        Left = 240
        Top = 261
        Width = 116
        Height = 17
        Hint = 
          'A process created with the token has an integrity level that is ' +
          'the lesser of the parent-process integrity level and the executa' +
          'ble-file integrity label.'
        Anchors = [akTop, akRight]
        Caption = 'New Process Min'
        State = cbGrayed
        TabOrder = 8
        OnClick = CheckBoxClick
      end
      object StaticVirtualization: TStaticText
        Left = 7
        Top = 290
        Width = 69
        Height = 17
        Caption = 'Virtualization:'
        TabOrder = 25
      end
      object CheckBoxVAllowed: TCheckBox
        Left = 112
        Top = 288
        Width = 73
        Height = 17
        Caption = 'Allowed'
        State = cbGrayed
        TabOrder = 10
        OnClick = CheckBoxClick
      end
      object CheckBoxVEnabled: TCheckBox
        Left = 240
        Top = 288
        Width = 116
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Enabled'
        State = cbGrayed
        TabOrder = 12
        OnClick = CheckBoxClick
      end
      object BtnSetSession: TButton
        Left = 360
        Top = 177
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 2
        OnClick = BtnSetSessionClick
      end
      object BtnSetIntegrity: TButton
        Left = 360
        Top = 204
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 4
        OnClick = BtnSetIntegrityClick
      end
      object BtnSetUIAccess: TButton
        Left = 360
        Top = 231
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 6
        OnClick = BtnSetUIAccessClick
      end
      object BtnSetOwner: TButton
        Left = 360
        Top = 312
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 15
        OnClick = BtnSetOwnerClick
      end
      object BtnSetPolicy: TButton
        Left = 360
        Top = 259
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 9
        OnClick = BtnSetPolicyClick
      end
      object BtnSetPrimary: TButton
        Left = 360
        Top = 339
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 17
        OnClick = BtnSetPrimaryClick
      end
      object BtnSetVEnabled: TButton
        Left = 360
        Top = 285
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 13
        OnClick = BtnSetVEnabledClick
      end
      object BtnSetAEnabled: TButton
        Left = 191
        Top = 284
        Width = 24
        Height = 23
        ImageAlignment = iaCenter
        ImageIndex = 5
        Images = FormMain.SmallIcons
        TabOrder = 11
        OnClick = BtnSetVAllowedClick
      end
      object StaticAppContainer: TStaticText
        Left = 7
        Top = 155
        Width = 74
        Height = 17
        Caption = 'AppContainer:'
        TabOrder = 26
      end
      object EditAppContainer: TEdit
        Left = 112
        Top = 150
        Width = 270
        Height = 21
        Margins.Right = 6
        Anchors = [akLeft, akTop, akRight]
        AutoSelect = False
        AutoSize = False
        ReadOnly = True
        TabOrder = 27
        Text = 'Unknown AppContainer'
        OnDblClick = EditAppContainerDblClick
      end
      object PanelGeneral: TPanel
        Left = 0
        Top = 0
        Width = 388
        Height = 118
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 28
        object ListViewGeneral: TListViewEx
          Left = 0
          Top = 0
          Width = 388
          Height = 118
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Width = 120
            end
            item
              AutoSize = True
            end>
          Groups = <
            item
              Header = 'General information'
              GroupID = 0
              State = [lgsNormal]
              HeaderAlign = taLeftJustify
              FooterAlign = taLeftJustify
              TitleImage = -1
            end>
          Items.ItemData = {
            055F0100000500000000000000FFFFFFFFFFFFFFFF0100000000000000000000
            000E4F0062006A00650063007400200061006400640072006500730073000755
            006E006B006E006F0077006E0010B8871600000000FFFFFFFFFFFFFFFF010000
            0000000000000000000C480061006E0064006C0065002000760061006C007500
            65000755006E006B006E006F0077006E00C07E871600000000FFFFFFFFFFFFFF
            FF0100000000000000000000000E4700720061006E0074006500640020006100
            630063006500730073000755006E006B006E006F0077006E00B87B8514000000
            00FFFFFFFFFFFFFFFF0100000000000000000000000A54006F006B0065006E00
            200074007900700065000755006E006B006E006F0077006E0058548514000000
            00FFFFFFFFFFFFFFFF0100000000000000000000000845006C00650076006100
            7400650064000755006E006B006E006F0077006E0000558514FFFFFFFFFFFFFF
            FFFFFF}
          MultiSelect = True
          GroupView = True
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          OnDblClick = ListViewGeneralDblClick
          ClipboardSourceColumn = 1
        end
      end
    end
    object TabAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 4
      object ListViewAdvanced: TListViewEx
        Left = 0
        Top = 0
        Width = 388
        Height = 369
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Width = 140
          end
          item
            Width = 220
          end>
        Groups = <
          item
            Header = 'Token Source'
            GroupID = 0
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
          end
          item
            Header = 'Token Statistics'
            GroupID = 1
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
          end
          item
            Header = 'Other'
            GroupID = 2
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
          end>
        Items.ItemData = {
          05050300000B00000000000000FFFFFFFFFFFFFFFF0100000000000000000000
          00044E0061006D0065000755006E006B006E006F0077006E0050763D1B000000
          00FFFFFFFFFFFFFFFF010000000000000000000000044C005500490044000755
          006E006B006E006F0077006E0048CC3D1B00000000FFFFFFFFFFFFFFFF010000
          0001000000000000000854006F006B0065006E002000490044000755006E006B
          006E006F0077006E0030C5D22C00000000FFFFFFFFFFFFFFFF01000000010000
          000000000017410075007400680065006E007400690063006100740069006F00
          6E002F004C006F0067006F006E002000490044000755006E006B006E006F0077
          006E00208AD22C00000000FFFFFFFFFFFFFFFF0100000001000000000000000F
          450078007000690072006100740069006F006E002000540069006D0065000755
          006E006B006E006F0077006E00A08DD22C00000000FFFFFFFFFFFFFFFF010000
          0001000000000000000F440079006E0061006D00690063002000430068006100
          72006700650064000755006E006B006E006F0077006E00B0A5D22C00000000FF
          FFFFFFFFFFFFFF01000000010000000000000011440079006E0061006D006900
          6300200041007600610069006C00610062006C0065000755006E006B006E006F
          0077006E008087D22C00000000FFFFFFFFFFFFFFFF0100000001000000000000
          000B470072006F0075007000200043006F0075006E0074000755006E006B006E
          006F0077006E00686AD22C00000000FFFFFFFFFFFFFFFF010000000100000000
          0000000F500072006900760069006C00650067006500200043006F0075006E00
          74000755006E006B006E006F0077006E007882D22C00000000FFFFFFFFFFFFFF
          FF0100000001000000000000000B4D006F006400690066006900650064002000
          490044000755006E006B006E006F0077006E004872D22C00000000FFFFFFFFFF
          FFFFFF0100000002000000000000000546006C006100670073000755006E006B
          006E006F0077006E00E890D22CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF}
        MultiSelect = True
        GroupView = True
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnResize = ListViewAdvancedResize
        ClipboardSourceColumn = 1
      end
    end
    object TabLogon: TTabSheet
      Caption = 'Logon'
      ImageIndex = 9
      inline FrameLogon: TFrameLogon
        Left = 0
        Top = 0
        Width = 388
        Height = 369
        Align = alClient
        TabOrder = 0
      end
    end
    object TabObject: TTabSheet
      Caption = 'Object'
      ImageIndex = 5
      object ListViewProcesses: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 207
        Width = 382
        Height = 159
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Process name'
            Width = 130
          end
          item
            Alignment = taCenter
            Caption = 'PID'
          end
          item
            Alignment = taCenter
            Caption = 'Handle'
            Width = 60
          end
          item
            AutoSize = True
            Caption = 'Access'
          end>
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object PanelObject: TPanel
        Left = 0
        Top = 0
        Width = 388
        Height = 201
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object ListViewObject: TListViewEx
          Left = 0
          Top = 0
          Width = 388
          Height = 201
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Width = 140
            end
            item
              Width = 220
            end>
          Groups = <
            item
              Header = 'Kernel object'
              GroupID = 0
              State = [lgsNormal]
              HeaderAlign = taLeftJustify
              FooterAlign = taLeftJustify
              TitleImage = -1
            end
            item
              Header = 'Quota charges'
              GroupID = 1
              State = [lgsNormal]
              HeaderAlign = taLeftJustify
              FooterAlign = taLeftJustify
              TitleImage = -1
            end
            item
              Header = 'References'
              GroupID = 2
              State = [lgsNormal]
              HeaderAlign = taLeftJustify
              FooterAlign = taLeftJustify
              TitleImage = -1
            end>
          Items.ItemData = {
            05F90100000700000000000000FFFFFFFFFFFFFFFF0100000000000000000000
            000E4F0062006A00650063007400200061006400640072006500730073000755
            006E006B006E006F0077006E0030AA981600000000FFFFFFFFFFFFFFFF010000
            000000000000000000125300700065006300690061006C002000610074007400
            72006900620075007400650073000755006E006B006E006F0077006E0028C898
            1600000000FFFFFFFFFFFFFFFF01000000010000000000000005500061006700
            650064000755006E006B006E006F0077006E0078AD981600000000FFFFFFFFFF
            FFFFFF010000000100000000000000094E006F006E0020005000610067006500
            64000755006E006B006E006F0077006E005079451E00000000FFFFFFFFFFFFFF
            FF0100000002000000000000000850006F0069006E0074006500720073000755
            006E006B006E006F0077006E00B889451E00000000FFFFFFFFFFFFFFFF010000
            00020000000000000007480061006E0064006C00650073000755006E006B006E
            006F0077006E00E086451E00000000FFFFFFFFFFFFFFFF010000000000000000
            00000007430072006500610074006F0072001B3C002000460065006100740075
            007200650020006900730020006E006F007400200073007500700070006F0072
            007400650064003E008887451EFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          MultiSelect = True
          GroupView = True
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          ClipboardSourceColumn = 1
        end
      end
    end
    object TabGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 1
      inline GroupsMemberFrame: TFrameGroups
        Left = 0
        Top = 0
        Width = 388
        Height = 369
        Align = alClient
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        inherited VST: TDevirtualizedTree
          Width = 388
          Height = 369
          PopupMenuEx = GroupPopup
        end
      end
    end
    object TabPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 2
      inline PrivilegesFrame: TFramePrivileges
        Left = 0
        Top = 0
        Width = 388
        Height = 369
        Align = alClient
        TabOrder = 0
        inherited VST: TDevirtualizedTree
          Width = 388
          Height = 369
          PopupMenuEx = PrivilegePopup
        end
      end
    end
    object TabRestricted: TTabSheet
      Caption = 'Restricting SIDs'
      ImageIndex = 3
      inline GroupsRestrictedFrame: TFrameGroups
        Left = 0
        Top = 0
        Width = 388
        Height = 369
        Align = alClient
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        inherited VST: TDevirtualizedTree
          Width = 388
          Height = 369
        end
      end
    end
    object TabDefaultDacl: TTabSheet
      Caption = 'Default DACL'
      ImageIndex = 7
      inline DefaultDaclFrame: TAclFrame
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 388
        Height = 336
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 33
        Align = alClient
        Constraints.MinHeight = 165
        Constraints.MinWidth = 320
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        inherited Tree: TDevirtualizedTree
          Width = 360
          Height = 311
        end
        inherited RightPanel: TPanel
          Left = 363
          Height = 311
          inherited btnAdd: TButton
            Top = 79
          end
          inherited btnCanonicalize: TButton
            Top = 143
          end
          inherited btnDelete: TButton
            Top = 206
          end
          inherited btnDown: TButton
            Top = 286
          end
        end
        inherited Search: TSearchFrame
          Width = 388
          inherited Splitter: TSplitter
            Left = 222
          end
          inherited tbxSearchBox: TButtonedEditEx
            Width = 222
          end
          inherited cbxColumn: TComboBox
            Left = 228
          end
        end
      end
      object btnDaclApply: TButton
        Left = 310
        Top = 341
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        TabOrder = 1
        OnClick = btnDaclApplyClick
      end
    end
    object TabAudit: TTabSheet
      Caption = 'Audit Overrides'
      ImageIndex = 8
      inline FrameAudit: TFrameAudit
        Left = 0
        Top = 0
        Width = 388
        Height = 369
        Align = alClient
        TabOrder = 0
        inherited LabelStatus: TLabel
          Top = 347
          Width = 279
        end
        inherited ListView: TListViewEx
          Width = 382
          Height = 337
        end
        inherited ButtonApply: TButton
          Left = 288
          Top = 342
        end
      end
    end
  end
  object ButtonClose: TButton
    Left = 324
    Top = 420
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
    OnClick = DoCloseForm
  end
  object ImageList: TImageList
    Left = 167
    Top = 307
  end
  object PrivilegePopup: TPopupMenu
    Left = 295
    Top = 307
    object MenuPrivEnable: TMenuItem
      Caption = 'Enable'
      ShortCut = 16453
      OnClick = ActionPrivilegeEnable
    end
    object MenuPrivDisable: TMenuItem
      Caption = 'Disable'
      ShortCut = 16452
      OnClick = ActionPrivilegeDisable
    end
    object MenuPrivRemove: TMenuItem
      Caption = 'Remove'
      ShortCut = 46
      OnClick = ActionPrivilegeRemove
    end
  end
  object GroupPopup: TPopupMenu
    Left = 231
    Top = 307
    object MenuGroupEnable: TMenuItem
      Caption = 'Enable'
      ShortCut = 16453
      OnClick = ActionGroupEnable
    end
    object MenuGroupDisable: TMenuItem
      Caption = 'Disable'
      ShortCut = 16452
      OnClick = ActionGroupDisable
    end
    object MenuGroupReset: TMenuItem
      Caption = 'Reset all'
      ShortCut = 16466
      OnClick = ActionGroupReset
    end
  end
end
