object DialogRestrictToken: TDialogRestrictToken
  Left = 0
  Top = 0
  Caption = 'Create Restricted Token'
  ClientHeight = 415
  ClientWidth = 360
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 340
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
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 200
    Top = 382
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 281
    Top = 382
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
    Width = 352
    Height = 356
    ActivePage = TabSheetSidDisable
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheetSidDisable: TTabSheet
      Caption = 'SIDs to disable'
      inline FrameGroupsDisable: TFrameGroups
        Left = 0
        Top = 0
        Width = 344
        Height = 328
        Align = alClient
        TabOrder = 0
        inherited ListView: TListViewEx
          Width = 338
          Height = 322
          Checkboxes = True
        end
      end
    end
    object TabSheetSidRestict: TTabSheet
      Caption = 'SIDs to restrict'
      ImageIndex = 1
      object CheckBoxWriteOnly: TCheckBox
        Left = 3
        Top = 308
        Width = 217
        Height = 17
        Hint = 
          'Restricting SIDs are always considered when evaluating write acc' +
          'ess. Also consider them when evaluating read and execute access.' +
          #13#10#13#10'Note: unchecking this item results in a less restrictive tok' +
          'en since the checks against the list above will take effect ONLY' +
          ' on attempts to perform write access. Such a token is also calle' +
          'd a "write-restricted token".'
        Anchors = [akLeft, akBottom]
        Caption = 'Restrict read && execute access'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        State = cbChecked
        TabOrder = 1
      end
      object ButtonAddSID: TButton
        Left = 263
        Top = 296
        Width = 78
        Height = 25
        Hint = 'Add a new restricted SID that is not present in the list.'
        Anchors = [akRight, akBottom]
        Caption = 'Add SID'
        ImageIndex = 1
        ImageMargins.Left = 3
        ImageMargins.Top = 1
        Images = FormMain.SmallIcons
        TabOrder = 2
        OnClick = ButtonAddSIDClick
      end
      object CheckBoxUsual: TCheckBox
        Left = 3
        Top = 291
        Width = 217
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Restrict write access'
        Checked = True
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        State = cbChecked
        TabOrder = 3
      end
      inline FrameGroupsRestrict: TFrameGroups
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 344
        Height = 287
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 41
        Align = alClient
        TabOrder = 0
        inherited ListView: TListViewEx
          Width = 338
          Height = 281
          Checkboxes = True
          PopupMenu = PopupMenu
          OnContextPopup = ListViewRestrictSIDContextPopup
          OnDblClick = MenuEditClick
        end
      end
    end
    object TabSheetPrivDelete: TTabSheet
      Caption = 'Privileges to delete'
      ImageIndex = 2
      object CheckBoxDisableMaxPriv: TCheckBox
        Left = 6
        Top = 4
        Width = 335
        Height = 17
        Hint = 
          'Ignore the list below and delete all the privileges except `SeCh' +
          'angeNotifyPrivilege`.'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Delete maximum privileges'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      inline PrivilegesFrame: TPrivilegesFrame
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 338
        Height = 299
        Margins.Top = 26
        Align = alClient
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        inherited ListViewEx: TListViewEx
          Width = 338
          Height = 299
          Checkboxes = True
        end
      end
    end
  end
  object CheckBoxLUA: TCheckBox
    Left = 14
    Top = 368
    Width = 120
    Height = 15
    Hint = 
      'Disable administrative SIDs and delete some privileges as UAC do' +
      'es.'
    Anchors = [akLeft, akBottom]
    Caption = 'LUA token'
    TabOrder = 3
  end
  object CheckBoxSandboxInert: TCheckBox
    Left = 14
    Top = 389
    Width = 120
    Height = 17
    Hint = 
      'Does not check AppLocker rules or apply Software Restriction Pol' +
      'icies for the process with this token.'#13#10#13#10'This action might requ' +
      'ire SeTcbPrivilege to take effect.'
    Anchors = [akLeft, akBottom]
    Caption = 'Sandbox inert'
    TabOrder = 4
  end
  object PopupMenu: TPopupMenu
    Left = 232
    Top = 128
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
