object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Token Universe :: Main Window'
  ClientHeight = 339
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TokenListView: TListView
    AlignWithMargins = True
    Left = 3
    Top = 41
    Width = 747
    Height = 295
    Align = alClient
    Columns = <
      item
        Caption = 'Token name'
        Width = 180
      end
      item
        Caption = 'Type'
        Width = 80
      end
      item
        Caption = 'Access'
        Width = 120
      end
      item
        Caption = 'User'
        Width = 180
      end
      item
        Alignment = taCenter
        Caption = 'Session'
      end
      item
        Alignment = taCenter
        Caption = 'Elevated'
        Width = 55
      end
      item
        Alignment = taCenter
        Caption = 'Integrity'
        Width = 58
      end>
    GridLines = True
    RowSelect = True
    PopupMenu = PopupMenu
    TabOrder = 0
    ViewStyle = vsReport
    OnEdited = TokenListViewEdited
    OnSelectItem = TokenListViewSelectItem
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 753
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 3
      Top = 7
      Width = 86
      Height = 25
      Caption = 'Open Process'
      TabOrder = 0
      OnClick = ActionOpenProcess
    end
  end
  object MainMenu: TMainMenu
    Left = 96
    Top = 112
    object Program1: TMenuItem
      Caption = 'Program'
      object RunasAdministrator1: TMenuItem
        Caption = 'Restart as Administrator'
      end
      object RunasSYSTEM1: TMenuItem
        Caption = 'Restart as SYSTEM'
      end
      object RunasSYSTEM2: TMenuItem
        Caption = 'Restart as SYSTEM+'
      end
    end
    object View1: TMenuItem
      Caption = 'Settings'
    end
    object Help1: TMenuItem
      Caption = 'Help'
    end
  end
  object PopupMenu: TPopupMenu
    Left = 168
    Top = 112
    object TokenDuplicate: TMenuItem
      Caption = 'Duplicate'
      Enabled = False
      ShortCut = 16452
      OnClick = ActionDuplicate
    end
    object TokenRestrict: TMenuItem
      Caption = 'Create restricted copy'
      Enabled = False
      ShortCut = 16466
    end
    object TokenRename: TMenuItem
      Caption = 'Rename'
      Enabled = False
      ShortCut = 113
      OnClick = ActionRename
    end
    object TokenClose: TMenuItem
      Caption = 'Close'
      Enabled = False
      ShortCut = 46
      OnClick = ActionClose
    end
    object HLine1: TMenuItem
      Caption = '-'
    end
    object TokenSendHandle: TMenuItem
      Caption = 'Copy handle to another process'
      Enabled = False
    end
    object TokenRun: TMenuItem
      Caption = 'Run program with this token'
      Enabled = False
      ShortCut = 16453
      OnClick = ActionRunWithToken
    end
    object HLine2: TMenuItem
      Caption = '-'
      Enabled = False
      Visible = False
    end
    object NewMenu: TMenuItem
      Caption = 'New'
      object NewOpenSelf: TMenuItem
        Caption = 'Open current process'
        ShortCut = 24655
        OnClick = ActionOpenSelf
      end
      object NewOpenProcess: TMenuItem
        Caption = 'Open other process'
        ShortCut = 16463
        OnClick = ActionOpenProcess
      end
      object NewOpenThread: TMenuItem
        Caption = 'Open other thread'
        Enabled = False
      end
      object HLine3: TMenuItem
        Caption = '-'
      end
      object NewLogonUser: TMenuItem
        Caption = 'Logon user'
        Enabled = False
        ShortCut = 16460
      end
      object NewQueryUserToken: TMenuItem
        Caption = 'WTSQueryUserToken'
        Enabled = False
      end
      object NewSaferApi: TMenuItem
        Caption = 'Create using Safer API'
        Enabled = False
      end
      object NewNtCreateToken: TMenuItem
        Caption = 'Create using NtCreateToken'
        Enabled = False
        ShortCut = 16462
      end
      object HLine4: TMenuItem
        Caption = '-'
      end
      object NewCopyHandle: TMenuItem
        Caption = 'Copy handle from other process'
        Enabled = False
        ShortCut = 24643
      end
      object NewSearchHandle: TMenuItem
        Caption = 'Search for token handles'
        Enabled = False
        ShortCut = 16454
      end
    end
    object ProgramRun: TMenuItem
      Caption = 'Run program'
      object MenuItem21: TMenuItem
        Caption = 'CreateProcess'
        Enabled = False
      end
      object MenuItem22: TMenuItem
        Caption = 'ShellExecuteEx'
        Enabled = False
      end
      object RunTaskAsInteractiveUser1: TMenuItem
        Caption = 'RunTaskAsInteractiveUser'
        Enabled = False
      end
      object MenuItem23: TMenuItem
        Caption = 'CreateProcessWithLogonW'
        Enabled = False
      end
    end
  end
end
