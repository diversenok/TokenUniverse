object AccessCheckForm: TAccessCheckForm
  Left = 0
  Top = 0
  Caption = 'Check Object Access'
  ClientHeight = 527
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControlModes: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
    Height = 102
    ActivePage = TabByName
    Align = alTop
    TabOrder = 0
    object TabByName: TTabSheet
      Caption = 'By NT Name'
      object lblNameType: TLabel
        Left = 3
        Top = 16
        Width = 63
        Height = 13
        Caption = 'Object Type:'
      end
      object lblName: TLabel
        Left = 3
        Top = 43
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object tbxName: TEdit
        Left = 72
        Top = 40
        Width = 537
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        TextHint = 'e.g.: \Registry\Machine\Software or \DosDevices\C:\Windows'
        OnChange = tbxNameChange
      end
      object tbxNameType: TEdit
        Left = 72
        Top = 13
        Width = 537
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        ReadOnly = True
        TabOrder = 1
        Text = 'Unknown'
      end
    end
    object TabByCID: TTabSheet
      Caption = 'By CID'
      ImageIndex = 1
      object lblCidtType: TLabel
        Left = 3
        Top = 16
        Width = 63
        Height = 13
        Caption = 'Parent Type:'
      end
      object lblCid: TLabel
        Left = 3
        Top = 43
        Width = 45
        Height = 13
        Caption = 'Client ID:'
      end
      object lblCidSubType: TLabel
        Left = 324
        Top = 16
        Width = 54
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Child Type:'
      end
      object tbxCid: TEdit
        Left = 72
        Top = 40
        Width = 457
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        TextHint = 'PID or TID value'
        OnChange = tbxCidChange
      end
      object cbxCidType: TComboBox
        Left = 72
        Top = 13
        Width = 225
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Process'
        OnChange = tbxCidChange
        Items.Strings = (
          'Process'
          'Thread')
      end
      object btnSelectCid: TButton
        Left = 534
        Top = 38
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Select...'
        TabOrder = 2
        OnClick = btnSelectCidClick
      end
      object cbxCidSubType: TComboBox
        Left = 384
        Top = 13
        Width = 225
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 3
        Text = 'None'
        OnChange = tbxCidChange
        Items.Strings = (
          'None'
          'Token'
          'Debug Object')
      end
    end
    object TabBySid: TTabSheet
      Caption = 'By SID'
      ImageIndex = 4
      object lblSidType: TLabel
        Left = 3
        Top = 16
        Width = 49
        Height = 13
        Caption = 'Category:'
      end
      object lblSid: TLabel
        Left = 3
        Top = 43
        Width = 21
        Height = 13
        Caption = 'SID:'
      end
      object lblSidLookupType: TLabel
        Left = 424
        Top = 16
        Width = 48
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'SID Type:'
      end
      object cbxSidType: TComboBox
        Left = 72
        Top = 13
        Width = 337
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 0
        Text = 'LSA Account'
        OnChange = tbxSidChange
        Items.Strings = (
          'LSA Account'
          'SAM Object (Domain/Group/Alias/User)')
      end
      inline SidEditor: TSidEditor
        Left = 72
        Top = 38
        Width = 538
        Height = 27
        Anchors = [akLeft, akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        inherited tbxSid: TEdit
          Width = 481
        end
        inherited btnDsPicker: TButton
          Left = 513
        end
        inherited btnCheatsheet: TButton
          Left = 485
        end
      end
      object tbxSidLookupType: TEdit
        Left = 480
        Top = 13
        Width = 130
        Height = 21
        Anchors = [akTop, akRight]
        Enabled = False
        NumbersOnly = True
        TabOrder = 2
        Text = 'Unknown'
      end
    end
  end
  inline AccessMaskFrame: TAccessMaskFrame
    AlignWithMargins = True
    Left = 3
    Top = 111
    Width = 629
    Height = 383
    Margins.Bottom = 33
    Align = alClient
    Constraints.MinHeight = 200
    Constraints.MinWidth = 180
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    inherited ListViewEx: TListViewEx
      Width = 629
      Height = 354
      Columns = <
        item
          AutoSize = True
        end>
    end
    inherited Panel: TPanel
      Top = 354
      Width = 629
      inherited ButtonFull: TButton
        Left = 559
      end
      inherited EditMask: TEdit
        Width = 481
      end
    end
  end
  object ButtonClose: TButton
    Left = 557
    Top = 497
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 2
    OnClick = ButtonCloseClick
  end
end
