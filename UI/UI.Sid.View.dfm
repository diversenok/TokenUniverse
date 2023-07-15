object DialogSidView: TDialogSidView
  Left = 0
  Top = 0
  Caption = 'SID Information'
  ClientHeight = 371
  ClientWidth = 387
  Color = clBtnFace
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
  object Pages: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 381
    Height = 335
    Margins.Bottom = 33
    ActivePage = TabSid
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object TabSid: TTabSheet
      Caption = 'General'
      object LinkLabelDomain: TLinkLabel
        Left = 97
        Top = 103
        Width = 266
        Height = 17
        Hint = 'View the domain SID'
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '-'
        TabOrder = 0
        OnLinkClick = LinkLabelDomainLinkClick
      end
      object LabelSid: TStaticText
        Left = 11
        Top = 34
        Width = 25
        Height = 17
        Caption = 'SID:'
        TabOrder = 1
      end
      object LabelType: TStaticText
        Left = 11
        Top = 57
        Width = 52
        Height = 17
        Caption = 'SID Type:'
        TabOrder = 2
      end
      object LabelFullName: TStaticText
        Left = 11
        Top = 11
        Width = 56
        Height = 17
        Caption = 'Full name: '
        TabOrder = 3
      end
      object EditFullName: TEdit
        Left = 97
        Top = 11
        Width = 266
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        ReadOnly = True
        TabOrder = 4
        Text = '-'
      end
      object EditSID: TEdit
        Left = 97
        Top = 34
        Width = 266
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        ReadOnly = True
        TabOrder = 5
        Text = '-'
      end
      object EditType: TEdit
        Left = 97
        Top = 57
        Width = 266
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        ReadOnly = True
        TabOrder = 6
        Text = '-'
      end
      object LabelSubAuthrities: TStaticText
        Left = 11
        Top = 80
        Width = 80
        Height = 17
        Caption = 'Sub authorities:'
        TabOrder = 7
      end
      object EditSubAuthorities: TEdit
        Left = 97
        Top = 80
        Width = 159
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        ReadOnly = True
        TabOrder = 8
        Text = '-'
      end
      object LinkLabelMinusOne: TLinkLabel
        Left = 279
        Top = 80
        Width = 81
        Height = 17
        Hint = 'View the parent SID'
        Anchors = [akTop, akRight]
        Caption = '<a>-1 sub authority</a>'
        TabOrder = 9
        OnLinkClick = LinkLabelMinusOneLinkClick
      end
      object StaticTextDomain: TStaticText
        Left = 11
        Top = 103
        Width = 43
        Height = 17
        Caption = 'Domain:'
        TabOrder = 10
      end
    end
    object TabDomain: TTabSheet
      Caption = 'Domain'
      ImageIndex = 1
    end
    object TabGroup: TTabSheet
      Caption = 'Group'
      ImageIndex = 2
    end
    object TabAlias: TTabSheet
      Caption = 'Alias'
      ImageIndex = 3
    end
    object TabUser: TTabSheet
      Caption = 'User'
      ImageIndex = 4
    end
    object TabLsaPrivileges: TTabSheet
      Caption = 'Privileges'
      ImageIndex = 5
      inline FrameLsaPrivileges: TFrameLsaPrivileges
        Left = 0
        Top = 0
        Width = 373
        Height = 289
        Align = alClient
        TabOrder = 0
        inherited LabelStatus: TLabel
          Top = 266
          Width = 283
        end
        inherited ButtonApply: TButton
          Top = 261
        end
        inherited PrivilegesFrame: TFramePrivileges
          Width = 367
          Height = 253
          inherited VST: TDevirtualizedTree
            Width = 367
            Height = 253
            PopupMenuEx = FrameLsaPrivileges.PopupMenu
          end
        end
      end
    end
    object TabLsaRights: TTabSheet
      Caption = 'Logon rights'
      ImageIndex = 6
      object LabelStatus: TLabel
        Left = 87
        Top = 266
        Width = 283
        Height = 13
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        EllipsisPosition = epEndEllipsis
      end
      object ButtonApply: TButton
        Left = 3
        Top = 261
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Apply'
        TabOrder = 0
        OnClick = ButtonApplyClick
      end
      inline LogonMaskFrame: TBitsFrame
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 367
        Height = 253
        Margins.Bottom = 33
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        inherited Tree: TDevirtualizedTree
          Width = 367
          Height = 225
          Columns = <
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
              Position = 0
              Text = 'Name'
              Width = 363
            end>
        end
        inherited BottomPanel: TPanel
          Top = 225
          Width = 367
          inherited tbxValue: TEdit
            Width = 221
          end
          inherited btnAll: TButton
            Left = 297
          end
        end
      end
    end
    object TabLsaAudit: TTabSheet
      Caption = 'Per-user audit'
      ImageIndex = 7
      inline FrameLsaAudit: TFrameAudit
        Left = 0
        Top = 0
        Width = 373
        Height = 289
        Align = alClient
        TabOrder = 0
        inherited LabelStatus: TLabel
          Top = 267
          Width = 264
        end
        inherited ListView: TListViewEx
          Width = 367
          Height = 257
          Columns = <
            item
              Width = 120
            end
            item
              Alignment = taCenter
              Caption = 'Succ Inc'
              Width = 55
            end
            item
              Alignment = taCenter
              Caption = 'Succ Exc'
              Width = 55
            end
            item
              Alignment = taCenter
              Caption = 'Fail Inc'
              Width = 55
            end
            item
              Alignment = taCenter
              Caption = 'Fail Exc'
              Width = 55
            end>
        end
        inherited ButtonApply: TButton
          Left = 273
          Top = 262
        end
      end
    end
    object TabLsaQuotas: TTabSheet
      Caption = 'Quotas'
      ImageIndex = 8
    end
  end
  object ButtonClose: TButton
    Left = 309
    Top = 342
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 8
    TabOrder = 1
    OnClick = ButtonCloseClick
  end
end
