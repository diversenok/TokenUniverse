object DialogSidView: TDialogSidView
  Left = 0
  Top = 0
  Caption = 'SID Information'
  ClientHeight = 371
  ClientWidth = 364
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
    Width = 358
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
        Width = 243
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
        Width = 243
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
        Width = 243
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
        Width = 243
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
        Width = 136
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        ReadOnly = True
        TabOrder = 8
        Text = '-'
      end
      object LinkLabelMinusOne: TLinkLabel
        Left = 256
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
    end
    object TabLsaRights: TTabSheet
      Caption = 'Logon rights'
      ImageIndex = 6
      inline FrameLsaRights: TFrameLsaRights
        Left = 0
        Top = 0
        Width = 350
        Height = 289
        Align = alClient
        TabOrder = 0
        inherited LabelStatus: TLabel
          Top = 266
          Width = 260
        end
        inherited ButtonApply: TButton
          Top = 261
        end
        inherited ListView: TListViewEx
          Width = 344
          Height = 255
        end
      end
    end
    object TabLsaAudit: TTabSheet
      Caption = 'Per-user audit'
      ImageIndex = 7
    end
    object TabLsaQuotas: TTabSheet
      Caption = 'Quotas'
      ImageIndex = 8
    end
  end
  object ButtonClose: TButton
    Left = 286
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
