object DialogPickUser: TDialogPickUser
  Left = 0
  Top = 0
  Anchors = [akTop]
  BorderIcons = [biSystemMenu]
  Caption = 'Choose user or group'
  ClientHeight = 187
  ClientWidth = 294
  Color = clBtnFace
  Constraints.MinHeight = 225
  Constraints.MinWidth = 310
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    294
    187)
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBoxSID: TComboBox
    Left = 8
    Top = 8
    Width = 218
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ComboBoxSIDChange
  end
  object ButtonFilter: TButton
    Left = 232
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Filter suggestions'
    Anchors = [akTop, akRight]
    ImageIndex = 3
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 213
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 156
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ButtonPick: TButton
    Left = 263
    Top = 6
    Width = 25
    Height = 25
    Hint = 'Use default user selection dialog'
    Anchors = [akTop, akRight]
    ImageIndex = 2
    ImageMargins.Left = 2
    ImageMargins.Top = 1
    Images = FormMain.SmallIcons
    TabOrder = 2
    OnClick = ButtonPickClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 35
    Width = 280
    Height = 115
    ActivePage = TabGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 3
    OnChange = PageControlChange
    object TabGeneral: TTabSheet
      Caption = 'General attributes'
      object CheckBoxDenyOnly: TCheckBox
        Left = 152
        Top = 32
        Width = 120
        Height = 17
        Caption = 'Use for deny only'
        TabOrder = 4
      end
      object CheckBoxEnabled: TCheckBox
        Left = 16
        Top = 9
        Width = 130
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object CheckBoxEnabledByDafault: TCheckBox
        Left = 16
        Top = 32
        Width = 130
        Height = 17
        Caption = 'Enabled by default'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object CheckBoxMandatory: TCheckBox
        Left = 152
        Top = 9
        Width = 120
        Height = 17
        Caption = 'Mandatory'
        TabOrder = 3
      end
      object CheckBoxResource: TCheckBox
        Left = 152
        Top = 55
        Width = 120
        Height = 17
        Caption = 'Resource'
        TabOrder = 5
      end
      object CheckBoxOwner: TCheckBox
        Left = 16
        Top = 55
        Width = 130
        Height = 17
        Caption = 'Owner'
        TabOrder = 2
      end
    end
    object TabLogon: TTabSheet
      Caption = 'Logon ID'
      ImageIndex = 1
      DesignSize = (
        272
        87)
      object CheckBoxLogon: TCheckBox
        Left = 16
        Top = 9
        Width = 150
        Height = 17
        Caption = 'Logon ID'
        TabOrder = 0
      end
      object ComboBoxLogonId: TComboBox
        Left = 16
        Top = 40
        Width = 240
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'Choose a logon session or insert it manually'
        OnChange = ComboBoxLogonIdChange
      end
    end
    object TabIntegrity: TTabSheet
      Caption = 'Integrity'
      ImageIndex = 2
      DesignSize = (
        272
        87)
      object CheckBoxIntegrity: TCheckBox
        Left = 16
        Top = 9
        Width = 150
        Height = 17
        Caption = 'Integrity'
        TabOrder = 0
      end
      object CheckBoxIntegrityEnabled: TCheckBox
        Left = 16
        Top = 32
        Width = 150
        Height = 17
        Caption = 'Integrity Enabled'
        TabOrder = 1
      end
      object ComboBoxIntegrity: TComboBox
        Left = 16
        Top = 55
        Width = 240
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'Select an integrity level or enter it manually'
      end
    end
  end
end
