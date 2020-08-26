object DialogGrantedAccess: TDialogGrantedAccess
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Granted Access Rights'
  ClientHeight = 320
  ClientWidth = 240
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 222
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  inline AccessMaskFrame: TAccessMaskFrame
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 234
    Height = 314
    Align = alClient
    Constraints.MinHeight = 200
    Constraints.MinWidth = 180
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    inherited ListViewEx: TListViewEx
      Width = 234
      Height = 285
    end
    inherited Panel: TPanel
      Top = 285
      Width = 234
      inherited ButtonFull: TButton
        Left = 164
      end
      inherited EditMask: TEdit
        Width = 86
      end
    end
  end
end
