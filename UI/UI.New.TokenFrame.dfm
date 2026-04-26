object FrameTokens: TFrameTokens
  Left = 0
  Top = 0
  Width = 669
  Height = 289
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object VST: TDevirtualizedTree
    Left = 0
    Top = 0
    Width = 669
    Height = 289
    Align = alClient
    ClipboardFormats.Strings = (
      'CSV'
      'Plain text'
      'Unicode text')
    DefaultNodeHeight = 24
    EmptyListMessage = ''
    Header.AutoSizeIndex = -1
    Header.MainColumn = -1
    HintMode = hmTooltip
    TabOrder = 0
    OnColumnVisibilityChanged = VSTColumnVisibilityChanged
    OnEditing = VSTEditing
    OnNewText = VSTNewText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
end
