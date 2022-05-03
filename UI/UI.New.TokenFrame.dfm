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
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 24
    Header.Height = 24
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoRestrictDrag, hoShowSortGlyphs, hoVisible, hoDisableAnimatedResize, hoHeaderClickAutoSort, hoAutoColumnPopupMenu, hoAutoResizeInclCaption]
    HintMode = hmTooltip
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.ExportMode = emSelected
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
    OnEditing = VSTEditing
    OnNewText = VSTNewText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
end
