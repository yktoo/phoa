object frDefSettingEditor: TfrDefSettingEditor
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    HintMode = hmTooltip
    ParentBackground = False
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnAfterCellPaint = tvMainAfterCellPaint
    OnChecked = tvMainChecked
    OnEnter = EmbedControlNotify
    OnExit = EmbedControlNotify
    OnFocusChanged = tvMainFocusChanged
    OnGetCellIsEmpty = tvMainGetCellIsEmpty
    OnGetText = tvMainGetText
    OnPaintText = tvMainPaintText
    OnInitNode = tvMainInitNode
    Columns = <
      item
        Position = 0
        Width = 300
      end
      item
        Position = 1
        Width = 16
      end>
  end
end
