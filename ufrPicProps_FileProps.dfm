inherited frPicProps_FileProps: TfrPicProps_FileProps
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 576
    Height = 300
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
    ParentBackground = False
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnBeforeCellPaint = tvMainBeforeCellPaint
    OnContextPopup = tvMainContextPopup
    OnFreeNode = tvMainFreeNode
    OnGetText = tvMainGetText
    OnPaintText = tvMainPaintText
    OnGetImageIndex = tvMainGetImageIndex
    OnInitNode = tvMainInitNode
    OnShortenString = tvMainShortenString
    Columns = <
      item
        Color = 16250871
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 200
      end
      item
        Position = 1
        Width = 372
      end>
    WideDefaultText = ''
  end
end
