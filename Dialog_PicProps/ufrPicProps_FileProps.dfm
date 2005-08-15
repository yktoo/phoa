inherited frPicProps_FileProps: TfrPicProps_FileProps
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 22
    Width = 576
    Height = 278
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
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
    OnResetNode = tvMainFreeNode
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
  object tbMain: TTBXToolbar
    Left = 0
    Top = 0
    Width = 576
    Height = 22
    Align = alTop
    ChevronHint = 'More buttons|'
    Images = fMain.ilActionsSmall
    TabOrder = 1
    object bChangeFile: TTBXItem
      Action = aChangeFile
      DisplayMode = nbdmImageAndText
    end
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 8
    Top = 32
    object aChangeFile: TAction
      Caption = '&Change file...'
      Hint = 'Change file...|Change file for selected picture'
      ImageIndex = 8
      ShortCut = 113
      OnExecute = aaChangeFile
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 36
    Top = 32
    LangData = {
      1400667250696350726F70735F46696C6550726F707300010500000006007476
      4D61696E0000060074624D61696E0101000000030000000B0043686576726F6E
      48696E74000B00624368616E676546696C6500000600616C4D61696E00000B00
      614368616E676546696C65010200000001000000070043617074696F6E020000
      00040048696E7400}
  end
end
