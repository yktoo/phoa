inherited frPicProps_Metadata: TfrPicProps_Metadata
  PopupMenu = pmMain
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 576
    Height = 256
    Align = alClient
    Header.AutoSizeIndex = 2
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
    HintMode = hmTooltip
    ParentBackground = False
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = tvMainBeforeCellPaint
    OnChange = tvMainChange
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
        Width = 90
      end
      item
        Position = 1
        Width = 200
      end
      item
        Position = 2
        Width = 282
      end>
    WideDefaultText = ''
  end
  object dkBottom: TTBXDock
    Left = 0
    Top = 256
    Width = 576
    Height = 44
    Position = dpBottom
    object dpDesc: TTBXDockablePanel
      Left = 0
      Top = 0
      MinClientHeight = 16
      MinClientWidth = 16
      Caption = 'Description'
      CaptionRotation = dpcrAlwaysHorz
      Color = clInfoBk
      DockedHeight = 40
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 0
      object lDesc: TTBXLabel
        Left = 0
        Top = 0
        Width = 572
        Height = 22
        Align = alClient
        Margins.Left = 10
        Margins.Right = 10
      end
    end
  end
  object pmMain: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 16
    Top = 164
    object ipmShowDescPanel: TTBXVisibilityToggleItem
      Caption = 'Description panel'
      Control = dpDesc
      Hint = 'Description panel|Show/hide the Description panel'
    end
  end
  object dklcMain: TDKLanguageController
    Left = 68
    Top = 164
    LangData = {
      1300667250696350726F70735F4D657461646174610001060000000800646B42
      6F74746F6D0000060074764D61696E0000060064704465736301010000003500
      0000070043617074696F6E0005006C4465736300000600706D4D61696E000010
      0069706D53686F774465736350616E656C010200000039000000070043617074
      696F6E3A000000040048696E7400}
  end
end
