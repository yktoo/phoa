inherited frWzPageFileOps_RepairSelLinks: TfrWzPageFileOps_RepairSelLinks
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 576
    Height = 260
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoHotTrack, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    HintMode = hmTooltip
    Images = fMain.ilActionsSmall
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = tvMainBeforeCellPaint
    OnGetText = tvMainGetText
    OnGetImageIndex = tvMainGetImageIndex
    OnInitNode = tvMainInitNode
    Columns = <
      item
        Color = 16250871
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 200
        WideText = 'File'
      end
      item
        Position = 1
        Width = 152
        WideText = 'Path'
      end
      item
        Alignment = taRightJustify
        Position = 2
        Width = 100
        WideText = 'Size'
      end
      item
        Alignment = taCenter
        Position = 3
        Width = 120
        WideText = 'Date'
      end>
  end
  object pBottom: TTntPanel
    Left = 0
    Top = 260
    Width = 576
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      576
      24)
    object cbDeleteUnmatched: TTntCheckBox
      Left = 4
      Top = 4
      Width = 567
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        '&Delete pictures having no matching file (only affects pictures ' +
        'with no valid file linked)'
      TabOrder = 0
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 12
    Top = 28
    LangData = {
      1E006672577A5061676546696C654F70735F52657061697253656C4C696E6B73
      000103000000060074764D61696E0000070070426F74746F6D00001100636244
      656C657465556E6D61746368656401010000000D000000070043617074696F6E
      00}
  end
end
