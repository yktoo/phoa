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
    Left = 4
    Top = 212
    object ipmShowDescPanel: TTBXVisibilityToggleItem
      Caption = 'Description panel'
      Control = dpDesc
      Hint = 'Description panel|Show/hide the Description panel'
    end
  end
  object dtlsMain: TDTLanguageSwitcher
    Language = 1033
    Left = 32
    Top = 212
    LangData = {
      1300667250696350726F70735F4D65746164617461020000000B0048656C704B
      6579776F72640500000019040000090400000704000016040000220400000400
      48696E7405000000190400000904000007040000160400002204000000000000
      070000000800646B426F74746F6D020000000B0048656C704B6579776F726405
      0000001904000009040000070400001604000022040000040048696E74050000
      0019040000090400000704000016040000220400000000000000000000060064
      704465736303000000070043617074696F6E0500000019040800CEEFE8F1E0ED
      E8E509040B004465736372697074696F6E07040C00426573636872656962756E
      6716040900446573637269E7E36F22040400CEEFE8F10B0048656C704B657977
      6F7264050000001904000009040000070400001604000022040000040048696E
      7405000000190400000904000007040000160400002204000000000000000000
      00080064746C734D61696E000000000000000000000000100069706D53686F77
      4465736350616E656C02000000070043617074696F6E0500000019040F00CFE0
      EDE5EBFC20EEEFE8F1E0EDE8FF090411004465736372697074696F6E2070616E
      656C07040C00426573636872656962756E67160411005061696E656C20646573
      6372697469766F22040C00CFE0EDE5EBFC20EEEFE8F1F3040048696E74050000
      0019042F00CFE0EDE5EBFC20EEEFE8F1E0EDE8FF7CCFEEEAE0E7E0F2FC2FF1EA
      F0FBF2FC20EFE0EDE5EBFC20EEEFE8F1E0EDE8FF090431004465736372697074
      696F6E2070616E656C7C53686F772F6869646520746865204465736372697074
      696F6E2070616E656C07042D00426573636872656962756E677C426573636872
      656962756E6720616E7A656967656E2F617573626C656E64656E160433005061
      696E656C206465736372697469766F7C4D6F73747261722F6F63756C74617220
      7061696E656C206465736372697469766F22042A00CFE0EDE5EBFC20EEEFE8F1
      F37CCFEEEAE0E7E0F2E82FF1F5EEE2E0F2E820EFE0EDE5EBFC20EEEFE8F1F300
      0000000000000005006C4465736303000000070043617074696F6E0500000019
      040000090400000704000016040000220400000B0048656C704B6579776F7264
      050000001904000009040000070400001604000022040000040048696E740500
      0000190400000904000007040000160400002204000000000000000000000600
      706D4D61696E000000000000000000000000060074764D61696E030000001000
      436C6970626F617264466F726D61747305000000190400000904000007040000
      16040000220400000B0048656C704B6579776F72640500000019040000090400
      00070400001604000022040000040048696E7405000000190400000904000007
      04000016040000220400000000000000000000}
  end
end
