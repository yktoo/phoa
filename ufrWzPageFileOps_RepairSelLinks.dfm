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
    HintMode = hmTooltip
    Images = fMain.ilActionsSmall
    ParentBackground = False
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
  object pBottom: TPanel
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
    object cbDeleteUnmatched: TCheckBox
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
  object dtlsMain: TDTLanguageSwitcher
    Language = 1033
    Left = 8
    Top = 28
    LangData = {
      1E006672577A5061676546696C654F70735F52657061697253656C4C696E6B73
      020000000B0048656C704B6579776F7264020000000904000019040000040048
      696E7402000000090400001904000000000000040000001100636244656C6574
      65556E6D61746368656403000000070043617074696F6E0200000009045A0026
      44656C65746520706963747572657320686176696E67206E6F206D6174636869
      6E672066696C6520286F6E6C7920616666656374732070696374757265732077
      697468206E6F2076616C69642066696C65206C696E6B6564291904680026D3E4
      E0EBE8F2FC20E8E7EEE1F0E0E6E5EDE8FF2C20E4EBFF20EAEEF2EEF0FBF520ED
      E5F220EFEEE4F5EEE4FFF9E5E3EE20F4E0E9EBE02028F2EEEBFCEAEE20E4EBFF
      20E8E7EEE1F0E0E6E5EDE8E920E1E5E720F1F3F9E5F1F2E2F3FEF9E5E3EE20F4
      E0E9EBE0290B0048656C704B6579776F72640200000009040000190400000400
      48696E740200000009040000190400000000000000000000080064746C734D61
      696E000000000000000000000000070070426F74746F6D030000000700436170
      74696F6E0200000009040000190400000B0048656C704B6579776F7264020000
      000904000019040000040048696E740200000009040000190400000000000000
      000000060074764D61696E030000001000436C6970626F617264466F726D6174
      730200000009040000190400000B0048656C704B6579776F7264020000000904
      000019040000040048696E740200000009040000190400000000000000000000}
  end
end
