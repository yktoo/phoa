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
    Language = 1046
    Left = 8
    Top = 28
    LangData = {
      1E006672577A5061676546696C654F70735F52657061697253656C4C696E6B73
      020000000B0048656C704B6579776F7264030000000904000019040000070400
      00040048696E7403000000090400001904000007040000000000000400000011
      00636244656C657465556E6D61746368656403000000070043617074696F6E03
      00000009045A002644656C65746520706963747572657320686176696E67206E
      6F206D61746368696E672066696C6520286F6E6C792061666665637473207069
      6374757265732077697468206E6F2076616C69642066696C65206C696E6B6564
      291904680026D3E4E0EBE8F2FC20E8E7EEE1F0E0E6E5EDE8FF2C20E4EBFF20EA
      EEF2EEF0FBF520EDE5F220EFEEE4F5EEE4FFF9E5E3EE20F4E0E9EBE02028F2EE
      EBFCEAEE20E4EBFF20E8E7EEE1F0E0E6E5EDE8E920E1E5E720F1F3F9E5F1F2E2
      F3FEF9E5E3EE20F4E0E9EBE0290704760042696C64657220646965206B65696E
      65207A75676568F6726967652044617465692062657369747A656E20266CF673
      6368656E20286265747269666674206E75722042696C6465722C20646965206D
      6974206B65696E65722067FC6C746967656E204461746569207665726C696E6B
      742073696E64290B0048656C704B6579776F7264030000000904000019040000
      07040000040048696E7403000000090400001904000007040000000000000000
      0000080064746C734D61696E000000000000000000000000070070426F74746F
      6D03000000070043617074696F6E030000000904000019040000070400000B00
      48656C704B6579776F726403000000090400001904000007040000040048696E
      74030000000904000019040000070400000000000000000000060074764D6169
      6E030000001000436C6970626F617264466F726D617473030000000904000019
      040000070400000B0048656C704B6579776F7264030000000904000019040000
      07040000040048696E7403000000090400001904000007040000000000000000
      0000}
  end
  object dklcMain: TDKLanguageController
    Left = 36
    Top = 28
    LangData = {
      1E006672577A5061676546696C654F70735F52657061697253656C4C696E6B73
      000104000000080064746C734D61696E0000060074764D61696E000007007042
      6F74746F6D00001100636244656C657465556E6D61746368656401010000000D
      000000070043617074696F6E00}
  end
end
