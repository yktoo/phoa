inherited frWzPageAddFiles_CheckFiles: TfrWzPageAddFiles_CheckFiles
  object tvFiles: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 576
    Height = 260
    Align = alClient
    ChangeDelay = 200
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoHotTrack, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    HintMode = hmTooltip
    Images = ilFiles
    PopupMenu = pmFiles
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnChange = tvFilesChange
    OnChecked = tvFilesChecked
    OnGetText = tvFilesGetText
    OnGetImageIndex = tvFilesGetImageIndex
    OnHeaderClick = tvFilesHeaderClick
    OnInitNode = tvFilesInitNode
    OnKeyAction = tvFilesKeyAction
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
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 1
    object cbShowPreview: TCheckBox
      Left = 352
      Top = 4
      Width = 221
      Height = 17
      Caption = 'Show pre&view'
      TabOrder = 0
      OnClick = cbShowPreviewClick
    end
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 8
    Top = 28
    object aFilesCheckAll: TAction
      Caption = 'Check &all files'
      Hint = 'Set check to all the files in the list'
      ImageIndex = 13
      OnExecute = aaFilesCheckAll
    end
    object aFilesUncheckAll: TAction
      Caption = '&Uncheck all files'
      Hint = 'Remove check from all the files'
      ImageIndex = 14
      OnExecute = aaFilesUncheckAll
    end
    object aFilesInvertChecks: TAction
      Caption = '&Invert file checks'
      Hint = 'Toggle each file'#39's check'
      ImageIndex = 40
      OnExecute = aaFilesInvertChecks
    end
  end
  object pmFiles: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 48
    Top = 28
    object ipmFilesCheckAll: TTBXItem
      Action = aFilesCheckAll
      ShortCut = 16449
    end
    object ipmFilesUncheckAll: TTBXItem
      Action = aFilesUncheckAll
      ShortCut = 16452
    end
    object ipmFilesInvertChecks: TTBXItem
      Action = aFilesInvertChecks
      ShortCut = 16457
    end
  end
  object ilFiles: TImageList
    ShareImages = True
    Left = 84
    Top = 28
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    StoreList.Strings = (
      'tvFiles.Header.Columns*')
    Left = 124
    Top = 28
    LangData = {
      1B006672577A5061676541646446696C65735F436865636B46696C657300010C
      0000000600616C4D61696E00000700747646696C657301040000001700000016
      004865616465722E436F6C756D6E735B305D2E54657874180000001600486561
      6465722E436F6C756D6E735B315D2E546578741900000016004865616465722E
      436F6C756D6E735B325D2E546578741A00000016004865616465722E436F6C75
      6D6E735B335D2E5465787400070070426F74746F6D00000E006146696C657343
      6865636B416C6C01020000000C000000070043617074696F6E0E000000040048
      696E740010006146696C6573556E636865636B416C6C01020000001000000007
      0043617074696F6E12000000040048696E740012006146696C6573496E766572
      74436865636B73010200000014000000070043617074696F6E16000000040048
      696E74000700706D46696C65730000100069706D46696C6573436865636B416C
      6C0000120069706D46696C6573556E636865636B416C6C0000140069706D4669
      6C6573496E76657274436865636B7300000700696C46696C657300000D006362
      53686F775072657669657701010000001B000000070043617074696F6E00}
  end
end
