inherited frWzPageAddFiles_SelFiles: TfrWzPageAddFiles_SelFiles
  object gbFilter: TTntGroupBox
    Left = 0
    Top = 140
    Width = 576
    Height = 120
    Align = alBottom
    Caption = 'File selection filter'
    TabOrder = 1
    Visible = False
    DesignSize = (
      576
      120)
    object lFileDateFrom: TTntLabel
      Left = 12
      Top = 68
      Width = 88
      Height = 13
      Caption = '&File modified from:'
      FocusControl = eFileDateFrom
    end
    object lFileMasks: TTntLabel
      Left = 12
      Top = 44
      Width = 139
      Height = 13
      Caption = 'File &masks (delimited with '#39';'#39'):'
      FocusControl = cbFileMasks
    end
    object lPresence: TTntLabel
      Left = 12
      Top = 20
      Width = 110
      Height = 13
      Caption = '&Photo album presence:'
      FocusControl = cbPresence
    end
    object lFileDateTo: TTntLabel
      Left = 364
      Top = 68
      Width = 14
      Height = 13
      Caption = '&to:'
      FocusControl = eFileDateTo
    end
    object lFileSizeFrom: TTntLabel
      Left = 12
      Top = 92
      Width = 66
      Height = 13
      Caption = 'File si&ze from:'
      FocusControl = eFileSizeFrom
    end
    object lFileSizeTo: TTntLabel
      Left = 364
      Top = 92
      Width = 14
      Height = 13
      Caption = 't&o:'
      FocusControl = eFileSizeTo
    end
    object cbFileMasks: TTntComboBox
      Left = 184
      Top = 40
      Width = 377
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 1
      Text = '*.*'
      OnChange = PageDataChange
    end
    object cbPresence: TTntComboBox
      Left = 184
      Top = 16
      Width = 377
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 0
      OnChange = PageDataChange
      Items.Strings = (
        'Disregard'
        'Only files which are absent at the photo album'
        'Only files which are present at the photo album')
    end
    object eFileDateFrom: TDateEdit
      Left = 184
      Top = 64
      Width = 89
      Height = 21
      BlanksChar = '_'
      NumGlyphs = 2
      TabOrder = 2
      OnChange = PageDataChange
    end
    object eFileDateTo: TDateEdit
      Left = 388
      Top = 64
      Width = 89
      Height = 21
      BlanksChar = '_'
      NumGlyphs = 2
      TabOrder = 4
      OnChange = PageDataChange
    end
    object eFileTimeFrom: TMaskEdit
      Left = 276
      Top = 64
      Width = 41
      Height = 21
      EditMask = '!99:99;1;_'
      MaxLength = 5
      TabOrder = 3
      Text = '  :  '
      OnChange = PageDataChange
    end
    object eFileTimeTo: TMaskEdit
      Left = 480
      Top = 64
      Width = 41
      Height = 21
      EditMask = '!99:99;1;_'
      MaxLength = 5
      TabOrder = 5
      Text = '  :  '
      OnChange = PageDataChange
    end
    object eFileSizeFrom: TRxSpinEdit
      Left = 184
      Top = 88
      Width = 89
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      MaxValue = 2147483647.000000000000000000
      TabOrder = 6
      OnChange = PageDataChange
    end
    object cbFileSizeFromUnit: TTntComboBox
      Left = 276
      Top = 88
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 7
      OnChange = PageDataChange
    end
    object eFileSizeTo: TRxSpinEdit
      Left = 388
      Top = 88
      Width = 89
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      MaxValue = 2147483647.000000000000000000
      Value = 999999999.000000000000000000
      TabOrder = 8
      OnChange = PageDataChange
    end
    object cbFileSizeToUnit: TTntComboBox
      Left = 480
      Top = 88
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      OnChange = PageDataChange
    end
  end
  object pMain: TTntPanel
    Left = 0
    Top = 260
    Width = 576
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      576
      24)
    object cbRecurseFolders: TTntCheckBox
      Left = 4
      Top = 4
      Width = 221
      Height = 17
      Caption = '&Recurse subfolders'
      TabOrder = 0
      OnClick = PageDataChange
    end
    object bAdvanced: TTntButton
      Left = 452
      Top = 1
      Width = 119
      Height = 23
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnClick = bAdvancedClick
    end
    object cbShowPreview: TTntCheckBox
      Left = 228
      Top = 4
      Width = 221
      Height = 17
      Caption = 'Show pre&view'
      TabOrder = 1
      OnClick = cbShowPreviewClick
    end
  end
  object tvMain: TVirtualExplorerTree
    Left = 0
    Top = 0
    Width = 576
    Height = 140
    Active = False
    Align = alClient
    AnimationDuration = 100
    AutoScrollDelay = 200
    AutoScrollInterval = 100
    ChangeDelay = 200
    ColumnDetails = cdUser
    ColumnMenuItemCount = 8
    DefaultNodeHeight = 17
    DragHeight = 250
    DragWidth = 150
    DrawSelectionMode = smBlendedRectangle
    FileObjects = [foFolders, foNonFolders]
    FileSizeFormat = fsfExplorer
    FileSort = fsFileExtension
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    Header.SortColumn = 0
    HintMode = hmHint
    ParentColor = False
    RootFolder = rfDesktop
    TabOrder = 0
    TabStop = True
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toMultiSelect, toSiblingSelectConstraint]
    TreeOptions.VETFolderOptions = [toFoldersExpandable, toForceHideRecycleBin]
    TreeOptions.VETShellOptions = [toRightAlignSizeColumn, toContextMenus, toShellHints]
    TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
    TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toExecuteOnDblClk]
    TreeOptions.VETImageOptions = [toImages, toThreadedImages]
    VETColors.FileTextColor = clNavy
    OnChange = tvMainChange
    OnEnumFolder = tvMainEnumFolder
    Columns = <
      item
        Color = 16250871
        Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 354
        ColumnDetails = cdFileName
        WideText = 'Name'
      end
      item
        Alignment = taRightJustify
        Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 80
        ColumnDetails = cdSize
        WideText = 'Size'
      end
      item
        Alignment = taRightJustify
        Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 2
        Width = 138
        ColumnDetails = cdModified
        WideText = 'Modified'
      end>
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'cbFileMasks.Text'
      'eFileTimeFrom.*'
      'eFileTimeTo.*')
    StoreList.Strings = (
      'tvMain.Header.Columns*')
    Left = 8
    Top = 24
    LangData = {
      19006672577A5061676541646446696C65735F53656C46696C65730001160000
      000800676246696C746572010100000007000000070043617074696F6E000D00
      6C46696C654461746546726F6D01010000000A000000070043617074696F6E00
      0A006C46696C654D61736B7301010000000D000000070043617074696F6E0009
      006C50726573656E6365010100000010000000070043617074696F6E000B006C
      46696C6544617465546F010100000013000000070043617074696F6E000B0063
      6246696C654D61736B7300000A00636250726573656E636501010000001D0000
      0005004974656D73000D006546696C654461746546726F6D00000B006546696C
      6544617465546F00000D006546696C6554696D6546726F6D00000B006546696C
      6554696D65546F00000500704D61696E00001000636252656375727365466F6C
      64657273010100000037000000070043617074696F6E00090062416476616E63
      65640000060074764D61696E01030000003800000016004865616465722E436F
      6C756D6E735B305D2E546578743900000016004865616465722E436F6C756D6E
      735B315D2E546578743A00000016004865616465722E436F6C756D6E735B325D
      2E54657874000D00636253686F775072657669657701010000003B0000000700
      43617074696F6E000D006C46696C6553697A6546726F6D01010000003C000000
      070043617074696F6E000D006546696C6553697A6546726F6D00001200636246
      696C6553697A6546726F6D556E697400000B006C46696C6553697A65546F0101
      0000003F000000070043617074696F6E000B006546696C6553697A65546F0000
      1000636246696C6553697A65546F556E69740000}
  end
end
