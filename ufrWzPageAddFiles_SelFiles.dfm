inherited frWzPageAddFiles_SelFiles: TfrWzPageAddFiles_SelFiles
  object gbFilter: TGroupBox
    Left = 0
    Top = 166
    Width = 576
    Height = 94
    Align = alBottom
    Caption = 'File selection filter'
    TabOrder = 1
    Visible = False
    DesignSize = (
      576
      94)
    object lFileDateFrom: TLabel
      Left = 12
      Top = 68
      Width = 88
      Height = 13
      Caption = '&File modified from:'
      FocusControl = eFileDateFrom
    end
    object lFileMasks: TLabel
      Left = 12
      Top = 44
      Width = 139
      Height = 13
      Caption = 'File &masks (delimited with '#39';'#39'):'
      FocusControl = cbFileMasks
    end
    object lPresence: TLabel
      Left = 12
      Top = 20
      Width = 110
      Height = 13
      Caption = '&Photo album presence:'
      FocusControl = cbPresence
    end
    object lFileDateTo: TLabel
      Left = 324
      Top = 68
      Width = 14
      Height = 13
      Caption = '&to:'
      FocusControl = eFileDateTo
    end
    object cbFileMasks: TComboBox
      Left = 184
      Top = 40
      Width = 369
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 1
      Text = '*.*'
      OnChange = PageDataChange
    end
    object cbPresence: TComboBox
      Left = 184
      Top = 16
      Width = 369
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
      Left = 348
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
      Text = '  *  '
      OnChange = PageDataChange
    end
    object eFileTimeTo: TMaskEdit
      Left = 440
      Top = 64
      Width = 41
      Height = 21
      EditMask = '!99:99;1;_'
      MaxLength = 5
      TabOrder = 5
      Text = '  *  '
      OnChange = PageDataChange
    end
  end
  object pMain: TPanel
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
    object cbRecurseFolders: TCheckBox
      Left = 4
      Top = 4
      Width = 221
      Height = 17
      Caption = '&Recurse subfolders'
      TabOrder = 0
      OnClick = PageDataChange
    end
    object bAdvanced: TButton
      Left = 452
      Top = 1
      Width = 119
      Height = 23
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnClick = bAdvancedClick
    end
    object cbShowPreview: TCheckBox
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
    Height = 166
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
      19006672577A5061676541646446696C65735F53656C46696C65730001100000
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
      43617074696F6E00}
  end
end
