inherited frWzPageFileOps_SelFolder: TfrWzPageFileOps_SelFolder
  object tvFolder: TVirtualExplorerTree
    Left = 0
    Top = 0
    Width = 576
    Height = 254
    Active = False
    Align = alClient
    AnimationDuration = 100
    AutoScrollDelay = 200
    AutoScrollInterval = 100
    ChangeDelay = 50
    ColumnDetails = cdUser
    ColumnMenuItemCount = 8
    DefaultNodeHeight = 17
    DragHeight = 250
    DragWidth = 150
    DrawSelectionMode = smBlendedRectangle
    FileSizeFormat = fsfExplorer
    FileSort = fsFileExtension
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Header.ParentFont = True
    HintMode = hmHint
    ParentColor = False
    RootFolder = rfDesktop
    TabOrder = 0
    TabStop = True
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.VETFolderOptions = [toFoldersExpandable, toForceHideRecycleBin]
    TreeOptions.VETShellOptions = [toRightAlignSizeColumn, toContextMenus, toShellHints]
    TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
    TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toExecuteOnDblClk]
    TreeOptions.VETImageOptions = [toImages, toThreadedImages]
    OnChange = tvFolderChange
    OnEdited = tvFolderEdited
    Columns = <>
  end
  object pFolderOptions: TPanel
    Left = 0
    Top = 254
    Width = 576
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      576
      30)
    object bCreateFolder: TButton
      Left = 470
      Top = 4
      Width = 102
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Create folder'
      TabOrder = 0
      OnClick = bCreateFolderClick
    end
    object eFolderPath: TEdit
      Left = 4
      Top = 4
      Width = 463
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = PageDataChange
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      'Font.Name'
      'SecondaryShortCuts'
      'tvFolder.Header.Font.Name')
    Left = 8
    Top = 12
    LangData = {
      19006672577A5061676546696C654F70735F53656C466F6C6465720001040000
      0008007476466F6C64657200000E0070466F6C6465724F7074696F6E7300000D
      0062437265617465466F6C64657201010000000E000000070043617074696F6E
      000B0065466F6C646572506174680000}
  end
end
