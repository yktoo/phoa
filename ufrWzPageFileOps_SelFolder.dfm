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
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
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
  object dtlsMain: TDTLanguageSwitcher
    Language = 1046
    Left = 4
    Top = 4
    LangData = {
      19006672577A5061676546696C654F70735F53656C466F6C646572020000000B
      0048656C704B6579776F72640300000009040000190400000704000004004869
      6E740300000009040000190400000704000000000000050000000D0062437265
      617465466F6C64657203000000070043617074696F6E0300000009040E002643
      726561746520666F6C64657219040E0026D1EEE7E4E0F2FC20EFE0EFEAF30704
      11004F72646E6572202665727374656C6C656E0B0048656C704B6579776F7264
      03000000090400001904000007040000040048696E7403000000090400001904
      0000070400000000000000000000080064746C734D61696E0000000000000000
      000000000B0065466F6C64657250617468040000000B0048656C704B6579776F
      726403000000090400001904000007040000040048696E740300000009040000
      19040000070400000700496D654E616D65030000000904000019040000070400
      000400546578740300000009040000190400000704000000000000000000000E
      0070466F6C6465724F7074696F6E7303000000070043617074696F6E03000000
      0904000019040000070400000B0048656C704B6579776F726403000000090400
      001904000007040000040048696E740300000009040000190400000704000000
      0000000000000008007476466F6C646572040000000B0048656C704B6579776F
      726403000000090400001904000007040000040048696E740300000009040000
      19040000070400001400526F6F74466F6C646572437573746F6D506174680300
      00000904000019040000070400001A005368656C6C436F6E746578745375624D
      656E7543617074696F6E03000000090400001904000007040000000000000000
      0000}
  end
  object dklcMain: TDKLanguageController
    Left = 32
    Top = 4
    LangData = {
      19006672577A5061676546696C654F70735F53656C466F6C6465720001050000
      00080064746C734D61696E000008007476466F6C64657200000E0070466F6C64
      65724F7074696F6E7300000D0062437265617465466F6C64657201010000000E
      000000070043617074696F6E000B0065466F6C646572506174680000}
  end
end
