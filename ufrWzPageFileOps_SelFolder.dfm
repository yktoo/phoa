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
    Language = 1033
    Left = 4
    Top = 4
    LangData = {
      19006672577A5061676546696C654F70735F53656C466F6C646572020000000B
      0048656C704B6579776F7264020000000904000019040000040048696E740200
      0000090400001904000000000000050000000D0062437265617465466F6C6465
      7203000000070043617074696F6E0200000009040E002643726561746520666F
      6C64657219040E0026D1EEE7E4E0F2FC20EFE0EFEAF30B0048656C704B657977
      6F7264020000000904000019040000040048696E740200000009040000190400
      000000000000000000080064746C734D61696E0000000000000000000000000B
      0065466F6C64657250617468040000000B0048656C704B6579776F7264020000
      000904000019040000040048696E740200000009040000190400000700496D65
      4E616D6502000000090400001904000004005465787402000000090400001904
      000000000000000000000E0070466F6C6465724F7074696F6E73030000000700
      43617074696F6E0200000009040000190400000B0048656C704B6579776F7264
      020000000904000019040000040048696E740200000009040000190400000000
      00000000000008007476466F6C646572040000000B0048656C704B6579776F72
      64020000000904000019040000040048696E7402000000090400001904000014
      00526F6F74466F6C646572437573746F6D506174680200000009040000190400
      001A005368656C6C436F6E746578745375624D656E7543617074696F6E020000
      0009040000190400000000000000000000}
  end
end
