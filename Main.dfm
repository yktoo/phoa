object fMain: TfMain
  Left = 401
  Top = 294
  AutoScroll = False
  Caption = 'PhoA'
  ClientHeight = 385
  ClientWidth = 615
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object dkTop: TTBXDock
    Left = 0
    Top = 0
    Width = 615
    Height = 26
    PopupMenu = pmView
    object tbMain: TTBXToolbar
      Left = 162
      Top = 0
      Caption = 'Toolbar'
      CloseButtonWhenDocked = True
      DockPos = 154
      Images = ilActionsSmall
      SystemFont = False
      TabOrder = 0
      object bNew: TTBXItem
        Action = aNew
      end
      object bOpen: TTBXSubmenuItem
        Action = aOpen
        DropdownCombo = True
        SubMenuImages = ilActionsSmall
        object bOpenMRU: TTBXMRUListItem
          MRUList = mruOpen
        end
      end
      object bSave: TTBXItem
        Action = aSave
      end
      object bSaveAs: TTBXItem
        Action = aSaveAs
      end
      object tbSep1: TTBXSeparatorItem
      end
      object bNewGroup: TTBXItem
        Action = aNewGroup
      end
      object bNewPic: TTBXItem
        Action = aNewPic
      end
      object bEdit: TTBXItem
        Action = aEdit
      end
      object bDelete: TTBXItem
        Action = aDelete
      end
      object tbSep2: TTBXSeparatorItem
      end
      object bUndo: TTBXSubmenuItem
        Action = aUndo
        AlwaysSelectFirst = True
        DropdownCombo = True
        ToolBoxPopup = True
        OnPopup = bUndoPopup
        object ulToolbarUndo: TTBXUndoList
          MaxVisibleItems = 16
          MinWidth = 120
          OnChange = ulToolbarUndoChange
          OnClick = ulToolbarUndoClick
        end
        object tbxlToolbarUndo: TTBXLabelItem
          Margin = 4
        end
      end
      object bCut: TTBXItem
        Action = aCut
      end
      object bCopy: TTBXItem
        Action = aCopy
      end
      object bPaste: TTBXItem
        Action = aPaste
      end
      object tbSep3: TTBXSeparatorItem
      end
      object bFind: TTBXItem
        Action = aFind
      end
      object bView: TTBXItem
        Action = aView
      end
      object tbSep4: TTBXSeparatorItem
      end
      object bSettings: TTBXItem
        Action = aSettings
      end
      object bHelpContents: TTBXItem
        Action = aHelpContents
      end
      object bExit: TTBXItem
        Action = aExit
      end
    end
    object tbMenu: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Menu'
      CloseButton = False
      Images = ilActionsSmall
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      SystemFont = False
      TabOrder = 1
      object smFile: TTBXSubmenuItem
        Caption = '&File'
        object iNew: TTBXItem
          Action = aNew
        end
        object iOpen: TTBXItem
          Action = aOpen
        end
        object iSave: TTBXItem
          Action = aSave
        end
        object iSaveAs: TTBXItem
          Action = aSaveAs
        end
        object iFileSep1: TTBXSeparatorItem
        end
        object iIniSaveSettings: TTBXItem
          Action = aIniSaveSettings
        end
        object iIniLoadSettings: TTBXItem
          Action = aIniLoadSettings
        end
        object iFileSep2: TTBXSeparatorItem
        end
        object iExit: TTBXItem
          Action = aExit
        end
        object iFileSep3: TTBXSeparatorItem
        end
        object smFileMRU: TTBXMRUListItem
          MRUList = mruOpen
        end
      end
      object smEdit: TTBXSubmenuItem
        Caption = '&Edit'
        object iUndo: TTBXItem
          Action = aUndo
        end
        object smUndoHistory: TTBXSubmenuItem
          Caption = 'U&ndo history'
          Hint = 
            'Displays list of changes made and allows to undo a number of act' +
            'ions'
          ImageIndex = 24
          LinkSubitems = bUndo
          ToolBoxPopup = True
        end
        object iEditSep1: TTBXSeparatorItem
        end
        object iCut: TTBXItem
          Action = aCut
        end
        object iCopy: TTBXItem
          Action = aCopy
        end
        object iPaste: TTBXItem
          Action = aPaste
        end
        object iEditSep2: TTBXSeparatorItem
        end
        object iNewGroup: TTBXItem
          Action = aNewGroup
        end
        object iNewPic: TTBXItem
          Action = aNewPic
        end
        object iDelete: TTBXItem
          Action = aDelete
        end
        object iEditSep3: TTBXSeparatorItem
        end
        object iEdit: TTBXItem
          Action = aEdit
        end
        object iEditSep4: TTBXSeparatorItem
        end
        object iSortPics: TTBXItem
          Action = aSortPics
        end
        object iSelectAll: TTBXItem
          Action = aSelectAll
        end
        object iSelectNone: TTBXItem
          Action = aSelectNone
        end
        object iEditSep5: TTBXSeparatorItem
        end
        object iView: TTBXItem
          Action = aView
        end
        object iViewSlideShow: TTBXItem
          Action = aViewSlideShow
        end
      end
      object smView: TTBXSubmenuItem
        Caption = '&View'
        object iToggleToolbar: TTBXVisibilityToggleItem
          Caption = '&Toolbar'
          Control = tbMain
          Hint = 'Toolbar|Show or hide the Toolbar'
        end
        object iToggleStatusbar: TTBXVisibilityToggleItem
          Caption = '&Status bar'
          Control = sbarMain
          Hint = 'Status bar|Show or hide the Status bar'
        end
        object iFlatMode: TTBXItem
          Action = aFlatMode
        end
        object iRemoveSearchResults: TTBXItem
          Action = aRemoveSearchResults
        end
        object tbViewSep1: TTBXSeparatorItem
        end
        object gismViewViews: TTBGroupItem
          LinkSubitems = gipmPhoaView
        end
      end
      object smTools: TTBXSubmenuItem
        Caption = '&Tools'
        object iSettings: TTBXItem
          Action = aSettings
        end
        object bStats: TTBXItem
          Action = aStats
        end
        object iPicOps: TTBXItem
          Action = aPicOps
        end
        object iFileOperations: TTBXItem
          Action = aFileOperations
        end
        object iToolsSep1: TTBXSeparatorItem
        end
        object iFind: TTBXItem
          Action = aFind
        end
        object iToolsSep2: TTBXSeparatorItem
        end
        object giTools_ToolsMenu: TTBGroupItem
        end
      end
      object smHelp: TTBXSubmenuItem
        Caption = '&Help'
        object iAbout: TTBXItem
          Action = aAbout
        end
        object iHelpContents: TTBXItem
          Action = aHelpContents
        end
        object iHelpFAQ: TTBXItem
          Action = aHelpFAQ
        end
        object tbSepHelpWebsite: TTBXSeparatorItem
        end
        object smHelpInternet: TTBXSubmenuItem
          Caption = '&Internet'
          object iHelpCheckUpdates: TTBXItem
            Action = aHelpCheckUpdates
          end
          object iHelpProductWebsite: TTBXItem
            Action = aHelpProductWebsite
          end
          object iHelpSupport: TTBXItem
            Action = aHelpSupport
          end
          object iHelpVendorWebsite: TTBXItem
            Action = aHelpVendorWebsite
          end
        end
      end
    end
  end
  object dkBottom: TTBXDock
    Left = 0
    Top = 357
    Width = 615
    Height = 9
    PopupMenu = pmView
    Position = dpBottom
  end
  object dkLeft: TTBXDock
    Left = 0
    Top = 26
    Width = 200
    Height = 331
    FixAlign = True
    PopupMenu = pmView
    Position = dpLeft
    object dpGroups: TTBXDockablePanel
      Left = 0
      Top = 0
      Caption = 'Picture Groups'
      CloseButton = False
      CloseButtonWhenDocked = False
      DockedWidth = 196
      DockPos = 4
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 0
      object tvGroups: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 196
        Height = 309
        Align = alClient
        ChangeDelay = 50
        DefaultNodeHeight = 16
        EditDelay = 500
        Header.AutoSizeIndex = -1
        Header.Font.Charset = RUSSIAN_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        Header.ParentFont = True
        Images = ilActionsSmall
        PopupMenu = pmGroups
        TabOrder = 0
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toRightClickSelect]
        TreeOptions.StringOptions = [toShowStaticText, toAutoAcceptEditChange]
        OnBeforeItemErase = tvGroupsBeforeItemErase
        OnChange = tvGroupsChange
        OnChecked = tvGroupsChecked
        OnCollapsed = SetGroupExpanded
        OnCollapsing = tvGroupsCollapsing
        OnCreateEditor = tvGroupsCreateEditor
        OnDragAllowed = tvGroupsDragAllowed
        OnDragOver = tvGroupsDragOver
        OnDragDrop = tvGroupsDragDrop
        OnEditCancelled = tvGroupsEditCancelled
        OnEdited = tvGroupsEdited
        OnEditing = tvGroupsEditing
        OnExpanded = SetGroupExpanded
        OnFreeNode = tvGroupsFreeNode
        OnGetText = tvGroupsGetText
        OnPaintText = tvGroupsPaintText
        OnGetImageIndex = tvGroupsGetImageIndex
        OnGetHint = tvGroupsGetHint
        OnInitNode = tvGroupsInitNode
        OnNewText = tvGroupsNewText
        Columns = <>
        WideDefaultText = ''
      end
    end
  end
  object dkRight: TTBXDock
    Left = 606
    Top = 26
    Width = 9
    Height = 331
    PopupMenu = pmView
    Position = dpRight
  end
  object sbarMain: TTBXStatusBar
    Left = 0
    Top = 366
    Width = 615
    Height = 19
    Images = ilActionsSmall
    Panels = <
      item
        StretchPriority = 1
        Tag = 0
      end
      item
        Size = 130
        Tag = 0
      end
      item
        Size = 130
        Tag = 0
      end>
    PopupMenu = pmView
    UseSystemFont = False
  end
  object alMain: TActionList
    Images = ilActionsSmall
    Left = 444
    Top = 44
    object aFlatMode: TAction
      Category = 'View'
      Caption = '&Flat mode'
      Hint = 
        'Flat mode|Show thumbnails from the selected group and all its su' +
        'bgroups'
      OnExecute = aaFlatMode
    end
    object aRemoveSearchResults: TAction
      Category = 'View'
      Caption = 'Remove Searc&h Results'
      Hint = 
        'Remove Search results|Remove the Search Results folder from the ' +
        'group tree'
      ImageIndex = 65
      ShortCut = 24690
      OnExecute = aaRemoveSearchResults
    end
    object aPhoaView_New: TAction
      Category = 'View'
      Caption = '&New view...'
      Hint = 'New view...|Create new photo album view'
      ImageIndex = 38
      ShortCut = 41005
      OnExecute = aaPhoaView_New
    end
    object aNew: TAction
      Category = 'File'
      Caption = '&New'
      Hint = 'New|Create new photo album'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = aaNew
    end
    object aOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open...|Open an existing photo album file'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = aaOpen
    end
    object aPhoaView_Delete: TAction
      Category = 'View'
      Caption = '&Delete view'
      Hint = 'Delete view|Delete current photo album view'
      ImageIndex = 7
      ShortCut = 41006
      OnExecute = aaPhoaView_Delete
    end
    object aSave: TAction
      Category = 'File'
      Caption = '&Save'
      Hint = 'Save|Save current photo album to file'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = aaSave
    end
    object aSaveAs: TAction
      Category = 'File'
      Caption = 'Save &as...'
      Hint = 'Save as...|Save current photo album into a different file'
      ImageIndex = 3
      ShortCut = 123
      OnExecute = aaSaveAs
    end
    object aIniSaveSettings: TAction
      Category = 'File'
      Caption = 'Sa&ve current settings to file...'
      Hint = 
        'Save current settings to file...|Save current program settings t' +
        'o an ini-file'
      ImageIndex = 63
      ShortCut = 24659
      OnExecute = aaIniSaveSettings
    end
    object aIniLoadSettings: TAction
      Category = 'File'
      Caption = '&Load settings from file...'
      Hint = 
        'Load settings from file...|Load program settings from an ini-fil' +
        'e'
      ImageIndex = 64
      ShortCut = 24655
      OnExecute = aaIniLoadSettings
    end
    object aExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Exits the program'
      ImageIndex = 4
      OnExecute = aaExit
    end
    object aUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Enabled = False
      Hint = 'Undo|Undo the last change'
      ImageIndex = 23
      ShortCut = 16474
      SecondaryShortCuts.Strings = (
        'Alt+BkSp')
      OnExecute = aaUndo
    end
    object aNewGroup: TAction
      Category = 'Edit'
      Caption = 'Add &group'
      Hint = 'Add group|Add new picture group'
      ImageIndex = 5
      ShortCut = 32813
      OnExecute = aaNewGroup
    end
    object aNewPic: TAction
      Category = 'Edit'
      Caption = 'Add &pictures...'
      Hint = 
        'Add pictures...|Open the Add Pictures wizard allowing you to add' +
        ' picture files to the group currently selected'
      ImageIndex = 6
      ShortCut = 45
      OnExecute = aaNewPic
    end
    object aDelete: TAction
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Delete the selected element(s) '
      ImageIndex = 7
      ShortCut = 46
      SecondaryShortCuts.Strings = (
        'Ctrl+Del')
      OnExecute = aaDelete
    end
    object aCut: TAction
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cut selected pictures into the clipboard'
      ImageIndex = 20
      ShortCut = 16472
      SecondaryShortCuts.Strings = (
        'Shift+Del')
      OnExecute = aaCut
    end
    object aCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copy selected pictures to the clipboard'
      ImageIndex = 21
      ShortCut = 16451
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = aaCopy
    end
    object aPaste: TAction
      Category = 'Edit'
      Caption = 'P&aste'
      Hint = 'Paste|Paste picture(s) from the clipboard'
      ImageIndex = 22
      ShortCut = 16470
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      OnExecute = aaPaste
    end
    object aEdit: TAction
      Category = 'Edit'
      Caption = '&Edit...'
      Hint = 'Edit...|Edit selected element(s)'
      ImageIndex = 8
      ShortCut = 32781
      OnExecute = aaEdit
    end
    object aSortPics: TAction
      Category = 'Edit'
      Caption = 'S&ort pictures...'
      Hint = 'Sort pictures...|Sort pictures by given criteria'
      ImageIndex = 16
      ShortCut = 16498
      OnExecute = aaSortPics
    end
    object aSelectAll: TAction
      Category = 'Edit'
      Caption = '&Select all'
      Hint = 'Select all|Select all pictures'
      ImageIndex = 13
      ShortCut = 16449
      OnExecute = aaSelectAll
    end
    object aSelectNone: TAction
      Category = 'Edit'
      Caption = 'Select &none'
      Hint = 'Select none|Select none of the pictures'
      ImageIndex = 14
      ShortCut = 16452
      OnExecute = aaSelectNone
    end
    object aView: TAction
      Category = 'Edit'
      Caption = '&View'
      Hint = 'View|Start view mode from the current picture'
      ImageIndex = 15
      ShortCut = 16397
      OnExecute = aaView
    end
    object aSettings: TAction
      Category = 'Tools'
      Caption = 'Pro&gram settings...'
      Hint = 'Program settings...|View/edit program settings'
      ImageIndex = 12
      ShortCut = 115
      OnExecute = aaSettings
    end
    object aStats: TAction
      Category = 'Tools'
      Caption = '&Statistics...'
      Hint = 'Statistics...|Summary photo album information'
      ImageIndex = 19
      ShortCut = 119
      OnExecute = aaStats
    end
    object aPicOps: TAction
      Category = 'Tools'
      Caption = '&Picture operations...'
      Hint = 
        'Picture operations...|Allows you to copy, move etc. selected pic' +
        'tures'
      ImageIndex = 18
      ShortCut = 16497
      OnExecute = aaPicOps
    end
    object aFileOperations: TAction
      Category = 'Tools'
      Caption = 'File ope&rations...'
      Hint = 
        'File operations...|Open the File Operations Wizard to perform fi' +
        'le copying, moving etc., or to repair broken picture file links'
      ImageIndex = 17
      ShortCut = 16466
      OnExecute = aaFileOperations
    end
    object aFind: TAction
      Category = 'Tools'
      Caption = '&Find...'
      Hint = 'Find...|Find pictures by given criteria'
      ImageIndex = 9
      ShortCut = 114
      OnExecute = aaFind
    end
    object aAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      Hint = 'About...|Version and Copyright information'
      ImageIndex = 10
      ShortCut = 16496
      OnExecute = aaAbout
    end
    object aHelpContents: TAction
      Category = 'Help'
      Caption = '&Help contents'
      Hint = 'Help contents|Display help contents'
      ImageIndex = 11
      OnExecute = aaHelpContents
    end
    object aPhoaView_Edit: TAction
      Category = 'View'
      Caption = '&Edit view...'
      Hint = 'Edit view...|Edit current photo album view'
      ImageIndex = 8
      ShortCut = 40973
      OnExecute = aaPhoaView_Edit
    end
    object aPhoaView_MakeGroup: TAction
      Category = 'View'
      Caption = '&Make a group...'
      Hint = 
        'Make a group...|Transfer current view'#39's folder hierarchy into a ' +
        'selected photo album group'
      ImageIndex = 39
      ShortCut = 41031
      OnExecute = aaPhoaView_MakeGroup
    end
    object aHelpFAQ: TAction
      Category = 'Help'
      Caption = 'FA&Q'
      Hint = 'FAQ|View the PhoA FAQ (Frequently Asked Questions) topic'
      OnExecute = aaHelpFAQ
    end
    object aHelpCheckUpdates: TAction
      Category = 'Help'
      Caption = 'Check for &updates'
      Hint = 'Check for updates|Check for program updates on the web'
      ImageIndex = 43
      OnExecute = aaHelpCheckUpdates
    end
    object aHelpProductWebsite: TAction
      Category = 'Help'
      Caption = 'PhoA ho&me site'
      Hint = 'PhoA home site|Open the program home site in the browser'
      ImageIndex = 47
      ShortCut = 16471
      OnExecute = aaHelpProductWebsite
    end
    object aHelpSupport: TAction
      Category = 'Help'
      Caption = 'Product &support'
      Hint = 'Product support|Open the product support page in the browser'
      OnExecute = aaHelpSupport
    end
    object aHelpVendorWebsite: TAction
      Category = 'Help'
      Caption = 'DK Software &Website'
      Hint = 'DK Software Website|Open the vendor home site in the browser'
      ImageIndex = 73
      OnExecute = aaHelpVendorWebsite
    end
    object aViewSlideShow: TAction
      Category = 'Edit'
      Caption = 'Vie&w slide show'
      Hint = 'View slide show|Start view mode with slide show'
      ImageIndex = 35
      ShortCut = 24589
      OnExecute = aaViewSlideShow
    end
  end
  object pmGroups: TTBXPopupMenu
    Images = ilActionsSmall
    OnPopup = pmGroupsPopup
    Left = 28
    Top = 316
    object ipmGroupsDelete: TTBXItem
      Action = aDelete
    end
    object ipmGroupsEdit: TTBXItem
      Action = aEdit
    end
    object ipmGroupsSep1: TTBXSeparatorItem
    end
    object ipmGroupsNewGroup: TTBXItem
      Action = aNewGroup
    end
    object ipmGroupsNewPic: TTBXItem
      Action = aNewPic
    end
    object ipmGroupsSep2: TTBXSeparatorItem
    end
    object ipmGroupsSortPics: TTBXItem
      Action = aSortPics
    end
    object ipmGroupsStats: TTBXItem
      Action = aStats
    end
    object ipmGroupsPicOps: TTBXItem
      Action = aPicOps
    end
    object ipmGroupsFileOperations: TTBXItem
      Action = aFileOperations
    end
    object ipmGroupsSep3: TTBXSeparatorItem
    end
    object giTools_GroupsMenu: TTBGroupItem
    end
  end
  object pmPics: TTBXPopupMenu
    Images = ilActionsSmall
    OnPopup = pmPicsPopup
    Left = 364
    Top = 104
    object ipmPicsView: TTBXItem
      Action = aView
      Options = [tboDefault]
    end
    object ipmPicsSep1: TTBXSeparatorItem
    end
    object ipmPicsCut: TTBXItem
      Action = aCut
    end
    object ipmPicsCopy: TTBXItem
      Action = aCopy
    end
    object ipmPicsPaste: TTBXItem
      Action = aPaste
    end
    object ipmPicsSep2: TTBXSeparatorItem
    end
    object ipmPicsEdit: TTBXItem
      Action = aEdit
    end
    object ipmPicsDelete: TTBXItem
      Action = aDelete
    end
    object ipmPicsSep3: TTBXSeparatorItem
    end
    object ipmPicsNewPic: TTBXItem
      Action = aNewPic
    end
    object ipmPicsSelectAll: TTBXItem
      Action = aSelectAll
    end
    object ipmPicsSelectNone: TTBXItem
      Action = aSelectNone
    end
    object ipmPicsFileOperations: TTBXItem
      Action = aFileOperations
    end
    object ipmPicsSep4: TTBXSeparatorItem
    end
    object giTools_PicsMenu: TTBGroupItem
    end
  end
  object mruOpen: TTBXMRUList
    HidePathExtension = False
    MaxItems = 10
    OnClick = mruOpenClick
    Prefix = 'MRU'
    Left = 416
    Top = 104
  end
  object pmView: TTBXPopupMenu
    Images = ilActionsSmall
    LinkSubitems = smView
    Left = 248
    Top = 104
  end
  object fpMain: TFormPlacement
    UseRegistry = True
    OnSavePlacement = fpMainSavePlacement
    OnRestorePlacement = fpMainRestorePlacement
    Left = 248
    Top = 168
  end
  object ilActionsSmall: TTBImageList
    ImagesBitmap.Data = {
      36F00000424D36F0000000000000360000002800000000050000100000000100
      18000000000000F0000051100000511000000000000000000000FF00FFFF00FF
      A46769A46769A46769A46769A46769A46769A46769A46769A46769A46769A467
      69A46769A46769FF00FFFF00FF0274AC0274AC0274AC0274AC0274AC0274AC02
      74AC0274AC0274AC0274AC0274AC0274AC0274ACFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF7F2B2876100FFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0005B70005B7FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B635A73635AFF00FF7B73
      6B5A524A6B635AFF00FFFF00FF314B62AC7D7EFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9C9C9C8C847B8C847B8C
      847B8C847B8C847BA59C94FF00FFFF00FFFF00FFFF00FFFF00FF9F490B9E5C3A
      B06942C9A08EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF38803B
      367E39337E36307C342E7B322C7A302A782EFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF59A4C11880A31880A31880A31880A31880A318
      80A31880A31880A31880A31880A3FF00FFFF00FFFF00FFFF00FFFF00FF327A35
      2E77322B752E267229226F261E6C221B6A1F17671C146518FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC09082D7A28FF1
      B094FEC5ABFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF8F2A03912C028B2803FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8E5D598E5D598E
      5D598E5D598E5D598E5D598E5D598E5D598E5D59734241FF00FFFF00FFFF00FF
      004B82004B82004B828E5D598E5D598E5D598E5D598E5D598E5D598E5D598E5D
      598E5D59734241FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF8E5D598E5D598E5D598E5D598E5D598E5D598E5D
      598E5D598E5D59734241FF00FF314B62AC7D7EFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF314B62
      AC7D7EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF314B62AC7D7EFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF314B62
      AC7D7EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF62691F627930
      61782F5E772D5D762C5B762A3C782E78631FAC5114AF4B0FB04A0CB04708B045
      04B04402AA4A0AA9470BFF00FFFF00FFFF00FFFF00FFFF00FF8A87866C64647F
      78786C64646C64646C6464FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF319CBD1880A318
      80A31880A31880A31880A31880A31880A31880A31880A384BBCDFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5F5550625C5A857F7C857F7C857F
      7CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBA702CC86633AF
      5A2FAF5A2EA36120A36120FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFB9BA6C9BA82A7D990A648A0A5D812A7A896DFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      A46769A46769A46769A46769A46769A46769A46769A46769A46769A46769A467
      69A46769A46769FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF21
      A5CE29ADCE1084ADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF6C6A6A6C6A6AFF00FFFF00FF6C6A6A6C6A6AFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF013002014103025104025104014303013302FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF59A4C11880A31880A31880A318
      80A31880A31880A31880A31880A31880A31880A3FF00FFFF00FFFF00FFFF00FF
      FF00FF59A4C11880A31880A31880A31880A31880A31880A31880A31880A31880
      A31880A3FF00FFFF00FFFF00FFFF00FFA46769A46769A36668A46769A46769A4
      6769A46769A46769A46769A16567985F61945D5FA06466FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFCE9162C8793AC87838CD8E5DFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCE9162C8
      793AC87838CD8E5DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF74523750A02F4D9D2B4DA233FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCE9162C8
      793AC87838CD8E5DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      CF9870DA8485CA895AD45B60FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC3366CC3366CC3366CC3366CC3366CC3366CC3366
      CC3366CC3366CC3366CC3366CC3366CC3366CC3366CC3366CC33FF00FFFF00FF
      FF00FF6F6F6F6F6F6F4E4E4E555555808080FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF296F29266D26226A221D651D1A
      621A175F17145C141059100D560D0A530A075007FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      B79184FEE9C7F4DAB5F3D5AAF2D0A0EFCB96EFC68BEDC182EBC17FEBC180EBC1
      80F2C782A46769FF00FF0274AC138AC457B7E06BCBF84BBFF74ABFF74ABFF74A
      BFF74ABFF64ABFF74ABFF64BC0F72398CC0274ACFF00FFFF00FFFF00FFFF00FF
      7F2B287F2B28A18283A18283A18283A18283A18283A18283A18283A182837A1C
      1C7F2B28FF00FFFF00FFFF00FFFF00FF7F2B287F2B28A18283A18283A18283A1
      8283A18283A18283A18283A182837A1C1C7F2B28FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF7F2B287F2B287F2B28BD6B6B76100FFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF187B9C187B9C187B9C187B9C187B9C18
      7B9C187B9C187B9C187B9C187B9C187B9C187B9C187B9CFF00FFFF00FFFF00FF
      1F661F1C641C1A611A175F17135C131159110E570E0C550C095309075007044E
      04FF00FFFF00FFFF00FFFF00FF0005B70005B7FF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FF0005B70005B7FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5A5252D694949C7B7B8473739C73
      73DE8C8C4A4242FF00FF5084B20F6FE1325F8CB87E7AFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFB8531BB64D14B54A10FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADE7CEC6EFDED6F7E7D6F7
      E7D6EFDED6EFDED69F978F8C847BA59C94FF00FFFF00FFFF00FFAD4E0ACB8B61
      D8A27CB06942B06942B06942C9A08EFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF114673113F73113F73113F7311487B11
      487B113F73113F7311467311467311406B113863FF00FFFF00FF286E2861CE63
      7ED78B3D727D3C717B3B717A366A7231645D2A782E1B6A1F17671C146518FF00
      FFFF00FFFF00FFFF00FF59A4C13AAAD63CABC952BFE149B8E149B8E149B8E149
      B8E149B8E147B6DF3DB0D33CA9C9187EA1FF00FFFF00FFFF00FF276D2788DA96
      6ED27674C2905EA983559F7854A27952A7794CA17053BB6F146518FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC69081E5B29AC8
      927CBD836ED99F85C48C78C39483C39586F1B094FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF8B28038F2A038B28038F2A03FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF92625AFEDDBCFF
      D9B4FFD5ACFFD1A5FFCE9DFECA97FFC790FFC48B734241FF00FFFF00FF004B82
      25ACDA20A7D81CA3D5A46769FBE7D3F8EEDCF6EDD7F4E9D3F4E9D0F4E7D0F4E6
      CFF6E7CE734241FF00FFFF00FFFF00FF6B2A006828005C2300521F004E1E004E
      1F004E1F004E1F004E1F004F1F004F1F00401800FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFA46769FBE7D3F8EEDCF6EDD7F4E9D3F4E9D0F4E7
      D0F4E6CFF6E7CE7342415084B20F6FE1325F8CB87E7AFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5084B20F6FE1
      325F8CB87E7AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF5084B20F6FE1325F8CB87E7AFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5084B20F6FE1
      325F8CB87E7AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFA467698E5D598E5D598E5D598E5D598E5D598E
      5D598E5D598E5D598E5D598E5D598E5D5980504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF296F2948D13F
      5DBD6F3D788B3F79923E768F366A69397E39EDBE9AECBC98ECBA95EBB892EBB6
      8FEAB48CEAB38AA9470BFF00FFFF00FFFF00FFFF00FF8F8C8C8A8786AA9F9DA4
      9490AC9995D2B1AFB696966C6464FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF319CBD36A6D24CB9D249
      B8E149B8E149B8E149B8E149B8E149B8E12BA4CD4CB6D21880A4FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFAB5617A85212AA5516A95517AA5116AC
      5114AF4B0FB04A0CB04708B04504B04402AA4A0AFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF857F7C857F7C857F7CC7C7C7B8B8B8CACACAC5C5C5A19F
      9F857F7CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF000000000000000000FF00FFFF00FFFF00FFFF00FF00
      0000000000000000FF00FFFF00FFFF00FF000000000000000000FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBA702CBF581CAB4F29A0512BA7
      4B23B14A23B9532FC45E40A55024A55024FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFE2D46CE5DE02FBFB00EAFE00C7F600A4EC0081E3015DD2002E89057284
      6FFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6971AD3D4BB42C
      3DB92C3CB13B469D595E8DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      485360FEE9C7F4DAB5F3D5AAF2D0A0EFCB96EFC68BEDC182EBC17FEBC180EBC1
      80F2C782A46769FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFC26D43C26D43C26D43C26D43C26D43C26D43C26D43C2
      6D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43FF00FFC26D43C26D43
      C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D
      43C26D43C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFADADADADADADFF00FFFF00FFFF00FFFF00FFADADADADAD
      ADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF29A5CE21A5CE10739C109CC621
      ADD631B5D6188CB510739C42ADCE39ADCEFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF6C6A6AAAA7A7A19F9F6C6A6A6C6A6A6C6A6AE5E3E36C6A6A6C6A6A6C6A
      6AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF014503014503037808039C0B039F0C039F0C039D0C027E09014D04014D
      04FF00FFFF00FFFF00FFFF00FFFF00FF59A4C13AAAD63CABC952BFE149B8E149
      B8E149B8E149B8E149B8E147B6DF3DB0D33CA9C9187EA1FF00FFFF00FFFF00FF
      59A4C13AAAD63CABC952BFE149B8E149B8E149B8E149B8E149B8E147B6DF3DB0
      D33CA9C9187EA1FF00FFFF00FFFF00FF485360F7E2C1DDC5A3E3C79FEECC9DEF
      CB96EFC68BEDC182E7BD7DD3AE73A88A5BA78959975E60FF00FFFF00FFFF00FF
      FF00FFFF00FFC87E43C87D41F0E0D6F1E2DAF2E5DEF4E8E2C87735C87633FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC87E43C87D41F0E0D6F1
      E2DAF2E5DEF4E8E2C87735C87633FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFC87E43B3703A887E79189255148F4F5BC0604B9B27499922FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC87E43C87D41F0E0D6F1
      E2DAF2E5DEF4E8E2C87735C87633FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC87129C8732EFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCA895A
      E2C1ABEBD6CAEEDED3F3E7E1E08292FF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC33BBE9A5B7E4A1AED99AAAD496AAD396AAD396AC
      D698B1DC9CADD899B1DC9CBAE7A4BAE7A4B2DD9DADD89962C331FF00FFB6A4A4
      7D6B6B967C7CB49898AA8B8B846767534747424242FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FF009C00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF009C00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF2E752E79CD7B70CA7260C46254BF5749
      BB4C3EB64133B23629AE2C20AA2313A51607A10B075007FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF007000007000FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      B79187FCEACEF3DABCF2D5B1F0D0A7EECB9EEDC793EDC28BE9BD81E9BD7FE9BD
      7FEFC481A46769FF00FF0274AC33AAE02392C489D9FA54C7F854C7F753C7F854
      C7F754C7F854C7F854C7F853C7F7279DCE6ACBE50274ACFF00FFFF00FF7F2B28
      CA4D4DB64545DDD4D5791617791617DCE0E0D7DADED7DADECED5D7BDBABD7610
      0F9A2D2D7F2B28FF00FFFF00FF7F2B28CA4D4DB64545DDD4D5791617791617DC
      E0E0D7DADECED5D7CED5D7BDBABD76100F9A2D2D7F2B28FF00FFFF00FFFF00FF
      7F2B287F2B28C66B6BD66B6BD66B6BC66B6B76100F7F2B287F2B287F2B287F2B
      287F2B287F2B28FF00FFFF00FF188CB5188CB5188CB5188CB5188CB5188CB518
      8CB5188CB5188CB5188CB5188CB5188CB5188CB5188CB5187B9CFF00FF266D26
      90DEA18BDC9C86DA9880D99378D68C73D5886ED38368D27F63D07A5ECE7558CD
      71044E04FF00FFFF00FFFF00FF0005B70005B70005B7FF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FF0005B70005B7FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B636B8C6B6BEFA5A5FFADADAD7B
      7B635252FF00FFFF00FF32A0FE37A1FF106FE2325F8BB67D79FF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFCD865FB54A10B54A10D69979D69979FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFCEC6BDF7E7D6F7F7F7E7E7E7CEB5A5D6
      AD94DEC6BDEFF7F7F7EFEFEFDED69F978FA59C94FF00FFFF00FFB5520CD9A581
      FEE5C7FCDCBBF6CEA8E8B288B06942B06942AF724DC9A08EFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF11487B1858941961A51966AD1966AD1961A519
      66AD1961A51961A51961A5185C9C18589411487B113659FF00FF2C722C69EE64
      1AD0022EB74C64BFE764BFE659ACCF64AB702C7A3053A87A4CA16F53BB6F1465
      18FF00FFFF00FFFF00FF319CBD51BEED54C1D783ECFF79E5FF79E5FF79E5FF79
      E5FF79E5FF74D9FC5CCAE854C0D7187EA1FF00FFFF00FFFF00FF2A702A81DF89
      2DD81C34A93E3A8281316073376B80407D9644858157B87317671BFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF97776797776797776786625897776789
      6B5E9375658663538F6758C18973D59A81D99980D2A390FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF912C02FF00FFFF00FF8B28038B2803FF00FF8B2803912C
      02FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF98665DFFE3C7DD
      903DDD903DDD903DDD903DDD903DDD903DFFC792734241FF00FFFF00FF004B82
      2DB4DE28AFDC22AAD9A46769F3DCCFDC9241DC913FDA913FDA913FDA903EDA91
      3FEBDAC2734241FF00FFFF00FF963A00BC4A00B54600AA4200A13F009A3D0098
      3C00993C00993C00993C00993C009F3F007D3100401800FF00FFFF00FF6B2A00
      6828005C2300521F004E1E00A46769F3DCCFDC9241DC913FDA913FDA913FDA90
      3EDA913FEBDAC273424132A0FE37A1FF106FE2325F8BB67D79FF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF32A0FE37A1FF
      106FE2325F8BB67D79FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF32A0FE37A1FF106FE2325F8BB67D79FF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF32A0FE37A1FF
      106FE2325F8BB67D79FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFA46769FCEACEF3DABCF2D5B1F0D0A7EECB9EED
      C793EDC28BE9BD81E9BD7FE9BD7FEFC48180504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF2E742E5BF34D
      1CD70463CEDB6DD0FA6ACAF35F7575397E39EDC09DEDBE9AECBC98ECBA95EBB8
      92EBB68FEAB48CB04200FF00FFFF00FFFF00FF8F8C8C8D8C8BADA48DDCCCA89A
      A0C5929DCDD9D2B7BAAF97BD9C9B6C6464FF00FFFF00FFFF00FFFF00FF0274AC
      0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274
      AC0274ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF319CBD4AB8E66FD8E679
      E5FF79E5FF79E5FF79E5FF79E5FF79DFFF41B5DE70D7E61880A4FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFC86318CA661CD17837D2783CD27638CF6B29D0
      6626D35F1FD4571AD44F14D5490BD44404C84700A9470BFF00FFFF00FFFF00FF
      857F7C857F7C857F7CCFCFCFEEEEEED5D5D5DBDBDBB1B1B1CDCDCDB5B5B5C6C6
      C6857F7CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF32A0F40E
      42ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      1F661F1C641C1A611A175F17135C131159110E570E0C550C095309075007044E
      04FF00FFFF00FFFF00FF000000FFFFFF0000006E6E6E6B6B6B68686862626200
      0000FFFFFF0000005252524E4E4E4A4A4A000000FFFFFF000000CE6300CE6300
      CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE63
      00CE6300CE6300FF00FFFF00FFFF00FFBA702CA8410E9E380696390378460A77
      4407993702A03A09A64013B04922A54F25A54F25FF00FFFF00FFFF00FFFF00FF
      EECC43FDE400FFFC00FFFF00F2FE00CBF700A1EB0078E00153D6013DD7152FC8
      334D7456FF00FFFF00FFFF00FFFF00FFFF00FF8890CC253AD4142DEA132CF013
      2DF1132CF0112AE71026CA2433A5656989FF00FFFF00FFFF00FFFF00FF4380B7
      1F6FC2656B86F3DABCF2D5B1F0D0A7EECB9EEDC793EDC28BE9BD81E9BD7FE9BD
      7FEFC481A46769FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFC26D43EBEBEBEDEDEDEEEEEEF0F0F0F2F2F2F4F4F4F5
      F5F5F7F7F7F8F8F8FAFAFAFBFBFBFDFDFDFFFFFFC26D43FF00FFC26D43EBEBEB
      EDEDEDEEEEEEF0F0F0F2F2F2F4F4F4F5F5F5F7F7F7F8F8F8FAFAFAFBFBFBFDFD
      FDFFFFFFC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADA8
      A8A8989898989898A8A8A8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF072ACA0625B0ADADADFF00FFFF00FFFF00FF072ACA0625B0ADAD
      ADFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF31A5CE31BDDE10ADDE10739C18
      C6F74AD6FF31A5CE5AC6DE63C6DE39A5C6FF00FFFF00FFFF00FFFF00FFFF00FF
      6C6A6ADAD9D9A19F9FA19F9FA19F9F3736363535356C6D6DBFBFBFE1E2E2B7B6
      B66C6A6A6C6A6A6C6A6AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      034F0903650904A30D03A60C03A00B029E0A039F0C03A00C03A50C03A60C0269
      06013402FF00FFFF00FFFF00FFFF00FF319CBD51BEED54C1D783ECFF79E5FF79
      E5FF79E5FF79E5FF79E5FF74D9FC5CCAE854C0D7187EA1FF00FFFF00FFFF00FF
      319CBD51BEED54C1D783ECFF79E5FF79E5FF79E5FF79E5FF79E5FF74D9FC5CCA
      E854C0D7187EA1FF00FFFF00FF4380B71F6FC25A5F77948573A8947BD8BB96EA
      C79BEDC793EABF89D2AA746453860005B7C8A46C9F6466FF00FFFF00FFFF00FF
      FF00FFC98148EBD5C8ECD8CCEEDDD2F0E0D6F1E2DAF2E5DEF4E8E2F5EBE6C876
      33FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC98148EBD5C8ECD8CCEEDDD2F0
      E0D6F1E2DAF2E5DEF4E8E2F5EBE6C87633FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC98148EBD5C8D3C1B6867D771C955A1A93575DC1625BC06059BF5D4999
      22FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC98148EBD5C8ECD8CCEEDDD2F0
      E0D6F1E2DAF2E5DEF4E8E2F5EBE6C87633FF00FFFF00FFFF00FFFF00FF14AD25
      10AA1E0CA61608A30F049F07009C00FF00FFFF00FFC97F43C87C3DC87938C876
      33C8732EC87129FF00FFFF00FFFF00FFFF00FFC8732EC38255C87938C87C3DFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCF9870DDB99F
      E6CCBCE8D0C3E9D4C8E8DAD2EADFD9CE575EFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC33B7E4A19FC58C81A1727C9A6D7E9D6F7794697A
      986C8DAF7C7E9D6F8CAE7CAED99AA9D2958EB07D87A87757AE2CD5C0C0B6A4A4
      DBD7D7E0DEDEDED5D5DAC6C6CEBABAB993937E62623E3E3EFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FF007308007308FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF007308007308FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF31773182D18452B96031AE4B4FBE5954
      BF5749BB4C41B74437AE4524A93E2CB03712A5150A530AFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF007000FF00FF007000FF00FFFF00FF007000FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF0274AC0274AC0274AC0274AC0274AC0274AC02
      74AC0274AC0274AC0274AC0274AC0274ACFF00FFFF00FFFF00FFFF00FFFF00FF
      B7938AFEEFDAF6E0C6F2DABCF2D5B2EFD0A9EECB9EEDC796EBC28CE9BD82E9BD
      7FEFC481A46769FF00FF0274AC57CAF80274AC99E3FB5ED1FA5ED1FA5ED1FA5E
      D1FA5ED1FA5FD1FA5ED1F85ED1F82CA1CE99EDF70274ACFF00FFFF00FF7F2B28
      C24A4BB14444E2D9D9791617791617D9D8DAD9DEE1D9DEE1D3D9DCC1BDC17611
      11982D2D7F2B28FF00FFFF00FF7F2B28C24A4BB14444E2D9D9791617791617D9
      D8DAD9DEE1D3D9DCD3D9DCC1BDC1761111982D2D7F2B28FF00FFFF00FFFF00FF
      7F2B28D66B6BD66B6BD66B6BCE6B6BC66B6B76100FC67B7BDE8C8CF79494F7A5
      A5F7A5A57F2B28FF00FF319CBD63CEFF188CB59CFFFF6BD6FF6BD6FF6BD6FF6B
      D6FF6BD6FF6BD6FF6BD6FF6BD6FF39B5DE9CF7FF188CB5187B9CFF00FF296F29
      96DFA615B8008BDC9C36677C35657A35657A35657A35657A305B6E2649585ECE
      75075007FF00FFFF00FFFF00FF0005B70005B60005B70005B7FF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FF0005B70005B7FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8C7B7BDE9C9C5242
      42FF00FFFF00FFFF00FFFF00FF37A4FE379FFF0E6DDE355F89BB7F79FF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFB54A10B54A10C97A50FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFE7D6CEF7E7DEF7FFFFCE9C84B54A10BD6339D6
      AD9CC65A21BD5221D6AD9CF7FFFFF7DED69F978FA59C94FF00FFB85510E0B293
      FFE6CCFFE1C3FDDDBCFCDAB6FFD9B2FCC991F1B06BBB6934FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF236A231B631B135C130B550B044E04FF
      00FF236A231B631B135C130B550B044E04FF00FFFF00FFFF00FFFF00FFFF00FF
      A7E3A79CE09C91DC9186D9867BD67BFF00FFA7E3A79CE09C91DC9186D9867BD6
      7BFF00FFFF00FFFF00FFFF00FF19578B1961A5226EB49BBBDFB5CDE74A8ECE21
      7BCF217BCF217BCF217BCF226EB418569C10478411406BFF00FF30763072EE6C
      21E5071FC81461D2D27FCEF0A4A4AB6261572E7B32488BA845878457B8731767
      1BFF00FFFF00FFFF00FF319CBD51BEED54C1D78DF1FF87EBFF87EBFF87EBFF87
      EBFF87EBFF80EBFC65CAE957BDD7187EA117671C146518FF00FF2E742E8AE691
      29F00D19CA02467BD44380AA376A81417C9651808862AC6F1B6A1FFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF327A352E77322B752E267229226F261E6C221B
      6A1F17671C146518796559947565D6A893EBB398D5A48FFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF912C02FF00FFFF00FF8B2803FF00FF8B28038B28038B28
      038F2A03FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA06C5FFEE9D3FF
      E5CAFFE1C1FFDCBAFED8B1FED3A9FFD0A1FFCC9A734241FF00FFFF00FF004B82
      34BAE22EB4E029AFDDA66A6AF6E1D5F7DCC0F7D0ABF7D0ABF7D0ABF6CEA5F2D3
      B1EBDCC5734241FF00FFFF00FFB74800E25800D35200CA4F00C24C00B84800B0
      4500AD4400AC4300AD4300AD4300B446009F3F004F1F00FF00FF963A00BC4A00
      B54600AA4200A13F009A3D00A66A6AF6E1D5F7DCC0F7D0ABF7D0ABF7D0ABF6CE
      A5F2D3B1EBDCC5734241FF00FF37A4FE379FFF0E6DDE355F89BB7F79FF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF37A4FE
      379FFF0E6DDE355F89BB7F79FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF37A4FE379FFF0E6DDE355F89BB7F79FF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF37A4FE
      379FFF0E6DDE355F89BB7F79FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFA0675BFEEFDAF6E0C6F2DABCF2D5B2C1C18800
      7000007000BDB672E9BD82E9BD7FEFC48180504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF347A3464F555
      1DDD0519C802ACD7EBC8ACA9756463397E39EEC2A0EDC09DEDBE9AECBC98ECBA
      95EBB892EBB68FB14503FF00FFFF00FF8F8C8CA6A5A2D2C3A0FFEBBFFFF0CEDC
      CCC3DCD1D2FFF6E9FDF9EFAAA792C2A19F625757FF00FFFF00FF0274AC48BCF6
      0274AC8CD8FA4BBFF74ABFF64ABFF74ABFF74ABFF64ABFF74ABFF64BBFF62398
      CC97E0F20274ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      9865429F6442885134845F46FF00FFFF00FFFF00FFFF00FFFF00FF9865429F64
      42885134845F46FF00FFFF00FFFF00FF9865429F6442885134845F466FD8E687
      EBFF87EBFF87EBFF87EBFF87EBFF86EEFF4BB6DF74D4E61880A4FF00FFFF00FF
      A7E3A79CE09C91DC9186D9867BD67BFF00FFA7E3A79CE09C91DC91509450044E
      04FF00FFFF00FFFF00FFFF00FFD46216DA7A3AFEFCFAF9E8DDFAEAE0EBAF87EB
      A883E16A2FE25D22E1531BE24812E0420AD34303B04200FF00FF857F7C857F7C
      BBBBBB9292927B7B7B949494C2C2C2D4D4D4E0E0E0B4B4B4D2D2D2C1C1C1C2C2
      C2857F7CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1B71FF00
      4BE4FF00FFFF00FF6B92E1FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF266D26
      90DEA18BDC9C86DA9880D99378D68C73D5886ED38368D27F63D07A5ECE7558CD
      71044E04FF00FFFF00FF000000000000000000A1A1A1A1A1A1A1A1A1A1A1A100
      0000000000000000A1A1A1A1A1A1A1A1A1000000000000000000CE6300FFFFFF
      FFFFF7FFFFFFFFF7E7FFEFD6FFE7C6FFD6ADFFD6ADFFD6ADFFD6ADFFD6ADFFD6
      ADFFD6ADCE6300FF00FFFF00FFBA702C9E3801A03900A64000A547005D580031
      5C01854900A23F009F39009F3805A54014B0573C9D512AFF00FFFF00FFFACF6B
      FEBF00FFD400FFEE00FFFE00F9FE00D2F6009FE8006BDC0145D50836DF322EEA
      5922D86B6F887CFF00FFFF00FFFF00FF838DDE1B33EC132DF4142EF5142EF514
      2EF5142EF5142EF5132DF4112AE51528B063688AFF00FFFF00FFFF00FFFF00FF
      32A3FF1672D76B6A80F2DABCF2D5B2EFD0A9EECB9EEDC796EBC28CE9BD82E9BD
      7FEFC481A46769FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4F00A74F00A7
      4F00A74F00A74F00A7FF00FF4F00A74F00A74F00A74F00A74F00A7FF00FF4F00
      A74F00A74F00A74F00A7C26D43E9E9E9EBEBEBEDEDEDEEEEEEFEDCD8FEDCD8FE
      DCD8FEDCD8FEDCD8F8F8F8FAFAFAFBFBFBFDFDFDC26D43FF00FFC26D43E9E9E9
      EBEBEBEDEDEDEEEEEEF0F0F0F2F2F2C02B1CF5F5F5F7F7F7F8F8F8FAFAFAFBFB
      FBFDFDFDC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADAD0B6E0D09
      6C0B076A09056806848484949494ADADADFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF072EDA0627B8FF00FFFF00FFFF00FFFF00FF072EDA0627B8FF00
      FFFF00FFFF00FFFF00FFFF00FF218CB510739C4AB5D684DEEF52C6DE39CEEF18
      D6FF39DEFF5AD6F773C6DEB5E7EF94CEE710739CFF00FFFF00FFFF00FF6C6A6A
      D4D3D3CACACA8E8C8C8E8C8C8E8C8C3C3B3B0A090A0707070B0B0B0707077A7A
      7ABBBBBB6C6A6AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF044F09
      066B110AAB1F07A415049E0D029D0A039D0A039E0C039E0C039E0C03A00C03A7
      0C026A06024C04FF00FFFF00FFFF00FF319CBD51BEED54C1D78DF1FF87EBFF87
      EBFF87EBFF87EBFF87EBFF80EBFC65CAE957BDD7187EA1FF00FFFF00FFFF00FF
      319CBD51BEED54C1D78DF1FF87EBFF87EBFF87EBFF87EBFF87EBFF80EBFC65CA
      E957BDD7187EA1FF00FFFF00FFFF00FF32A3FF0005B72D2E6B736859A7937ADA
      BE9AE7C599D9B6897562832B279FBE9A68E7BD7CA46769FF00FFFF00FFFF00FF
      C9824CE8CFC0E9D2C4EBD5C8ECD8CCEEDDD2F0E0D6F1E2DAF2E5DEF4E8E2F5EB
      E6C87633FF00FFFF00FFFF00FFFF00FFC9824CE8CFC0E9D2C4EBD5C8ECD8CCEE
      DDD2F0E0D6F1E2DAF2E5DEF4E8E2F5EBE6C87633FF00FFFF00FFFF00FFFF00FF
      C9824CE8CFC0E9D2C4D2BEB3857A731F985F1E965C60C3675DC1625BC06059BF
      5D499922FF00FFFF00FFFF00FFFF00FFC9824CE8CFC0E9D2C4EBD5C8ECD8CCEE
      DDD2F0E0D6F1E2DAF2E5DEF4E8E2F5EBE6C87633FF00FFFF00FFFF00FF14AD25
      31C65A21BB4710B03400A521049F07FF00FFFF00FFC98148DBB59AD3A483CB93
      6CC38255C8732EFF00FFFF00FFFF00FFFF00FFC87633CB936CD3A483DBB59AC9
      8148C9834CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCF9870E0C0AC
      E3C7B4E6CCBCBBB6A47B7F737B8175797968FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC33AAB87C96491798330199330099330099330081
      794C9933008F5A2A7D9B6E99BC85946D3A98360499875260BF30D5C0C0EDF1F1
      F0F3F3E7E8E8DBD5D5DAC8C8D3BCBCC6A3A3C699995D5050FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF00730800730800730800730800730800
      730800730800730800730807A10B007308FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF00730832C4530073080073080073080073080073080073080073080073
      08007308FF00FFFF00FFFF00FFFF00FF337A3387D28A17933210963133AF4B60
      C46254BF5751BD5413912E10963125A93E21AB240D560DFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF007000007000FF00FFFF00FFFF00FFFF00FF007000FF00
      FFFF00FFFF00FFFF00FF0274AC138AC456B6E16ACBF84BBFF74ABFF74ABFF74A
      BFF74ABFF64ABFF74ABFF64BC0F72398CC0C81BAFF00FFFF00FFFF00FFFF00FF
      BA978FFFF4E5F7E5CFF4E0C5F3DABBF2D5B1F0D0A6EECB9EEDC795EBC28AEABF
      81EFC480A46769FF00FF0274AC5ED3FA0B81B782D5EF79E0FA6ADCFA69DCFB69
      DCFB6ADCFB69DCFB69DCFA6ADDFB2FA6CF9FF0F70274ACFF00FFFF00FF7F2B28
      C24A4AB04242E6DCDC791617791617D5D3D5D8DEE1D8DEE1D7DDE0C6C2C5700F
      0F962C2C7F2B28FF00FFFF00FF7F2B28C24A4AB04242E6DCDC791617791617D5
      D3D5D8DEE1D7DDE0D7DDE0C6C2C5700F0F962C2C7F2B28FF00FFFF00FFFF00FF
      7F2B28D66B6BD66B6BD67373D67373CE6B7376100F0094000094000094000094
      00F7A5A57F2B28FF00FF319CBD63CEFF188CB59CFFFF7BE7FF7BE7FF7BE7FF7B
      E7FF7BE7FF7BE7FF7BE7FF7BDEFF42B5DE9CFFFF188CB5187B9CFF00FF2C722C
      9BE1AA25FF0816C10013AB005FB6DB60B7DD5FB6DB5FB6DB529DBD68D27F63D0
      7A095309FF00FFFF00FFFF00FFFF00FF0006D70005BA0005B70005B7FF00FFFF
      00FFFF00FFFF00FF0005B70005B7FF00FFFF00FFFF00FFFF00FF6B635A736352
      7363527363527363527363527363527363527363527363527B736BDE9C9C3929
      2173635A73635A84736BFF00FFFF00FF37A4FE359EFF0F6FDE35608BA67B7FFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFB64D14B54A10B75017FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFEFDED6F7FFFFC67B5ABD4A10C65218C6A594FF
      FFFFDE9473BD4A10B54A10CE9C84F7FFFFEFDED68C847BFF00FFB95814E3BA9D
      FFE9D3FEE5CAB9CEC5CCD1BEFEDAB5FFBF6EFAB864C1743DFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF2A712A90DDA182D99574D5890B550BFF
      00FF2A712A90DDA182D99574D5890B550BFF00FFFF00FFFF00FFFF00FFFF00FF
      B2E6B290DDA182D99574D58986D986FF00FFB2E6B290DDA182D99574D58986D9
      86FF00FFFF00FFFF00FFFF00FF185C9C226EB43183CFFFFFFFFFFFFFFFFFFF49
      93D7217BCF217BCF217BCF2272BC1966AD185894114673FF00FF397E3982F37A
      22EA081CC314ADD6D2CEB1AEB99F9C636258307C3465C1E95D929B64AF711B6A
      1F17671C146518FF00FF319CBD54C1ED54C1D799FFFF98FFFF98FEFF9AFCFF98
      FFFF98FFFF8FF6FE6ED7F654C1D7187EA14CA17053BB6F14651830763090E795
      2DF4101BD303367BE91175EC4A7FBF5B5354564D4D56885D1C6620FF00FFFF00
      FFFF00FFFF00FFFF00FF276D2788DA966ED27674C29060AC865CAC8258AA7F53
      A87A4CA17053BB6F1465188A6958D0A38DE1B399DAAB95FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF8F2A038B28038B28038B2803FF00FF8B2803FF00FFFF00
      FF902B02FF00FFFF00FF8E5D598E5D598E5D598E5D598E5D59A77463FEEEDDDD
      903DDD903DDD903DDD903DDD903DDD903DFED1A4734241FF00FFFF00FF004B82
      3CBFE736BAE331B5E1AA6D6BF7E5DCDC9241DC913EDC913EDC913EDC903DDA91
      3FEEDECA734241FF00FFFF00FFC44D00E75B00D95600D15300C94E00CE7328CF
      8240CA8041C17332B15510A74100AD4300993C004F1F00FF00FFB74800E25800
      D35200CA4F00C24C00B84800AA6D6BF7E5DCDC9241DC913EDC913EDC913EDC90
      3DDA913FEEDECA734241FF00FFFF00FF37A4FE359EFF0F6FDE35608BBD9494FF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      37A4FE359EFF0F6FDE35608BBD9494FF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF37A4FE359EFF0F6FDE35608BBD9494FF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      37A4FE359EFF0F6FDE35608BBD9494FF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFA0675BFFF4E5F7E5CF007000C4CA97007000C2
      C187C0BD80007000BDB66FEABF81EFC48080504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF7F2B28FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC24A4A7F2B28FF00FFFF00FFFF
      00FF7F2B28FF00FFFF00FFFF00FF7F2B28FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF7F2B28FF00FFFF00FFFF00FF7F2B28FF00FFFF00FFFF00FFC24A4A7F2B
      28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF7F
      2B28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3F843F7CF76D
      25D7253F7EE38190DCC9ACA9756463397E39EFC5A4EEC2A0EDC09DEDBE9AECBC
      98ECBA95EBB892B14503FF00FFFF00FFAFAEAEBEB096FDEDBBFFEAB8FFE5B3FF
      E4B1FFDFADFFDAA9FFEBD2FDFBF3B5AD946C6464FF00FFFF00FF0274AC4FC4F7
      0274AC92DDFB54C7F854C7F753C7F854C7F754C7F854C7F854C7F853C7F7279D
      CE9DE3F20274ACFF00FFFF00FFFF00FF9865429F6442885134845F46FF00FFFF
      00FFFF00FFFF00FFFF00FF9865429F6442885134845F46FF00FFFF00FFB77C52
      C77D4CA36F489569438F5E3B80553DFF00FFFF00FFFF00FFB77C52C77D4CA36F
      489569438F5E3B80553DFF00FFB77C52C77D4CA36F489569438F5E3B80553D98
      FFFF98FFFF9AFCFF98FFFF98FFFF95FCFF5AC5F370D8E61880A4FF00FFFF00FF
      B2E6B290DDA182D99574D58986D986FF00FFB2E6B290DDA150945074D5890B55
      0BFF00FFFF00FFFF00FFFF00FFD6681EDD8545FDF5EFE8A97AEEBD9CFCF0E8FD
      F5EFF1B699E56431E3521FE34C17E2450ED54607B14503FF00FF9F9C9AD6D6D6
      8A8A8A525252494949585858A0A0A0C7C7C7E0E0E0B4B4B4D6D6D6C7C7C7C4C4
      C4A5A5A5FF00FFFF00FFFF00FFFF00FF6B92E1004BE43787FFFF00FF1D72FF00
      4BE4FF00FF2D51AB054BD92D51ABFF00FFFF00FFFF00FFFF00FF787878757575
      7171716E6E6E6B6B6B6868686262625E5E5E5A5A5A5656565252524E4E4E4A4A
      4A464646424242FF00FFFF00FF7C7C7CC1C1C1BCBCBCBABABAB6B6B61C2CB813
      23AF0919A500109CA9A9A9A6A6A6A3A3A3A1A1A1464646FF00FFCE6300FFFFFF
      FFFFFFFFFFFFFFFFFFFFF7E7FFEFD6FFE7C6FFDEB5FFD6ADFFD6ADFFD6ADFFD6
      ADFFD6ADCE6300FF00FFFF00FFB25109AA4300B54E00BB5500B35B005C660004
      6D00496200A25600B24B00A74100993B04885E379F583AFF00FFFF00FFF69D02
      F5A500FCB900FED300F7ED00BCB0128E7C1F76711D5C8C1039CB252DEA5925F5
      811EFC9F12A575FF00FFFF00FF818FF92139F1142EF51A34F55B6DF7818FF98A
      93D7818FF94458F0142EF5132DF4122BE81E2EA9858791FF00FFFF00FFFF00FF
      A0675B34A1FF1572D45E6782F3DABBF2D5B1F0D0A6EECB9EEDC795EBC28AEABF
      81EFC480A46769FF00FFFF00FFFF00FFFF00FF01679901679901679901679901
      6799016799016799016799016799FF00FFFF00FFFF00FFFF00FFC7F5F3C7F5F3
      C7F5F3C7F5F3C7F5F34F00A7C7F5F3C7F5F3C7F5F3C7F5F3C7F5F34F00A7C7F5
      F3C7F5F3C7F5F3C7F5F3C26D43E8E8E8E9E9E9EBEBEBEDEDEDF7C9C4F7C9C4F7
      C9C4F7C9C4F7C9C4F7F7F7F8F8F8FAFAFAFBFBFBC26D43FF00FFC26D43E8E8E8
      E9E9E9EBEBEBEDEDEDEEEEEEC63C2EC63C2EC63C2EF5F5F5F7F7F7F8F8F8FAFA
      FAFBFBFBC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1073150E71121D9E1B1E
      981E1F93211F8E23056806036604949494FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFADADADADADADFF00FFFF00FFFF00FFFF00FFADADADADAD
      ADFF00FFFF00FFFF00FFFF00FF63BDD64AB5D652BDE794EFFF8CEFFF5AE7FF21
      DEFF18DEFF52E7FF7BEFFF7BEFFF189CCE21A5D629A5CEFF00FF6C6A6ACACACA
      CACACA8E8C8CD7D4D4CECBCBBFBCBCB1AFAFA3A0A08886865E5B5C0707070909
      090808086C6A6A767373FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF044F09
      10AC300DAB2809A41C039E0F16AA20D5F2D8E9F8EA48C052039E0C039E0C039F
      0C03A70C024C04FF00FFFF00FFFF00FF319CBD54C1ED54C1D799FFFF98FFFF98
      FEFF9AFCFF98FFFF98FFFF8FF6FE6ED7F654C1D7187EA1FF00FFFF00FFFF00FF
      319CBD54C1ED54C1D799FFFF98FFFF98FEFF9AFCFF98FFFF98FFFF8FF6FE6ED7
      F654C1D7187EA1FF00FFFF00FFFF00FFA0675B0005B70005B7282D6E7F7262AD
      987FC1A88677678E0005B7B39477E0B77BEEC37FA46769FF00FFFF00FFC98551
      E5CAB8E7CDBCE8CFC0E9D2C4EBD5C8ECD8CCEEDDD2F0E0D6F1E2DAF2E5DEF4E8
      E2F5EBE6C87633FF00FFFF00FFC98551E5CAB8E7CDBCE8CFC0E9D2C4EBD5C8EC
      D8CCEEDDD2F0E0D6F1E2DAF2E5DEF4E8E2F5EBE6C87633FF00FFFF00FFC98551
      E5CAB8E7CDBCE8CFC0D0BCAF857871249A6322996062C46960C3675DC1625BC0
      6059BF5D499922FF00FFFF00FFC98551E5CAB8E7CDBCE8CFC0E9D2C4EBD5C8EC
      D8CCEEDDD2F0E0D6F1E2DAF2E5DEF4E8E2F5EBE6C87633FF00FFFF00FFFF00FF
      18B12D31C65A21BB4710B03408A30FFF00FFFF00FFC9834CE1C1ACDBB59AD3A4
      83C87938FF00FFFF00FFFF00FFFF00FFFF00FFC87938D3A483DBB59AE1C1ACE6
      CCBBECD6C9C98856CA8A5AFF00FFFF00FFFF00FFFF00FFFF00FFCA895ADDBAA1
      005184E4C7B42DA06C2198581B934E1F8F3BFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC339B4C189933008476489FBC839D5D2899330082
      7A4C9933008E5A2A70875F935B299838069B9862B2DE9E65CA33FF00FFD5C0C0
      F7F5F7EDF0F2DEDCDCD4B1B1CE9292C68E8EC59999FF00FFFF00FFFF00FFFF00
      FF777978375437585858FF00FFFF00FF00730840CE683ACA5F32C4532DC04A28
      BC4223B83A1CB22E15AD230EA71707A10B007308FF00FFFF00FFFF00FFFF00FF
      00730840CE683ACA5F32C4532DC04A28BC4223B83A1CB22E15AD230EA71707A1
      0B007308FF00FFFF00FFFF00FFFF00FF367D3693D8954AAB5817933252B85F6F
      C9715CC36057C05B37A34613912E37AE452AAF2D105910FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF007000007000007000FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF0274AC33AAE02392C489D9FA54C7F854C7F753C7F854
      C7F754C7F854C7F854C7F853C7F7279DCEBAEBEF0274ACFF00FFFF00FFFF00FF
      C09E95FFFBF0F8EADCF6E3CFF4E0C6F2D9BCF2D5B1F0D0A9EDCB9EEDC695EBC2
      8AEFC583A46769FF00FF0274AC68DAFB2BA4D14AB2D797EBFC74E5FB74E5FB74
      E5FC74E5FC74E5FB74E5FC046B0B33A9CFA3F4F752BBD70274ACFF00FF7F2B28
      C24A4AB04141EADEDEE7DDDDDDD4D5D7D3D5D5D7D9D5D7D9D7D8DACAC2C57E17
      179E31317F2B28FF00FFFF00FF7F2B28C24A4AB04141EADEDEE7DDDDDDD4D5D7
      D3D5D5D7D9D7D8DAD7D8DACAC2C57E17179E31317F2B28FF00FFFF00FFFF00FF
      7F2B28D67373D67373DE7373DE7373D6737376100F0094000094000094000094
      00F7A5A57F2B28FF00FF319CBD63CEFF188CB59CFFFF84E7FF84E7FF84E7FF84
      E7FF84E7FF84E7FF84E7FF84EFFF4AB5DEA5F7FF188CB5187B9CFF00FF2F752F
      A0E2AF35FF161FE20618C60270D4FF70D4FF70D4FF6FD2FD5DB2D760535168D2
      7F0C550CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0005B70005B70005B6FF
      00FF0005B60005B70005B7FF00FFFF00FFFF00FFFF00FFFF00FF6B635AFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF847B7BDEA5A5524A
      42FFFFFF635A5A73635AFF00FFFF00FFFF00FF38A5FE329DFF156DCE444F5BFF
      00FF9C6B65AF887BAF887EAA8075FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFC97A50B54A10B54A10FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7E7E7F7F7F7D6AD94BD4A10CE6331CE6329CE6B39DE
      8C6BCE6329CE6331C65A29B54A10DEC6BDF7EFE79F978FA59C94B95813EBCAB3
      FFECD9D0DBD0188CB58BC0C6FCD9B0EEB35DE8C294BE7440FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF3278329EE2AD90DDA182D995135C13FF
      00FF3278329EE2AD90DDA182D995135C13FF00FFFF00FFFF00FFFF00FFFF00FF
      BDEABD9EE2AD90DDA182D99591DC91FF00FFBDEABD9EE2AD90DDA182D99591DC
      91FF00FFFF00FFFF00FFFF00FF1961A52272BC94BCE8FFFFFFFFFFFFFFFFFFBC
      DCF8217BCF217BCF217BCF2272BC195FAD185C9C113F73FF00FF438843BAF2BB
      4DB4A93A8AC64576FF8692D7BAA09D65625A337D36BAAAAB87797968A3701E6C
      224CA17053BB6F146518319CBD61C9ED78C1D7E6FFFFE4FFFFE5FEFFE5FEFFE8
      FFFFEBFFFFE2FBFE9CE1FA75C1D7187EA145888457B87317671B387D38A5EEA6
      2FF6121CD7031BC2120B98FD008CFF53739C363D352E51321A541D1C65201B6A
      1F17671C146518FF00FF2A702A81DF892DD81C34A93E408F8E4B91AF4B91AE48
      8CA845888457B87317671B866455C4927CDB9D84D9AB95FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF8B2803912C028F2A038B28038F2A03FF00FFFF00
      FF8B2803FF00FFFF00FF92625AFEDDBCFFD9B4FFD5ACFFD1A5B17D67FEF3E6FE
      EFDEFEEAD8FFE7CFFEE2C7FEDEBDFFD9B6FED5AD734241FF00FFFF00FF004B82
      43C5EB3EC1E738BCE5AF726BF8EAE2F8E7D4F8DDC2F7DDC1F7DABFF6D8BBF2DC
      C2EFE1D0734241FF00FFFF00FFCA4F00F36807E96004DE5A01D45300EAB788FE
      FEFEFEFEFEFEFEFEFEFEFEC47A3DAD4300993C004E1F00FF00FFC44D00E75B00
      D95600D15300C94E00CE7328AF726BF8EAE2F8E7D4F8DDC2F7DDC1F7DABFF6D8
      BBF2DCC2EFE1D0734241FF00FFFF00FFFF00FF38A5FE329DFF156DCE5A6B73FF
      00FFAD7B73C6A59CD6B5A5CEA59CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF38A5FE329DFF156DCE5A6B73FF00FFAD7B73C6A59CD6B5A5CEA59CFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF38A5FE329DFF156DCE5A6B73FF
      00FFAD7B73C6A59CD6B5A5CEA59CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF38A5FE329DFF156DCE5A6B73FF00FFAD7B73C6A59CD6B5A5CEA59CFF00
      FFFF00FFFF00FFFF00FFFF00FFA7756BFFFBF0F8EADC007000007000C4C998F2
      D5B1F0D0A9BFBD80007000EBC28AEFC58380504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF7F2B28C24A4AFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC24A4A7F2B28FF00FFFF00FF7F
      2B28C24A4AFF00FFFF00FF7F2B28C24A4AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC24A4A7F2B28FF00FFFF00FFC24A4A7F2B28FF00FFFF00FFC24A4A7F2B
      28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC
      908F7F2B28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4C8F4CDBF3DF
      5D97FF4E83FF4E83FFE0C1BD897573397E39EFC7A7EFC5A4EEC2A0EDC09DEDBE
      9AECBC98ECBA95B14503FF00FF8F8C8CB5B2AFFBE8B7FFF8C5FFF7C4FFF5C2FF
      EDBAFFE4B1FFE0AEFFDAA9FFF5E7DED8BFCDAEAB6C6464FF00FF0274AC57CAF8
      0274AC99E3FB5ED1FA5ED1FA5ED1FA5ED1FA5ED1FA5FD1FA5ED1F85ED1F82CA1
      CEA3E9F30274ACFF00FFFF00FFB77C52C77D4CA36F489569438F5E3B80553DFF
      00FFFF00FFFF00FFB77C52C77D4CA36F489569438F5E3B80553DBA7F4DFBD0BB
      D78A5CC17C53AE6F49A168468B5E3AFF00FFFF00FFBA7F4DFBD0BBD78A5CC17C
      53AE6F49A168468B5E3ABA7F4DFBD0BBD78A5CC17C53AE6F49A168468B5E3AE4
      FFFFE4FFFFE5FDFFE4FFFFEBFFFFEBFFFF7DD4F89FD8E61880A4FF00FFFF00FF
      BDEABD9EE2AD90DDA182D99591DC91FF00FFBDEABD50945090DDA182D995135C
      13FF00FFFF00FFFF00FFFF00FFD77227DE8448F9E9DDF3D5C1E18D55E7A375F3
      CFB9FAE2D6E77345E45922E24E1AE04914D64B0EB14809FF00FFA09E9DBDBDBD
      5F5F5F554D496050494C4C4C8383835AB5D45AB5D4B4B4B4DADADACACACAC5C5
      C5A7A7A7FF00FFFF00FFFF00FFFF00FFFF00FF0050F2004BE43D86FF1170FF00
      56E40870A00348D5004BE439518FFF00FFFF00FFFF00FFFF00FF7C7C7C9B9B9B
      C4C4C4AEAEAEA1A1A19D9D9D1C2CB81323AF0919A500109CADADADB4B4B4B3B3
      B38B8B8B464646FF00FFFF00FF7F7F7FC3C3C3BEBEBEBCBCBCBABABAB6B6B61C
      2CB81323AFAEAEAEABABABA9A9A9A6A6A6A3A3A34A4A4AFF00FFCE6300FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFF7E7FFEFD6FFE7C6FFDEB5FFD6ADFFD6ADFFD6
      ADFFD6ADCE6300FF00FFBA702CB04900BD5600C55F00CC66009D6F00267D0008
      7E00427500A26800C25D00B05300864D025761207C643CA36120FCB36CE07600
      E68500EF9800F0AA00B089279F783B9E783B936F357F602E59632E23DD871AFE
      B214FFCB0FF7D56D908BFF00FF3F54F3142EF52139F4818FF9FF00FFFF00FFFF
      00FFFF00FF5768ED142EF5142EF5132DF41128D84A518CFF00FFFF00FFFF00FF
      A7756BFFFBF033A6FF4078AD8E675EAC7F7597645EAC7D6FCAA083EDC695EBC2
      8AEFC583A46769FF00FFFF00FFFF00FFFF00FF01679999FFFF99FFFF99FFFF99
      FFFF99FFFF99FFFF99FFFF016799FF00FFFF00FFFF00FFFF00FFC7F5F3016799
      016799016799C7F5F34F00A7C7F5F3016799016799016799C7F5F34F00A7C7F5
      F3016799016799C7F5F3C26D43E6E6E6E8E8E8E9E9E9EBEBEBF1B7B1F1B7B1F1
      B7B1F1B7B1F1B7B1F5F5F5F7F7F7F8F8F8FAFAFAC26D43FF00FFC26D43E6E6E6
      E8E8E8E9E9E9EBEBEBCC4D40CC4D40CC4D40CC4D40CC4D40F5F5F5F7F7F7F8F8
      F8FAFAFAC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1275171CA7161DA3191D
      9E1B1E981E1F93211F8E23056806848484A8A8A8FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF0A34EF0628C1949494FF00FFFF00FFFF00FF0A34EF0628C19494
      94FF00FFFF00FFFF00FFFF00FF1094C6189CCE4AC6EF84DEF794EFFF73D6EF5A
      BDCE52B5CE29C6EF00D6FF00CEFF08D6FF08CEF708B5E7FF00FF6C6A6ACACACA
      8E8C8CEFEEEEFFFEFEFBFAFAE3E0E1DEDEDEDEDDDDCFCECEBDBCBCADABAB8B89
      895856567A7878757373FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0357060D8223
      17B6410EA92D05A013049F0D07A01182D589FFFFFFF4FCF640BC4A039E0C039E
      0C03A50C037B08014203FF00FFFF00FF319CBD61C9ED78C1D7E6FFFFE4FFFFE5
      FEFFE5FEFFE8FFFFEBFFFFE2FBFE9CE1FA75C1D7187EA1FF00FFFF00FFFF00FF
      319CBD61C9ED78C1D7E6FFFFE4FFFFE5FEFFE5FEFFE8FFFFEBFFFFE2FBFE9CE1
      FA75C1D7187EA1FF00FFFF00FFFF00FFA7756B9A99E40B27C50005B74C384E50
      3B414830451210A7A07E67E2BD8EEBC28AEFC583A46769FF00FFFF00FFC98653
      E4C7B3E5CAB8E7CDBCE8CFC0E9D2C4E6D1C4D9C6BBDACBC1EBDCD2F1E2DAF2E5
      DEF4E8E2C87735FF00FFFF00FFC98653E4C7B3E5CAB8E7CDBCE8CFC0E9D2C4E6
      D1C4D9C6BBD5C6BCD7C8BFD7CAC3D8CDC6DACFCAB36A2FFF00FFFF00FFC98653
      E4C7B3E5CAB8E7CDBCCFB9AC84776F279D68269C6563C46A62C46960C3675DC1
      625BC0604B9B27FF00FFFF00FFB87B4CCCB2A0CDB5A5CFB7A8CFB9ACD0BCAFD2
      BEB3D3C1B6DACBC1EBDCD2F1E2DAF2E5DEF4E8E2C87735FF00FFFF00FFFF00FF
      18B12D3CCD6731C65A21BB470CA616FF00FFFF00FFC98550E6CCBBE1C1ACDBB5
      9AC87C3DFF00FFFF00FFFF00FFFF00FFFF00FFC87C3DDBB59AE1C1ACE6CCBBEC
      D6C9F1E1D8F6ECE7FBF7F5CA8E62CB926AFF00FFFF00FFFF00FFCA895A005184
      DDB9A1E0C0AB35A7775EBE6159BC5A668C53FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC3399330099330074875EA4CC919F612C99330082
      7A4C99330092501E924D1C973E0C9AA36DB4E09FBBE9A566CC33FF00FFFF00FF
      D9ABA3DBB7B4D0B3BACA8184CD6B6CB871718C7C7CFF00FFFF00FF7779782E57
      321F873515721BFF00FFFF00FFFF00FF00730845D27040CE683ACA5F32C4532D
      C04A28BC4223B83A1CB22E15AD230EA717007308FF00FFFF00FFFF00FFFF00FF
      00730845D27040CE683ACA5F32C4532DC04A28BC4223B83A1CB22E15AD230EA7
      17007308FF00FFFF00FFFF00FFFF00FF3B813B9EDC9F93D89587D28A7CCD7F39
      AC4E23A8423CB75058C05B51BD5441B74433B236145C14FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF0274AC57CAF80274AC99E2FB5ED1FA5ED1FA5ED1FA5E
      D1FA5ED1FA5FD1FA5ED1F85ED1F82CA1CEBAEBEF0274ACFF00FFFF00FFFF00FF
      C6A49AFFFFFCFAF0E6F8EADAF7E5CFF4E0C5F2DABAF2D5B1F0D0A7EECB9DEBC7
      93F2C98CA46769FF00FF0274AC70E3FB5CD1EF1184B6FCFFFFB8F4FEBAF4FEBA
      F4FEBAF4FEB8F4FE046B0B25AA42046B0BD4F7FACAF3F70274ACFF00FF7F2B28
      BF4748B84545BA4C4CBD5757BB5756B64E4EB44949B44949BD5251BB4B4CB542
      42BF4A4A7F2B28FF00FFFF00FF7F2B28BF4748B84545BA4C4CBD5757BB5756B6
      4E4EB44949BD5251BD5251BB4B4CB54242BF4A4A7F2B28FF00FFFF00FFFF00FF
      7F2B28E77B7BE77B7BE77B7BE78484D6737376100F0094000084000084000084
      00F7A5A57F2B28FF00FF319CBD63CEFF188CB59CFFFF94FFFF94FFFF94FFFF94
      FFFF94FFFF94FFFF94FFFF8CF7FF52BDE79CFFFF188CB5187B9CFF00FF317731
      A6E4B335FF161FE30619C80219C80271D6FF71D6FFCBAFACB097946254536ED3
      830E570EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0005B60006C700
      06C70006CE0005B4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B635AFFFFFF
      E73900D63900CE3100B53100B53100A52900A52900FFFFFF847B7BDEA5A56352
      52FFFFFF635A5A736352FF00FFFF00FFFF00FFFF00FF3BABFFA1CAE7AD8679A9
      8373E0CFB1FFFFDAFFFFDDFCF8CFCCB29FA1746BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFB54A10B54A10D08B66FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7EFE7F7F7F7C66331C65A29CE6B31CE5A21CE8C6BF7
      E7DECE6B39C65A21CE6331C65218C67B52F7FFFFDECEC68C847BBA5A15EED5C1
      CCE0DD188CB5ADCFD0188CB5DDCAAC5F7573B99564D68A43956A3FC9BBAEFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF418641ABE6B89EE2AD90DDA11B631BFF
      00FF418641ABE6B89EE2AD90DDA11B631BFF00FFFF00FFFF00FFFF00FFFF00FF
      D3F1D3ABE6B89EE2AD90DDA19CE09CFF00FFD3F1D3ABE6B89EE2AD90DDA19CE0
      9CFF00FFFF00FFFF00FFFF00FF195FAD398BD7F7FFFFFFFFFFEFF7FFFFFFFFFF
      FFFF5A9EDE217BCF217BCF2272BC195FAD1961A511487BFF00FF4E914EE2F6E8
      75B1FB568FFF548DFF7299F3CDBAB6837B72367E39C5A9A68C78766CA674226F
      2645888457B87317671B319CBD86EEFF57C1DF2396BC2396BC2596BC2F97BC2F
      97BC2F97BC2D96BB1D8EB52F97BC65C1E95D929B64AF711B6A1F408540BAF1BB
      4FEB4B29C2483C87CC2C80C4099FF9119FF42C89DF3D5F5E1E4726427F5F4B97
      6E4B9E6E53BB6F1465182E742E8AE69129F00D19CA0254C8B26BCBF56ACBF465
      C1E95D929B64AF711B6A1F897061D1AA97EDC4A9D6A994FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF8729058A35118B2803912C028F2A038F2A
      038B2803FF00FFFF00FF98665DFFE3C7DD903DDD903DDD903DBB8369FFF7EFDD
      903DDD903DDD903DDD903DDD903DDD903DFEDAB8734241FF00FFFF00FF004B82
      4BCBEF45C7ED40C2E9B4776CFBF0EBDD9341DD903DDD903DDC903DDC903CDC91
      40F4E9DA734241FF00FFFF00FFCA4F00F67A1CF06B0CEB6204E15900D85802D3
      5C09CB5C0BC55B0EDDA674FEFEFEAD4300993C004E1F00FF00FFCA4F00F36807
      E96004DE5A01D45300EAB788B4776CFBF0EBDD9341DD903DDD903DDC903DDC90
      3CDC9140F4E9DA734241FF00FFFF00FFFF00FFFF00FF3BABFFA1CAE7A5948CB5
      9C8CF7E7CEFFFFD6FFFFD6FFFFD6E7DEBDCEADA5FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF3BABFFA1CAE7A5948CB59C8CF7E7CEFFFFD6FFFFD6FFFFD6E7DE
      BDCEADA5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3BABFFA1CAE7A5948CB5
      9C8CF7E7CEFFFFD6FFFFD6FFFFD6E7DEBDCEADA5FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF3BABFFA1CAE7A5948CB59C8CF7E7CEFFFFD6FFFFD6FFFFD6E7DE
      BDCEADA5FF00FFFF00FFFF00FFA7756BFFFFFCFAF0E6007000007000007000F2
      DABAF2D5B1F0D0A7EECB9DEBC793F2C98C80504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF7F2B28C24A4AC24A4AFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC24A4A7F2B28FF00FF7F2B28C2
      4A4AC24A4AFF00FF7F2B28C24A4AC24A4AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC24A4AC24A4A7F2B28FF00FFC24A4AC24A4A7F2B28FF00FFC24A4A7F2B
      28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC
      908FC24A4A7F2B28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF599B59E6FDFB
      ABDEF693CBEE89C7E583C6D9AAC4A4397E39F0C8AAEFC7A7EFC5A4EEC2A0EDC0
      9DEDBE9AECBC98B14809FF00FFABABABCDC1B2FFF6C6FFFFDEFFFFD3FFFFCDAD
      A7888C7660FFD9A7FFDCAAFFE9C9F1E8D1AB98937F7575FF00FF0274AC5ED3FA
      0274ACA1E9FC69DCFA6ADCFA69DCFB69DCFB6ADCFB69DCFB69DCFA6ADDFB2FA6
      CFA9EEF30274ACFF00FFBA7F4DFBD0BBD78A5CC17C53AE6F49A168468B5E3AFF
      00FFFF00FFBA7F4DFBD0BBD78A5CC17C53AE6F49A168468B5E3ACF8F5AFFFFFF
      FFDCC5D68F60C37E54B4744DB26949E3BCA8E3BCA8CF8F5AFFFFFFFFDCC5D68F
      60C37E54B4744DB26949CF8F5AFFFFFFFFDCC5D68F60C37E54B4744DB2694923
      96BC2396BC2B97BC2F97BC2F97BC2F97BC178AB32F97BCB26949FF00FFFF00FF
      D3F1D3ABE6B89EE2AD90DDA19CE09CFF00FF509450ABE6B89EE2AD90DDA11B63
      1BFF00FFFF00FFFF00FFFF00FFDA732EDE8848E5A06DF8E2D2F2CFB8E28D54EC
      B08CFBF0EBF8DCCEF6C8B6ED906BE35B25D55117B24D10FF00FF999796B4B4B4
      5656568A6136A968244C4C4C848484B7B7B760BBDD5AB5D45AB5D4CDCDCDC8C8
      C8A9A9A9FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0050F40076E400A6E400
      B3E6008BE4005BE42D51ABFF00FFFF00FFFF00FFFF00FFFF00FF7F7F7F9D9D9D
      C5C5C5B1B1B1A8A8A8A2A2A2C1C1C11C2CB81323AFBFBFBFB4B4B49C9C9CB4B4
      B48C8C8C4A4A4AFF00FF000000000000000000C1C1C1BEBEBEBCBCBCBABABA24
      34C01C2CB8B1B1B1AEAEAEABABABA9A9A9000000000000000000CE6300FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7E7FFEFD6FFE7C6FFDEB5FFD6ADFFD6
      ADFFD6ADCE6300FF00FFC47128BF5900CA6400D06B00C873009A80004E8C005A
      8900A87C00CF7000A16D006A690038610022640C4F6627895624ED6829C94800
      CF5400D66500B26E14AD894F8D6F3F8F71417A5E2F8E6B347C5D2D368E740DFD
      E909FFF706FFFD2A9390818FF91832F5142EF5818FF9FF00FFFF00FFFF00FFFF
      00FF5163EA1630F4142EF52039F3142EF5132CEF303FAFFF00FFFF00FFFF00FF
      A7756BFFFFFCFAF0E6AD8A88B78F84D8BAA5EED5B6D7B298B58877CBA083EBC7
      93F2C98CA46769FF00FFFF00FFFF00FFFF00FF01679999FFFFE8AEA8E8AEA8E8
      AEA8E8AEA8E8AEA899FFFF016799FF00FFFF00FFFF00FFFF00FFC7F5F3C7F5F3
      016799C7F5F3C7F5F34F00A7C7F5F3016799C7F5F3C7F5F3C7F5F34F00A7C7F5
      F3C7F5F3C7F5F3016799C26D43E5E5E5E6E6E6E8E8E8E9E9E9EAA59EEAA59EEA
      A59EEAA59EEAA59EF4F4F4F5F5F5F7F7F7F8F8F8C26D43FF00FFC26D43E5E5E5
      E6E6E6E8E8E8D25E52D25E52D25E52D25E52D25E52D25E52D25E52F5F5F5F7F7
      F7F8F8F8C26D43FF00FFFF00FFFF00FFFF00FFFF00FFCEA58CAD7342AD7342AD
      7342AD7342AD7342734A29FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFAD7342FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF15781B1BB1101CAB14FFFFFFFF
      FFFF1D9E1B1E981E1F93211F8E23056806989898FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF0A35F20628C1848484A8A8A8FF00FFFF00FF0A35F20628C18484
      84A8A8A8FF00FFFF00FFFF00FF188CBD29ADDE39BDE76BD6F794C6D694949494
      949494949494949442BDDE00D6FF10D6FF189CCE089CCEFF00FF6C6A6A8E8C8C
      FFFFFFFEFCFCFAFAFAD5D4D5989193A09899B2ABACC4C0C1D7D7D7D8D8D8C7C6
      C6B7B6B6918F8F6C6969FF00FFB36563FF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCE9C9CFF00FF03570617A341
      18B54A11AB3406A011039E0C049F0D039E0C74D07DFCFEFCF3FBF43EBC48039E
      0C03A10C03960A014203FF00FFFF00FF319CBD86EEFF57C1DF2396BC2396BC25
      96BC2F97BC2F97BC2F97BC2D96BB1D8EB52F97BCBC7545FF00FFFF00FFFF00FF
      00840086EEFF57C1DF2396BC2396BC2596BC2F97BC2F97BC2F97BC2D96BB1D8E
      B52F97BCBC7545FF00FFFF00FFFF00FFA7756BFFFFFCF9EFE5352DA50006C124
      23AB0005B5846E76A37B6BC79D80EBC793F2C98CA46769FF00FFD09B74E0BEA8
      E2C3AFE4C7B3E5CAB8E7CDBCE8CFC0D5C0B3998B829A8D85D9CAC0F0E0D6F1E2
      DAF2E5DEF4E8E2CD8E5DD09B74E0BEA8E2C3AFE4C7B3E5CAB8E7CDBCE8CFC0D5
      C0B3998B82857A73867D77887E7988807B89817D8A8380745035D09B74E0BEA8
      E2C3AFE4C7B3E5CAB8CFB7A883756C2BA06D299F6A64C46B63C46A62C46960C3
      675DC1625BC0604DA233BE8E6A927C6D806E6381706581726882746A83756C84
      776F8578719A8D85D9CAC0F0E0D6F1E2DAF2E5DEF4E8E2CD8E5DFF00FFFF00FF
      FF00FF1BB3333CCD6731C65A10AA1EFF00FFFF00FFC98856ECD6C9E6CCBBC981
      48FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC97F43C98148C9834CC98550C9
      8856CA8A5ACA8C5ECA8E62CB9066CB926ACB926AFF00FFFF00FF005184004AA6
      DAB499DDB9A13EAE8163C1695CB95AFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC3399330099330074875EA4CC919F612C99330082
      7A4C99330099330093451479885E94B883B0DB9CBBE8A566CC33FF00FFFF00FF
      E5984BF9AB36E9A769D48A80C25D6393545855555577797832603F228D3929B3
      4A29913BFF00FFFF00FFFF00FFFF00FF00730800730800730800730800730800
      73080073080073080073081CB22E007308FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF00730845D2700073080073080073080073080073080073080073080073
      08007308FF00FFFF00FFFF00FFFF00FF3D833DA7DFA89EDC9F94D89676C67B10
      8F2D10963124A8425BC25F54BF5749BB4C3EB641175F17FF00FFAD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD73420274AC5ED3FA0B81B782D5EF79E0FB6ADCFA69DCFB69
      DCFB6ADCFB69DCFB69DCFA6ADDFB2FA6CFBAEBEF097EB4FF00FFFF00FFFF00FF
      CBA99EFFFFFFFEF7F2FAEFE6F8EAD9F7E3CFF6E0C5F2DABBF2D4B1F0D0A7EECC
      9EF3CE97A46769FF00FF0274AC7AEBFE7AEBFC0A7FB50274AC0274AC0274AC02
      74AC0274AC046B0B38CE6547E77F29B44A046B0B0274AC0274ACFF00FF7F2B28
      A33B39B1605DC68684CB918FCC9190CC908FCB8988CB8988C98988CB9391CC96
      96BD4B4C7F2B28FF00FFFF00FF7F2B28A33B39B1605DC68684CB918FCC9190CC
      908FCB8988C98988C98988CB9391CC9696BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B28F7848CEF8484EF9494FFDEDEDE8C8C76100F0084000084000063000063
      00F7A5A57F2B28FF00FF319CBD6BD6FF188CB59CFFFF9CFFFF9CFFFF9CFFFFA5
      F7FF9CFFFF9CFFFF9CFFFF9CFFFF63CEFF9CFFFF188CB5187B9CFF00FF387D38
      BCEBC639FF1A20E70619CA0219C802FFFFFFCFB2AFCDB0ADB0979462545373D5
      88115911FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0006C100
      05C10006DAFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B635AFFFFFF
      E73900D63900CE3100B53100B53100A52900A52900FFFFFF847B7BDEADAD6352
      52FFFFFF635A5A735A52FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC0917DFC
      E9ACFFFFCCFFFFCFFFFFD0FFFFDEFFFFFAE3D3D1996965FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFB54A10B54A10B8531BFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7EFE7EFDED6C65A21CE6331CE6331CE5A21C6846BFF
      FFFFEFAD94C64A10CE6331CE6329C65A29F7EFEFEFDED68C847BB95914F3E1D3
      B1D8DFB9D8DAFAECD9188CB5D4D9C9D8C7ACDEAC77F4CA9EF4C391D39557B984
      51CEBCABFF00FFFF00FFFF00FFFF00FF5094504186413278322A712A236A23FF
      00FF5094504186413278322A712A236A23FF00FFFF00FFFF00FFFF00FFFF00FF
      E9F8E9D3F1D3BDEABDB2E6B2A7E3A7FF00FFE9F8E9D3F1D3BDEABDB2E6B2A7E3
      A7FF00FFFF00FFFF00FFFF00FF2272BCA5CBEFFFFFFFFFFFFF63A3DFD6E5F8FF
      FFFFDFEDFF217BCF2277C6226EB41961A51961A5104784FF00FF5B9D5BF0FDFB
      C4EBF4A9D8EA9CD3E08DCED493D9B5A5C7A338803BC5A9A68C787671A6782672
      295D929B64AF711B6A1F319CBDB8FAFF92FAFF92FAFF92FAFF30763090E7952D
      F4101BD3031EC81269D4EA78D2F7BAAAAB87797968A3701E6C22498D49DDF5E1
      A6D0E5538AFF4A7EFF396EA7386F9202B7FF00ACFF1D8DE12448442447552D58
      6A3E7B7757B77317671B30763090E7952DF4101BD3031EC81269D4EA78D2F7BA
      AAAB87797968A3701E6C22886858BEA398C0AAA6DEB39DFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF7D635A745B518F43238B28038B28038B28
      03FF00FFFF00FFFF00FFA06C5FFEE9D3FFE5CAFFE1C1FFDCBAC48B6CFEFAF7FE
      F7F0FEF4EAFEF2E3FEEDDCFEE9D4FEE5CBFEE1C2734241FF00FFFF00FF004B82
      52D0F34CCCEF47C7EEB87B6EFEF4F0FEF4EBFAEBDDFAEADAF8E7D7F8E9D8F7EB
      DDE1DAD3734241FF00FFFF00FFCA4F00F7913CF07516EF6707ED6202F4BB87DC
      5600D45300CB4F00C55709FEFEFEB647009D3D004F1F00FF00FFCA4F00F67A1C
      F06B0CEB6204E15900D85802B87B6EFEF4F0FEF4EBFAEBDDFAEADAF8E7D7F8E9
      D8F7EBDDE1DAD3734241FF00FFFF00FFFF00FFFF00FFFF00FFCEB5B5D6B5A5FF
      EFC6FFFFD639B552218429FFFFDEFFFFEFF7F7EFB58C8CFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFCEB5B5D6B5A5FFEFC6FFFFD6FFFFD6FFFFD6FFFFDEFFFF
      EFF7F7EFB58C8CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEB5B56F6D6B71
      706D71706E70706E70706D70706D6F6F6E6F6E6E6B6868FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFCEB5B5D1AE9A97523A8C4329A1644DA1662E97543D8C43
      2A9F6253B58C8CFF00FFFF00FFBC8268FFFFFFFEF7F2FAEFE6F8EAD9F7E3CFF6
      E0C5007000007000007000EECC9EF3CE9780504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFC24A4AC24A4AC24A4AC24A4AFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC24A4A7F2B28C24A4AC24A4AC2
      4A4AC24A4AC24A4AC24A4AC24A4AC24A4AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC24A4AC24A4AC24A4AC24A4AC24A4AC24A4AC24A4AC24A4AC24A4A7F2B
      28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC
      908FC24A4AC24A4AC24A4AFF00FFFF00FFFF00FFFF00FFFF00FFDA732E599B59
      4C8F4C3F843F347A342E742E296F29B4AF85F0CAADF0C8AAEFC7A7EFC5A4EEC2
      A0EDC09DEDBE9AB24D10FF00FFB2B2B2CFC1B3A0CAF3EDF6FDFFFFE7FFFFD899
      998C4545456D6455968870F6E0BD8899E089808B847979FF00FF0274AC68DAFB
      0274ACA7EFFC74E5FB74E5FB74E5FB74E5FC74E5FC74E5FB74E5FC74E5FB33A9
      CFACF0F40274ACFF00FFCF8F5AFFFFFFFFDCC5D68F60C37E54B4744DB26949E3
      BCA8E3BCA8CF8F5AFFFFFFFFDCC5D68F60C37E54B4744DB26949DE9D65DA9962
      3184FF3184FF3184FFCB8156BF7551A5714DA5714DDE9D65DA9962FFFFDBE69F
      6AD88F5FCB8156BF7551DE9D65DA9962FFFFDBE69F6AD88F5FCB8156BF7551A5
      714DA5714DDE9D65DA9962FFFFDBE69F6AD88F5FCB8156BF7551FF00FFFF00FF
      E9F8E9D3F1D3BDEABDB2E6B2A7E3A7FF00FF5094504186413278322A712A236A
      23FF00FFFF00FFFF00FFFF00FFDA7938E08C4EE19055E6A576F9E9DDF1C7ABE0
      834AECAC86ECA582EFB193FAE9E2F0AD90D46224B15015FF00FF919191BDBDBD
      66666652504D55524D5D5D5DA8A8A8B8B8B8DADADA39ACE33CABED218FB9CBCB
      CBACACACFF00FFFF00FFFF00FF159CF10F7BCF258AF1077EF7008BE4FF00FFFF
      00FFFF00FF0672C62D51AB2543913151A3FF00FFFF00FFFF00FF8484849E9E9E
      C8C8C8B2B2B2A9A9A9A3A3A3A2A2A22434C01C2CB8C2C2C2B7B7B79D9D9DB6B6
      B68D8D8D4E4E4EFF00FF000000FFFFFF000000C3C3C3C1C1C1BEBEBE3040CC2A
      3AC62434C0B3B3B3B1B1B1AEAEAEABABAB000000FFFFFF000000CE6300FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7E7FFEFD6FFE7C6FFDEB5FFD6
      ADFFD6ADCE6300FF00FFC86C13CA6400CF6F00AD7E008D8C02B29100B29501D8
      9200F18B00E982008D7D001B7A00006F00086600346312855714DA2209B21900
      B41D00B12200985A2DA28659897048B89D719E8254755A2D80612E4C694B01F9
      F900FFFF00FFFF0A999E818FF9142EF5122CED818FF9FF00FFFF00FFFF00FF50
      62E9142EF3142EF5485CF2818FF91831F5132DF31629BEFF00FFFF00FFFF00FF
      BC8268FFFFFFFEF7F2AF847FDAC0B4F7E3CFF6E0C5FFFFF4D7B198AC7D6FEECC
      9EF3CE97A46769FF00FFFF00FFFF00FFFF00FF01679999FFFF99FFFF99FFFF99
      FFFF99FFFF99FFFF99FFFF016799FF00FFFF00FFFF00FFFF00FFC7F5F3C7F5F3
      016799C7F5F3C7F5F34F00A7C7F5F3C7F5F3016799C7F5F3C7F5F34F00A7C7F5
      F3C7F5F3C7F5F3016799C26D43E2E2E2E5E5E5E6E6E6E8E8E8E4938AE4938AE4
      938AE4938AE4938AF2F2F2F4F4F4F5F5F5F7F7F7C26D43FF00FFC26D43E2E2E2
      E5E5E5D86F64D86F64D86F64D86F64D86F64D86F64D86F64D86F64D86F64F5F5
      F5F7F7F7C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA58CAD7342AD
      7342AD7342734A29FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFCEA58CB57B4A734A29FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF177A1D1AB50E1BB110FFFFFFFF
      FFFFFFFFFF1D9E1B1E981E1F9321076A09989898FF00FFFF00FFFF00FFFF00FF
      FF00FF1344FF0A35F60629C205219E989898FF00FF1344FF0A35F60629C20521
      9E989898FF00FFFF00FFFF00FF1894BD21ADDE31B5E75ACEF7949494EFE7E7B5
      B5B5ADA5A5E7B5B59494945ADEFF9CF7FFB5EFFF399CBDFF00FFFF00FF6C6A6A
      6C6A6AEDEBEBB1A6A77A6F728A83889692959690919D97989A93959E9899BBBA
      BAD1D1D1C2C2C26C6A6AFF00FFB36563B66C6ABA7472BE7B7AC18181C48787C7
      8D8DCA9393CD9899CE9C9CCE9C9CCE9C9CCE9C9CCE9C9CFF00FF06680D21B151
      1EB751BFEDCFBAEAC6B7E9C2B8EAC5B5E9C2B7E9C1F6FCF7FFFFFFEEFAEF54C5
      5E03A00C039F0C014A04FF00FFFF00FF005184B8FAFF92FAFF92FAFF92FAFF92
      FAFFB95813F0D1B9F0E2CD2190B6E8D4B6F9D1ABC38255FF00FFFF00FF008400
      319CBDB8FAFF92FAFF92FAFF92FAFF92FAFFB95813F0D1B9F0E2CD2190B6E8D4
      B6F9D1ABC38255FF00FFFF00FFFF00FFBC8268FFFFFFF8F1EC936F6B0006C100
      06CE8A7E6F9D9D96A88A77A37769EECC9EF3CE97A46769FF00FFCA8959DEBBA3
      E0BEA8E2C3AFE4C7B3E5CAB8E7CDBC168F480F8A3C857871D3C1B6EEDDD2F0E0
      D6F1E2DAF2E5DEC87838CA8959DEBBA3E0BEA8E2C3AFE4C7B3E5CAB8E7CDBC29
      9E66259C61239A5C1F97571C945119914C158F46128C413E9111CA8959DEBBA3
      E0BEA8E2C3AFE4C7B3CDB5A582746A30A3722EA26E65C46E64C46B63C46A62C4
      6960C3675DC1624D9D2B58AA46259C61239A5C1F97571C945119914C158F4612
      8C410F8A3C857871D3C1B6EEDDD2F0E0D6F1E2DAF2E5DEC87838FF00FFFF00FF
      FF00FF1BB33347D4743CCD6714AD25FF00FFFF00FFCA8A5AF1E1D8ECD6C9C983
      4CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF005184004EAE
      D58D86D8AE9146B48C005B94FF00FFFF00FF62793061782F5E772D5D762C5B76
      2A3C782EFF00FFFF00FF66CC3399330099330073855C9CC38A9D612C99330082
      7A4C993300983301973A08888B5B7E9D6F93B782AED99A65CA32FF00FFDBC0C1
      EEA547FFB122FFB732FFBD4CEF9935BF6B4551544823984B2CBD5238C55B4A8F
      55FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FF007308007308FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF007308007308FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF408640B0E3B1A6DFA79BDB9C8AD28D30
      9A43108F2D39AB4E6EC97160C46254BF5749BB4C1A621AFF00FFAD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD73420274AC68DAFB2BA4D14AB1D796EBFB74E5FB74E5FB74
      E5FC74E5FC74E5FB74E5FC74E5FB33A9CFBAEBEFBAEBEF0274ACFF00FFFF00FF
      CFAC9FFFFFFFFFFEFCFCF6F0FAEFE6F7EADAF6E3CFF4E0C5F3D9BBF3D4B0F0D0
      A6F6D3A0A46769FF00FF0274AC83F2FE82F3FE82F3FE83F2FC83F3FE82F3FE83
      F2FE046B0B2DC0513FDC6E3ED86E46E57B28B04A046B0BFF00FFFF00FF7F2B28
      BD4B4CF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      F7BD4B4C7F2B28FF00FFFF00FF7F2B28BD4B4CF7F7F7F7F7F7F7F7F7F7F7F743
      74FF4374FFF7F7F7F7F7F7F7F7F7F7F7F7BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B28F7848CEF8484F79C9CFFFFFFDE8C8C76100FFFCEADF7B584F7B584F7B5
      84F7A5A57F2B28FF00FF319CBD7BDEFF3184FF3184FF3184FFF7FFFFF7FFFFF7
      FFFFF7FFFFFFFFFFFFFFFFFFFFFF84D6F7F7FFFF188CB5187B9CFF00FF3F843F
      CBEFD23AFF1A22EC0819CB024576FF4576FFCFB2AFCDB0ADB0979462545378D6
      8C135C13FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0005B60006D700
      06CE0006DA0006E9FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B635AFFFFFF
      E73900D63900CE3100B53100B53100A52900A52900FFFFFF847B7BDEB5B56352
      52FFFFFF635A5A735A52FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB08978FA
      D192FEF4C2FFFFD0FFFFDAFFFFF6FFFFFCFFFFFCB69384FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFB54A10B54A10B54A10B54A10FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7EFE7F7DECECE5A21CE6331CE6331CE6329C65221CE
      B5A5FFFFFFE79C7BC65218CE6329C65A29F7EFEFEFDED68C847BBC5E1AF4E6DD
      FEF6EEFEF3E7FEF0E0188CB58EC5CEFDE5CBF3D0ABBE7034D9AB7DF2C79CE5B9
      8AA090865D64A5A8AAC8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF2277C62277D6227FD6217BCF217BCF4288D6F7
      F7FFFFFFFF9CC3E82272BC195FAD1961A51961A5104784FF00FFFF00FF5B9D5B
      4E914E438843397E393076302C722C286E28407999D3B5B29A83817AAC802B75
      2E87797968A3701E6C22FF00FF319CBDE1FFFFE1FFFFDBFFFF387D38A5EEA62F
      F6121CD7031BC212DBEEEACEB1AEC5A9A68C78766CA674226F26529552EBFBF3
      B2E6F773B4FF66A5FF4786D0438FD001DDFF1FF8FF15C1FF2D92DF4E7BBD3463
      784B767D61AA6E1B6A1F387D38A5EEA62FF6121CD7031BC212DBEEEACEB1AEC5
      A9A68C78766CA674226F26836B5E758CB87492DDC6ABA1FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF756257977975756257756257FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFA77463FEEEDDDD903DDD903DDD903DCC9370FEFEFCFE
      FBF8FEF8F3FEF6EDFEF2E6E5D5D0C6B1AFA79395734241FF00FFFF00FF004B82
      59D5F654D1F34FCFF0BD816FFEF6F2FFFFFFFEFFFEFBF8F7FAFAF7A0675BA067
      5BA0675BA0675BFF00FFFF00FFCA4F00F79F54F07B1FEF6A0AF7BA81FEFEFEE9
      5B00DE5700D45300DE8841FEFEFEC14B00A54100582300FF00FFCA4F00F7913C
      F07516EF6707ED6202F4BB87BD816FFEF6F2FFFFFFFEFFFEFBF8F7FAFAF7A067
      5BA0675BA0675BA0675BFF00FFFF00FFFF00FFFF00FFFF00FFC6948CF7DEB5F7
      D6A5FFF7CE39B552218429FFFFEFFFFFF7FFFFFFDED6BDFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFC6948CF7DEB5F7D6A5FFF7CEFFFFD6FFFFDEFFFFEFFFFF
      F7FFFFFFDED6BDFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC6948C716F6DED
      AD6DF0BA7CF0BD7FF0BD82F0BD89F0BD8CF0BD8F6D6D6BFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFC6948CF7DEB5EBC6947F2B28FFFFD6B17F6DF2EAD57F2B
      28FFFFFFDED6BDFF00FFFF00FFBC8268FFFFFFFFFEFC007000CADABAF7EADAF6
      E3CFC5CE9F007000007000F0D0A6F6D3A080504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFCC908FC24A4AC24A4AFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC24A4A7F2B28FF00FFCC908FC2
      4A4AC24A4AFF00FFCC908FC24A4AC24A4AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC24A4AC24A4ACC908FFF00FFC24A4AC24A4ACC908FFF00FFC24A4A7F2B
      28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC
      908FC24A4AC24A4AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDA7938F4D7C2
      F4D6C0F3D4BDF2D2BAF2D0B7F1CEB4F1CDB2F0CBAF7F4A45F0C8AAEFC7A7EFC5
      A4EEC2A0EDC09DB15015FF00FFB8B0A7CEC8C2FEEEDFFFFFFFFFFFF6FFFFE3FF
      FFD9BDBD9F5F564AEFCFA1FFEEC9E5D5B29F9692827E7EFF00FF0274AC70E3FB
      0274ACFFFFFFBAF4FEB8F4FEBAF4FEBAF4FEBAF4FEB8F4FEBAF4FEB8F4FE83C9
      E0D4F7FA0274ACFF00FFDE9D65DA9962FFFFDBE69F6AD88F5FCB8156BF7551A5
      714DA5714DDE9D65DA9962FFFFDBE69F6AD88F5FCB8156BF7551FF00FFFFBD83
      3184FF42B5F73184FFC8906AE3BCA8FF00FFFF00FFFF00FFFFBD83D89760D499
      70C8906AC8906AE3BCA8FF00FFFFBD83D89760D49970C8906AC8906AE3BCA8E1
      FFFFB7E7EF36A6D2FFBD83D89760D49970C8906AC8906AE3BCA8FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFDC8240E09258E1945BE19257E39C67FBEFE5EC
      B48EDC7334DD6E2BDD6523E38551FCEFE6D9834FAE551AFF00FF8B8B8BC5C5C5
      939393666666656565868686D9D9D974BCD53DA9CF51BFEE61C6FE2DA3DE178E
      C0AEAEAEFF00FFFF00FFFF00FF004BE4004BE4004BE40066E400CAEEFF00FFFF
      00FFFF00FF008BE4004BE4004BE4004BE43B6ED5FF00FFFF00FF888888A1A1A1
      CDCDCDB3B3B3AAAAAAA3A3A33040CC2A3AC62434C0C2C2C2B8B8B89D9D9DB8B8
      B88F8F8F525252FF00FF000000000000000000C5C5C5C3C3C3C1C1C1BEBEBEBC
      BCBCBABABAB6B6B6B3B3B3B1B1B1AEAEAE000000000000000000CE6300FFFFFF
      948C8C948C8C948C8C948C8CFFFFFFFFFFFFFFFFFFFFF7E7FFEFD6FFE7C6FFDE
      B5FFD6ADCE6300FF00FFD07616CD6C00A98306459C1437A71F81A820BDA81DFE
      A314FD9A06F89200D488003F84001F76002569002960036B641CD90A24AD021B
      AE0220A90126A35F4C9D855D8F7955C6AF8BB99E72A887528F6D364F6C5100DD
      FB00E4FF00E8FF0A93AA5E70F7142EF51029E08A93D7FF00FFFF00FF5061E914
      2EF3142EF54E61F7FF00FFFF00FF273FF4142EF51327C3FF00FFFF00FFFF00FF
      BC8268FFFFFFFFFEFC976560F6E9E0F7EADAF6E3CFFFFFEBEFD4B797645EF0D0
      A6F6D3A0A46769FF00FFFF00FFFF00FFFF00FF01679999FFFFE8AEA8E8AEA8E8
      AEA8E8AEA8E8AEA899FFFF016799FF00FFFF00FFFF00FFFF00FFC7F5F3016799
      016799C7F5F3C7F5F34F00A7C7F5F3C7F5F3C7F5F3016799C7F5F34F00A7C7F5
      F3C7F5F3016799C7F5F3C26D43E1E1E1E2E2E2E5E5E5E6E6E6DE8177DE8177DE
      8177DE8177DE8177F0F0F0F2F2F2F4F4F4F5F5F5C26D43FF00FFC26D43E1E1E1
      E2E2E2E5E5E5E6E6E6DE8177DE8177DE8177DE8177DE8177F0F0F0F2F2F2F4F4
      F4F5F5F5C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA58CAD
      7342734A29FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFCEA58CAD7342AD7342AD7342734A29FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF197C1F19B90B1AB50EFFFFFF1C
      AB14FFFFFFFFFFFF1D9E1B1E981E096C0BA8A8A8FF00FFFF00FFFF00FFFF00FF
      FF00FF1547FF0C39FD062AC60522A0989898FF00FF1547FF0C39FD062AC60522
      A0989898FF00FFFF00FFFF00FF108CBD189CCE21ADDE42C6EF949494EFE7E7B5
      B5B5ADA5A5DEB5B59494947BE7FFADEFFF8CCEE74AA5C6FF00FFFF00FFFF00FF
      FF00FF6C6A6ABB897FA7876D8B6F647D67606F62657973798F8B8EA9A3A4CBCA
      CAC1C1C16C6A6AFF00FFFF00FFB36563B66C6ABA7472BE7B7AC18181C48787C7
      8D8DCA9393CD9899CE9C9CCE9C9CCE9C9CCE9C9CCE9C9CFF00FF0874123EBD69
      2ABA5CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFEFCFFFFFFFFFFFFAAE3
      B003A00C039F0C025205FF00FF0051846385E72153A5005184E1FFFFDBFFFFDB
      FFFFB95914E5D8C91E8FB63B9CBB3595B4FAD3AEC6885DFF00FF00840000A521
      FF00FF319CBDE1FFFFE1FFFFDBFFFFDBFFFFB95914E5D8C91E8FB63B9CBB3595
      B4FAD3AEC6885DFF00FFFF00FFFF00FFBC8268FAFAFAD9D8D6271D980006D55A
      58BF0006E7A5A5BF9F8D7A7F544FE8C9A0F5D29FA46769FF00FFCA8A5BDCB89E
      DEBBA3E0BEA8E2C3AFE4C7B3E5CAB81F965418904984776FD2BEB3ECD8CCEEDD
      D2F0E0D6F1E2DAC8793ACA8A5BDCB89EDEBBA3E0BEA8E2C3AFE4C7B3E5CAB82F
      A26E2B9F67279D62249B5D2198581D95521B934D179047419317CA8A5BDCB89E
      DEBBA3E0BEA8E2C3AFCCB2A081726834A67631A47367C56F65C46E64C46B63C4
      6A62C46960C36750A02F5CAE4D2B9F67279D62249B5D2198581D95521B934D17
      9047148D4284776FD2BEB3ECD8CCEEDDD2F0E0D6F1E2DAC8793AFF00FFFF00FF
      FF00FFFF00FF1EB63947D47418B12DFF00FFFF00FFCA8C5EF6ECE7C98856FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF005184004EAE
      005B94FF00FFFF00FF003E9C003E9C296F2948D13F5DBD6F3D788B3F79923E76
      8F366A69397E39FF00FF66CC33993301993300778A5F85A676985E2A99330081
      794C9933009157269265339835039588557E9D6F91B5805EBD2FFF00FFCEB0AD
      F5B24DFFC14AFFBC3BFFA215EE9C2AA78C5445BA6E4DD06F3FA451777978FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FF009C00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF009C00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF438943BAE7BB68C2753BB2546FCA7A8B
      D28E76C67B7CCD7F52B96031AE4B4FBE5954BF571D651DFF00FFAD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD73420274AC70E3FB5CD1EF1184B7FEFFFFB8F4FCBAF4FCBA
      F4FCBAF4FEB8F4FEBAF4FCB8F4FE83C9DEE3FEFEC5EFF60274ACFF00FFFF00FF
      D4B0A1FFFFFFFFFFFFFFFEFCFEF7F0FAEFE5F8EAD9F7E5CEF6DEC4F3D9B8F4D8
      B1EBCFA4A46769FF00FF0274ACFEFEFE89FAFF89FAFE89FAFE8AF8FE8AFAFE04
      6B0B046B0B046B0B046B0B3CD86A2EBF53046B0B046B0B046B0BFF00FF7F2B28
      BD4B4CF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      F7BD4B4C7F2B28FF00FFFF00FF7F2B28BD4B4CF7F7F7F7F7F7F7F7F7F7F7F743
      74FF4374FFF7F7F7F7F7F7F7F7F7F7F7F7BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B28F78C8CF78C8CF78C8CF78C8CDE7B8476100FFFCEADFFD6BDFFD6BDFFD6
      BDF7A5A57F2B28FF00FF319CBD84EFFF3184FF42B5F73184FF188CB5188CB518
      8CB5188CB5188CB5188CB5188CB5188CB5188CB5188CB5FF00FFFF00FF468B46
      3184FF3184FF3184FF4679FF4577FF4576FF4576FFCFB2AFB39A9764555480D9
      93175F17FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0006E50006DA0006D3FF
      00FFFF00FF0006E50006EFFF00FFFF00FFFF00FFFF00FFFF00FF6B635AFFFFFF
      E73900D63900CE3100B53100B53100A52900A52900FFFFFF84847BDEB5B56352
      52FFFFFF635A5A735A52FF00FFFF00FFFF00FFFF00FFFF00FFB08978FEDA97ED
      B478FBEEBBFFFFD3FFFFDCFFFFF4FFFFF4FFFFE2E9DDBCA67B73FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFCD865FBA551FD49775FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7EFE7F7EFE7DE6B31D66B31CE6329C65A21C65218BD
      4A10DECEC6FFFFFFD67342CE5A21CE6B39FFF7F7EFDED68C847BBE6221F6ECE6
      FFF9F5FFF6EEFFF3E7E0E6DE188CB5EFE4D1F7DBBFB26631FF00FFD9B490D9B4
      909694AD2553CD4B57A8FF00FFFF00FF236A231B631B135C130B550B044E04FF
      00FF236A231B631B135C130B550B044E04FF00FFFF00FFFF00FFFF00FFFF00FF
      A7E3A79CE09C91DC9186D9867BD67BFF00FFA7E3A79CE09C91DC9186D9867BD6
      7BFF00FFFF00FFFF00FFFF00FF227FD6297FDF2987DF2277D6227FD6217BCF63
      A3DFFFFFFFFFFFFF5A99CE1966AD1961A51961A5104784FF00FFFF00FFFF00FF
      529552EBFBF3B2E6F773B4FF66A5FF65A5FF6AA6FC9FBFD7A8B19B93BF952E77
      318C78766CA674226F26FF00FFFF00FF319CBD319CBD319CBD408540BAF1BB4F
      EB4B29C2483C87CC4576FFABA2C1C5A9A68C787670A6782672295EA05EFAFDFC
      E6F9F5CEEDE8BEE5DC1AB5FF00EAFF00F5FF00EDFF1CF4FF17D5FF2E9EDC6261
      6C675D5D649D6C1E6C22408540BAF1BB4FEB4B29C2483C87CC4576FFABA2C1C5
      A9A68C787670A6782672298A73649DA6B092B0DDCBB1A5FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF756257C6ABAB756257987A76756257FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFB17D67FEF3E6FEEFDEFEEAD8FFE7CFD59C74FEFEFEFE
      FEFEFEFBFAFEFAF4FEF6EEA0675BA0675BA0675B9D6559FF00FFFF00FF004B82
      5FDAF85BD8F655D3F3C2846FFFF8F6FFFFFFFFFFFFFFFFFFFFFFFFA0675BD792
      55F47A41FF00FFFF00FFFF00FFCA4F00F8A760F28128FAD0A9FEFEFEFEFEFEFB
      E6D1FAD8B7F7D7B7FEFEFEE9AB73CE5000B14500682800FF00FFCA4F00F79F54
      F07B1FEF6A0AF7BA81FEFEFEC2846FFFF8F6FFFFFFFFFFFFFFFFFFFFFFFFA067
      5BD79255F47A41FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDEBDA5FFE7ADF7
      CE94FFF7CE39B552218429FFFFF7FFFFF7FFFFEFF7EFD6C69C94FF00FFFF00FF
      FF00FFFF00FFFF00FFDEBDA5FFE7AD0021A50021A50021A50021A50021A50021
      A50021A5F7EFD6C69C94FF00FFFF00FFFF00FFFF00FFFF00FFDEBDA572706CED
      AA66F0BA7CF0BD82F0BD86F0BD8CF0BD8CF0BD896F6E6DC69C94FF00FFFF00FF
      FF00FFFF00FFFF00FFDEBDA5FFE7ADEBBF857F2B28FFFFDEFFFFE7F2EADC7F2B
      28FFFFEFF7EFD6C69C94FF00FFD1926DFFFFFFFFFFFFCEE7CC007000CADAB8C9
      D7B0007000C6CC9E007000F4D8B1EBCFA480504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFCC908FC24A4AFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC24A4A7F2B28FF00FFFF00FFCC
      908FC24A4AFF00FFFF00FFCC908FC24A4AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC24A4ACC908FFF00FFFF00FFC24A4ACC908FFF00FFFF00FFC24A4A7F2B
      28FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC
      908FC24A4AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDC8240F5D9C5
      F4D7C2F4D6C0F3D4BDF2D2BAF2D0B7F1CEB4B49376AD73427F4A45F0C8AAEFC7
      A7EFC5A4EEC2A0AE551AFF00FFFF00FFD9D9D9DDC6AEFFFDFCFFFFFDFFFFF6FF
      FFE8FFFFD8B4B493554D44E4CCA7C2B697939090939090FF00FF0274AC7AEBFE
      0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274
      AC0274AC0274ACFF00FFFF00FFFFBD83D89760D49970C8906AC8906AE3BCA8FF
      00FFFF00FFFF00FFFFBD83D89760D49970C8906AC8906AE3BCA83184FF3184FF
      3184FF42B5F73184FF3184FF3184FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5714DDC825AFF00FF319CBD31
      9CBD36A6D2FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      A7E3A79CE09C91DC91509450044E04FF00FF236A231B631B135C130B550B044E
      04FF00FFFF00FFFF00FFFF00FFDE8848E39861E39861E19359DF8D51F7E1D1EA
      B086DC7734DC7836E1844BDC7334F4D2BCE2A47AAF571DFF00FF858585B1B1B1
      CCCCCCBEBEBEC1C1C1DBDBDB5AB5D464D3F366D2F866D0FB67CEFE5BC1FD3CAB
      F0299CD4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF007FD0008BE4FF00FFFF
      00FFFF00FF047BD62D51AB4855846B92E1FF00FFFF00FFFF00FF8B8B8BA4A4A4
      D1D1D1B3B3B3ACACACA4A4A4B9B9B9B9B9B9C3C3C3C3C3C3B8B8B89E9E9EB9B9
      B9909090565656FF00FFFF00FF8F8F8FCECECEC8C8C8C5C5C5C3C3C3C1C1C136
      46D23040CCBABABAB6B6B6B3B3B3B1B1B1AEAEAE5A5A5AFF00FFCE6300FFFFFF
      313129509450418641212139FFFFFFFFFFFFFFFFFFFFFFF7FFF7E7FFEFD6FFE7
      C6FFDEB5CE6300FF00FFDE963FD076017898151BB53825BE4C3BC25384C354DD
      C150FFB335FEA00FF791008D89002E7F001773000F65006A7A39EB2978B90150
      BC015EC00171A3366AA68E67BDAA8B998360B69C73B18F5A926F3736739300A0
      FE00B1FF00BCFF2A8BB66879F8142EF51129E38A93D7FF00FF4F61EA142EF314
      2EF54E61F7FF00FFFF00FFFF00FF243CF4132DF42A3DC9FF00FFFF00FFFF00FF
      D1926DFFFFFFFFFFFFB08884DECAC4FAEFE5F8EAD9FFFFD4D9B8A5AC7F74F4D8
      B1EBCFA4A46769FF00FFFF00FFFF00FFFF00FF01679999FFFF99FFFF99FFFF99
      FFFF016799016799016799016799FF00FFFF00FFFF00FFFF00FFC7F5F3C7F5F3
      016799C7F5F3C7F5F34F00A7C7F5F3C7F5F3C7F5F3016799C7F5F34F00A7C7F5
      F3C7F5F3C7F5F3016799C26D43DFDFDFE1E1E1D86F64D86F64D86F64D86F64D8
      6F64D86F64D86F64D86F64D86F64F2F2F2F4F4F4C26D43FF00FFC26D43DFDFDF
      E1E1E1E2E2E2E5E5E5E4938AE4938AE4938AE4938AE4938AEEEEEEF0F0F0F2F2
      F2F4F4F4C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFAD
      7342FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFCEA58CAD7342AD7342AD7342AD7342AD7342734A29FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1A7D2119BD0919B90BFFFFFF1B
      B1101CAB14FFFFFFFFFFFF1D9E1B0B6E0DADADADFF00FFFF00FFFF00FFFF00FF
      FF00FF174AFF0C39FF072BCB0522A2989898FF00FF174AFF0C39FF072BCB0522
      A2989898FF00FFFF00FFFF00FFFF00FF0884AD21A5D631BDEF949494EFE7E7B5
      B5B5ADA5A5DEB5B594949463E7FF6BCEE7007BA5FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFBD8281FFE3B4FFD39FE9B281C99973BA916CBD8281807D7E6C6A
      6A6C6A6AFF00FFFF00FFFF00FFB36563FF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCE9C9CFF00FF06780E54C57A
      44C67452C77D57CA8156CA8057CA8149C57379D592FAFEFAFFFFFF97DDA51AAD
      3307A518039D0C0146032165A5417DC76B8DEF7395F74A75CE003E9C319CBD31
      9CBDB95914D6DDD8B7D8DAB9D0CE3196B8FCD6B2CA9068FF00FF00840000AD21
      FF00FFFF00FF319CBD009400319CBD319CBDB95914D6DDD8B7D8DAB9D0CE3196
      B8FCD6B2CA9068FF00FFFF00FFFF00FFCC8F6ADDDDDD2D31E70006E4877C95E6
      DCD3C7BDD8181CF0A58B7D7E5D55DAC19EE7CBA1A46769FF00FFD19970DBB49A
      DCB89EDEBBA3E0BEA8E2C3AFE4C7B3289D6221985683756CD0BCAFEBD5C8ECD8
      CCEEDDD2F0E0D6CE9162D19970DBB49ADCB89EDEBBA3E0BEA8E2C3AFE4C7B333
      A67431A47061C0665EBF625CBE5E59BC5A56BA5654B952469D29D19970DBB49A
      DCB89EDEBBA3E0BEA8CAAE9C81706538AA7C35A77768C57167C56F65C46E64C4
      6B63C46A62C46955AA4261B55862BB615EB95B5BB85859B65456B55153B44D1D
      944E19914983756CD0BCAFEBD5C8ECD8CCEEDDD2F0E0D6CE9162FF00FFFF00FF
      FF00FFFF00FF1BB33347D4741BB333FF00FFFF00FFCA8E62FBF7F5CA8A5AFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF009C00049F0708A30F0CA61610
      AA1E14AD2518B12D1BB3331EB6391BB3331BB333FF00FFFF00FF005B94005184
      0055BE005B9400388C00388C4A75CE0051845BF34D1CD70463CEDB6DD0FA6ACA
      F35F7575397E39FF00FF66CC33A05D28993300966C388D8F5D96501D99330081
      794C9933008F5A2A80A071A3844C983301997B468FB17E58B02CCECBCBC7A494
      FBC259FFCE5EB3883966503FE8C29EEDCA989E9D75496841777978FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF468B46B9E6BA1B95361096313DB35699
      DA9A94D89687D28A17933210963133AF4B60C462226A22FF00FFAD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD73420274AC7AEBFE7AEBFC0A7FB50274AC0274AC0274AC02
      74AC0274AC0274AC0274AC0274AC0274AC0274AC0274AC0274ACFF00FFFF00FF
      D9B5A1FFFFFFFFFFFFFFFFFFFFFEFCFCF7F0FAEFE5F8E9D9F8E7D1FBEACEDECE
      B4B6AA93A46769FF00FFFF00FF0274ACFEFEFE8FFEFF8FFEFF8FFEFF0273A32B
      A4D12BA4D12BA4D1046B0B35D35E20A73A046B0BFF00FFFF00FFFF00FF7F2B28
      BD4B4CF7F7F7BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFF7F7
      F7BD4B4C7F2B28FF00FFFF00FF7F2B28BD4B4CF7F7F7BFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFF7F7F7BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B28F78C8CF79494F79494F78C8CE7848476100FFFCEADFFD6BDFFD6BDFFD6
      BDF7A5A57F2B28FF00FF3184FF3184FF3184FF42B5F73184FF3184FF3184FFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF107BA5FF00FFFF00FFFF00FFFF00FF4E914E
      3184FF42B5F73184FF548BFF5188FF5188FF5188FFEBCBC7CDB0AD7C6A6886DA
      981A611AFF00FFFF00FFFF00FFFF00FFFF00FF0006F80006DA0006EFFF00FFFF
      00FFFF00FFFF00FF0006F80006F6FF00FFFF00FFFF00FFFF00FF6B635AFFFFFF
      E73900D63900CE3100B53100B53100A52900A52900FFFFFF848484DEBDBD6352
      52FFFFFF635A52735A52FF00FFFF00FFFF00FFFF00FFFF00FFB18A78FFDE99E9
      A167F4D199FEFCCCFFFFD5FFFFDAFFFFDCFFFFD7EFE6C5A97E75FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7E7E7FFFFFFF79C6BE76329CE8C6BEFE7DED67B52BD
      3100D69C7BFFFFFFDE8C63CE5218E79C73FFFFFFDEC6BDADADADBF6627F8F1ED
      FFFDFBFFF9F5FFF6EEFEF4E763BAD4B8D4D3F7DFC6B76A32FF00FFFF00FFFF00
      FFFF00FFA9ACDAFF00FFFF00FFFF00FF2A712A90DDA182D99574D5890B550BFF
      00FF2A712A90DDA182D99574D5890B550BFF00FFFF00FFFF00FFFF00FFFF00FF
      B2E6B290DDA182D99574D58986D986FF00FFB2E6B290DDA182D99574D58986D9
      86FF00FFFF00FFFF00FFFF00FF227FD64293DE4293DE297FDF2183DF2277D621
      7BCF84B3DEF7F7FFEFF7FF326FB41961A51961A5104784FF00FFFF00FFFF00FF
      5EA05EFAFDFCE6F9F5CEEDE8BEE5DCAFE1D19BDABF97DAB79BDBA794DAA1327A
      358C787671A678267229FF00FFFF00FFFF00FFFF00FFFF00FF498D49DDF5E1A6
      D0E5538AFF4A7EFF4A7EFF5683F7D3B5B29A84817AAC802B752EFF00FF5EA05E
      529552498D49408540219E992EE3FF40F1FF1EFAFF00FFFF07FEFF24EBF8696B
      816D5D5C68A070226F26498D49DDF5E1A6D0E5538AFF4A7EFF4A7EFF5683F7D3
      B5B29A84817AAC802B752E907563D8B59EDFC5B3E4C1ABFF00FFFF00FFFF00FF
      FF00FFFF00FF756257E2D4D4756257937D76C5A7A7756257FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFBB8369FFF7EFDD903DDD903DDD903DDCA175FEFEFEFE
      FEFEFEFEFEFEFCFBFEFAF6A0675BE5A154DC832AFF00FFFF00FFFF00FF004B82
      65DEFB60DCF85CD8F7C58870D1926DD1926DD1926DD1926DD1926DA0675BB79A
      6F004B82FF00FFFF00FFFF00FFCA4F00F8AD6BF38832FACCA4FEFEFEFEFEFEFA
      D4B2F7BB84F7BA82EE8E3EDD5600DA5500BF4A00772E00FF00FFCA4F00F8A760
      F28128FAD0A9FEFEFEFEFEFEC58870D1926DD1926DD1926DD1926DD1926DA067
      5BB79A6FFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE7C6AD39B55239
      B55239B55239B552218429218429218429218429F7F7D6C6AD9CFF00FFFF00FF
      FF00FFFF00FFFF00FFE7C6ADFFDEAD0029E70029E70029E70029E70029E70029
      E70029E7F7F7D6C6AD9CFF00FFFF00FFFF00FFFF00FFFF00FFE7C6AD72706DEA
      A460EDB473F0BD7FF0BD82F0BD86F0BD86F0BD826F6F6DC6AD9CFF00FFFF00FF
      FF00FFFF00FFFF00FFE7C6ADFFDEADE4B0777F2B28D8C096A1662EF2EACE7F2B
      28FFFFDEF7F7D6C6AD9CFF00FFD1926DFFFFFFFFFFFFFFFFFFCEE7CC00700000
      7000C9D5B0F8E7D1FBEACEDECEB4B6AA9380504BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC908FFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC908FC24A4AFF00FFFF00FFFF
      00FFCC908FFF00FFFF00FFFF00FFCC908FFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFCC908FFF00FFFF00FFFF00FFCC908FFF00FFFF00FFFF00FFCC908FC24A
      4AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCC
      908FFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDE8848F5DAC7
      F5D9C5F4D7C2F4D6C0F3D4BDF2D2BAF2D0B7F1CEB4B49376AD73427F4A45F0C8
      AA7F4A45EFC5A4AF571DAD7342734A29DEDAD8D2D0CDE8DDD2FFFDFCFFFFFFF3
      F9FFF3F8F8FFFDCFC3B995CAB8968C8B87777777FF00FFAD73420274AC83F2FE
      82F3FE82F3FE83F2FC83F3FE82F3FE83F2FE82F3FC83F2FE82F3FE82F3FE036F
      A7FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5714DDC825AFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3184FF8CD6F7
      B5DEF7B5DEF7B5DEF78CD6F73184FFFF00FFFF00FFA5714DFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE3BCA8A5714DFF00FFFF
      00FFFF00FFA5714DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      B2E6B290DDA150945074D5890B550BFF00FF2A712A90DDA182D99574D5890B55
      0BFF00FFFF00FFFF00FFFF00FFDE8C50E59F6BE59C67E2965EDF9055F7E1CFEB
      B893DC7E38E8A97AF9E7DBE08A50F5D7C3E0A37AAE581EFF00FF747474777777
      6F6F6F939393A3A3A38484845AB5D48BEAFE55CAEE3CBBE638B7E63DB8EA52BE
      F944ADF82A9BD4FF00FFFF00FFFF00FFFF00FF9BB8F30054E00071E400A6E400
      8BE4008BE40056E42754B1FF00FFFF00FFFF00FFFF00FFFF00FF8F8F8FA8A8A8
      D5D5D5D1D1D1BEBEBEBBBBBBBABABA3646D23040CCC3C3C3BABABA9E9E9EBCBC
      BC9191915A5A5AFF00FFFF00FF929292D0D0D0CACACAC8C8C8C5C5C5C3C3C33C
      4CD83646D2BCBCBCBABABAB6B6B6B3B3B3B1B1B15E5E5EFF00FFCE6300FFFFFF
      31313917C600FFFFFF418641FFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFF7E7FFEF
      D6FFE7C6CE6300FF00FFCB8006CB80063FB0352AC45639CB6653D3745ED476A7
      DA82FBD475FFB02EFD9A06A09000558300257700066A00066A00FC6CD3C90185
      C80192CC01A4BA01A6B97489CFB68FC3AA82C1A474B18D53735C640348F4006A
      FF0082FF0091FE6D9CC5818FF9142EF5132DF12F40C54357EC142EF4142EF54D
      61F7FF00FFFF00FFFF00FF818FF9142EF5142EF35562C0FF00FFFF00FFFF00FF
      D1926DFFFFFFFFFFFFD5BFBCBA9793DECAC4F6E9DEDAC0B4B78F84B28C7BDECE
      B4B6AA93A46769FF00FFFF00FFFF00FFFF00FF01679999FFFFE8AEA8E8AEA8E8
      AEA8016799FFFFFFFFFFFF016799FF00FFFF00FFFF00FFFF00FFC7F5F3C7F5F3
      016799C7F5F3C7F5F34F00A7C7F5F3016799016799C7F5F3C7F5F34F00A7C7F5
      F3016799016799C7F5F3C26D43DCDCDCDFDFDFE1E1E1D25E52D25E52D25E52D2
      5E52D25E52D25E52D25E52EEEEEEF0F0F0F2F2F2C26D43FF00FFC26D43DCDCDC
      DFDFDFE1E1E1E2E2E2EAA59EEAA59EEAA59EEAA59EEAA59EEDEDEDEEEEEEF0F0
      F0F2F2F2C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1A7D2119BD0919B90B1A
      B50E1BB1101CAB141CA7160E7112ADADADFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF1C4FFF0E3CFF072CD10523A7989898FF00FF1C4FFF0E3CFF072CD10523
      A7989898FF00FFFF00FFFF00FFFF00FFFF00FF108CB5088CBD949494EFE7E7B5
      B5B5ADA5A5E7BDB5949494189CC62194BDFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFBD8281FFE0B8FFD3A7FFD09DFFCE90FFC688BD8281FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF06780E4CBD69
      83DDA722B6551CB24E22B55422B55455CA7FE1F6E9FFFFFF88D99D10AB2F0CA6
      2706A716038C0A0146032165A53274C64978D74978D700388C003E9C005B94FF
      00FFBC5F1CF8EDE6FEF4EACFDCD82692B8F6E2CAD0996EFF00FF00840000AD21
      009400FF00FFFF00FF009C21009C21FF00FFBC5F1CF8EDE6FEF4EACFDCD82692
      B8F6E2CAD0996EFF00FFFF00FFFF00FFC086645659D70006F6695FACA78884DB
      C7C1F6E9DE8375CB2723DA91737DCDBEA6B3A790A46769FF00FFFF00FFCA8B5D
      DBB49ADCB89EDEBBA3E0BEA8E2C3AF2FA26C299E6382746ACFB9ACE9D2C4EBD5
      C8ECD8CCC87D41FF00FFFF00FFCA8B5DDBB49ADCB89EDEBBA3E0BEA8E2C3AF38
      A97A35A77565C36B61C0665EBF625CBE5E59BC5A4A9D2AFF00FFFF00FFCA8B5D
      DBB49ADCB89EDEBBA3C8AA96806E633CAD813AAB7E6AC67368C57167C56F65C4
      6E64C46B57A63EFF00FFFF00FF5FB05262BB615EB95B5BB85859B65456B55121
      98561E955082746ACFB9ACE9D2C4EBD5C8ECD8CCC87D41FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF1BB3331EB639FF00FFFF00FFCB9066CA8E62FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF049F0700A52110B03421BB4731
      C65A3CCD6747D47447D4741BB3331BB333FF00FFFF00FFFF00FFFF00FF005184
      005F9C227BC63263C64978D77395F72153A564F5551DDD0519C802ACD7EBC8AC
      A9756463397E39FF00FF66CC33B8D795A4743DA05D269F5D26993E0A99330082
      7A4C9933008F5A2A81A172AFD99AADA165A05D27A7965B62C431CECBCBDCB78C
      FFD368E5CB7C439CC42F9CD557818EBAA57BD1905D624D4CFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD734273
      4A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342AD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD7342AD7342734A29498E49CBEDCC5BB1691B953667C174A6
      DEA79EDC9F93D8954AAB5817933252B85F70CA72266D26AD7342CEA58CAD7342
      FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA5
      8CAD7342FF00FFCEA58C0274AC83F2FE82F3FE82F3FE83F2FC83F3FE82F3FE83
      F2FE82F3FC83F2FE82F3FE82F3FE036FA7FF00FFFF00FFFF00FFFF00FFFF00FF
      DDB7A4FFFFFFFFFFFFFFFFFFFFFFFFFFFEFCFCF6EFFCF3E6EDD8C9B68A7BA17B
      6F9C7667A46769FF00FFFF00FFFF00FF0274AC0274AC0274AC0274ACFF00FFFF
      00FFFF00FFFF00FF046B0B28C24A046B0BFF00FFFF00FFFF00FFFF00FF7F2B28
      BD4B4CF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      F7BD4B4C7F2B28FF00FFFF00FF7F2B28BD4B4CF7F7F7F7F7F7F7F7F7F7F7F743
      74FF4374FF4374FFF7F7F7F7F7F7F7F7F7BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B28F78C8CFF9494FF9494F79494E78C8C76100FFFCEADFFD6BDFFD6BDFFD6
      BDF7A5A57F2B28FF00FF3184FF8CD6F7B5DEF7B5DEF7B5DEF78CD6F73184FF18
      8CB5188CB5188CB5188CB5188CB5107BA5FF00FFFF00FFFF00FF3184FF3184FF
      3184FF42B5F73184FF3184FF3184FF72B6FF72B5FF6FB2FF96DFA6C1A6A38BDC
      9C1C641CFF00FFFF00FFFF00FFFF00FF0006F60006F60006F8FF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FF0006F60006F6FF00FFFF00FFFF00FF6B635AFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7EFEFEF848484DEBDBD5A52
      4AFFFFFF635A5273635AFF00FFFF00FFFF00FFFF00FFFF00FFAA7F73FAE0A4F0
      B778EEBA7BF6DDA6FEFBCCFFFFD3FFFFD1FFFFD7D9C5A7A3756CFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B84EF00109C6B84EFFF00FFFF00
      FFFF00FFFF00FFFF00FFF7EFE7F7EFEFFFEFDEFF8C4ADE845AEFFFFFFFFFFFE7
      BDA5F7FFFFEFFFFFE77339E77339FFEFEFF7E7DEA59C94FF00FFBE662DF8F2EF
      FFFEFEFFFDFCFFF9F5FFF6EEC5E0E199CDD8F4DAC2AC652EFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF3278329EE2AD90DDA182D995135C13FF
      00FF3278329EE2AD90DDA182D995135C13FF00FFFF00FFFF00FFFF00FFFF00FF
      BDEABD9EE2AD90DDA182D99591DC91FF00FFBDEABD9EE2AD90DDA182D99591DC
      91FF00FFFF00FFFF00FFFF00FF2987DF5299E84B9CE7318BDF2987DF2987DF21
      83DF217BCF84B3DEEFF7FFDEEEF8226EB41966AD104784FF00FFFF00FFFF00FF
      FF00FF5DA05D529552498D49408540387D383076302E742E2A702A276D27B3A7
      989A84817AAC802B752EFF00FFFF00FFFF00FFFF00FFFF00FF529552EBFBF3B2
      E6F773B4FF66A5FF66A5FF6AA6FC9FBFD7A8B19B93BF952E7731FF00FFFF00FF
      FF00FFFF00FFFF00FF408540BAF1BB2EE2CD0DF0F300F0FF00BFFF4182CA6156
      5A584C4B67996F267229529552EBFBF3B2E6F773B4FF66A5FF66A5FF6AA6FC9F
      BFD7A8B19B93BF952E7731A27358DDA07EEBB08FDFBAA5FF00FFFF00FFFF00FF
      FF00FF756257FBF8F8756257FF00FF756257D5C0C0756257FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFC48B6CFEFAF7FEF7F0FEF4EAFEF2E3E1A577D1926DD1
      926DD1926DD1926DD1926DA0675BEB9E43FF00FFFF00FFFF00FFFF00FF004B82
      69E1FE66DEFB63DDFB63DDFB5DD9F857D5F650CFF249C9EE42C2EA3CBFE738BB
      E5004B82FF00FFFF00FFFF00FFCA4F00FAB77BF49646F27F24F8BF8AFEFEFEEF
      6C0DEF6A0AEF6404EE5E00E95B00E55A00CB4F00873500FF00FFCA4F00F8AD6B
      F38832FACCA4FEFEFEFEFEFEFAD4B2F7BB84F7BA82EE8E3EDD5600DA5500BF4A
      00772E00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDEBDAD94DE8C94
      DE8C94DE8C94DE8C39B55239B55239B55239B552F7EFD6C6A59CFF00FFFF00FF
      FF00FFFF00FFFF00FFDEBDADFFE7B59CB5FF9CB5FF638CF7638CF7638CF7638C
      F7526BF7F7EFD6C6A59CFF00FFFF00FFFF00FFFF00FFFF00FFDEBDAD73706DEA
      A460EDAA66F0B779F0BD82F0BD82F0BD82F0BD82706F6DC6A59CFF00FFFF00FF
      FF00FFFF00FFFF00FFDEBDADF9DEAC9950357F2B28FFEFC6D8C0969C5D477F2B
      28FFFFDEF7EFD6C6A59CFF00FFDA9D75FFFFFFFFFFFFFFFFFFFFFFFFFFFEFCFC
      F6EFFCF3E6EDD8C9A0675BA0675BA0675BA0675BFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDE8C50F6DCCA
      F5DAC7F5D9C5F4D7C2F4D6C0F3D4BDF2D2BAF2D0B7F1CEB4B49376AD73427F4A
      457F4A45EFC7A7AE581EAD7342734A29FF00FFDFDFDFCECCCADDC6AEFEF1E4B3
      D7FBACD4F9FFF0C6CBBA9BA7A5A1969696734A29FF00FFAD73420274ACFEFEFE
      89FAFF89FAFE89FAFE8AF8FE8AFAFE89F8FE8AFAFE8AFAFF89FAFF8AFAFF036F
      A7FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE3BCA8A5714DFF00FFFF
      00FFFF00FFA5714DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3184FF3184FF
      3184FFB5DEF73184FF3184FF3184FFFF00FFFF00FFA5714DFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5714DFF
      00FFFF00FFA5714DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      BDEABD50945090DDA182D995135C13FF00FF3278329EE2AD90DDA182D995135C
      13FF00FFFF00FFFF00FFFF00FFE19257E7A879E6A072E39A65E1945BEDC09FF8
      E7DCE1945BDF8A4BE2965EE9AC7FFAEEE6D6824AAE581EFF00FF646464595959
      6666668686869F9F9FBEBEBE5AB5D495F0FF62D4F357CBF053C7F04CC2EF4EC1
      F257BFFA39AAED158DBDFF00FFFF00FFFF00FF0054E0004BE44967B31B56CF00
      56E46B92E11355DB004BE42F66D5FF00FFFF00FFFF00FFFF00FF929292AAAAAA
      DADADAD5D5D5C2C2C2BEBEBEBDBDBD3C4CD83646D2CECECEC3C3C3A7A7A7BDBD
      BD9393935E5E5EFF00FF000000000000000000A1A1A1A1A1A1A1A1A100000000
      0000000000A1A1A1A1A1A1A1A1A1A1A1A1000000000000000000CE6300FFFFFF
      424242524A4A5A524A41864117C600FFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFF7
      E7FFEFD6CE6300FF00FFFF00FF83B24728BF4D37CC676EDA82B4E9A198E9A6CD
      F1B8E5E496ECBC47ECA112D09001778500327A0046780BFF00FFFF00FFE602C8
      CE01B0D201C2D700DAC701DBB53BA6AD69899C627F6734A00C03F20019FF003D
      FF0059FF035FE8FF00FF818FF91E36F5142EF5132CEE132DF4142EF54C60F7FF
      00FFFF00FFFF00FF818FF93248EC142EF51F37ED8A93D7FF00FFFF00FFFF00FF
      DA9D75FFFFFFFFFFFFFFFFFFD5BFBCB08884976560AF867FCAA79DA56B5FA56B
      5FA56B5FA46769FF00FFFF00FFFF00FFFF00FF01679999FFFF99FFFF99FFFF99
      FFFF016799FFFFFF016799FF00FFFF00FFFF00FFFF00FFFF00FFC7F5F3C7F5F3
      C7F5F3C7F5F3C7F5F34F00A7C7F5F3C7F5F3C7F5F3C7F5F3C7F5F34F00A7C7F5
      F3C7F5F3C7F5F3C7F5F3C26D43D7D7D7DCDCDCDFDFDFE1E1E1CC4D40CC4D40CC
      4D40CC4D40CC4D40EBEBEBEDEDEDEEEEEEF0F0F0C26D43FF00FFC26D43D7D7D7
      DCDCDCDFDFDFE1E1E1F1B7B1F1B7B1F1B7B1F1B7B1F1B7B1EBEBEBEDEDEDEEEE
      EEF0F0F0C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1C7F231A7D2119BD0919
      B90B1AB50E1BB110127517107315FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF2054FF1343FF072FDD0626B4A8A8A8FF00FF2054FF1343FF072FDD0626
      B4A8A8A8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494E7E7E7AD
      ADADADA5A5DEB5B5949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFC08683FFE7CFFFE0C0FFD9B2FFD3A5FFD099BD8281FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF139923
      AAE7C568D08E16AF481BB14C39BF68F4FCF7FFFFFF84D99E11AA320EA7290BA4
      2009AF1C036B0AFF00FF0051840051840051843263C600388CFF00FFFF00FFFF
      00FFBE6425FAF4F0FFF8F3FBF3E942A1C1CFD9D0D19A6FFF00FF009400008400
      00BD21009400008C21008C214ACE73008400BE6425FAF4F0FFF8F3FBF3E942A1
      C1CFD9D0D19A6FFF00FFFF00FFFF00FF4F3DBE0006F68081C9E4E4E4D1BCB9B0
      8884976560AF867FCAA79D85577AA2695DA46A5EA46769FF00FFFF00FFCA8D60
      D9B195DBB49ADCB89EDEBBA3E0BEA836A77531A46D817268CFB7A8E8CFC0E9D2
      C4EBD5C8C87E43FF00FFFF00FFCA8D60D9B195DBB49ADCB89EDEBBA3E0BEA83D
      AE803AAB7C67C46E65C36B61C0665EBF625CBE5E4EA030FF00FFFF00FFCA8D60
      D9B195DBB49ADCB89EC6A7927E6B5F40B0853EAE836CC8766AC67368C57167C5
      6F65C46E59A841FF00FFFF00FF62B35864BC6362BB615EB95B5BB85859B65427
      9C5D239A57817268CFB7A8E8CFC0E9D2C4EBD5C8C87E43FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF1BB3331BB333FF00FFFF00FFCB926ACB926AFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF08A30F10B03421BB4731C65A3C
      CD6747D4741EB6391BB333FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      0051840051840051844978D76B8DEF6385E700518425D7253F7EE38190DCC9AC
      A9756463397E39FF00FF66CC33BCEAA6BCEAA6BCEAA6BCEAA6A0622C9933008B
      8351993300935D2B8CAE7CB2DD9DBCEAA6BCEAA6BCEAA666CC33CECBCBEBC88C
      FFDF79D1D29F51ABD34DBDF247B8ED95AD90E8B0705E5252FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD734273
      4A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342AD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD7342AD7342734A294C914CD9F3D9CBEDCCB9E6BABAE7BBB0
      E3B1A7DFA89EDC9F93D89587D28A82D18479CD7B296F29AD7342FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF0274ACFEFEFE89FAFF89FAFE89FAFE8AF8FE8AFAFE89
      F8FE8AFAFE8AFAFF89FAFF8AFAFF036FA7FF00FFFF00FFFF00FFFF00FFFF00FF
      E2BCA5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFBFFFEF7DAC1BAAD735BE19E
      55E68F31B56D4DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FF046B0B17A42B19A730046B0BFF00FFFF00FFFF00FFFF00FF7F2B28
      BD4B4CF7F7F7BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFF7F7
      F7BD4B4C7F2B28FF00FFFF00FF7F2B28BD4B4CF7F7F7BFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBF4374FF4374FFBFBFBFF7F7F7BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B28F78C8CFF9494FF9C9CFF9494E78C8C76100FFFCEADFFD6BDFFD6BDFFD6
      BDF7A5A57F2B28FF00FF3184FF3184FF3184FFB5DEF73184FF3184FF3184FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3184FF8CD6F7
      B5DEF7B5DEF7B5DEF78CD6F73184FFBCEBC6A6E4B3A0E2AF9BE1AA96DFA690DE
      A11F661FFF00FFFF00FFFF00FF0006F60006F60006F6FF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FF0006F6FF00FFFF00FF6B635A6B635A
      6B635A6B635A6B635A6B635A6B635A6B635A635A5A5A524A847B7BDEC6C64A42
      396B635A6B635A73635AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEB293FF
      FEDDF4D1A5EEBA7BF2C78FF8E1ABFCF0BAFCFACAA3776FFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00109C00109C00109CFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFEFDED6FFFFFFFFEFCEFFB573EFAD84EFE7DEEF
      F7F7EFE7DEF7A57BFF8C4AFFDECEFFFFFFEFDED6CECEC6FF00FFBE5D0FC98457
      D7A786E5C6B2EEDBD0F7EDE7F9F3ECF9F5EBF5DEC9AC672DFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF418641ABE6B89EE2AD90DDA11B631BFF
      00FF418641ABE6B89EE2AD90DDA11B631BFF00FFFF00FFFF00FFFF00FFFF00FF
      D3F1D3ABE6B89EE2AD90DDA19CE09CFF00FFD3F1D3ABE6B89EE2AD90DDA19CE0
      9CFF00FFFF00FFFF00FFFF00FF4293DE6AABE85299E84293DE398FDF318BDF31
      8BDF2987DF227FD673B2E7DEEEF8C6DEF0226EB4104784FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF529552EBFBF3B2E6F773B4FF66A5FF66A5FF6AA6FC9FBF
      D7A8B19B93BF952E7731FF00FFFF00FFFF00FFFF00FFFF00FF5EA05EFAFDFCE6
      F9F5CEEDE8BEE5DCAFE1D19BDABF97DAB79BDBA794DAA1327A35FF00FFFF00FF
      FF00FFFF00FFFF00FF498D49DDF5E1A6D0E5489EFE2AB3FB26D1FF12BEFC6585
      C06153516F9C742B752E5EA05EFAFDFCE6F9F5CEEDE8BEE5DCAFE1D19BDABF97
      DAB79BDBA794DAA1327A35DB8253FA9762FC9D6AD6AA93FF00FFFF00FFFF00FF
      FF00FF756257756257FF00FFFF00FF756257E6D9D9756257FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFCC9370FEFEFCFEFBF8FEF8F3FEF6EDFEF2E6E5D5D0C6
      B1AFA79395734241FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF004B82
      6AE2FE6AE2FE58585858585858585858585858585858585858585845C7EB40C1
      E9004B82FF00FFFF00FFFF00FFCA4F00FABA80F7B477F6A45CF49A4EF8C595F2
      842CF07A1EEF6A0AEE6203EF5E00F25E00D95500963A00FF00FFCA4F00FAB77B
      F49646F27F24F8BF8AFEFEFEEF6C0DEF6A0AEF6404EE5E00E95B00E55A00CB4F
      00873500FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC69C94FFEFC6FF
      EFC6F7D6A594DE8C39B552FFF7CEFFF7D6FFFFD6E7DEBDFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFC69C94FFEFC6FFEFC6F7D6A5F7CE9CF7E7B5FFF7CEFFF7
      D6FFFFD6E7DEBDFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC69C9473716EF0
      B779EDAD6DEDAA69EDB473F0BA7CF0BA7FF0BD7F6F6E6CFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFC69C94FFEFC6F2DCB1985137F7CE9CF7E7B5F2E3B89956
      3FFFFFD6E7DEBDFF00FFFF00FFDA9D75FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FEFBFFFEF7DAC1BAA0675BE19E55E68F31B56D4DFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE19257F6DECC
      F6DCCAF5DAC7F5D9C5F4D7C2F4D6C0F3D4BDF2D2BAF2D0B7F1CEB4B49376AD73
      427F4A45F0C8AAAE581EAD7342734A29FF00FFAD7342DEDDDDDADAD9CFC7BFCC
      BFB0C6BAABB9B4ADAEAEAEA2A2A2AD7342734A29FF00FFAD7342FF00FF0274AC
      FEFEFE8FFEFF8FFEFF8FFEFF0274AC0274AC0274AC0274AC0274AC0274ACFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5714DFF
      00FFFF00FFA5714DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      3184FFB5DEF73184FFFF00FFFF00FFA5714DA5714DFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5
      714DA5714DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      509450ABE6B89EE2AD90DDA11B631BFF00FF418641ABE6B89EE2AD90DDA11B63
      1BFF00FFFF00FFFF00FFFF00FFE49A64EBB58DE9AC83E59F71E49D68E4A170F6
      DAC6FBEEE5F3D0B7F4D6C2FBEFE7EAAE86D07030AB581BFF00FFA9A8A7A8A7A6
      8A89898181818484849A9A9A5AB5D4A6F5FF6FDEF766D6F35DCFF256C9F14BC1
      EE58C5F74EBDF41A92C1FF00FFFF00FF6B92E11C64F66B92E1FF00FF145CEE00
      4BE4FF00FFFF00FF286BF25F8CE7FF00FFFF00FFFF00FFFF00FF949494929292
      8F8F8F8B8B8B8888888484847F7F7F7C7C7C7878787575757171716E6E6E6B6B
      6B686868626262FF00FF000000FFFFFF0000008B8B8B888888848484000000FF
      FFFF0000007575757171716E6E6E6B6B6B000000FFFFFF000000CE6300FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF17C600418641FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      F7FFF7E7CE6300FF00FFFF00FF33C55933C55951CD688BE190D3F4B7D0F7BECD
      F5BB9AE19174C150BCA51AE88E01C87C00487D00487D00FF00FFFF00FFFC6BFA
      DE01D3D700DADD00F1E300FEDD00FEAF00F58600F65900FE2400FF0202FF0019
      FF0035FE6D8CE6FF00FFFF00FF6879F8152FF5142EF5142EF5132CE76875D28A
      93D78A93D7707CD9253CE8142EF51730F4717DDCFF00FFFF00FFFF00FFFF00FF
      DA9D75FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFBFFFEF7DAC1BAA56B5FE19E
      55E68F31B56D4DFF00FFFF00FFFF00FFFF00FF01679901679901679901679901
      6799016799016799FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4F00A74F00A7
      4F00A74F00A74F00A7FF00FF4F00A74F00A74F00A74F00A74F00A7FF00FF4F00
      A74F00A74F00A74F00A7C26D43D3D3D3D7D7D7DCDCDCDFDFDFE1E1E1C63C2EC6
      3C2EC63C2EE8E8E8E9E9E9EBEBEBEDEDEDEEEEEEC26D43FF00FFC26D43D3D3D3
      D7D7D7DCDCDCDFDFDFF7C9C4F7C9C4F7C9C4F7C9C4F7C9C4E9E9E9EBEBEBEDED
      EDEEEEEEC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1A7D2119
      7C1F177A1D15781BFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF2C62FF1B4EFF0A35F6072BCBFF00FFFF00FF2C62FF1B4EFF0A35F6072B
      CBFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494B5B5B5A5
      A5A59C9494ADA5A5949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFBD8281FEEBD8FFE6CCFFDEBDFFD8B1FED3A4BD8281FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF139923
      56C573C5F0D866CF8C20B45223B554AAE6C096DEB119B14813AC3C12AA340FB0
      2D0A991F036B0AFF00FF005184FF00FF005184227BC6005B94FF00FFFF00FFFF
      00FFBE662DFBF7F5FFFDFCFFF8F3D5E6E43298BBCA976DFF00FFFF00FF008400
      009C0021C63131C65A4AD67373F7A521A542BE662DFBF7F5FFFDFCFFF8F3D5E6
      E43298BBCA976DFF00FFFF00FFFF00FF0006F6797BEEEDEDEDFBFBFBFFFFFFFF
      FFFFFFFEFBFFFEF7DAC1BAA56B5FE19E55E68F31B56D4DFF00FFFF00FFFF00FF
      CA8D60D9B195DBB49ADCB89EDEBBA33CAD7D38A976817065CDB5A5E7CDBCE8CF
      C0C98148FF00FFFF00FFFF00FFFF00FFCA8D60D9B195DBB49ADCB89EDEBBA342
      B1863FAF816AC57167C46E65C36B61C06654A63CFF00FFFF00FFFF00FFFF00FF
      CA8D60D9B195DBB49AC5A58D7D6A5C44B28A42B1876EC8786CC8766AC67368C5
      715DAC49FF00FFFF00FFFF00FFFF00FF62B35864BC6362BB615EB95B5BB8582C
      A064299E5E817065CDB5A5E7CDBCE8CFC0C98148FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF1BB333FF00FFFF00FFCB926AFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0CA61621BB4731C65A3CCD671B
      B3331BB333FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF0051843274C6417DC7005184DBF3DF5D97FF4E83FF4E83FFE0C1
      BD897573397E39FF00FF66CC33BCEAA6BCEAA6BCEAA6BCEAA6A2642D993300A7
      9C629933009E642EB1DC9CBAE7A4BCEAA6BCEAA6BCEAA666CC33CECBCBDDBF89
      F8DB8BFFEEA4EFEDB285BCBC92C8C8FFEF9CD6A77A646061FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD734273
      4A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342AD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD7342AD7342734A29FF00FF4C914C498E49468B4643894340
      86403D833D3B813B367D36337A333177312E752EFF00FFAD7342FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF007000007000007000FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FF0274ACFEFEFE8FFEFF8FFEFF8FFEFF0274AC02
      74AC0274AC0274AC0274AC0274ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      E6BFA7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDCC7C5B88265F8B5
      5CBF7A5CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FF046B0B11A122046B0BFF00FFFF00FFFF00FFFF00FFFF00FF7F2B28
      BD4B4CF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      F7BD4B4C7F2B28FF00FFFF00FF7F2B28BD4B4CF7F7F7F7F7F7F7F7F74374FF43
      74FFF7F7F74374FF4374FFF7F7F7F7F7F7BD4B4C7F2B28FF00FFFF00FFFF00FF
      7F2B287F2B28E79C9CFF9494FF9C9CEF8C9476100FFFCEADFFD6BDFFD6BDFFD6
      BDF7A5A57F2B28FF00FFFF00FFFF00FF3184FFB5DEF73184FF319CBDFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3184FF3184FF
      3184FFB5DEF73184FF3184FF3184FF387D383177312F752F2C722C296F29266D
      26FF00FFFF00FFFF00FF0006F60006F60006F6FF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF848C8C5A5252948C8CFFE7E75A52
      52524A426B5A52FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA1746BE1
      D4D3FFFEEEF7CC8CF0B473F7C788FCE3A5C2A088A5776CFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B84EF00109C6B84EFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFF7EFEFEFDED6FFFFFFFFFFFFFFF7C6FFDEADFF
      CE94FFCE94FFD6ADFFF7F7FFFFFFEFDED6CECEC6FF00FFFF00FFC9640AC35C02
      C15A01C05B07C1631AC26D2DCB8450D39C77D9A683AC6428FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF5094504186413278322A712A236A23FF
      00FF5094504186413278322A712A236A23FF00FFFF00FFFF00FFFF00FFFF00FF
      E9F8E9D3F1D3BDEABDB2E6B2A7E3A7FF00FFE9F8E9D3F1D3BDEABDB2E6B2A7E3
      A7FF00FFFF00FFFF00FFFF00FF4B9CE784BCF06AABE85299E84B9CE74293DE39
      8FDF318BDF2987DF2183DF4A97DEB5D4EF9BBBDF113F73FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF5EA05EFAFDFCE6F9F5CEEDE8BEE5DCAFE1D19BDABF97DA
      B79BDBA794DAA1327A35FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5EA05E52
      9552498D49408540387D383076302E742E2A702A276D27FF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF529552EBFBF3B2E6F773B4FF66A5FF66A5FF34D0FF528D
      DF99A18D90BB922E7731FF00FF5EA05E529552498D49408540387D383076302E
      742E2A702A276D27D9A07FE59D75E9986DE99D74E6C3ADFF00FFFF00FFFF00FF
      FF00FF756257FF00FFFF00FFFF00FF756257F7F2F2756257FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFD59C74FEFEFEFEFEFEFEFBFAFEFAF4FEF6EEA0675BA0
      675BA0675B9D6559FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF004B82
      6AE2FE6AE2FE585858C1B0AAC1B0A9C1B0A9C1B0A9C0ACA45858584DCCF047C7
      ED004B82FF00FFFF00FFFF00FFCB4F00FAA65DFABB82FABD87FAB77BF8AC69F7
      9D4FF6872DF47413F36604F36000FA6200E35900A14000FF00FFCA4F00FABA80
      F7B477F6A45CF49A4EF8C595F2842CF07A1EEF6A0AEE6203EF5E00F25E00D955
      00963A00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDEC6ADFF
      FFFFFFF7EF94DE8C39B552F7CE9CFFE7B5FFF7C6BD9C8CFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFDEC6ADFFFFFFFFF7EFF7CE94EFBD84F7CE9CFFE7
      B5FFF7C6BD9C8CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF716F6D73
      7271737170726F6B716E6A716E6B71706C71706D6D6B6AFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFDEC6ADFFFFFFFFF7EFF7CE94EFBD84F7CE9CFFE7
      B5FFF7C6BD9C8CFF00FFFF00FFE7AB79FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFDCC7C5A0675BF8B55CBF7A5CFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE49A64F7E0D0
      F6DECCF6DCCAF5DAC7F5D9C5F4D7C2F4D6C0F3D4BDF2D2BAB49376B49376B493
      76AD7342F0CAADAB581BAD7342734A29FF00FFAD7342734A29FF00FFB2ADA7BD
      BDBDB7B7B79E9494888282FF00FFAD7342734A29FF00FFAD7342FF00FFFF00FF
      0274AC0274AC0274AC0274ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA5
      714DA5714DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      3184FF3184FF3184FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      5094504186413278322A712A236A23FF00FF5094504186413278322A712A236A
      23FF00FFFF00FFFF00FFFF00FFE6A270EDBA95EAAF88E6A576E5A06DE49866E4
      9D68ECB892F2CCB0EEBFA0E59F6BDC7E40CF6D2BAE5C20FF00FFFF00FFFF00FF
      FF00FF857F7C857F7CFF00FF5AB5D45AB5D463D0EA48C4E547C3E745C1E944BF
      EA60CCF850BFEE3BA2C8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1C64F600
      4BE4FF00FFFF00FFFF00FF9BB8F3FF00FFFF00FFFF00FFFF00FFFF00FF60A260
      FFFFFFF7FDF8E9F9ECDAF4DFCBEFD2BCEBC6A6E4B3A0E2AF9BE1AA96DFA690DE
      A11F661FFF00FFFF00FF000000000000000000FF00FFFF00FFFF00FF00000000
      0000000000FF00FFFF00FFFF00FFFF00FF000000000000000000CE6300CE6300
      CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE63
      00CE6300CE6300FF00FFFF00FFFF00FF85C55885C5589CD97FD2E69BD3F1B0B6
      E69962D57831C35591A721DF8B03E68205C87C00FF00FFFF00FFFF00FFFF00FF
      FA43FBE400F2E100FDE600FFD400FFB200FF8F00FF6800FF3B00FF1300FF0003
      FF4554F8FF00FFFF00FFFF00FFFF00FF495DF7152EF5142EF5132CEF1027DD0F
      26D61028E0132DF1142EF5162FF45365EAFF00FFFF00FFFF00FFFF00FFFF00FF
      E7AB79FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDCC7C5A56B5FF8B5
      5CBF7A5CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFC26D43CECECED3D3D3D7D7D7DCDCDCDFDFDFE1E1E1C0
      2B1CE5E5E5E6E6E6E8E8E8E9E9E9EBEBEBEDEDEDC26D43FF00FFC26D43CECECE
      D3D3D3D7D7D7DCDCDCFEDCD8FEDCD8FEDCD8FEDCD8FEDCD8E8E8E8E9E9E9EBEB
      EBEDEDEDC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF2054FF0F3DFFFF00FFFF00FFFF00FFFF00FF2054FF0F3DFFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494E7E7E7C6
      C6C6A5A5A5B5A5A5949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      BD8281FFFFF2FFFFF2FFEBD8FFE5CAFFE1BDF3C7A7BD8281FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      1399236ACC88D0F4E39AE1B650C77A38BD672CBA5D30BB602FBC5D23BC4F11A3
      3006620FFF00FFFF00FFFF00FFFF00FF005184005F9C0055BE005B94FF00FFFF
      00FFC15F0D005184D69E73E0B698E7CAB2EBD4C1C69164FF00FFFF00FFFF00FF
      0084000084000084004AD6736BEF9C63E794008400CA8047D69E73E0B698E7CA
      B2EBD4C1C69164FF00FFFF00FFFF00FFE7AB79FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFDCC7C5A56B5FF8B55CBF7A5CFF00FFFF00FFFF00FFFF00FF
      FF00FFCA8D60D9B195DBB49ADCB89E43B2863EAE7F806E63CCB2A0E5CAB8C982
      4CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCA8D60D9B195DBB49ADCB89E46
      B58C43B3886CC6746AC57167C46E5CAB49FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFCA8D60D9B195C4A18A7C685948B58F45B48C6FC97A6EC8786CC87661AF
      50FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF62B35864BC6362BB615EB95B31
      A46A2EA265806E63CCB2A0E5CAB8C9824CFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF10AA1E31C65A18B12D18B12DFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF0051840051842165A52165A5599B59E6FDFBABDEF693CBEE89C7E583C6
      D9AAC4A4397E39FF00FF66CC33BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BC
      EAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA666CC33CECBCBB4ABA9
      B7A69ECAB79EDDCDABEEDEB4F4E5AFFEECA0BD9577904B8FFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD734273
      4A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342AD7342734A29
      FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD73
      42734A29FF00FFAD7342AD7342734A29FF00FFAD7342734A29FF00FFAD734273
      4A29FF00FFAD7342734A29FF00FFAD7342734A29FF00FFAD7342FF00FFFF00FF
      FF00FFFF00FF007000FF00FFFF00FFFF00FFFF00FF007000007000FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF0274AC0274AC0274AC0274ACFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      E4BCA4FBF4F0FBF4EFFAF3EFFAF3EFF8F2EFF7F2EFF7F2EFD8C2C0B77F62C183
      6CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF04
      6B0B046B0B046B0B046B0BFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      7F2B28F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
      F77F2B28FF00FFFF00FFFF00FFFF00FF7F2B28F7F7F7F7F7F7F7F7F7F7F7F743
      74FF4374FF4374FFF7F7F7F7F7F7F7F7F77F2B28FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF7F2B287F2B28D68484DE8C8C76100F7F2B287F2B287F2B287F2B
      287F2B287F2B28FF00FFFF00FFFF00FF3184FF3184FF3184FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      3184FFB5DEF73184FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF0006F60006F6FF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF848C8CEFEFEFD6C6C66B6363DEC6
      C6FFE7E74A4A4AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF98
      6865BA9587EAD7A4EAD59EE0C097A5776CA5776CFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFF7EFEFEFDED6F7E7E7FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFEFE7DEEFDED6DEDEDEFF00FFFF00FFFF00FFFF00FFD9B490
      E2A159DA8C3FCF7721CA690DC76208C55D03C35C03A75F1EFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF4B9CE7398FDF2987DF227FD6227FD621
      7BCF217BCF2277C62272BC226EB41966AD185894FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF5EA05E529552498D49408540387D383076302E74
      2E2A702A276D27FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF5EA05EFAFDFCE6F9F5CEEDE8BEE5DCAFE1D19BDABF97DA
      B79BDBA794DAA1327A35FF00FFFF00FFFF00FFFF00FFFF00FFE5CAB9FDF3DAFD
      F1D0FDF0CFFDEECDFEECCBFEEAC9FEE8C8FFE7C7EED4BBFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF756257756257FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFDCA175FEFEFEFEFEFEFEFEFEFEFCFBFEFAF6A0675BE5
      A154DC832AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      004B82004B82585858D9CCC6F8F7F6F7F6F4F7F6F4C2B5AD585858004B82004B
      82FF00FFFF00FFFF00FFFF00FFFF00FFCF5C0AD46C1CD46E1ED46E1ED36A18D1
      6513CF5C0ACC5504CB5101CA5000CB5000BA4800FF00FFFF00FFCB4F00FAA65D
      FABB82FABD87FAB77BF8AC69F79D4FF6872DF47413F36604F36000FA6200E359
      00A14000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD6
      BDBDF7EFD694DE8C39B552FFE7B5F7DEB5CEAD9CFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFD6BDBDF7EFD6FFEFC6FFE7ADFFE7B5F7DE
      B5CEAD9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD6
      BDBDF7EFD6FFEFC6FFE7ADFFE7B5F7DEB5CEAD9CFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFD6BDBDF7EFD6FFEFC6FFE7ADFFE7B5F7DE
      B5CEAD9CFF00FFFF00FFFF00FFE7AB79FBF4F0FBF4EFFAF3EFFAF3EFF8F2EFF7
      F2EFF7F2EFD8C2C0A0675BC1836CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE6A270F7E1D1
      F7E0D0F6DECCF6DCCAF5DAC7F5D9C5F4D7C2F4D6C0F3D4BDF2D2BAF2D0B7F1CE
      B4F1CDB2F0CBAFAE5C20CEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CAD
      7342FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFE5A16FE29760E08F54DF8B4DDE8848DE
      8646DD8443DD813DDC7E38DA7A34D76F29CD6D27FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF5AB5D45AB5D4ACF1FA9DEBFC94E5FA78D9
      F75BCAF164BEDFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9BB8F34A
      82F4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      60A2605598554E914E468B463F843F387D383177312F752F2C722C296F29266D
      26FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCE6300
      CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE6300CE63
      00CE6300FF00FFFF00FFFF00FFFF00FFFF00FFBAD77EBAD77EB3D171AED87D75
      D4763ACD6926BF4E92A11DECA428DF8B03FF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFD6BFDF302FFE700FFCC00FFAF00FF9100FF7200FF4D00FF2B03FF706C
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6475F81C35F5142EF5142EF514
      2EF5142EF5142EF52139F47482EEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      E7AB79FBF4F0FBF4EFFAF3EFFAF3EFF8F2EFF7F2EFF7F2EFD8C2C0A56B5FC183
      6CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFC26D43C9C9C9CECECED3D3D3D7D7D7DCDCDCDFDFDFE1
      E1E1E2E2E2E5E5E5E6E6E6E8E8E8E9E9E9EBEBEBC26D43FF00FFC26D43C9C9C9
      CECECED3D3D3D7D7D7DCDCDCDFDFDFE1E1E1E2E2E2E5E5E5E6E6E6E8E8E8E9E9
      E9EBEBEBC26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF949494E7E7E7EF
      EFEFBDBDBDA59C9C949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      BD8281BD8281BD8281FBEFE2FBE3CFFBDDC2BD8281FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF13992313992398E1B5BDEED4A7E7C490E0B178D99F49C7791B9D3D1B9D
      3DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF005184005184004EAE004EAE00
      4AA6005184D39050D27D2EC96E1CC76C1CC87129B66826FF00FFFF00FFFF00FF
      FF00FFFF00FF00840031C64A42C65A008400EDCCAED39050D27D2EC96E1CC76C
      1CC87129B66826FF00FFFF00FFFF00FFE7AB79FBF4F0FBF4EFFAF3EFFAF3EFF8
      F2EFF7F2EFF7F2EFD8C2C0A56B5FC1836CFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFCA8D60CA8B5DDBB49A4AB78F45B388927C6DB87A4CC98551FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCA8D60CA8B5DDBB49A4B
      B89248B68D6EC77764B45861B053FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFCA8D60B97F558F75644CB9944AB79070C87B68B65D66B45AFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF62B3585FB05262BB6136
      A87033A66C927C6DB87A4CC98551FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF14AD2514AD25FF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF599B594C8F4C3F843F347A342E74
      2E296F29FF00FFFF00FF66CC33BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BC
      EAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA6BCEAA666CC33FF00FFBFA4A4
      BFA4A4BFA4A4BFA4A4BFA4A4BA9693C6A08FB58E88FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CAD
      7342FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CCEA58CAD7342
      FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA5
      8CAD7342FF00FFCEA58CCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CAD
      7342FF00FFCEA58CAD7342FF00FFCEA58CAD7342FF00FFCEA58CFF00FFFF00FF
      FF00FFFF00FFFF00FF007000FF00FFFF00FF007000FF00FF007000FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      E8C4ADEBCBB7EBCBB7EACBB7EACAB6EACAB6EACAB6EACAB6E3C2B1A56B5FFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF046B0B046B0B04
      6B0B046B0BFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF7F2B287F2B2876100FFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      3184FF3184FF3184FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF6B6363848C8C948C8CFF00FF848C
      8C948C8C948C8CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFA77E70A98073A4786EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFF7EFE7F7F7EFF7EFEFF7EFEFF7
      EFEFF7EFEFF7EFEFF7EFEFF7EFE7FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFD9B490E5BB8BCD9248A87943FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF5EA05E529552498D49408540387D383076302E74
      2E2A702A276D27FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE7C5BAE6
      C3B7E6C2B5E6C2B3E9C4B5ECC7B5EAD4BDE9D5BCDECEBEFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FF756257FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFE1A577D1926DD1926DD1926DD1926DD1926DA0675BEB
      9E43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF585858585858585858585858585858FF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCF5C0A
      D46C1CD46E1ED46E1ED36A18D16513CF5C0ACC5504CB5101CA5000CB5000BA48
      00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFCEAD94CEAD9CDEBDA5DEBDA5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEAD94CEAD9CDEBDA5DEBDA5FF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFCEAD94CEAD9CDEBDA5DEBDA5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEAD94CEAD9CDEBDA5DEBDA5FF00
      FFFF00FFFF00FFFF00FFFF00FFE7AB79CF8E68CF8E68CF8E68CF8E68CF8E68CF
      8E68CF8E68CF8E68A0675BFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE5A16FE5A16F
      E29760E08F54DF8B4DE08F54DE8646DE8848DE8646DD8443DD813DDC7E38DA7A
      34D76F29CD6D27AE5C20FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5AB5D45AB5D45AB5D45AB5
      D45EC6EAFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC6E7ADC6E7ADB7
      D47EC0CA6ABDD58B92A11DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFD6CFFF22AFFD90AFFB80AFFA52AFFA96DFFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF7887F94F63F73A
      50F65467F78391F9FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      E7AB79D1926DD1926DD1926DD1926DD1926DD1926DD1926DD1926DA56B5FFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFC26D43C26D43C26D43C26D43C26D43C26D43C26D43C2
      6D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43FF00FFC26D43C26D43
      C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D43C26D
      43C26D43C26D43FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF94949494
      9494949494949494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFBD8281BD8281BD8281FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF1DA435139923139923139923139923138C2AFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF005B9400518400518400
      5184FF00FFFF00FFFF00FFFF00FFE2C9AEE1B581B78345FF00FFFF00FFFF00FF
      FF00FF00840000840021A53121A531FF00FFFF00FFFF00FFFF00FFFF00FFE2C9
      AEE1B581B78345FF00FFFF00FFFF00FFE7AB79D1926DD1926DD1926DD1926DD1
      926DD1926DD1926DD1926DA56B5FFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFD1997070BD6C6BB965BE895FFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD1997070
      BD6C6DBB676CBC67FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFC08C6770BD6C6EBB696DBD6AFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF61B5585C
      AE4D58AA46BE895FFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FF66CC3366CC3366CC3366CC3366CC3366CC3366CC3366
      CC3366CC3366CC3366CC3366CC3366CC3366CC3366CC3366CC33FF00FFFF00FF
      FF00FFFF00FFFF00FFBFA4A4BFA4A4BFA4A4FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FF007000007000FF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    Left = 244
    Top = 44
  end
  object ilActionsMiddle: TImageList
    Height = 24
    Width = 24
    Left = 316
    Top = 44
  end
  object ilActionsLarge: TImageList
    Height = 32
    Width = 32
    Left = 388
    Top = 44
  end
  object pmPhoaView: TTBXPopupMenu
    Images = ilActionsSmall
    Left = 304
    Top = 104
    object gipmPhoaView: TTBGroupItem
      object iPhoaView_SetDefault: TTBXItem
        Caption = '&Picture groups'
        Hint = 'Picture groups|Display photo album picture group tree'
        ImageIndex = 36
        ShortCut = 16455
        OnClick = SetPhoaViewClick
      end
      object iPhoaViewSep1: TTBXSeparatorItem
      end
      object gipmPhoaViewViews: TTBGroupItem
      end
      object iPhoaViewSep2: TTBXSeparatorItem
      end
      object iPhoaView_New: TTBXItem
        Action = aPhoaView_New
      end
      object iPhoaView_Delete: TTBXItem
        Action = aPhoaView_Delete
      end
      object iPhoaView_Edit: TTBXItem
        Action = aPhoaView_Edit
      end
      object iPhoaViewSep3: TTBXSeparatorItem
      end
      object iPhoaView_MakeGroup: TTBXItem
        Action = aPhoaView_MakeGroup
      end
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'mruOpen.Prefix')
    StoreList.Strings = (
      '*.ChevronHint')
    OnLanguageChanged = dklcMainLanguageChanged
    Left = 292
    Top = 168
    LangData = {
      0500664D61696E010100000003000000070043617074696F6E01B00000000500
      646B546F700000060074624D61696E010200000008000000070043617074696F
      6E090000000B0043686576726F6E48696E74000400624E657700000500624F70
      656E00000800624F70656E4D5255000005006253617665000007006253617665
      41730000060074625365703100000900624E657747726F757000000700624E65
      77506963000005006245646974000007006244656C6574650000060074625365
      70320000050062556E646F00000D00756C546F6F6C626172556E646F00000F00
      7462786C546F6F6C626172556E646F00000400624375740000050062436F7079
      0000060062506173746500000600746253657033000005006246696E64000005
      00625669657700000600746253657034000009006253657474696E677300000D
      006248656C70436F6E74656E74730000050062457869740000060074624D656E
      75010200000011000000070043617074696F6E120000000B0043686576726F6E
      48696E74000600736D46696C65010100000013000000070043617074696F6E00
      0400694E657700000500694F70656E0000050069536176650000070069536176
      654173000009006946696C65536570310000100069496E695361766553657474
      696E67730000100069496E694C6F616453657474696E6773000009006946696C
      6553657032000005006945786974000009006946696C65536570330000090073
      6D46696C654D525500000600736D456469740101000000180000000700436170
      74696F6E00050069556E646F00000D00736D556E646F486973746F7279010200
      00001A000000070043617074696F6E1B000000040048696E7400090069456469
      745365703100000400694375740000050069436F707900000600695061737465
      0000090069456469745365703200000900694E657747726F757000000700694E
      6577506963000007006944656C65746500000900694564697453657033000005
      006945646974000009006945646974536570340000090069536F727450696373
      00000A006953656C656374416C6C00000B006953656C6563744E6F6E65000005
      00695669657700000600736D5669657701010000002000000007004361707469
      6F6E000E0069546F67676C65546F6F6C62617201020000002200000007004361
      7074696F6E23000000040048696E7400100069546F67676C6553746174757362
      6172010200000024000000070043617074696F6E25000000040048696E740014
      006952656D6F7665536561726368526573756C747300000A0074625669657753
      65703100000D006769736D56696577566965777300000700736D546F6F6C7301
      0100000027000000070043617074696F6E0009006953657474696E6773000006
      0062537461747300000700695069634F707300000F006946696C654F70657261
      74696F6E7300000A0069546F6F6C7353657031000005006946696E6400000A00
      69546F6F6C7353657032000011006769546F6F6C735F546F6F6C734D656E7500
      000600736D48656C7001010000002B000000070043617074696F6E0006006941
      626F757400000D006948656C70436F6E74656E7473000008006948656C704641
      5100001000746253657048656C7057656273697465000013006948656C705072
      6F647563745765627369746500000800646B426F74746F6D00000600646B4C65
      667400000800647047726F757073010100000034000000070043617074696F6E
      000800747647726F75707300000700646B526967687400000800736261724D61
      696E00000600616C4D61696E000014006152656D6F7665536561726368526573
      756C747301030000003E000000070043617074696F6E3D000000080043617465
      676F727940000000040048696E74000D006150686F61566965775F4E65770103
      00000042000000070043617074696F6E41000000080043617465676F72794400
      0000040048696E74000400614E6577010300000046000000070043617074696F
      6E45000000080043617465676F727948000000040048696E74000500614F7065
      6E01030000004A000000070043617074696F6E49000000080043617465676F72
      794C000000040048696E740010006150686F61566965775F44656C6574650103
      0000004E000000070043617074696F6E4D000000080043617465676F72795000
      0000040048696E74000500615361766501030000005200000007004361707469
      6F6E51000000080043617465676F727954000000040048696E74000700615361
      76654173010300000056000000070043617074696F6E55000000080043617465
      676F727958000000040048696E7400100061496E695361766553657474696E67
      7301030000005A000000070043617074696F6E59000000080043617465676F72
      795C000000040048696E7400100061496E694C6F616453657474696E67730103
      0000005E000000070043617074696F6E5D000000080043617465676F72796000
      0000040048696E74000500614578697401030000006200000007004361707469
      6F6E61000000080043617465676F727964000000040048696E7400050061556E
      646F010300000066000000070043617074696F6E65000000080043617465676F
      727968000000040048696E74000900614E657747726F757001030000006A0000
      00070043617074696F6E69000000080043617465676F72796C00000004004869
      6E74000700614E657750696301030000006E000000070043617074696F6E6D00
      0000080043617465676F727970000000040048696E740007006144656C657465
      010300000072000000070043617074696F6E71000000080043617465676F7279
      74000000040048696E7400040061437574010300000076000000070043617074
      696F6E75000000080043617465676F727978000000040048696E740005006143
      6F707901030000007A000000070043617074696F6E7900000008004361746567
      6F72797C000000040048696E7400060061506173746501030000007E00000007
      0043617074696F6E7D000000080043617465676F727980000000040048696E74
      0005006145646974010300000082000000070043617074696F6E810000000800
      43617465676F727984000000040048696E7400090061536F7274506963730103
      00000086000000070043617074696F6E85000000080043617465676F72798800
      0000040048696E74000A006153656C656374416C6C01030000008A0000000700
      43617074696F6E89000000080043617465676F72798C000000040048696E7400
      0B006153656C6563744E6F6E6501030000008E000000070043617074696F6E8D
      000000080043617465676F727990000000040048696E74000500615669657701
      0300000092000000070043617074696F6E91000000080043617465676F727994
      000000040048696E740009006153657474696E67730103000000960000000700
      43617074696F6E95000000080043617465676F727998000000040048696E7400
      060061537461747301030000009A000000070043617074696F6E990000000800
      43617465676F72799C000000040048696E74000700615069634F707301030000
      009E000000070043617074696F6E9D000000080043617465676F7279A0000000
      040048696E74000F006146696C654F7065726174696F6E730103000000A20000
      00070043617074696F6EA1000000080043617465676F7279A400000004004869
      6E740005006146696E640103000000A6000000070043617074696F6EA5000000
      080043617465676F7279A8000000040048696E740006006141626F7574010300
      0000AA000000070043617074696F6EA9000000080043617465676F7279AC0000
      00040048696E74000D006148656C70436F6E74656E74730103000000AE000000
      070043617074696F6EAD000000080043617465676F7279B0000000040048696E
      74000E006150686F61566965775F456469740103000000B20000000700436170
      74696F6EB1000000080043617465676F7279B4000000040048696E7400130061
      50686F61566965775F4D616B6547726F75700103000000B60000000700436170
      74696F6EB5000000080043617465676F7279B8000000040048696E7400080061
      48656C704641510103000000BA000000070043617074696F6EB9000000080043
      617465676F7279BC000000040048696E740013006148656C7050726F64756374
      576562736974650103000000BE000000070043617074696F6EBD000000080043
      617465676F7279C0000000040048696E74000800706D47726F75707300000F00
      69706D47726F75707344656C65746500000D0069706D47726F75707345646974
      00000D0069706D47726F757073536570310000110069706D47726F7570734E65
      7747726F757000000F0069706D47726F7570734E657750696300000D0069706D
      47726F757073536570320000110069706D47726F757073536F72745069637300
      000E0069706D47726F757073537461747300000F0069706D47726F7570735069
      634F70730000170069706D47726F75707346696C654F7065726174696F6E7300
      000D0069706D47726F75707353657033000012006769546F6F6C735F47726F75
      70734D656E7500000600706D5069637300000B0069706D506963735669657700
      000B0069706D506963735365703100000A0069706D5069637343757400000B00
      69706D50696373436F707900000C0069706D50696373506173746500000B0069
      706D506963735365703200000B0069706D506963734564697400000D0069706D
      5069637344656C65746500000B0069706D506963735365703300000D0069706D
      506963734E65775069630000100069706D5069637353656C656374416C6C0000
      110069706D5069637353656C6563744E6F6E650000150069706D506963734669
      6C654F7065726174696F6E7300000B0069706D50696373536570340000100067
      69546F6F6C735F506963734D656E75000007006D72754F70656E00000600706D
      566965770000060066704D61696E00000E00696C416374696F6E73536D616C6C
      00000F00696C416374696F6E734D6964646C6500000E00696C416374696F6E73
      4C6172676500000A00706D50686F615669657700000C006769706D50686F6156
      696577000014006950686F61566965775F53657444656661756C740102000000
      CC000000070043617074696F6ECD000000040048696E74000D006950686F6156
      69657753657031000011006769706D50686F6156696577566965777300000D00
      6950686F61566965775365703200000D006950686F61566965775F4E65770000
      10006950686F61566965775F44656C65746500000E006950686F61566965775F
      4564697400000D006950686F615669657753657033000013006950686F615669
      65775F4D616B6547726F75700000090061466C61744D6F64650103000000CF00
      0000070043617074696F6ECE000000080043617465676F7279D0000000040048
      696E7400090069466C61744D6F6465000012006148656C7056656E646F725765
      62736974650103000000D2000000070043617074696F6ED10000000800436174
      65676F7279D3000000040048696E740011006148656C70436865636B55706461
      7465730103000000D5000000070043617074696F6ED400000008004361746567
      6F7279D6000000040048696E74000E00736D48656C70496E7465726E65740101
      000000D7000000070043617074696F6E0012006948656C7056656E646F725765
      6273697465000011006948656C70436865636B5570646174657300000C006148
      656C70537570706F72740103000000D9000000070043617074696F6ED8000000
      080043617465676F7279DA000000040048696E74000C006948656C7053757070
      6F727400000E006156696577536C69646553686F770103000000DC0000000700
      43617074696F6EDB000000080043617465676F7279DD000000040048696E7400
      090069456469745365703500000E006956696577536C69646553686F770000}
  end
end
