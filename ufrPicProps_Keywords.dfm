inherited frPicProps_Keywords: TfrPicProps_Keywords
  Height = 298
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 22
    Width = 576
    Height = 276
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Header.ParentFont = True
    Images = fMain.ilActionsSmall
    ParentBackground = False
    PopupMenu = pmMain
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    TreeOptions.StringOptions = [toSaveCaptions, toShowStaticText, toAutoAcceptEditChange]
    OnChange = tvMainChange
    OnChecked = tvMainChecked
    OnChecking = tvMainChecking
    OnEdited = tvMainEdited
    OnEditing = tvMainEditing
    OnGetText = tvMainGetText
    OnPaintText = tvMainPaintText
    OnGetImageIndex = tvMainGetImageIndex
    OnInitNode = tvMainInitNode
    OnNewText = tvMainNewText
    Columns = <>
    WideDefaultText = ''
  end
  object tbMain: TTBXToolbar
    Left = 0
    Top = 0
    Width = 576
    Height = 22
    Align = alTop
    ChevronHint = 'More buttons|'
    Images = fMain.ilActionsSmall
    SystemFont = False
    TabOrder = 1
    object bCheckedOnly: TTBXItem
      Action = aCheckedOnly
      DisplayMode = nbdmImageAndText
    end
    object bAdd: TTBXItem
      Action = aAdd
      DisplayMode = nbdmImageAndText
      ShortCut = 45
    end
    object bEdit: TTBXItem
      Action = aEdit
      DisplayMode = nbdmImageAndText
      ShortCut = 32781
    end
    object tbSep1: TTBXSeparatorItem
    end
    object bCheckAll: TTBXItem
      Action = aCheckAll
      DisplayMode = nbdmImageAndText
    end
    object bUncheckAll: TTBXItem
      Action = aUncheckAll
      DisplayMode = nbdmImageAndText
    end
    object bInvertCheck: TTBXItem
      Action = aInvertCheck
      DisplayMode = nbdmImageAndText
    end
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 40
    Top = 252
    object aCheckedOnly: TAction
      AutoCheck = True
      Caption = 'C&hecked only'
      Hint = 'Checked only|Display only checked keywords'
      ImageIndex = 41
      OnExecute = aaCheckedOnly
    end
    object aAdd: TAction
      Caption = '&Add'
      Hint = 'Add a keyword|Add new keyword to selected pictures'
      ImageIndex = 0
      OnExecute = aaAdd
    end
    object aEdit: TAction
      Caption = '&Edit'
      Hint = 'Edit keyword|Edit selected keyword'
      ImageIndex = 8
      OnExecute = aaEdit
    end
    object aCheckAll: TAction
      Caption = '&Check all'
      Hint = 'Check all|Check all the keywords in the list'
      ImageIndex = 13
      OnExecute = aaCheckAll
    end
    object aUncheckAll: TAction
      Caption = '&Uncheck all'
      Hint = 'Uncheck all|Remove check from all the keywords in the list'
      ImageIndex = 14
      OnExecute = aaUncheckAll
    end
    object aInvertCheck: TAction
      Caption = '&Invert'
      Hint = 'Invert check|Toggle keyword check'
      ImageIndex = 40
      OnExecute = aaInvertCheck
    end
  end
  object pmMain: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 68
    Top = 252
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      'Font.Name'
      'SecondaryShortCuts'
      'tvMain.Header.Font.Name')
    Left = 12
    Top = 252
    LangData = {
      1300667250696350726F70735F4B6579776F726473000111000000060074624D
      61696E0101000000090000000B0043686576726F6E48696E7400060074625365
      703100000500624564697400000600616C4D61696E0000050061456469740102
      00000082000000070043617074696F6E84000000040048696E7400060074764D
      61696E00000C0062436865636B65644F6E6C7900000400624164640000090062
      436865636B416C6C00000B0062556E636865636B416C6C00000C0062496E7665
      7274436865636B00000C0061436865636B65644F6E6C79010200000089000000
      070043617074696F6E8B000000040048696E740004006141646401020000008D
      000000070043617074696F6E8F000000040048696E7400090061436865636B41
      6C6C010200000091000000070043617074696F6E93000000040048696E74000B
      0061556E636865636B416C6C010200000095000000070043617074696F6E9700
      0000040048696E74000C0061496E76657274436865636B010200000099000000
      070043617074696F6E9B000000040048696E74000600706D4D61696E0000}
  end
end
