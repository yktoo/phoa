inherited frWzPageFileOps_SelPics: TfrWzPageFileOps_SelPics
  DesignSize = (
    576
    284)
  object lCountInfo: TLabel
    Left = 0
    Top = 271
    Width = 576
    Height = 13
    Align = alBottom
    Alignment = taCenter
  end
  object rbAllPics: TRadioButton
    Left = 4
    Top = 24
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '<all pics>'
    TabOrder = 1
    OnClick = RBSelPicturesClick
  end
  object rbSelPics: TRadioButton
    Left = 4
    Top = 4
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Selected pictures'
    TabOrder = 0
    OnClick = RBSelPicturesClick
  end
  object rbSelGroups: TRadioButton
    Left = 4
    Top = 44
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Pictures in selected &groups:'
    TabOrder = 2
    OnClick = RBSelPicturesClick
  end
  object tvGroups: TVirtualStringTree
    Left = 24
    Top = 64
    Width = 551
    Height = 105
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultNodeHeight = 16
    Header.AutoSizeIndex = 0
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
    PopupMenu = pmGroups
    TabOrder = 3
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused]
    TreeOptions.StringOptions = [toSaveCaptions, toShowStaticText, toAutoAcceptEditChange]
    OnBeforeItemErase = tvGroupsBeforeItemErase
    OnChecked = tvGroupsChecked
    OnGetText = tvGroupsGetText
    OnPaintText = tvGroupsPaintText
    OnGetImageIndex = tvGroupsGetImageIndex
    OnGetHint = tvGroupsGetHint
    OnInitNode = tvGroupsInitNode
    Columns = <>
    WideDefaultText = 'Photo Album'
  end
  object gbValidity: TGroupBox
    Left = 8
    Top = 176
    Width = 561
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'File link validity filter'
    ParentBackground = False
    TabOrder = 4
    DesignSize = (
      561
      89)
    object rbValidityAny: TRadioButton
      Left = 16
      Top = 20
      Width = 533
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All selected &pictures'
      TabOrder = 0
      OnClick = UpdateCountInfoNotify
    end
    object rbValidityValid: TRadioButton
      Left = 16
      Top = 40
      Width = 533
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Only pictures having &valid (existent) file linked'
      TabOrder = 1
      OnClick = UpdateCountInfoNotify
    end
    object rbValidityInvalid: TRadioButton
      Left = 16
      Top = 60
      Width = 533
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Only pictures having &NO valid (existent) file linked'
      TabOrder = 2
      OnClick = UpdateCountInfoNotify
    end
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 36
    Top = 72
    object aCheckAll: TAction
      Caption = '&Check all groups'
      Hint = 'Set check to all the groups having pictures'
      ImageIndex = 13
      OnExecute = aaCheckAll
    end
    object aUncheckAll: TAction
      Caption = '&Uncheck all groups'
      Hint = 'Remove check from all the groups'
      ImageIndex = 14
      OnExecute = aaUncheckAll
    end
    object aInvertChecks: TAction
      Caption = '&Invert group checks'
      Hint = 'Toggle each group'#39's check'
      ImageIndex = 40
      OnExecute = aaInvertChecks
    end
  end
  object pmGroups: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 84
    Top = 72
    object ipmGroupsCheckAll: TTBXItem
      Action = aCheckAll
      ShortCut = 16449
    end
    object ipmGroupsUncheckAll: TTBXItem
      Action = aUncheckAll
      ShortCut = 16452
    end
    object ipmGroupsInvertChecks: TTBXItem
      Action = aInvertChecks
      ShortCut = 16457
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'rbAllPics.Caption')
    Left = 136
    Top = 72
    LangData = {
      17006672577A5061676546696C654F70735F53656C5069637300011100000008
      00747647726F75707300000600616C4D61696E00000800706D47726F75707300
      000A006C436F756E74496E666F000009007262416C6C50696373000009007262
      53656C50696373010100000040000000070043617074696F6E000B0072625365
      6C47726F757073010100000043000000070043617074696F6E000A0067625661
      6C6964697479010100000046000000070043617074696F6E000D00726256616C
      6964697479416E79010100000049000000070043617074696F6E000F00726256
      616C696469747956616C696401010000004C000000070043617074696F6E0011
      00726256616C6964697479496E76616C696401010000004F0000000700436170
      74696F6E00090061436865636B416C6C01020000005100000007004361707469
      6F6E53000000040048696E74000B0061556E636865636B416C6C010200000055
      000000070043617074696F6E57000000040048696E74000D0061496E76657274
      436865636B73010200000059000000070043617074696F6E5B00000004004869
      6E7400110069706D47726F757073436865636B416C6C0000130069706D47726F
      757073556E636865636B416C6C0000150069706D47726F757073496E76657274
      436865636B730000}
  end
end
