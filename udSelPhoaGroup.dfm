inherited dSelPhoaGroup: TdSelPhoaGroup
  ActiveControl = tvGroups
  Caption = 'Select picture group'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lGroup: TTntLabel [0]
    Left = 12
    Top = 12
    Width = 33
    Height = 13
    Caption = '&Group:'
    FocusControl = tvGroups
  end
  object tvGroups: TVirtualStringTree [2]
    Left = 12
    Top = 28
    Width = 608
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultNodeHeight = 16
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Header.ParentFont = True
    Images = fMain.ilActionsSmall
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.StringOptions = [toShowStaticText, toAutoAcceptEditChange]
    OnBeforeCellPaint = tvGroupsBeforeCellPaint
    OnBeforeItemErase = tvGroupsBeforeItemErase
    OnChange = tvGroupsChange
    OnCollapsed = tvGroupsExpandedCollapsed
    OnCollapsing = tvGroupsCollapsing
    OnExpanded = tvGroupsExpandedCollapsed
    OnFreeNode = tvGroupsFreeNode
    OnGetText = tvGroupsGetText
    OnPaintText = tvGroupsPaintText
    OnGetImageIndex = tvGroupsGetImageIndex
    OnGetHint = tvGroupsGetHint
    OnInitNode = tvGroupsInitNode
    Columns = <>
    WideDefaultText = 'Photo Album'
  end
  inherited pButtonsBottom: TTntPanel
    TabOrder = 1
    inherited bCancel: TTntButton
      Left = 472
    end
    inherited bOK: TTntButton
      Left = 392
    end
    inherited bHelp: TTntButton
      Left = 550
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Top = 404
    LangData = {
      0D006453656C50686F6147726F7570010100000003000000070043617074696F
      6E010700000008006276426F74746F6D00000E0070427574746F6E73426F7474
      6F6D000007006243616E63656C01010000000C000000070043617074696F6E00
      0300624F4B01010000000F000000070043617074696F6E0005006248656C7001
      0100000012000000070043617074696F6E0006006C47726F7570010100000015
      000000070043617074696F6E000800747647726F7570730000}
  end
end
