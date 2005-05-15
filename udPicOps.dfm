inherited dPicOps: TdPicOps
  ActiveControl = cbOp
  Caption = 'Picture operations'
  PixelsPerInch = 96
  TextHeight = 13
  object lGroup: TLabel [1]
    Left = 12
    Top = 52
    Width = 33
    Height = 13
    Caption = '&Group:'
    FocusControl = tvGroups
  end
  object lOp: TLabel [2]
    Left = 12
    Top = 12
    Width = 52
    Height = 13
    Caption = '&Operation:'
    FocusControl = cbOp
  end
  inherited pButtonsBottom: TPanel
    inherited bCancel: TButton
      Left = 472
    end
    inherited bOK: TButton
      Left = 392
    end
    inherited bHelp: TButton
      Left = 550
    end
  end
  object cbOp: TComboBox
    Left = 12
    Top = 28
    Width = 608
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    ItemHeight = 13
    TabOrder = 1
    OnChange = DlgDataChange
    Items.Strings = (
      'Move selected pictures to the group specified below'
      'Copy selected pictures to the group specified below'
      'Delete selected pictures from the group specified below'
      
        'Leave only selected pictures to the group specified below (inter' +
        'sect)')
  end
  object tvGroups: TVirtualStringTree
    Left = 12
    Top = 68
    Width = 608
    Height = 317
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
    TabOrder = 2
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.StringOptions = [toShowStaticText, toAutoAcceptEditChange]
    OnBeforeItemErase = tvGroupsBeforeItemErase
    OnChange = tvGroupsChange
    OnFreeNode = tvGroupsFreeNode
    OnGetText = tvGroupsGetText
    OnPaintText = tvGroupsPaintText
    OnGetImageIndex = tvGroupsGetImageIndex
    OnGetHint = tvGroupsGetHint
    OnInitNode = tvGroupsInitNode
    Columns = <>
    WideDefaultText = 'Photo Album'
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Top = 400
    LangData = {
      0700645069634F7073010100000003000000070043617074696F6E0109000000
      08006276426F74746F6D00000E0070427574746F6E73426F74746F6D00000700
      6243616E63656C01010000000C000000070043617074696F6E000300624F4B01
      010000000F000000070043617074696F6E0005006248656C7001010000001200
      0000070043617074696F6E0006006C47726F7570010100000015000000070043
      617074696F6E0003006C4F70010100000018000000070043617074696F6E0004
      0063624F7001010000001D00000005004974656D73000800747647726F757073
      0000}
  end
end
