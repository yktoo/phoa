inherited dSelPhoaGroup: TdSelPhoaGroup
  Left = 508
  Top = 182
  Caption = 'Select picture group'
  ClientHeight = 435
  ClientWidth = 473
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lGroup: TLabel [0]
    Left = 12
    Top = 12
    Width = 33
    Height = 13
    Caption = '&Group:'
    FocusControl = tvGroups
  end
  inherited bvBottom: TBevel
    Top = 398
    Width = 473
  end
  object tvGroups: TVirtualStringTree [2]
    Left = 12
    Top = 28
    Width = 449
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultNodeHeight = 16
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = fMain.ilActionsSmall
    ParentBackground = False
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.StringOptions = [toShowStaticText, toAutoAcceptEditChange]
    OnChange = tvGroupsChange
    OnGetText = tvGroupsGetText
    OnPaintText = tvGroupsPaintText
    OnGetImageIndex = tvGroupsGetImageIndex
    OnInitNode = tvGroupsInitNode
    Columns = <>
    WideDefaultText = 'Photo Album'
  end
  inherited pButtonsBottom: TPanel
    Top = 400
    Width = 473
    TabOrder = 1
    DesignSize = (
      473
      35)
    inherited bCancel: TButton
      Left = 313
    end
    inherited bOK: TButton
      Left = 233
    end
    inherited bHelp: TButton
      Left = 391
    end
  end
  inherited dklcMain: TDKLanguageController
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
