inherited dSelKeywords: TdSelKeywords
  Caption = 'Select keywords'
  ClientWidth = 354
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lMain: TTntLabel [0]
    Left = 12
    Top = 12
    Width = 329
    Height = 25
    AutoSize = False
    Caption = '&Select one or more keywords from the complete list:'
    WordWrap = True
  end
  inherited bvBottom: TBevel
    Width = 354
  end
  object bReset: TTntButton [2]
    Left = 8
    Top = 403
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Reset'
    TabOrder = 0
    OnClick = bResetClick
  end
  object tvMain: TVirtualStringTree [3]
    Left = 12
    Top = 40
    Width = 331
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Header.ParentFont = True
    Images = fMain.ilActionsSmall
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    TreeOptions.StringOptions = [toSaveCaptions, toShowStaticText, toAutoAcceptEditChange]
    OnChecked = tvMainChecked
    OnGetText = tvMainGetText
    OnPaintText = tvMainPaintText
    OnGetImageIndex = tvMainGetImageIndex
    OnInitNode = tvMainInitNode
    Columns = <>
    WideDefaultText = ''
  end
  inherited pButtonsBottom: TTntPanel
    Width = 354
    TabOrder = 2
    DesignSize = (
      354
      35)
    inherited bCancel: TTntButton
      Left = 195
    end
    inherited bOK: TTntButton
      Left = 115
    end
    inherited bHelp: TTntButton
      Left = 273
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Top = 400
    LangData = {
      0C006453656C4B6579776F726473010100000003000000070043617074696F6E
      010800000008006276426F74746F6D00000E0070427574746F6E73426F74746F
      6D000007006243616E63656C01010000000C000000070043617074696F6E0003
      00624F4B01010000000F000000070043617074696F6E0005006248656C700101
      00000012000000070043617074696F6E0005006C4D61696E0101000000150000
      00070043617074696F6E00060062526573657401010000001800000007004361
      7074696F6E00060074764D61696E0000}
  end
end
