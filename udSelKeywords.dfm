inherited dSelKeywords: TdSelKeywords
  Left = 601
  Top = 198
  Caption = 'Select keywords'
  ClientHeight = 431
  ClientWidth = 354
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lMain: TLabel [0]
    Left = 12
    Top = 12
    Width = 329
    Height = 25
    AutoSize = False
    Caption = '&Select one or more keywords from the complete list:'
    WordWrap = True
  end
  inherited bvBottom: TBevel
    Top = 394
    Width = 354
  end
  object bReset: TButton [2]
    Left = 8
    Top = 399
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
    Height = 341
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = fMain.ilActionsSmall
    ParentBackground = False
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
  inherited pButtonsBottom: TPanel
    Top = 396
    Width = 354
    TabOrder = 2
    DesignSize = (
      354
      35)
    inherited bCancel: TButton
      Left = 195
    end
    inherited bOK: TButton
      Left = 115
    end
    inherited bHelp: TButton
      Left = 273
    end
  end
  inherited dklcMain: TDKLanguageController
    Left = 40
    Top = 40
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
