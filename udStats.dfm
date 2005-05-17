inherited dStats: TdStats
  Caption = 'Photo album statistics'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object tvMain: TVirtualStringTree [1]
    Left = 12
    Top = 12
    Width = 609
    Height = 373
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
    Header.ParentFont = True
    HintMode = hmTooltip
    Images = fMain.ilActionsSmall
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    OnFreeNode = tvMainFreeNode
    OnGetText = tvMainGetText
    OnPaintText = tvMainPaintText
    OnGetImageIndex = tvMainGetImageIndex
    Columns = <
      item
        Color = 16250871
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 300
      end
      item
        Position = 1
        Width = 305
      end>
  end
  inherited pButtonsBottom: TPanel
    TabOrder = 1
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'fpMain.IniFileName'
      'fpMain.IniSection')
    Top = 400
    LangData = {
      0600645374617473010100000003000000070043617074696F6E010600000008
      006276426F74746F6D00000E0070427574746F6E73426F74746F6D0000070062
      43616E63656C01010000000C000000070043617074696F6E000300624F4B0101
      0000000F000000070043617074696F6E0005006248656C700101000000120000
      00070043617074696F6E00060074764D61696E0000}
  end
end
