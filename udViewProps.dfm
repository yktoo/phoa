inherited dViewProps: TdViewProps
  Left = 579
  Caption = 'Properties: photo album view'
  ClientHeight = 435
  ClientWidth = 426
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 398
    Width = 426
  end
  object lName: TLabel [1]
    Left = 12
    Top = 12
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = eName
  end
  object lGrouping: TLabel [2]
    Left = 12
    Top = 52
    Width = 111
    Height = 13
    Caption = '&Picture grouping order:'
  end
  inherited pButtonsBottom: TPanel
    Top = 400
    Width = 426
    DesignSize = (
      426
      35)
    inherited bCancel: TButton
      Left = 266
    end
    inherited bOK: TButton
      Left = 186
    end
    inherited bHelp: TButton
      Left = 344
    end
  end
  object eName: TEdit
    Left = 12
    Top = 28
    Width = 403
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = DlgDataChange
  end
  object tvGrouping: TVirtualStringTree
    Left = 12
    Top = 68
    Width = 402
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
    Images = fMain.ilActionsSmall
    ParentBackground = False
    PopupMenu = pmGrouping
    TabOrder = 2
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
    OnAfterCellPaint = tvGroupingAfterCellPaint
    OnChange = tvGroupingChange
    OnChecked = tvGroupingChecked
    OnDragAllowed = tvGroupingDragAllowed
    OnDragOver = tvGroupingDragOver
    OnDragDrop = tvGroupingDragDrop
    OnGetText = tvGroupingGetText
    OnGetImageIndex = tvGroupingGetImageIndex
    OnInitNode = tvGroupingInitNode
    OnKeyAction = tvGroupingKeyAction
    OnMouseDown = tvGroupingMouseDown
    Columns = <
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 248
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 150
      end>
    WideDefaultText = ''
  end
  inline frSorting: TfrSorting
    Left = 12
    Top = 231
    Width = 402
    Height = 154
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    inherited lMain: TLabel
      Width = 402
      Caption = '&Sort pictures in each group by:'
    end
    inherited tvMain: TVirtualStringTree
      Width = 402
      Height = 139
      Columns = <
        item
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
          Position = 0
          Width = 248
        end
        item
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
          Position = 1
          Width = 150
        end>
      WideDefaultText = ''
    end
    inherited pmMain: TTBXPopupMenu
      inherited ipmsmProp: TTBXSubmenuItem
        Caption = '&Picture property'
      end
      inherited ipmDelete: TTBXItem
        Caption = '&Delete'
      end
      inherited ipmMoveUp: TTBXItem
        Caption = 'Move &up'
      end
      inherited ipmMoveDown: TTBXItem
        Caption = 'Move do&wn'
      end
    end
  end
  object dklcMain: TDKLanguageController
    Left = 76
    Top = 104
    LangData = {
      0A00645669657750726F7073010100000003000000070043617074696F6E0110
      00000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D00
      0007006243616E63656C01010000000C000000070043617074696F6E00030062
      4F4B01010000000F000000070043617074696F6E0005006248656C7001010000
      0012000000070043617074696F6E0005006C4E616D6501010000001500000007
      0043617074696F6E0009006C47726F7570696E67010100000018000000070043
      617074696F6E000500654E616D6500000A00747647726F7570696E6700000900
      6672536F7274696E6700010800000005006C4D61696E01010000002400000007
      0043617074696F6E00060074764D61696E00000600706D4D61696E0000090069
      706D736D50726F70010100000028000000070043617074696F6E00090069706D
      44656C65746501010000002A000000070043617074696F6E00060069706D5365
      700000090069706D4D6F7665557001010000002D000000070043617074696F6E
      000B0069706D4D6F7665446F776E01010000002F000000070043617074696F6E
      000A00706D47726F7570696E670000090069706D736D50726F70010100000031
      000000070043617074696F6E00090069706D44656C6574650101000000330000
      00070043617074696F6E00060069706D5365700000090069706D4D6F76655570
      010100000036000000070043617074696F6E000B0069706D4D6F7665446F776E
      010100000038000000070043617074696F6E00}
  end
  object pmGrouping: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 32
    Top = 104
    object ipmsmProp: TTBXSubmenuItem
      Caption = '&Picture property'
    end
    object ipmDelete: TTBXItem
      Caption = '&Delete'
      ImageIndex = 7
      ShortCut = 46
      OnClick = ipmDeleteClick
    end
    object ipmSep: TTBXSeparatorItem
    end
    object ipmMoveUp: TTBXItem
      Caption = 'Move &up'
      ImageIndex = 55
      ShortCut = 16422
      OnClick = ipmMoveUpClick
    end
    object ipmMoveDown: TTBXItem
      Caption = 'Move do&wn'
      ImageIndex = 56
      ShortCut = 16424
      OnClick = ipmMoveDownClick
    end
  end
end
