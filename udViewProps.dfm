inherited dViewProps: TdViewProps
  Left = 370
  Top = 189
  Caption = 'Properties: photo album view'
  ClientHeight = 435
  ClientWidth = 592
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 398
    Width = 592
  end
  object pcMain: TPageControl [1]
    Left = 4
    Top = 4
    Width = 583
    Height = 390
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = pcMainChange
    object tsGeneral: TTabSheet
      Caption = 'General'
      DesignSize = (
        575
        362)
      object lName: TLabel
        Left = 8
        Top = 4
        Width = 27
        Height = 13
        Caption = '&Name:'
        FocusControl = eName
      end
      object lGrouping: TLabel
        Left = 8
        Top = 44
        Width = 107
        Height = 13
        Caption = '&Picture grouping order:'
      end
      object eName: TEdit
        Left = 8
        Top = 20
        Width = 557
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = DlgDataChange
      end
      object tvGrouping: TVirtualStringTree
        Left = 8
        Top = 60
        Width = 556
        Height = 137
        Anchors = [akLeft, akTop, akRight, akBottom]
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
        Header.ParentFont = True
        Images = fMain.ilActionsSmall
        PopupMenu = pmGrouping
        TabOrder = 1
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
            Width = 402
            WideText = 'Grouping property'
          end
          item
            Alignment = taCenter
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 1
            Width = 150
            WideText = 'Unclassified in own folder'
          end>
        WideDefaultText = ''
      end
      inline frSorting: TfrSorting
        Left = 8
        Top = 200
        Width = 556
        Height = 150
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 2
        inherited lMain: TLabel
          Width = 556
          Caption = '&Sort pictures in each group by:'
        end
        inherited tvMain: TVirtualStringTree
          Width = 556
          Height = 135
          Header.Font.Name = 'Tahoma'
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
              Position = 0
              Width = 402
              WideText = 'Picture property'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
              Position = 1
              Width = 150
              WideText = 'Sort direction'
            end>
          WideDefaultText = ''
        end
      end
    end
    object tsFilterExpr: TTabSheet
      Caption = 'Filter expression'
      ImageIndex = 1
      inline frExprPicFilter: TfrExprPicFilter
        Left = 0
        Top = 0
        Width = 575
        Height = 362
        Align = alClient
        TabOrder = 0
        inherited dkExprTop: TTBXDock
          Width = 575
        end
        inherited eExpression: TSynEdit
          Width = 575
        end
        inherited dklcMain: TDKLanguageController
          Options = [dklcoIgnoreEmptyProps]
          LangData = {
            0F0066724578707250696346696C7465720001200000000900646B4578707254
            6F7000000A007462457870724D61696E01010000000100000007004361707469
            6F6E000400624E657700000500624F70656E00000800694D52554F70656E0000
            07006253617665417300000F007462536570496E7365727450726F7000000C00
            736D496E7365727450726F70010200000002000000070043617074696F6E0300
            0000040048696E74001000736D496E736572744F70657261746F720102000000
            04000000070043617074696F6E05000000040048696E74000800746253657043
            757400000400624375740000050062436F707900000600625061737465000009
            007462536570556E646F0000050062556E646F00000500625265646F00001000
            746253657053796E746178436865636B00000C006253796E746178436865636B
            00000B006545787072657373696F6E00000600616C4D61696E00000400614E65
            77010200000009000000070043617074696F6E0A000000040048696E74000500
            614F70656E01020000000C000000070043617074696F6E0D000000040048696E
            740007006153617665417301020000000F000000070043617074696F6E100000
            00040048696E7400040061437574010200000012000000070043617074696F6E
            13000000040048696E7400050061436F70790102000000150000000700436170
            74696F6E16000000040048696E74000600615061737465010200000018000000
            070043617074696F6E19000000040048696E7400050061556E646F0102000000
            1B000000070043617074696F6E1C000000040048696E74000500615265646F01
            020000001E000000070043617074696F6E1F000000040048696E74000C006153
            796E746178436865636B010200000021000000070043617074696F6E22000000
            040048696E74000C00706D45787072657373696F6E000007006D72754F70656E
            000007007363704D61696E01010000002600000005005469746C6500}
        end
      end
    end
  end
  inherited pButtonsBottom: TPanel
    Top = 400
    Width = 592
    TabOrder = 1
    DesignSize = (
      592
      35)
    inherited bCancel: TButton
      Left = 432
      TabOrder = 1
    end
    inherited bOK: TButton
      Left = 352
      TabOrder = 0
    end
    inherited bHelp: TButton
      Left = 510
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'frExprPicFilter.*')
    StoreList.Strings = (
      'frSorting.tvMain.Header.Columns*'
      'tvGrouping.Header.Columns*')
    Left = 32
    Top = 404
    LangData = {
      0A00645669657750726F7073010100000003000000070043617074696F6E0114
      00000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D00
      0007006243616E63656C01010000000C000000070043617074696F6E00030062
      4F4B01010000000F000000070043617074696F6E0005006248656C7001010000
      0012000000070043617074696F6E0005006C4E616D6501010000001500000007
      0043617074696F6E0009006C47726F7570696E67010100000018000000070043
      617074696F6E000500654E616D6500000A00747647726F7570696E6701020000
      003900000016004865616465722E436F6C756D6E735B305D2E546578743A0000
      0016004865616465722E436F6C756D6E735B315D2E546578740009006672536F
      7274696E6700010800000005006C4D61696E0101000000240000000700436170
      74696F6E00060074764D61696E01020000003B00000016004865616465722E43
      6F6C756D6E735B305D2E546578743C00000016004865616465722E436F6C756D
      6E735B315D2E54657874000600706D4D61696E0000090069706D736D50726F70
      010100000028000000070043617074696F6E00090069706D44656C6574650101
      0000002A000000070043617074696F6E00060069706D5365700000090069706D
      4D6F7665557001010000002D000000070043617074696F6E000B0069706D4D6F
      7665446F776E01010000002F000000070043617074696F6E000A00706D47726F
      7570696E670000090069706D736D50726F700101000000310000000700436170
      74696F6E00090069706D44656C65746501010000003300000007004361707469
      6F6E00060069706D5365700000090069706D4D6F766555700101000000360000
      00070043617074696F6E000B0069706D4D6F7665446F776E0101000000380000
      00070043617074696F6E00060070634D61696E00000900747347656E6572616C
      01010000003D000000070043617074696F6E000C00747346696C746572457870
      7201010000003E000000070043617074696F6E000F0066724578707250696346
      696C7465720001200000000900646B45787072546F7000000A00746245787072
      4D61696E00000400624E657700000500624F70656E00000800694D52554F7065
      6E000007006253617665417300000F007462536570496E7365727450726F7000
      000C00736D496E7365727450726F7000001000736D496E736572744F70657261
      746F7200000800746253657043757400000400624375740000050062436F7079
      00000600625061737465000009007462536570556E646F0000050062556E646F
      00000500625265646F00001000746253657053796E746178436865636B00000C
      006253796E746178436865636B00000B006545787072657373696F6E00000600
      616C4D61696E00000400614E657700000500614F70656E000007006153617665
      417300000400614375740000050061436F707900000600615061737465000005
      0061556E646F00000500615265646F00000C006153796E746178436865636B00
      000C00706D45787072657373696F6E000007006D72754F70656E000007007363
      704D61696E0000}
  end
  object pmGrouping: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 4
    Top = 404
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
