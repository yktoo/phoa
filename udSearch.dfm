inherited dSearch: TdSearch
  Left = 451
  Top = 212
  Caption = 'Find pictures'
  ClientHeight = 464
  ClientWidth = 640
  OldCreateOrder = True
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 427
    Width = 640
  end
  object gbSearch: TGroupBox [1]
    Left = 12
    Top = 8
    Width = 617
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Perform search'
    TabOrder = 0
    object rbAll: TRadioButton
      Left = 16
      Top = 16
      Width = 137
      Height = 17
      Caption = 'In the &photo album'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = DlgDataChange
    end
    object rbCurGroup: TRadioButton
      Left = 156
      Top = 16
      Width = 145
      Height = 17
      Caption = 'In the &current group'
      TabOrder = 1
      OnClick = DlgDataChange
    end
    object rbSearchResults: TRadioButton
      Left = 304
      Top = 16
      Width = 189
      Height = 17
      Caption = 'In the search &results'
      TabOrder = 2
      OnClick = DlgDataChange
    end
  end
  inherited pButtonsBottom: TPanel
    Top = 429
    Width = 640
    TabOrder = 2
    DesignSize = (
      640
      35)
    inherited bCancel: TButton
      Left = 479
      TabOrder = 1
    end
    inherited bOK: TButton
      Left = 399
      Caption = 'Find'
      TabOrder = 0
    end
    inherited bHelp: TButton
      Left = 557
    end
  end
  object pcCriteria: TPageControl
    Left = 12
    Top = 56
    Width = 616
    Height = 362
    ActivePage = tsExpression
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnChange = pcCriteriaChange
    object tsSimple: TTabSheet
      Caption = 'Simple search'
      object tvSimpleCriteria: TVirtualStringTree
        Left = 0
        Top = 26
        Width = 608
        Height = 308
        Align = alClient
        Header.AutoSizeIndex = 2
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoShowSortGlyphs, hoVisible]
        Header.ParentFont = True
        PopupMenu = pmSimple
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
        OnChecked = tvSimpleCriteriaChecked
        OnCreateEditor = tvSimpleCriteriaCreateEditor
        OnFocusChanged = tvSimpleCriteriaFocusChanged
        OnGetText = tvSimpleCriteriaGetText
        OnPaintText = tvSimpleCriteriaPaintText
        OnInitNode = tvSimpleCriteriaInitNode
        Columns = <
          item
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 0
            Width = 180
            WideText = 'Picture property'
          end
          item
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 1
            Width = 130
            WideText = 'Condition'
          end
          item
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 2
            Width = 298
            WideText = 'Value'
          end>
        WideDefaultText = ''
      end
      object dkSimpleTop: TTBXDock
        Left = 0
        Top = 0
        Width = 608
        Height = 26
        AllowDrag = False
        object tbSimpleMain: TTBXToolbar
          Left = 0
          Top = 0
          Caption = 'Toolbar'
          Images = fMain.ilActionsSmall
          TabOrder = 0
          object bSimpleReset: TTBXItem
            Action = aSimpleReset
          end
          object bSimpleCrDelete: TTBXItem
            Action = aSimpleCrDelete
            DisplayMode = nbdmImageAndText
          end
          object bSimpleConvertToExpression: TTBXItem
            Action = aSimpleConvertToExpression
            DisplayMode = nbdmImageAndText
          end
        end
      end
    end
    object tsExpression: TTabSheet
      Caption = 'Expression search'
      ImageIndex = 1
      inline frExprPicFilter: TfrExprPicFilter
        Left = 0
        Top = 0
        Width = 608
        Height = 334
        Align = alClient
        TabOrder = 0
        inherited dkExprTop: TTBXDock
          Width = 608
        end
        inherited eExpression: TSynEdit
          Width = 608
          Height = 308
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
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'frExprPicFilter.*')
    StoreList.Strings = (
      '*.ChevronHint'
      'tvSimpleCriteria.Header.Columns*')
    Left = 64
    Top = 432
    LangData = {
      070064536561726368010100000003000000070043617074696F6E011A000000
      08006276426F74746F6D00000E0070427574746F6E73426F74746F6D00000700
      6243616E63656C01010000000C000000070043617074696F6E000300624F4B01
      010000000F000000070043617074696F6E0005006248656C7001010000001200
      0000070043617074696F6E001000747653696D706C6543726974657269610103
      0000003C00000016004865616465722E436F6C756D6E735B305D2E546578743D
      00000016004865616465722E436F6C756D6E735B315D2E546578743E00000016
      004865616465722E436F6C756D6E735B325D2E54657874000800676253656172
      636801010000001B000000070043617074696F6E0005007262416C6C01010000
      001E000000070043617074696F6E000A00726243757247726F75700101000000
      21000000070043617074696F6E000F007262536561726368526573756C747301
      0100000024000000070043617074696F6E000A00706343726974657269610000
      0800747353696D706C65010100000028000000070043617074696F6E000C0074
      7345787072657373696F6E010100000029000000070043617074696F6E000800
      706D53696D706C6500000F0069706D736D53696D706C6550726F700101000000
      34000000070043617074696F6E000F0069706D53696D706C6544656C65746500
      000B00646B53696D706C65546F7000000C00746253696D706C654D61696E0102
      00000036000000070043617074696F6E370000000B0043686576726F6E48696E
      74000600616C4D61696E00000F006153696D706C65437244656C657465010200
      000038000000070043617074696F6E39000000040048696E74000F006253696D
      706C65437244656C65746500001A006153696D706C65436F6E76657274546F45
      787072657373696F6E01020000003A000000070043617074696F6E3B00000004
      0048696E74001A006253696D706C65436F6E76657274546F4578707265737369
      6F6E00000C006153696D706C65526573657401020000003F0000000700436170
      74696F6E40000000040048696E74000C006253696D706C65526573657400000F
      0066724578707250696346696C7465720001200000000900646B45787072546F
      7000000A007462457870724D61696E00000400624E657700000500624F70656E
      00000800694D52554F70656E000007006253617665417300000F007462536570
      496E7365727450726F7000000C00736D496E7365727450726F7000001000736D
      496E736572744F70657261746F72000008007462536570437574000004006243
      75740000050062436F707900000600625061737465000009007462536570556E
      646F0000050062556E646F00000500625265646F00001000746253657053796E
      746178436865636B00000C006253796E746178436865636B00000B0065457870
      72657373696F6E00000600616C4D61696E00000400614E657700000500614F70
      656E000007006153617665417300000400614375740000050061436F70790000
      06006150617374650000050061556E646F00000500615265646F00000C006153
      796E746178436865636B00000C00706D45787072657373696F6E000007006D72
      754F70656E000007007363704D61696E0000}
  end
  object pmSimple: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    OnPopup = pmSimplePopup
    Left = 8
    Top = 432
    object ipmsmSimpleProp: TTBXSubmenuItem
      Caption = '&Picture property'
    end
    object ipmSimpleDelete: TTBXItem
      Action = aSimpleCrDelete
    end
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 36
    Top = 432
    object aSimpleReset: TAction
      Caption = '&Reset'
      Hint = 'Reset|Clear entered criteria'
      OnExecute = aaSimpleReset
    end
    object aSimpleCrDelete: TAction
      Caption = '&Delete'
      Hint = 'Delete selected criterion'
      ImageIndex = 7
      OnExecute = aaSimpleCrDelete
    end
    object aSimpleConvertToExpression: TAction
      Caption = 'Convert to e&xpression'
      Hint = 'Convert selected criteria to search expression'
      ImageIndex = 81
      OnExecute = aaSimpleConvertToExpression
    end
  end
end
