inherited dSearch: TdSearch
  Caption = 'Find pictures'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object gbSearch: TTntGroupBox [1]
    Left = 12
    Top = 8
    Width = 609
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Perform search'
    TabOrder = 0
    object rbAll: TTntRadioButton
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
    object rbCurGroup: TTntRadioButton
      Left = 156
      Top = 16
      Width = 145
      Height = 17
      Caption = 'In the &current group'
      TabOrder = 1
      OnClick = DlgDataChange
    end
    object rbSearchResults: TTntRadioButton
      Left = 304
      Top = 16
      Width = 189
      Height = 17
      Caption = 'In the search &results'
      TabOrder = 2
      OnClick = DlgDataChange
    end
  end
  inherited pButtonsBottom: TTntPanel
    TabOrder = 2
    inherited bCancel: TTntButton
      Left = 471
      TabOrder = 1
    end
    inherited bOK: TTntButton
      Left = 391
      Caption = 'Find'
      TabOrder = 0
    end
    inherited bHelp: TTntButton
      Left = 549
    end
  end
  object pcCriteria: TTntPageControl
    Left = 12
    Top = 56
    Width = 608
    Height = 333
    ActivePage = tsSimple
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnChange = pcCriteriaChange
    object tsSimple: TTntTabSheet
      Caption = 'Simple search'
      object tvSimpleCriteria: TVirtualStringTree
        Left = 0
        Top = 26
        Width = 600
        Height = 279
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
        OnBeforeCellPaint = tvSimpleCriteriaBeforeCellPaint
        OnChecked = tvSimpleCriteriaChecked
        OnCreateEditor = tvSimpleCriteriaCreateEditor
        OnEditing = tvSimpleCriteriaEditing
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
            Width = 286
            WideText = 'Value'
          end>
        WideDefaultText = ''
      end
      object dkSimpleTop: TTBXDock
        Left = 0
        Top = 0
        Width = 600
        Height = 26
        AllowDrag = False
        object tbSimpleMain: TTBXToolbar
          Left = 0
          Top = 0
          Caption = 'Toolbar'
          ChevronHint = 'More buttons|'
          Images = fMain.ilActionsSmall
          TabOrder = 0
          object bSimpleReset: TTBXItem
            Action = aSimpleReset
            DisplayMode = nbdmImageAndText
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
    object tsExpression: TTntTabSheet
      Caption = 'Expression search'
      ImageIndex = 1
      inline frExprPicFilter: TfrExprPicFilter
        Left = 0
        Top = 0
        Width = 600
        Height = 305
        Align = alClient
        TabOrder = 0
        inherited dkExprTop: TTBXDock
          Width = 600
        end
        inherited mExpression: TTntMemo
          Width = 600
          Height = 279
        end
        inherited dklcMain: TDKLanguageController
          Options = [dklcoIgnoreEmptyProps]
          LangData = {
            0F0066724578707250696346696C74657200011A0000000900646B4578707254
            6F7000000A007462457870724D61696E01020000000100000007004361707469
            6F6E270000000B0043686576726F6E48696E74000400624E657700000500624F
            70656E00000800694D52554F70656E000007006253617665417300000F007462
            536570496E7365727450726F7000000C00736D496E7365727450726F70010200
            000002000000070043617074696F6E03000000040048696E74001000736D496E
            736572744F70657261746F72010200000004000000070043617074696F6E0500
            0000040048696E74000800746253657043757400000400624375740000050062
            436F70790000060062506173746500001000746253657053796E746178436865
            636B00000C006253796E746178436865636B00000600616C4D61696E00000400
            614E6577010200000009000000070043617074696F6E0A000000040048696E74
            000500614F70656E01020000000C000000070043617074696F6E0D0000000400
            48696E740007006153617665417301020000000F000000070043617074696F6E
            10000000040048696E7400040061437574010200000012000000070043617074
            696F6E13000000040048696E7400050061436F70790102000000150000000700
            43617074696F6E16000000040048696E74000600615061737465010200000018
            000000070043617074696F6E19000000040048696E74000C006153796E746178
            436865636B010200000021000000070043617074696F6E22000000040048696E
            74000C00706D45787072657373696F6E000007006D72754F70656E00000B006D
            45787072657373696F6E0000}
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
      'tvSimpleCriteria.Header.Columns*')
    Left = 64
    Top = 404
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
      0066724578707250696346696C74657200011A0000000900646B45787072546F
      7000000A007462457870724D61696E00000400624E657700000500624F70656E
      00000800694D52554F70656E000007006253617665417300000F007462536570
      496E7365727450726F7000000C00736D496E7365727450726F7000001000736D
      496E736572744F70657261746F72000008007462536570437574000004006243
      75740000050062436F7079000006006250617374650000100074625365705379
      6E746178436865636B00000C006253796E746178436865636B00000600616C4D
      61696E00000400614E657700000500614F70656E000007006153617665417300
      000400614375740000050061436F70790000060061506173746500000C006153
      796E746178436865636B00000C00706D45787072657373696F6E000007006D72
      754F70656E00000B006D45787072657373696F6E0000}
  end
  object pmSimple: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    OnPopup = pmSimplePopup
    Left = 8
    Top = 404
    object ipmsmSimpleProp: TTBXSubmenuItem
      Caption = '&Picture property'
    end
    object ipmSimpleDelete: TTBXItem
      Action = aSimpleCrDelete
    end
  end
  object alMain: TTntActionList
    Images = fMain.ilActionsSmall
    Left = 36
    Top = 404
    object aSimpleReset: TTntAction
      Caption = '&Reset'
      Hint = 'Reset|Clear entered criteria'
      ImageIndex = 0
      OnExecute = aaSimpleReset
    end
    object aSimpleCrDelete: TTntAction
      Caption = '&Delete'
      Hint = 'Delete selected criterion'
      ImageIndex = 7
      OnExecute = aaSimpleCrDelete
    end
    object aSimpleConvertToExpression: TTntAction
      Caption = 'Convert to e&xpression'
      Hint = 'Convert selected criteria to search expression'
      ImageIndex = 81
      OnExecute = aaSimpleConvertToExpression
    end
  end
end
