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
      TabOrder = 2
    end
    inherited bOK: TButton
      Left = 399
      Caption = 'Find'
    end
    inherited bHelp: TButton
      Left = 557
      TabOrder = 3
    end
    object bReset: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 23
      Action = aReset
      TabOrder = 0
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
            Width = 294
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
      object eExpression: TSynEdit
        Left = 0
        Top = 26
        Width = 608
        Height = 308
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = pmExpression
        TabOrder = 1
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 0
        Gutter.ShowLineNumbers = True
        OnChange = DlgDataChange
        OnStatusChange = eExpressionStatusChange
      end
      object dkExprTop: TTBXDock
        Left = 0
        Top = 0
        Width = 608
        Height = 26
        AllowDrag = False
        object tbExprMain: TTBXToolbar
          Left = 0
          Top = 0
          Caption = 'Toolbar'
          Images = fMain.ilActionsSmall
          TabOrder = 0
          object smExprInsertProp: TTBXSubmenuItem
            Caption = 'Insert propert&y'
            DisplayMode = nbdmImageAndText
            Hint = 'Insert a picture property into the expression'
            Options = [tboDropdownArrow]
          end
          object smExprInsertOperator: TTBXSubmenuItem
            Caption = 'Insert &operator'
            DisplayMode = nbdmImageAndText
            Hint = 'Insert an operator into the expression'
            Options = [tboDropdownArrow]
          end
          object tbSepExprSyntaxCheck: TTBXSeparatorItem
          end
          object bExprSyntaxCheck: TTBXItem
            Action = aExprSyntaxCheck
            DisplayMode = nbdmImageAndText
          end
          object tbSepExprUndo: TTBXSeparatorItem
          end
          object bExprUndo: TTBXItem
            Action = aExprUndo
          end
          object bExprRedo: TTBXItem
            Action = aExprRedo
          end
          object tbSepExprCut: TTBXSeparatorItem
          end
          object bExprCut: TTBXItem
            Action = aExprCut
          end
          object bExprCopy: TTBXItem
            Action = aExprCopy
          end
          object bExprPaste: TTBXItem
            Action = aExprPaste
          end
        end
      end
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'scpMain.EndOfTokenChr'
      'scpMain.TitleFont.*'
      'scpMain.TriggerChars')
    StoreList.Strings = (
      '*.ChevronHint'
      'tvSimpleCriteria.Header.Columns*')
    Left = 116
    Top = 432
    LangData = {
      070064536561726368010100000003000000070043617074696F6E012F000000
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
      0100000024000000070043617074696F6E00060062526573657400000A007063
      437269746572696100000800747353696D706C65010100000028000000070043
      617074696F6E000C00747345787072657373696F6E0101000000290000000700
      43617074696F6E000B006545787072657373696F6E00000900646B4578707254
      6F7000000A007462457870724D61696E01020000002A00000007004361707469
      6F6E2B0000000B0043686576726F6E48696E74001000736D45787072496E7365
      727450726F7001020000002C000000070043617074696F6E2E00000004004869
      6E74001400736D45787072496E736572744F70657261746F7201020000002D00
      0000070043617074696F6E2F000000040048696E740007007363704D61696E01
      010000003300000005005469746C65000800706D53696D706C6500000F006970
      6D736D53696D706C6550726F70010100000034000000070043617074696F6E00
      0F0069706D53696D706C6544656C65746500000B00646B53696D706C65546F70
      00000C00746253696D706C654D61696E01020000003600000007004361707469
      6F6E370000000B0043686576726F6E48696E74000600616C4D61696E00000F00
      6153696D706C65437244656C657465010200000038000000070043617074696F
      6E39000000040048696E74000F006253696D706C65437244656C65746500001A
      006153696D706C65436F6E76657274546F45787072657373696F6E0102000000
      3A000000070043617074696F6E3B000000040048696E74001A006253696D706C
      65436F6E76657274546F45787072657373696F6E000006006152657365740102
      0000003F000000070043617074696F6E40000000040048696E74001000614578
      707253796E746178436865636B010200000041000000070043617074696F6E42
      000000040048696E74001000624578707253796E746178436865636B00000800
      6145787072437574010200000043000000070043617074696F6E440000000400
      48696E740009006145787072436F707901020000004500000007004361707469
      6F6E46000000040048696E74000A006145787072506173746501020000004700
      0000070043617074696F6E4A000000040048696E740009006145787072556E64
      6F010200000048000000070043617074696F6E4B000000040048696E74000900
      61457870725265646F010200000049000000070043617074696F6E4C00000004
      0048696E7400090062457870725265646F000009006245787072556E646F0000
      0A0062457870725061737465000009006245787072436F707900000800624578
      707243757400000D00746253657045787072556E646F00001400746253657045
      78707253796E746178436865636B00000C00706D45787072657373696F6E0000
      0C007462536570457870724375740000}
  end
  object scpMain: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[].,{}<>=/\!?"'#39'#%^&+-*| '
    TriggerChars = '$'
    Title = 'Properties'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ShortCut = 16416
    Editor = eExpression
    TimerInterval = 200
    Left = 84
    Top = 124
  end
  object pmSimple: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    OnPopup = pmSimplePopup
    Left = 32
    Top = 124
    object ipmsmSimpleProp: TTBXSubmenuItem
      Caption = '&Picture property'
    end
    object ipmSimpleDelete: TTBXItem
      Action = aSimpleCrDelete
    end
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 132
    Top = 124
    object aReset: TAction
      Caption = '&Reset'
      Hint = 'Reset|Clear entered criteria or expression'
      OnExecute = aaReset
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
      OnExecute = aaSimpleConvertToExpression
    end
    object aExprSyntaxCheck: TAction
      Caption = 'Syntax check'
      Hint = 'Syntax check the expression'
      ImageIndex = 57
      ShortCut = 16504
      OnExecute = aaExprSyntaxCheck
    end
    object aExprUndo: TAction
      Caption = '&Undo'
      Hint = 'Undo|Undo the last change'
      ImageIndex = 23
      OnExecute = aaExprUndo
    end
    object aExprRedo: TAction
      Caption = '&Redo'
      Hint = 'Redo|Redo the last undone change'
      ImageIndex = 80
      OnExecute = aaExprRedo
    end
    object aExprCut: TAction
      Caption = '&Cut'
      Hint = 'Cut|Cut selected text to the clipboard'
      ImageIndex = 20
      OnExecute = aaExprCut
    end
    object aExprCopy: TAction
      Caption = 'Co&py'
      Hint = 'Copy|Copy selected text to the clipboard'
      ImageIndex = 21
      OnExecute = aaExprCopy
    end
    object aExprPaste: TAction
      Caption = '&Paste'
      Hint = 'Paste|Paste text from the clipboard'
      ImageIndex = 22
      OnExecute = aaExprPaste
    end
  end
  object pmExpression: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 184
    Top = 124
  end
end
