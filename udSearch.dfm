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
          object bExprNew: TTBXItem
            Action = aExprNew
          end
          object bExprOpen: TTBXSubmenuItem
            Action = aExprOpen
            DropdownCombo = True
            object iMRUExprOpen: TTBXMRUListItem
              MRUList = mruExprOpen
            end
          end
          object bExprSaveAs: TTBXItem
            Action = aExprSaveAs
          end
          object TBXSeparatorItem1: TTBXSeparatorItem
          end
          object smExprInsertProp: TTBXSubmenuItem
            Caption = 'Insert propert&y'
            DisplayMode = nbdmImageAndText
            Hint = 'Insert a picture property into the expression'
            ImageIndex = 82
            Options = [tboDropdownArrow]
          end
          object smExprInsertOperator: TTBXSubmenuItem
            Caption = 'Insert &operator'
            DisplayMode = nbdmImageAndText
            Hint = 'Insert an operator into the expression'
            ImageIndex = 82
            Options = [tboDropdownArrow]
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
          object tbSepExprUndo: TTBXSeparatorItem
          end
          object bExprUndo: TTBXItem
            Action = aExprUndo
          end
          object bExprRedo: TTBXItem
            Action = aExprRedo
          end
          object tbSepExprSyntaxCheck: TTBXSeparatorItem
          end
          object bExprSyntaxCheck: TTBXItem
            Action = aExprSyntaxCheck
            DisplayMode = nbdmImageAndText
          end
        end
      end
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'mruExprOpen.Prefix'
      'scpMain.EndOfTokenChr'
      'scpMain.TitleFont.*'
      'scpMain.TriggerChars')
    StoreList.Strings = (
      '*.ChevronHint'
      'tvSimpleCriteria.Header.Columns*')
    Left = 116
    Top = 432
    LangData = {
      070064536561726368010100000003000000070043617074696F6E0138000000
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
      6153696D706C65437244656C657465010300000038000000070043617074696F
      6E4E000000080043617465676F727939000000040048696E74000F006253696D
      706C65437244656C65746500001A006153696D706C65436F6E76657274546F45
      787072657373696F6E01030000003A000000070043617074696F6E4F00000008
      0043617465676F72793B000000040048696E74001A006253696D706C65436F6E
      76657274546F45787072657373696F6E0000060061526573657401030000003F
      000000070043617074696F6E4D000000080043617465676F7279400000000400
      48696E74001000614578707253796E746178436865636B010300000041000000
      070043617074696F6E50000000080043617465676F727942000000040048696E
      74001000624578707253796E746178436865636B000008006145787072437574
      010300000043000000070043617074696F6E53000000080043617465676F7279
      44000000040048696E740009006145787072436F707901030000004500000007
      0043617074696F6E54000000080043617465676F727946000000040048696E74
      000A0061457870725061737465010300000047000000070043617074696F6E55
      000000080043617465676F72794A000000040048696E74000900614578707255
      6E646F010300000048000000070043617074696F6E5100000008004361746567
      6F72794B000000040048696E7400090061457870725265646F01030000004900
      0000070043617074696F6E52000000080043617465676F72794C000000040048
      696E7400090062457870725265646F000009006245787072556E646F00000A00
      62457870725061737465000009006245787072436F7079000008006245787072
      43757400000D00746253657045787072556E646F000014007462536570457870
      7253796E746178436865636B00000C00706D45787072657373696F6E00000C00
      7462536570457870724375740000080061457870724E65770103000000570000
      00070043617074696F6E56000000080043617465676F72795800000004004869
      6E7400090061457870724F70656E01030000005A000000070043617074696F6E
      59000000080043617465676F72795B000000040048696E74000B006145787072
      53617665417301030000005D000000070043617074696F6E5C00000008004361
      7465676F72795E000000040048696E74000B0062457870725361766541730000
      090062457870724F70656E0000080062457870724E6577000011005442585365
      70617261746F724974656D3100000C00694D5255457870724F70656E00000B00
      6D7275457870724F70656E0000}
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
      Category = 'General'
      Caption = '&Reset'
      Hint = 'Reset|Clear entered criteria or expression'
      OnExecute = aaReset
    end
    object aSimpleCrDelete: TAction
      Category = 'Simple search'
      Caption = '&Delete'
      Hint = 'Delete selected criterion'
      ImageIndex = 7
      OnExecute = aaSimpleCrDelete
    end
    object aSimpleConvertToExpression: TAction
      Category = 'Simple search'
      Caption = 'Convert to e&xpression'
      Hint = 'Convert selected criteria to search expression'
      ImageIndex = 81
      OnExecute = aaSimpleConvertToExpression
    end
    object aExprNew: TAction
      Category = 'Expression search'
      Caption = '&New'
      Hint = 'New|Clear the expression'
      ImageIndex = 0
      OnExecute = aaExprNew
    end
    object aExprOpen: TAction
      Category = 'Expression search'
      Caption = '&Open...'
      Hint = 'Open...|Open a text file containing the search expression'
      ImageIndex = 1
      OnExecute = aaExprOpen
    end
    object aExprSaveAs: TAction
      Category = 'Expression search'
      Caption = 'Save &as...'
      Hint = 'Save as...|Save the expression to a file'
      ImageIndex = 3
      OnExecute = aaExprSaveAs
    end
    object aExprCut: TAction
      Category = 'Expression search'
      Caption = '&Cut'
      Hint = 'Cut|Cut selected text to the clipboard'
      ImageIndex = 20
      OnExecute = aaExprCut
    end
    object aExprCopy: TAction
      Category = 'Expression search'
      Caption = 'Co&py'
      Hint = 'Copy|Copy selected text to the clipboard'
      ImageIndex = 21
      OnExecute = aaExprCopy
    end
    object aExprPaste: TAction
      Category = 'Expression search'
      Caption = '&Paste'
      Hint = 'Paste|Paste text from the clipboard'
      ImageIndex = 22
      OnExecute = aaExprPaste
    end
    object aExprUndo: TAction
      Category = 'Expression search'
      Caption = '&Undo'
      Hint = 'Undo|Undo the last change'
      ImageIndex = 23
      OnExecute = aaExprUndo
    end
    object aExprRedo: TAction
      Category = 'Expression search'
      Caption = '&Redo'
      Hint = 'Redo|Redo the last undone change'
      ImageIndex = 80
      OnExecute = aaExprRedo
    end
    object aExprSyntaxCheck: TAction
      Category = 'Expression search'
      Caption = 'Syntax check'
      Hint = 'Syntax check the expression'
      ImageIndex = 57
      ShortCut = 16504
      OnExecute = aaExprSyntaxCheck
    end
  end
  object pmExpression: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 184
    Top = 124
  end
  object mruExprOpen: TTBXMRUList
    HidePathExtension = False
    MaxItems = 15
    OnClick = mruExprOpenClick
    Prefix = 'MRU'
    Left = 256
    Top = 124
  end
end
