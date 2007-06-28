object frExprPicFilter: TfrExprPicFilter
  Left = 0
  Top = 0
  Width = 539
  Height = 362
  TabOrder = 0
  object dkExprTop: TTBXDock
    Left = 0
    Top = 0
    Width = 539
    Height = 26
    AllowDrag = False
    object tbExprMain: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Toolbar'
      ChevronHint = 'More buttons|'
      Images = fMain.ilActionsSmall
      TabOrder = 0
      object bNew: TTBXItem
        Action = aNew
      end
      object bOpen: TTBXSubmenuItem
        Action = aOpen
        DropdownCombo = True
        object iMRUOpen: TTBXMRUListItem
          MRUList = mruOpen
        end
      end
      object bSaveAs: TTBXItem
        Action = aSaveAs
      end
      object tbSepInsertProp: TTBXSeparatorItem
      end
      object smInsertProp: TTBXSubmenuItem
        Caption = 'Insert propert&y'
        DisplayMode = nbdmImageAndText
        Hint = 'Insert a picture property into the expression'
        ImageIndex = 82
        Options = [tboDropdownArrow]
      end
      object smInsertOperator: TTBXSubmenuItem
        Caption = 'Insert &operator'
        DisplayMode = nbdmImageAndText
        Hint = 'Insert an operator into the expression'
        ImageIndex = 82
        Options = [tboDropdownArrow]
      end
      object tbSepCut: TTBXSeparatorItem
      end
      object bCut: TTBXItem
        Action = aCut
      end
      object bCopy: TTBXItem
        Action = aCopy
      end
      object bPaste: TTBXItem
        Action = aPaste
      end
      object tbSepUndo: TTBXSeparatorItem
      end
      object bUndo: TTBXItem
        Action = aUndo
      end
      object bRedo: TTBXItem
        Action = aRedo
      end
      object tbSepSyntaxCheck: TTBXSeparatorItem
      end
      object bSyntaxCheck: TTBXItem
        Action = aSyntaxCheck
        DisplayMode = nbdmImageAndText
      end
    end
  end
  object eExpression: TSynEdit
    Left = 0
    Top = 26
    Width = 539
    Height = 336
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = pmExpression
    TabOrder = 1
    OnEnter = EnableActionsNotify
    OnExit = EnableActionsNotify
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.ShowLineNumbers = True
    OnChange = eExpressionChange
    OnStatusChange = eExpressionStatusChange
  end
  object alMain: TTntActionList
    Images = fMain.ilActionsSmall
    Left = 132
    Top = 124
    object aNew: TTntAction
      Caption = '&New'
      Hint = 'New|Clear the expression'
      ImageIndex = 0
      OnExecute = aaNew
    end
    object aOpen: TTntAction
      Caption = '&Open...'
      Hint = 'Open...|Open a text file containing the search expression'
      ImageIndex = 1
      OnExecute = aaOpen
    end
    object aSaveAs: TTntAction
      Caption = 'Save &as...'
      Hint = 'Save as...|Save the expression to a file'
      ImageIndex = 3
      OnExecute = aaSaveAs
    end
    object aCut: TTntAction
      Caption = '&Cut'
      Hint = 'Cut|Cut selected text to the clipboard'
      ImageIndex = 20
      OnExecute = aaCut
    end
    object aCopy: TTntAction
      Caption = 'Co&py'
      Hint = 'Copy|Copy selected text to the clipboard'
      ImageIndex = 21
      OnExecute = aaCopy
    end
    object aPaste: TTntAction
      Caption = '&Paste'
      Hint = 'Paste|Paste text from the clipboard'
      ImageIndex = 22
      OnExecute = aaPaste
    end
    object aUndo: TTntAction
      Caption = '&Undo'
      Hint = 'Undo|Undo the last change'
      ImageIndex = 23
      OnExecute = aaUndo
    end
    object aRedo: TTntAction
      Caption = '&Redo'
      Hint = 'Redo|Redo the last undone change'
      ImageIndex = 80
      OnExecute = aaRedo
    end
    object aSyntaxCheck: TTntAction
      Caption = 'Syntax check'
      Hint = 'Syntax check the expression'
      ImageIndex = 57
      ShortCut = 16504
      OnExecute = aaSyntaxCheck
    end
  end
  object pmExpression: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 184
    Top = 124
  end
  object mruOpen: TTBXMRUList
    HidePathExtension = False
    MaxItems = 15
    OnClick = mruOpenClick
    Prefix = 'MRU'
    Left = 244
    Top = 124
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
    Left = 92
    Top = 124
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'mruOpen.Prefix'
      'scpMain.EndOfTokenChr'
      'scpMain.TitleFont.*'
      'scpMain.TriggerChars')
    Left = 292
    Top = 124
    LangData = {
      0F0066724578707250696346696C7465720001200000000900646B4578707254
      6F7000000A007462457870724D61696E01020000000100000007004361707469
      6F6E270000000B0043686576726F6E48696E74000400624E657700000500624F
      70656E00000800694D52554F70656E000007006253617665417300000F007462
      536570496E7365727450726F7000000C00736D496E7365727450726F70010200
      000002000000070043617074696F6E03000000040048696E74001000736D496E
      736572744F70657261746F72010200000004000000070043617074696F6E0500
      0000040048696E74000800746253657043757400000400624375740000050062
      436F707900000600625061737465000009007462536570556E646F0000050062
      556E646F00000500625265646F00001000746253657053796E74617843686563
      6B00000C006253796E746178436865636B00000B006545787072657373696F6E
      00000600616C4D61696E00000400614E65770102000000090000000700436170
      74696F6E0A000000040048696E74000500614F70656E01020000000C00000007
      0043617074696F6E0D000000040048696E740007006153617665417301020000
      000F000000070043617074696F6E10000000040048696E740004006143757401
      0200000012000000070043617074696F6E13000000040048696E740005006143
      6F7079010200000015000000070043617074696F6E16000000040048696E7400
      0600615061737465010200000018000000070043617074696F6E190000000400
      48696E7400050061556E646F01020000001B000000070043617074696F6E1C00
      0000040048696E74000500615265646F01020000001E00000007004361707469
      6F6E1F000000040048696E74000C006153796E746178436865636B0102000000
      21000000070043617074696F6E22000000040048696E74000C00706D45787072
      657373696F6E000007006D72754F70656E000007007363704D61696E01010000
      002600000005005469746C6500}
  end
end
