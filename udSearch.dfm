inherited dSearch: TdSearch
  Left = 451
  Top = 212
  Caption = 'Find pictures'
  ClientHeight = 464
  ClientWidth = 640
  OldCreateOrder = True
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
      Caption = '&Reset'
      TabOrder = 0
      OnClick = bResetClick
    end
  end
  object pcCriteria: TPageControl
    Left = 12
    Top = 56
    Width = 616
    Height = 362
    ActivePage = tsSimple
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnChange = DlgDataChange
    object tsSimple: TTabSheet
      Caption = 'Simple search'
      DesignSize = (
        608
        334)
      object lCriteria: TLabel
        Left = 4
        Top = 4
        Width = 73
        Height = 13
        Caption = '&Search criteria:'
      end
      object tvSimpleCriteria: TVirtualStringTree
        Left = 3
        Top = 20
        Width = 594
        Height = 309
        Anchors = [akLeft, akTop, akRight, akBottom]
        Header.AutoSizeIndex = 2
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
        Header.ParentFont = True
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus]
        OnCreateEditor = tvSimpleCriteriaCreateEditor
        OnFocusChanged = tvSimpleCriteriaFocusChanged
        OnGetText = tvSimpleCriteriaGetText
        OnInitNode = tvSimpleCriteriaInitNode
        Columns = <
          item
            Color = 16250871
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
            Position = 0
            Width = 180
          end
          item
            Position = 1
            Width = 130
          end
          item
            Position = 2
            Width = 280
          end>
        WideDefaultText = ''
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
            ImageIndex = 57
            Options = [tboDropdownArrow]
          end
          object smExprInsertOperator: TTBXSubmenuItem
            Caption = 'Insert &operator'
            DisplayMode = nbdmImageAndText
            Hint = 'Insert an operator into the expression'
            ImageIndex = 57
            Options = [tboDropdownArrow]
          end
        end
      end
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    StoreList.Strings = (
      '*.ChevronHint')
    Left = 116
    Top = 432
    LangData = {
      070064536561726368010100000003000000070043617074696F6E0115000000
      08006276426F74746F6D00000E0070427574746F6E73426F74746F6D00000700
      6243616E63656C01010000000C000000070043617074696F6E000300624F4B01
      010000000F000000070043617074696F6E0005006248656C7001010000001200
      0000070043617074696F6E0009006C4372697465726961010100000015000000
      070043617074696F6E001000747653696D706C65437269746572696100000800
      676253656172636801010000001B000000070043617074696F6E000500726241
      6C6C01010000001E000000070043617074696F6E000A00726243757247726F75
      70010100000021000000070043617074696F6E000F0072625365617263685265
      73756C7473010100000024000000070043617074696F6E000600625265736574
      010100000027000000070043617074696F6E000A007063437269746572696100
      000800747353696D706C65010100000028000000070043617074696F6E000C00
      747345787072657373696F6E010100000029000000070043617074696F6E000B
      006545787072657373696F6E00000900646B45787072546F7000000A00746245
      7870724D61696E01020000002A000000070043617074696F6E2B0000000B0043
      686576726F6E48696E74001000736D45787072496E7365727450726F70010200
      00002C000000070043617074696F6E2E000000040048696E74001400736D4578
      7072496E736572744F70657261746F7201020000002D00000007004361707469
      6F6E2F000000040048696E740007007363704D61696E0104000000300000000D
      00456E644F66546F6B656E4368723300000005005469746C65320000000E0054
      69746C65466F6E742E4E616D65310000000C0054726967676572436861727300}
  end
  object scpMain: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. '
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
    Left = 44
    Top = 112
  end
end
