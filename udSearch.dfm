inherited dSearch: TdSearch
  Left = 486
  Top = 244
  Caption = 'Find pictures'
  ClientHeight = 435
  ClientWidth = 525
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lCriteria: TLabel [0]
    Left = 12
    Top = 52
    Width = 73
    Height = 13
    Caption = '&Search criteria:'
  end
  inherited bvBottom: TBevel
    Top = 398
    Width = 525
  end
  object tvCriteria: TVirtualStringTree [2]
    Left = 12
    Top = 68
    Width = 502
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 2
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
    ParentBackground = False
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus]
    OnCreateEditor = tvCriteriaCreateEditor
    OnFocusChanged = tvCriteriaFocusChanged
    OnGetText = tvCriteriaGetText
    OnPaintText = tvCriteriaPaintText
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
        Width = 188
      end>
    WideDefaultText = ''
  end
  object gbSearch: TGroupBox [3]
    Left = 12
    Top = 8
    Width = 502
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
    Top = 400
    Width = 525
    TabOrder = 2
    DesignSize = (
      525
      35)
    inherited bCancel: TButton
      Left = 364
    end
    inherited bOK: TButton
      Left = 284
      Caption = 'Find'
    end
    inherited bHelp: TButton
      Left = 442
    end
    object bReset: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 23
      Caption = '&Reset'
      TabOrder = 3
      OnClick = bResetClick
    end
  end
  object dklcMain: TDKLanguageController
    Options = [dklcoAutoSaveLangSource, dklcoIgnoreEmptyProps]
    Left = 20
    Top = 76
    LangData = {
      070064536561726368010100000003000000070043617074696F6E010C000000
      08006276426F74746F6D00000E0070427574746F6E73426F74746F6D00000700
      6243616E63656C01010000000C000000070043617074696F6E000300624F4B01
      010000000F000000070043617074696F6E0005006248656C7001010000001200
      0000070043617074696F6E0009006C4372697465726961010100000015000000
      070043617074696F6E000A007476437269746572696100000800676253656172
      636801010000001B000000070043617074696F6E0005007262416C6C01010000
      001E000000070043617074696F6E000A00726243757247726F75700101000000
      21000000070043617074696F6E000F007262536561726368526573756C747301
      0100000024000000070043617074696F6E000600625265736574010100000027
      000000070043617074696F6E00}
  end
end
