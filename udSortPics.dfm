inherited dSortPics: TdSortPics
  Left = 589
  Top = 254
  Caption = 'Sort pictures'
  ClientHeight = 344
  ClientWidth = 398
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 307
    Width = 398
  end
  object bReset: TButton [1]
    Left = 315
    Top = 314
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Reset'
    TabOrder = 0
    OnClick = bResetClick
  end
  inherited pButtonsBottom: TPanel
    Top = 309
    Width = 398
    TabOrder = 1
    DesignSize = (
      398
      35)
    inherited bCancel: TButton
      Left = 240
    end
    inherited bOK: TButton
      Left = 160
    end
    inherited bHelp: TButton
      Left = 318
    end
  end
  object gbWhereToSort: TGroupBox [3]
    Left = 12
    Top = 12
    Width = 373
    Height = 69
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Where to sort pictures'
    TabOrder = 2
    object rbCurGroup: TRadioButton
      Left = 16
      Top = 20
      Width = 345
      Height = 17
      Caption = 'In t&he current group'
      TabOrder = 0
      OnClick = DlgDataChange
    end
    object rbAllGroups: TRadioButton
      Left = 16
      Top = 40
      Width = 345
      Height = 17
      Caption = 'In &all photo album groups'
      TabOrder = 1
      OnClick = DlgDataChange
    end
  end
  inline frSorting: TfrSorting [4]
    Left = 13
    Top = 86
    Width = 372
    Height = 207
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    inherited lMain: TLabel
      Width = 372
      Caption = '&Sort pictures by:'
    end
    inherited tvMain: TVirtualStringTree
      Width = 372
      Height = 192
      Columns = <
        item
          Position = 0
          Width = 218
        end
        item
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
  inherited dklcMain: TDKLanguageController
    Options = [dklcoAutoSaveLangSource, dklcoIgnoreEmptyProps]
    Left = 60
    Top = 126
    LangData = {
      090064536F727450696373010100000003000000070043617074696F6E010A00
      000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D0000
      07006243616E63656C01010000000C000000070043617074696F6E000300624F
      4B01010000000F000000070043617074696F6E0005006248656C700101000000
      12000000070043617074696F6E00060062526573657401010000001500000007
      0043617074696F6E000D0067625768657265546F536F72740101000000180000
      00070043617074696F6E000A00726243757247726F757001010000001B000000
      070043617074696F6E000B007262416C6C47726F75707301010000001E000000
      070043617074696F6E0009006672536F7274696E6700010800000005006C4D61
      696E010100000023000000070043617074696F6E00060074764D61696E000006
      00706D4D61696E0000090069706D736D50726F70010100000027000000070043
      617074696F6E00090069706D44656C6574650101000000290000000700436170
      74696F6E00060069706D5365700000090069706D4D6F7665557001010000002C
      000000070043617074696F6E000B0069706D4D6F7665446F776E01010000002E
      000000070043617074696F6E00}
  end
end
