inherited dSettings: TdSettings
  Left = 376
  Top = 236
  Caption = 'Program settings'
  ClientHeight = 435
  ClientWidth = 632
  OldCreateOrder = True
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 398
    Width = 632
  end
  object pMain: TPanel [1]
    Left = 0
    Top = 0
    Width = 632
    Height = 398
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object dkNav: TTBXDock
      Left = 0
      Top = 0
      Width = 27
      Height = 398
      AllowDrag = False
      Position = dpLeft
      object tbNav: TTBXToolbar
        Left = 0
        Top = 0
        FullSize = True
        Images = fMain.ilActionsSmall
        Options = [tboImageAboveCaption, tboSameWidth]
        ProcessShortCuts = True
        SystemFont = False
        TabOrder = 0
      end
    end
    object pEditor: TPanel
      Left = 27
      Top = 0
      Width = 605
      Height = 398
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  inherited pButtonsBottom: TPanel
    Top = 400
    Width = 632
    TabOrder = 1
    DesignSize = (
      632
      35)
    inherited bCancel: TButton
      Left = 473
      Caption = 'Close'
    end
    inherited bOK: TButton
      Left = 393
    end
    inherited bHelp: TButton
      Left = 551
    end
  end
  inherited dklcMain: TDKLanguageController
    Options = [dklcoAutoSaveLangSource, dklcoIgnoreEmptyProps]
    Top = 400
    LangData = {
      09006453657474696E6773010100000003000000070043617074696F6E010A00
      000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D0000
      07006243616E63656C01010000000C000000070043617074696F6E000300624F
      4B01010000000F000000070043617074696F6E0005006248656C700101000000
      12000000070043617074696F6E000500704D61696E00000500646B4E61760000
      050074624E61760000070070456469746F720000060066704D61696E01020000
      001D0000000B00496E6946696C654E616D651E0000000A00496E695365637469
      6F6E00}
  end
  object fpMain: TFormPlacement
    IniFileName = '\\\'
    IniSection = '\\\'
    UseRegistry = True
    Left = 32
    Top = 400
  end
end
