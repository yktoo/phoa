inherited dSettings: TdSettings
  Caption = 'Program settings'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pMain: TTntPanel [1]
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
        ChevronHint = 'More buttons|'
        FullSize = True
        Images = fMain.ilActionsSmall
        Options = [tboImageAboveCaption, tboSameWidth]
        ProcessShortCuts = True
        SystemFont = False
        TabOrder = 0
      end
    end
    object pEditor: TTntPanel
      Left = 27
      Top = 0
      Width = 605
      Height = 398
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  inherited pButtonsBottom: TTntPanel
    TabOrder = 1
    inherited bCancel: TTntButton
      Caption = 'Close'
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'fpMain.IniFileName'
      'fpMain.IniSection')
    Top = 400
    LangData = {
      09006453657474696E6773010100000003000000070043617074696F6E010900
      000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D0000
      07006243616E63656C01010000000C000000070043617074696F6E000300624F
      4B01010000000F000000070043617074696F6E0005006248656C700101000000
      12000000070043617074696F6E000500704D61696E00000500646B4E61760000
      050074624E61760101000000130000000B0043686576726F6E48696E74000700
      70456469746F720000}
  end
end
