inherited dPicProps: TdPicProps
  Left = 376
  Top = 334
  Caption = 'Properties: picture'
  ClientHeight = 433
  ClientWidth = 590
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 396
    Width = 590
  end
  object pMain: TPanel [1]
    Left = 0
    Top = 0
    Width = 590
    Height = 395
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object dkNav: TTBXDock
      Left = 0
      Top = 0
      Width = 82
      Height = 395
      AllowDrag = False
      PopupMenu = pmNav
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
        object bPgFileProps: TTBXItem
          Caption = 'File properties'
          ImageIndex = 12
          ShortCut = 16433
          OnClick = PageButtonClick
        end
        object bPgMetadata: TTBXItem
          Tag = 1
          Caption = 'Metadata'
          ImageIndex = 42
          ShortCut = 16434
          OnClick = PageButtonClick
        end
        object bPgView: TTBXItem
          Tag = 2
          Caption = 'View && Adjust'
          ImageIndex = 15
          ShortCut = 16435
          OnClick = PageButtonClick
        end
        object bPgData: TTBXItem
          Tag = 3
          Caption = 'Data'
          ImageIndex = 8
          ShortCut = 16436
          OnClick = PageButtonClick
        end
        object bPgKeywords: TTBXItem
          Tag = 4
          Caption = 'Keywords'
          ImageIndex = 41
          ShortCut = 16437
          OnClick = PageButtonClick
        end
        object bPgGroups: TTBXItem
          Tag = 5
          Caption = 'Groups'
          ImageIndex = 36
          ShortCut = 16438
          OnClick = PageButtonClick
        end
      end
    end
    object pPages: TPanel
      Left = 82
      Top = 0
      Width = 508
      Height = 395
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  inherited pButtonsBottom: TPanel
    Top = 398
    Width = 590
    TabOrder = 1
    DesignSize = (
      590
      35)
    inherited bCancel: TButton
      Left = 429
      Caption = 'Close'
    end
    inherited bOK: TButton
      Left = 349
    end
    inherited bHelp: TButton
      Left = 507
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      'fpMain.IniFileName'
      'fpMain.IniSection'
      '*.SecondaryShortCuts')
    Top = 400
    LangData = {
      09006450696350726F7073010100000003000000070043617074696F6E011200
      000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D0000
      07006243616E63656C01010000000C000000070043617074696F6E000300624F
      4B01010000000F000000070043617074696F6E0005006248656C700101000000
      12000000070043617074696F6E000500704D61696E00000500646B4E61760000
      050074624E617600000C0062506746696C6550726F707301010000001A000000
      070043617074696F6E000B006250674D6574616461746101010000001C000000
      070043617074696F6E0007006250675669657701010000001E00000007004361
      7074696F6E00070062506744617461010100000020000000070043617074696F
      6E000B006250674B6579776F726473010100000022000000070043617074696F
      6E00090062506747726F757073010100000024000000070043617074696F6E00
      06007050616765730000060066704D61696E00000700696C46696C6573000005
      00706D4E61760000}
  end
  object fpMain: TFormPlacement
    IniFileName = 'Software\DaleTech\PhoA'
    IniSection = 'PicPropsDialog'
    UseRegistry = True
    Left = 32
    Top = 400
  end
  object ilFiles: TImageList
    ShareImages = True
    Left = 60
    Top = 400
  end
  object pmNav: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 88
    Top = 400
  end
end
