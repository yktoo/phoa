inherited dPicProps: TdPicProps
  Caption = 'Properties: picture'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pMain: TPanel [1]
    Left = 0
    Top = 0
    Width = 632
    Height = 397
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object dkNav: TTBXDock
      Left = 0
      Top = 0
      Width = 82
      Height = 397
      AllowDrag = False
      PopupMenu = pmNav
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
      Width = 550
      Height = 397
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  inherited pButtonsBottom: TPanel
    TabOrder = 1
    inherited bCancel: TButton
      Left = 471
      Caption = 'Close'
    end
    inherited bOK: TButton
      Left = 391
    end
    inherited bHelp: TButton
      Left = 549
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
      09006450696350726F7073010100000003000000070043617074696F6E011100
      000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D0000
      07006243616E63656C01010000000C000000070043617074696F6E000300624F
      4B01010000000F000000070043617074696F6E0005006248656C700101000000
      12000000070043617074696F6E000500704D61696E00000500646B4E61760000
      050074624E61760101000000250000000B0043686576726F6E48696E74000C00
      62506746696C6550726F707301010000001A000000070043617074696F6E000B
      006250674D6574616461746101010000001C000000070043617074696F6E0007
      006250675669657701010000001E000000070043617074696F6E000700625067
      44617461010100000020000000070043617074696F6E000B006250674B657977
      6F726473010100000022000000070043617074696F6E00090062506747726F75
      7073010100000024000000070043617074696F6E000600705061676573000007
      00696C46696C657300000500706D4E61760000}
  end
  object ilFiles: TImageList
    ShareImages = True
    Left = 28
    Top = 400
  end
  object pmNav: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 56
    Top = 400
  end
end
