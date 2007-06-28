inherited frWzPageFileOps_RepairOptions: TfrWzPageFileOps_RepairOptions
  DesignSize = (
    576
    284)
  object gbMatchFindingOptions: TTntGroupBox
    Left = 4
    Top = 0
    Width = 567
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Searching for the matching file'
    ParentBackground = False
    TabOrder = 0
    object rbSearchByName: TTntRadioButton
      Left = 16
      Top = 24
      Width = 497
      Height = 17
      Caption = 'Match file by its &name'
      TabOrder = 0
      OnClick = PageDataChange
    end
    object rbSearchBySize: TTntRadioButton
      Left = 16
      Top = 44
      Width = 497
      Height = 17
      Caption = 'Match file by its si&ze'
      TabOrder = 1
      OnClick = PageDataChange
    end
    object rbSearchByNameSize: TTntRadioButton
      Left = 16
      Top = 64
      Width = 497
      Height = 17
      Caption = 'Match file by both name and siz&e'
      TabOrder = 2
      OnClick = PageDataChange
    end
  end
  object cbLookSubfolders: TTntCheckBox
    Left = 4
    Top = 176
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Look in the subfolders of the destination folder'
    TabOrder = 2
    OnClick = PageDataChange
  end
  object gbAlreadyInUseMode: TTntGroupBox
    Left = 4
    Top = 104
    Width = 567
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 'If a matching file found but is in use by another picture'
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      567
      65)
    object rbSkipFilesInUse: TTntRadioButton
      Left = 16
      Top = 20
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'S&kip it and look for the next match'
      TabOrder = 0
      OnClick = PageDataChange
    end
    object rbRelinkFilesInUse: TTntRadioButton
      Left = 16
      Top = 40
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Delete the picture the file was linked to, and relink it to the ' +
        'ne&w picture'
      TabOrder = 1
      OnClick = PageDataChange
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 540
    Top = 184
    LangData = {
      1D006672577A5061676546696C654F70735F5265706169724F7074696F6E7300
      0108000000150067624D6174636846696E64696E674F7074696F6E7301010000
      0007000000070043617074696F6E000E00726253656172636842794E616D6501
      010000000A000000070043617074696F6E000E00726253656172636842795369
      7A6501010000000D000000070043617074696F6E001200726253656172636842
      794E616D6553697A65010100000010000000070043617074696F6E0010006362
      4C6F6F6B537562666F6C64657273010100000013000000070043617074696F6E
      0012006762416C7265616479496E5573654D6F64650101000000160000000700
      43617074696F6E0010007262536B697046696C6573496E557365010100000019
      000000070043617074696F6E001200726252656C696E6B46696C6573496E5573
      6501010000001C000000070043617074696F6E00}
  end
end
