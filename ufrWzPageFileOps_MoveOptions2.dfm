inherited frWzPageFileOps_MoveOptions2: TfrWzPageFileOps_MoveOptions2
  DesignSize = (
    576
    284)
  object lOverwriteMode: TLabel
    Left = 4
    Top = 92
    Width = 151
    Height = 13
    Caption = 'If the target file already e&xists:'
    FocusControl = cbOverwriteMode
  end
  object lNoOriginalFileMode: TLabel
    Left = 4
    Top = 4
    Width = 156
    Height = 13
    Caption = 'If the &original file does not exist:'
    FocusControl = cbNoOriginalFileMode
  end
  object cbDeleteOriginal: TCheckBox
    Left = 4
    Top = 48
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Delete original files'
    TabOrder = 1
    OnClick = AdjustOptionsNotify
  end
  object cbDeleteToRecycleBin: TCheckBox
    Left = 24
    Top = 68
    Width = 547
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delete to the rec&ycle bin'
    TabOrder = 2
    OnClick = PageDataChange
  end
  object cbUseCDOptions: TCheckBox
    Left = 4
    Top = 264
    Width = 567
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Set up &CD/DVD creation options as well'
    TabOrder = 4
    OnClick = PageDataChange
  end
  object cbOverwriteMode: TComboBox
    Left = 4
    Top = 108
    Width = 567
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Skip the current picture'
      'Prompt to overwrite the file'
      'Always overwrite the file')
  end
  object cbNoOriginalFileMode: TComboBox
    Left = 4
    Top = 20
    Width = 567
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'Treat as an error'
      'Update the link only if the target file exists'
      'Update the link anyway')
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 540
    Top = 140
    LangData = {
      1C006672577A5061676546696C654F70735F4D6F76654F7074696F6E73320001
      070000000E006C4F76657277726974654D6F6465010100000007000000070043
      617074696F6E0013006C4E6F4F726967696E616C46696C654D6F646501010000
      000A000000070043617074696F6E001000636244656C6574654F726967696E61
      6C01010000000D000000070043617074696F6E001400636244656C657465546F
      52656379636C6542696E010100000010000000070043617074696F6E000E0063
      6255736543444F7074696F6E73010100000013000000070043617074696F6E00
      0F0063624F76657277726974654D6F646501010000001800000005004974656D
      7300140063624E6F4F726967696E616C46696C654D6F646501010000001D0000
      0005004974656D7300}
  end
end
