inherited frWzPageFileOps_DelOptions: TfrWzPageFileOps_DelOptions
  DesignSize = (
    576
    284)
  object cbDeleteToRecycleBin: TCheckBox
    Left = 4
    Top = 4
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delete files to the Rec&ycle Bin'
    TabOrder = 0
    OnClick = PageDataChange
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 504
    Top = 36
    LangData = {
      1A006672577A5061676546696C654F70735F44656C4F7074696F6E7300010100
      00001400636244656C657465546F52656379636C6542696E0101000000070000
      00070043617074696F6E00}
  end
end
