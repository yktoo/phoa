inherited frWzPageFileOps_CDOptions: TfrWzPageFileOps_CDOptions
  DesignSize = (
    576
    284)
  object lMediaLabel: TLabel
    Left = 24
    Top = 236
    Width = 93
    Height = 13
    Caption = '&Label the media as:'
    FocusControl = eMediaLabel
  end
  object lPhoaDesc: TLabel
    Left = 24
    Top = 48
    Width = 118
    Height = 13
    Caption = '&Photo album description:'
    FocusControl = mPhoaDesc
  end
  object cbCopyExecutable: TCheckBox
    Left = 4
    Top = 176
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Copy the p&rogram itself to the destination folder'
    TabOrder = 4
    OnClick = AdjustOptionsNotify
  end
  object cbCreatePhoa: TCheckBox
    Left = 4
    Top = 4
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Create a photo album &file with selected pictures to:'
    TabOrder = 0
    OnClick = AdjustOptionsNotify
  end
  object ePhoaFileName: TEdit
    Left = 24
    Top = 24
    Width = 547
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = PageDataChange
  end
  object cbCreateAutorun: TCheckBox
    Left = 4
    Top = 216
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Create &autorun.inf file for autoplaying the CD/DVD, in the dest' +
      'ination folder'
    TabOrder = 6
    OnClick = AdjustOptionsNotify
  end
  object eMediaLabel: TEdit
    Left = 24
    Top = 252
    Width = 547
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = PageDataChange
  end
  object mPhoaDesc: TMemo
    Left = 24
    Top = 64
    Width = 547
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssVertical
    TabOrder = 2
    OnChange = PageDataChange
  end
  object cbIncludeViews: TCheckBox
    Left = 24
    Top = 156
    Width = 547
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Also include existing &views'
    TabOrder = 3
    OnClick = PageDataChange
  end
  object cbCopyIniSettings: TCheckBox
    Left = 24
    Top = 196
    Width = 547
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Copy the current program settings to phoa.&ini'
    TabOrder = 5
    OnClick = PageDataChange
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      'Font.Name'
      'SecondaryShortCuts')
    Left = 60
    Top = 76
    LangData = {
      19006672577A5061676546696C654F70735F43444F7074696F6E7300010A0000
      000B006C4D656469614C6162656C010100000007000000070043617074696F6E
      0009006C50686F614465736301010000000A000000070043617074696F6E0010
      006362436F707945786563757461626C6501010000000D000000070043617074
      696F6E000C00636243726561746550686F610101000000100000000700436170
      74696F6E000D006550686F6146696C654E616D6500000F006362437265617465
      4175746F72756E010100000017000000070043617074696F6E000B00654D6564
      69614C6162656C000009006D50686F614465736300000E006362496E636C7564
      655669657773010100000022000000070043617074696F6E0011006362436F70
      79496E6953657474696E6773010100000025000000070043617074696F6E00}
  end
end
