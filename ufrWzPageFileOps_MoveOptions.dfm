inherited frWzPageFileOps_MoveOptions: TfrWzPageFileOps_MoveOptions
  DesignSize = (
    576
    284)
  object gbFileArranging: TGroupBox
    Left = 4
    Top = 0
    Width = 567
    Height = 197
    Anchors = [akLeft, akTop, akRight]
    Caption = 'File arranging'
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      567
      197)
    object lBaseFolder: TLabel
      Left = 36
      Top = 68
      Width = 99
      Height = 13
      Caption = 'Use path &relative to:'
      FocusControl = cbBaseFolder
    end
    object lBaseGroup: TLabel
      Left = 36
      Top = 132
      Width = 130
      Height = 13
      Caption = 'Use path r&elative to group:'
      FocusControl = cbBaseGroup
    end
    object rbPutFlatly: TRadioButton
      Left = 16
      Top = 24
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Put all the files into the destination folder'
      TabOrder = 0
      OnClick = AdjustOptionsNotify
    end
    object rbMaintainFolderLayout: TRadioButton
      Left = 16
      Top = 48
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Maintain original folder layout'
      TabOrder = 1
      OnClick = AdjustOptionsNotify
    end
    object rbMaintainGroupLayout: TRadioButton
      Left = 16
      Top = 112
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Arran&ge files into folders maintaining group layout'
      TabOrder = 3
      OnClick = AdjustOptionsNotify
    end
    object cbAllowDuplicating: TCheckBox
      Left = 36
      Top = 172
      Width = 517
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Allo&w multiple copies of the file'
      TabOrder = 5
      OnClick = PageDataChange
    end
    object cbBaseFolder: TComboBox
      Left = 36
      Top = 84
      Width = 515
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 2
      OnChange = PageDataChange
    end
    object cbBaseGroup: TComboBox
      Left = 36
      Top = 148
      Width = 515
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 4
      OnChange = PageDataChange
    end
  end
  object gbFileNaming: TGroupBox
    Left = 4
    Top = 204
    Width = 567
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'File naming'
    TabOrder = 1
    DesignSize = (
      567
      73)
    object lReplaceChar: TLabel
      Left = 16
      Top = 48
      Width = 200
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Replace &unallowed symbols with the char:'
      FocusControl = eReplaceChar
    end
    object cbRenameFiles: TCheckBox
      Left = 16
      Top = 20
      Width = 293
      Height = 17
      Caption = 'Rename files using &format:'
      TabOrder = 0
      OnClick = AdjustOptionsNotify
    end
    object eFileNameFormat: TEdit
      Left = 312
      Top = 20
      Width = 241
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = PageDataChange
    end
    object eReplaceChar: TMaskEdit
      Left = 528
      Top = 44
      Width = 25
      Height = 21
      Anchors = [akTop, akRight]
      EditMask = 'c;; '
      MaxLength = 1
      TabOrder = 2
      Text = '_'
      OnChange = PageDataChange
      OnKeyPress = eReplaceCharKeyPress
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'eReplaceChar.*')
    Left = 536
    Top = 12
    LangData = {
      1B006672577A5061676546696C654F70735F4D6F76654F7074696F6E7300010E
      0000000F00676246696C65417272616E67696E67010100000007000000070043
      617074696F6E000B006C42617365466F6C64657201010000000A000000070043
      617074696F6E000A006C4261736547726F757001010000000D00000007004361
      7074696F6E000C006C5265706C61636543686172010100000010000000070043
      617074696F6E000B007262507574466C61746C79010100000013000000070043
      617074696F6E00160072624D61696E7461696E466F6C6465724C61796F757401
      0100000016000000070043617074696F6E00150072624D61696E7461696E4772
      6F75704C61796F7574010100000019000000070043617074696F6E0012006362
      416C6C6F774475706C69636174696E6701010000001C00000007004361707469
      6F6E000C00636242617365466F6C64657200000B0063624261736547726F7570
      00000C00655265706C6163654368617200000C00676246696C654E616D696E67
      01010000001D000000070043617074696F6E000D00636252656E616D6546696C
      657301010000001E000000070043617074696F6E000F006546696C654E616D65
      466F726D61740000}
  end
end
