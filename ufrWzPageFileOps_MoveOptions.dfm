inherited frWzPageFileOps_MoveOptions: TfrWzPageFileOps_MoveOptions
  DesignSize = (
    576
    284)
  object gbFileArranging: TGroupBox
    Left = 4
    Top = 0
    Width = 567
    Height = 221
    Anchors = [akLeft, akTop, akRight]
    Caption = 'File arranging'
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      567
      221)
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
    object lReplaceChar: TLabel
      Left = 38
      Top = 194
      Width = 200
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Replace &unallowed symbols with the char:'
      FocusControl = eReplaceChar
    end
    object rbPutFlatly: TRadioButton
      Left = 16
      Top = 24
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Put all the files into the destination folder'
      TabOrder = 0
      OnClick = RBFileArrangingClick
    end
    object rbMaintainFolderLayout: TRadioButton
      Left = 16
      Top = 48
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Maintain original folder layout'
      TabOrder = 1
      OnClick = RBFileArrangingClick
    end
    object rbMaintainGroupLayout: TRadioButton
      Left = 16
      Top = 112
      Width = 535
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Arran&ge files into folders maintaining group layout'
      TabOrder = 3
      OnClick = RBFileArrangingClick
    end
    object cbAllowDuplicating: TCheckBox
      Left = 36
      Top = 172
      Width = 515
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
    object eReplaceChar: TMaskEdit
      Left = 526
      Top = 188
      Width = 27
      Height = 21
      Anchors = [akTop, akRight]
      EditMask = 'c;; '
      MaxLength = 1
      TabOrder = 6
      Text = '_'
      OnChange = PageDataChange
      OnKeyPress = eReplaceCharKeyPress
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      'eReplaceChar.EditMask'
      'eReplaceChar.Text'
      'Font.Name'
      'SecondaryShortCuts')
    Left = 536
    Top = 12
    LangData = {
      1B006672577A5061676546696C654F70735F4D6F76654F7074696F6E7300010B
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
      00000C00655265706C616365436861720000}
  end
end
