inherited frWzPageFileOps_SelTask: TfrWzPageFileOps_SelTask
  object lCopyFiles: TLabel
    Left = 28
    Top = 24
    Width = 543
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Copies the files of pictures you select, in the specified folder' +
      '. Use this option to prepare photo album to write onto a CD or D' +
      'VD'
    Transparent = False
    WordWrap = True
  end
  object lMoveFiles: TLabel
    Left = 28
    Top = 76
    Width = 543
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Moves the files of pictures you select, into the specified folde' +
      'r, and updates picture file links appropriately'
    Transparent = False
    WordWrap = True
  end
  object lDeleteFiles: TLabel
    Left = 28
    Top = 128
    Width = 543
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Deletes pictures you select, and physically deletes correspondin' +
      'g files. Use this option to delete unwanted picture files'
    Transparent = False
    WordWrap = True
  end
  object lRepairFileLinks: TLabel
    Left = 28
    Top = 232
    Width = 543
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Allows you to repair broken file links by finding the files in t' +
      'he specified folder, and, optionally, to delete pictures no long' +
      'er associated with valid (existent) files'
    Transparent = False
    WordWrap = True
  end
  object lNBUndoable: TLabel
    Left = 0
    Top = 264
    Width = 576
    Height = 20
    Align = alBottom
    AutoSize = False
    Caption = #8226' NB: These operations are not undoable!'
    Layout = tlCenter
    WordWrap = True
  end
  object lRebuildThumbs: TLabel
    Left = 28
    Top = 180
    Width = 543
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Rereads the picture files, recreates the thumbnails and updates ' +
      'the file-related information, such as image dimensions'
    Transparent = False
    WordWrap = True
  end
  object rbCopyFiles: TRadioButton
    Left = 4
    Top = 4
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Copy picture files to a folder'
    TabOrder = 0
    OnClick = PageDataChange
  end
  object rbMoveFiles: TRadioButton
    Left = 4
    Top = 56
    Width = 551
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Move picture files to a folder'
    TabOrder = 1
    OnClick = PageDataChange
  end
  object rbDeleteFiles: TRadioButton
    Left = 4
    Top = 108
    Width = 551
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Delete pictures and files'
    TabOrder = 2
    OnClick = PageDataChange
  end
  object rbRepairFileLinks: TRadioButton
    Left = 4
    Top = 212
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Re&pair picture file links'
    TabOrder = 4
    OnClick = PageDataChange
  end
  object rbRebuildThumbs: TRadioButton
    Left = 4
    Top = 160
    Width = 567
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Rebuild thumbnails'
    TabOrder = 3
    OnClick = PageDataChange
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 536
    Top = 264
    LangData = {
      17006672577A5061676546696C654F70735F53656C5461736B00010B0000000A
      006C436F707946696C6573010100000007000000070043617074696F6E000A00
      6C4D6F766546696C657301010000000A000000070043617074696F6E000C006C
      44656C65746546696C657301010000000D000000070043617074696F6E001000
      6C52657061697246696C654C696E6B7301010000001000000007004361707469
      6F6E000B006C4E42556E646F61626C6501010000001300000007004361707469
      6F6E000E006C52656275696C645468756D627301010000001600000007004361
      7074696F6E000B007262436F707946696C657301010000001900000007004361
      7074696F6E000B0072624D6F766546696C657301010000001C00000007004361
      7074696F6E000D00726244656C65746546696C657301010000001F0000000700
      43617074696F6E001100726252657061697246696C654C696E6B730101000000
      22000000070043617074696F6E000F00726252656275696C645468756D627301
      0100000025000000070043617074696F6E00}
  end
end
