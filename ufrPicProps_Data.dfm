inherited frPicProps_Data: TfrPicProps_Data
  Height = 298
  object lPlace: TLabel
    Left = 192
    Top = 8
    Width = 29
    Height = 13
    Caption = '&Place:'
    FocusControl = cbPlace
  end
  object lDesc: TLabel
    Left = 8
    Top = 128
    Width = 132
    Height = 13
    Caption = 'D&escription (for displaying):'
    FocusControl = mDesc
  end
  object lFilmNumber: TLabel
    Left = 8
    Top = 48
    Width = 103
    Height = 13
    Caption = '&Film number or name:'
    FocusControl = cbFilmNumber
  end
  object lFrameNumber: TLabel
    Left = 447
    Top = 48
    Width = 73
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'F&rame number:'
    FocusControl = eFrameNumber
  end
  object lAuthor: TLabel
    Left = 8
    Top = 88
    Width = 37
    Height = 13
    Caption = '&Author:'
    FocusControl = cbAuthor
  end
  object lNotes: TLabel
    Left = 8
    Top = 200
    Width = 110
    Height = 13
    Caption = '&Notes (additional info):'
    FocusControl = mNotes
  end
  object lDate: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = '&Date:'
    FocusControl = eDate
  end
  object lTime: TLabel
    Left = 120
    Top = 8
    Width = 26
    Height = 13
    Caption = '&Time:'
    FocusControl = eTime
  end
  object lMedia: TLabel
    Left = 8
    Top = 254
    Width = 100
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Media name or code:'
    FocusControl = cbMedia
  end
  object cbPlace: TComboBox
    Left = 192
    Top = 24
    Width = 374
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    OnChange = PicPropEditorChange
  end
  object mDesc: TMemo
    Left = 8
    Top = 144
    Width = 558
    Height = 53
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssVertical
    TabOrder = 6
    OnChange = PicPropEditorChange
  end
  object cbFilmNumber: TComboBox
    Left = 8
    Top = 64
    Width = 436
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnChange = PicPropEditorChange
  end
  object eFrameNumber: TEdit
    Left = 447
    Top = 64
    Width = 119
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 4
    OnChange = PicPropEditorChange
  end
  object cbAuthor: TComboBox
    Left = 8
    Top = 104
    Width = 558
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    ItemHeight = 13
    Sorted = True
    TabOrder = 5
    OnChange = PicPropEditorChange
  end
  object mNotes: TMemo
    Left = 8
    Top = 216
    Width = 558
    Height = 35
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 7
    OnChange = PicPropEditorChange
  end
  object eDate: TDateEdit
    Left = 8
    Top = 24
    Width = 108
    Height = 21
    BlanksChar = #8230
    NumGlyphs = 2
    TabOrder = 0
    OnChange = PicPropEditorChange
  end
  object eTime: TMaskEdit
    Left = 120
    Top = 24
    Width = 69
    Height = 21
    EditMask = '!99:99:99;1;'#8230
    MaxLength = 8
    TabOrder = 1
    Text = '  :  :  '
    OnChange = PicPropEditorChange
  end
  object cbMedia: TComboBox
    Left = 8
    Top = 270
    Width = 558
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    DropDownCount = 16
    ItemHeight = 13
    Sorted = True
    TabOrder = 8
    OnChange = PicPropEditorChange
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      'eTime.EditMask'
      'eTime.Text'
      'Font.Name'
      'SecondaryShortCuts')
    Left = 476
    Top = 212
    LangData = {
      0F00667250696350726F70735F4461746100011200000006006C506C61636501
      0100000007000000070043617074696F6E0005006C4465736301010000000A00
      0000070043617074696F6E000B006C46696C6D4E756D62657201010000000D00
      0000070043617074696F6E000C006C4672616D654E756D626572010100000010
      000000070043617074696F6E0007006C417574686F7201010000001300000007
      0043617074696F6E0006006C4E6F746573010100000016000000070043617074
      696F6E0005006C44617465010100000019000000070043617074696F6E000500
      6C54696D6501010000001C000000070043617074696F6E0006006C4D65646961
      01010000001F000000070043617074696F6E0007006362506C61636500000500
      6D4465736300000C00636246696C6D4E756D62657200000C00654672616D654E
      756D626572000008006362417574686F72000006006D4E6F7465730000050065
      44617465000005006554696D650000070063624D656469610000}
  end
end
