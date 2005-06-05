inherited dGroupProps: TdGroupProps
  ActiveControl = eText
  Caption = 'Properties: Picture group'
  PixelsPerInch = 96
  TextHeight = 13
  object lID: TLabel [1]
    Left = 12
    Top = 12
    Width = 15
    Height = 13
    Caption = '&ID:'
    FocusControl = eID
  end
  object lText: TLabel [2]
    Left = 76
    Top = 12
    Width = 26
    Height = 13
    Caption = '&Text:'
    FocusControl = eText
  end
  object lDesc: TLabel [3]
    Left = 12
    Top = 52
    Width = 57
    Height = 13
    Caption = '&Description:'
    FocusControl = mDescription
  end
  inherited pButtonsBottom: TPanel
    TabOrder = 4
    inherited bCancel: TButton
      TabOrder = 1
    end
    inherited bOK: TButton
      TabOrder = 0
    end
  end
  object eID: TEdit
    Left = 12
    Top = 28
    Width = 61
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object eText: TEdit
    Left = 76
    Top = 28
    Width = 542
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = DlgDataChange
  end
  object mDescription: TMemo
    Left = 12
    Top = 68
    Width = 606
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
    OnChange = DlgDataChange
  end
  object gbGroupIcon: TGroupBox
    Left = 12
    Top = 328
    Width = 606
    Height = 57
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Group icon'
    TabOrder = 3
    object bGroupIconSelect: TButton
      Left = 44
      Top = 20
      Width = 85
      Height = 23
      Action = aGroupIconSelect
      TabOrder = 1
    end
    object bGroupIconReset: TButton
      Left = 136
      Top = 20
      Width = 85
      Height = 23
      Action = aGroupIconReset
      TabOrder = 2
    end
    object iGroupIcon: TImage32
      Left = 16
      Top = 24
      Width = 16
      Height = 16
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 4
    Top = 404
    LangData = {
      0B006447726F757050726F7073010100000003000000070043617074696F6E01
      1200000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D
      000007006243616E63656C01010000000C000000070043617074696F6E000300
      624F4B01010000000F000000070043617074696F6E0005006248656C70010100
      000012000000070043617074696F6E0003006C49440101000000150000000700
      43617074696F6E0005006C54657874010100000018000000070043617074696F
      6E0005006C4465736301010000001B000000070043617074696F6E0003006549
      4400000500655465787400000C006D4465736372697074696F6E00000B006762
      47726F757049636F6E01010000001C000000070043617074696F6E0010006247
      726F757049636F6E53656C65637400000600616C4D61696E000010006147726F
      757049636F6E53656C65637401030000001F000000070043617074696F6E1E00
      0000080043617465676F727920000000040048696E74000F006247726F757049
      636F6E526573657400000F006147726F757049636F6E52657365740103000000
      22000000070043617074696F6E21000000080043617465676F72792300000004
      0048696E74000A006947726F757049636F6E0000}
  end
  object alMain: TActionList
    Left = 32
    Top = 404
    object aGroupIconSelect: TAction
      Category = 'Group icon'
      Caption = '&Select...'
      Hint = 'Select...|Load group icon from a file'
      OnExecute = aaGroupIconSelect
    end
    object aGroupIconReset: TAction
      Category = 'Group icon'
      Caption = '&Reset'
      Hint = 'Reset|Revert group icon to the default one'
      OnExecute = aaGroupIconReset
    end
  end
end
