inherited dToolProps: TdToolProps
  Left = 408
  Top = 211
  Caption = 'Properties: Tool'
  ClientHeight = 371
  ClientWidth = 592
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 334
    Width = 592
  end
  object lKind: TLabel [1]
    Left = 296
    Top = 12
    Width = 24
    Height = 13
    Caption = '&Kind:'
    FocusControl = cbKind
  end
  object lName: TLabel [2]
    Left = 12
    Top = 52
    Width = 250
    Height = 13
    Caption = '&Name (caption; use '#39'&&'#39' to indicate the shortcut key):'
  end
  object lHint: TLabel [3]
    Left = 296
    Top = 52
    Width = 23
    Height = 13
    Caption = '&Hint:'
  end
  object lRunCommand: TLabel [4]
    Left = 12
    Top = 92
    Width = 113
    Height = 13
    Caption = '&Command (application):'
  end
  object lRunParams: TLabel [5]
    Left = 12
    Top = 132
    Width = 59
    Height = 13
    Caption = '&Parameters:'
  end
  object lRunFolder: TLabel [6]
    Left = 12
    Top = 172
    Width = 163
    Height = 13
    Caption = '&Folder (leave empty for defaults):'
  end
  object lRunShowCommand: TLabel [7]
    Left = 380
    Top = 132
    Width = 70
    Height = 13
    Caption = 'W&indow state:'
    FocusControl = cbRunShowCommand
  end
  object lMasks: TLabel [8]
    Left = 12
    Top = 12
    Width = 198
    Height = 13
    Caption = 'File &masks (empty mask matches all files):'
  end
  inherited pButtonsBottom: TPanel
    Top = 336
    Width = 592
    TabOrder = 11
    inherited bCancel: TButton
      Left = 433
    end
    inherited bOK: TButton
      Left = 353
    end
    inherited bHelp: TButton
      Left = 511
    end
  end
  object cbKind: TComboBox [10]
    Left = 296
    Top = 28
    Width = 285
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 1
    OnChange = cbKindChange
    OnDrawItem = cbKindDrawItem
  end
  object eName: TEdit [11]
    Left = 12
    Top = 68
    Width = 281
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = DlgDataChange
  end
  object eHint: TEdit [12]
    Left = 296
    Top = 68
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = DlgDataChange
  end
  object eRunCommand: TEdit [13]
    Left = 12
    Top = 108
    Width = 485
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = DlgDataChange
  end
  object bBrowseRunCommand: TButton [14]
    Left = 500
    Top = 108
    Width = 81
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '&Browse...'
    TabOrder = 5
    OnClick = bBrowseRunCommandClick
  end
  object eRunParams: TEdit [15]
    Left = 12
    Top = 148
    Width = 365
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = DlgDataChange
  end
  object eRunFolder: TEdit [16]
    Left = 12
    Top = 188
    Width = 485
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = DlgDataChange
  end
  object bBrowseRunFolder: TButton [17]
    Left = 500
    Top = 188
    Width = 81
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Bro&wse...'
    TabOrder = 9
    OnClick = bBrowseRunFolderClick
  end
  object cbRunShowCommand: TComboBox [18]
    Left = 380
    Top = 148
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    OnChange = DlgDataChange
    Items.Strings = (
      'Normal'
      'Minimized'
      'Maximized')
  end
  object gbUsage: TGroupBox [19]
    Left = 12
    Top = 212
    Width = 569
    Height = 109
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Tool usage'
    ParentBackground = False
    TabOrder = 10
    DesignSize = (
      569
      109)
    object cbUsageTools: TCheckBox
      Left = 16
      Top = 20
      Width = 537
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show in Tools menu'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = DlgDataChange
    end
    object cbUsageGroups: TCheckBox
      Left = 16
      Top = 40
      Width = 537
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show in Picture Groups popup menu'
      TabOrder = 1
      OnClick = DlgDataChange
    end
    object cbUsageThViewer: TCheckBox
      Left = 16
      Top = 60
      Width = 537
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show in Thumbnails Window popup menu'
      TabOrder = 2
      OnClick = DlgDataChange
    end
    object cbUsageViewMode: TCheckBox
      Left = 16
      Top = 80
      Width = 537
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show in View mode popup menu'
      TabOrder = 3
      OnClick = DlgDataChange
    end
  end
  object eMasks: TEdit [20]
    Left = 12
    Top = 28
    Width = 281
    Height = 21
    TabOrder = 0
    OnChange = DlgDataChange
  end
  inherited dklcMain: TDKLanguageController
    Left = 68
    Top = 336
    LangData = {
      0A0064546F6F6C50726F7073010100000003000000070043617074696F6E011E
      000000080064746C734D61696E000008006276426F74746F6D00000E00704275
      74746F6E73426F74746F6D000007006243616E63656C01010000000C00000007
      0043617074696F6E000300624F4B01010000000F000000070043617074696F6E
      0005006248656C70010100000012000000070043617074696F6E0005006C4B69
      6E64010100000015000000070043617074696F6E0005006C4E616D6501010000
      0018000000070043617074696F6E0005006C48696E7401010000001B00000007
      0043617074696F6E000B006C52756E436F6D6D616E6401010000001E00000007
      0043617074696F6E000A006C52756E506172616D730101000000210000000700
      43617074696F6E000A006C52756E466F6C646572010100000024000000070043
      617074696F6E000F006C52756E53686F77436F6D6D616E640101000000270000
      00070043617074696F6E0006006C4D61736B7301010000002A00000007004361
      7074696F6E00060063624B696E6400000500654E616D65000005006548696E74
      00000B006552756E436F6D6D616E64000011006242726F77736552756E436F6D
      6D616E6401010000003E000000070043617074696F6E000A006552756E506172
      616D7300000A006552756E466F6C646572000010006242726F77736552756E46
      6F6C646572010100000049000000070043617074696F6E001000636252756E53
      686F77436F6D6D616E6401010000004E00000005004974656D73000700676255
      73616765010100000051000000070043617074696F6E000C0063625573616765
      546F6F6C73010100000054000000070043617074696F6E000D00636255736167
      6547726F757073010100000057000000070043617074696F6E000F0063625573
      616765546856696577657201010000005A000000070043617074696F6E000F00
      63625573616765566965774D6F646501010000005D000000070043617074696F
      6E000600654D61736B7300000C006F6452756E436F6D6D616E64010200000064
      000000060046696C7465726600000005005469746C6500}
  end
  object odRunCommand: TOpenDialog
    Filter = 
      'Applications (*.bat;*.com;*.exe)|*.bat;*.com;*.exe|All files (*.' +
      '*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select an application'
    Left = 12
    Top = 336
  end
end
