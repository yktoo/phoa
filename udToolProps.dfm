inherited dToolProps: TdToolProps
  Left = 443
  Top = 313
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
  object cbKind: TComboBox
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
  object eName: TEdit
    Left = 12
    Top = 68
    Width = 281
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = DlgDataChange
  end
  object eHint: TEdit
    Left = 296
    Top = 68
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = DlgDataChange
  end
  object eRunCommand: TEdit
    Left = 12
    Top = 108
    Width = 485
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = DlgDataChange
  end
  object bBrowseRunCommand: TButton
    Left = 500
    Top = 108
    Width = 81
    Height = 23
    Anchors = [akTop, akRight]
    Caption = '&Browse...'
    TabOrder = 5
    OnClick = bBrowseRunCommandClick
  end
  object eRunParams: TEdit
    Left = 12
    Top = 148
    Width = 365
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = DlgDataChange
  end
  object eRunFolder: TEdit
    Left = 12
    Top = 188
    Width = 485
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = DlgDataChange
  end
  object bBrowseRunFolder: TButton
    Left = 500
    Top = 188
    Width = 81
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Bro&wse...'
    TabOrder = 9
    OnClick = bBrowseRunFolderClick
  end
  object cbRunShowCommand: TComboBox
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
  object gbUsage: TGroupBox
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
  object eMasks: TEdit
    Left = 12
    Top = 28
    Width = 281
    Height = 21
    TabOrder = 0
    OnChange = DlgDataChange
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 40
    Top = 336
    LangData = {
      0A0064546F6F6C50726F7073010100000003000000070043617074696F6E011D
      00000008006276426F74746F6D00000E0070427574746F6E73426F74746F6D00
      0007006243616E63656C01010000000C000000070043617074696F6E00030062
      4F4B01010000000F000000070043617074696F6E0005006248656C7001010000
      0012000000070043617074696F6E0005006C4B696E6401010000001500000007
      0043617074696F6E0005006C4E616D6501010000001800000007004361707469
      6F6E0005006C48696E7401010000001B000000070043617074696F6E000B006C
      52756E436F6D6D616E6401010000001E000000070043617074696F6E000A006C
      52756E506172616D73010100000021000000070043617074696F6E000A006C52
      756E466F6C646572010100000024000000070043617074696F6E000F006C5275
      6E53686F77436F6D6D616E64010100000027000000070043617074696F6E0006
      006C4D61736B7301010000002A000000070043617074696F6E00060063624B69
      6E6400000500654E616D65000005006548696E7400000B006552756E436F6D6D
      616E64000011006242726F77736552756E436F6D6D616E6401010000003E0000
      00070043617074696F6E000A006552756E506172616D7300000A006552756E46
      6F6C646572000010006242726F77736552756E466F6C64657201010000004900
      0000070043617074696F6E001000636252756E53686F77436F6D6D616E640101
      0000004E00000005004974656D73000700676255736167650101000000510000
      00070043617074696F6E000C0063625573616765546F6F6C7301010000005400
      0000070043617074696F6E000D006362557361676547726F7570730101000000
      57000000070043617074696F6E000F0063625573616765546856696577657201
      010000005A000000070043617074696F6E000F0063625573616765566965774D
      6F646501010000005D000000070043617074696F6E000600654D61736B730000
      0C006F6452756E436F6D6D616E64010200000064000000060046696C74657266
      00000005005469746C6500}
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
