inherited dSelKeywords: TdSelKeywords
  Top = 213
  Caption = 'Select keywords'
  ClientHeight = 431
  ClientWidth = 354
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lMain: TLabel [0]
    Left = 12
    Top = 12
    Width = 186
    Height = 26
    Caption = '&Select one or more keywords from the complete list:'
    WordWrap = True
  end
  inherited bvBottom: TBevel
    Top = 394
    Width = 354
  end
  object bReset: TButton [2]
    Left = 8
    Top = 399
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Reset'
    TabOrder = 0
    OnClick = bResetClick
  end
  object tvMain: TVirtualStringTree [3]
    Left = 12
    Top = 40
    Width = 331
    Height = 341
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = fMain.ilActionsSmall
    ParentBackground = False
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    TreeOptions.StringOptions = [toSaveCaptions, toShowStaticText, toAutoAcceptEditChange]
    OnChecked = tvMainChecked
    OnGetText = tvMainGetText
    OnPaintText = tvMainPaintText
    OnGetImageIndex = tvMainGetImageIndex
    OnInitNode = tvMainInitNode
    Columns = <>
    WideDefaultText = ''
  end
  inherited pButtonsBottom: TPanel
    Top = 396
    Width = 354
    TabOrder = 2
    DesignSize = (
      354
      35)
    inherited bCancel: TButton
      Left = 195
    end
    inherited bOK: TButton
      Left = 115
    end
    inherited bHelp: TButton
      Left = 273
    end
  end
  inherited dtlsMain: TDTLanguageSwitcher
    Left = 12
    Top = 40
    LangData = {
      0C006453656C4B6579776F72647304000000070043617074696F6E0500000019
      041300C2FBE1EEF020EAEBFEF7E5E2FBF520F1EBEEE209040F0053656C656374
      206B6579776F726473070419005363686CFC7373656C77F67274657220617573
      77E4686C656E1604180053656C6563696F6E61722070616C617672612D636861
      766522041300C2E8E1B3F020EAEBFEF7EEE2E8F520F1EBB3E2080048656C7046
      696C650500000019040000090400000704000016040000220400000B0048656C
      704B6579776F7264050000001904000009040000070400001604000022040000
      040048696E740500000019040000090400000704000016040000220400000000
      00000700000007006243616E63656C03000000070043617074696F6E05000000
      19040600CEF2ECE5EDE00904060043616E63656C070409004162627265636865
      6E1604080043616E63656C617222040700C2B3E4ECB3EDE00B0048656C704B65
      79776F7264050000001904000009040000070400001604000022040000040048
      696E740500000019040000090400000704000016040000220400000000000000
      00000005006248656C7003000000070043617074696F6E0500000019040700D1
      EFF0E0E2EAE00904040048656C700704050048696C666516040500416A756461
      22040700C4EEE2B3E4EAE00B0048656C704B6579776F72640500000019040000
      09040000070400001604000022040000040048696E7405000000190400000904
      000007040000160400002204000000000000000000000300624F4B0300000007
      0043617074696F6E0500000019040200CECA090402004F4B070402004F4B1604
      02004F4B22040200CECA0B0048656C704B6579776F7264050000001904000009
      040000070400001604000022040000040048696E740500000019040000090400
      0007040000160400002204000000000000000000000600625265736574030000
      00070043617074696F6E050000001904060026D1E1F0EEF10904060026526573
      657407040D00265A7572FC636B7365747A656E16040A00265265696E69636961
      722204090026D1EAE8E4E0EDEDFF0B0048656C704B6579776F72640500000019
      04000009040000070400001604000022040000040048696E7405000000190400
      000904000007040000160400002204000000000000000000000700636C624D61
      696E040000000B0048656C704B6579776F726405000000190400000904000007
      0400001604000022040000040048696E74050000001904000009040000070400
      0016040000220400000700496D654E616D650500000019040000090400000704
      0000160400002204000005004974656D73050000001904000009040000070400
      0016040000220400000000000000000000080064746C734D61696E0000000000
      0000000000000005006C4D61696E03000000070043617074696F6E0500000019
      04400026C2FBE1E5F0E8F2E520E8E720F1EFE8F1EAE020E8F1EFEEEBFCE7F3FE
      F9E8F5F1FF20E220F4EEF2EEE0EBFCE1EEECE520EAEBFEF7E5E2FBE520F1EBEE
      E2E03A090434002653656C656374206F6E65206F72206D6F7265206B6579776F
      7264732066726F6D2074686520636F6D706C657465206C6973743A07043B0026
      57E4686C656E205369652065696E206F646572206D656872657265205363686C
      FC7373656C77F6727465722061757320646572204C697374653A160430002653
      656C6563696F6E617220756D61206F75206D6169732070616C61767261732D63
      68617665206461206C697374613A2204440026C2E8E1E5F0B3F2FC20E7B320F1
      EFB3F1EAF320EAEBFEF7EEE2B320F1EBEEE2E02C20F9EE20E2E8EAEEF0E8F1F2
      EEE2F3FEF2F1FF20F320F4EEF2EEE0EBFCE1EEECB33A0B0048656C704B657977
      6F7264050000001904000009040000070400001604000022040000040048696E
      7405000000190400000904000007040000160400002204000000000000000000
      00}
  end
end
