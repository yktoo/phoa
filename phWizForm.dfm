object PhoaWizardForm: TPhoaWizardForm
  Left = 424
  Top = 242
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = '<wizard caption>'
  ClientHeight = 401
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinHeight = 430
  Constraints.MinWidth = 600
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object bvBottom: TBevel
    Left = 0
    Top = 363
    Width = 592
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object bvTopPanel: TBevel
    Left = 0
    Top = 61
    Width = 592
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object pMain: TPanel
    Left = 0
    Top = 63
    Width = 592
    Height = 300
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 0
  end
  object pButtons: TPanel
    Left = 0
    Top = 365
    Width = 592
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      592
      36)
    object bCancel: TButton
      Left = 508
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = bCancelClick
    end
    object bNext: TButton
      Left = 368
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Next >'
      Default = True
      TabOrder = 2
      OnClick = bNextClick
    end
    object bHelp: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 23
      Caption = 'Help'
      TabOrder = 0
      OnClick = bHelpClick
    end
    object bBack: TButton
      Left = 288
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '< Back'
      TabOrder = 1
      OnClick = bBackClick
    end
  end
  object pHeader: TPanel
    Left = 0
    Top = 0
    Width = 592
    Height = 61
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 8
    Color = clWindow
    ParentBackground = False
    TabOrder = 2
    object lHeading: TLabel
      Left = 8
      Top = 8
      Width = 531
      Height = 45
      Align = alClient
      AutoSize = False
      Caption = '<heading>'
      Transparent = False
      Layout = tlCenter
      WordWrap = True
    end
    object iIcon: TImage
      Left = 539
      Top = 8
      Width = 45
      Height = 45
      Align = alRight
      Center = True
    end
  end
  object dtlsMain: TDTLanguageSwitcher
    Language = 1033
    Left = 120
    Top = 360
    LangData = {
      0E0050686F6157697A617264466F726D04000000070043617074696F6E050000
      0019041600C4EEE1E0E2EBE5EDE8E520E8E7EEE1F0E0E6E5EDE8E9090410003C
      77697A6172642063617074696F6E3E0704110042696C6465722068696E7A7566
      FC67656E1604100041646963696F6E617220696D6167656D22041300C4EEE4E0
      E2E0EDEDFF20E7EEE1F0E0E6E5EDFC080048656C7046696C6505000000190400
      00090400000704000016040000220400000B0048656C704B6579776F72640500
      00001904000009040000070400001604000022040000040048696E7405000000
      1904000009040000070400001604000022040000000000000900000005006242
      61636B03000000070043617074696F6E05000000190407003C20CDE0E7E0E409
      0406003C204261636B070408003C205A7572FC636B160408003C20566F6C7461
      72220407003C20CDE0E7E0E40B0048656C704B6579776F726405000000190400
      0009040000070400001604000022040000040048696E74050000001904000009
      040000070400001604000022040000000000000000000007006243616E63656C
      03000000070043617074696F6E0500000019040600CEF2ECE5EDE00904060043
      616E63656C0704090041626272656368656E1604080043616E63656C61722204
      0700C2B3E4ECB3EDE00B0048656C704B6579776F726405000000190400000904
      0000070400001604000022040000040048696E74050000001904000009040000
      070400001604000022040000000000000000000005006248656C700300000007
      0043617074696F6E0500000019040700D1EFF0E0E2EAE00904040048656C7007
      04050048696C666516040500416A75646122040700C4EEE2B3E4EAE00B004865
      6C704B6579776F72640500000019040000090400000704000016040000220400
      00040048696E7405000000190400000904000007040000160400002204000000
      000000000000000500624E65787403000000070043617074696F6E0500000019
      040700C4E0EBE5E5203E090406004E657874203E07040800576569746572203E
      160409005072F378696D61203E22040600C4E0EBB3203E0B0048656C704B6579
      776F726405000000190400000904000007040000160400002204000004004869
      6E74050000001904000009040000070400001604000022040000000000000000
      000008006276426F74746F6D020000000B0048656C704B6579776F7264010000
      0009040000040048696E7401000000090400000000000000000000080064746C
      734D61696E00000000000000000000000008006C48656164696E670300000007
      0043617074696F6E05000000190409003C68656164696E673E090409003C6865
      6164696E673E070409003C68656164696E673E160409003C68656164696E673E
      220409003C68656164696E673E0B0048656C704B6579776F7264050000001904
      000009040000070400001604000022040000040048696E740500000019040000
      090400000704000016040000220400000000000000000000080070427574746F
      6E7303000000070043617074696F6E0500000019040000090400000704000016
      040000220400000B0048656C704B6579776F7264050000001904000009040000
      070400001604000022040000040048696E740500000019040000090400000704
      0000160400002204000000000000000000000500704D61696E03000000070043
      617074696F6E0500000019040000090400000704000016040000220400000B00
      48656C704B6579776F7264050000001904000009040000070400001604000022
      040000040048696E740500000019040000090400000704000016040000220400
      000000000000000000}
  end
end
