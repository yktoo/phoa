inherited dPicOps: TdPicOps
  Top = 246
  ActiveControl = cbOp
  Caption = 'Picture operations'
  ClientHeight = 433
  ClientWidth = 399
  OldCreateOrder = True
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 396
    Width = 399
  end
  object lGroup: TLabel [1]
    Left = 12
    Top = 52
    Width = 33
    Height = 13
    Caption = '&Group:'
    FocusControl = tvGroups
  end
  object lOp: TLabel [2]
    Left = 12
    Top = 12
    Width = 52
    Height = 13
    Caption = '&Operation:'
    FocusControl = cbOp
  end
  inherited pButtonsBottom: TPanel
    Top = 398
    Width = 399
    DesignSize = (
      399
      35)
    inherited bCancel: TButton
      Left = 239
    end
    inherited bOK: TButton
      Left = 159
    end
    inherited bHelp: TButton
      Left = 317
    end
  end
  object cbOp: TComboBox [4]
    Left = 12
    Top = 28
    Width = 375
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    ItemHeight = 13
    TabOrder = 1
    OnChange = DlgDataChange
    Items.Strings = (
      'Move selected pictures to the group specified below'
      'Copy selected pictures to the group specified below'
      'Delete selected pictures from the group specified below'
      
        'Leave only selected pictures to the group specified below (inter' +
        'sect)')
  end
  object tvGroups: TVirtualStringTree [5]
    Left = 12
    Top = 68
    Width = 375
    Height = 315
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultNodeHeight = 16
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = fMain.ilActionsSmall
    ParentBackground = False
    TabOrder = 2
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.StringOptions = [toShowStaticText, toAutoAcceptEditChange]
    OnBeforeItemErase = tvGroupsBeforeItemErase
    OnChange = tvGroupsChange
    OnGetText = tvGroupsGetText
    OnPaintText = tvGroupsPaintText
    OnGetImageIndex = tvGroupsGetImageIndex
    OnGetHint = tvGroupsGetHint
    OnInitNode = tvGroupsInitNode
    Columns = <>
    WideDefaultText = 'Photo Album'
  end
  inherited dtlsMain: TDTLanguageSwitcher
    Left = 20
    Top = 76
    LangData = {
      0700645069634F707304000000070043617074696F6E0500000019041800CEEF
      E5F0E0F6E8E820F120E8E7EEE1F0E0E6E5EDE8FFECE809041200506963747572
      65206F7065726174696F6E7307040F0042696C646F7065726174696F6E656E16
      0415004F70657261E7F5657320636F6D20696D6167656E7322041800CEEFE5F0
      E0F6B3B320B3E720E7EEE1F0E0E6E5EDEDFFECE8080048656C7046696C650500
      000019040000090400000704000016040000220400000B0048656C704B657977
      6F7264050000001904000009040000070400001604000022040000040048696E
      7405000000190400000904000007040000160400002204000000000000090000
      0007006243616E63656C03000000070043617074696F6E0500000019040600CE
      F2ECE5EDE00904060043616E63656C0704090041626272656368656E16040800
      43616E63656C617222040700C2B3E4ECB3EDE00B0048656C704B6579776F7264
      050000001904000009040000070400001604000022040000040048696E740500
      0000190400000904000007040000160400002204000000000000000000000500
      6248656C7003000000070043617074696F6E0500000019040700D1EFF0E0E2EA
      E00904040048656C700704050048696C666516040500416A75646122040700C4
      EEE2B3E4EAE00B0048656C704B6579776F726405000000190400000904000007
      0400001604000022040000040048696E74050000001904000009040000070400
      00160400002204000000000000000000000300624F4B03000000070043617074
      696F6E0500000019040200CECA090402004F4B070402004F4B160402004F4B22
      040200CECA0B0048656C704B6579776F72640500000019040000090400000704
      00001604000022040000040048696E7405000000190400000904000007040000
      16040000220400000000000000000000040063624F70050000000B0048656C70
      4B6579776F726405000000190400000904000007040000160400002204000004
      0048696E74050000001904000009040000070400001604000022040000070049
      6D654E616D650500000019040000090400000704000016040000220400000500
      4974656D73050000001904DC00CFE5F0E5ECE5F1F2E8F2FC20E2FBE4E5EBE5ED
      EDFBE520E8E7EEE1F0E0E6E5EDE8FF20E220F3EAE0E7E0EDEDF3FE20E3F0F3EF
      EFF30D0ACAEEEFE8F0EEE2E0F2FC20E2FBE4E5EBE5EDEDFBE520E8E7EEE1F0E0
      E6E5EDE8FF20E220F3EAE0E7E0EDEDF3FE20E3F0F3EFEFF30D0AD3E4E0EBE8F2
      FC20E2FBE4E5EBE5EDEDFBE520E8E7EEE1F0E0E6E5EDE8FF20E8E720F3EAE0E7
      E0EDEDEEE920E3F0F3EFEFFB0D0ACEF1F2E0E2E8F2FC20F2EEEBFCEAEE20E2FB
      E4E5EBE5EDEDFBE520E8E7EEE1F0E0E6E5EDE8FF20E220F3EAE0E7E0EDEDEEE9
      20E3F0F3EFEFE50D0A0904EA004D6F76652073656C6563746564207069637475
      72657320746F207468652067726F7570207370656369666965642062656C6F77
      0D0A436F70792073656C656374656420706963747572657320746F2074686520
      67726F7570207370656369666965642062656C6F770D0A44656C657465207365
      6C65637465642070696374757265732066726F6D207468652067726F75702073
      70656369666965642062656C6F770D0A4C65617665206F6E6C792073656C6563
      74656420706963747572657320746F207468652067726F757020737065636966
      6965642062656C6F772028696E74657273656374290D0A070403014175736765
      77E4686C74652042696C64657220696E2064696520756E74656E20616E676567
      6562656E652047727570706520766572736368696562656E0D0A417573676577
      E4686C74652042696C64657220696E2064696520756E74656E20616E67656765
      62656E6520477275707065206B6F70696572656E0D0A417573676577E4686C74
      652042696C646572206175732064657220756E74656E20616E6765676562656E
      656E20477275707065206CF6736368656E0D0A4E757220617573676577E4686C
      74652042696C64657220696E2064657220756E74656E20616E6765676562656E
      656E20477275707065206C617373656E2028696E74657273656374290D0A1604
      E0004D6F76657220617320696D6167656E732073656C6563696F6E6164617320
      70617261206F20677275706F20696E64696361646F0D0A436F70696172206173
      20696D6167656E732073656C6563696F6E616461732070617261206F20677275
      706F20696E64696361646F0D0A4578636C75697220617320696D6167656E7320
      73656C6563696F6E6164617320646F20677275706F20696E64696361646F0D0A
      44656978617220736F6D656E746520617320696D6167656E732073656C656369
      6F6E61646173206E6F20677275706F20696E64696361646F2028636F72746529
      0D0A2204CD00CFE5F0E5ECB3F1F2E8F2E820E2E8E4B3EBE5EDB320E7EEE1F0E0
      E6E5EDEDFF20E220E7E0E7EDE0F7E5EDF320E3F0F3EFF30D0ACAEEEFB3FEE2E0
      F2E820E2E8E4B3EBE5EDB320E7EEE1F0E0E6E5EDEDFF20E220E7E0E7EDE0F7E5
      EDF320E3F0F3EFF30D0AC2E8E4E0EBE8F2E820E2B3E4B3EBE5EDB320E7EEE1F0
      E0E6E5EDEDFF20E720E7E0E7E0EDF7E5EDEEBF20E3F0F3EFE80D0AC7E0EBE8F8
      E8F2E820F2B3EBFCEAE820E2E8E4B3EBE5EDB320E7EEE1F0E0E6E5EDEDFF20E2
      20E7E0E7EDE0F7E5EDB3E920E3F0F3EFB30D0A04005465787405000000190400
      00090400000704000016040000220400000000000000000000080064746C734D
      61696E00000000000000000000000006006C47726F7570030000000700436170
      74696F6E050000001904080026C3F0F3EFEFE03A090407002647726F75703A07
      040800264772757070653A1604070026477275706F3A2204070026C3F0F3EFE0
      3A0B0048656C704B6579776F7264050000001904000009040000070400001604
      000022040000040048696E740500000019040000090400000704000016040000
      22040000000000000000000003006C4F7003000000070043617074696F6E0500
      000019040A0026CEEFE5F0E0F6E8FF3A09040B00264F7065726174696F6E3A07
      040B00264F7065726174696F6E3A16040A00264F70657261E7E36F3A22040900
      26CEEFE5F0E0F6B3FF0B0048656C704B6579776F726405000000190400000904
      0000070400001604000022040000040048696E74050000001904000009040000
      07040000160400002204000000000000000000000500704D61696E0300000007
      0043617074696F6E050000001904000009040000070400001604000022040000
      0B0048656C704B6579776F726405000000190400000904000007040000160400
      0022040000040048696E74050000001904000009040000070400001604000022
      04000000000000000000000800747647726F757073020000000B0048656C704B
      6579776F72640500000019040000090400000704000016040000220400000400
      48696E7405000000190400000904000007040000160400002204000000000000
      00000000}
  end
end
