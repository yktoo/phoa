inherited frPicProps_View: TfrPicProps_View
  Height = 298
  PopupMenu = pmMain
  OnContextPopup = FrameContextPopup
  OnMouseWheel = FrameMouseWheel
  object iMain: TImage32
    Left = 9
    Top = 26
    Width = 558
    Height = 263
    Align = alClient
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    TabOrder = 0
    TabStop = True
    OnMouseDown = iMainMouseDown
    OnMouseMove = iMainMouseMove
    OnMouseUp = iMainMouseUp
    OnResize = iMainResize
  end
  object dkTop: TTBXDock
    Left = 0
    Top = 0
    Width = 576
    Height = 26
    object tbMain: TTBXToolbar
      Left = 0
      Top = 0
      Align = alTop
      Caption = 'Main Toolbar'
      ChevronHint = 'More buttons|'
      DockMode = dmCannotFloat
      Images = fMain.ilActionsSmall
      SystemFont = False
      TabOrder = 0
      object cbViewFile: TTBXComboBoxItem
        EditWidth = 330
        Hint = 'File to view'
        ShowImage = True
        OnChange = cbViewFileChange
        DropDownList = True
        MinListWidth = 330
        ShowListImages = True
        OnAdjustImageIndex = cbViewFileAdjustImageIndex
      end
      object bViewZoomIn: TTBXItem
        Action = aZoomIn
      end
      object bViewZoomOut: TTBXItem
        Action = aZoomOut
      end
      object bViewZoomActual: TTBXItem
        Action = aZoomActual
      end
      object bViewZoomFit: TTBXItem
        Action = aZoomFit
      end
    end
    object tbTools: TTBXToolbar
      Left = 428
      Top = 0
      Caption = 'Tools'
      ChevronHint = 'More buttons|'
      DockMode = dmCannotFloat
      DockPos = 428
      Images = fMain.ilActionsSmall
      TabOrder = 1
      object bRotate0: TTBXItem
        Action = aRotate0
      end
      object bRotate90: TTBXItem
        Action = aRotate90
      end
      object bRotate180: TTBXItem
        Action = aRotate180
      end
      object bRotate270: TTBXItem
        Action = aRotate270
      end
      object tbSepFlipHorz: TTBXSeparatorItem
      end
      object bFlipHorz: TTBXItem
        Action = aFlipHorz
      end
      object bFlipVert: TTBXItem
        Action = aFlipVert
      end
    end
  end
  object dkLeft: TTBXDock
    Left = 0
    Top = 26
    Width = 9
    Height = 263
    Position = dpLeft
  end
  object dkRight: TTBXDock
    Left = 567
    Top = 26
    Width = 9
    Height = 263
    Position = dpRight
  end
  object dkBottom: TTBXDock
    Left = 0
    Top = 289
    Width = 576
    Height = 9
    Position = dpBottom
  end
  object alMain: TActionList
    Images = fMain.ilActionsSmall
    Left = 36
    Top = 252
    object aZoomIn: TAction
      Category = 'Zoom'
      Caption = 'Zoom &in'
      Hint = 'Zoom in|Enlarge the image'
      ImageIndex = 25
      OnExecute = aaZoomIn
    end
    object aZoomOut: TAction
      Category = 'Zoom'
      Caption = 'Zoom ou&t'
      Hint = 'Zoom out|Zoom image out'
      ImageIndex = 26
      OnExecute = aaZoomOut
    end
    object aZoomActual: TAction
      Category = 'Zoom'
      Caption = 'Zoom &actual'
      Hint = 'Set zoom to 1:1'
      ImageIndex = 28
      OnExecute = aaZoomActual
    end
    object aZoomFit: TAction
      Category = 'Zoom'
      Caption = '&Fit window'
      Hint = 'Set zoom to fit window'
      ImageIndex = 27
      OnExecute = aaZoomFit
    end
    object aRotate0: TAction
      Category = 'Tools'
      Caption = '&No rotation'
      Hint = 'No rotation|Don'#39't apply rotation to the image'
      ImageIndex = 66
      OnExecute = aaRotate0
    end
    object aRotate90: TAction
      Category = 'Tools'
      Caption = 'Rotate CW by &90'#176
      Hint = 'Rotate CW by 90'#176'|Rotate the image clockwise by 90'#176
      ImageIndex = 67
      OnExecute = aaRotate90
    end
    object aRotate180: TAction
      Category = 'Tools'
      Caption = 'Rotate by &180'#176
      Hint = 'Rotate by 180'#176'|Rotate the image by 180'#176
      ImageIndex = 68
      OnExecute = aaRotate180
    end
    object aRotate270: TAction
      Category = 'Tools'
      Caption = 'Rotate CCW by 9&0'#176
      Hint = 
        'Rotate CCW by 90'#176'|Rotate the image counter-clockwise by 90'#176' (or ' +
        'clockwise by 270'#176')'
      ImageIndex = 69
      OnExecute = aaRotate270
    end
    object aFlipHorz: TAction
      Category = 'Tools'
      Caption = 'Flip &horizontally'
      Hint = 'Flip horizontally|Flip the image horizontally'
      ImageIndex = 70
      OnExecute = aaFlipHorz
    end
    object aFlipVert: TAction
      Category = 'Tools'
      Caption = 'Flip &vertically'
      Hint = 'Flip vertically|Flip the image vertically'
      ImageIndex = 71
      OnExecute = aaFlipVert
    end
  end
  object pmMain: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    Left = 64
    Top = 252
    object gipmMainToolbar: TTBGroupItem
    end
    object ipmSep: TTBXSeparatorItem
    end
    object gipmToolsToolbar: TTBGroupItem
    end
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts')
    Left = 8
    Top = 252
    LangData = {
      0F00667250696350726F70735F566965770001220000000500646B546F700000
      060074624D61696E010200000008000000070043617074696F6E6C0000000B00
      43686576726F6E48696E74000800646B426F74746F6D00000600646B4C656674
      00000700646B526967687400000600616C4D61696E00000500694D61696E0000
      0A0063625669657746696C6501010000003E000000040048696E74000B006256
      6965775A6F6F6D496E00000C0062566965775A6F6F6D4F757400000F00625669
      65775A6F6F6D41637475616C00000C0062566965775A6F6F6D46697400000700
      7462546F6F6C73010200000042000000070043617074696F6E6D0000000B0043
      686576726F6E48696E7400080062526F74617465300000090062526F74617465
      393000000A0062526F7461746531383000000A0062526F746174653237300000
      0D007462536570466C6970486F727A0000090062466C6970486F727A00000900
      62466C69705665727400000700615A6F6F6D496E010300000045000000070043
      617074696F6E44000000080043617465676F727947000000040048696E740008
      00615A6F6F6D4F7574010300000049000000070043617074696F6E4800000008
      0043617465676F72794B000000040048696E74000B00615A6F6F6D4163747561
      6C01030000004D000000070043617074696F6E4C000000080043617465676F72
      794F000000040048696E74000800615A6F6F6D46697401030000005100000007
      0043617074696F6E50000000080043617465676F727953000000040048696E74
      00080061526F7461746530010300000055000000070043617074696F6E540000
      00080043617465676F727957000000040048696E7400090061526F7461746539
      30010300000059000000070043617074696F6E58000000080043617465676F72
      795B000000040048696E74000A0061526F7461746531383001030000005D0000
      00070043617074696F6E5C000000080043617465676F72795F00000004004869
      6E74000A0061526F74617465323730010300000061000000070043617074696F
      6E60000000080043617465676F727963000000040048696E7400090061466C69
      70486F727A010300000065000000070043617074696F6E640000000800436174
      65676F727967000000040048696E7400090061466C6970566572740103000000
      69000000070043617074696F6E68000000080043617465676F72796B00000004
      0048696E74000600706D4D61696E00000F006769706D4D61696E546F6F6C6261
      720000060069706D536570000010006769706D546F6F6C73546F6F6C62617200
      00}
  end
end
