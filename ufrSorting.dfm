object frSorting: TfrSorting
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object lMain: TLabel
    Left = 0
    Top = 0
    Width = 320
    Height = 15
    Align = alTop
    AutoSize = False
    Caption = '&'#1055#1086#1088#1103#1076#1086#1082' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1081':'
  end
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 15
    Width = 320
    Height = 225
    Align = alClient
    DragMode = dmAutomatic
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
    Images = fMain.ilActionsSmall
    ParentBackground = False
    PopupMenu = pmMain
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
    OnChange = tvMainChange
    OnChecked = tvMainChecked
    OnDragAllowed = tvMainDragAllowed
    OnDragOver = tvMainDragOver
    OnDragDrop = tvMainDragDrop
    OnGetText = tvMainGetText
    OnGetImageIndex = tvMainGetImageIndex
    OnInitNode = tvMainInitNode
    OnKeyAction = tvMainKeyAction
    OnMouseDown = tvMainMouseDown
    Columns = <
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 166
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 150
      end>
    WideDefaultText = ''
  end
  object pmMain: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    OnPopup = pmMainPopup
    Left = 8
    Top = 40
    object ipmsmProp: TTBXSubmenuItem
      Caption = '&'#1057#1074#1086#1081#1089#1090#1074#1086' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
    end
    object ipmDelete: TTBXItem
      Caption = '&'#1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 7
      ShortCut = 46
      OnClick = ipmDeleteClick
    end
    object ipmSep: TTBXSeparatorItem
    end
    object ipmMoveUp: TTBXItem
      Caption = #1057#1076#1074#1080#1085#1091#1090#1100' &'#1074#1074#1077#1088#1093
      ImageIndex = 55
      ShortCut = 16422
      OnClick = ipmMoveUpClick
    end
    object ipmMoveDown: TTBXItem
      Caption = #1057#1076#1074#1080#1085#1091#1090#1100' '#1074'&'#1085#1080#1079
      ImageIndex = 56
      ShortCut = 16424
      OnClick = ipmMoveDownClick
    end
  end
end
