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
  end
  object tvMain: TVirtualStringTree
    Left = 0
    Top = 15
    Width = 320
    Height = 225
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
    Header.ParentFont = True
    Images = fMain.ilActionsSmall
    PopupMenu = pmMain
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag]
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
        WideText = 'Picture property'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 150
        WideText = 'Sort direction'
      end>
    WideDefaultText = ''
  end
  object pmMain: TTBXPopupMenu
    Images = fMain.ilActionsSmall
    OnPopup = pmMainPopup
    Left = 8
    Top = 40
    object ipmsmProp: TTBXSubmenuItem
      Caption = '&Picture property'
    end
    object ipmDelete: TTBXItem
      Caption = '&Delete'
      ImageIndex = 7
      ShortCut = 46
      OnClick = ipmDeleteClick
    end
    object ipmSep: TTBXSeparatorItem
    end
    object ipmMoveUp: TTBXItem
      Caption = 'Move &up'
      ImageIndex = 55
      ShortCut = 16422
      OnClick = ipmMoveUpClick
    end
    object ipmMoveDown: TTBXItem
      Caption = 'Move do&wn'
      ImageIndex = 56
      ShortCut = 16424
      OnClick = ipmMoveDownClick
    end
  end
end
