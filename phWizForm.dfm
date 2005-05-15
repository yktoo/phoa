inherited PhoaWizardForm: TPhoaWizardForm
  BorderIcons = [biSystemMenu, biMaximize]
  PixelsPerInch = 96
  TextHeight = 13
  object bvBottom: TBevel
    Left = 0
    Top = 397
    Width = 632
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object bvTopPanel: TBevel
    Left = 0
    Top = 61
    Width = 632
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object pMain: TPanel
    Left = 0
    Top = 63
    Width = 632
    Height = 334
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 0
  end
  object pButtons: TPanel
    Left = 0
    Top = 399
    Width = 632
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      632
      36)
    object bCancel: TButton
      Left = 548
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
      Left = 408
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
      Left = 328
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
    Width = 632
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
      Width = 571
      Height = 45
      Align = alClient
      AutoSize = False
      Caption = '<heading>'
      Transparent = False
      Layout = tlCenter
      WordWrap = True
    end
    object iIcon: TImage
      Left = 579
      Top = 8
      Width = 45
      Height = 45
      Align = alRight
      Center = True
    end
  end
end
