inherited PhoaDialog: TPhoaDialog
  KeyPreview = True
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object bvBottom: TBevel
    Left = 0
    Top = 398
    Width = 632
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object pButtonsBottom: TTntPanel
    Left = 0
    Top = 400
    Width = 632
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      632
      35)
    object bCancel: TTntButton
      Left = 473
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = bCancelClick
    end
    object bOK: TTntButton
      Left = 393
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = bOKClick
    end
    object bHelp: TTntButton
      Left = 551
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Help'
      TabOrder = 2
      OnClick = bHelpClick
    end
  end
end
