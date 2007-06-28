inherited frWzPage_Processing: TfrWzPage_Processing
  object gbMain: TTntGroupBox
    Left = 0
    Top = 0
    Width = 576
    Height = 284
    Anchors = []
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      576
      284)
    object lInfo: TTntLabel
      Left = 26
      Top = 167
      Width = 526
      Height = 53
      Alignment = taCenter
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Layout = tlBottom
    end
    object iThumb: TImage32
      Left = 28
      Top = 20
      Width = 524
      Height = 145
      Anchors = [akLeft, akTop, akRight, akBottom]
      BitmapAlign = baCenter
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 2
    end
    object pbMain: TTntProgressBar
      Left = 24
      Top = 227
      Width = 530
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
    end
    object bInterrupt: TTntButton
      Left = 232
      Top = 249
      Width = 111
      Height = 23
      Anchors = [akBottom]
      Enabled = False
      TabOrder = 1
      OnClick = bInterruptClick
    end
  end
end
