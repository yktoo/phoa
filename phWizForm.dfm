object PhoaWizardForm: TPhoaWizardForm
  Left = 368
  Top = 257
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
  Position = poMainFormCenter
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
end
