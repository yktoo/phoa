object PhoaDialog: TPhoaDialog
  Left = 591
  Top = 336
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'PhoaDialog'
  ClientHeight = 169
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object bvBottom: TBevel
    Left = 0
    Top = 132
    Width = 290
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object pButtonsBottom: TPanel
    Left = 0
    Top = 134
    Width = 290
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      290
      35)
    object bCancel: TButton
      Left = 131
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = bCancelClick
    end
    object bOK: TButton
      Left = 51
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
    object bHelp: TButton
      Left = 209
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
