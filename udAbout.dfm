object dAbout: TdAbout
  Left = 476
  Top = 274
  HelpType = htKeyword
  BorderStyle = bsNone
  Caption = 'About'
  ClientHeight = 191
  ClientWidth = 363
  Color = clWhite
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object iMain: TImage32
    Left = 0
    Top = 0
    Width = 363
    Height = 191
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnMouseDown = iMainMouseDown
    OnMouseMove = iMainMouseMove
    OnMouseUp = iMainMouseUp
    object lWebsite: TLabel
      Left = 84
      Top = 28
      Width = 193
      Height = 11
      Cursor = crHandPoint
      HelpType = htKeyword
      AutoSize = False
      Transparent = True
      OnClick = lWebsiteClick
    end
    object lOK: TLabel
      Left = 308
      Top = 132
      Width = 29
      Height = 17
      Cursor = crHandPoint
      HelpType = htKeyword
      Alignment = taCenter
      AutoSize = False
      Caption = 'OK'
      Color = 15790320
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
      OnClick = lOKClick
    end
  end
  object TheTimer: TTimer
    Interval = 5000
    OnTimer = TheTimerTick
    Left = 32
    Top = 160
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      'Font.Name'
      'SecondaryShortCuts')
    Left = 60
    Top = 160
    LangData = {
      06006441626F7574010100000003000000070043617074696F6E010400000005
      00694D61696E000008006C57656273697465000003006C4F4B01010000000C00
      0000070043617074696F6E00080054686554696D65720000}
  end
end
