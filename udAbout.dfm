object dAbout: TdAbout
  Left = 486
  Top = 301
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
  object dtlsMain: TDTLanguageSwitcher
    Language = 1033
    Left = 4
    Top = 160
    LangData = {
      06006441626F757404000000070043617074696F6E0500000019040B00CE20EF
      F0EEE3F0E0ECECE50904050041626F757407040400DC62657216040C00536F62
      7265206F2050686F4122040C00CFF0EE20EFF0EEE3F0E0ECF3080048656C7046
      696C650500000019040000090400000704000016040000220400000B0048656C
      704B6579776F7264050000001904000009040000070400001604000022040000
      040048696E740500000019040000090400000704000016040000220400000000
      000005000000080064746C734D61696E0000000000000000000000000500694D
      61696E020000000B0048656C704B6579776F7264050000001904000009040000
      070400001604000022040000040048696E740500000019040000090400000704
      00001604000022040000000000000000000003006C4F4B030000000700436170
      74696F6E05000000190402004F4B090402004F4B070402004F4B160402004F4B
      220402004F4B0B0048656C704B6579776F726405000000190400000904000007
      0400001604000022040000040048696E74050000001904000009040000070400
      001604000022040000000000000000000008006C576562736974650300000007
      0043617074696F6E050000001904000009040000070400001604000022040000
      0B0048656C704B6579776F726405000000190400000904000007040000160400
      0022040000040048696E74050000001904000009040000070400001604000022
      0400000000000000000000080054686554696D65720000000000000000000000
      00}
  end
  object TheTimer: TTimer
    Interval = 5000
    OnTimer = TheTimerTick
    Left = 32
    Top = 160
  end
end
