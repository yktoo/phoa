inherited fAddFilesWizard: TfAddFilesWizard
  Left = 364
  Caption = 'Add pictures wizard'
  ClientWidth = 632
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Width = 632
  end
  inherited bvTopPanel: TBevel
    Width = 632
  end
  inherited pMain: TPanel
    Width = 632
    object dpPreview: TTBXDockablePanel
      Left = 40
      Top = 4
      Caption = 'Preview'
      FloatingWidth = 128
      FloatingHeight = 128
      SupportedDocks = []
      TabOrder = 0
      Visible = False
      OnResize = dpPreviewResize
      OnVisibleChanged = dpPreviewVisibleChanged
      object iPreview: TImage32
        Left = 0
        Top = 0
        Width = 128
        Height = 128
        Align = alClient
        Bitmap.OuterColor = -1
        Bitmap.StretchFilter = sfDraft
        BitmapAlign = baCenter
        Scale = 1.000000000000000000
        ScaleMode = smScale
        TabOrder = 0
      end
    end
  end
  inherited pButtons: TPanel
    Width = 632
    DesignSize = (
      632
      36)
    inherited bCancel: TButton
      Left = 548
    end
    inherited bNext: TButton
      Left = 408
    end
    inherited bBack: TButton
      Left = 328
    end
  end
  inherited pHeader: TPanel
    Width = 632
    inherited lHeading: TLabel
      Width = 571
    end
    inherited iIcon: TImage
      Left = 579
    end
  end
  object pProcess: TPanel
    Left = 39
    Top = 172
    Width = 553
    Height = 57
    Anchors = []
    ParentBackground = False
    TabOrder = 3
    Visible = False
  end
  object dklcMain: TDKLanguageController
    IgnoreList.Strings = (
      '*.Font.Name'
      '*.SecondaryShortCuts'
      'lHeading.Caption')
    Left = 92
    Top = 368
    LangData = {
      0F006641646446696C657357697A617264010100000003000000070043617074
      696F6E010E00000008006276426F74746F6D00000A006276546F7050616E656C
      00000500704D61696E0000080070427574746F6E73000007006243616E63656C
      010100000011000000070043617074696F6E000500624E657874010100000014
      000000070043617074696F6E0005006248656C70010100000017000000070043
      617074696F6E000500624261636B01010000001A000000070043617074696F6E
      00070070486561646572000008006C48656164696E67000005006949636F6E00
      0008007050726F636573730000090064705072657669657701010000001B0000
      00070043617074696F6E00080069507265766965770000}
  end
end
