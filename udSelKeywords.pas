//**********************************************************************************************************************
//  $Id: udSelKeywords.pas,v 1.9 2004-10-18 19:27:03 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSelKeywords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj,
  phDlg, VirtualTrees, StdCtrls, ExtCtrls, DKLang;

type
  TdSelKeywords = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    lMain: TLabel;
    bReset: TButton;
    tvMain: TVirtualStringTree;
    procedure bResetClick(Sender: TObject);
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Проект
    FProject: IPhotoAlbumProject;
     // Список ключевых слов
    FKeywords: IPhotoAlbumKeywordList;
     // Строка с ключевыми словами
    FKeywordStr: String;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function SelectPhoaKeywords(AProject: IPhotoAlbumProject; var sKeywords: String): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main, phSettings;

  function SelectPhoaKeywords(AProject: IPhotoAlbumProject; var sKeywords: String): Boolean;
  begin
    with TdSelKeywords.Create(Application) do
      try
        FProject    := AProject;
        FKeywordStr := sKeywords;
        Result := Execute;
        if Result then sKeywords := FKeywordStr;
      finally
        Free;
      end;
  end;

  procedure TdSelKeywords.bResetClick(Sender: TObject);
  begin
    FKeywords.SelectedKeywords := '';
    tvMain.ReinitChildren(nil, False);
    tvMain.Invalidate;
    Modified := True;
  end;

  procedure TdSelKeywords.ButtonClick_OK;
  begin
    FKeywordStr := FKeywords.SelectedKeywords;
    inherited ButtonClick_OK;
  end;

  procedure TdSelKeywords.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_select_keywords;
     // Составляем список ключевых слов
    FKeywords := NewPhotoAlbumKeywordList;
    FKeywords.PopulateFromPicList(FProject.Pics, nil, 0);
    FKeywords.SelectedKeywords := FKeywordStr;
     // Настраиваем дерево
    ApplyTreeSettings(tvMain);
    tvMain.RootNodeCount := FKeywords.Count;
  end;

  procedure TdSelKeywords.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    with FKeywords.KWData[Node.Index]^ do
      case Node.CheckState of
        csUncheckedNormal: State := pksOff;
        csCheckedNormal:   State := pksOn;
      end;
    Modified := True;
  end;

  procedure TdSelKeywords.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := iiKeyword;
  end;

  procedure TdSelKeywords.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    case TextType of
      ttNormal: CellText := AnsiToUnicodeCP(FKeywords[Node.Index], cMainCodePage);
      ttStatic: CellText := AnsiToUnicodeCP(Format('(%d)', [FKeywords.KWData[Node.Index].iCount]), cMainCodePage);
    end;
  end;

  procedure TdSelKeywords.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    Node.CheckType := ctCheckBox;
    case FKeywords.KWData[Node.Index].State of
      pksOn: Node.CheckState := csCheckedNormal;
      else   Node.CheckState := csUncheckedNormal;
    end;
  end;

  procedure TdSelKeywords.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Количество вхождений ключевого слова красим серым
    if TextType=ttStatic then TargetCanvas.Font.Color := clGrayText;
  end;

end.

