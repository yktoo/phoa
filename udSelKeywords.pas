//**********************************************************************************************************************
//  $Id: udSelKeywords.pas,v 1.14 2007-06-28 18:41:40 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSelKeywords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj,
  phDlg, VirtualTrees, StdCtrls, ExtCtrls, DKLang, TntStdCtrls, TntExtCtrls;

type
  TdSelKeywords = class(TPhoaDialog)
    bReset: TTntButton;
    dklcMain: TDKLanguageController;
    lMain: TTntLabel;
    tvMain: TVirtualStringTree;
    procedure bResetClick(Sender: TObject);
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Проект
    FProject: IPhotoAlbumProject;
     // Список ключевых слов
    FKeywords: IPhotoAlbumKeywordList;
     // Строка с ключевыми словами
    FKeywordStr: WideString;
  protected
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
  end;

  function SelectPhoaKeywords(AProject: IPhotoAlbumProject; var wsKeywords: WideString): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main, phSettings;

  function SelectPhoaKeywords(AProject: IPhotoAlbumProject; var wsKeywords: WideString): Boolean;
  begin
    with TdSelKeywords.Create(Application) do
      try
        FProject    := AProject;
        FKeywordStr := wsKeywords;
        Result := ExecuteModal(False, False);
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

  procedure TdSelKeywords.DoCreate;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_select_keywords;
    FKeywords := NewPhotoAlbumKeywordList;
    ApplyTreeSettings(tvMain);
  end;

  procedure TdSelKeywords.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
     // Составляем список ключевых слов
    FKeywords.PopulateFromPicList(FProject.Pics, nil, 0);
    FKeywords.SelectedKeywords := FKeywordStr;
     // Настраиваем дерево
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
      ttNormal: CellText := PhoaAnsiToUnicode(FKeywords[Node.Index]);
      ttStatic: CellText := PhoaAnsiToUnicode(Format('(%d)', [FKeywords.KWData[Node.Index].iCount]));
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

