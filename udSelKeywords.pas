//**********************************************************************************************************************
//  $Id: udSelKeywords.pas,v 1.7 2004-09-11 17:52:36 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSelKeywords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, PhObj,
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
    FPhoA: TPhotoAlbum;
     // Список ключевых слов
    FKeywords: TKeywordList;
    FKeywordStr: String;
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function SelectPhoaKeywords(PhoA: TPhotoAlbum; var sKeywords: String): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main, phSettings;

  function SelectPhoaKeywords(PhoA: TPhotoAlbum; var sKeywords: String): Boolean;
  begin
    with TdSelKeywords.Create(Application) do
      try
        FPhoA       := PhoA;
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

  procedure TdSelKeywords.FinalizeDialog;
  begin
    FKeywords.Free;
    inherited FinalizeDialog;
  end;

  procedure TdSelKeywords.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_select_keywords;
     // Составляем список ключевых слов
    FKeywords := TKeywordList.Create;
    FKeywords.PopulateFromPhoA(FPhoA, nil, 0);
    FKeywords.SelectedKeywords := FKeywordStr;
     // Настраиваем дерево
    ApplyTreeSettings(tvMain);
    tvMain.RootNodeCount := FKeywords.Count;
  end;

  procedure TdSelKeywords.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    with FKeywords[Node.Index]^ do
      case Node.CheckState of
        csUncheckedNormal: State := ksOff;
        csCheckedNormal:   State := ksOn;
      end;
    Modified := True;
  end;

  procedure TdSelKeywords.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := iiKeyword;
  end;

  procedure TdSelKeywords.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    with FKeywords[Node.Index] ^ do
      if TextType=ttNormal then CellText := AnsiToUnicodeCP(sKeyword, cMainCodePage) else CellText := AnsiToUnicodeCP(Format('(%d)', [iCount]), cMainCodePage);
  end;

  procedure TdSelKeywords.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    Node.CheckType := ctCheckBox;
    case FKeywords[Node.Index].State of
      ksOn: Node.CheckState := csCheckedNormal;
      else  Node.CheckState := csUncheckedNormal;
    end;
  end;

  procedure TdSelKeywords.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Количество вхождений ключевого слова красим серым
    if TextType=ttStatic then TargetCanvas.Font.Color := clGrayText;
  end;

end.

