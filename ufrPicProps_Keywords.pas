//**********************************************************************************************************************
//  $Id: ufrPicProps_Keywords.pas,v 1.14 2004-10-19 15:03:31 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrPicProps_Keywords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phWizard, Menus, TB2Item, TBX, ActnList, TB2Dock,
  TB2Toolbar, VirtualTrees, phPicPropsDlgPage, DKLang;

type
  TfrPicProps_Keywords = class(TPicPropsDialogPage)
    alMain: TActionList;
    aAdd: TAction;
    aEdit: TAction;
    tvMain: TVirtualStringTree;
    tbMain: TTBXToolbar;
    bAdd: TTBXItem;
    bEdit: TTBXItem;
    pmMain: TTBXPopupMenu;
    aCheckAll: TAction;
    aUncheckAll: TAction;
    aInvertCheck: TAction;
    bInvertCheck: TTBXItem;
    bUncheckAll: TTBXItem;
    bCheckAll: TTBXItem;
    tbSep1: TTBXSeparatorItem;
    aCheckedOnly: TAction;
    bCheckedOnly: TTBXItem;
    dklcMain: TDKLanguageController;
    procedure aaAdd(Sender: TObject);
    procedure aaCheckAll(Sender: TObject);
    procedure aaCheckedOnly(Sender: TObject);
    procedure aaEdit(Sender: TObject);
    procedure aaInvertCheck(Sender: TObject);
    procedure aaUncheckAll(Sender: TObject);
    procedure tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure tvMainEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvMainEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Флаг того, что страница проинициализирована
    FInitialized: Boolean;
     // Список ключевых слов
    FKeywords: IPhotoAlbumKeywordList;
     // Индекс узла, который надо сфокусировать после окончания редактирования
    FNodeToFocusIndex: Integer;
     // Callback-процедура для определения выбранности изображения
    procedure IsPicSelectedCallback(Pic: IPhoaPic; out bSelected: Boolean);
     // Настраивает доступность Actions
    procedure EnableActions;
     // Ставит, снимает или инвертирует отметку у всех слов
    procedure CheckKeywords(Mode: TMassCheckMode);
     // Выделяет, фокусирует и делает видимым узел с заданным индексом
    procedure FocusNode(Index: Integer);
  protected
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure AfterDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure InitializePage; override;
  public
    procedure Apply(var sOpParamName: String; var OpParams: IPhoaOperationParams); override;
  end;

implementation
{$R *.dfm}
uses phUtils, Main, phSettings;

  procedure TfrPicProps_Keywords.aaAdd(Sender: TObject);
  var idx: Integer;
  begin
     // Вставляем слово
    idx := FKeywords.InsertNew;
    tvMain.RootNodeCount := FKeywords.Count;
    tvMain.ReinitChildren(nil, False);
     // Выделяем узел
    FocusNode(idx);
     // Вводим в режим редактирования слова
    tvMain.EditNode(tvMain.FocusedNode, -1);
    Modified;
  end;

  procedure TfrPicProps_Keywords.aaCheckAll(Sender: TObject);
  begin
    CheckKeywords(mcmAll);
  end;

  procedure TfrPicProps_Keywords.aaCheckedOnly(Sender: TObject);
  var
    n: PVirtualNode;
    bShow: Boolean;
  begin
    bShow := not aCheckedOnly.Checked;
    with tvMain do begin
      BeginUpdate;
      try
        n := GetFirst;
        while n<>nil do begin
          IsVisible[n] := bShow or (n.CheckState<>csUncheckedNormal);
          n := GetNext(n);
        end;
      finally
      	EndUpdate;
      end;
    end;
  end;

  procedure TfrPicProps_Keywords.aaEdit(Sender: TObject);
  begin
     // Вводим в режим редактирования слова
    tvMain.EditNode(tvMain.FocusedNode, -1);
  end;

  procedure TfrPicProps_Keywords.aaInvertCheck(Sender: TObject);
  begin
    CheckKeywords(mcmInvert);
  end;

  procedure TfrPicProps_Keywords.aaUncheckAll(Sender: TObject);
  begin
    CheckKeywords(mcmNone);
  end;

  procedure TfrPicProps_Keywords.AfterDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited AfterDisplay(ChangeMethod);
    StorageForm.ActiveControl := tvMain;
  end;

  procedure TfrPicProps_Keywords.Apply(var sOpParamName: String; var OpParams: IPhoaOperationParams);
  begin
     // Если страница инициализирована, возвращаем параметры для операции изменения списка ключевых слов
    if FInitialized then begin
      sOpParamName := 'EditKeywordsOpParams';
      OpParams     := NewPhoaOperationParams(['Pics', EditedPics, 'KeywordList', FKeywords]);
    end;
  end;

  procedure TfrPicProps_Keywords.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    if not FInitialized then begin
       // Составляем список ключевых слов
      FKeywords.PopulateFromPicList(App.Project.Pics, IsPicSelectedCallback, EditedPics.Count);
       // Настраиваем дерево
      tvMain.RootNodeCount := FKeywords.Count;
      FInitialized := True;
    end;
    EnableActions;
  end;

  procedure TfrPicProps_Keywords.CheckKeywords(Mode: TMassCheckMode);
  var i: Integer;
  begin
    for i := 0 to FKeywords.Count-1 do
      with FKeywords.KWData[i]^ do
        case Mode of
          mcmAll:  State := pksOn;
          mcmNone: State := pksOff;
          else     if State=pksOff then State := pksOn else State := pksOff;
        end;
    tvMain.ReinitChildren(nil, False);
    tvMain.Invalidate;
    Modified;
  end;

  procedure TfrPicProps_Keywords.EnableActions;
  begin
    aEdit.Enabled := tvMain.FocusedNode<>nil;
  end;

  procedure TfrPicProps_Keywords.FocusNode(Index: Integer);
  var n: PVirtualNode;
  begin
    n := tvMain.GetFirst;
    while (n<>nil) and (Index>0) do begin
      n := tvMain.GetNext(n);
      Dec(Index);
    end;
     // Выделяем
    if n<>nil then
      with tvMain do begin
        ScrollIntoView(n, False);
        Selected[n] := True;
        FocusedNode := n;
      end;
  end;

  procedure TfrPicProps_Keywords.InitializePage;
  begin
    inherited InitializePage;
    ApplyTreeSettings(tvMain);
    pmMain.LinkSubitems := tbMain.Items;
    FKeywords := NewPhotoAlbumKeywordList;
  end;

  procedure TfrPicProps_Keywords.IsPicSelectedCallback(Pic: IPhoaPic; out bSelected: Boolean);
  begin
    bSelected := EditedPics.IndexOfID(Pic.ID)>=0;
  end;

  procedure TfrPicProps_Keywords.tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    EnableActions;
  end;

  procedure TfrPicProps_Keywords.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    with FKeywords.KWData[Node.Index]^ do
      case Node.CheckState of
        csUncheckedNormal: begin
          State := pksOff;
           // Скрываем узел, если стоит показ только отмеченных
          if aCheckedOnly.Checked then Sender.IsVisible[Node] := False;
        end;
        csMixedNormal:   State := pksGrayed;
        csCheckedNormal: State := pksOn;
      end;
    Modified;
  end;

  procedure TfrPicProps_Keywords.tvMainChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
  begin
     // Выставляем тип птицы: если изначально было Grayed - после Unchecked идёт Grayed
    if (Node.CheckState=csUncheckedNormal) and (NewState=csCheckedNormal) then
      with FKeywords.KWData[Node.Index]^ do
        if (iSelCount>0) and (iSelCount<EditedPics.Count) then NewState := csMixedNormal;
  end;

  procedure TfrPicProps_Keywords.tvMainEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
    if FNodeToFocusIndex>=0 then FocusNode(FNodeToFocusIndex);
  end;

  procedure TfrPicProps_Keywords.tvMainEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
  begin
    FNodeToFocusIndex := -1;
  end;

  procedure TfrPicProps_Keywords.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    case FKeywords.KWData[Node.Index].Change of
      pkcNone:    ImageIndex := iiKeyword;
      pkcAdd:     ImageIndex := iiAsterisk;
      pkcReplace: ImageIndex := iiEdit;
    end;
  end;

  procedure TfrPicProps_Keywords.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var p: PPhoaKeywordData;
  begin
    case TextType of
       // Текст ключевого слова
      ttNormal: CellText := AnsiToUnicodeCP(FKeywords[Node.Index], cMainCodePage);
       // Количество вхождений слова
      ttStatic: begin
        p := FKeywords.KWData[Node.Index];
        if (p.Change=pkcNone) and (p.iCount>0) then CellText := Format(iif(p.iSelCount>0, '(%d/%d)', '(%1:d)'), [p.iSelCount, p.iCount]);
      end
    end;
  end;

  procedure TfrPicProps_Keywords.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    Node.CheckType := ctCheckBox;
    case FKeywords.KWData[Node.Index].State of
      pksOff:    Node.CheckState := csUncheckedNormal;
      pksGrayed: Node.CheckState := csMixedNormal;
      pksOn:     Node.CheckState := csCheckedNormal;
    end;
     // Настраиваем видимость узла
    Sender.IsVisible[Node] := not aCheckedOnly.Checked or (Node.CheckState<>csUncheckedNormal);
  end;

  procedure TfrPicProps_Keywords.tvMainNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
  begin
    FNodeToFocusIndex := FKeywords.Rename(Node.Index, UnicodeToAnsiCP(NewText, cMainCodePage));
    tvMain.ReinitChildren(nil, False);
    Modified;
  end;

  procedure TfrPicProps_Keywords.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    case TextType of
       // Вновь добавленные ключевые слова красим в синий цвет (если узел не выделен)
      ttNormal: if not (vsSelected in Node.States) and (FKeywords.KWData[Node.Index].Change=pkcAdd) then TargetCanvas.Font.Color := clNavy;
       // Количество вхождений ключевого слова красим серым
      ttStatic: TargetCanvas.Font.Color := clGrayText;
    end;
  end;

end.

