//**********************************************************************************************************************
//  $Id: ufrPicProps_Groups.pas,v 1.14 2004-10-18 19:27:03 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrPicProps_Groups;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phWizard, VirtualTrees, phPicPropsDlgPage;

type
  TfrPicProps_Groups = class(TPicPropsDialogPage)
    tvMain: TVirtualStringTree;
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Возвращает количество выбранных изображений среди изображений группы
    function  GetSelCount(Group: IPhotoAlbumPicGroup): Integer;
  protected
    procedure InitializePage; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure AfterDisplay(ChangeMethod: TPageChangeMethod); override;
  public
    function  CanApply: Boolean; override;
    procedure Apply(AOperations: TPhoaOperations; var Changes: TPhoaOperationChanges); override;
  end;

implementation
{$R *.dfm}
uses Main, phUtils, phSettings, udMsgBox;

type
   // Данные, ассоциированные с каждым узлом дерева групп
  PGroupData = ^TGroupData;
  TGroupData = record
    Group: IPhotoAlbumPicGroup; // Ссылка на группу
    iSelCount: Integer;         // Количество изображений в группе из числа выбранных
  end;

  procedure TfrPicProps_Groups.AfterDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited AfterDisplay(ChangeMethod);
    StorageForm.ActiveControl := tvMain;
  end;

  procedure TfrPicProps_Groups.Apply(AOperations: TPhoaOperations; var Changes: TPhoaOperationChanges);
  var
    n: PVirtualNode;
    pgd: PGroupData;
    bChanges, bLinked: Boolean;
    Pics: IPhoaMutablePicList;
    Pic: IPhoaPic;
    OpParams: IPhoaOperationParams;
    i: Integer;
  begin
     // Если страница инициализировалась, создаём операции добавления/удаления изображений
    if tvMain.RootNodeCount>0 then begin
       // Крутим цикл по узлам дерева (по группам)
      n := tvMain.GetFirst;
      while n<>nil do begin
         // Определяем, есть ли изменения
        pgd := tvMain.GetNodeData(n);
        case n.CheckState of
           // -- Надо включить все
          csCheckedNormal:   bChanges := pgd.iSelCount<EditedPics.Count;
           // -- Надо выключить все
          csUncheckedNormal: bChanges := pgd.iSelCount>0;
           // -- Не менялось
          else               bChanges := False;
        end;
         // Если есть - составляем список изображений
        if bChanges then begin
          Pics := NewPhotoAlbumPicList(False);
          for i := 0 to EditedPics.Count-1 do begin
            Pic := EditedPics[i];
            bLinked := pgd.Group.IsPicLinked(Pic.ID, False);
            if (n.CheckState=csCheckedNormal) <> bLinked then Pics.Add(Pic, False);
          end;
           // Выполняем (создаём) операцию
          OpParams := NewPhoaOperationParams(['Group', pgd.Group, 'Pics', Pics]);
          if n.CheckState=csCheckedNormal then
            TPhoaOp_InternalPicToGroupAdding.Create(AOperations, App.Project, OpParams, Changes)
          else
            TPhoaOp_InternalPicFromGroupRemoving.Create(AOperations, App.Project, OpParams, Changes);
        end;
         // Переходим к следующей группе
        n := tvMain.GetNext(n);
      end;
    end;
  end;

  procedure TfrPicProps_Groups.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    with tvMain do
      if RootNodeCount=0 then begin
        BeginUpdate;
        try
          RootNodeCount := 1;
           // Инициализируем все узлы, чтобы раскрылись отмеченные
          ReinitChildren(nil, True);
          OffsetXY := Point(0, 0);
        finally
          EndUpdate;
        end;
      end;
  end;

  function TfrPicProps_Groups.CanApply: Boolean;
  var
    i, iUnlinkedCount: Integer;
    bAllInGroups: Boolean;

     // Возвращает True, если изображение содержится в какой-нибудь группе
    function PicLinked(iPicID: Integer): Boolean;
    var Node: PVirtualNode;
    begin
      Result := False;
       // Цикл по дереву
      Node := tvMain.GetFirst;
      while (Node<>nil) and not Result do begin
        case Node.CheckState of
           // Если настоящая птица - то понятно, что как минимум в этой группе будут ВСЕ выделенные, проверять дальше
           //   бессмысленно
          csCheckedNormal: begin
            Result := True;
            bAllInGroups := True;
          end;
           // Если неполная птица - проверяем наличие в этой группе конкретно этого изображения
          csMixedNormal: Result := PGroupData(tvMain.GetNodeData(Node)).Group.IsPicLinked(iPicID, False);
        end;
        Node := tvMain.GetNext(Node);
      end;
    end;

  begin
     // Если страница не инициализировалась
    if tvMain.RootNodeCount=0 then
      Result := True
     // Иначе проверяем, что каждое изображение содержится в какой-либо группе
    else begin
      bAllInGroups := False;
      iUnlinkedCount := 0;
       // Цикл по выбранным изображениям
      for i := 0 to EditedPics.Count-1 do begin
        if not PicLinked(EditedPics[i].ID) then Inc(iUnlinkedCount);
        if bAllInGroups then Break;
      end;
       // Проверяем количество несвязанных
      Result := iUnlinkedCount=0;
      if not Result then PhoaError('SErrNotAllPicsLinked', [iUnlinkedCount, EditedPics.Count]);
    end;
  end;

  function TfrPicProps_Groups.GetSelCount(Group: IPhotoAlbumPicGroup): Integer;
  var i: Integer;
  begin
    Result := 0;
    for i := 0 to EditedPics.Count-1 do
      if Group.IsPicLinked(EditedPics[i].ID, False) then Inc(Result);
  end;

  procedure TfrPicProps_Groups.InitializePage;
  begin
    inherited InitializePage;
    ApplyTreeSettings(tvMain);
    tvMain.NodeDataSize := SizeOf(TGroupData);
  end;

  procedure TfrPicProps_Groups.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Modified;
  end;

  procedure TfrPicProps_Groups.tvMainChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
  begin
     // Выставляем тип птицы: если изначально было Grayed - после Unchecked идёт Grayed
    if (Node.CheckState=csUncheckedNormal) and (NewState=csCheckedNormal) then
      with PGroupData(Sender.GetNodeData(Node))^ do
        if (iSelCount>0) and (iSelCount<EditedPics.Count) then NewState := csMixedNormal;
  end;

  procedure TfrPicProps_Groups.tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Finalize(PGroupData(Sender.GetNodeData(Node))^);
  end;

  procedure TfrPicProps_Groups.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := iif(Sender.NodeParent[Node]=nil, iiPhoA, iiFolder);
    Ghosted := Node.CheckState=csUncheckedNormal;
  end;

  procedure TfrPicProps_Groups.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    p: PGroupData;
    s: String;
  begin
    p := Sender.GetNodeData(Node);
    if p<>nil then
      if TextType=ttStatic then begin
        if p.Group.Pics.Count>0 then s := Format(iif(p.iSelCount>0, '(%d/%d)', '(%1:d)'), [p.iSelCount, p.Group.Pics.Count]);
      end else if Sender.NodeParent[Node]<>nil then s := p.Group.Text
      else s := ConstVal('SPhotoAlbumNode');
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TfrPicProps_Groups.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var p, pp: PGroupData;
  begin
    p := Sender.GetNodeData(Node);
    if ParentNode=nil then
      p.Group := App.Project.RootGroupX
    else begin
      pp := Sender.GetNodeData(ParentNode);
      p.Group := pp.Group.GroupsX[Node.Index];
    end;
    Sender.ChildCount[Node] := p.Group.Groups.Count;
     // Считаем количество выбранных изображений среди изображений группы
    p.iSelCount := GetSelCount(p.Group);
     // Настраиваем CheckBox
    Node.CheckType  := ctCheckBox;
    if      p.iSelCount=0                then Node.CheckState := csUncheckedNormal
    else if p.iSelCount<EditedPics.Count then Node.CheckState := csMixedNormal
    else                                      Node.CheckState := csCheckedNormal;
    if p.iSelCount>0 then Sender.FullyVisible[Node] := True;
  end;

  procedure TfrPicProps_Groups.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if TextType=ttStatic then TargetCanvas.Font.Color := clGrayText;
  end;

end.
