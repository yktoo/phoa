//**********************************************************************************************************************
//  $Id: ufrPicProps_FileProps.pas,v 1.6 2007-07-04 18:48:49 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrPicProps_FileProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, phWizard,
  phPicPropsDlgPage, DKLang, ActnList, TntActnList, TB2Item, TBX, TB2Dock,
  TB2Toolbar, VirtualTrees;

type
  TfrPicProps_FileProps = class(TPicPropsDialogPage)
    aChangeFile: TTntAction;
    alMain: TTntActionList;
    bChangeFile: TTBXItem;
    dklcMain: TDKLanguageController;
    tbMain: TTBXToolbar;
    tvMain: TVirtualStringTree;
    procedure aaChangeFile(Sender: TObject);
    procedure tvMainBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvMainContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvMainShortenString(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const S: WideString; TextSpace: Integer; RightToLeft: Boolean; var Result: WideString; var Done: Boolean);
  private
     // Скэшированное значение настройки ISettingID_Dlgs_PP_ExpFileProps
    FExpandAll: Boolean;
     // Возвращает индекс файла, соответствующего текущему выделенному узлу; -1, если нет выделенного узла
    function  GetCurFileIndex: Integer;
  protected
    procedure DoCreate; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
  public
    procedure FileChanged(iIndex: Integer); override;
    procedure Apply(var wsOpParamName: WideString; var OpParams: IPhoaOperationParams); override;
  end;

implementation
{$R *.dfm}
uses TypInfo, VirtualShellUtilities, phUtils, phSettings, GraphicEx, Main;

type
  PNamespace = ^TNamespace;

  procedure TfrPicProps_FileProps.aaChangeFile(Sender: TObject);
  var idx: Integer;

    procedure UpdatePicFile(const wsFileName: WideString);
    var iPicID: Integer;
    begin
       // Ищем изображение с таким файлом
      iPicID := Dialog.FindPicIDByFileName(wsFileName);
       // Не нашли - изменяем файл
      if iPicID=0 then Dialog.PictureFiles[idx] := wsFileName
       // Если нашли и это не то же самое изображение (т.е. файл изменён) - ошибка
      else if iPicID<>EditedPics[idx].ID then PhoaExceptionConst('SErrPicFileAlreadyInUse', [wsFileName, iPicID]);
    end;

  begin
    idx := GetCurFileIndex;
    if idx>=0 then begin
      with TOpenDialog.Create(Self) do
        try
          FileName := Dialog.PictureFiles[idx];
          Filter   := FileFormatList.GetGraphicFilter([], fstExtension, [foCompact, foIncludeAll, foIncludeExtension], nil);
          Options  := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
          Title    := DKLangConstW('SDlgTitle_SelectPicFile');
          if Execute then UpdatePicFile(FileName);
        finally
          Free;
        end;
    end;
  end;

  procedure TfrPicProps_FileProps.Apply(var wsOpParamName: WideString; var OpParams: IPhoaOperationParams);
  var
    i: Integer;
    ChgList: IPhoaPicFileChangeList;
    Pic: IPhotoAlbumPic;
    wsFileName: WideString;
  begin
     // Если страница посещалась/есть файлы
    if tvMain.RootNodeCount>0 then begin
       // Составляем список изменений файлов
      ChgList := NewPhoaPicFileChangeList;
      for i := 0 to EditedPics.Count-1 do begin
        Pic        := EditedPics[i];
        wsFileName := Dialog.PictureFiles[i];
        if Pic.FileName<>wsFileName then ChgList.Add(Pic, wsFileName);
      end;
       // Если есть изменения - возвращаем параметры подоперации
      if ChgList.Count>0 then begin
        wsOpParamName := 'EditFilesOpParams';
        OpParams      := NewPhoaOperationParams(['FileChangeList', ChgList]);
      end;
    end;
  end;

  procedure TfrPicProps_FileProps.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    if tvMain.RootNodeCount=0 then begin
      tvMain.RootNodeCount := EditedPics.Count;
      ActivateFirstVTNode(tvMain);
    end;
  end;

  procedure TfrPicProps_FileProps.DoCreate;
  begin
    inherited DoCreate;
    ApplyTreeSettings(tvMain);
    tvMain.NodeDataSize := SizeOf(Pointer);
    tvMain.Images := FileImages;
    FExpandAll := SettingValueBool(ISettingID_Dlgs_PP_ExpFileProps);
  end;

  procedure TfrPicProps_FileProps.FileChanged(iIndex: Integer);
  var n: PVirtualNode;
  begin
    inherited FileChanged(iIndex);
     // При изменении файла "сбрасываем" (очищаем) узел
    n := GetVTRootNodeByIndex(tvMain, iIndex);
    if n<>nil then tvMain.ResetNode(n);
  end;

  function TfrPicProps_FileProps.GetCurFileIndex: Integer;
  var n: PVirtualNode;
  begin
    n := tvMain.FocusedNode;
    if n=nil then
      Result := -1
    else begin
      if tvMain.NodeParent[n]<>nil then n := tvMain.NodeParent[n];
      Result := n.Index;
    end;
  end;

  procedure TfrPicProps_FileProps.tvMainBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  begin
     // Для корневого элемента (файла) устанавливаем цвет фона clBtnFace
    if Sender.NodeParent[Node]=nil then
      with TargetCanvas do begin
        Brush.Color := clBtnFace;
        FillRect(CellRect);
      end;
  end;

  procedure TfrPicProps_FileProps.tvMainContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  var idx: Integer;
  begin
    idx := GetCurFileIndex;
    if idx>=0 then ShowFileShellContextMenu(Dialog.PictureFiles[idx]);
    Handled := True;
  end;

  procedure TfrPicProps_FileProps.tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
     // Удаляем объект TNamespace узла для корневого элемента
    if Sender.NodeParent[Node]=nil then FreeAndNil(PNamespace(Sender.GetNodeData(Node))^);
  end;

  procedure TfrPicProps_FileProps.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if (Kind in [ikNormal, ikSelected]) and (Sender.NodeParent[Node]=nil) and (Column=0) then ImageIndex := FileImageIndex[Node.Index];
  end;

  procedure TfrPicProps_FileProps.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    nParent: PVirtualNode;
    NS: TNamespace;
    DFProp: TDiskFileProp;
  begin
    nParent := Sender.NodeParent[Node];
     // Для узлов файлов (корневых)
    if nParent=nil then begin
      if Column=0 then CellText := Dialog.PictureFiles[Node.Index];
     // Для элементов свойств файла (дочерних узлов по отношению к файлам)
    end else begin
      NS := PNamespace(Sender.GetNodeData(nParent))^;
      DFProp := TDiskFileProp(Node.Index);
      case Column of
         // Имя свойства
        0: if NS=nil then CellText := DKLangConstW('SError') else CellText := DiskFilePropName(DFProp);
         // Значение свойства
        1: if NS=nil then CellText := DKLangConstW('SErrFileNotFound') else CellText := DiskFilePropValue(DFProp, NS);
      end;
    end;
  end;

  procedure TfrPicProps_FileProps.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var p: PNamespace;
  begin
     // Создаём объект пространства имён на каждый файл (корневой элемент)
    if ParentNode=nil then begin
      p := Sender.GetNodeData(Node);
       // При создании объекта данные сразу считываются или возникает exception, если файла нет
      try
        p^ := TNamespace.CreateFromFileName(Dialog.PictureFiles[Node.Index]);
      except
        p^ := nil;
      end;
       // Раскрываем узел, если надо
      if FExpandAll then Include(InitialStates, ivsExpanded);
       // Если произошла ошибка при считывании, отображаем единственную строку с описанием ошибки
      Sender.ChildCount[Node] := iif(p^=nil, 1, Byte(High(TDiskFileProp))+1);
    end;
  end;

  procedure TfrPicProps_FileProps.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  var nParent: PVirtualNode;
  begin
    nParent := Sender.NodeParent[Node];
     // Выделяем полужирным имена файлов (корневые узлы)
    if nParent=nil then TargetCanvas.Font.Style := [fsBold]
     // В случае ошибки выделяем красным
    else if PNamespace(Sender.GetNodeData(nParent))^=nil then TargetCanvas.Font.Color := clRed;
  end;

  procedure TfrPicProps_FileProps.tvMainShortenString(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const S: WideString; TextSpace: Integer; RightToLeft: Boolean; var Result: WideString; var Done: Boolean);
  begin
    Result := ShortenFileName(TargetCanvas, TextSpace-10, S);
    Done := True;
  end;

end.
