//**********************************************************************************************************************
//  $Id: ufrPicProps_Metadata.pas,v 1.5 2007-06-30 10:36:21 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrPicProps_Metadata;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, phWizard,
  phPicPropsDlgPage, DKLang, TB2Item, TBX, Menus, TBXDkPanels, TB2Dock,
  VirtualTrees;

type
  TfrPicProps_Metadata = class(TPicPropsDialogPage)
    dkBottom: TTBXDock;
    dklcMain: TDKLanguageController;
    dpDesc: TTBXDockablePanel;
    ipmShowDescPanel: TTBXVisibilityToggleItem;
    lDesc: TTBXLabel;
    pmMain: TTBXPopupMenu;
    tvMain: TVirtualStringTree;
    procedure tvMainBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvMainShortenString(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const S: WideString; TextSpace: Integer; RightToLeft: Boolean; var Result: WideString; var Done: Boolean);
  private
     // Скэшированное значение настройки ISettingID_Dlgs_PP_ExpMetadata
    FExpandAll: Boolean;
     // Обновляет описание
    procedure UpdateDesc;
  protected
    procedure DoCreate; override;
    function  GetRegistrySection: WideString; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
  public
    procedure FileChanged(iIndex: Integer); override;
  end;

implementation
{$R *.dfm}
uses ConsVars, phUtils, phMetadata, udSettings, phSettings, Main;

type
  PPExifTag = ^PExifTag;

  procedure TfrPicProps_Metadata.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    if tvMain.RootNodeCount=0 then begin
      tvMain.RootNodeCount := EditedPics.Count;
      ActivateFirstVTNode(tvMain);
    end;
  end;

  procedure TfrPicProps_Metadata.DoCreate;
  begin
    inherited DoCreate;
     // Настраиваем tvMain
    ApplyTreeSettings(tvMain);
    tvMain.NodeDataSize := SizeOf(Pointer);
    tvMain.Images := FileImages;
     // Кэшируем настройки
    FExpandAll := SettingValueBool(ISettingID_Dlgs_PP_ExpMetadata);
     // Обновляем описание
    UpdateDesc;
  end;

  procedure TfrPicProps_Metadata.FileChanged(iIndex: Integer);
  var n: PVirtualNode;
  begin
    inherited FileChanged(iIndex);
     // При изменении файла "сбрасываем" (очищаем) узел
    n := GetVTRootNodeByIndex(tvMain, iIndex);
    if n<>nil then tvMain.ResetNode(n);
  end;

  function TfrPicProps_Metadata.GetRegistrySection: WideString;
  begin
    Result := SRegWizPage_PicProp_Metadata;
  end;

  procedure TfrPicProps_Metadata.tvMainBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  begin
     // Для корневого элемента (файла) устанавливаем цвет фона clBtnFace
    if Sender.NodeParent[Node]=nil then
      with TargetCanvas do begin
        Brush.Color := clBtnFace;
        FillRect(CellRect);
      end;
  end;

  procedure TfrPicProps_Metadata.tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
     // Отображаем описание
    UpdateDesc;
  end;

  procedure TfrPicProps_Metadata.tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
     // Удаляем объект списка метаданных для корневых элементов
    if Sender.NodeParent[Node]=nil then FreeAndNil(PImageMetadata(Sender.GetNodeData(Node))^);
  end;

  procedure TfrPicProps_Metadata.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if (Kind in [ikNormal, ikSelected]) and (Sender.NodeParent[Node]=nil) and (Column=0) then ImageIndex := FileImageIndex[Node.Index];
  end;

  procedure TfrPicProps_Metadata.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var nParent: PVirtualNode;
  begin
    nParent := Sender.NodeParent[Node];
     // Для узлов файлов (корневых)
    if nParent=nil then begin
      case Column of
         // Имя файла
        0: CellText := Dialog.PictureFiles[Node.Index];
         // Если ошибка - выводим сюда
        2: begin
          case PImageMetadata(Sender.GetNodeData(Node))^.StatusCode of
            IMS_OK:                CellText := '';
            IMS_CannotOpenFile:    CellText := 'IMS_CannotOpenFile';
            IMS_ReadFailure:       CellText := 'IMS_ReadFailure';
            IMS_NotAJPEGFile:      CellText := 'IMS_NotAJPEGFile';
            IMS_NoMetadata:        CellText := 'IMS_NoMetadata';
            IMS_InvalidEXIFHeader: CellText := 'IMS_InvalidEXIFHeader';
            IMS_InvalidTIFFHeader: CellText := 'IMS_InvalidTIFFHeader';
            else                   CellText := 'IMS_InternalError';
          end;
          if CellText<>'' then CellText := DKLangConstW(CellText);
        end;
      end;
     // Для элементов строк метаданных (дочерних узлов по отношению к файлам)
    end else
      case Column of
         // Код тега
        0: CellText := WideFormat('%.4x', [PPExifTag(Sender.GetNodeData(Node))^.iTag]);
         // Имя тега
        1: CellText := PPExifTag(Sender.GetNodeData(Node))^.sName;
         // Загруженное значение тега
        2: CellText := Copy(PImageMetadata(Sender.GetNodeData(nParent))^.EXIFData[Node.Index], 1, 200);
      end;
  end;

  procedure TfrPicProps_Metadata.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    pim: PImageMetadata;
    ppt: PPExifTag;
  begin
     // Файл (корневой элемент)
    if ParentNode=nil then begin
      pim := Sender.GetNodeData(Node);
       // Создаём объект списка метаданных на каждый файл. При создании класса данные сразу считываются
      pim^ := TImageMetadata.Create(Dialog.PictureFiles[Node.Index]);
       // Если не произошло ошибки при считывании
      if pim^.StatusCode=IMS_OK then begin
        Sender.ChildCount[Node] := pim^.EXIFData.Count;
         // Раскрываем узел, если надо
        if FExpandAll then Include(InitialStates, ivsExpanded);
      end;
     // Тег/данные тега (вложенный узел)
    end else begin
      ppt := Sender.GetNodeData(Node);
      pim := Sender.GetNodeData(ParentNode);
       // Ищем и запоминаем тег
      ppt^ := FindEXIFTag(Integer(pim^.EXIFData.Objects[Node.Index]));
    end;
  end;

  procedure TfrPicProps_Metadata.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if Sender.NodeParent[Node]=nil then
      case Column of
         // Выделяем полужирным имена файлов
        0: TargetCanvas.Font.Style := [fsBold];
         // В случае ошибки выделяем красным
        2: if PImageMetadata(Sender.GetNodeData(Node))^.StatusCode<>IMS_OK then TargetCanvas.Font.Color := $0000c0;
      end;
  end;

  procedure TfrPicProps_Metadata.tvMainShortenString(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const S: WideString; TextSpace: Integer; RightToLeft: Boolean; var Result: WideString; var Done: Boolean);
  begin
    Result := ShortenFileName(TargetCanvas, TextSpace-10, S);
    Done := True;
  end;

  procedure TfrPicProps_Metadata.UpdateDesc;
  var
    n: PVirtualNode;
    ws: WideString;
  begin
    n := tvMain.FocusedNode;
    if tvMain.NodeParent[n]=nil then
      ws := DKLangConstW('SMsg_NoMetatagSelected')
    else begin
      ws := PPExifTag(tvMain.GetNodeData(n))^^.sDesc;
      if ws='' then ws := DKLangConstW('SNone');
    end;
    lDesc.Caption := ws;
  end;

end.

