//**********************************************************************************************************************
//  $Id: udStats.pas,v 1.16 2004-10-12 12:38:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udStats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualShellUtilities, Placemnt, 
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars, phDlg,
  DKLang, VirtualTrees, StdCtrls, ExtCtrls;

type
  PPStatsData = ^PStatsData;
  PStatsData = ^TStatsData;
  TStatsData = record
    sName:   String;
    sValue:  String;
    iImgIdx: Integer;
  end;

  TdStats = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    tvMain: TVirtualStringTree;
    fpMain: TFormPlacement;
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  private
     // Проект
    FProject: IPhotoAlbumProject;
     // Текущая группа
    FGroup: IPhotoAlbumPicGroup;
     // Создаёт в памяти новый объект данных статистики
    function NewStatData(const sName, sValue: String; iImgIdx: Integer = -1): PStatsData; overload;
    function NewStatData(const sName: String; iValue: Integer): PStatsData; overload;
  protected
    procedure InitializeDialog; override;
  end;

  procedure ShowProjectStats(AProject: IPhotoAlbumProject; AGroup: IPhotoAlbumPicGroup);

implementation
{$R *.dfm}
uses phUtils, Main, phPhoa, phSettings;

  procedure ShowProjectStats(AProject: IPhotoAlbumProject; AGroup: IPhotoAlbumPicGroup);
  begin
    with TdStats.Create(Application) do
      try
        FProject := AProject;
        FGroup   := AGroup;
        Execute;
      finally
        Free;
      end;
  end;

  procedure TdStats.InitializeDialog;
  var n0, n1: PVirtualNode;

     // Добавляет узлы свойств файла фотоальбома
    procedure AddPhoaFileProps(nParent: PVirtualNode);
    var
      ns: TNamespace;
      DFProp: TDiskFileProp;
    begin
      if FProject.FileName<>'' then
        try
          ns := TNamespace.CreateFromFileName(FProject.FileName);
          for DFProp := Low(DFProp) to High(DFProp) do tvMain.AddChild(nParent, NewStatData(DiskFilePropName(DFProp), DiskFilePropValue(DFProp, ns)));
        except
          on EVSTInvalidFileName do {ignore};
        end;
    end;

     // Добавляет узлы статистики группы
    procedure AddGroupStats(Group: IPhotoAlbumPicGroup; nParent: PVirtualNode);
    var
      iCntNestedGroups: Integer;  // Количество вложенных подгрупп
      iCntPicsInGroup: Integer;   // Количество изображений в группе
      iCntPics: Integer;          // Общее количество изображений (в группе и вложенных подгруппах)
      iCntDistinctPics: Integer;  // Количество различных изображений в группе и вложенных подгруппах
      i64TotalFileSize: Int64;    // Суммарный размер файлов изображений
      i64AverageFileSize: Int64;  // Средний размер файлов изображений
      i64TotalThumbSize: Int64;   // Суммарный размер эскизов
      i64AverageThumbSize: Int64; // Средний размер эскизов
      i64MaxFileSize: Int64;      // Размер самого большого файла
      i64MinFileSize: Int64;      // Размер самого маленького файла
      sMaxFileName: String;       // Имя самого большого файла
      sMinFileName: String;       // Имя самого маленького файла
      i: Integer;
      i64FSize: Int64;
      IDs: TIntegerList;
      Pic: IPhoaPic;

      procedure ProcessGroup(Group: IPhotoAlbumPicGroup);
      var
        i: Integer;
        gChild: IPhotoAlbumPicGroup;
      begin
        Inc(iCntPics, Group.Pics.Count);
         // Добавляем ID изображений в список
        for i := 0 to Group.Pics.Count-1 do IDs.Add(Group.Pics[i].ID);
         // Рекурсивно вызываем для вложенных групп
        for i := 0 to Group.Groups.Count-1 do begin
          gChild := Group.GroupsX[i];
          Inc(iCntNestedGroups);
          ProcessGroup(gChild);
        end;
      end;

    begin
      iCntNestedGroups  := 0;
      iCntPics          := 0;
      i64TotalFileSize  := 0;
      i64TotalThumbSize := 0;
      IDs := TIntegerList.Create(False);
      try
        iCntPicsInGroup  := Group.Pics.Count;
         // Рекурсивно прогоняем группы
        ProcessGroup(Group);
        iCntDistinctPics := IDs.Count;
         // Считаем размеры файлов/эскизов
        i64MaxFileSize := 0;
        i64MinFileSize := MaxInt;
        sMaxFileName   := '';
        sMinFileName   := '';
        for i := 0 to IDs.Count-1 do begin
          Pic := FProject.Pics.ItemsByID[IDs[i]];
          i64FSize := Pic.FileSize;
          Inc(i64TotalFileSize,  i64FSize);
          Inc(i64TotalThumbSize, Length(Pic.ThumbnailData));
           // -- Ищем самый большой файл
          if i64FSize>i64MaxFileSize then begin
            i64MaxFileSize := i64FSize;
            sMaxFileName   := Pic.FileName;
          end;
           // -- Ищем самый маленький файл
          if i64FSize<i64MinFileSize then begin
            i64MinFileSize := i64FSize;
            sMinFileName   := Pic.FileName;
          end;
        end;
         // -- Находим средние значения
        if iCntDistinctPics=0 then begin
          i64AverageFileSize  := 0;
          i64AverageThumbSize := 0;
        end else begin
          i64AverageFileSize  := i64TotalFileSize  div iCntDistinctPics;
          i64AverageThumbSize := i64TotalThumbSize div iCntDistinctPics;
        end;
      finally
        IDs.Free;
      end;
      with tvMain do begin
        AddChild(nParent, NewStatData('@SStat_CntNestedGroups', iCntNestedGroups));
        AddChild(nParent, NewStatData('@SStat_CntPicsInGroup',  iCntPicsInGroup));
        AddChild(nParent, NewStatData('@SStat_CntPics',         iCntPics));
        AddChild(nParent, NewStatData('@SStat_CntDistinctPics', iCntDistinctPics));
        AddChild(nParent, NewStatData('@SStat_TotalFileSize',   HumanReadableSize(i64TotalFileSize)));
        AddChild(nParent, NewStatData('@SStat_AvgFileSize',     HumanReadableSize(i64AverageFileSize)));
        AddChild(nParent, NewStatData('@SStat_TotalThumbSize',  HumanReadableSize(i64TotalThumbSize)));
        AddChild(nParent, NewStatData('@SStat_AvgThumbSize',    HumanReadableSize(i64AverageThumbSize)));
        if sMaxFileName<>'' then begin
          AddChild(nParent, NewStatData('@SStat_MaxFileName',   sMaxFileName));
          AddChild(nParent, NewStatData('@SStat_MaxFileSize',   HumanReadableSize(i64MaxFileSize)));
        end;
        if sMinFileName<>'' then begin
          AddChild(nParent, NewStatData('@SStat_MinFileName',   sMinFileName));
          AddChild(nParent, NewStatData('@SStat_MinFileSize',   HumanReadableSize(i64MinFileSize)));
        end;
      end;
    end;

  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_stats;
    MakeSizeable;
     // Настраиваем fpMain
    fpMain.IniFileName := SRegRoot;
    fpMain.IniSection  := SRegStats_Root;
     // Настраиваем tvMain
    tvMain.NodeDataSize := SizeOf(Pointer);
    ApplyTreeSettings(tvMain);
     // Заполняем данные
    StartWait;
    tvMain.BeginUpdate;
    try
       // -- Фотоальбом
      n0 := tvMain.AddChild(nil, NewStatData('@SStat_PhotoAlbum', '', iiPhoA));
        n1 := tvMain.AddChild(n0, NewStatData('@SStat_PhoaFilename', FProject.FileName));
          AddPhoaFileProps(n1);
          tvMain.AddChild(n1, NewStatData('@SStats_PhoaFileRevision', aPhFileRevisions[ValidRevisionIndex(GetIndexOfRevision(FProject.FileRevision))].sName));
        tvMain.AddChild(n0, NewStatData('@SStats_DistinctPics', FProject.Pics.Count));
        AddGroupStats(FProject.RootGroupX, n0);
       // -- Текущая группа
      if (FGroup<>nil) and (FGroup.ID<>FProject.RootGroup.ID) then begin
        n0 := tvMain.AddChild(nil, NewStatData('@SStat_Group', '', iiFolder));
        AddGroupStats(FGroup, n0);
      end;
       // Разворачиваем всё дерево
      tvMain.FullExpand;
    finally
      tvMain.EndUpdate;
      StopWait;
    end;
  end;

  function TdStats.NewStatData(const sName, sValue: String; iImgIdx: Integer = -1): PStatsData;
  var s: String;
  begin
     // Если строка начинается на '@' - это имя константы
    if sName[1]='@' then s := ConstVal(Copy(sName, 2, MaxInt)) else s := sName;
    New(Result);
    Result^.sName   := s;
    Result^.sValue  := sValue;
    Result^.iImgIdx := iImgIdx;
  end;

  function TdStats.NewStatData(const sName: String; iValue: Integer): PStatsData;
  begin
    Result := NewStatData(sName, IntToStr(iValue));
  end;

  procedure TdStats.tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Dispose(PPStatsData(Sender.GetNodeData(Node))^);
  end;

  procedure TdStats.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Column=0 then ImageIndex := PPStatsData(Sender.GetNodeData(Node))^^.iImgIdx;
  end;

  procedure TdStats.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var ppsd: PPStatsData;
  begin
    ppsd := Sender.GetNodeData(Node);
    case Column of
      0: CellText := AnsiToUnicodeCP(ppsd^^.sName,  cMainCodePage);
      1: CellText := AnsiToUnicodeCP(ppsd^^.sValue, cMainCodePage);
    end;
  end;

  procedure TdStats.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if Sender.NodeParent[Node]=nil then TargetCanvas.Font.Style := [fsBold];
  end;

end.

