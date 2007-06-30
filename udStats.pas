//**********************************************************************************************************************
//  $Id: udStats.pas,v 1.25 2007-06-30 10:36:20 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udStats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, VirtualShellUtilities,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phDlg, DKLang, VirtualTrees, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls;

type
  PPStatsData = ^PStatsData;
  PStatsData = ^TStatsData;
  TStatsData = record
    wsName:  WideString;
    wsValue: WideString;
    iImgIdx: Integer;
  end;

  TdStats = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    tvMain: TVirtualStringTree;
    procedure tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Собирает и отображает статистику проекта
    procedure LoadStats;
  protected
    function  GetRelativeRegistryKey: AnsiString; override;
    function  GetSizeable: Boolean; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
  end;

  procedure ShowProjectStats(AApp: IPhotoAlbumApp);

implementation
{$R *.dfm}
uses phUtils, Main, phPhoa, phSettings;

  procedure ShowProjectStats(AApp: IPhotoAlbumApp);
  begin
    with TdStats.Create(Application) do
      try
        FApp := AApp;
        ExecuteModal(False, False);
      finally
        Free;
      end;
  end;

  procedure TdStats.DoCreate;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_stats;
     // Настраиваем tvMain
    tvMain.NodeDataSize := SizeOf(Pointer);
    ApplyTreeSettings(tvMain);
  end;

  procedure TdStats.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
     // Заполняем данные
    LoadStats;
  end;

  function TdStats.GetRelativeRegistryKey: AnsiString;
  begin
    Result := SRegStats_Root;
  end;

  function TdStats.GetSizeable: Boolean;
  begin
    Result := True;
  end;

  procedure TdStats.LoadStats;
  var n0, n1: PVirtualNode;

     // Создаёт в памяти новый объект данных статистики
    function NewStatData(const wsName, wsValue: WideString; iImgIdx: Integer = -1): PStatsData; overload;
    begin
      New(Result);
      Result.wsName  := ConstValEx(wsName);
      Result.wsValue := wsValue;
      Result.iImgIdx := iImgIdx;
    end;

    function NewStatData(const wsName: WideString; iValue: Integer): PStatsData; overload;
    begin
      Result := NewStatData(wsName, IntToStr(iValue));
    end;

     // Добавляет узлы свойств файла фотоальбома
    procedure AddPhoaFileProps(nParent: PVirtualNode);
    var
      ns: TNamespace;
      DFProp: TDiskFileProp;
    begin
      if FApp.Project.FileName<>'' then
        try
          ns := TNamespace.CreateFromFileName(FApp.Project.FileName);
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
      wsMaxFileName: WideString;  // Имя самого большого файла
      wsMinFileName: WideString;  // Имя самого маленького файла
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
        wsMaxFileName  := '';
        wsMinFileName  := '';
        for i := 0 to IDs.Count-1 do begin
          Pic := FApp.Project.Pics.ItemsByID[IDs[i]];
          i64FSize := Pic.FileSize;
          Inc(i64TotalFileSize,  i64FSize);
          Inc(i64TotalThumbSize, Length(Pic.ThumbnailData));
           // -- Ищем самый большой файл
          if i64FSize>i64MaxFileSize then begin
            i64MaxFileSize := i64FSize;
            wsMaxFileName  := Pic.FileName;
          end;
           // -- Ищем самый маленький файл
          if i64FSize<i64MinFileSize then begin
            i64MinFileSize := i64FSize;
            wsMinFileName  := Pic.FileName;
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
        if wsMaxFileName<>'' then begin
          AddChild(nParent, NewStatData('@SStat_MaxFileName',   wsMaxFileName));
          AddChild(nParent, NewStatData('@SStat_MaxFileSize',   HumanReadableSize(i64MaxFileSize)));
        end;
        if wsMinFileName<>'' then begin
          AddChild(nParent, NewStatData('@SStat_MinFileName',   wsMinFileName));
          AddChild(nParent, NewStatData('@SStat_MinFileSize',   HumanReadableSize(i64MinFileSize)));
        end;
      end;
    end;

  begin
    StartWait;
    tvMain.BeginUpdate;
    try
       // -- Фотоальбом
      n0 := tvMain.AddChild(nil, NewStatData('@SStat_PhotoAlbum', '', iiPhoA));
        n1 := tvMain.AddChild(n0, NewStatData('@SStat_PhoaFilename', FApp.Project.FileName));
          AddPhoaFileProps(n1);
          tvMain.AddChild(
            n1,
            NewStatData(
              '@SStats_PhoaFileRevision',
              aPhFileRevisions[ValidRevisionIndex(GetIndexOfRevision(FApp.Project.FileRevision))].wsName));
        tvMain.AddChild(n0, NewStatData('@SStats_DistinctPics', FApp.Project.Pics.Count));
        AddGroupStats(FApp.ProjectX.RootGroupX, n0);
       // -- Текущая группа
      if (FApp.CurGroup<>nil) and (FApp.CurGroup.ID<>FApp.Project.RootGroup.ID) then begin
        n0 := tvMain.AddChild(nil, NewStatData('@SStat_Group', '', iiFolder));
        AddGroupStats(FApp.CurGroupX, n0);
      end;
       // Разворачиваем всё дерево
      tvMain.FullExpand;
    finally
      tvMain.EndUpdate;
      StopWait;
    end;
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
      0: CellText := ppsd^^.wsName;
      1: CellText := ppsd^^.wsValue;
    end;
  end;

  procedure TdStats.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if Sender.NodeParent[Node]=nil then TargetCanvas.Font.Style := [fsBold];
  end;

end.

