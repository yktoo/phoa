//**********************************************************************************************************************
//  $Id: udStats.pas,v 1.6 2004-08-30 14:10:08 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit udStats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, PhObj, ConsVars, VirtualShellUtilities,
  Dialogs, phDlg, DTLangTools, StdCtrls, VirtualTrees, ExtCtrls, Placemnt;

type
  PPStatsData = ^PStatsData;
  PStatsData = ^TStatsData;
  TStatsData = record
    sName:   String;
    sValue:  String;
    iImgIdx: Integer;
  end;

  TdStats = class(TPhoaDialog)
    tvMain: TVirtualStringTree;
    fpMain: TFormPlacement;
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  private
     // Фотоальбом
    FPhoA: TPhotoAlbum;
     // Текущая группа
    FGroup: TPhoaGroup;
     // Выбранные изображения
    FSelPics: TPicArray;
     // Создаёт в памяти новый объект данных статистики
    function NewStatData(const sName, sValue: String; iImgIdx: Integer = -1): PStatsData; overload;
    function NewStatData(const sName: String; iValue: Integer): PStatsData; overload;
  protected
    procedure InitializeDialog; override;
  end;

  procedure ShowPhoaStats(PhoA: TPhotoAlbum; Group: TPhoaGroup; aSelPics: TPicArray);

implementation
{$R *.dfm}
uses phUtils, Main, phPhoa, phSettings;

  procedure ShowPhoaStats(PhoA: TPhotoAlbum; Group: TPhoaGroup; aSelPics: TPicArray);
  begin
    with TdStats.Create(Application) do
      try
        FPhoA    := PhoA;
        FGroup   := Group;
        FSelPics := aSelPics;
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
      if FPhoA.FileName<>'' then
        try
          ns := TNamespace.CreateFromFileName(FPhoA.FileName);
          for DFProp := Low(DFProp) to High(DFProp) do tvMain.AddChild(nParent, NewStatData(DiskFilePropName(DFProp), DiskFilePropValue(DFProp, ns)));
        except
          on EVSTInvalidFileName do {ignore};
        end;
    end;

     // Добавляет узлы статистики группы
    procedure AddGroupStats(Group: TPhoaGroup; nParent: PVirtualNode);
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
      Pic: TPhoaPic;

      procedure ProcessGroup(Group: TPhoaGroup);
      var
        i: Integer;
        gChild: TPhoaGroup;
      begin
        Inc(iCntPics, Group.PicIDs.Count);
         // Добавляем ID изображений в список
        for i := 0 to Group.PicIDs.Count-1 do IDs.Add(Group.PicIDs[i]);
         // Рекурсивно вызываем для вложенных групп
        for i := 0 to Group.Groups.Count-1 do begin
          gChild := Group.Groups[i];
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
        iCntPicsInGroup  := Group.PicIDs.Count;
         // Рекурсивно прогоняем группы
        ProcessGroup(Group);
        iCntDistinctPics := IDs.Count;
         // Считаем размеры файлов/эскизов
        i64MaxFileSize := 0;
        i64MinFileSize := MaxInt;
        sMaxFileName   := '';
        sMinFileName   := '';
        for i := 0 to IDs.Count-1 do begin
          Pic := FPhoA.Pics.PicByID(IDs[i]);
          i64FSize := Pic.PicFileSize;
          Inc(i64TotalFileSize,  i64FSize);
          Inc(i64TotalThumbSize, Length(Pic.ThumbnailData));
           // -- Ищем самый большой файл
          if i64FSize>i64MaxFileSize then begin
            i64MaxFileSize := i64FSize;
            sMaxFileName   := Pic.PicFileName;
          end;
           // -- Ищем самый маленький файл
          if i64FSize<i64MinFileSize then begin
            i64MinFileSize := i64FSize;
            sMinFileName   := Pic.PicFileName;
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
    try
       // -- Фотоальбом
      n0 := tvMain.AddChild(nil, NewStatData('@SStat_PhotoAlbum', '', iiPhoA));
        n1 := tvMain.AddChild(n0, NewStatData('@SStat_PhoaFilename', FPhoA.FileName));
          AddPhoaFileProps(n1);
          tvMain.AddChild(n1, NewStatData('@SStats_PhoaFileRevision', aPhFileRevisions[ValidRevisionIndex(GetIndexOfRevision(FPhoA.FileRevision))].sName));
        tvMain.AddChild(n0, NewStatData('@SStats_DistinctPics', FPhoA.Pics.Count));
        AddGroupStats(FPhoA.RootGroup, n0);
       // -- Текущая группа
      if (FGroup<>nil) and (FGroup<>FPhoA.RootGroup) then begin
        n0 := tvMain.AddChild(nil, NewStatData('@SStat_Group', '', iiFolder));
        AddGroupStats(FGroup, n0);
      end;
       // Разворачиваем всё дерево
      tvMain.FullExpand;
    finally
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
