//**********************************************************************************************************************
//  $Id: phNativeIntf.pas,v 1.16 2007-07-01 18:06:53 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phNativeIntf;

interface

uses Windows, ImgList, ActnList, phIntf, phAppIntf, phMutableIntf, phPhoa;

type

   //===================================================================================================================
   // IPhotoAlbumPic - изображение фотоальбома
   //===================================================================================================================

  IPhotoAlbumKeywordList = interface;
  IPhotoAlbumPicList     = interface;

  IPhotoAlbumPic = interface(IPhoaMutablePic)
    ['{AE945E5F-9BF1-4FD0-92C9-92716D7BB631}']
     // Добавляет изображение в список List. При iNewID<0 оставляет ID изображения неизменным, при iNewID=0 распределяет
     //   изображению новый ID, уникальный в List. При iNewID>0 присваивает этот ID изображению
    procedure PutToList(List: IPhoaMutablePicList; iNewID: Integer = -1);
     // Загрузка/сохранение с помощью Streamer
     //   -- Параметр bEx...Relative контролирует, осуществлять ли преобразование относительного <-> абсолютного пути
     //      к файлу изображения
     //   -- Параметр PProps указывает, какие свойства сохранять и восстанавливать (при этом все внутренние данные,
     //      связанные с изображением, т.е. с файлом, сохраняются только при наличии ppFileName in PProps)
    procedure StreamerLoad(Streamer: TPhoaStreamer; bExpandRelative: Boolean; PProps: TPicProperties);
    procedure StreamerSave(Streamer: TPhoaStreamer; bExtractRelative: Boolean; PProps: TPicProperties);
     // Prop handlers
    function  GetKeywordsX: IPhotoAlbumKeywordList;
     // Props
     // -- 'Native' version of Keywords
    property KeywordsX: IPhotoAlbumKeywordList read GetKeywordsX;
  end;

   //===================================================================================================================
   // IPhotoAlbumKeywordList - список ключевых слов
   //===================================================================================================================

   // Требуемое изменение [текста] ключевого слова
  TPhoaKeywordChange = (pkcNone, pkcAdd, pkcReplace);
   // Состояние выбора ключевого слова
  TPhoaKeywordState  = (pksOff, pksGrayed, pksOn);

   // Дополнительные данные ключевого слова
  PPhoaKeywordData = ^TPhoaKeywordData;
  TPhoaKeywordData = record
    wsKeyword:    WideString;         // Ключевое слово
    wsOldKeyword: WideString;         // Прежнее ключевое слово, если нужно заменить существующее на другое
    Change:       TPhoaKeywordChange; // Требуемое изменение [текста] ключевого слова
    State:        TPhoaKeywordState;  // Состояние выбора ключевого слова
    iCount:       Integer;            // Количество вхождений в фотоальбом (для существующих, т.е. Change<>kcAdd)
    iSelCount:    Integer;            // Количество упоминаний среди выбранных изображений (заполняется в PopulateFromPicList при предоставлении Callback-процедуры)
  end;

   // Callback-процедура, вызываемая из IPhotoAlbumKeywordList.PopulateFromPicList() для определения, выбрано
   // изображение или нет
  TPhoaKeywordIsPicSelectedProc = procedure(Pic: IPhoaPic; out bSelected: Boolean) of object;

  IPhotoAlbumKeywordList = interface(IPhoaMutableKeywordList)
    ['{B14C063F-43EC-48BA-9724-562A97E5E2C7}']
     // Добавляет слово. При повторном добавлении слова оно не добавляется, только приращивается счётчик. Если
     //   bSelected=True, приращивается также счётчик iSelCount
    function  AddEx(const wsKeyword: WideString; bSelected: Boolean): Integer;
     // Заполняет список на основе ключевых слов изображений из списка. Если передана процедура IsPicSelCallback,
     //   параллельно заполняются счётчики TKeywordRec.iSelCount, в соответствии с iTotalSelCount выставляется
     //   TKeywordRec.State
    procedure PopulateFromPicList(Pics: IPhoaPicList; IsPicSelCallback: TPhoaKeywordIsPicSelectedProc; iTotalSelCount: Integer);
     // Добавляет новое слово с уникальным именем и возвращает его индекс в списке
    function  InsertNew: Integer;
     // Prop handlers
    function  GetKWData(Index: Integer): PPhoaKeywordData;
    function  GetSelectedKeywords: WideString;
    procedure SetSelectedKeywords(const Value: WideString);
     // Props
     // -- Данные ключевых слов по индексу
    property KWData[Index: Integer]: PPhoaKeywordData read GetKWData; 
     // -- Разделённые запятой выбранные слова. При присваивании расставляет выбранность слов
    property SelectedKeywords: WideString read GetSelectedKeywords write SetSelectedKeywords;
  end;

   //===================================================================================================================
   // IPhotoAlbumPicList - сортированный по ID список изображений фотоальбома
   //===================================================================================================================

  IPhotoAlbumProject = interface;

  IPhotoAlbumPicList = interface(IPhoaMutablePicList)
    ['{AE945E5F-9BF1-4FD0-92C9-92716D7BB632}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Создаёт полные копии изображений с Src (объекты, которыми владеет только сам список)
    procedure DuplicatePics(PicList: IPhoaPicList);
     // Prop handlers
    function  GetItemsByIDX(iID: Integer): IPhotoAlbumPic;
    function  GetItemsByFileNameX(const wsFileName: WideString): IPhotoAlbumPic;
    function  GetItemsX(Index: Integer): IPhotoAlbumPic;
     // Props
     // -- 'Native' version of ItemsByID[]
    property ItemsByIDX[iID: Integer]: IPhotoAlbumPic read GetItemsByIDX;
     // -- 'Native' version of ItemsByFileName[]
    property ItemsByFileNameX[const wsFileName: WideString]: IPhotoAlbumPic read GetItemsByFileNameX;
     // -- 'Native' version of Items[]
    property ItemsX[Index: Integer]: IPhotoAlbumPic read GetItemsX; default;
  end;

   //===================================================================================================================
   // IPhotoAlbumPicGroup - группа изображений фотоальбома
   //===================================================================================================================

  IPhotoAlbumPicGroupList = interface;

  PPhotoAlbumPicGroup = ^IPhotoAlbumPicGroup;
  IPhotoAlbumPicGroup = interface(IPhoaMutablePicGroup)
    ['{9C951B51-2C66-4C35-B61B-8EDCBEAD8AC0}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Вызывается после загрузки иерархии групп из Streamer-а
    procedure Loaded(Project: IPhotoAlbumProject);
     // Рекурсивно просматривает группу и все подгруппы, назначая ID группам, его не имеющим
    procedure FixupIDs;
     // Рекурсивная процедура, назначающая ID группам, имеющим ID=0
    procedure InternalFixupIDs(var iMaxGroupID: Integer);
     // Prop handlers
    function  GetGroupByIDX(iID: Integer): IPhotoAlbumPicGroup;
    function  GetGroupByPathX(const wsPath: WideString): IPhotoAlbumPicGroup;
    function  GetGroupsX: IPhotoAlbumPicGroupList;
    function  GetOwnerX: IPhotoAlbumPicGroup;
    function  GetPicsX: IPhotoAlbumPicList;
    function  GetRootX: IPhotoAlbumPicGroup;
     // Props
     // -- 'Native' version of Groups
    property GroupsX: IPhotoAlbumPicGroupList read GetGroupsX;
     // -- 'Native' version of GroupByID[]
    property GroupByIDX[iID: Integer]: IPhotoAlbumPicGroup read GetGroupByIDX;
     // -- 'Native' version of GroupByPath[]
    property GroupByPathX[const wsPath: WideString]: IPhotoAlbumPicGroup read GetGroupByPathX;
     // -- 'Native' version of Owner
    property OwnerX: IPhotoAlbumPicGroup read GetOwnerX;
     // -- 'Native' version of Pics
    property PicsX: IPhotoAlbumPicList read GetPicsX;
     // -- 'Native' version of Root
    property RootX: IPhotoAlbumPicGroup read GetRootX;
  end;

   //===================================================================================================================
   // IPhotoAlbumPicGroupList - список групп изображений фотоальбома
   //===================================================================================================================

  IPhotoAlbumPicGroupList = interface(IPhoaMutablePicGroupList)
    ['{5B299022-5911-4154-8307-37170FDD7952}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Prop handlers
    function  GetItemsX(Index: Integer): IPhotoAlbumPicGroup;
    function  GetOwnerX: IPhotoAlbumPicGroup;
     // Props
     // -- 'Native' version of Items[]
    property ItemsX[Index: Integer]: IPhotoAlbumPicGroup read GetItemsX; default;
     // -- 'Native' version of Owner
    property OwnerX: IPhotoAlbumPicGroup read GetOwnerX;
  end;

   //===================================================================================================================
   // IPhotoAlbumPicSorting - сортировка изображений
   //===================================================================================================================

  IPhotoAlbumPicSorting = interface(IPhoaMutablePicSorting)
    ['{6010D0DF-0EA5-4461-96DC-956131E4BD34}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  end;
  
   //===================================================================================================================
   // IPhotoAlbumPicSortingList - список сортировок изображений
   //===================================================================================================================

  IPhotoAlbumPicSortingList = interface(IPhoaMutablePicSortingList)
    ['{6010D0DF-0EA5-4461-96DC-956131E4BD35}']
     // Сохранение/загрузка из реестра
    procedure RegSave(const sRoot, sSection: AnsiString);
    procedure RegLoad(const sRoot, sSection: AnsiString);
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Сбрасывает список сортировок в состояние по умолчанию
    procedure RevertToDefaults;
     // Prop handlers
    function  GetItemsX(Index: Integer): IPhotoAlbumPicSorting;
     // Props
     // -- 'Native' version of Items[]
    property ItemsX[Index: Integer]: IPhotoAlbumPicSorting read GetItemsX; default;
  end;

   //===================================================================================================================
   // IPhotoAlbumPicGrouping - группировка изображений фотоальбома
   //===================================================================================================================

  IPhotoAlbumPicGrouping = interface(IPhoaMutablePicGrouping)
    ['{BADBDBE4-C412-4CD6-94F9-3AAEB0102D90}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  end;

   //===================================================================================================================
   // IPhotoAlbumPicGroupingList - список группировок изображений фотоальбома
   //===================================================================================================================

  IPhotoAlbumPicGroupingList = interface(IPhoaMutablePicGroupingList)
    ['{BADBDBE4-C412-4CD6-94F9-3AAEB0102D91}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Prop handlers
    function  GetItemsX(Index: Integer): IPhotoAlbumPicGrouping;
     // Props
     // -- 'Native' version of Items[]
    property ItemsX[Index: Integer]: IPhotoAlbumPicGrouping read GetItemsX; default;
  end;

   //===================================================================================================================
   // IPhotoAlbumView - представление фотоальбома
   //===================================================================================================================

  IPhotoAlbumViewList = interface;

  IPhotoAlbumView = interface(IPhoaMutableView)
    ['{54AF158C-1917-47F8-ABBD-AFDB4C5E64B7}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Prop handlers
    function  GetGroupingsX: IPhotoAlbumPicGroupingList;
    function  GetListX: IPhotoAlbumViewList;
    function  GetRootGroupX: IPhotoAlbumPicGroup;
    function  GetSortingsX: IPhotoAlbumPicSortingList;
     // Props
     // -- 'Native' version of Groupings
    property GroupingsX: IPhotoAlbumPicGroupingList read GetGroupingsX;
     // -- 'Native' version of List
    property ListX: IPhotoAlbumViewList read GetListX;
     // -- 'Native' version of RootGroup
    property RootGroupX: IPhotoAlbumPicGroup read GetRootGroupX;
     // -- 'Native' version of Sortings
    property SortingsX: IPhotoAlbumPicSortingList read GetSortingsX;
  end;

   //===================================================================================================================
   // IPhotoAlbumViewList - список представлений фотоальбома
   //===================================================================================================================

  IPhotoAlbumViewList = interface(IPhoaMutableViewList)
    ['{54AF158C-1917-47F8-ABBD-AFDB4C5E64B8}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Prop handlers
    function  GetItemsX(Index: Integer): IPhotoAlbumView;
    function  GetPicsX: IPhotoAlbumPicList;
     // Props
     // -- 'Native' version of Items[]
    property ItemsX[Index: Integer]: IPhotoAlbumView read GetItemsX; default;
     // -- 'Native' version of Pics
    property PicsX: IPhotoAlbumPicList read GetPicsX;
  end;

   //===================================================================================================================
   // IPhotoAlbumProject - проект PhoA
   //===================================================================================================================

  IPhotoAlbumProject = interface(IPhoaMutableProject)
    ['{769DBE0B-D86B-4F89-A557-9A8DA083E508}']
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer; const wsGenerator, wsRemark: WideString);
     // Prop handlers
    function  GetCurrentViewX: IPhotoAlbumView;
    function  GetPicsX: IPhotoAlbumPicList;
    function  GetRootGroupX: IPhotoAlbumPicGroup;
    function  GetViewRootGroupX: IPhotoAlbumPicGroup;
    function  GetViewsX: IPhotoAlbumViewList;
     // Props
     // -- 'Native' version of CurrentView
    property CurrentViewX: IPhotoAlbumView read GetCurrentViewX;
     // -- 'Native' version of Pics
    property PicsX: IPhotoAlbumPicList read GetPicsX;
     // -- 'Native' version of RootGroup
    property RootGroupX: IPhotoAlbumPicGroup read GetRootGroupX;
     // -- 'Native' version of ViewRootGroup
    property ViewRootGroupX: IPhotoAlbumPicGroup read GetViewRootGroupX;
     // -- 'Native' version of Views
    property ViewsX: IPhotoAlbumViewList read GetViewsX;
  end;

   //===================================================================================================================
   // IPhotoAlbumApp - интерфейс самого приложения
   //===================================================================================================================

  IPhotoAlbumApp = interface(IPhoaMutableApp)
    ['{328D859C-8CDA-494B-B5E8-6AF9AB5E51FD}']
     // Выполняет заданную операцию с заданными параметрами
    procedure PerformOperation(const wsOpName: WideString; const aParams: Array of Variant);
     // Prop handlers
    function  GetCurGroupX: IPhotoAlbumPicGroup;
    function  GetImageList: TCustomImageList;
    function  GetProjectX: IPhotoAlbumProject;
    function  GetSelectedPicsX: IPhotoAlbumPicList;
    function  GetViewedPicsX: IPhotoAlbumPicList;
    procedure SetCurGroupX(Value: IPhotoAlbumPicGroup);
     // Props
     // -- 'Native' version of CurGroup
    property CurGroupX: IPhotoAlbumPicGroup read GetCurGroupX write SetCurGroupX;
     // -- Стандартный ImageList приложения
    property ImageList: TCustomImageList read GetImageList;
     // -- 'Native' version of Project
    property ProjectX: IPhotoAlbumProject read GetProjectX;
     // -- 'Native' version of SelectedPics
    property SelectedPicsX: IPhotoAlbumPicList read GetSelectedPicsX;
     // -- 'Native' version of ViewedPics
    property ViewedPicsX: IPhotoAlbumPicList read GetViewedPicsX;
  end;

   //===================================================================================================================
   // IPhoaDataStream - интерфейс потока данных
   //===================================================================================================================

  IPhoaDataStream = interface(IInterface)
    ['{2C2E5BCD-F397-4824-B2EC-09F23D47334F}']
     // Стирает содержимое потока
    procedure Clear;
     // Методы для записи данных в поток
    procedure WriteRaw (const Data: TPhoaRawData);
    procedure WriteWStr(const ws: WideString);
    procedure WriteInt (i: Integer);
    procedure WriteByte(b: Byte);
    procedure WriteBool(b: Boolean);
     // Методы для чтения данных из потока
    function  ReadRaw:  TPhoaRawData;
    function  ReadWStr: WideString;
    function  ReadInt:  Integer;
    function  ReadByte: Byte;
    function  ReadBool: Boolean;
     // Prop handlers
    function  GetPosition: Int64; 
     // Props
     // -- Текущее положение в потоке данных
    property Position: Int64 read GetPosition;
  end;

   //===================================================================================================================
   // IPhoaUndoDataStream - интерфейс потока данных отката
   //===================================================================================================================

  IPhoaUndoDataStream = interface(IPhoaDataStream)
    ['{71515E8A-42FC-4763-8EE2-797B0170E497}']
     // Процедуры начала/окончания процесса считывания данных отката. BeginUndo позиционирует файл в заданную позицию,
     //   и, если это первый вызов BeginUndo, запоминает эту позицию. EndUndo уменьшает счётчик вложенных считываний,
     //   и, если это последний вызов EndUndo и bTruncate=True, усекает файл по запомненной в первом вызове BeginUndo
     //   позиции
    procedure BeginUndo(i64Position: Int64);
    procedure EndUndo(bTruncate: Boolean);
     // Prop handlers
    function  GetFileName: WideString;
     // Props
     // -- Имя файла данных (временного)
    property FileName: WideString read GetFileName;
  end;

implementation

end.
