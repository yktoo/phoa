//*****************************************************************************
//
// PhoA plugin system interface declarations
// The whole code (c)2002-2004 Dmitry Kann, except where otherwise explicitly
// noted
// Home sites:
//   http://devtools.narod.ru/
//   http://phoa.narod.ru/
//
// Contact email: phoa@narod.ru
//
// Target platform: Borland Delphi 7 (but may compile on earlier platforms)
// Target OS:       Windows
// Language:        Object Pascal
//
//*****************************************************************************
unit phIntf;

interface

   //-------------------------------------------------------------------------------------------------------------------
   // List of links to the pictures
   //-------------------------------------------------------------------------------------------------------------------
type
  IPhoaPicLinks = interface(IInterface)
    ['{11D5E0DC-1CC0-471D-83BF-693834372FDA}']
     // Adds a picture to the list. Returns the index of the newly-added item
    function Add(Pic: IPhoaPic): Integer; stdcall;
     // Копирует все ссылки на изображения с фотоальбома
    procedure CopyFromPhoa(PhoA: TPhotoAlbum);
     // Копирует все ссылки на изображения с группы. Если Group=nil, просто очищает список
    procedure CopyFromGroup(PhoA: TPhotoAlbum; Group: TPhoaGroup);
     // Копирует все ID изображений в группу
    procedure CopyToGroup(Group: TPhoaGroup);
     // Возвращает индекс изображения с заданным ID, или -1, если нет такого
    function IndexOfID(iID: Integer): Integer;
     // Props
    property Items[Index: Integer]: TPhoaPic read GetItems; default;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Список сортировок изображений
   //-------------------------------------------------------------------------------------------------------------------

   // Сортировка изображений (помещается в 4 байта)
  TPhoaSorting = packed record
    Prop:    TPicProperty; // Свойство для сортировки
    Order:   TSortOrder;   // Направление сортировки
    wUnused: Word;
  end;

   // Список сортировок изображений
  TPhoaSortings = class(TList)
  private
    function  GetItems(Index: Integer): TPhoaSorting;
  public
    procedure Add(_Prop: TPicProperty; _Order: TSortOrder);
     // Ищет индекс сортировки по свойству. Если не найден, возвращает -1
    function  IndexOf(Prop: TPicProperty): Integer;
     // Сравнивает два изображения (для сортировки)
    function  SortComparePics(Pic1, Pic2: TPhoaPic): Integer;
     // Возвращает True, если содержимое списка идентично списку Sortings
    function  IdenticalWith(Sortings: TPhoaSortings): Boolean;
     // Переключает направление сортировки с индексом Index
    procedure ToggleOrder(Index: Integer);
     // Устанавливает свойство для сортировки у пункта с индексом Index
    procedure SetProp(Index: Integer; Prop: TPicProperty);
     // Сохранение/загрузка из реестра
    procedure RegSave(const sSection: String);
    procedure RegLoad(const sSection: String);
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Сбрасывает список сортировок в состояние по уиолчанию
    procedure RevertToDefaults;
     // Props
    property Items[Index: Integer]: TPhoaSorting read GetItems; default;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Группа (категория) изображений
   //-------------------------------------------------------------------------------------------------------------------

  PPhoaGroup = ^TPhoaGroup;
  TPhoaGroup = class(TObject)
  private
     // Prop storage
    FExpanded: Boolean;
    FText: String;
    FGroups: TPhoaGroups;
    FOwner: TPhoaGroup;
    FPicIDs: TIntegerList;
     // Возвращает абсолютный индекс группы Group относительно себя и наоборот
    function  GetAbsoluteIndexOf(Group: TPhoaGroup): Integer;
    function  GetGroupByAbsoluteIndex(AbsIndex: Integer): TPhoaGroup;
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Prop handlers
    procedure SetOwner(Value: TPhoaGroup);
    function  GetIndex: Integer;
    procedure SetIndex(Value: Integer);
  public
    constructor Create(_Owner: TPhoaGroup);
    destructor Destroy; override;
     // Возвращает True, если ID изображения присутствует в списке группы (или любой её подгруппы при bRecursive=True)
    function  IsPicLinked(iID: Integer; bRecursive: Boolean): Boolean;
     // Копирует свойства группы (наименование, Expanded, список ID изображений и иерархию групп)
    procedure Assign(gSource: TPhoaGroup);
     // Props
     // -- True, если соответствующий группе узел дерева развёрнут
    property Expanded: Boolean read FExpanded write FExpanded;
     // -- Список групп, входящих в данную группу
    property Groups: TPhoaGroups read FGroups;
     // -- Индекс группы в её владельце (Owner)
    property Index: Integer read GetIndex write SetIndex;
     // -- Группа-владелец данной группы
    property Owner: TPhoaGroup read FOwner write SetOwner;
     // -- Список ID изображений, входящих в группу
    property PicIDs: TIntegerList read FPicIDs; 
     // -- Текст (наименование) группы
    property Text: String read FText write FText;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Список групп изображений
   //-------------------------------------------------------------------------------------------------------------------

  TPhoaGroups = class(TList)
  private
    FOwner: TPhoaGroup;
    function GetItems(Index: Integer): TPhoaGroup;
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  public
    constructor Create(_Owner: TPhoaGroup);
    function  Add(Group: TPhoaGroup): Integer;
    procedure Delete(Index: Integer);
    procedure Clear; override;
    property  Items[Index: Integer]: TPhoaGroup read GetItems; default;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Изображение
   //-------------------------------------------------------------------------------------------------------------------

  TPhoaPic = class(TObject)
  private
     // Фотоальбом. Должен использоваться только для получения размеров и качества эскизов
    FPhoA: TPhotoAlbum;
     // Prop storage
    FList: TPhoaPics;
    FID: Integer;
    FPicAuthor: String;
    FPicDateTime: TDateTime;
    FPicDesc: String;
    FPicFileName: String;
    FPicFileSize: Integer;
    FPicFilmNumber: String;
    FPicFormat: TPixelFormat;
    FPicFrameNumber: String;
    FPicKeywords: TStrings;
    FPicNotes: String;
    FPicPlace: String;
    FThumbHeight: Integer;
    FThumbnailData: String;
    FThumbWidth: Integer;
    FPicHeight: Integer;
    FPicWidth: Integer;
    FPicMedia: String;
     // Загрузка/сохранение с помощью Streamer
     //   -- Параметр bEx...Relative контролирует, осуществлять ли преобразование относительного <-> абсолютного пути
     //      к файлу изображения
     //   -- Параметр PProps указывает, какие свойства сохранять и восстанавливать (при этом все внутренние данные,
     //      связанные с изображением, т.е. с файлом, сохраняются только при наличии ppFileName in PProps)
    procedure StreamerLoad(Streamer: TPhoaStreamer; bExpandRelative: Boolean; PProps: TPicProperties);
    procedure StreamerSave(Streamer: TPhoaStreamer; bExtractRelative: Boolean; PProps: TPicProperties);
     // Prop handlers
    procedure SetList(Value: TPhoaPics);
    function  GetRawData(PProps: TPicProperties): String;
    procedure SetRawData(PProps: TPicProperties; const Value: String);
    function  GetProps(PicProp: TPicProperty): String;
    procedure SetProps(PicProp: TPicProperty; const Value: String);
  public
    constructor Create(PhoA: TPhotoAlbum; List: TPhoaPics);
    destructor Destroy; override;
     // Копирует все данные изображения
    procedure Assign(Src: TPhoaPic);
     // Строит эскиз из файла и обновляет размер изображения и эскиза
    procedure MakeThumbnail;
     // Распределяет новый ID, уникальный в списке-владельце
    procedure IDNeeded;
     // Отрисовывает эскиз на битмэпе
    procedure PaintThumbnail(Bitmap: TBitmap);
     // Составляет описание изображения из свойств Props, выбирая только указанные данные.
     //   Если задано sNameValSep, то выводит также наименование свойств, разделяя имя от значения этой строкой.
     //   sPropSep - разделительная строка между отдельными свойствами
    function  GetPropStrs(Props: TPicProperties; const sNameValSep, sPropSep: String): String;
     // Сбрасывает указанные свойства в их значения по умолчанию
    procedure CleanupProps(Props: TPicProperties);
     // Props
     // -- Уникальный идентификатор
    property ID: Integer read FID;
     // -- Список - владелец изображения (может быть nil)
    property List: TPhoaPics read FList write SetList;
     // -- Автор изображения
    property PicAuthor: String read FPicAuthor write FPicAuthor;
     // -- Дата и время изображения
    property PicDateTime: TDateTime read FPicDateTime write FPicDateTime;
     // -- Описание изображения (для отображения)
    property PicDesc: String read FPicDesc write FPicDesc;
     // -- Имя файла изображения
    property PicFileName: String read FPicFileName write FPicFileName;
     // -- Размер файла изображения
    property PicFileSize: Integer read FPicFileSize write FPicFileSize;
     // -- Номер или название плёнки
    property PicFilmNumber: String read FPicFilmNumber write FPicFilmNumber;
     // -- Формат файла изображения
    property PicFormat: TPixelFormat read FPicFormat;
     // -- Номер кадра
    property PicFrameNumber: String read FPicFrameNumber write FPicFrameNumber;
     // -- Высота изображения в пикселах
    property PicHeight: Integer read FPicHeight write FPicHeight;
     // -- Список ключевых слов
    property PicKeywords: TStrings read FPicKeywords;
     // -- Носитель с файлом изображения
    property PicMedia: String read FPicMedia write FPicMedia;
     // -- Примечания
    property PicNotes: String read FPicNotes write FPicNotes;
     // -- Место изображения
    property PicPlace: String read FPicPlace write FPicPlace;
     // -- Ширина изображения в пикселах
    property PicWidth: Integer read FPicWidth write FPicWidth;
     // -- Свойства по индексу
    property Props[PicProp: TPicProperty]: String read GetProps write SetProps;
     // -- Бинарные данные изображения (свойства, указанные в PProps)
    property RawData[PProps: TPicProperties]: String read GetRawData write SetRawData;
     // -- Реальная высота эскиза
    property ThumbHeight: Integer read FThumbHeight;
     // -- Бинарные JPEG-данные эскиза
    property ThumbnailData: String read FThumbnailData;
     // -- Реальная ширина эскиза
    property ThumbWidth: Integer read FThumbWidth;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Список изображений, в котором изображения регистрируются и которому принадлежат
   //-------------------------------------------------------------------------------------------------------------------

  TPhoaPics = class(TList)
  private
    FPhoA: TPhotoAlbum;
    function GetItems(Index: Integer): TPhoaPic;
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  public
    constructor Create(_PhoA: TPhotoAlbum);
    function  Add(Pic: TPhoaPic): Integer;
    procedure Delete(Index: Integer);
    procedure Clear; override;
     // Возвращает следующий свободный ID изображения
    function GetFreePicID: Integer;
     // Возвращает изображение по его ID (nil, если не найдено)
    function PicByID(_ID: Integer): TPhoaPic;
     // Возвращает изображение по его имени файла (nil, если не найдено)
    function PicByFileName(const sFileName: String): TPhoaPic;
     // Props
    property Items[Index: Integer]: TPhoaPic read GetItems; default;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Представление
   //-------------------------------------------------------------------------------------------------------------------

  TPhoaView = class;

   // Вспомогательный список, хранящий ссылки на изображения фотоальбома
  TPhoaViewHelperPics = class(TPhoaPicLinks)
  public
     // Сортирует изображения для представления (для последующей группировки)
    procedure Sort(View: TPhoaView);
  end;

   // Группировка изображений (помещается в 4 байта)
  TPhoaGrouping = packed record
    Prop: TGroupByProperty; // Свойство для группировки
    bUnclassified: Boolean; // Помещать ли неклассифицированные изображения в отдельную группу
    wUnused: Word;
  end;

   // Нетипизированная группировка (из простых чисел)
  TRawPhoaGrouping = packed record
    bProp: Byte;
    bUnclassified: Byte;
    wUnused: Word;
  end;

   // Список группировок изображений
  TPhoaGroupings = class(TList)
  private
    function  GetItems(Index: Integer): TPhoaGrouping;
  public
    procedure Add(_Prop: TGroupByProperty; _bUnclassified: Boolean);
     // Возвращает True, если содержимое списка идентично списку Groupings
    function  IdenticalWith(Groupings: TPhoaGroupings): Boolean;
     // Сравнивает два изображения (для сортировки)
    function  SortComparePics(Pic1, Pic2: TPhoaPic): Integer;
     // Переключает bUnclassified группировки с индексом Index
    procedure ToggleUnclassified(Index: Integer);
     // Устанавливает свойство для группировки у пункта с индексом Index
    procedure SetProp(Index: Integer; Prop: TGroupByProperty);
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Props
    property Items[Index: Integer]: TPhoaGrouping read GetItems; default;
  end;

  TPhoaView = class(TObject)
  private
     // Список-владелец представления
    FList: TPhoaViews;
     // Prop storage
    FRootGroup: TPhoaGroup;
    FName: String;
    FGroupings: TPhoaGroupings;
    FSortings: TPhoaSortings;
     // Prop handlers
    function  GetRootGroup: TPhoaGroup;
    function  GetIndex: Integer;
  protected
     // Создание списка групп по критериям представления
    procedure ProcessGroups;
  public
    constructor Create(List: TPhoaViews);
    destructor Destroy; override;
     // Удаление списка групп по критериям представления
    procedure UnprocessGroups;
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Props
     // -- Индекс представления в списке-владельце
    property Index: Integer read GetIndex;
     // -- Наименование представления
    property Name: String read FName write FName;
     // -- Список группировок изображений
    property Groupings: TPhoaGroupings read FGroupings;
     // -- Корневая группа - строится по критериям представления автоматически при обращении
    property RootGroup: TPhoaGroup read GetRootGroup;
     // -- Список сортировок изображений
    property Sortings: TPhoaSortings read FSortings;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Список представлений
   //-------------------------------------------------------------------------------------------------------------------

  TPhoaViews = class(TList)
  private
    FPhoA: TPhotoAlbum;
    function GetItems(Index: Integer): TPhoaView;
  public
    constructor Create(PhoA: TPhotoAlbum);
    function  Add(View: TPhoaView): Integer;
    procedure Delete(Index: Integer);
    procedure Clear; override;
     // Сортирует представления по наименованию
    procedure Sort;
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Props
    property Items[Index: Integer]: TPhoaView read GetItems; default;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Фотоальбом
   //-------------------------------------------------------------------------------------------------------------------

  TPhoaOperations = class; 

  TPhotoAlbum = class(TObject)
  private
     // Экземпляр просмотрщика эскизов
    FViewer: TThumbnailViewer;
     // Prop storage
    FRootGroup: TPhoaGroup;
    FPics: TPhoaPics;
    FViews: TPhoaViews;
    FFileName: String;
    FDescription: String;
    FThumbnailQuality: Byte;
    FThumbnailWidth: Integer;
    FThumbnailHeight: Integer;
    FFileRevision: Integer;
     // Загрузка/сохранение с помощью Streamer
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
     // Prop handlers
    procedure SetFileName(const Value: String);
    procedure SetThumbnailHeight(Value: Integer);
    procedure SetThumbnailWidth(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
     // Очищает все внутренние поля фотоальбома
    procedure New(UndoOperations: TPhoaOperations);
     // Загрузка/сохранение в файл
     // -- Загружает фотоальбом из файла
    procedure FileLoad(const sFileName: String; UndoOperations: TPhoaOperations);
     // -- Записывает фотоальбом в текущий файл
    procedure FileSave(UndoOperations: TPhoaOperations);
     // -- Записывает фотоальбом в любой файл (с заданной ревизией iRevisionNumber)
    procedure FileSaveTo(const sFileName: String; iRevisionNumber: Integer; UndoOperations: TPhoaOperations);
     // Удаляет все изображения, на которые не ссылается ни одна из групп фотоальбома и помещает удаляемые изображения
     //   в UndoOperations
    procedure RemoveUnlinkedPics(UndoOperations: TPhoaOperations);
     // Props
     // -- Текст описания фотоальбома
    property Description: String read FDescription write FDescription;
     // -- Имя файла фотоальбома
    property FileName: String read FFileName write SetFileName;
     // -- Текущая ревизия файла фотоальбома
    property FileRevision: Integer read FFileRevision;
     // -- Список изображений фотоальбома
    property Pics: TPhoaPics read FPics;
     // -- Корневая (фиктивная) группа, владеющая всеми группами фотоальбома
    property RootGroup: TPhoaGroup read FRootGroup;
     // -- Высота эскиза в пикселах
    property ThumbnailHeight: Integer read FThumbnailHeight write SetThumbnailHeight;
     // -- Качество JPEG-эскиза (0..100)
    property ThumbnailQuality: Byte read FThumbnailQuality write FThumbnailQuality;
     // -- Ширина эскиза в пикселах
    property ThumbnailWidth: Integer read FThumbnailWidth write SetThumbnailWidth;
     // -- Список представлений фотоальбома
    property Views: TPhoaViews read FViews;
  end;

implementation

end.
