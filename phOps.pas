//**********************************************************************************************************************
//  $Id: phOps.pas,v 1.3 2004-10-13 14:29:09 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phOps;

interface
uses
  Windows, Messages, SysUtils, Classes, phObj, phPhoa, phIntf, phMutableIntf, phNativeIntf;

type  
   // Возможные операции с изображениями (доступные через пункт меню Сервис | Операции с изображениями)
  TPictureOperation = (
    popMoveToTarget,     // Переместить выделенные изображения в указанную группу
    popCopyToTarget,     // Копировать выделенные изображения в указанную группу
    popRemoveFromTarget, // Удалить выделенные изображения из указанной группы
    popIntersectTarget); // Оставить только выделенные изображения в указанной группе

   // Флаги форматов буфера обмена для данных изображений
  TPicClipboardFormat = (
    pcfPhoa,          // Внутренний формат данных программы (wClipbrdPicFormatID)
    pcfHDrop,         // Список Shell-объектов (т.е. файлов)
    pcfPlainList,     // Простой текстовый список путей к файлам
    pcfSingleBitmap); // Bitmap-изображение эскиза (в случае единственного изображения)
  TPicClipboardFormats = set of TPicClipboardFormat;

   //===================================================================================================================
   // Файл отката PhoA (организован по принципу стека)
   //===================================================================================================================
   // Формат файла:
   //    <данные1><тип1><данные2><тип2>...
   //    Позиция в потоке всегда сохраняется *за последним байтом потока*

   // Тип данных, сохраняемых в файле
  TPhoaUndoFileDatatype = (pufdStr, pufdInt, pufdByte, pufdBool);

  TPhoaUndoFile = class(TObject)
  private
     // Файловый поток данных отката
    FStream: TFileStream;
     // Счётчик вложенности вызовов BeginUndo/EndUndo
    FUndoCounter: Integer;
     // Положение, запомненное в первом вызове BeginUndo
    FUndoPosition: Int64;
     // Prop storage
    FFileName: String;
     // Создаёт поток, если он ещё не создан
    procedure CreateStream;
     // Записывает в поток тип данных
    procedure WriteDatatype(DT: TPhoaUndoFileDatatype);
     // Считывает из файла байт типа данных и проверяет его на соответствие DTRequired. Если не совпадает, вызывает
     //   Exception
    procedure ReadCheckDatatype(DTRequired: TPhoaUndoFileDatatype);
     // Prop handlers
    function  GetPosition: Int64;
  public
    constructor Create;
    destructor Destroy; override;
     // Уничтожает поток и файл 
    procedure Clear;
     // Методы для записи/чтения данных из файла
    procedure WriteStr (const s: String);
    procedure WriteInt (i: Integer);
    procedure WriteByte(b: Byte);
    procedure WriteBool(b: Boolean);
    function  ReadStr: String;
    function  ReadInt: Integer;
    function  ReadByte: Byte;
    function  ReadBool: Boolean;
     // Процедуры начала/окончания процесса считывания данных отката. BeginUndo позиционирует файл в заданную позицию,
     //   и, если это первый вызов BeginUndo, запоминает эту позицию. EndUndo уменьшает счётчик вложенных считываний,
     //   и, если это последний вызов EndUndo, усекает файл по запомненной в первом вызове BeginUndo позиции
    procedure BeginUndo(i64Position: Int64);
    procedure EndUndo;
     // Props
     // -- Имя файла данных (временного)
    property FileName: String read FFileName;
     // -- Текущее положение в потоке данных. Создаёт поток при первом обращении
    property Position: Int64 read GetPosition;
  end;

   // Базовая операция, не изменяющая состояния фотоальбома

  TBaseOperation = class(TObject)
  end;

  TPhoaOperations = class;

   // Флаги требуемого обновления операции отката
  TUndoInvalidationFlag  = (
     // -- Флаги действий при выполнении (eXecution)
    uifXReloadViews,         // Перегрузить список представлений (и обновить индекс текущего представления)
    uifXUpdateViewIndex,     // Обновить индекс текущего представления (мог измениться)
    uifXReinitParent,        // Переинициализировать родителя узла группы операции. Должно быть заполнено Op.ParentGroupAbsIdx
    uifXReinitSiblings,      // Переинициализировать соседние с узлом группы операции узлы дерева. Должно быть заполнено Op.ParentGroupAbsIdx
    uifXReinitRecursive,     // При переинициализации (флаги uifXReinitParent и uifXReinitSiblings) действовать рекурсивно
    uifXEditGroup,           // Ввести узел группы Op.GroupAbsIdx в режим редактирования его текста. Должно быть заполнено Op.GroupAbsIdx
    uifXUpdateThumbParams,   // Обновить параметры эскизов (могли измениться)
     // -- Флаги действий при откате (Undoing)
    uifUReloadViews,         // Перегрузить список представлений (и обновить индекс текущего представления)
    uifUUpdateViewIndex,     // Обновить индекс текущего представления
    uifUReinitAll,           // Переинициализировать все узлы дерева
    uifUReinitParent,        // Переинициализировать родителя узла группы операции. Должно быть заполнено Op.ParentGroupAbsIdx
    uifUReinitRecursive,     // При переинициализации (флаг uifUReinitParent) действовать рекурсивно
    uifUUpdateThumbParams);  // Обновить параметры эскизов (могли измениться)

  TUndoInvalidationFlags = set of TUndoInvalidationFlag;

   // Базовая (абстрактная) операция фотоальбома, принадлежащая списку (буферу отката), которая может быть отменена
  TPhoaOperation = class(TBaseOperation)
  private
     // Позиция данных отката операции в Undo-файле данных отката (UndoFile)
    FUndoDataPosition: Int64;
     // Prop storage
    FList: TPhoaOperations;
    FProject: IPhotoAlbumProject;
    FOpGroupID: Integer;
    FOpParentGroupID: Integer;
     // Prop handlers
    function  GetOpGroup: IPhotoAlbumPicGroup;
    function  GetParentOpGroup: IPhotoAlbumPicGroup;
    procedure SetOpGroup(Value: IPhotoAlbumPicGroup);
    procedure SetParentOpGroup(Value: IPhotoAlbumPicGroup);
    function  GetUndoFile: TPhoaUndoFile;
  protected
     // Prop storage
    FSavepoint: Boolean;
     // Prop handlers
    function  GetInvalidationFlags: TUndoInvalidationFlags; virtual;
     // Основная процедура отката изменений, внесённых операцией. В базовом классе не делает ничего
    procedure RollbackChanges; virtual;
     // Props
     // -- Возвращает группу, соответствующую GroupID
    property OpGroup: IPhotoAlbumPicGroup read GetOpGroup write SetOpGroup;
     // -- Возвращает группу, соответствующую ParentGroupID
    property OpParentGroup: IPhotoAlbumPicGroup read GetParentOpGroup write SetParentOpGroup;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
    destructor Destroy; override;
     // Процедура, откатывающая изменения, внесённые операцией (вызовом RollbackChanges()), и уничтожающая
     //   объект-операцию
    procedure Undo; 
     // Наименование операции
    function Name: String;
     // Props
     // -- Флаги требуемого обновления после отката операции
    property InvalidationFlags: TUndoInvalidationFlags read GetInvalidationFlags;
     // -- Список - владелец операции
    property List: TPhoaOperations read FList;
     // -- ID группы, которой касается операция (если касается)
    property OpGroupID: Integer read FOpGroupID;
     // -- ID родителя группы, которой касается операция (если касается)
    property OpParentGroupID: Integer read FOpParentGroupID;
     // -- Фотоальбом
    property Project: IPhotoAlbumProject read FProject;
     // -- Указывает, что после данной операции было произведено сохранение фотоальбома, т.е., если эта операция -
     //    последняя в буфере отката, то это указывает на unmodified-состояние фотоальбома
    property Savepoint: Boolean read FSavepoint;
     // -- Файл данных отката (получается через FList)
    property UndoFile: TPhoaUndoFile read GetUndoFile;
  end;

   // Список сделанных операций
  TPhoaOperations = class(TList)
  private
     // Счётчик блокировки
    FUpdateLock: Integer;
     // Prop storage
    FUndoFile: TPhoaUndoFile;
    FOnStatusChange: TNotifyEvent;
    FOnOpUndone: TNotifyEvent;
    FOnOpDone: TNotifyEvent;
     // Prop handlers
    function  GetItems(Index: Integer): TPhoaOperation;
    function  GetCanUndo: Boolean;
  protected
     // Вызывает OnStatusChange
    procedure DoStatusChange;
     // Откатывает весь буфер (предназначено для вторичных буферов множественных операций)
    procedure UndoAll;
  public
    constructor Create(AUndoFile: TPhoaUndoFile);
    function  Add(Item: TPhoaOperation): Integer;
    function  Remove(Item: TPhoaOperation): Integer;
    procedure Delete(Index: Integer);
    procedure Clear; override;
     // Установка/снятие блокировки
    procedure BeginUpdate;
    procedure EndUpdate;
     // Props
     // -- Возвращает True, если в списке есть операции для отмены
    property CanUndo: Boolean read GetCanUndo;
     // -- Индексированный список операций
    property Items[Index: Integer]: TPhoaOperation read GetItems; default;
     // -- Файл данных отката
    property UndoFile: TPhoaUndoFile read FUndoFile;
     // -- Событие, возникающее при выполнении операции (точнее, при регистрации операции в списке)
    property OnOpDone: TNotifyEvent read FOnOpDone write FOnOpDone;
     // -- Событие, возникающее при откате операции
    property OnOpUndone: TNotifyEvent read FOnOpUndone write FOnOpUndone;
     // -- Событие смены состояния (содержимого списка - вызывается при добавлении/удалении операции, или изменения SavePoint)
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
  end;

   // Буфер отката PhoA. Является только *самостоятельным* объектом и обладает собственным файлом отката
  TPhoaUndo = class(TPhoaOperations)
  private
     // True, если "пустое" состояние буфера отката соответствует сохранённому состоянию фотоальбома
    FSavepointOnEmpty: Boolean;
     // Prop handlers
    function  GetLastOpName: String;
    function  GetIsUnmodified: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
     // Устанавливает, что текущее состояние фотоальбома является сохранённым
    procedure SetSavepoint;
     // Устанавливает, что текущее состояние является модифицированным, но откат невомозжен
    procedure SetNonUndoable;
     // Props
     // -- Возвращает True, если текущее состояние буфера отката соответствует сохранённому состоянию фотоальбома
    property IsUnmodified: Boolean read GetIsUnmodified;
     // -- Возвращает наименование последней сделанной операции
    property LastOpName: String read GetLastOpName;
  end;

   // Абстрактная операция, состоящая из нескольких операций. При отмене откатывает все операции оптом
  TPhoaMultiOp = class(TPhoaOperation)
  protected
     // Отдельные операции
    FOperations: TPhoaOperations;
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
    destructor Destroy; override;
     // Props
    property Operations: TPhoaOperations read FOperations;
  end;

   /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   // Реальные операции
   /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

   //===================================================================================================================
   // Операция создания группы ребёнком в текущей группе (CurGroup)
   //===================================================================================================================

  TPhoaOp_GroupNew = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; CurGroup: IPhotoAlbumPicGroup);
  end;

   //===================================================================================================================
   // Операция переименования группы
   //===================================================================================================================

  TPhoaOp_GroupRename = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; const sNewText: String);
  end;

   //===================================================================================================================
   // Операция редактирования свойств группы
   //===================================================================================================================

  TPhoaOp_GroupEdit = class(TPhoaOp_GroupRename)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; const sNewText, sNewDescription: String);
  end;

   //===================================================================================================================
   // Операция удаления группы
   //===================================================================================================================

  TPhoaOp_GroupDelete = class(TPhoaOperation)
  private
     // Список удалений зависимых узлов
    FCascadedDeletes: TPhoaOperations;
     // Операция удаления неиспользуемых изображений
    FUnlinkedPicsRemove: TPhoaOperation;
     // Список ID изображений группы
    FPicIDs: TIntegerList;
     // Внутренняя (оптимизированная на использование заранее известного Owner-а) процедура отката
    procedure InternalRollback(gOwner: IPhotoAlbumPicGroup);
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
     // Параметр bPerform контролирует, выполнять ли удаление (а также поиск неиспользуемых изображений). Должен быть
     //   True при вызове для изначальной операции удаления группы. Для вложенных групп конструктор вызывается уже с
     //   параметром False (чтобы удаление и сканирование неиспользуемых изображений происходило только единожды, после
     //   сохранения всей структуры удаляемых групп)
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; bPerform: Boolean);
    destructor Destroy; override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления неиспользуемых изображений из проекта
   //===================================================================================================================

  TPhoaOp_InternalUnlinkedPicsRemoving = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
  end;

   //===================================================================================================================
   // Комплексная операция редактирования изображений, служит контейнером для операций:
   //  - TPhoaOp_InternalEditPicProps
   //  - TPhoaOp_InternalEditPicKeywords
   //  - TPhoaOp_InternalPicFromGroupRemoving
   //  - TPhoaOp_InternalPicToGroupAdding
   //===================================================================================================================

  TPhoaMultiOp_PicEdit = class(TPhoaMultiOp)
  end;

   //===================================================================================================================
   // Список [требуемых] изменений свойств изображений
   //===================================================================================================================

  PPicPropertyChange = ^TPicPropertyChange;
  TPicPropertyChange = record
    sNewValue: String;
    Prop: TPicProperty;
  end;

  TPicPropertyChanges = class(TList)
  private
     // Prop handlers
    function  GetItems(Index: Integer): PPicPropertyChange;
    function  GetChangedProps: TPicProperties;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function  Add(const sNewValue: String; Prop: TPicProperty): Integer;
     // Props
     // -- Набор изменяющихся свойств
    property ChangedProps: TPicProperties read GetChangedProps;
     // -- Элементы списка по индексу
    property Items[Index: Integer]: PPicPropertyChange read GetItems; default;
  end;

   //===================================================================================================================
   // Внутренняя операция редактирования свойств изображения, кроме ключевых слов
   //===================================================================================================================

  TPhoaOp_InternalEditPicProps = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Pics: IPhotoAlbumPicList; ChangeList: TPicPropertyChanges);
  end;

   //===================================================================================================================
   // Внутренняя операция редактирования ключевых слов изображения
   //===================================================================================================================

  TPhoaOp_InternalEditPicKeywords = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Pics: IPhotoAlbumPicList; Keywords: TKeywordList);
  end;

   //===================================================================================================================
   // Операция применения преобразований изображения
   //===================================================================================================================

  TPhoaOp_StoreTransform = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Pic: IPhoaMutablePic; NewRotation: TPicRotation; NewFlips: TPicFlips);
  end;

   //===================================================================================================================
   // Операция добавления нескольких изображений (используется как контейнер для операций TPhoaOp_InternalPicAdd)
   //===================================================================================================================

  TPhoaMultiOp_PicAdd = class(TPhoaMultiOp)
  end;

   //===================================================================================================================
   // Внутренняя операция добавления изображения (используется как часть TPhoaMultiOp_PicAdd и TPhoaMultiOp_PicPaste)
   //===================================================================================================================

  TPhoaOp_InternalPicAdd = class(TPhoaOperation)
  private
     // True, если файл изображения уже был зарегистрирован в фотоальбоме до добавления изображения
    FExisting: Boolean;
     // Регистрирует изображение в группе, если его там не было, и запоминает данные отката
    procedure RegisterPic(Group: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; const sFilename: String; out AddedPic: IPhotoAlbumPic); overload;
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic); overload;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления изображений (по списку их ID) из группы
   //===================================================================================================================

  TPhoaOp_InternalPicFromGroupRemoving = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList);
  end;

   //===================================================================================================================
   // Внутренняя операция добавления изображений (по списку их ID) в группу
   //===================================================================================================================

  TPhoaOp_InternalPicToGroupAdding = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList);
  end;

   //===================================================================================================================
   // Операция копирования в буфер обмена нескольких изображений
   //===================================================================================================================

  TPhoaBaseOp_PicCopy = class(TBaseOperation)
    constructor Create(Pics: IPhotoAlbumPicList; ClipFormats: TPicClipboardFormats);
  end;

   //===================================================================================================================
   // Операция удаления/вырезания в буфер обмена нескольких изображений
   //===================================================================================================================

  TPhoaMultiOp_PicDelete = class(TPhoaMultiOp)
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList);
  end;

   //===================================================================================================================
   // Операция вставки нескольких изображений из буфера обмена
   //===================================================================================================================

  TPhoaMultiOp_PicPaste = class(TPhoaMultiOp)
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup);
  end;

   //===================================================================================================================
   // Операция редактирования свойств фотоальбома
   //===================================================================================================================

  TPhoaOp_PhoAEdit = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; const NewThSize: TSize; bNewThQuality: Byte; const sNewDescription: String);
  end;

   //===================================================================================================================
   // Операция [меж]групповой операции с изображениями
   //===================================================================================================================

  TPhoaMultiOp_PicOperation = class(TPhoaMultiOp)
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; SourceGroup, TargetGroup: IPhotoAlbumPicGroup; Pics: IPhoaPicList; PicOperation: TPictureOperation);
  end;

   //===================================================================================================================
   // Внутренняя операция сортировки изображений в одной группе
   //===================================================================================================================

  TPhoaOp_InternalGroupPicSort = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList);
  end;

   //===================================================================================================================
   // Операция сортировки изображений
   //===================================================================================================================

  TPhoaMultiOp_PicSort = class(TPhoaMultiOp)
  private
     // Рекурсивная (при bRecursive=True) процедура, создающая операции сортировки группы
    procedure AddGroupSortOp(Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean);
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean);
  end;

   //===================================================================================================================
   // Операция перетаскивания группы
   //===================================================================================================================

  TPhoaOp_GroupDragAndDrop = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group, NewParentGroup: IPhotoAlbumPicGroup; iNewIndex: Integer);
  end;

   //===================================================================================================================
   // Операция перетаскивания изображений в группу
   //===================================================================================================================

  TPhoaMultiOp_PicDragAndDropToGroup = class(TPhoaMultiOp)
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; SourceGroup, TargetGroup: IPhotoAlbumPicGroup; Pics: IPhoaPicList; bCopy: Boolean);
  end;

   //===================================================================================================================
   // Операция перетаскивания (переупорядочивания) изображений внутри группы
   //===================================================================================================================

  TPhoaOp_PicDragAndDropInsideGroup = class(TPhoaOperation)
  protected
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList; idxNew: Integer);
  end;

   //===================================================================================================================
   // Операция создания представления
   //===================================================================================================================

  TPhoaOp_ViewNew = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; const sName: String; Groupings: IPhotoAlbumPicGroupingList; Sortings: IPhotoAlbumPicSortingList);
  end;

   //===================================================================================================================
   // Операция изменения представления
   //===================================================================================================================

  TPhoaOp_ViewEdit = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
     // Если NewGroupings=nil и NewSortings=nil, значит, это просто переименование представления
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; View: IPhotoAlbumView; const sNewName: String; NewGroupings: IPhotoAlbumPicGroupingList; NewSortings: IPhotoAlbumPicSortingList);
  end;

   //===================================================================================================================
   // Операция удаления представления
   //===================================================================================================================

  TPhoaOp_ViewDelete = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
  end;

   //===================================================================================================================
   // Операция создания группы фотоальбома из представления
   //===================================================================================================================

  TPhoaOp_ViewMakeGroup = class(TPhoaOperation)
  protected
    function  GetInvalidationFlags: TUndoInvalidationFlags; override;
    procedure RollbackChanges; override;
  public
     // Group - группа, куда помещать папки представления
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup);
  end;

   // Запись/чтение содержимого TPhoaGroupings в/из Undo-файла
  procedure UndoWriteGroupings(UndoFile: TPhoaUndoFile; Groupings: IPhotoAlbumPicGroupingList);
  procedure UndoReadGroupings(UndoFile: TPhoaUndoFile; Groupings: IPhotoAlbumPicGroupingList);
   // Запись/чтение содержимого TPhoaGroupings в/из Undo-файла
  procedure UndoWriteSortings(UndoFile: TPhoaUndoFile; Sortings: IPhotoAlbumPicSortingList);
  procedure UndoReadSortings(UndoFile: TPhoaUndoFile; Sortings: IPhotoAlbumPicSortingList);

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses
  TypInfo, Clipbrd,
  VirtualDataObject, GR32,
  phUtils, phGraphics, ConsVars, phSettings;

  procedure UndoWriteGroupings(UndoFile: TPhoaUndoFile; Groupings: IPhotoAlbumPicGroupingList);
  var
    i: Integer;
    Grouping: IPhotoAlbumPicGrouping;
  begin
    UndoFile.WriteInt(Groupings.Count);
    for i := 0 to Groupings.Count-1 do begin
      Grouping := Groupings[i];
      UndoFile.WriteByte(Byte(Grouping.Prop));
      UndoFile.WriteBool(Grouping.UnclassifiedInOwnFolder);
    end;
  end;

  procedure UndoReadGroupings(UndoFile: TPhoaUndoFile; Groupings: IPhotoAlbumPicGroupingList);
  var
    i: Integer;
    Grouping: IPhotoAlbumPicGrouping;
  begin
    Groupings.Clear;
    for i := 0 to UndoFile.ReadInt-1 do begin
      Grouping := NewPhotoAlbumPicGrouping;
      Grouping.Prop                    := TPicGroupByProperty(UndoFile.ReadByte);
      Grouping.UnclassifiedInOwnFolder := UndoFile.ReadBool;
      Groupings.Add(Grouping);
    end;
  end;

  procedure UndoWriteSortings(UndoFile: TPhoaUndoFile; Sortings: IPhotoAlbumPicSortingList);
  var
    i: Integer;
    Sorting: IPhotoAlbumPicSorting;
  begin
    UndoFile.WriteInt(Sortings.Count);
    for i := 0 to Sortings.Count-1 do begin
      Sorting := Sortings[i];
      UndoFile.WriteByte(Byte(Sorting.Prop));
      UndoFile.WriteByte(Byte(Sorting.Direction));
    end;
  end;

  procedure UndoReadSortings(UndoFile: TPhoaUndoFile; Sortings: IPhotoAlbumPicSortingList);
  var
    i: Integer;
    Sorting: IPhotoAlbumPicSorting;
  begin
    Sortings.Clear;
    for i := 0 to UndoFile.ReadInt-1 do begin
      Sorting := NewPhotoAlbumPicSorting;
      Sorting.Prop      := TPicProperty(UndoFile.ReadByte);
      Sorting.Direction := TPhoaSortDirection(UndoFile.ReadByte);
      Sortings.Add(Sorting);
    end;
  end;

   //===================================================================================================================
   // TPhoaUndoFile
   //===================================================================================================================

  procedure TPhoaUndoFile.BeginUndo(i64Position: Int64);
  begin
    if FUndoCounter=0 then FUndoPosition := i64Position;
    FStream.Position := i64Position;
    Inc(FUndoCounter);
  end;

  procedure TPhoaUndoFile.Clear;
  begin
    if FStream<>nil then begin
      FreeAndNil(FStream);
      SysUtils.DeleteFile(FFileName);
    end;
  end;

  constructor TPhoaUndoFile.Create;
  begin
    inherited Create;
     // Определяем имя файла
    FFileName := Format('%sphoa_undo_%.8x.tmp', [GetWindowsTempPath, GetCurrentProcessId]);
  end;

  procedure TPhoaUndoFile.CreateStream;
  begin
    if FStream=nil then FStream := TFileStream.Create(FFileName, fmCreate);
  end;

  destructor TPhoaUndoFile.Destroy;
  begin
     // Уничтожаем поток и файл
    Clear;
    inherited Destroy;
  end;

  procedure TPhoaUndoFile.EndUndo;
  begin
    Assert(FUndoCounter>0, 'Excessive TPhoaUndoFile.EndUndo() call');
    Dec(FUndoCounter);
     // Счётчик сравнялся с нулём - позиционируем в запомненную позицию и усекаем файл
    if FUndoCounter=0 then begin
      FStream.Position := FUndoPosition;
      FStream.Size     := FUndoPosition;
    end;
  end;

  function TPhoaUndoFile.GetPosition: Int64;
  begin
    CreateStream;
    Result := FStream.Position;
  end;

  function TPhoaUndoFile.ReadBool: Boolean;
  begin
    ReadCheckDatatype(pufdBool);
    Result := StreamReadByte(FStream)<>0;
  end;

  function TPhoaUndoFile.ReadByte: Byte;
  begin
    ReadCheckDatatype(pufdByte);
    Result := StreamReadByte(FStream);
  end;

  procedure TPhoaUndoFile.ReadCheckDatatype(DTRequired: TPhoaUndoFileDatatype);
  var DTActual: TPhoaUndoFileDatatype;
  begin
    Byte(DTActual) := StreamReadByte(FStream);
    if DTActual<>DTRequired then
      raise Exception.CreateFmt(
        'Invalid undo stream datatype; required: %s, actual: %s',
        [GetEnumName(TypeInfo(TPhoaUndoFileDatatype), Byte(DTRequired)), GetEnumName(TypeInfo(TPhoaUndoFileDatatype), Byte(DTActual))]);
  end;

  function TPhoaUndoFile.ReadInt: Integer;
  begin
    ReadCheckDatatype(pufdInt);
    Result := StreamReadInt(FStream);
  end;

  function TPhoaUndoFile.ReadStr: String;
  begin
    ReadCheckDatatype(pufdStr);
    Result := StreamReadStr(FStream);
  end;

  procedure TPhoaUndoFile.WriteBool(b: Boolean);
  begin
    WriteDatatype(pufdBool);
    StreamWriteByte(FStream, Byte(b));
  end;

  procedure TPhoaUndoFile.WriteByte(b: Byte);
  begin
    WriteDatatype(pufdByte);
    StreamWriteByte(FStream, b);
  end;

  procedure TPhoaUndoFile.WriteDatatype(DT: TPhoaUndoFileDatatype);
  begin
    StreamWriteByte(FStream, Byte(DT));
  end;

  procedure TPhoaUndoFile.WriteInt(i: Integer);
  begin
    WriteDatatype(pufdInt);
    StreamWriteInt(FStream, i);
  end;

  procedure TPhoaUndoFile.WriteStr(const s: String);
  begin
    WriteDatatype(pufdStr);
    StreamWriteStr(FStream, s);
  end;

   //===================================================================================================================
   // TPhoaOperation
   //===================================================================================================================

  constructor TPhoaOperation.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
  begin
    FList := AList;
    FList.Add(Self);
    FProject := AProject;
    FUndoDataPosition := FList.UndoFile.Position;
  end;

  destructor TPhoaOperation.Destroy;
  begin
    FProject := nil;
    FList.Remove(Self);
    inherited Destroy;
  end;

  function TPhoaOperation.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [];
  end;

  function TPhoaOperation.GetOpGroup: IPhotoAlbumPicGroup;
  begin
    Result := FProject.RootGroupX.GroupByIDX[FOpGroupID];
  end;

  function TPhoaOperation.GetParentOpGroup: IPhotoAlbumPicGroup;
  begin
    Result := FProject.RootGroupX.GroupByIDX[FOpParentGroupID];
  end;

  function TPhoaOperation.GetUndoFile: TPhoaUndoFile;
  begin
    Result := FList.UndoFile;
  end;

  function TPhoaOperation.Name: String;
  begin
    Result := ConstVal(ClassName);
  end;

  procedure TPhoaOperation.RollbackChanges;
  begin
    { does nothing }
  end;

  procedure TPhoaOperation.SetOpGroup(Value: IPhotoAlbumPicGroup);
  begin
    FOpGroupID := Value.ID;
  end;

  procedure TPhoaOperation.SetParentOpGroup(Value: IPhotoAlbumPicGroup);
  begin
    FOpParentGroupID := Value.ID;
  end;

  procedure TPhoaOperation.Undo;
  begin
    try
       // Позиционируем undo-файл в запомненную позицию
      UndoFile.BeginUndo(FUndoDataPosition);
      try
         // Откатываем изменения
        RollbackChanges;
      finally
         // Возвращаем позицию в undo-файле на место
        UndoFile.EndUndo;
      end;
    finally
       // Уничтожаем объект
      Destroy;
    end;
  end;

   //===================================================================================================================
   // TPhoaOperations
   //===================================================================================================================

  function TPhoaOperations.Add(Item: TPhoaOperation): Integer;
  begin
    Result := inherited Add(Item);
    DoStatusChange;
    if Assigned(FOnOpDone) then FOnOpDone(Self);
  end;

  procedure TPhoaOperations.BeginUpdate;
  begin
    Inc(FUpdateLock);
  end;

  procedure TPhoaOperations.Clear;
  begin
    BeginUpdate;
    try
      while Count>0 do Delete(0);
      inherited Clear;
    finally
      EndUpdate;
    end;
  end;

  constructor TPhoaOperations.Create(AUndoFile: TPhoaUndoFile);
  begin
    inherited Create;
    FUndoFile := AUndoFile;
  end;

  procedure TPhoaOperations.Delete(Index: Integer);
  begin
    GetItems(Index).Free;
  end;

  procedure TPhoaOperations.DoStatusChange;
  begin
    if Assigned(FOnStatusChange) then FOnStatusChange(Self);
  end;

  procedure TPhoaOperations.EndUpdate;
  begin
    if FUpdateLock>0 then begin
      Dec(FUpdateLock);
      if FUpdateLock=0 then DoStatusChange;
    end;
  end;

  function TPhoaOperations.GetCanUndo: Boolean;
  begin
    Result := Count>0;
  end;

  function TPhoaOperations.GetItems(Index: Integer): TPhoaOperation;
  begin
    Result := TPhoaOperation(inherited Items[Index]);
  end;

  function TPhoaOperations.Remove(Item: TPhoaOperation): Integer;
  begin
    Result := inherited Remove(Item);
    if Result>=0 then begin
      DoStatusChange;
      if Assigned(FOnOpUndone) then FOnOpUndone(Self);
    end;
  end;

  procedure TPhoaOperations.UndoAll;
  var i: Integer;
  begin
    BeginUpdate;
    try
      for i := Count-1 downto 0 do GetItems(i).Undo;
    finally
      EndUpdate;
    end;
  end;

   //===================================================================================================================
   // TPhoaUndo
   //===================================================================================================================

  procedure TPhoaUndo.Clear;
  begin
    inherited Clear;
     // Обрезаем файл
    UndoFile.Clear;
  end;

  constructor TPhoaUndo.Create;
  begin
    inherited Create(TPhoaUndoFile.Create);
    FSavepointOnEmpty := True;
  end;

  destructor TPhoaUndo.Destroy;
  var UFile: TPhoaUndoFile;
  begin
    UFile := UndoFile;
    inherited Destroy;
     // Уничтожаем файл отката 
    UFile.Free;
  end;

  function TPhoaUndo.GetIsUnmodified: Boolean;
  begin
    if Count=0 then Result := FSavepointOnEmpty else Result := GetItems(Count-1).FSavepoint;
  end;

  function TPhoaUndo.GetLastOpName: String;
  begin
    if Count=0 then Result := '' else Result := GetItems(Count-1).Name;
  end;

  procedure TPhoaUndo.SetNonUndoable;
  begin
    BeginUpdate;
    try
      Clear;
      FSavepointOnEmpty := False;
    finally
      EndUpdate;
    end;
  end;

  procedure TPhoaUndo.SetSavepoint;
  var i: Integer;
  begin
    BeginUpdate;
    try
      for i := 0 to Count-1 do Items[i].FSavepoint := i=Count-1;
      FSavepointOnEmpty := Count=0;
    finally
      EndUpdate;
    end;
  end;

   //===================================================================================================================
   // TPhoaMultiOp
   //===================================================================================================================

  constructor TPhoaMultiOp.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
  begin
    inherited Create(AList, AProject);
    FOperations := TPhoaOperations.Create(List.UndoFile);
  end;

  destructor TPhoaMultiOp.Destroy;
  begin
    FOperations.Free;
    inherited Destroy;
  end;

  procedure TPhoaMultiOp.RollbackChanges;
  begin
    inherited RollbackChanges;
    FOperations.UndoAll;
  end;

   //===================================================================================================================
   // TPhoaOp_NewGroup
   //===================================================================================================================

  constructor TPhoaOp_GroupNew.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; CurGroup: IPhotoAlbumPicGroup);
  var g: IPhotoAlbumPicGroup;
  begin
    inherited Create(AList, AProject);
     // Создаём дочернюю группу
    g := NewPhotoAlbumPicGroup(CurGroup, Project.RootGroupX.MaxGroupID+1);
    g.Text := ConstVal('SDefaultNewGroupName');
    OpParentGroup := CurGroup;
    OpGroup       := g;
  end;

  function TPhoaOp_GroupNew.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXReinitParent, uifXEditGroup, // Execution flags
      uifUReinitParent];               // Undo flags
  end;

  procedure TPhoaOp_GroupNew.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Удаляем группу операции
    OpGroup.Owner := nil;
  end;

   //===================================================================================================================
   // TPhoaOp_GroupRename
   //===================================================================================================================

  constructor TPhoaOp_GroupRename.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; const sNewText: String);
  begin
    inherited Create(AList, AProject);
     // Запоминаем данные отката
    OpGroup := Group;
    UndoFile.WriteStr(Group.Text);
     // Выполняем операцию
    Group.Text := sNewText;
  end;

  procedure TPhoaOp_GroupRename.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Получаем группу и восстанавливаем текст
    OpGroup.Text := UndoFile.ReadStr;
  end;

   //===================================================================================================================
   // TPhoaOp_GroupEdit
   //===================================================================================================================

  constructor TPhoaOp_GroupEdit.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; const sNewText, sNewDescription: String);
  begin
    inherited Create(AList, AProject, Group, sNewText);
     // Запоминаем данные отката
    UndoFile.WriteStr(Group.Description);
     // Выполняем операцию
    Group.Description := sNewDescription;
  end;

  procedure TPhoaOp_GroupEdit.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Получаем группу и восстанавливаем описание
    OpGroup.Description := UndoFile.ReadStr;
  end;

   //===================================================================================================================
   // TPhoaOp_GroupDelete
   //===================================================================================================================

  constructor TPhoaOp_GroupDelete.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; bPerform: Boolean);
  var i: Integer;
  begin
    inherited Create(AList, AProject);
     // Запоминаем данные удаляемой группы
    OpGroup       := Group;
    OpParentGroup := Group.OwnerX;
    UndoFile.WriteStr (Group.Text);
    UndoFile.WriteStr (Group.Description);
    UndoFile.WriteInt (Group.Index);
    UndoFile.WriteBool(Group.Expanded);
     // Запоминаем содержимое (ID изображений)
    if Group.Pics.Count>0 then begin
      FPicIDs := TIntegerList.Create(False);
      for i := 0 to Group.Pics.Count-1 do FPicIDs.Add(Group.Pics[i].ID);
    end;
     // Запоминаем список каскадно удаляемых узлов
    if Group.Groups.Count>0 then begin
      FCascadedDeletes := TPhoaOperations.Create(List.UndoFile);
      for i := 0 to Group.Groups.Count-1 do TPhoaOp_GroupDelete.Create(FCascadedDeletes, Project, Group.GroupsX[i], False);
    end;
     // Выполняем операцию
    if bPerform then begin
       // Удаляем группу
      Group.Owner := nil;
       // Удаляем неиспользуемые изображения
      FUnlinkedPicsRemove := TPhoaOp_InternalUnlinkedPicsRemoving.Create(List, Project);
    end;
  end;

  destructor TPhoaOp_GroupDelete.Destroy;
  begin
    FCascadedDeletes.Free;
    FUnlinkedPicsRemove.Free;
    FPicIDs.Free;
    inherited Destroy;
  end;

  function TPhoaOp_GroupDelete.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXReinitParent, uifXReinitRecursive,  // Execution flags
      uifUReinitParent, uifUReinitRecursive]; // Undo flags
  end;

  procedure TPhoaOp_GroupDelete.InternalRollback(gOwner: IPhotoAlbumPicGroup);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
  begin
     // Восстанавливаем группу
    g := NewPhotoAlbumPicGroup(gOwner, OpGroupID);
    g.Text        := UndoFile.ReadStr;
    g.Description := UndoFile.ReadStr;
    g.Index       := UndoFile.ReadInt;
    g.Expanded    := UndoFile.ReadBool;
    if FPicIDs<>nil then
      for i := 0 to FPicIDs.Count-1 do g.PicsX.Add(Project.Pics.ItemsByID[FPicIDs[i]], False);
     // Восстанавливаем каскадно удалённые группы
    if FCascadedDeletes<>nil then
      for i := 0 to FCascadedDeletes.Count-1 do TPhoaOp_GroupDelete(FCascadedDeletes[i]).InternalRollback(g);
  end;

  procedure TPhoaOp_GroupDelete.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Восстанавливаем ветку групп/узлов
    InternalRollback(OpParentGroup);
     // Восстанавливаем удалённые (несвязанные) изображения
    if FUnlinkedPicsRemove<>nil then FUnlinkedPicsRemove.Undo;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalUnlinkedPicsRemoving
   //===================================================================================================================

  constructor TPhoaOp_InternalUnlinkedPicsRemoving.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
  var
    i: Integer;
    Pic: IPhotoAlbumPic;
  begin
    inherited Create(AList, AProject);
     // Цикл по всем изображениям фотоальбома
    for i := Project.Pics.Count-1 downto 0 do begin
      Pic := Project.PicsX[i];
       // Если изображение не связано ни с одной группой
      if not Project.RootGroup.IsPicLinked(Pic.ID, True) then begin
         // Пишем флаг продолжения
        UndoFile.WriteBool(True);
         // Сохраняем данные изображения
        UndoFile.WriteStr(Pic.RawData[PPAllProps]);
         // Удаляем изображение из списка
        Pic.Release;
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_InternalUnlinkedPicsRemoving.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Читаем данные, пока не встретим стоп-флаг
    while UndoFile.ReadBool do
       // Создаём изображение
      with NewPhotoAlbumPic do begin
         // Загружаем данные
        RawData[PPAllProps] := UndoFile.ReadStr;
         // Кладём в список (ID уже загружен)
        PutToList(Project.PicsX, False);
      end;
  end;

   //===================================================================================================================
   // TPicPropertyChanges
   //===================================================================================================================

  function TPicPropertyChanges.Add(const sNewValue: String; Prop: TPicProperty): Integer;
  var p: PPicPropertyChange;
  begin
    New(p);
    Result := inherited Add(p);
    p^.sNewValue := sNewValue;
    p^.Prop      := Prop;
  end;

  function TPicPropertyChanges.GetChangedProps: TPicProperties;
  var i: Integer;
  begin
    Result := [];
    for i := 0 to Count-1 do Include(Result, GetItems(i).Prop);
  end;

  function TPicPropertyChanges.GetItems(Index: Integer): PPicPropertyChange;
  begin
    Result := PPicPropertyChange(inherited Items[Index]);
  end;

  procedure TPicPropertyChanges.Notify(Ptr: Pointer; Action: TListNotification);
  begin
    if Action=lnDeleted then Dispose(PPicPropertyChange(Ptr));
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicProps
   //===================================================================================================================

  constructor TPhoaOp_InternalEditPicProps.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Pics: IPhotoAlbumPicList; ChangeList: TPicPropertyChanges);
  var
    iPic, iChg: Integer;
    Pic: IPhotoAlbumPic;
    ChangedProps: TPicProperties;
  begin
    inherited Create(AList, AProject);
     // Сохраняем набор изменяющихся свойств
    ChangedProps := ChangeList.ChangedProps;
    UndoFile.WriteInt(PicPropsToInt(ChangedProps));
     // Сохраняем количество изображений
    UndoFile.WriteInt(Pics.Count);
     // Цикл по изображениям
    for iPic := 0 to Pics.Count-1 do begin
       // Запоминаем старые данные
      Pic := Pics[iPic];
      UndoFile.WriteInt(Pic.ID);
      UndoFile.WriteStr(Pic.RawData[ChangedProps]);
       // Применяем новые данные
      for iChg := 0 to ChangeList.Count-1 do
        with ChangeList[iChg]^ do Pic.Props[Prop] := sNewValue;
    end;
  end;

  procedure TPhoaOp_InternalEditPicProps.RollbackChanges;
  var
    i, iPicID: Integer;
    ChangedProps: TPicProperties;
    sPicData: String;
  begin
    inherited RollbackChanges;
     // Получаем набор изменённых свойств
    ChangedProps := IntToPicProps(UndoFile.ReadInt);
     // Возвращаем данные изменённых изображений
    for i := 0 to UndoFile.ReadInt-1 do begin
      iPicID   := UndoFile.ReadInt;
      sPicData := UndoFile.ReadStr;
      Project.PicsX.ItemsByIDX[iPicID].RawData[ChangedProps] := sPicData;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicKeywords
   //===================================================================================================================

  constructor TPhoaOp_InternalEditPicKeywords.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Pics: IPhotoAlbumPicList; Keywords: TKeywordList);
  var
    iPic, iCnt, iKwd, idxKeyword: Integer;
    Pic: IPhotoAlbumPic;
    pkr: PKeywordRec;
    bKWSaved: Boolean;
    PicKeywords: IPhoaMutableKeywordList;

     // Сохраняет ключевые слова изображения в FSavedKeywords, если этого ещё не сделано
    procedure SavePicKeywords;
    begin
      if not bKWSaved then begin
        UndoFile.WriteBool(True); // Признак записи ключевого слова (в противоположность стоп-флагу)
        UndoFile.WriteInt(Pic.ID);
        UndoFile.WriteStr(Pic.Keywords.CommaText);
        bKWSaved := True;
      end;
    end;

  begin
    inherited Create(AList, AProject);
     // Цикл по изображениям
    iCnt := Pics.Count;
    for iPic := 0 to iCnt-1 do begin
      Pic := Pics[iPic];
      PicKeywords := Pic.KeywordsM;
      bKWSaved := False;
       // Цикл по ключевым словам
      for iKwd := 0 to Keywords.Count-1 do begin
        pkr := Keywords[iKwd];
        case pkr.Change of
           // КС не менялось. Проверяем изменение птицы
          kcNone:
             // Если Grayed - менять нечего по определению. Если не выделено, и КС не содержится ни в одном изображении
             //   - менять нечего. Если выделено полностью, и КС содержится во всех изображениях - менять нечего.
             //   Иначе проверяем наличие КС в изображении
            if ((pkr.State=ksOff) and (pkr.iSelCount>0)) or ((pkr.State=ksOn) and (pkr.iSelCount<iCnt)) then begin
              idxKeyword := PicKeywords.IndexOf(pkr.sKeyword);
              case pkr.State of
                 // Надо убрать КС. Если оно есть - убираем
                ksOff:
                  if idxKeyword>=0 then begin
                    SavePicKeywords;
                    PicKeywords.Delete(idxKeyword);
                  end;
                 // Надо добавить КС. Если нет - добавляем
                ksOn:
                  if idxKeyword<0 then begin
                    SavePicKeywords;
                    PicKeywords.Add(pkr.sKeyword);
                  end;
              end;
            end;
           // Добавление нового КС. Если есть птица - надо добавить
          kcAdd:
            if pkr.State=ksOn then begin
              SavePicKeywords;
              PicKeywords.Add(pkr.sKeyword);
            end;
           // КС менялось. Если только оно не полностью отсутствовало и отсутствует, ...
          kcReplace:
            if (pkr.State<>ksOff) or (pkr.iSelCount>0) then begin
               // ... ищем старое КС и удаляем, ...
              idxKeyword := PicKeywords.IndexOf(pkr.sOldKeyword);
              if idxKeyword>=0 then begin
                SavePicKeywords;
                PicKeywords.Delete(idxKeyword);
              end;
               // ... если состояние ksOn - добавляем новое всем, если ksGrayed - добавляем только в те, где было старое
              if (pkr.State=ksOn) or ((pkr.State=ksGrayed) and (idxKeyword>=0)) then begin
                SavePicKeywords;
                PicKeywords.Add(pkr.sKeyword);
              end;
            end;
        end;
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False); 
  end;

  procedure TPhoaOp_InternalEditPicKeywords.RollbackChanges;
  var iPicID: Integer;
  begin
    inherited RollbackChanges;
     // Возвращаем КС изменённым изображениям: крутим цикл, пока не встретим стоп-флаг
    while UndoFile.ReadBool do begin
      iPicID    := UndoFile.ReadInt;
      Project.PicsX.ItemsByIDX[iPicID].KeywordsM.CommaText := UndoFile.ReadStr;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_StoreTransform
   //===================================================================================================================

  constructor TPhoaOp_StoreTransform.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Pic: IPhoaMutablePic; NewRotation: TPicRotation; NewFlips: TPicFlips);
  begin
    inherited Create(AList, AProject);
     // Сохраняем прежние свойства
    UndoFile.WriteInt(Pic.ID);
    UndoFile.WriteByte(Byte(Pic.Rotation));
    UndoFile.WriteByte(Byte(Pic.Flips));
     // Применяем новые свойства
    Pic.Rotation := NewRotation;
    Pic.Flips    := NewFlips;
  end;

  procedure TPhoaOp_StoreTransform.RollbackChanges;
  var Pic: IPhotoAlbumPic;
  begin
    inherited RollbackChanges;
    Pic          := Project.PicsX.ItemsByIDX[UndoFile.ReadInt];
    Pic.Rotation := TPicRotation(UndoFile.ReadByte);
    Pic.Flips    := TPicFlips(Byte(UndoFile.ReadByte)); // Странный typecast, но иначе не компилируется
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicAdd
   //===================================================================================================================

  constructor TPhoaOp_InternalPicAdd.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; const sFilename: String; out AddedPic: IPhotoAlbumPic);
  var Pic: IPhotoAlbumPic;
  begin
    inherited Create(AList, AProject);
     // Ищем уже существующее изображение с тем же файлом
    Pic := Project.PicsX.ItemsByFileNameX[sFilename];
    FExisting := Pic<>nil;
     // Если файл ещё не использовался, создаём экземпляр изображения
    if not FExisting then begin
      Pic := NewPhotoAlbumPic;
       // Добавляем в список, получая ID
      Pic.PutToList(Project.PicsX, True);
       // Присваиваем имя файла и строим эскиз
      Pic.FileName := sFilename;
      Pic.ReloadPicFileData(Project.ThumbnailSize, TPhoaStretchFilter(SettingValueInt(ISettingID_Browse_ViewerStchFilt)), Project.ThumbnailQuality);
    end;
     // Добавляем в группу
    RegisterPic(Group, Pic);
    AddedPic := Pic;
  end;

  constructor TPhoaOp_InternalPicAdd.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
  var PicEx: IPhotoAlbumPic;
  begin
    inherited Create(AList, AProject);
     // Ищем уже существующее изображение с тем же файлом
    PicEx := Project.PicsX.ItemsByFileNameX[Pic.FileName];
    FExisting := PicEx<>nil;
     // Если новое изображение - заносим в список, распределяя новый ID. Иначе игнорируем Pic
    if not FExisting then Pic.PutToList(Project.PicsX, True) else Pic := PicEx;
     // Добавляем в группу
    RegisterPic(Group, Pic);
  end;

  procedure TPhoaOp_InternalPicAdd.RegisterPic(Group: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
  var bAdded: Boolean;
  begin
     // Добавляем изображение в группу, если его не было
    Group.PicsX.Add(Pic, True, bAdded);
    if bAdded then begin
       // Сохраняем данные для отката
      OpGroup := Group;
      UndoFile.WriteInt(Pic.ID);
    end else
      UndoFile.WriteInt(0);
  end;

  procedure TPhoaOp_InternalPicAdd.RollbackChanges;
  var iPicID: Integer;
  begin
    inherited RollbackChanges;
     // Если реально операция была сделана
    iPicID := UndoFile.ReadInt;
    if iPicID>0 then begin
       // Удаляем из группы
      OpGroup.PicsX.Remove(iPicID);
       // Если было добавлено новое изображение, удаляем и из фотоальбома
      if not FExisting then Project.PicsX.ItemsByIDX[iPicID].Release;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_PicFromGroupRemove
   //===================================================================================================================

  constructor TPhoaOp_InternalPicFromGroupRemoving.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList);
  var i, idx: Integer;
  begin
    inherited Create(AList, AProject);
     // Запоминаем группу
    OpGroup := Group;
     // Запоминаем ID и индексы
    for i := 0 to Pics.Count-1 do begin
       // Если есть такой ID в группе, записываем и удаляем
      idx := Group.Pics.IndexOfID(Pics[i].ID);
      if idx>=0 then begin
         // Пишем флаг продолжения
        UndoFile.WriteBool(True);
         // Пишем ID
        UndoFile.WriteInt(Pics[i].ID);
         // Пишем индекс
        UndoFile.WriteInt(idx);
         // Удаляем изображение
        Group.PicsX.Delete(idx);
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False); 
  end;

  procedure TPhoaOp_InternalPicFromGroupRemoving.RollbackChanges;
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
    IIs: TIntegerList;
  begin
    inherited RollbackChanges;
    g := OpGroup;
     // Загружаем ID и индексы во временный список
    IIs := TIntegerList.Create(True);
    try
      while UndoFile.ReadBool do begin
        IIs.Add(UndoFile.ReadInt);
        IIs.Add(UndoFile.ReadInt);
      end;
       // Восстанавливаем изображения в обратном порядке, чтобы они встали на свои места
      i := IIs.Count-2; // i указывает на ID, i+1 - на индекс
      while i>=0 do begin
        g.PicsX.Insert(IIs[i+1], Project.Pics.ItemsByID[IIs[i]], False);
        Dec(i, 2);
      end;
    finally
      IIs.Free;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicToGroupAdding
   //===================================================================================================================

  constructor TPhoaOp_InternalPicToGroupAdding.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList);
  var
    i: Integer;
    bAdded: Boolean;
    Pic: IPhoaPic;
  begin
    inherited Create(AList, AProject);
    OpGroup := Group;
     // Добавляем изображения в группу и в undo-файл
    for i := 0 to Pics.Count-1 do begin
      Pic := Pics[i];
      Group.PicsX.Add(Pic, True, bAdded);
      if bAdded then begin
        UndoFile.WriteBool(True); // Флаг продолжения
        UndoFile.WriteInt (Pic.ID);
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False); 
  end;

  procedure TPhoaOp_InternalPicToGroupAdding.RollbackChanges;
  var g: IPhotoAlbumPicGroup;
  begin
    inherited RollbackChanges;
     // Удаляем добавленные изображения (считываем ID добавленных изображений из файла, пока не встретим стоп-флаг)
    g := OpGroup;
    while UndoFile.ReadBool do g.PicsX.Remove(UndoFile.ReadInt);
  end;

   //===================================================================================================================
   // TPhoaBaseOp_PicCopy
   //===================================================================================================================

  constructor TPhoaBaseOp_PicCopy.Create(Pics: IPhotoAlbumPicList; ClipFormats: TPicClipboardFormats);

     // Копирует в буфер обмена данные изображения PhoA
    procedure CopyPhoaData;
    var
      i: Integer;
      ms: TMemoryStream;
      Streamer: TPhoaStreamer;
      hRec: THandle;
      p: Pointer;
    begin
       // Сохраняем данные изображений во временный поток
      ms := TMemoryStream.Create;
      try
        Streamer := TPhoaStreamer.Create(ms, psmWrite, '');
        try
           // Добавляем данные изображений
          for i := 0 to Pics.Count-1 do begin
            Streamer.WriteChunk(IPhChunk_Pic_Open);
            Pics[i].StreamerSave(Streamer, False, PPAllProps);
            Streamer.WriteChunk(IPhChunk_Pic_Close);
          end;
        finally
          Streamer.Free;
        end;
         // Выделяем память и блокируем её
        hRec := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, ms.Size);
        p := GlobalLock(hRec);
        try
           // Переписываем строку в память
          Move(ms.Memory^, p^, ms.Size);
        finally
          GlobalUnlock(hRec);
        end;
      finally
        ms.Free;
      end;
       // Копируем
      Clipboard.SetAsHandle(wClipbrdPicFormatID, hRec);
    end;

     // Копирует в буфер обмена объекты "Файл"
    procedure CopyFileObjects;
    var
      i: Integer;
      SL: TStringList;
    begin
      SL := TStringList.Create;
      try
         // Дубликаты игнорируем
        SL.Sorted := True;
        SL.Duplicates := dupIgnore;
         // Составляем список полных путей файлов в StringList
        for i := 0 to Pics.Count-1 do SL.Add(Pics[i].FileName);
         // Создаём объект THDrop
        with THDrop.Create do
          try
             // Помещаем список файлов в THDrop
            AssignFiles(SL);
             // Помещаем файлы в clipboard
            SaveToClipboard(True);
          finally
            Free;
          end;
      finally
        SL.Free;
      end;
    end;

     // Копирует в буфер обмена текстовый список путей к файлам изображений
    procedure CopyFileList;
    var
      i: Integer;
      s: String;
    begin
       // Составляем список полных путей файлов
      s := '';
      for i := 0 to Pics.Count-1 do s := s+Pics[i].FileName+S_CRLF;
       // Помещаем текст в clipboard
      Clipboard.AsText := s;
    end;

     // Копирует в буфер обмена bitmap-эскиз изображения Pic
    procedure CopyThumbBitmap(Pic: IPhoaPic);
    var bmp32: TBitmap32;
    begin
       // Отрисовываем эскиз
      bmp32 := TBitmap32.Create;
      try
        bmp32.SetSize(Pic.ThumbnailSize.cx, Pic.ThumbnailSize.cy);
        PaintThumbnail(Pic, bmp32);
         // Помещаем bitmap в clipboard
        Clipboard.Assign(bmp32);
      finally
        bmp32.Free;
      end;
    end;

  begin
    StartWait;
    try
      if Pics.Count>0 then begin
        Clipboard.Open;
        try
           // Помещаем PhoA-данные
          if pcfPhoa in ClipFormats then CopyPhoaData;
           // Помещаем объекты "Файл"
          if pcfHDrop in ClipFormats then CopyFileObjects;
           // Помещаем список путей файлов
          if pcfPlainList in ClipFormats then CopyFileList;
           // Помещаем изображение эскиза (в случае единственного изображения)
          if (pcfSingleBitmap in ClipFormats) and (Pics.Count=1) then CopyThumbBitmap(Pics[0]);
        finally
          Clipboard.Close;
        end;
      end;
    finally
      StopWait;
    end;
  end;

   //===================================================================================================================
   // TPhoaMultiOp_PicDelete
   //===================================================================================================================

  constructor TPhoaMultiOp_PicDelete.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList);
  begin
    inherited Create(AList, AProject);
    OpGroup := Group;
     // Удаляем изображения из группы
    TPhoaOp_InternalPicFromGroupRemoving.Create(FOperations, Project, Group, Pics);
     // Удаляем несвязанные изображения из фотоальбома
    TPhoaOp_InternalUnlinkedPicsRemoving.Create(FOperations, Project);
  end;

   //===================================================================================================================
   // TPhoaMultiOp_PicPaste
   //===================================================================================================================

  constructor TPhoaMultiOp_PicPaste.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup);
  var
    hRec: THandle;
    ms: TMemoryStream;
    Streamer: TPhoaStreamer;
    Pic: IPhotoAlbumPic;
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
    inherited Create(AList, AProject);
    StartWait;
    try
      if Clipboard.HasFormat(wClipbrdPicFormatID) then begin
        OpGroup := Group;
         // Сорздаём временный поток
        ms := TMemoryStream.Create;
        try
           // Получаем данные из буфера обмена
          hRec := Clipboard.GetAsHandle(wClipbrdPicFormatID);
          ms.Write(GlobalLock(hRec)^, GlobalSize(hRec));
          GlobalUnlock(hRec);
          ms.Position := 0;
           // Создаём Streamer
          Streamer := TPhoaStreamer.Create(ms, psmRead, '');
          try
             // Создаём загруженные изображения
            while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
              case Code of
                 // Picture
                IPhChunk_Pic_Open: begin
                  Pic := NewPhotoAlbumPic;
                  Pic.StreamerLoad(Streamer, False, PPAllProps);
                   // Создаём дочернюю операцию добавления изображения
                  TPhoaOp_InternalPicAdd.Create(FOperations, Project, Group, Pic);
                end;
                 // Ensure unknown nested structures are skipped whole
                else Streamer.SkipNestedChunks(Code);
              end;
          finally
            Streamer.Free;
          end;
        finally
          ms.Free;
        end;
      end;
    finally
      StopWait;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_PhoAEdit
   //===================================================================================================================

  constructor TPhoaOp_PhoAEdit.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; const NewThSize: TSize; bNewThQuality: Byte; const sNewDescription: String);
  begin
    inherited Create(AList, AProject);
     // Сохраняем старые свойства
    UndoFile.WriteInt (Project.ThumbnailSize.cx);
    UndoFile.WriteInt (Project.ThumbnailSize.cy);
    UndoFile.WriteByte(Project.ThumbnailQuality);
    UndoFile.WriteStr (Project.Description);
     // Выполняем операцию
    Project.ThumbnailSize    := NewThSize;
    Project.ThumbnailQuality := bNewThQuality;
    Project.Description      := sNewDescription;
  end;

  function TPhoaOp_PhoAEdit.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXUpdateThumbParams,  // Execution flags
      uifUUpdateThumbParams]; // Undo flags
  end;

  procedure TPhoaOp_PhoAEdit.RollbackChanges;
  var Sz: TSize;
  begin
    inherited RollbackChanges;
     // Восстанавливаем свойства фотоальбома
    Sz.cx   := UndoFile.ReadInt;
    Sz.cy   := UndoFile.ReadInt;
    Project.ThumbnailSize    := Sz;
    Project.ThumbnailQuality := UndoFile.ReadByte;
    Project.Description      := UndoFile.ReadStr;
  end;

   //===================================================================================================================
   // TPhoaMultiOp_PicOperation
   //===================================================================================================================

  constructor TPhoaMultiOp_PicOperation.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; SourceGroup, TargetGroup: IPhotoAlbumPicGroup; Pics: IPhoaPicList; PicOperation: TPictureOperation);
  var
    i: Integer;
    IntersectPics: IPhoaMutablePicList;
    Pic: IPhoaPic;
  begin
    inherited Create(AList, AProject);
     // Копирование/перемещение: копируем выделенные изображения
    if PicOperation in [popMoveToTarget, popCopyToTarget] then TPhoaOp_InternalPicToGroupAdding.Create(FOperations, Project, TargetGroup, Pics);
     // Если перемещение - удаляем выделенные изображения из исходной группы
    if PicOperation=popMoveToTarget then TPhoaOp_InternalPicFromGroupRemoving.Create(FOperations, Project, SourceGroup, Pics);
     // Удаление выделенных изображений из указанной группы
    if PicOperation=popRemoveFromTarget then TPhoaOp_InternalPicFromGroupRemoving.Create(FOperations, Project, TargetGroup, Pics);
     // Оставить только выделенные изображения в указанной группе
    if PicOperation=popIntersectTarget then begin
      IntersectPics := NewPhotoAlbumPicList(False);
      for i := 0 to TargetGroup.Pics.Count-1 do begin
        Pic := TargetGroup.Pics[i];
        if Pics.IndexOfID(Pic.ID)<0 then IntersectPics.Add(Pic, False);
      end;
      if IntersectPics.Count>0 then begin
        TPhoaOp_InternalPicFromGroupRemoving.Create(FOperations, Project, TargetGroup, IntersectPics);
        TPhoaOp_InternalUnlinkedPicsRemoving.Create(FOperations, Project);
      end;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalGroupPicSort
   //===================================================================================================================

  constructor TPhoaOp_InternalGroupPicSort.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList);
  var i: Integer;
  begin
    inherited Create(AList, AProject);
     // Запоминаем группу
    OpGroup := Group;
     // Запоминаем порядок следования ID изображений в группе
    UndoFile.WriteInt(Group.Pics.Count);
    for i := 0 to Group.Pics.Count-1 do UndoFile.WriteInt(Group.Pics[i].ID);
     // Сортируем изображения в группе
    Group.PicsX.SortingsSort(Sortings);
  end;

  procedure TPhoaOp_InternalGroupPicSort.RollbackChanges;
  var i: Integer;
  begin
    inherited RollbackChanges;
     // Восстанавливаем старый порядок следования ID изображений в группе
    OpGroup.PicsX.Clear;
    for i := 0 to UndoFile.ReadInt-1 do OpGroup.PicsX.Add(Project.Pics.ItemsByID[UndoFile.ReadInt], False);
  end;

   //===================================================================================================================
   // TPhoaMultiOp_PicSort
   //===================================================================================================================

  procedure TPhoaMultiOp_PicSort.AddGroupSortOp(Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean);
  var i: Integer;
  begin
     // Сортируем изображения в группе
    TPhoaOp_InternalGroupPicSort.Create(FOperations, Project, Group, Sortings);
     // При необходимости сортируем и в подгруппах
    if bRecursive then
      for i := 0 to Group.Groups.Count-1 do AddGroupSortOp(Group.GroupsX[i], Sortings, True);
  end;

  constructor TPhoaMultiOp_PicSort.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean);
  begin
    inherited Create(AList, AProject);
     // Запускаем сортировку
    AddGroupSortOp(Group, Sortings, bRecursive);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupDragAndDrop
   //===================================================================================================================

  constructor TPhoaOp_GroupDragAndDrop.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group, NewParentGroup: IPhotoAlbumPicGroup; iNewIndex: Integer);
  var gOldParent: IPhotoAlbumPicGroup;
  begin
    inherited Create(AList, AProject);
     // Запоминаем данные отката
    gOldParent := Group.OwnerX;
    UndoFile.WriteInt(Group.Index);
     // Перемещаем группу
    Group.Owner := NewParentGroup;
    if iNewIndex>=0 then Group.Index := iNewIndex; // Индекс -1 означает добавление последним ребёнком
     // Запоминаем группы (ID прежнего родителя и ID группы)
    OpParentGroup := gOldParent;
    OpGroup       := Group;
  end;

  function TPhoaOp_GroupDragAndDrop.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [uifUReinitAll];
  end;

  procedure TPhoaOp_GroupDragAndDrop.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Восстанавливаем положение группы
    with OpGroup do begin
      Owner := OpParentGroup;
      Index := UndoFile.ReadInt;
    end;
  end;

   //===================================================================================================================
   // TPhoaMultiOp_PicDragAndDropToGroup
   //===================================================================================================================

  constructor TPhoaMultiOp_PicDragAndDropToGroup.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; SourceGroup, TargetGroup: IPhotoAlbumPicGroup; Pics: IPhoaPicList; bCopy: Boolean);
  begin
    inherited Create(AList, AProject);
     // Выполняем операцию
    TPhoaOp_InternalPicToGroupAdding.Create(FOperations, Project, TargetGroup, Pics);
    if not bCopy then TPhoaOp_InternalPicFromGroupRemoving.Create(FOperations, Project, SourceGroup, Pics);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDragAndDropInsideGroup
   //===================================================================================================================

  constructor TPhoaOp_PicDragAndDropInsideGroup.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup; Pics: IPhoaPicList; idxNew: Integer);
  var i, idxOld: Integer;
  begin
    inherited Create(AList, AProject);
     // Запоминаем группу
    OpGroup := Group;
     // Выполняем операцию
    for i := 0 to Pics.Count-1 do begin
       // -- Пишем признак продолжения
      UndoFile.WriteBool(True);
       // -- Запоминаем индексы
      idxOld := Group.Pics.IndexOfID(Pics[i].ID);
      if idxOld<idxNew then Dec(idxNew);
      UndoFile.WriteInt(idxOld);
      UndoFile.WriteInt(idxNew);
       // -- Перемещаем изображение на новое место
      Group.PicsX.Move(idxOld, idxNew);
      Inc(idxNew);
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_PicDragAndDropInsideGroup.RollbackChanges;
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
    Indexes: TIntegerList;
  begin
    inherited RollbackChanges;
    g := OpGroup;
     // Загружаем индексы из файла во временный список
    Indexes := TIntegerList.Create(True);
    try
      while UndoFile.ReadBool do begin
        Indexes.Add(UndoFile.ReadInt);
        Indexes.Add(UndoFile.ReadInt);
      end;
       // Восстанавливаем изображения в обратном порядке, чтобы они встали на свои места
      i := Indexes.Count-2; // i указывает на старый индекс, i+1 - на новый
      while i>=0 do begin
        g.PicsX.Move(Indexes[i+1], Indexes[i]);
        Dec(i, 2);
      end;
    finally
      Indexes.Free;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_ViewNew
   //===================================================================================================================

  constructor TPhoaOp_ViewNew.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; const sName: String; Groupings: IPhotoAlbumPicGroupingList; Sortings: IPhotoAlbumPicSortingList);
  var
    View: IPhotoAlbumView;
    iNewViewIndex: Integer;
  begin
    inherited Create(AList, AProject);
     // Сохраняем предыдущий индекс представления
    UndoFile.WriteInt(Project.ViewIndex);
     // Выполняем операцию
    View := NewPhotoAlbumView(Project.ViewsX);
    View.Name := sName;
    View.GroupingsX.Assign(Groupings);
    View.SortingsX.Assign(Sortings);
     // Сохраняем новый индекс представления
    iNewViewIndex := View.Index;
    UndoFile.WriteInt(iNewViewIndex);
     // Перегружаем список
    Project.ViewIndex := iNewViewIndex;
  end;

  function TPhoaOp_ViewNew.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXReloadViews, uifXUpdateViewIndex,  // Execution flags
      uifUReloadViews, uifUUpdateViewIndex]; // Undo flags
  end;

  procedure TPhoaOp_ViewNew.RollbackChanges;
  var iPrevViewIndex, iNewViewIndex: Integer;
  begin
    inherited RollbackChanges;
     // Получаем сохранённые данные
    iPrevViewIndex := UndoFile.ReadInt;
    iNewViewIndex  := UndoFile.ReadInt;
     // Удаляем представление
    Project.ViewsX.Delete(iNewViewIndex);
     // Восстанавливаем прежнее выбранное представление
    Project.ViewIndex := iPrevViewIndex;
  end;

   //===================================================================================================================
   // TPhoaOp_ViewEdit
   //===================================================================================================================

  constructor TPhoaOp_ViewEdit.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; View: IPhotoAlbumView; const sNewName: String; NewGroupings: IPhotoAlbumPicGroupingList; NewSortings: IPhotoAlbumPicSortingList);
  var bWriteGroupings, bWriteSortings: Boolean;
  begin
    inherited Create(AList, AProject);
     // Сохраняем данные отката и применяем изменения
    UndoFile.WriteStr(View.Name);
    View.Name := sNewName;
     // Запоминаем новый индекс представления (ПОСЛЕ присвоения имени, т.к. оно изменяет позицию представления в списке)
    UndoFile.WriteInt(View.Index);
     // Список группировок создаём и сохраняем только в том случае, если это не переименование и он различается
    bWriteGroupings := (NewGroupings<>nil) and not View.Groupings.IdenticalWith(NewGroupings);
     // Пишем признак наличия группировок
    UndoFile.WriteBool(bWriteGroupings);
    if bWriteGroupings then begin
      UndoWriteGroupings(UndoFile, View.GroupingsX);
      View.GroupingsX.Assign(NewGroupings);
      View.Invalidate;
    end;
     // Список сортировок создаём и сохраняем только в том случае, если это не переименование и он различается
    bWriteSortings := (NewSortings<>nil) and not View.Sortings.IdenticalWith(NewSortings);
     // Пишем признак наличия сортировок
    UndoFile.WriteBool(bWriteSortings);
    if bWriteSortings then begin
      UndoWriteSortings(UndoFile, View.SortingsX);
      View.SortingsX.Assign(NewSortings);
      View.Invalidate;
    end;
     // Обновляем текущий индекс представления (мог поменяться после переименования представления)
    Project.ViewIndex := View.Index;
  end;

  function TPhoaOp_ViewEdit.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXReloadViews, uifXUpdateViewIndex,  // Execution flags
      uifUReloadViews, uifUUpdateViewIndex]; // Undo flags
  end;

  procedure TPhoaOp_ViewEdit.RollbackChanges;
  var
    sViewName: String;
    iViewIndex: Integer;
    View: IPhotoAlbumView;
  begin
    inherited RollbackChanges;
     // Восстанавливаем представление
    sViewName  := UndoFile.ReadStr;
    iViewIndex := UndoFile.ReadInt;
    View := Project.ViewsX[iViewIndex];
    View.Name := sViewName;
    if UndoFile.ReadBool then UndoReadGroupings(UndoFile, View.GroupingsX);
    if UndoFile.ReadBool then UndoReadSortings (UndoFile, View.SortingsX);
    View.Invalidate;
     // Обновляем текущий индекс представления (мог поменяться после переименования представления)
    Project.ViewIndex := View.Index;
  end;

   //===================================================================================================================
   // TPhoaOp_ViewDelete
   //===================================================================================================================

  constructor TPhoaOp_ViewDelete.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject);
  var View: IPhotoAlbumView;
  begin
    inherited Create(AList, AProject);
     // Сохраняем данные отката
    View := Project.CurrentViewX;
    UndoFile.WriteStr(View.Name);
    UndoWriteGroupings(UndoFile, View.GroupingsX);
    UndoWriteSortings (UndoFile, View.SortingsX);
     // Удаляем представление
    Project.ViewsX.Delete(Project.ViewIndex);
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
  end;

  function TPhoaOp_ViewDelete.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXReloadViews, uifXUpdateViewIndex,  // Execution flags
      uifUReloadViews, uifUUpdateViewIndex]; // Undo flags
  end;

  procedure TPhoaOp_ViewDelete.RollbackChanges;
  var View: IPhotoAlbumView;
  begin
    inherited RollbackChanges;
      // Создаём представление
    View := NewPhotoAlbumView(Project.ViewsX);
    View.Name := UndoFile.ReadStr;
    UndoReadGroupings(UndoFile, View.GroupingsX);
    UndoReadSortings (UndoFile, View.SortingsX);
     // Активизируем представление
    Project.ViewIndex := View.Index;
  end;

   //===================================================================================================================
   // TPhoaOp_ViewMakeGroup
   //===================================================================================================================

  constructor TPhoaOp_ViewMakeGroup.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Group: IPhotoAlbumPicGroup);
  var
    g: IPhotoAlbumPicGroup;
    View: IPhotoAlbumView;
  begin
    inherited Create(AList, AProject);
    View := Project.CurrentViewX;
     // Создаём группы (изначально с нулевыми ID)
    g := NewPhotoAlbumPicGroup(Group, 0);
    g.Assign(View.RootGroup, False, True, True);
    g.Text := View.Name;
     // Распределяем группам настоящие ID
    Project.RootGroupX.FixupIDs;
     // Запоминаем созданную (корневую) группу 
    OpGroup := g;
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
  end;

  function TPhoaOp_ViewMakeGroup.GetInvalidationFlags: TUndoInvalidationFlags;
  begin
    Result := [
      uifXUpdateViewIndex,  // Execution flags
      uifUUpdateViewIndex]; // Undo flags
  end;

  procedure TPhoaOp_ViewMakeGroup.RollbackChanges;
  begin
    inherited RollbackChanges;
     // Удаляем корневую группу копии представления
    OpGroup.Owner := nil;
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
  end;

end.
