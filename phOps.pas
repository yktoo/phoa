//**********************************************************************************************************************
//  $Id: phOps.pas,v 1.8 2004-10-19 07:31:32 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phOps;

interface
uses
  Windows, Messages, SysUtils, Classes, Contnrs, phObj, phPhoa, phIntf, phMutableIntf, phNativeIntf;

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

   //===================================================================================================================
   // Интерфейс параметров операции PhoA
   //===================================================================================================================

  IPhoaOperationParams = interface(IInterface)
    ['{4AF24473-A666-4373-9BD6-DC0868647DC9}']
     // Получает интерфейсное значение параметра. Если bRequired=True, то проверяет наличие параметра и при отсутствии
     //   вызывает Exception; при bRequired=False и отсутствии параметра в Intf возвращает nil. Если параметр есть,
     //   обязательно проверяет доступность интерфейса, при его отсутствии вызвает Exception
    procedure ObtainValIntf(const sName: String; GUID: TGUID; out Intf; bRequired: Boolean = True);
     // Prop handlers
    function  GetCount: Integer;
    function  GetNames(Index: Integer): String;
    function  GetValBool(const sName: String): Boolean;
    function  GetValByte(const sName: String): Byte;
    function  GetValInt(const sName: String): Integer;
    function  GetValStr(const sName: String): String;
    function  GetValues(const sName: String): Variant;
    function  GetValuesByIndex(Index: Integer): Variant;
    procedure SetValues(const sName: String; const Value: Variant);
    procedure SetValuesByIndex(Index: Integer; const Value: Variant);
     // Props
     // -- Количество параметров
    property Count: Integer read GetCount;
     // -- Наименования параметров по индексу
    property Names[Index: Integer]: String read GetNames;
     // -- Типизированные значения параметров с проверкой на существование и тип
    property ValBool[const sName: String]: Boolean                 read GetValBool;
    property ValByte[const sName: String]: Byte                    read GetValByte;
    property ValInt [const sName: String]: Integer                 read GetValInt;
    property ValStr [const sName: String]: String                  read GetValStr;
     // -- Значения параметров по имени (при присваивании Unassigned параметр удаляется)
    property Values[const sName: String]: Variant read GetValues write SetValues; default;
     // -- Значения параметров по индексу (при присваивании Unassigned параметр удаляется)
    property ValuesByIndex[Index: Integer]: Variant read GetValuesByIndex write SetValuesByIndex;
  end;

   //===================================================================================================================
   // Список [требуемых] изменений свойств изображений
   //===================================================================================================================

  PPicPropertyChange = ^TPicPropertyChange;
  TPicPropertyChange = record
    sNewValue: String;
    Prop: TPicProperty;
  end;

  IPhoaPicPropertyChangeList = interface(IInterface)
    ['{13EEEA04-FF5A-42B3-861E-C0C7F5A8A334}']
     // Добавляет новую запись
    function  Add(const sNewValue: String; Prop: TPicProperty): Integer;
     // Prop handlers
    function  GetChangedProps: TPicProperties;
    function  GetCount: Integer;
    function  GetItems(Index: Integer): PPicPropertyChange;
     // Props
     // -- Набор изменяющихся свойств
    property ChangedProps: TPicProperties read GetChangedProps;
     // -- Количество элементов в списке
    property Count: Integer read GetCount;
     // -- Элементы списка по индексу
    property Items[Index: Integer]: PPicPropertyChange read GetItems; default;
  end;

   //===================================================================================================================
   // Базовая операция, не изменяющая состояния фотоальбома
   //===================================================================================================================

  TBaseOperation = class(TObject)
  end;

  TPhoaOperations = class;

   // Флаги произведённых операцией изменений
  TPhoaOperationChange  = (
    pocProjectProps,     // Изменились свойства проекта
    pocProjectPicList,   // Изменилось содержимое списка изображений проекта
    pocViewList,         // Изменилось содержимое списка представлений
    pocViewIndex,        // Изменился индекс текущего представления
    pocGroupStructure,   // Изменилась структура групп
    pocGroupProps,       // Изменились свойства групп
    pocGroupPicList,     // Изменилось содержимое списков изображений групп
    pocPicProps);        // Изменились свойства изображений
  TPhoaOperationChanges = set of TPhoaOperationChange;

   //===================================================================================================================
   // Базовая (абстрактная) операция фотоальбома, принадлежащая списку (буферу отката), которая может быть отменена
   //   Params:
   //     Project: IPhotoAlbumProject - проект
   //===================================================================================================================

  TPhoaOperation = class(TBaseOperation)
  private
     // Позиция данных отката операции в Undo-файле данных отката (UndoFile)
    FUndoDataPosition: Int64;
     // Prop storage
    FList: TPhoaOperations;
    FProject: IPhotoAlbumProject;
    FOpGroupID: Integer;
    FOpParentGroupID: Integer;
    FOperations: TPhoaOperations;
     // Prop handlers
    function  GetOpGroup: IPhotoAlbumPicGroup;
    function  GetParentOpGroup: IPhotoAlbumPicGroup;
    procedure SetOpGroup(Value: IPhotoAlbumPicGroup);
    procedure SetParentOpGroup(Value: IPhotoAlbumPicGroup);
    function  GetUndoFile: TPhoaUndoFile;
    function  GetOperations: TPhoaOperations;
  protected
     // Prop storage
    FSavepoint: Boolean;
     // Основная процедура выполнения операции, вызывается при её создании. В базовом классе не делает ничего
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); virtual;
     // Основная процедура отката изменений, внесённых операцией. В базовом классе откатывает подчинённые операции
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); virtual;
     // Props
     // -- Возвращает группу, соответствующую GroupID
    property OpGroup: IPhotoAlbumPicGroup read GetOpGroup write SetOpGroup;
     // -- Возвращает группу, соответствующую ParentGroupID
    property OpParentGroup: IPhotoAlbumPicGroup read GetParentOpGroup write SetParentOpGroup;
  public
     // Типовой конструктор для создания операции из параметров
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); virtual;
    destructor Destroy; override;
     // Процедура, откатывающая изменения, внесённые операцией (вызовом RollbackChanges()), и уничтожающая
     //   объект-операцию. В Changes добавляет набор флагов внесённых в процессе отката изменений
    procedure Undo(var Changes: TPhoaOperationChanges);
     // Наименование операции
    function Name: String;
     // Props
     // -- Список - владелец операции
    property List: TPhoaOperations read FList;
     // -- Список подчинённых операций. Создаётся при первом обращении
    property Operations: TPhoaOperations read GetOperations;
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

   //===================================================================================================================
   // Список сделанных операций
   //===================================================================================================================

  TPhoaOperations = class(TObject)
  private
     // Собственно список
    FList: TList;
     // Prop storage
    FUndoFile: TPhoaUndoFile;
     // Prop handlers
    function  GetItems(Index: Integer): TPhoaOperation;
    function  GetCanUndo: Boolean;
    function  GetCount: Integer;
  protected
     // Откатывает весь буфер (предназначено для вторичных буферов множественных операций)
    procedure UndoAll(var Changes: TPhoaOperationChanges);
  public
    constructor Create(AUndoFile: TPhoaUndoFile);
    destructor Destroy; override;
     // Добавляет операцию в список, возвращая индекс свежедобавленной операции
    function  Add(Item: TPhoaOperation): Integer; virtual;
     // Удаляет операцию из списка, возвращая индекс операции, который она имела перед удалением
    function  Remove(Item: TPhoaOperation): Integer; virtual;
     // Удаляет операцию из списка по индексу
    procedure Delete(Index: Integer); virtual;
     // Очищает список
    procedure Clear; virtual;
     // Props
     // -- Возвращает True, если в списке есть операции для отмены
    property CanUndo: Boolean read GetCanUndo;
     // -- Количество операций в списке
    property Count: Integer read GetCount;
     // -- Индексированный список операций
    property Items[Index: Integer]: TPhoaOperation read GetItems; default;
     // -- Файл данных отката
    property UndoFile: TPhoaUndoFile read FUndoFile;
  end;

   //===================================================================================================================
   // Буфер отката PhoA. Является только *самостоятельным* объектом и обладает собственным файлом отката
   //===================================================================================================================

  TPhoaUndo = class(TPhoaOperations)
  private
     // True, если "пустое" состояние буфера отката соответствует сохранённому состоянию фотоальбома
    FSavepointOnEmpty: Boolean;
     // Prop storage
    FMaxCount: Integer;
     // Ограничивает количество операций в списке числом MaxCount
    procedure LimitCount;
     // Prop handlers
    function  GetLastOpName: String;
    function  GetIsUnmodified: Boolean;
    procedure SetMaxCount(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function  Add(Item: TPhoaOperation): Integer; override;
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
     // -- Максимальное количество операций в списке
    property MaxCount: Integer read FMaxCount write SetMaxCount;
  end;

   //*******************************************************************************************************************
   //
   // Реальные операции
   //
   //*******************************************************************************************************************

   //===================================================================================================================
   // Операция создания группы внутри существующей группы
   //   Params:
   //     Group:        IPhotoAlbumPicGroup - группа, в которой создавать группу
   //     out NewGroup: IPhotoAlbumPicGroup - созданная группа
   //===================================================================================================================

  TPhoaOp_GroupNew = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция переименования группы
   //   Params:
   //     Group:   IPhotoAlbumPicGroup - группа, которую переименовываем
   //     NewText: String              - новое наименование группы
   //===================================================================================================================

  TPhoaOp_GroupRename = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция редактирования свойств группы
   //   Params:
   //     Group:          IPhotoAlbumPicGroup - редактируемая группа
   //     NewText:        String              - новое наименование группы
   //     NewDescription: String              - новое описание группы
   //===================================================================================================================

  TPhoaOp_GroupEdit = class(TPhoaOp_GroupRename)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция удаления группы (также вычищает несвязанные после удаления изображения)
   //   Params:
   //     Group: IPhotoAlbumPicGroup - удаляемая группа
   //===================================================================================================================

  TPhoaOp_GroupDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления группы, создаётся из TPhoaOp_GroupDelete, удаляет только группу (с подгруппами) и не
   //   заботится о несвязанных изображениях
   //   Params:
   //     Group: IPhotoAlbumPicGroup - удаляемая группа
   //===================================================================================================================

  TPhoaOp_InternalGroupDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления неиспользуемых изображений из проекта
   //   Params:
   //     <none>
   //===================================================================================================================

  TPhoaOp_InternalUnlinkedPicsRemoving = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Комплексная операция редактирования изображений, служит контейнером для операций:
   //  - TPhoaOp_InternalEditPicProps
   //  - TPhoaOp_InternalEditPicKeywords
   //  - TPhoaOp_InternalPicFromGroupRemoving
   //  - TPhoaOp_InternalPicToGroupAdding
   //   Params:
   //     <none>
   //===================================================================================================================

  TPhoaOp_PicEdit = class(TPhoaOperation)
  end;

   //===================================================================================================================
   // Внутренняя операция редактирования свойств изображения, кроме ключевых слов
   //   Params:
   //     Pics:       IPhotoAlbumPicList         - список изображений, чьи свойства изменяются
   //     ChangeList: IPhoaPicPropertyChangeList - список требуемых изменений свойств
   //===================================================================================================================

  TPhoaOp_InternalEditPicProps = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция редактирования ключевых слов изображения
   //   Params:
   //     Pics:        IPhotoAlbumPicList     - список изображений, чьи ключевые слова изменяются
   //     KeywordList: IPhotoAlbumKeywordList - список ключевых слов, которые нужно установить изображениям
   //===================================================================================================================

  TPhoaOp_InternalEditPicKeywords = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция применения преобразований изображения
   //   Params:
   //     Pic:         IPhotoAlbumPic     - изображение, к которому применяются новые преобразования
   //     NewRotation: Byte(TPicRotation) - typecasted поворот
   //     NewFlips:    Byte(TPicFlips)    - typecasted отражения
   //===================================================================================================================

  TPhoaOp_StoreTransform = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция добавления изображений
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, куда добавляются изображения
   //     Pics:  IPhotoAlbumPicList  - список добавляемых к проекту изображений
   //===================================================================================================================

  TPhoaOp_PicAdd = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления изображений (по списку их ID) из группы. Не выполняет поиск несвязанных изображений!
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, откуда удаляются изображения
   //     Pics:  IPhotoAlbumPicList  - список удаляемых изображений
   //===================================================================================================================

  TPhoaOp_InternalPicFromGroupRemoving = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция добавления изображений (по списку их ID) в группу
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, куда добавляются изображения
   //     Pics:  IPhotoAlbumPicList  - список добавляемых изображений
   //===================================================================================================================

  TPhoaOp_InternalPicToGroupAdding = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция копирования в буфер обмена нескольких изображений
   //===================================================================================================================

  TPhoaBaseOp_PicCopy = class(TBaseOperation)
    constructor Create(Pics: IPhotoAlbumPicList; ClipFormats: TPicClipboardFormats);
  end;

   //===================================================================================================================
   // Операция удаления/вырезания в буфер обмена нескольких изображений  (удаляет несвязанные изображения после удаления
   //   их из группы)
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, откуда удаляются изображения
   //     Pics:  IPhotoAlbumPicList  - список удаляемых изображений
   //===================================================================================================================

  TPhoaOp_PicDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция вставки нескольких изображений из буфера обмена
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, откуда удаляются изображения
   //===================================================================================================================

  TPhoaOp_PicPaste = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция редактирования свойств проекта
   //   Params:
   //     NewThWidth:     Integer - новая ширина эскиза
   //     NewThHeight:    Integer - новая высота эскиза
   //     NewThQuality:   Byte    - новое качество эскиза
   //     NewDescription: String  - новое описание проекта
   //===================================================================================================================

  TPhoaOp_ProjectEdit = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция [меж]групповой операции с изображениями
   //   Params:
   //     SourceGroup:  IPhotoAlbumPicGroup     - исходная группа с изображениями
   //     TargetGroup:  IPhotoAlbumPicGroup     - целевая группа
   //     Pics:         IPhoaPicList            - изображения для операции
   //     PicOperation: Byte(TPictureOperation) - выполняемая операция
   //===================================================================================================================

  TPhoaOp_PicOperation = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция сортировки изображений в одной группе
   //   Params:
   //     Group:    IPhotoAlbumPicGroup       - группа, в которой сортируются изображения
   //     Sortings: IPhotoAlbumPicSortingList - список сортировок
   //===================================================================================================================

  TPhoaOp_InternalGroupPicSort = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция сортировки изображений
   //   Params:
   //     Group:     IPhotoAlbumPicGroup       - группа, в которой сортируются изображения
   //     Sortings:  IPhotoAlbumPicSortingList - список сортировок
   //     Recursive: Boolean                   - если True, изображения сортируются также и в подгруппах
   //===================================================================================================================

  TPhoaOp_PicSort = class(TPhoaOperation)
  private
     // Рекурсивная (при bRecursive=True) процедура, создающая операции сортировки группы
    procedure AddGroupSortOp(Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean; var Changes: TPhoaOperationChanges);
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция перетаскивания группы
   //   Params:
   //     Group:          IPhotoAlbumPicGroup - перетаскиваемая группа
   //     NewParentGroup: IPhotoAlbumPicGroup - новая родительская группа для Group
   //     NewIndex:       Integer             - новый индекс в родительской группе
   //===================================================================================================================

  TPhoaOp_GroupDragAndDrop = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция перетаскивания изображений в группу
   //   Params:
   //     SourceGroup:  IPhotoAlbumPicGroup - исходная группа
   //     TargetGroup:  IPhotoAlbumPicGroup - целевая группа
   //     Pics:         IPhotoAlbumPicList  - перетаскиваемые изображения
   //     Copy:         Boolean             - если True, это операция копирования; если False - операция перемещения
   //===================================================================================================================

  TPhoaOp_PicDragAndDropToGroup = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция перетаскивания (переупорядочивания) изображений внутри группы
   //   Params:
   //     Group:    IPhotoAlbumPicGroup - группа с изображениями
   //     Pics:     IPhotoAlbumPicList  - перетаскиваемые изображения
   //     NewIndex: Integer             - индекс вставки изображений
   //===================================================================================================================

  TPhoaOp_PicDragAndDropInsideGroup = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция создания представления
   //   Params:
   //     Name:      String                     - наименование представления
   //     Groupings: IPhotoAlbumPicGroupingList - список группировок представления
   //     Sortings:  IPhotoAlbumPicSortingList  - список сортировок представления
   //===================================================================================================================

  TPhoaOp_ViewNew = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция изменения представления
   //   Params:
   //     View:      IPhotoAlbumView            - изменяемое представление
   //     Name:      String                     - новое наименование представления
   //     Groupings: IPhotoAlbumPicGroupingList - новый список группировок представления
   //     Sortings:  IPhotoAlbumPicSortingList  - новый список сортировок представления
   //       Если Groupings=nil и Sortings=nil, значит, это просто переименование представления
   //===================================================================================================================

  TPhoaOp_ViewEdit = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция удаления представления
   //   Params:
   //     <none>
   //===================================================================================================================

  TPhoaOp_ViewDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция создания группы фотоальбома из представления
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, куда помещать папки представления
   //===================================================================================================================

  TPhoaOp_ViewMakeGroup = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(var Changes: TPhoaOperationChanges); override;
  end;

resourcestring
  SPhoaOpErrMsg_ParamNotFound         = 'Operation parameter named "%s" not found';
  SPhoaOpErrMsg_ParamTypeMismatch     = 'Operation parameter "%s" type mismatch. Expected: "%s", actual: "%s"';
  SPhoaOpErrMsg_CannotObtainParamIntf = 'Operation parameter "%s" doesn''t implement required interface';

   // Создаёт экземпляр IPhoaOperationParams и заполняет его параметрами (описания параметров должны идти в Params
   //   парами: [имя1, значение1, имя2, значение2, ...])
  function  NewPhoaOperationParams(const Params: Array of Variant): IPhoaOperationParams;

  function  NewPhoaPicPropertyChangeList: IPhoaPicPropertyChangeList;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses
  TypInfo, Clipbrd,
  VirtualDataObject, GR32,
  phUtils, phGraphics, ConsVars, phSettings, Variants;

   // Запись содержимого IPhotoAlbumPicGroupingList в Undo-файл
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

   // Чтение содержимого IPhotoAlbumPicGroupingList из Undo-файла
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

   // Запись содержимого IPhotoAlbumPicSortingList в Undo-файл
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

   // Чтение содержимого IPhotoAlbumPicSortingList из Undo-файла
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
   // TPhoaOperationParams
   //===================================================================================================================
type
  TPhoaOperationParams = class(TInterfacedObject, IPhoaOperationParams)
  private
     // Сам список
    FList: TStringList;
     // Удаляет параметр по индексу
    procedure Delete(iIndex: Integer);
     // Возвращает значение параметра по имени. Если такого параметра нет, вызывает Exception
    function  GetValueStrict(const sName: String): Variant;
     // Проверяет тип значения. При несоответствии вызывает Exception
    procedure CheckVarType(const sName: String; const v: Variant; RequiredType: TVarType);
     // IPhoaOperationParams
    procedure ObtainValIntf(const sName: String; GUID: TGUID; out Intf; bRequired: Boolean = True);
    function  GetCount: Integer;
    function  GetNames(Index: Integer): String;
    function  GetValBool(const sName: String): Boolean;
    function  GetValByte(const sName: String): Byte;
    function  GetValInt(const sName: String): Integer;
    function  GetValStr(const sName: String): String;
    function  GetValues(const sName: String): Variant;
    function  GetValuesByIndex(Index: Integer): Variant;
    procedure SetValues(const sName: String; const Value: Variant);
    procedure SetValuesByIndex(Index: Integer; const Value: Variant);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  procedure TPhoaOperationParams.CheckVarType(const sName: String; const v: Variant; RequiredType: TVarType);
  begin
    if VarType(v)<>RequiredType then PhoaException(SPhoaOpErrMsg_ParamTypeMismatch, [sName, VarTypeAsText(RequiredType), VarTypeAsText(VarType(v))]);
  end;

  constructor TPhoaOperationParams.Create;
  begin
    inherited Create;
    FList := TStringList.Create;
    FList.Sorted := True;
  end;

  procedure TPhoaOperationParams.Delete(iIndex: Integer);
  begin
    Dispose(PVariant(FList.Objects[iIndex]));
    FList.Delete(iIndex);
  end;

  destructor TPhoaOperationParams.Destroy;
  var i: Integer;
  begin
     // Стираем список
    for i := FList.Count-1 downto 0 do Delete(i);
    inherited Destroy;
  end;

  function TPhoaOperationParams.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhoaOperationParams.GetNames(Index: Integer): String;
  begin
    Result := FList[Index];
  end;

  function TPhoaOperationParams.GetValBool(const sName: String): Boolean;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varBoolean);
    Result := v;
  end;

  function TPhoaOperationParams.GetValByte(const sName: String): Byte;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varByte);
    Result := v;
  end;

  function TPhoaOperationParams.GetValInt(const sName: String): Integer;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varInteger);
    Result := v;
  end;

  function TPhoaOperationParams.GetValStr(const sName: String): String;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varString);
    Result := v;
  end;

  function TPhoaOperationParams.GetValues(const sName: String): Variant;
  var idx: Integer;
  begin
    idx := FList.IndexOf(sName);
    if idx<0 then Result := Unassigned else Result := GetValuesByIndex(idx);
  end;

  function TPhoaOperationParams.GetValuesByIndex(Index: Integer): Variant;
  begin
    Result := PVariant(FList.Objects[Index])^;
  end;

  function TPhoaOperationParams.GetValueStrict(const sName: String): Variant;
  begin
    Result := GetValues(sName);
    if VarIsEmpty(Result) then PhoaException(SPhoaOpErrMsg_ParamNotFound, [sName]);
  end;

  procedure TPhoaOperationParams.ObtainValIntf(const sName: String; GUID: TGUID; out Intf; bRequired: Boolean = True);
  var v: Variant;
  begin
    IInterface(Intf) := nil;
    if bRequired then v := GetValueStrict(sName) else v := GetValues(sName);
    if not VarIsEmpty(v) and not VarSupports(v, GUID, Intf) then PhoaException(SPhoaOpErrMsg_CannotObtainParamIntf, [sName]);
  end;

  procedure TPhoaOperationParams.SetValues(const sName: String; const Value: Variant);
  var
    idx: Integer;
    p: PVariant;
  begin
    idx := FList.IndexOf(sName);
    if idx<0 then begin
      if not VarIsEmpty(Value) then begin
        New(p);
        SetValuesByIndex(FList.AddObject(sName, Pointer(p)), Value);
      end;
    end else
      SetValuesByIndex(idx, Value);
  end;

  procedure TPhoaOperationParams.SetValuesByIndex(Index: Integer; const Value: Variant);
  begin
    if VarIsEmpty(Value) then Delete(Index) else PVariant(FList.Objects[Index])^ := Value;
  end;

   //===================================================================================================================
   // TPhoaPicPropertyChangeList
   //===================================================================================================================
type
  TPhoaPicPropertyChangeList = class(TInterfacedObject, IPhoaPicPropertyChangeList)
  private
     // Собственно список
    FList: TList;
     // Удаляет элемент из списка
    procedure Delete(Index: Integer);
     // IPhoaPicPropertyChangeList
    function  Add(const sNewValue: String; Prop: TPicProperty): Integer;
    function  GetChangedProps: TPicProperties;
    function  GetCount: Integer;
    function  GetItems(Index: Integer): PPicPropertyChange;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  function TPhoaPicPropertyChangeList.Add(const sNewValue: String; Prop: TPicProperty): Integer;
  var p: PPicPropertyChange;
  begin
    New(p);
    Result := FList.Add(p);
    p^.sNewValue := sNewValue;
    p^.Prop      := Prop;
  end;

  constructor TPhoaPicPropertyChangeList.Create;
  begin
    inherited Create;
    FList := TList.Create;
  end;

  procedure TPhoaPicPropertyChangeList.Delete(Index: Integer);
  begin
    Dispose(GetItems(Index));
    FList.Delete(Index);
  end;

  destructor TPhoaPicPropertyChangeList.Destroy;
  var i: Integer;
  begin
    for i := FList.Count-1 downto 0 do Delete(i);
    FList.Free;
    inherited Destroy;
  end;

  function TPhoaPicPropertyChangeList.GetChangedProps: TPicProperties;
  var i: Integer;
  begin
    Result := [];
    for i := 0 to FList.Count-1 do Include(Result, GetItems(i).Prop);
  end;

  function TPhoaPicPropertyChangeList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhoaPicPropertyChangeList.GetItems(Index: Integer): PPicPropertyChange;
  begin
    Result := FList[Index];
  end;

   //===================================================================================================================
   // TPhoaOperation
   //===================================================================================================================

  constructor TPhoaOperation.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    inherited Create;
    FProject := AProject;
     // Регистрируемся в списке операций
    FList := AList;
    FList.Add(Self);
     // Запоминаем позицию Undo-данных
    FUndoDataPosition := FList.UndoFile.Position;
     // Выполняем операцию
    Perform(Params, Changes); 
  end;

  destructor TPhoaOperation.Destroy;
  begin
    FProject := nil;
    FOperations.Free;
    FList.Remove(Self);
    inherited Destroy;
  end;

  function TPhoaOperation.GetOperations: TPhoaOperations;
  begin
    if FOperations=nil then FOperations := TPhoaOperations.Create(List.UndoFile);
    Result := FOperations;
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

  procedure TPhoaOperation.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    { does nothing }
  end;

  procedure TPhoaOperation.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
    if FOperations<>nil then FOperations.UndoAll(Changes);
  end;

  procedure TPhoaOperation.SetOpGroup(Value: IPhotoAlbumPicGroup);
  begin
    FOpGroupID := Value.ID;
  end;

  procedure TPhoaOperation.SetParentOpGroup(Value: IPhotoAlbumPicGroup);
  begin
    FOpParentGroupID := Value.ID;
  end;

  procedure TPhoaOperation.Undo(var Changes: TPhoaOperationChanges);
  begin
    try
       // Позиционируем undo-файл в запомненную позицию
      UndoFile.BeginUndo(FUndoDataPosition);
      try
         // Откатываем изменения
        RollbackChanges(Changes);
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
    Result := FList.Add(Item);
  end;

  procedure TPhoaOperations.Clear;
  var i: Integer;
  begin
    for i := FList.Count-1 downto 0 do Delete(i);
  end;

  constructor TPhoaOperations.Create(AUndoFile: TPhoaUndoFile);
  begin
    inherited Create;
    FList := TList.Create;
    FUndoFile := AUndoFile;
  end;

  procedure TPhoaOperations.Delete(Index: Integer);
  begin
    GetItems(Index).Free;
  end;

  destructor TPhoaOperations.Destroy;
  begin
    Clear;
    FList.Free;
    inherited Destroy;
  end;

  function TPhoaOperations.GetCanUndo: Boolean;
  begin
    Result := FList.Count>0;
  end;

  function TPhoaOperations.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhoaOperations.GetItems(Index: Integer): TPhoaOperation;
  begin
    Result := TPhoaOperation(FList[Index]);
  end;

  function TPhoaOperations.Remove(Item: TPhoaOperation): Integer;
  begin
    Result := FList.Remove(Item);
  end;

  procedure TPhoaOperations.UndoAll(var Changes: TPhoaOperationChanges);
  var i: Integer;
  begin
    for i := FList.Count-1 downto 0 do GetItems(i).Undo(Changes);
  end;

   //===================================================================================================================
   // TPhoaUndo
   //===================================================================================================================

  function TPhoaUndo.Add(Item: TPhoaOperation): Integer;
  begin
    Result := inherited Add(Item);
     // Ограничиваем размер списка
    LimitCount; 
  end;

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
    FMaxCount := MaxInt;
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

  procedure TPhoaUndo.LimitCount;
  var i: Integer;
  begin
    for i := Count-1 downto FMaxCount do Delete(i);
  end;

  procedure TPhoaUndo.SetMaxCount(Value: Integer);
  begin
    if FMaxCount<>Value then begin
      FMaxCount := Value;
      LimitCount;
    end;
  end;

  procedure TPhoaUndo.SetNonUndoable;
  begin
    Clear;
    FSavepointOnEmpty := False;
  end;

  procedure TPhoaUndo.SetSavepoint;
  var i: Integer;
  begin
    for i := 0 to Count-1 do Items[i].FSavepoint := i=Count-1;
    FSavepointOnEmpty := Count=0;
  end;

   //===================================================================================================================
   // TPhoaOp_NewGroup
   //===================================================================================================================

  procedure TPhoaOp_GroupNew.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var Group, NewGroup: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
     // Создаём дочернюю группу
    NewGroup := NewPhotoAlbumPicGroup(Group, Project.RootGroupX.MaxGroupID+1);
    NewGroup.Text := ConstVal('SDefaultNewGroupName');
    OpParentGroup := Group;
    OpGroup       := NewGroup;
     // Возвращаем через Params созданную группу
    Params['NewGroup'] := NewGroup;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
  end;

  procedure TPhoaOp_GroupNew.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
     // Удаляем группу операции
    OpGroup.Owner := nil;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupRename
   //===================================================================================================================

  procedure TPhoaOp_GroupRename.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var Group: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, Changes);
     // Запоминаем данные отката
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    OpGroup := Group;
    UndoFile.WriteStr(Group.Text);
     // Выполняем операцию
    Group.Text := Params.ValStr['NewText'];
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
  end;

  procedure TPhoaOp_GroupRename.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
     // Получаем группу и восстанавливаем текст
    OpGroup.Text := UndoFile.ReadStr;
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupEdit
   //===================================================================================================================

  procedure TPhoaOp_GroupEdit.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var Group: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, Changes);
     // Запоминаем данные отката
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    UndoFile.WriteStr(Group.Description);
     // Выполняем операцию
    Group.Description := Params.ValStr['NewDescription'];
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
  end;

  procedure TPhoaOp_GroupEdit.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
     // Получаем группу и восстанавливаем описание
    OpGroup.Description := UndoFile.ReadStr;
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupDelete
   //===================================================================================================================

  procedure TPhoaOp_GroupDelete.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    inherited Perform(Params, Changes);
     // Удаляем группу (и подгруппы)
    TPhoaOp_InternalGroupDelete.Create(Operations, Project, Params, Changes);
     // Удаляем неиспользуемые изображения
    TPhoaOp_InternalUnlinkedPicsRemoving.Create(Operations, Project, Params, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalGroupDelete
   //===================================================================================================================

  procedure TPhoaOp_InternalGroupDelete.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    Group: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, Changes);
     // Запоминаем данные удаляемой группы
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    OpGroup       := Group;
    OpParentGroup := Group.OwnerX;
    UndoFile.WriteStr (Group.Text);
    UndoFile.WriteStr (Group.Description);
    UndoFile.WriteInt (Group.Index);
    UndoFile.WriteBool(Group.Expanded);
     // Записываем ID изображений и удаляем изображения из группы
    UndoFile.WriteInt(Group.Pics.Count);
    for i := 0 to Group.Pics.Count-1 do UndoFile.WriteInt(Group.Pics[i].ID);
    Group.PicsX.Clear;
     // Каскадно удаляем группы
    for i := Group.Groups.Count-1 downto 0 do
      TPhoaOp_InternalGroupDelete.Create(
        Operations,
        Project, 
        NewPhoaOperationParams(['Group', Group.GroupsX[i]]),
        Changes);
     // Удаляем группу
    Group.Owner := nil;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
  end;

  procedure TPhoaOp_InternalGroupDelete.RollbackChanges(var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
  begin
     // Восстанавливаем группу
    g := NewPhotoAlbumPicGroup(OpParentGroup, OpGroupID);
    g.Text        := UndoFile.ReadStr;
    g.Description := UndoFile.ReadStr;
    g.Index       := UndoFile.ReadInt;
    g.Expanded    := UndoFile.ReadBool;
     // Восстанавливаем изображения
    for i := 0 to UndoFile.ReadInt-1 do g.PicsX.Add(Project.Pics.ItemsByID[UndoFile.ReadInt], False);
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
     // Восстанавливаем вложенные группы
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalUnlinkedPicsRemoving
   //===================================================================================================================

  procedure TPhoaOp_InternalUnlinkedPicsRemoving.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    Pic: IPhotoAlbumPic;
  begin
    inherited Perform(Params, Changes);
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
         // Добавляем флаги изменений
        Include(Changes, pocProjectPicList);
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_InternalUnlinkedPicsRemoving.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
     // Читаем данные, пока не встретим стоп-флаг
    while UndoFile.ReadBool do
       // Создаём изображение
      with NewPhotoAlbumPic do begin
         // Загружаем данные
        RawData[PPAllProps] := UndoFile.ReadStr;
         // Кладём в список (ID уже загружен)
        PutToList(Project.PicsX, False);
         // Добавляем флаги изменений
        Include(Changes, pocProjectPicList);
      end;
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicProps
   //===================================================================================================================

  procedure TPhoaOp_InternalEditPicProps.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Pics: IPhotoAlbumPicList;
    ChangeList: IPhoaPicPropertyChangeList;
    iPic, iChg: Integer;
    Pic: IPhotoAlbumPic;
    ChangedProps: TPicProperties;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics',       IPhotoAlbumPicList,         Pics);
    Params.ObtainValIntf('ChangeList', IPhoaPicPropertyChangeList, ChangeList);
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
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
  end;

  procedure TPhoaOp_InternalEditPicProps.RollbackChanges(var Changes: TPhoaOperationChanges);
  var
    i, iPicID: Integer;
    ChangedProps: TPicProperties;
    sPicData: String;
  begin
     // Получаем набор изменённых свойств
    ChangedProps := IntToPicProps(UndoFile.ReadInt);
     // Возвращаем данные изменённых изображений
    for i := 0 to UndoFile.ReadInt-1 do begin
      iPicID   := UndoFile.ReadInt;
      sPicData := UndoFile.ReadStr;
      Project.PicsX.ItemsByIDX[iPicID].RawData[ChangedProps] := sPicData;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicKeywords
   //===================================================================================================================

  procedure TPhoaOp_InternalEditPicKeywords.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Pics: IPhotoAlbumPicList;
    KeywordList: IPhotoAlbumKeywordList;
    iPic, iCnt, iKwd, idxKeyword: Integer;
    Pic: IPhotoAlbumPic;
    sKeyword: String;
    pkd: PPhoaKeywordData;
    bKWSaved: Boolean;
    PicKeywords: IPhotoAlbumKeywordList;

     // Сохраняет ключевые слова изображения в FSavedKeywords, если этого ещё не сделано
    procedure SavePicKeywords;
    begin
      if not bKWSaved then begin
        UndoFile.WriteBool(True); // Признак записи ключевого слова (в противоположность стоп-флагу)
        UndoFile.WriteInt(Pic.ID);
        UndoFile.WriteStr(Pic.Keywords.CommaText);
        bKWSaved := True;
         // Добавляем флаги изменений
        Include(Changes, pocPicProps);
      end;
    end;

  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics',        IPhotoAlbumPicList,     Pics);
    Params.ObtainValIntf('KeywordList', IPhotoAlbumKeywordList, KeywordList);
     // Цикл по изображениям
    iCnt := Pics.Count;
    for iPic := 0 to iCnt-1 do begin
      Pic := Pics[iPic];
      PicKeywords := Pic.KeywordsX;
      bKWSaved := False;
       // Цикл по ключевым словам
      for iKwd := 0 to KeywordList.Count-1 do begin
        sKeyword := KeywordList[iKwd];
        pkd := KeywordList.KWData[iKwd];
        case pkd.Change of
           // КС не менялось. Проверяем изменение птицы
          pkcNone:
             // Если Grayed - менять нечего по определению. Если не выделено, и КС не содержится ни в одном изображении
             //   - менять нечего. Если выделено полностью, и КС содержится во всех изображениях - менять нечего.
             //   Иначе проверяем наличие КС в изображении
            if ((pkd.State=pksOff) and (pkd.iSelCount>0)) or ((pkd.State=pksOn) and (pkd.iSelCount<iCnt)) then begin
              idxKeyword := PicKeywords.IndexOf(sKeyword);
              case pkd.State of
                 // Надо убрать КС. Если оно есть - убираем
                pksOff:
                  if idxKeyword>=0 then begin
                    SavePicKeywords;
                    PicKeywords.Delete(idxKeyword);
                  end;
                 // Надо добавить КС. Если нет - добавляем
                pksOn:
                  if idxKeyword<0 then begin
                    SavePicKeywords;
                    PicKeywords.Add(sKeyword);
                  end;
              end;
            end;
           // Добавление нового КС. Если есть птица - надо добавить
          pkcAdd:
            if pkd.State=pksOn then begin
              SavePicKeywords;
              PicKeywords.Add(sKeyword);
            end;
           // КС менялось. Если только оно не полностью отсутствовало и отсутствует, ...
          pkcReplace:
            if (pkd.State<>pksOff) or (pkd.iSelCount>0) then begin
               // ... ищем старое КС и удаляем, ...
              idxKeyword := PicKeywords.IndexOf(pkd.sOldKeyword);
              if idxKeyword>=0 then begin
                SavePicKeywords;
                PicKeywords.Delete(idxKeyword);
              end;
               // ... если состояние pksOn - добавляем новое всем, если pksGrayed - добавляем только в те, где было старое
              if (pkd.State=pksOn) or ((pkd.State=pksGrayed) and (idxKeyword>=0)) then begin
                SavePicKeywords;
                PicKeywords.Add(sKeyword);
              end;
            end;
        end;
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_InternalEditPicKeywords.RollbackChanges(var Changes: TPhoaOperationChanges);
  var iPicID: Integer;
  begin
     // Возвращаем КС изменённым изображениям: крутим цикл, пока не встретим стоп-флаг
    while UndoFile.ReadBool do begin
      iPicID    := UndoFile.ReadInt;
      Project.PicsX.ItemsByIDX[iPicID].KeywordsM.CommaText := UndoFile.ReadStr;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_StoreTransform
   //===================================================================================================================

  procedure TPhoaOp_StoreTransform.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var Pic: IPhotoAlbumPic;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pic', IPhotoAlbumPic, Pic);
     // Сохраняем прежние свойства
    UndoFile.WriteInt(Pic.ID);
    UndoFile.WriteByte(Byte(Pic.Rotation));
    UndoFile.WriteByte(Byte(Pic.Flips));
     // Применяем новые свойства
    Pic.Rotation := TPicRotation(Params.ValByte['NewRotation']);
    Pic.Flips    := TPicFlips   (Params.ValByte['NewFlips']);
     // Добавляем флаги изменений
    Include(Changes, pocPicProps);
  end;

  procedure TPhoaOp_StoreTransform.RollbackChanges(var Changes: TPhoaOperationChanges);
  var Pic: IPhotoAlbumPic;
  begin
    Pic          := Project.PicsX.ItemsByIDX[UndoFile.ReadInt];
    Pic.Rotation := TPicRotation(UndoFile.ReadByte);
    Pic.Flips    := TPicFlips(Byte(UndoFile.ReadByte)); // Странный typecast, но иначе не компилируется
     // Добавляем флаги изменений
    Include(Changes, pocPicProps);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicAdd
   //===================================================================================================================

  procedure TPhoaOp_PicAdd.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    NewPics: IPhotoAlbumPicList;
    Group: IPhotoAlbumPicGroup;
    Pic, PicEx: IPhotoAlbumPic;
    bExisting, bAddedToGroup: Boolean;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('NewPics', IPhotoAlbumPicList,  NewPics);
    Params.ObtainValIntf('Group',   IPhotoAlbumPicGroup, Group);
     // Сохраняем данные для отката
    OpGroup := Group;
     // Перебираем все новые изображения
    UndoFile.WriteInt(NewPics.Count);
    for i := 0 to NewPics.Count-1 do begin
      Pic := NewPics[i];
       // Ищем уже существующее изображение с тем же файлом
      PicEx := Project.PicsX.ItemsByFileNameX[Pic.FileName];
      bExisting := PicEx<>nil;
       // Если есть такое изображение - игнорируем Pic
      if bExisting then
        Pic := PicEx
       // Иначе заносим в список, распределяя новый ID
      else begin
        Pic.PutToList(Project.PicsX, True);
        Include(Changes, pocProjectPicList);
      end;
       // Добавляем изображение в группу, если его там не было
      Group.PicsX.Add(Pic, True, bAddedToGroup);
      if bAddedToGroup then Include(Changes, pocGroupPicList);
       // Сохраняем ID изображения, флаг существования, флаг добавления в группу
      UndoFile.WriteInt(Pic.ID);
      UndoFile.WriteBool(bExisting);
      UndoFile.WriteBool(bAddedToGroup);
    end;
  end;

  procedure TPhoaOp_PicAdd.RollbackChanges(var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    i, iPicID: Integer;
    bExisting, bAddedToGroup: Boolean;
  begin
     // Получаем группу, куда добавляли изображения
    Group := OpGroup;
     // Считываем данные по каждому изображению
    for i := 0 to UndoFile.ReadInt-1 do begin
      iPicID        := UndoFile.ReadInt;
      bExisting     := UndoFile.ReadBool;
      bAddedToGroup := UndoFile.ReadBool;
       // Если было добавлено в группу - удаляем
      if bAddedToGroup then begin
        Group.PicsX.Remove(iPicID);
        Include(Changes, pocGroupPicList);
      end;
       // Если было добавлено новое изображение, удаляем и из списка проекта
      if not bExisting then begin
        Project.PicsX.ItemsByIDX[iPicID].Release;
        Include(Changes, pocProjectPicList);
      end;
    end;
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicFromGroupRemoving
   //===================================================================================================================

  procedure TPhoaOp_InternalPicFromGroupRemoving.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Pics:  IPhotoAlbumPicList;
    i, idx: Integer;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('Pics',  IPhotoAlbumPicList,  Pics);
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
         // Добавляем флаги изменений
        Include(Changes, pocGroupPicList);
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_InternalPicFromGroupRemoving.RollbackChanges(var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
    IIs: TIntegerList;
  begin
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
         // Добавляем флаги изменений
        Include(Changes, pocGroupPicList);
      end;
    finally
      IIs.Free;
    end;
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicToGroupAdding
   //===================================================================================================================

  procedure TPhoaOp_InternalPicToGroupAdding.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Pics:  IPhotoAlbumPicList;
    i: Integer;
    bAdded: Boolean;
    Pic: IPhoaPic;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('Pics',  IPhotoAlbumPicList,  Pics);
    OpGroup := Group;
     // Добавляем изображения в группу и в undo-файл
    for i := 0 to Pics.Count-1 do begin
      Pic := Pics[i];
      Group.PicsX.Add(Pic, True, bAdded);
      if bAdded then begin
        UndoFile.WriteBool(True); // Флаг продолжения
        UndoFile.WriteInt (Pic.ID);
         // Добавляем флаги изменений
        Include(Changes, pocGroupPicList);
      end;
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_InternalPicToGroupAdding.RollbackChanges(var Changes: TPhoaOperationChanges);
  var g: IPhotoAlbumPicGroup;
  begin
     // Удаляем добавленные изображения (считываем ID добавленных изображений из файла, пока не встретим стоп-флаг)
    g := OpGroup;
    while UndoFile.ReadBool do begin
      g.PicsX.Remove(UndoFile.ReadInt);
       // Добавляем флаги изменений
      Include(Changes, pocGroupPicList);
    end;
    inherited RollbackChanges(Changes);
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
   // TPhoaOp_PicDelete
   //===================================================================================================================

  procedure TPhoaOp_PicDelete.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    inherited Perform(Params, Changes);
     // Удаляем изображения из группы
    TPhoaOp_InternalPicFromGroupRemoving.Create(Operations, Project, Params, Changes);
     // Удаляем несвязанные изображения из фотоальбома
    TPhoaOp_InternalUnlinkedPicsRemoving.Create(Operations, Project, Params, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicPaste
   //===================================================================================================================

  procedure TPhoaOp_PicPaste.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    PastedPics: IPhotoAlbumPicList;
    hRec: THandle;
    ms: TMemoryStream;
    Streamer: TPhoaStreamer;
  begin
    inherited Perform(Params, Changes);
    if Clipboard.HasFormat(wClipbrdPicFormatID) then begin
       // Получаем параметры
      Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
      OpGroup := Group;
       // Создаём список изображений для вставки
      PastedPics := NewPhotoAlbumPicList(False);
       // Создаём временный поток
      ms := TMemoryStream.Create;
      try
         // Получаем данные из буфера обмена
        hRec := Clipboard.GetAsHandle(wClipbrdPicFormatID);
        ms.Write(GlobalLock(hRec)^, GlobalSize(hRec));
        GlobalUnlock(hRec);
        ms.Position := 0;
         // Создаём Streamer и загружаем изображения
        Streamer := TPhoaStreamer.Create(ms, psmRead, '');
        try
          PastedPics.StreamerLoad(Streamer);
        finally
          Streamer.Free;
        end;
      finally
        ms.Free;
      end;
       // Создаём дочернюю операцию добавления изображений
      TPhoaOp_PicAdd.Create(Operations, Project, NewPhoaOperationParams(['Group', Group, 'Pics', PastedPics]), Changes);
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_ProjectEdit
   //===================================================================================================================

  procedure TPhoaOp_ProjectEdit.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    inherited Perform(Params, Changes);
     // Сохраняем старые свойства
    UndoFile.WriteInt (Project.ThumbnailSize.cx);
    UndoFile.WriteInt (Project.ThumbnailSize.cy);
    UndoFile.WriteByte(Project.ThumbnailQuality);
    UndoFile.WriteStr (Project.Description);
     // Выполняем операцию
    Project.ThumbnailSize    := Size(Params.ValInt['NewThWidth'], Params.ValInt['NewThHeight']);
    Project.ThumbnailQuality := Params.ValByte['NewThQuality'];
    Project.Description      := Params.ValStr ['NewDescription'];
     // Добавляем флаги изменений
    Include(Changes, pocProjectProps);
  end;

  procedure TPhoaOp_ProjectEdit.RollbackChanges(var Changes: TPhoaOperationChanges);
  var Sz: TSize;
  begin
     // Восстанавливаем свойства фотоальбома
    Sz.cx   := UndoFile.ReadInt;
    Sz.cy   := UndoFile.ReadInt;
    Project.ThumbnailSize    := Sz;
    Project.ThumbnailQuality := UndoFile.ReadByte;
    Project.Description      := UndoFile.ReadStr;
     // Добавляем флаги изменений
    Include(Changes, pocProjectProps);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicOperation
   //===================================================================================================================

  procedure TPhoaOp_PicOperation.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    SourceGroup, TargetGroup: IPhotoAlbumPicGroup;
    Pics: IPhoaPicList;
    i: Integer;
    IntersectPics: IPhoaMutablePicList;
    Pic: IPhoaPic;
    PicOperation: TPictureOperation;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('SourceGroup', IPhotoAlbumPicGroup, SourceGroup);
    Params.ObtainValIntf('TargetGroup', IPhotoAlbumPicGroup, TargetGroup);
    Params.ObtainValIntf('Pics',        IPhoaPicList,        Pics);
    PicOperation := TPictureOperation(Params.ValByte['PicOperation']);
     // Копирование/перемещение: копируем выделенные изображения
    if PicOperation in [popMoveToTarget, popCopyToTarget] then
      TPhoaOp_InternalPicToGroupAdding.Create(
        Operations,
        Project,
        NewPhoaOperationParams(['Group', TargetGroup, 'Pics', Pics]),
        Changes);
     // Если перемещение - удаляем выделенные изображения из исходной группы
    if PicOperation=popMoveToTarget then
      TPhoaOp_InternalPicFromGroupRemoving.Create(
        Operations,
        Project,
        NewPhoaOperationParams(['Group', SourceGroup,'Pics', Pics]),
        Changes);
     // Удаление выделенных изображений из указанной группы
    if PicOperation=popRemoveFromTarget then
      TPhoaOp_InternalPicFromGroupRemoving.Create(
        Operations,
        Project,
        NewPhoaOperationParams(['Group', TargetGroup, 'Pics', Pics]),
        Changes);
     // Оставить только выделенные изображения в указанной группе
    if PicOperation=popIntersectTarget then begin
      IntersectPics := NewPhotoAlbumPicList(False);
      for i := 0 to TargetGroup.Pics.Count-1 do begin
        Pic := TargetGroup.Pics[i];
        if Pics.IndexOfID(Pic.ID)<0 then IntersectPics.Add(Pic, False);
      end;
      if IntersectPics.Count>0 then begin
        TPhoaOp_InternalPicFromGroupRemoving.Create(
          Operations,
          Project,
          NewPhoaOperationParams(['Group', TargetGroup, 'Pics', IntersectPics]),
          Changes);
        TPhoaOp_InternalUnlinkedPicsRemoving.Create(Operations, Project, nil, Changes);
      end;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalGroupPicSort
   //===================================================================================================================

  procedure TPhoaOp_InternalGroupPicSort.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Sortings: IPhotoAlbumPicSortingList;
    i: Integer;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group',    IPhotoAlbumPicGroup,       Group);
    Params.ObtainValIntf('Sortings', IPhotoAlbumPicSortingList, Sortings);
     // Запоминаем группу
    OpGroup := Group;
     // Запоминаем порядок следования ID изображений в группе
    UndoFile.WriteInt(Group.Pics.Count);
    for i := 0 to Group.Pics.Count-1 do UndoFile.WriteInt(Group.Pics[i].ID);
     // Сортируем изображения в группе
    Group.PicsX.SortingsSort(Sortings);
     // Добавляем флаги изменений
    Include(Changes, pocGroupPicList);
  end;

  procedure TPhoaOp_InternalGroupPicSort.RollbackChanges(var Changes: TPhoaOperationChanges);
  var i: Integer;
  begin
     // Восстанавливаем старый порядок следования ID изображений в группе
    OpGroup.PicsX.Clear;
    for i := 0 to UndoFile.ReadInt-1 do OpGroup.PicsX.Add(Project.Pics.ItemsByID[UndoFile.ReadInt], False);
     // Добавляем флаги изменений
    Include(Changes, pocGroupPicList);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicSort
   //===================================================================================================================

  procedure TPhoaOp_PicSort.AddGroupSortOp(Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean; var Changes: TPhoaOperationChanges);
  var i: Integer;
  begin
     // Сортируем изображения в группе
    TPhoaOp_InternalGroupPicSort.Create(Operations, Project, NewPhoaOperationParams(['Group', Group, 'Sortings', Sortings]), Changes);
     // При необходимости сортируем и в подгруппах
    if bRecursive then
      for i := 0 to Group.Groups.Count-1 do AddGroupSortOp(Group.GroupsX[i], Sortings, True, Changes);
  end;

  procedure TPhoaOp_PicSort.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Sortings: IPhotoAlbumPicSortingList;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group',    IPhotoAlbumPicGroup,       Group);
    Params.ObtainValIntf('Sortings', IPhotoAlbumPicSortingList, Sortings);
     // Запускаем сортировку
    AddGroupSortOp(Group, Sortings, Params.ValBool['Recursive'], Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupDragAndDrop
   //===================================================================================================================

  procedure TPhoaOp_GroupDragAndDrop.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group, NewParentGroup, gOldParent: IPhotoAlbumPicGroup;
    iNewIndex: Integer;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group',          IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('NewParentGroup', IPhotoAlbumPicGroup, NewParentGroup);
    iNewIndex := Params.ValInt['NewIndex'];
     // Запоминаем данные отката
    gOldParent := Group.OwnerX;
    UndoFile.WriteInt(Group.Index);
     // Перемещаем группу
    Group.Owner := NewParentGroup;
    if iNewIndex>=0 then Group.Index := iNewIndex; // Индекс -1 означает добавление последним ребёнком
     // Запоминаем группы (ID прежнего родителя и ID группы)
    OpParentGroup := gOldParent;
    OpGroup       := Group;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
  end;

  procedure TPhoaOp_GroupDragAndDrop.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
     // Восстанавливаем положение группы
    with OpGroup do begin
      Owner := OpParentGroup;
      Index := UndoFile.ReadInt;
    end;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDragAndDropToGroup
   //===================================================================================================================

  procedure TPhoaOp_PicDragAndDropToGroup.Perform( Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    SourceGroup, TargetGroup: IPhotoAlbumPicGroup;
    Pics: IPhoaPicList;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('SourceGroup', IPhotoAlbumPicGroup, SourceGroup);
    Params.ObtainValIntf('TargetGroup', IPhotoAlbumPicGroup, TargetGroup);
    Params.ObtainValIntf('Pics',        IPhotoAlbumPicList,  Pics);
     // Выполняем операцию
    TPhoaOp_InternalPicToGroupAdding.Create(
      Operations,
      Project,
      NewPhoaOperationParams(['Group', TargetGroup, 'Pics', Pics]),
      Changes);
    if not Params.ValBool['Copy'] then
      TPhoaOp_InternalPicFromGroupRemoving.Create(
        Operations,
        Project,
        NewPhoaOperationParams(['Group', SourceGroup, 'Pics', Pics]),
        Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDragAndDropInsideGroup
   //===================================================================================================================

  procedure TPhoaOp_PicDragAndDropInsideGroup.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Pics: IPhoaPicList;
    i, idxOld, iNewIndex: Integer;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('Pics',  IPhotoAlbumPicList,  Pics);
    iNewIndex := Params.ValInt['NewIndex'];
     // Запоминаем группу
    OpGroup := Group;
     // Выполняем операцию
    for i := 0 to Pics.Count-1 do begin
       // -- Пишем признак продолжения
      UndoFile.WriteBool(True);
       // -- Запоминаем индексы
      idxOld := Group.Pics.IndexOfID(Pics[i].ID);
      if idxOld<iNewIndex then Dec(iNewIndex);
      UndoFile.WriteInt(idxOld);
      UndoFile.WriteInt(iNewIndex);
       // -- Перемещаем изображение на новое место
      Group.PicsX.Move(idxOld, iNewIndex);
      Inc(iNewIndex);
       // Добавляем флаги изменений
      Include(Changes, pocGroupPicList);
    end;
     // Пишем стоп-флаг
    UndoFile.WriteBool(False);
  end;

  procedure TPhoaOp_PicDragAndDropInsideGroup.RollbackChanges(var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
    Indexes: TIntegerList;
  begin
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
     // Добавляем флаги изменений
    Include(Changes, pocGroupPicList);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewNew
   //===================================================================================================================

  procedure TPhoaOp_ViewNew.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Groupings: IPhotoAlbumPicGroupingList;
    Sortings: IPhotoAlbumPicSortingList;
    View: IPhotoAlbumView;
    iNewViewIndex: Integer;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Groupings', IPhotoAlbumPicGroupingList, Groupings);
    Params.ObtainValIntf('Sortings',  IPhotoAlbumPicSortingList,  Sortings);
     // Сохраняем предыдущий текущий индекс представления проекта
    UndoFile.WriteInt(Project.ViewIndex);
     // Выполняем операцию
    View := NewPhotoAlbumView(Project.ViewsX);
    View.Name := Params.ValStr['Name'];
    View.GroupingsX.Assign(Groupings);
    View.SortingsX.Assign(Sortings);
     // Сохраняем новый индекс представления
    iNewViewIndex := View.Index;
    UndoFile.WriteInt(iNewViewIndex);
     // Перегружаем список
    Project.ViewIndex := iNewViewIndex;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
  end;

  procedure TPhoaOp_ViewNew.RollbackChanges(var Changes: TPhoaOperationChanges);
  var iPrevViewIndex, iNewViewIndex: Integer;
  begin
     // Получаем сохранённые данные
    iPrevViewIndex := UndoFile.ReadInt;
    iNewViewIndex  := UndoFile.ReadInt;
     // Удаляем представление
    Project.ViewsX.Delete(iNewViewIndex);
     // Восстанавливаем прежнее выбранное представление
    Project.ViewIndex := iPrevViewIndex;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewEdit
   //===================================================================================================================

  procedure TPhoaOp_ViewEdit.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    View: IPhotoAlbumView;
    Groupings: IPhotoAlbumPicGroupingList;
    Sortings: IPhotoAlbumPicSortingList;
    bWriteGroupings, bWriteSortings: Boolean;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('View',      IPhotoAlbumView,            View);
    Params.ObtainValIntf('Groupings', IPhotoAlbumPicGroupingList, Groupings, False);
    Params.ObtainValIntf('Sortings',  IPhotoAlbumPicSortingList,  Sortings,  False);
     // Сохраняем данные отката и применяем изменения
    UndoFile.WriteStr(View.Name);
    View.Name := Params.ValStr['Name'];
     // Запоминаем новый индекс представления (ПОСЛЕ присвоения имени, т.к. оно изменяет позицию представления в списке)
    UndoFile.WriteInt(View.Index);
     // Список группировок создаём и сохраняем, если он есть
    bWriteGroupings := Groupings<>nil;
    UndoFile.WriteBool(bWriteGroupings); // Признак наличия группировок
    if bWriteGroupings then begin
      UndoWriteGroupings(UndoFile, View.GroupingsX);
      View.GroupingsX.Assign(Groupings);
      View.Invalidate;
    end;
     // Список сортировок создаём и сохраняем, если он есть
    bWriteSortings := Sortings<>nil;
    UndoFile.WriteBool(bWriteSortings); // Признак наличия сортировок
    if bWriteSortings then begin
      UndoWriteSortings(UndoFile, View.SortingsX);
      View.SortingsX.Assign(Sortings);
      View.Invalidate;
    end;
     // Обновляем текущий индекс представления (мог поменяться после переименования представления)
    Project.ViewIndex := View.Index;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
  end;

  procedure TPhoaOp_ViewEdit.RollbackChanges(var Changes: TPhoaOperationChanges);
  var
    sViewName: String;
    iViewIndex: Integer;
    View: IPhotoAlbumView;
  begin
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
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewDelete
   //===================================================================================================================

  procedure TPhoaOp_ViewDelete.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var View: IPhotoAlbumView;
  begin
    inherited Perform(Params, Changes);
     // Сохраняем данные отката
    View := Project.CurrentViewX;
    UndoFile.WriteStr(View.Name);
    UndoWriteGroupings(UndoFile, View.GroupingsX);
    UndoWriteSortings (UndoFile, View.SortingsX);
     // Удаляем представление
    Project.ViewsX.Delete(Project.ViewIndex);
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
  end;

  procedure TPhoaOp_ViewDelete.RollbackChanges(var Changes: TPhoaOperationChanges);
  var View: IPhotoAlbumView;
  begin
      // Создаём представление
    View := NewPhotoAlbumView(Project.ViewsX);
    View.Name := UndoFile.ReadStr;
    UndoReadGroupings(UndoFile, View.GroupingsX);
    UndoReadSortings (UndoFile, View.SortingsX);
     // Активизируем представление
    Project.ViewIndex := View.Index;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewMakeGroup
   //===================================================================================================================

  procedure TPhoaOp_ViewMakeGroup.Perform(Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  var
    Group, ViewGroup: IPhotoAlbumPicGroup;
    View: IPhotoAlbumView;
  begin
    inherited Perform(Params, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
     // Получаем представление
    View := Project.CurrentViewX;
     // Создаём группы (изначально с нулевыми ID)
    ViewGroup := NewPhotoAlbumPicGroup(Group, 0);
    ViewGroup.Assign(View.RootGroup, False, True, True);
    ViewGroup.Text := View.Name;
     // Распределяем группам настоящие ID
    Project.RootGroupX.FixupIDs;
     // Запоминаем созданную (корневую) группу
    OpGroup := ViewGroup;
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
     // Добавляем флаги изменений
    Changes := Changes+[pocViewIndex, pocGroupStructure];
  end;

  procedure TPhoaOp_ViewMakeGroup.RollbackChanges(var Changes: TPhoaOperationChanges);
  begin
     // Удаляем корневую группу копии представления
    OpGroup.Owner := nil;
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
     // Добавляем флаги изменений
    Changes := Changes+[pocViewIndex, pocGroupStructure];
    inherited RollbackChanges(Changes);
  end;

   //===================================================================================================================

  function NewPhoaOperationParams(const Params: Array of Variant): IPhoaOperationParams;
  var i, idxParam: Integer;
  begin
    Result := TPhoaOperationParams.Create;
    if Length(Params)>0 then
      for i := 0 to High(Params) div 2 do begin
        idxParam := i*2;
        Result[Params[idxParam]] := Params[idxParam+1];
      end;
  end;

  function NewPhoaPicPropertyChangeList: IPhoaPicPropertyChangeList;
  begin
    Result := TPhoaPicPropertyChangeList.Create;
  end;

end.
