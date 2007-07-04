//**********************************************************************************************************************
//  $Id: phOps.pas,v 1.27 2007-07-04 18:48:36 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phOps;

interface
uses
  Windows, Messages, SysUtils, Classes, Contnrs,
  TntSysUtils,
  phObj, phPhoa, phIntf, phMutableIntf, phNativeIntf;

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
   // Интерфейс параметров операции PhoA
   //===================================================================================================================

  IPhoaOperationParams = interface(IInterface)
    ['{4AF24473-A666-4373-9BD6-DC0868647DC9}']
     // Получает интерфейсное значение параметра. Если bRequired=True, то проверяет наличие параметра и при отсутствии
     //   вызывает exception; при bRequired=False и отсутствии параметра в Intf возвращает nil. Если параметр есть,
     //   обязательно проверяет доступность интерфейса, при его отсутствии вызвает exception
    procedure ObtainValIntf(const sName: AnsiString; GUID: TGUID; out Intf; bRequired: Boolean = True);
     // Prop handlers
    function  GetCount: Integer;
    function  GetNames(Index: Integer): AnsiString;
    function  GetValBool(const sName: AnsiString): Boolean;
    function  GetValByte(const sName: AnsiString): Byte;
    function  GetValInt(const sName: AnsiString): Integer;
    function  GetValStr(const sName: AnsiString): WideString;
    function  GetValues(const sName: AnsiString): Variant;
    function  GetValuesByIndex(Index: Integer): Variant;
    procedure SetValues(const sName: AnsiString; const Value: Variant);
    procedure SetValuesByIndex(Index: Integer; const Value: Variant);
     // Props
     // -- Количество параметров
    property Count: Integer read GetCount;
     // -- Наименования параметров по индексу
    property Names[Index: Integer]: AnsiString read GetNames;
     // -- Типизированные значения параметров с проверкой на существование и тип
    property ValBool[const sName: AnsiString]: Boolean    read GetValBool;
    property ValByte[const sName: AnsiString]: Byte       read GetValByte;
    property ValInt [const sName: AnsiString]: Integer    read GetValInt;
    property ValStr [const sName: AnsiString]: WideString read GetValStr;
     // -- Значения параметров по имени (при присваивании Unassigned параметр удаляется)
    property Values[const sName: AnsiString]: Variant read GetValues write SetValues; default;
     // -- Значения параметров по индексу (при присваивании Unassigned параметр удаляется)
    property ValuesByIndex[Index: Integer]: Variant read GetValuesByIndex write SetValuesByIndex;
  end;

   //===================================================================================================================
   // Список [требуемых] изменений свойств изображений
   //===================================================================================================================

  PPicPropertyChange = ^TPicPropertyChange;
  TPicPropertyChange = record
    vNewValue: Variant;
    Prop: TPicProperty;
  end;

  IPhoaPicPropertyChangeList = interface(IInterface)
    ['{13EEEA04-FF5A-42B3-861E-C0C7F5A8A334}']
     // Добавляет новую запись
    function  Add(const vNewValue: Variant; Prop: TPicProperty): Integer;
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
   // Список [требуемых] изменений файлов изображений
   //===================================================================================================================

  PPicFileChange = ^TPicFileChange;
  TPicFileChange = record
    Pic:        IPhotoAlbumPic; // Изображение
    wsFileName: WideString;     // Имя нового файла, сопоставляемого Pic
  end;

  IPhoaPicFileChangeList = interface(IInterface)
    ['{9B327389-E6BC-4297-845C-563002068720}']
     // Добавляет новую запись
    function  Add(Pic: IPhotoAlbumPic; const wsFileName: WideString): Integer;
     // Prop handlers
    function  GetCount: Integer;
    function  GetItems(Index: Integer): PPicFileChange;
     // Props
     // -- Количество элементов в списке
    property Count: Integer read GetCount;
     // -- Элементы списка по индексу
    property Items[Index: Integer]: PPicFileChange read GetItems; default;
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
    pocPicProps,         // Изменились свойства изображений
    pocNonUndoable);     // Внесённые изменения не предполагают отката
  TPhoaOperationChanges = set of TPhoaOperationChange;

   //===================================================================================================================
   // Базовая (абстрактная) операция фотоальбома, принадлежащая списку (буферу отката), которая может быть отменена
   //   Params:
   //     Project: IPhotoAlbumProject - проект
   //===================================================================================================================

  TPhoaOperationClass = class of TPhoaOperation;

  TPhoaOperation = class(TBaseOperation)
  private
     // Позиция данных отката операции в Undo-файле данных отката (UndoStream)
    FUndoDataPosition: Int64;
     // Prop storage
    FGUIStateUndoDataPosition: Int64;
    FList: TPhoaOperations;
    FOperations: TPhoaOperations;
    FOpGroupID: Integer;
    FOpParentGroupID: Integer;
    FProject: IPhotoAlbumProject;
     // Prop handlers
    function  GetOperations: TPhoaOperations;
    function  GetOpGroup: IPhotoAlbumPicGroup;
    function  GetParentOpGroup: IPhotoAlbumPicGroup;
    procedure SetOpGroup(Value: IPhotoAlbumPicGroup);
    procedure SetParentOpGroup(Value: IPhotoAlbumPicGroup);
  protected
     // Prop storage
    FSavepoint: Boolean;
     // Основная процедура выполнения операции, вызывается при её создании. В базовом классе не делает ничего
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); virtual;
     // Основная процедура отката изменений, внесённых операцией. В базовом классе откатывает подчинённые операции
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); virtual;
     // Создаёт новый экземпляр операции класса OpClass и добавляет в список дочерних операций
    procedure AddChild(OpClass: TPhoaOperationClass; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
     // Props
     // -- Возвращает группу, соответствующую OpGroupID
    property OpGroup: IPhotoAlbumPicGroup read GetOpGroup write SetOpGroup;
     // -- Возвращает группу, соответствующую OpParentGroupID
    property OpParentGroup: IPhotoAlbumPicGroup read GetParentOpGroup write SetParentOpGroup;
  public
    constructor Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges); virtual;
    destructor Destroy; override;
     // Процедура, откатывающая изменения, внесённые операцией (вызовом RollbackChanges()), и уничтожающая
     //   объект-операцию. В Changes добавляет набор флагов внесённых в процессе отката изменений
    procedure Undo(var Changes: TPhoaOperationChanges);
     // Наименование операции
    function Name: WideString;
     // Props
     // -- Позиция в Undo-файле данных о состоянии интерфейса
    property GUIStateUndoDataPosition: Int64 read FGUIStateUndoDataPosition write FGUIStateUndoDataPosition;
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
  end;

   //===================================================================================================================
   // Фабрика операций (изготавливает новые экземпляры операций по имени операции)
   //===================================================================================================================

  IPhoaOperationFactory = interface(IInterface)
    ['{BE27C06A-6F28-4A8E-9F41-20350D45D8AC}']
     // Регистрирует новый класс операций в списке
    procedure RegisterOpClass(const sOpName: AnsiString; OpClass: TPhoaOperationClass);
     // Инстанцирует и возвращает новую операцию
    function  NewOperation(const sOpName: AnsiString; AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges): TPhoaOperation;
     // Prop handlers
    function  GetClassByName(const sOpName: AnsiString): TPhoaOperationClass;
     // Props
     // -- Возвращает класс по имени операции. Если нет такого, вызывает Exception
    property ClassByName[const sOpName: AnsiString]: TPhoaOperationClass read GetClassByName;
  end;

   //===================================================================================================================
   // Список сделанных операций
   //===================================================================================================================

  TPhoaOperations = class(TObject)
  private
     // Собственно список
    FList: TList;
     // Prop storage
    FUndoStream: IPhoaUndoDataStream;
     // Prop handlers
    function  GetItems(Index: Integer): TPhoaOperation;
    function  GetCanUndo: Boolean;
    function  GetCount: Integer;
  protected
     // Откатывает весь буфер (предназначено для вторичных буферов множественных операций)
    procedure UndoAll(var Changes: TPhoaOperationChanges);
  public
    constructor Create(AUndoStream: IPhoaUndoDataStream);
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
     // -- Хранилище данных отката
    property UndoStream: IPhoaUndoDataStream read FUndoStream;
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
    function  GetLastOpName: WideString;
    function  GetIsUnmodified: Boolean;
    procedure SetMaxCount(Value: Integer);
  public
    constructor Create;
    function  Add(Item: TPhoaOperation): Integer; override;
    procedure Clear; override;
     // Устанавливает, что текущее состояние фотоальбома является сохранённым
    procedure SetSavepoint;
     // Очищает содержимое буфера отката и устанавливает состояние модифицированности проекта в bModified
    procedure SetNonUndoable(bModified: Boolean);
     // Props
     // -- Возвращает True, если текущее состояние буфера отката соответствует сохранённому состоянию фотоальбома
    property IsUnmodified: Boolean read GetIsUnmodified;
     // -- Возвращает наименование последней сделанной операции
    property LastOpName: WideString read GetLastOpName;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция переименования группы
   //   Params:
   //     Group:   IPhotoAlbumPicGroup - группа, которую переименовываем
   //     NewText: WideString          - новое наименование группы
   //===================================================================================================================

  TPhoaOp_GroupRename = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция редактирования свойств группы
   //   Params:
   //     Group:          IPhotoAlbumPicGroup - редактируемая группа
   //     NewText:        WideString          - новое наименование группы
   //     NewDescription: WideString          - новое описание группы
   //     NewIconData:    TPhoaRawData        - PNG-данные значка группы
   //===================================================================================================================

  TPhoaOp_GroupEdit = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция удаления группы (также вычищает несвязанные после удаления изображения)
   //   Params:
   //     Group: IPhotoAlbumPicGroup - удаляемая группа
   //===================================================================================================================

  TPhoaOp_GroupDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления группы, создаётся из TPhoaOp_GroupDelete, удаляет только группу (с подгруппами) и не
   //   заботится о несвязанных изображениях
   //   Params:
   //     Group: IPhotoAlbumPicGroup - удаляемая группа
   //===================================================================================================================

  TPhoaOp_InternalGroupDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления неиспользуемых изображений из проекта
   //   Params:
   //     <none>
   //===================================================================================================================

  TPhoaOp_InternalUnlinkedPicsRemoving = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Комплексная операция редактирования изображений, служит контейнером для операций:
   //  - TPhoaOp_InternalEditPicFiles
   //  - TPhoaOp_InternalEditPicProps
   //  - TPhoaOp_InternalEditPicKeywords
   //  - TPhoaOp_InternalEditPicToGroupBelonging
   //   Params:
   //     EditFilesOpParams:    IPhoaOperationParams - параметры для дочерней операции изменения свойств на странице
   //                                                  [свойств] файлов
   //     EditViewOpParams:     IPhoaOperationParams - параметры для дочерней операции изменения свойств на странице
   //                                                  просмотра
   //     EditDataOpParams:     IPhoaOperationParams - параметры для дочерней операции изменения свойств на странице
   //                                                  данных
   //     EditKeywordsOpParams: IPhoaOperationParams - параметры для дочерней операции изменения свойств на странице
   //                                                  ключевых слов
   //     EditGroupOpParams:    IPhoaOperationParams - параметры для дочерней операции изменения свойств на странице
   //                                                  принадлежности изображений группам
   //===================================================================================================================

  TPhoaOp_PicEdit = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция изменения файлов изображений
   //   Params:
   //     FileChangeList: IPhoaPicFileChangeList - список требуемых изменений файлов
   //===================================================================================================================

  TPhoaOp_InternalEditPicFiles = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция редактирования свойств изображения, кроме ключевых слов
   //   Params:
   //     Pics:       IPhotoAlbumPicList         - список изображений, чьи свойства изменяются
   //     ChangeList: IPhoaPicPropertyChangeList - список требуемых изменений свойств
   //===================================================================================================================

  TPhoaOp_InternalEditPicProps = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция редактирования ключевых слов изображения
   //   Params:
   //     Pics:        IPhotoAlbumPicList     - список изображений, чьи ключевые слова изменяются
   //     KeywordList: IPhotoAlbumKeywordList - список ключевых слов, которые нужно установить изображениям
   //===================================================================================================================

  TPhoaOp_InternalEditPicKeywords = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция изменения принадлежности изображений группам
   //   Params:
   //     Pics:             IPhotoAlbumPicList      - список изображений, чья принадлежность группам изменяется
   //     AddToGroups:      IPhotoAlbumPicGroupList - список групп, в которые необходимо добавить изображения Pics
   //     RemoveFromGroups: IPhotoAlbumPicGroupList - список групп, из которых необходимо удалить изображения Pics
   //===================================================================================================================

  TPhoaOp_InternalEditPicToGroupBelonging = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция добавления изображений
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, куда добавляются изображения
   //     Pics:  IPhotoAlbumPicList  - список добавляемых к проекту изображений
   //===================================================================================================================

  TPhoaOp_PicAdd = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления изображений из списка изображений проекта
   //   Params:
   //     Pics: IPhotoAlbumPicList - список удаляемых изображений
   //===================================================================================================================

  TPhoaOp_InternalPicFromProjectRemoving = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция удаления изображений (по списку их ID) из группы. Не выполняет поиск несвязанных изображений!
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, откуда удаляются изображения
   //     Pics:  IPhotoAlbumPicList  - список удаляемых изображений
   //===================================================================================================================

  TPhoaOp_InternalPicFromGroupRemoving = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция добавления изображений (по списку их ID) в группу
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, куда добавляются изображения
   //     Pics:  IPhotoAlbumPicList  - список добавляемых изображений
   //===================================================================================================================

  TPhoaOp_InternalPicToGroupAdding = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция удаления изображений из всех групп и фотоальбома
   //   Params:
   //     Pics: IPhotoAlbumPicList  - список удаляемых изображений
   //===================================================================================================================

  TPhoaOp_PicDeleteFromProject = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция удаления изображений из всех групп и удаления соответствующих файлов
   //   Params:
   //     Pics: IPhotoAlbumPicList  - список удаляемых изображений
   //===================================================================================================================

  TPhoaOp_PicDeleteWithFiles = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция вставки нескольких изображений из буфера обмена
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, откуда удаляются изображения
   //===================================================================================================================

  TPhoaOp_PicPaste = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция редактирования свойств проекта
   //   Params:
   //     NewThWidth:     Integer    - новая ширина эскиза
   //     NewThHeight:    Integer    - новая высота эскиза
   //     NewThQuality:   Byte       - новое качество эскиза
   //     NewDescription: WideString - новое описание проекта
   //===================================================================================================================

  TPhoaOp_ProjectEdit = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Внутренняя операция сортировки изображений в одной группе
   //   Params:
   //     Group:    IPhotoAlbumPicGroup       - группа, в которой сортируются изображения
   //     Sortings: IPhotoAlbumPicSortingList - список сортировок
   //===================================================================================================================

  TPhoaOp_InternalGroupPicSort = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
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
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция создания представления
   //   Params:
   //     Name:             WideString                 - наименование представления
   //     FilterExpression: WideString                 - выражение фильтра изображений
   //     Groupings:        IPhotoAlbumPicGroupingList - список группировок представления
   //     Sortings:         IPhotoAlbumPicSortingList  - список сортировок представления
   //===================================================================================================================

  TPhoaOp_ViewNew = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция изменения представления
   //   Params:
   //     View:             IPhotoAlbumView            - изменяемое представление
   //     Name:             WideString                 - новое наименование представления
   //     FilterExpression: WideString                 - новое выражение фильтра изображений
   //     Groupings:        IPhotoAlbumPicGroupingList - новый список группировок представления
   //     Sortings:         IPhotoAlbumPicSortingList  - новый список сортировок представления
   //       Если Groupings=nil и Sortings=nil, значит, это просто переименование представления
   //===================================================================================================================

  TPhoaOp_ViewEdit = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция удаления представления
   //   Params:
   //     <none>
   //===================================================================================================================

  TPhoaOp_ViewDelete = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

   //===================================================================================================================
   // Операция создания группы фотоальбома из представления
   //   Params:
   //     Group: IPhotoAlbumPicGroup - группа, куда помещать папки представления
   //===================================================================================================================

  TPhoaOp_ViewMakeGroup = class(TPhoaOperation)
  protected
    procedure Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
    procedure RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges); override;
  end;

var
   // Глобальный экземпляр фабрики операций
  OperationFactory: IPhoaOperationFactory;

resourcestring
  SPhoaOpErrMsg_ParamNotFound         = 'Operation parameter named "%s" not found';
  SPhoaOpErrMsg_ParamTypeMismatch     = 'Operation parameter "%s" type mismatch. Expected: "%s", actual: "%s"';
  SPhoaOpErrMsg_CannotObtainParamIntf = 'Operation parameter "%s" doesn''t implement required interface';
  SPhoaOpErrMsg_OperationNotFound     = 'Operation "%s" not found';

   // Создаёт экземпляр IPhoaOperationParams и заполняет его параметрами (описания параметров должны идти в Params
   //   парами: [имя1, значение1, имя2, значение2, ...])
  function  NewPhoaOperationParams(const Params: Array of Variant): IPhoaOperationParams;
  function  NewPhoaPicPropertyChangeList: IPhoaPicPropertyChangeList;
  function  NewPhoaPicFileChangeList: IPhoaPicFileChangeList;
  function  NewPhoaUndoDataStream: IPhoaUndoDataStream;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses
  TypInfo, TntClipBrd,
  VirtualDataObject, GR32, DKLang,
  phUtils, phGraphics, ConsVars, phSettings, Variants;

   // Запись содержимого IPhotoAlbumPicGroupingList в Undo-файл
  procedure UndoWriteGroupings(UndoStream: IPhoaUndoDataStream; Groupings: IPhotoAlbumPicGroupingList);
  var
    i: Integer;
    Grouping: IPhotoAlbumPicGrouping;
  begin
    UndoStream.WriteInt(Groupings.Count);
    for i := 0 to Groupings.Count-1 do begin
      Grouping := Groupings[i];
      UndoStream.WriteByte(Byte(Grouping.Prop));
      UndoStream.WriteBool(Grouping.UnclassifiedInOwnFolder);
    end;
  end;

   // Чтение содержимого IPhotoAlbumPicGroupingList из Undo-файла
  procedure UndoReadGroupings(UndoStream: IPhoaUndoDataStream; Groupings: IPhotoAlbumPicGroupingList);
  var
    i: Integer;
    Grouping: IPhotoAlbumPicGrouping;
  begin
    Groupings.Clear;
    for i := 0 to UndoStream.ReadInt-1 do begin
      Grouping := NewPhotoAlbumPicGrouping;
      Grouping.Prop                    := TPicGroupByProperty(UndoStream.ReadByte);
      Grouping.UnclassifiedInOwnFolder := UndoStream.ReadBool;
      Groupings.Add(Grouping);
    end;
  end;

   // Запись содержимого IPhotoAlbumPicSortingList в Undo-файл
  procedure UndoWriteSortings(UndoStream: IPhoaUndoDataStream; Sortings: IPhotoAlbumPicSortingList);
  var
    i: Integer;
    Sorting: IPhotoAlbumPicSorting;
  begin
    UndoStream.WriteInt(Sortings.Count);
    for i := 0 to Sortings.Count-1 do begin
      Sorting := Sortings[i];
      UndoStream.WriteByte(Byte(Sorting.Prop));
      UndoStream.WriteByte(Byte(Sorting.Direction));
    end;
  end;

   // Чтение содержимого IPhotoAlbumPicSortingList из Undo-файла
  procedure UndoReadSortings(UndoStream: IPhoaUndoDataStream; Sortings: IPhotoAlbumPicSortingList);
  var
    i: Integer;
    Sorting: IPhotoAlbumPicSorting;
  begin
    Sortings.Clear;
    for i := 0 to UndoStream.ReadInt-1 do begin
      Sorting := NewPhotoAlbumPicSorting;
      Sorting.Prop      := TPicProperty(UndoStream.ReadByte);
      Sorting.Direction := TPhoaSortDirection(UndoStream.ReadByte);
      Sortings.Add(Sorting);
    end;
  end;

   //===================================================================================================================
   // TPhoaOperationParams
   //===================================================================================================================
type
  PPhoaOperationParam = ^TPhoaOperationParam;
  TPhoaOperationParam = record
    sName:  AnsiString; // Наименование параметра
    vValue: Variant;    // Значение параметра
  end;

  TPhoaOperationParams = class(TInterfacedObject, IPhoaOperationParams)
  private
     // Список параметров
    FParams: Array of TPhoaOperationParam;
     // Удаляет параметр по индексу
    procedure Delete(iIndex: Integer);
     // Возвращает значение параметра по имени. Если такого параметра нет, вызывает Exception
    function  GetValueStrict(const sName: AnsiString): Variant;
     // Проверяет тип значения. При несоответствии вызывает Exception
    procedure CheckVarType(const sName: AnsiString; const v: Variant; RequiredType: TVarType);
     // Возвращает индекс записи по имени параметра, или -1, если нет такой
    function  IndexOfName(const sName: AnsiString): Integer;
     // IPhoaOperationParams
    procedure ObtainValIntf(const sName: AnsiString; GUID: TGUID; out Intf; bRequired: Boolean = True);
    function  GetCount: Integer;
    function  GetNames(Index: Integer): AnsiString;
    function  GetValBool(const sName: AnsiString): Boolean;
    function  GetValByte(const sName: AnsiString): Byte;
    function  GetValInt(const sName: AnsiString): Integer;
    function  GetValStr(const sName: AnsiString): WideString;
    function  GetValues(const sName: AnsiString): Variant;
    function  GetValuesByIndex(Index: Integer): Variant;
    procedure SetValues(const sName: AnsiString; const Value: Variant);
    procedure SetValuesByIndex(Index: Integer; const Value: Variant);
  public
    destructor Destroy; override;
  end;

  procedure TPhoaOperationParams.CheckVarType(const sName: AnsiString; const v: Variant; RequiredType: TVarType);
  begin
    if VarType(v)<>RequiredType then PhoaException(SPhoaOpErrMsg_ParamTypeMismatch, [sName, VarTypeAsText(RequiredType), VarTypeAsText(VarType(v))]);
  end;

  procedure TPhoaOperationParams.Delete(iIndex: Integer);
  var iNewCount: Integer;
  begin
    iNewCount := High(FParams);
     // Финализируем удаляемый элемент
    Finalize(FParams[iIndex]);
     // Сдвигаем элементы
    Move(FParams[iIndex+1], FParams[iIndex], SizeOf(TPhoaOperationParam)*(iNewCount-iIndex));
     // Зануляем последний элемент, чтобы его "не финализировало"
    FillChar(FParams[iNewCount], SizeOf(TPhoaOperationParam), 0);
     // Урезаем массив параметров
    SetLength(FParams, iNewCount); 
  end;

  destructor TPhoaOperationParams.Destroy;
  begin
     // Стираем список
    FParams := nil;
    inherited Destroy;
  end;

  function TPhoaOperationParams.GetCount: Integer;
  begin
    Result := Length(FParams);
  end;

  function TPhoaOperationParams.GetNames(Index: Integer): AnsiString;
  begin
    Result := FParams[Index].sName;
  end;

  function TPhoaOperationParams.GetValBool(const sName: AnsiString): Boolean;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varBoolean);
    Result := v;
  end;

  function TPhoaOperationParams.GetValByte(const sName: AnsiString): Byte;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varByte);
    Result := v;
  end;

  function TPhoaOperationParams.GetValInt(const sName: AnsiString): Integer;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varInteger);
    Result := v;
  end;

  function TPhoaOperationParams.GetValStr(const sName: AnsiString): WideString;
  var v: Variant;
  begin
    v := GetValueStrict(sName);
    CheckVarType(sName, v, varOleStr);
    Result := v;
  end;

  function TPhoaOperationParams.GetValues(const sName: AnsiString): Variant;
  var idx: Integer;
  begin
    idx := IndexOfName(sName);
    if idx<0 then Result := Unassigned else Result := FParams[idx].vValue;
  end;

  function TPhoaOperationParams.GetValuesByIndex(Index: Integer): Variant;
  begin
    Result := FParams[Index].vValue;
  end;

  function TPhoaOperationParams.GetValueStrict(const sName: AnsiString): Variant;
  var idx: Integer;
  begin
    idx := IndexOfName(sName);
    if idx<0 then PhoaException(SPhoaOpErrMsg_ParamNotFound, [sName]);
    Result := FParams[idx].vValue;
  end;

  function TPhoaOperationParams.IndexOfName(const sName: AnsiString): Integer;
  begin
    for Result := 0 to High(FParams) do
       // Для ускорения сравниваем, используя не-Ansi-версию, т.к. параметры не должны содержать национальных символов 
      if SameText(FParams[Result].sName, sName) then Exit;
    Result := -1;
  end;

  procedure TPhoaOperationParams.ObtainValIntf(const sName: AnsiString; GUID: TGUID; out Intf; bRequired: Boolean = True);
  var v: Variant;
  begin
    IInterface(Intf) := nil;
    if bRequired then v := GetValueStrict(sName) else v := GetValues(sName);
    if not VarIsEmpty(v) and not VarSupports(v, GUID, Intf) then PhoaException(SPhoaOpErrMsg_CannotObtainParamIntf, [sName]);
  end;

  procedure TPhoaOperationParams.SetValues(const sName: AnsiString; const Value: Variant);
  var idx: Integer;
  begin
    idx := IndexOfName(sName);
    if idx<0 then begin
      if not VarIsEmpty(Value) then begin
        idx := Length(FParams);
        SetLength(FParams, idx+1);
        FParams[idx].sName  := sName;
        FParams[idx].vValue := Value;
      end;
    end else
      SetValuesByIndex(idx, Value);
  end;

  procedure TPhoaOperationParams.SetValuesByIndex(Index: Integer; const Value: Variant);
  begin
    if VarIsEmpty(Value) then Delete(Index) else FParams[Index].vValue := Value;
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
    function  Add(const vNewValue: Variant; Prop: TPicProperty): Integer;
    function  GetChangedProps: TPicProperties;
    function  GetCount: Integer;
    function  GetItems(Index: Integer): PPicPropertyChange;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  function TPhoaPicPropertyChangeList.Add(const vNewValue: Variant; Prop: TPicProperty): Integer;
  var p: PPicPropertyChange;
  begin
    New(p);
    Result := FList.Add(p);
    p.vNewValue := vNewValue;
    p.Prop      := Prop;
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
   // TPhoaPicFileChangeList
   //===================================================================================================================
type
  TPhoaPicFileChangeList = class(TInterfacedObject, IPhoaPicFileChangeList)
  private
     // Собственно список
    FList: TList;
     // Удаляет элемент из списка
    procedure Delete(Index: Integer);
     // IPhoaPicFileChangeList
    function  Add(Pic: IPhotoAlbumPic; const wsFileName: WideString): Integer;
    function  GetCount: Integer;
    function  GetItems(Index: Integer): PPicFileChange;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  function TPhoaPicFileChangeList.Add(Pic: IPhotoAlbumPic; const wsFileName: WideString): Integer;
  var p: PPicFileChange;
  begin
    New(p);
    Result := FList.Add(p);
    p.Pic        := Pic;
    p.wsFileName := wsFileName;
  end;

  constructor TPhoaPicFileChangeList.Create;
  begin
    inherited Create;
    FList := TList.Create;
  end;

  procedure TPhoaPicFileChangeList.Delete(Index: Integer);
  begin
    Dispose(GetItems(Index));
    FList.Delete(Index);
  end;

  destructor TPhoaPicFileChangeList.Destroy;
  var i: Integer;
  begin
    for i := FList.Count-1 downto 0 do Delete(i);
    FList.Free;
    inherited Destroy;
  end;

  function TPhoaPicFileChangeList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhoaPicFileChangeList.GetItems(Index: Integer): PPicFileChange;
  begin
    Result := FList[Index];
  end;

   //===================================================================================================================
   // TPhoaOperation
   //===================================================================================================================

  procedure TPhoaOperation.AddChild(OpClass: TPhoaOperationClass; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    OpClass.Create(Operations, Project, Params, Changes);
  end;

  constructor TPhoaOperation.Create(AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges);
  begin
    inherited Create;
    FProject := AProject;
     // Регистрируемся в списке операций
    FList := AList;
    FList.Add(Self);
     // Запоминаем позицию Undo-данных
    FUndoDataPosition := FList.UndoStream.Position;
     // Выполняем операцию
    Perform(Params, FList.UndoStream, Changes); 
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
    if FOperations=nil then FOperations := TPhoaOperations.Create(List.UndoStream);
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

  function TPhoaOperation.Name: WideString;
  begin
    Result := DKLangConstW(ClassName);
  end;

  procedure TPhoaOperation.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
    { does nothing }
  end;

  procedure TPhoaOperation.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
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
  var UndoStream: IPhoaUndoDataStream;
  begin
    try
       // Позиционируем undo-файл в запомненную позицию
      UndoStream := FList.UndoStream; 
      UndoStream.BeginUndo(FUndoDataPosition);
      try
         // Откатываем изменения
        RollbackChanges(UndoStream, Changes);
      finally
         // Возвращаем позицию в undo-файле на место
        UndoStream.EndUndo(True);
      end;
    finally
       // Уничтожаем объект
      Destroy;
    end;
  end;

   //===================================================================================================================
   // TPhoaOperationFactory - реализация IPhoaOperationFactory
   //===================================================================================================================
type
  TPhoaOperationFactory = class(TInterfacedObject, IPhoaOperationFactory)
  private
     // Список зарегистрированных классов
    FClasses: TStringList;
     // IPhoaOperationFactory
    function  GetClassByName(const sOpName: AnsiString): TPhoaOperationClass;
    function  NewOperation(const sOpName: AnsiString; AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges): TPhoaOperation;
    procedure RegisterOpClass(const sOpName: AnsiString; OpClass: TPhoaOperationClass);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  constructor TPhoaOperationFactory.Create;
  begin
    inherited Create;
    FClasses := TStringList.Create;
    FClasses.Sorted     := True;
    FClasses.Duplicates := dupError;
  end;

  destructor TPhoaOperationFactory.Destroy;
  begin
    FClasses.Free;
    inherited Destroy;
  end;

  function TPhoaOperationFactory.GetClassByName(const sOpName: AnsiString): TPhoaOperationClass;
  var idx: Integer;
  begin
    Result := nil; // Satisfy the compiler
    idx := FClasses.IndexOf(sOpName);
    if idx<0 then PhoaException(SPhoaOpErrMsg_OperationNotFound, [sOpName]) else Result := TPhoaOperationClass(FClasses.Objects[idx]);
  end;

  function TPhoaOperationFactory.NewOperation(const sOpName: AnsiString; AList: TPhoaOperations; AProject: IPhotoAlbumProject; Params: IPhoaOperationParams; var Changes: TPhoaOperationChanges): TPhoaOperation;
  begin
    Result := GetClassByName(sOpName).Create(AList, AProject, Params, Changes);
  end;

  procedure TPhoaOperationFactory.RegisterOpClass(const sOpName: AnsiString; OpClass: TPhoaOperationClass);
  begin
    FClasses.AddObject(sOpName, Pointer(OpClass));
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

  constructor TPhoaOperations.Create(AUndoStream: IPhoaUndoDataStream);
  begin
    inherited Create;
    FList := TList.Create;
    FUndoStream := AUndoStream;
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
    UndoStream.Clear;
  end;

  constructor TPhoaUndo.Create;
  begin
    inherited Create(NewPhoaUndoDataStream);
    FSavepointOnEmpty := True;
    FMaxCount := MaxInt;
  end;

  function TPhoaUndo.GetIsUnmodified: Boolean;
  begin
    if Count=0 then Result := FSavepointOnEmpty else Result := GetItems(Count-1).FSavepoint;
  end;

  function TPhoaUndo.GetLastOpName: WideString;
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

  procedure TPhoaUndo.SetNonUndoable(bModified: Boolean);
  begin
    Clear;
    FSavepointOnEmpty := not bModified;
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

  procedure TPhoaOp_GroupNew.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Group, NewGroup: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
     // Создаём дочернюю группу
    NewGroup := NewPhotoAlbumPicGroup(Group, Project.RootGroupX.MaxGroupID+1);
    NewGroup.Text := DKLangConstW('SDefaultNewGroupName');
    OpParentGroup := Group;
    OpGroup       := NewGroup;
     // Возвращаем через Params созданную группу
    Params['NewGroup'] := NewGroup;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
  end;

  procedure TPhoaOp_GroupNew.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
     // Удаляем группу операции
    OpGroup.Owner := nil;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupRename
   //===================================================================================================================

  procedure TPhoaOp_GroupRename.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Group: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Запоминаем данные отката
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    OpGroup := Group;
    UndoStream.WriteWStr(Group.Text);
     // Выполняем операцию
    Group.Text := Params.ValStr['NewText'];
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
  end;

  procedure TPhoaOp_GroupRename.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
     // Получаем группу и восстанавливаем текст
    OpGroup.Text := UndoStream.ReadWStr;
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupEdit
   //===================================================================================================================

  procedure TPhoaOp_GroupEdit.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Group: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Запоминаем данные отката
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    OpGroup := Group;
    UndoStream.WriteWStr(Group.Text);
    UndoStream.WriteWStr(Group.Description);
    UndoStream.WriteRaw (Group.IconData);
     // Выполняем операцию
    Group.Text        := Params.ValStr['NewText'];
    Group.Description := Params.ValStr['NewDescription'];
    Group.IconData    := Params.ValStr['NewIconData'];
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
  end;

  procedure TPhoaOp_GroupEdit.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Group: IPhotoAlbumPicGroup;
  begin
     // Получаем группу
    Group := OpGroup;
     // Восстанавливаем свойства
    Group.Text        := UndoStream.ReadWStr;
    Group.Description := UndoStream.ReadWStr;
    Group.IconData    := UndoStream.ReadRaw;
     // Добавляем флаги изменений
    Include(Changes, pocGroupProps);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupDelete
   //===================================================================================================================

  procedure TPhoaOp_GroupDelete.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Удаляем группу (и подгруппы)
    AddChild(TPhoaOp_InternalGroupDelete, Params, Changes);
     // Удаляем неиспользуемые изображения
    AddChild(TPhoaOp_InternalUnlinkedPicsRemoving, Params, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalGroupDelete
   //===================================================================================================================

  procedure TPhoaOp_InternalGroupDelete.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    Group: IPhotoAlbumPicGroup;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Запоминаем данные удаляемой группы
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    OpGroup       := Group;
    OpParentGroup := Group.OwnerX;
    UndoStream.WriteWStr(Group.Text);
    UndoStream.WriteWStr(Group.Description);
    UndoStream.WriteInt (Group.Index);
    UndoStream.WriteBool(Group.Expanded);
    UndoStream.WriteRaw (Group.IconData);
     // Записываем ID изображений и удаляем изображения из группы
    UndoStream.WriteInt(Group.Pics.Count);
    for i := 0 to Group.Pics.Count-1 do UndoStream.WriteInt(Group.Pics[i].ID);
    Group.PicsX.Clear;
     // Каскадно удаляем группы
    for i := Group.Groups.Count-1 downto 0 do
      AddChild(TPhoaOp_InternalGroupDelete, NewPhoaOperationParams(['Group', Group.GroupsX[i]]), Changes);
     // Удаляем группу
    Group.Owner := nil;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
  end;

  procedure TPhoaOp_InternalGroupDelete.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
  begin
     // Восстанавливаем группу
    g := NewPhotoAlbumPicGroup(OpParentGroup, OpGroupID);
    g.Text        := UndoStream.ReadWStr;
    g.Description := UndoStream.ReadWStr;
    g.Index       := UndoStream.ReadInt;
    g.Expanded    := UndoStream.ReadBool;
    g.IconData    := UndoStream.ReadRaw;
     // Восстанавливаем изображения
    for i := 0 to UndoStream.ReadInt-1 do g.PicsX.Add(Project.Pics.ItemsByID[UndoStream.ReadInt], False);
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
     // Восстанавливаем вложенные группы
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalUnlinkedPicsRemoving
   //===================================================================================================================

  procedure TPhoaOp_InternalUnlinkedPicsRemoving.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    Pic: IPhotoAlbumPic;
    UsedPics: IPhotoAlbumPicList;

     // Рекурсивно добавляет в UsedPics все изображения группы и вложенных групп
    procedure AddGroupPics(Group: IPhotoAlbumPicGroup);
    var i: Integer;
    begin
      UsedPics.Add(Group.Pics, True);
      for i := 0 to Group.Groups.Count-1 do AddGroupPics(Group.GroupsX[i]);
    end;

  begin
    inherited Perform(Params, UndoStream, Changes);
     // Составляем список используемых изображений
    UsedPics := NewPhotoAlbumPicList(True);
    AddGroupPics(Project.RootGroupX);
     // Цикл по всем изображениям фотоальбома
    for i := Project.Pics.Count-1 downto 0 do begin
      Pic := Project.PicsX[i];
       // Если изображение не связано ни с одной группой
      if UsedPics.IndexOfID(Pic.ID)<0 then begin
         // Пишем флаг продолжения
        UndoStream.WriteBool(True);
         // Сохраняем данные изображения
        UndoStream.WriteRaw(Pic.RawData[PPAllProps]);
         // Удаляем изображение из списка
        Project.PicsX.Delete(i);
         // Добавляем флаги изменений
        Include(Changes, pocProjectPicList);
      end;
    end;
     // Пишем стоп-флаг
    UndoStream.WriteBool(False);
  end;

  procedure TPhoaOp_InternalUnlinkedPicsRemoving.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
     // Читаем данные, пока не встретим стоп-флаг
    while UndoStream.ReadBool do
       // Создаём изображение
      with NewPhotoAlbumPic do begin
         // Загружаем данные
        RawData[PPAllProps] := UndoStream.ReadRaw;
         // Кладём в список (ID уже загружен)
        PutToList(Project.PicsX);
         // Добавляем флаги изменений
        Include(Changes, pocProjectPicList);
      end;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicEdit
   //===================================================================================================================

  procedure TPhoaOp_PicEdit.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);

     // Выполняет операцию класса OpClass с параметрами, получаемыми из собственного параметра с именем sOpParamName,
     //   если он указан
    procedure PerformIfSpecified(const sOpParamName: AnsiString; OpClass: TPhoaOperationClass);
    var OpParams: IPhoaOperationParams;
    begin
      Params.ObtainValIntf(sOpParamName, IPhoaOperationParams, OpParams, False);
      if OpParams<>nil then AddChild(OpClass, OpParams, Changes);
    end;

  begin
    inherited Perform(Params, UndoStream, Changes);
     // Выполняем требуемые дочерние операции
    PerformIfSpecified('EditFilesOpParams',    TPhoaOp_InternalEditPicFiles);
    PerformIfSpecified('EditViewOpParams',     TPhoaOp_InternalEditPicProps);
    PerformIfSpecified('EditDataOpParams',     TPhoaOp_InternalEditPicProps);
    PerformIfSpecified('EditKeywordsOpParams', TPhoaOp_InternalEditPicKeywords);
    PerformIfSpecified('EditGroupOpParams',    TPhoaOp_InternalEditPicToGroupBelonging);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicFiles
   //===================================================================================================================

  procedure TPhoaOp_InternalEditPicFiles.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    FileChangeList: IPhoaPicFileChangeList;
    Pic: IPhotoAlbumPic;
    i: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('FileChangeList', IPhoaPicFileChangeList, FileChangeList);
     // Пишем количество изменений
    UndoStream.WriteInt(FileChangeList.Count);
     // Цикл по списку требуемых изменений
    for i := 0 to FileChangeList.Count-1 do begin
      Pic := FileChangeList[i].Pic;
       // Сохраняем ID и прежний файл изображения
      UndoStream.WriteInt (Pic.ID);
      UndoStream.WriteWStr(Pic.FileName);
       // Применяем новый файл
      Pic.FileName := FileChangeList[i].wsFileName;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
  end;

  procedure TPhoaOp_InternalEditPicFiles.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var i, iPicID: Integer;
  begin
     // Возвращаем имена файлов изменённым изображениям обратно
    for i := 0 to UndoStream.ReadInt-1 do begin
      iPicID   := UndoStream.ReadInt;
      Project.PicsX.ItemsByIDX[iPicID].FileName := UndoStream.ReadWStr;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicProps
   //===================================================================================================================

  procedure TPhoaOp_InternalEditPicProps.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Pics: IPhotoAlbumPicList;
    ChangeList: IPhoaPicPropertyChangeList;
    iPic, iChg: Integer;
    Pic: IPhotoAlbumPic;
    ChangedProps: TPicProperties;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics',       IPhotoAlbumPicList,         Pics);
    Params.ObtainValIntf('ChangeList', IPhoaPicPropertyChangeList, ChangeList);
     // Сохраняем набор изменяющихся свойств
    ChangedProps := ChangeList.ChangedProps;
    UndoStream.WriteInt(PicPropsToInt(ChangedProps));
     // Сохраняем количество изображений
    UndoStream.WriteInt(Pics.Count);
     // Цикл по изображениям
    for iPic := 0 to Pics.Count-1 do begin
       // Запоминаем старые данные
      Pic := Pics[iPic];
      UndoStream.WriteInt(Pic.ID);
      UndoStream.WriteRaw(Pic.RawData[ChangedProps]);
       // Применяем новые данные
      for iChg := 0 to ChangeList.Count-1 do
        with ChangeList[iChg]^ do Pic.PropValues[Prop] := vNewValue;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
  end;

  procedure TPhoaOp_InternalEditPicProps.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i, iPicID: Integer;
    ChangedProps: TPicProperties;
    PicData: TPhoaRawData;
  begin
     // Получаем набор изменённых свойств
    ChangedProps := IntToPicProps(UndoStream.ReadInt);
     // Возвращаем данные изменённых изображений
    for i := 0 to UndoStream.ReadInt-1 do begin
      iPicID  := UndoStream.ReadInt;
      PicData := UndoStream.ReadRaw;
      Project.PicsX.ItemsByIDX[iPicID].RawData[ChangedProps] := PicData;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicKeywords
   //===================================================================================================================

  procedure TPhoaOp_InternalEditPicKeywords.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Pics: IPhotoAlbumPicList;
    KeywordList: IPhotoAlbumKeywordList;
    iPic, iCnt, iKwd, idxKeyword: Integer;
    Pic: IPhotoAlbumPic;
    wsKeyword: WideString;
    pkd: PPhoaKeywordData;
    bKWSaved: Boolean;
    PicKeywords: IPhotoAlbumKeywordList;

     // Сохраняет ключевые слова изображения в FSavedKeywords, если этого ещё не сделано
    procedure SavePicKeywords;
    begin
      if not bKWSaved then begin
        UndoStream.WriteBool(True); // Признак записи ключевого слова (в противоположность стоп-флагу)
        UndoStream.WriteInt (Pic.ID);
        UndoStream.WriteWStr(Pic.Keywords.CommaText);
        bKWSaved := True;
         // Добавляем флаги изменений
        Include(Changes, pocPicProps);
      end;
    end;

  begin
    inherited Perform(Params, UndoStream, Changes);
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
        wsKeyword := KeywordList[iKwd];
        pkd := KeywordList.KWData[iKwd];
        case pkd.Change of
           // КС не менялось. Проверяем изменение птицы
          pkcNone:
             // Если Grayed - менять нечего по определению. Если не выделено, и КС не содержится ни в одном изображении
             //   - менять нечего. Если выделено полностью, и КС содержится во всех изображениях - менять нечего.
             //   Иначе проверяем наличие КС в изображении
            if ((pkd.State=pksOff) and (pkd.iSelCount>0)) or ((pkd.State=pksOn) and (pkd.iSelCount<iCnt)) then begin
              idxKeyword := PicKeywords.IndexOf(wsKeyword);
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
                    PicKeywords.Add(wsKeyword);
                  end;
              end;
            end;
           // Добавление нового КС. Если есть птица - надо добавить
          pkcAdd:
            if pkd.State=pksOn then begin
              SavePicKeywords;
              PicKeywords.Add(wsKeyword);
            end;
           // КС менялось. Если только оно не полностью отсутствовало и отсутствует, ...
          pkcReplace:
            if (pkd.State<>pksOff) or (pkd.iSelCount>0) then begin
               // ... ищем старое КС и удаляем, ...
              idxKeyword := PicKeywords.IndexOf(pkd.wsOldKeyword);
              if idxKeyword>=0 then begin
                SavePicKeywords;
                PicKeywords.Delete(idxKeyword);
              end;
               // ... если состояние pksOn - добавляем новое всем, если pksGrayed - добавляем только в те, где было старое
              if (pkd.State=pksOn) or ((pkd.State=pksGrayed) and (idxKeyword>=0)) then begin
                SavePicKeywords;
                PicKeywords.Add(wsKeyword);
              end;
            end;
        end;
      end;
    end;
     // Пишем стоп-флаг
    UndoStream.WriteBool(False);
  end;

  procedure TPhoaOp_InternalEditPicKeywords.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var iPicID: Integer;
  begin
     // Возвращаем КС изменённым изображениям: крутим цикл, пока не встретим стоп-флаг
    while UndoStream.ReadBool do begin
      iPicID    := UndoStream.ReadInt;
      Project.PicsX.ItemsByIDX[iPicID].KeywordsM.CommaText := UndoStream.ReadWStr;
       // Добавляем флаги изменений
      Include(Changes, pocPicProps);
    end;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalEditPicToGroupBelonging
   //===================================================================================================================

  procedure TPhoaOp_InternalEditPicToGroupBelonging.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Pics: IPhotoAlbumPicList;
    AddToGroups, RemoveFromGroups: IPhotoAlbumPicGroupList;
    i: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics',             IPhotoAlbumPicList,      Pics);
    Params.ObtainValIntf('AddToGroups',      IPhotoAlbumPicGroupList, AddToGroups);
    Params.ObtainValIntf('RemoveFromGroups', IPhotoAlbumPicGroupList, RemoveFromGroups);
     // Отрабатываем добавления
    for i := 0 to AddToGroups.Count-1 do
      AddChild(
        TPhoaOp_InternalPicToGroupAdding,
        NewPhoaOperationParams(['Group', AddToGroups[i], 'Pics', Pics]),
        Changes);
     // Отрабатываем удаления
    for i := 0 to RemoveFromGroups.Count-1 do
      AddChild(
        TPhoaOp_InternalPicFromGroupRemoving,
        NewPhoaOperationParams(['Group', RemoveFromGroups[i], 'Pics', Pics]),
        Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_StoreTransform
   //===================================================================================================================

  procedure TPhoaOp_StoreTransform.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Pic: IPhotoAlbumPic;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pic', IPhotoAlbumPic, Pic);
     // Сохраняем прежние свойства
    UndoStream.WriteInt(Pic.ID);
    UndoStream.WriteByte(Byte(Pic.Rotation));
    UndoStream.WriteByte(Byte(Pic.Flips));
     // Применяем новые свойства
    Pic.Rotation := TPicRotation(Params.ValByte['NewRotation']);
    Pic.Flips    := TPicFlips   (Params.ValByte['NewFlips']);
     // Добавляем флаги изменений
    Include(Changes, pocPicProps);
  end;

  procedure TPhoaOp_StoreTransform.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Pic: IPhotoAlbumPic;
  begin
    Pic          := Project.PicsX.ItemsByIDX[UndoStream.ReadInt];
    Pic.Rotation := TPicRotation(UndoStream.ReadByte);
    Pic.Flips    := TPicFlips(Byte(UndoStream.ReadByte)); // Странный typecast, но иначе не компилируется
     // Добавляем флаги изменений
    Include(Changes, pocPicProps);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicAdd
   //===================================================================================================================

  procedure TPhoaOp_PicAdd.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    Pics: IPhotoAlbumPicList;
    Group: IPhotoAlbumPicGroup;
    Pic, PicEx: IPhotoAlbumPic;
    bExisting, bAddedToGroup: Boolean;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics',  IPhotoAlbumPicList,  Pics);
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
     // Сохраняем данные для отката
    OpGroup := Group;
     // Перебираем все новые изображения
    UndoStream.WriteInt(Pics.Count);
    for i := 0 to Pics.Count-1 do begin
      Pic := Pics[i];
       // Ищем уже существующее изображение с тем же файлом
      PicEx := Project.PicsX.ItemsByFileNameX[Pic.FileName];
      bExisting := PicEx<>nil;
       // Если есть такое изображение - игнорируем Pic
      if bExisting then
        Pic := PicEx
       // Иначе заносим в список, распределяя новый ID
      else begin
        Pic.PutToList(Project.PicsX, 0);
        Include(Changes, pocProjectPicList);
      end;
       // Добавляем изображение в группу, если его там не было
      Group.PicsX.Add(Pic, True, bAddedToGroup);
      if bAddedToGroup then Include(Changes, pocGroupPicList);
       // Сохраняем ID изображения, флаг существования, флаг добавления в группу
      UndoStream.WriteInt(Pic.ID);
      UndoStream.WriteBool(bExisting);
      UndoStream.WriteBool(bAddedToGroup);
    end;
  end;

  procedure TPhoaOp_PicAdd.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    i, iPicID: Integer;
    bExisting, bAddedToGroup: Boolean;
  begin
     // Получаем группу, куда добавляли изображения
    Group := OpGroup;
     // Считываем данные по каждому изображению
    for i := 0 to UndoStream.ReadInt-1 do begin
      iPicID        := UndoStream.ReadInt;
      bExisting     := UndoStream.ReadBool;
      bAddedToGroup := UndoStream.ReadBool;
       // Если было добавлено в группу - удаляем
      if bAddedToGroup then begin
        Group.PicsX.Remove(iPicID);
        Include(Changes, pocGroupPicList);
      end;
       // Если было добавлено новое изображение, удаляем и из списка проекта
      if not bExisting then begin
        Project.PicsX.Remove(iPicID);
        Include(Changes, pocProjectPicList);
      end;
    end;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicFromProjectRemoving
   //===================================================================================================================

  procedure TPhoaOp_InternalPicFromProjectRemoving.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Pics: IPhotoAlbumPicList;
    Pic: IPhotoAlbumPic;
    i: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics', IPhotoAlbumPicList, Pics);
     // Удаляем изображения из фотоальбома
    for i := 0 to Pics.Count-1 do begin
       // Пишем флаг продолжения
      UndoStream.WriteBool(True);
       // Сохраняем данные изображения
      Pic := Pics[i];
      UndoStream.WriteRaw(Pic.RawData[PPAllProps]);
       // Удаляем изображение из списка
      Project.PicsX.Remove(Pic.ID);
       // Добавляем флаги изменений
      Include(Changes, pocProjectPicList);
    end;
     // Пишем стоп-флаг
    UndoStream.WriteBool(False);
  end;

  procedure TPhoaOp_InternalPicFromProjectRemoving.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
     // Читаем данные, пока не встретим стоп-флаг
    while UndoStream.ReadBool do
       // Создаём изображение
      with NewPhotoAlbumPic do begin
         // Загружаем данные
        RawData[PPAllProps] := UndoStream.ReadRaw;
         // Кладём в список (ID уже загружен)
        PutToList(Project.PicsX);
         // Добавляем флаги изменений
        Include(Changes, pocProjectPicList);
      end;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicFromGroupRemoving
   //===================================================================================================================

  procedure TPhoaOp_InternalPicFromGroupRemoving.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Pics:  IPhotoAlbumPicList;
    i, idx: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
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
        UndoStream.WriteBool(True);
         // Пишем ID
        UndoStream.WriteInt(Pics[i].ID);
         // Пишем индекс
        UndoStream.WriteInt(idx);
         // Удаляем изображение
        Group.PicsX.Delete(idx);
         // Добавляем флаги изменений
        Include(Changes, pocGroupPicList);
      end;
    end;
     // Пишем стоп-флаг
    UndoStream.WriteBool(False);
  end;

  procedure TPhoaOp_InternalPicFromGroupRemoving.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
    IIs: TIntegerList;
  begin
    g := OpGroup;
     // Загружаем ID и индексы во временный список
    IIs := TIntegerList.Create(True);
    try
      while UndoStream.ReadBool do begin
        IIs.Add(UndoStream.ReadInt);
        IIs.Add(UndoStream.ReadInt);
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
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_InternalPicToGroupAdding
   //===================================================================================================================

  procedure TPhoaOp_InternalPicToGroupAdding.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Pics:  IPhotoAlbumPicList;
    i: Integer;
    bAdded: Boolean;
    Pic: IPhoaPic;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('Pics',  IPhotoAlbumPicList,  Pics);
    OpGroup := Group;
     // Добавляем изображения в группу и в undo-файл
    for i := 0 to Pics.Count-1 do begin
      Pic := Pics[i];
      Group.PicsX.Add(Pic, True, bAdded);
      if bAdded then begin
        UndoStream.WriteBool(True); // Флаг продолжения
        UndoStream.WriteInt (Pic.ID);
         // Добавляем флаги изменений
        Include(Changes, pocGroupPicList);
      end;
    end;
     // Пишем стоп-флаг
    UndoStream.WriteBool(False);
  end;

  procedure TPhoaOp_InternalPicToGroupAdding.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var g: IPhotoAlbumPicGroup;
  begin
     // Удаляем добавленные изображения (считываем ID добавленных изображений из файла, пока не встретим стоп-флаг)
    g := OpGroup;
    while UndoStream.ReadBool do begin
      g.PicsX.Remove(UndoStream.ReadInt);
       // Добавляем флаги изменений
      Include(Changes, pocGroupPicList);
    end;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaBaseOp_PicCopy
   //===================================================================================================================

  constructor TPhoaBaseOp_PicCopy.Create(Pics: IPhotoAlbumPicList; ClipFormats: TPicClipboardFormats);

     // Копирует в буфер обмена данные изображения PhoA
    procedure CopyPhoaData;
    var
      i, iSize: Integer;
      ms: TMemoryStream;
      Streamer: TPhoaStreamer;
      hRec: THandle;
      p: PByte;
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
        iSize := ms.Size;
         // Выделяем память
        hRec := GlobalAlloc(GMEM_MOVEABLE, iSize+SizeOf(iSize));
        if hRec=0 then RaiseLastOSError;
         // Блокируем память, получая указатель
        p := GlobalLock(hRec);
        if p=nil then RaiseLastOSError;
        try
           // Пишем размер блока
          Move(iSize, p^, SizeOf(iSize));
          Inc(p, SizeOf(iSize));
           // Переписываем строку в память
          Move(ms.Memory^, p^, iSize);
        finally
          GlobalUnlock(hRec);
        end;
      finally
        ms.Free;
      end;
       // Копируем
      TntClipboard.SetAsHandle(wClipbrdPicFormatID, hRec);
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
      ws: WideString;
    begin
       // Составляем список полных путей файлов
      ws := '';
      for i := 0 to Pics.Count-1 do ws := ws+Pics[i].FileName+S_CRLF;
       // Помещаем текст в clipboard
      TntClipboard.AsWideText := ws;
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
        TntClipboard.Assign(bmp32);
      finally
        bmp32.Free;
      end;
    end;

  begin
    StartWait;
    try
      if Pics.Count>0 then begin
        TntClipboard.Open;
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
          TntClipboard.Close;
        end;
      end;
    finally
      StopWait;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_PicDelete
   //===================================================================================================================

  procedure TPhoaOp_PicDelete.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Удаляем изображения из группы
    AddChild(TPhoaOp_InternalPicFromGroupRemoving, Params, Changes);
     // Удаляем несвязанные изображения из фотоальбома
    AddChild(TPhoaOp_InternalUnlinkedPicsRemoving, nil, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDeleteFromProject
   //===================================================================================================================

  procedure TPhoaOp_PicDeleteFromProject.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream;var Changes: TPhoaOperationChanges);

     // Рекурсивная процедура удаления изображений из группы Group
    procedure DoDeletePics(Group: IPhotoAlbumPicGroup);
    var i: Integer;
    begin
       // Удаляем изображения из группы
      Params.Values['Group'] := Group;
      AddChild(TPhoaOp_InternalPicFromGroupRemoving, Params, Changes);
       // Рекурсивно вызываем для подгрупп
      for i := 0 to Group.GroupsX.Count-1 do DoDeletePics(Group.GroupsX[i]);
    end;

  begin
    inherited Perform(Params, UndoStream, Changes);
     // Удаляем изображения из групп
    DoDeletePics(Project.RootGroupX);
     // Удаляем изображения из фотоальбома
    AddChild(TPhoaOp_InternalPicFromProjectRemoving, Params, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDeleteWithFiles
   //===================================================================================================================

  procedure TPhoaOp_PicDeleteWithFiles.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    Pics: IPhotoAlbumPicList;
    Pic: IPhotoAlbumPic;

     // Удаляет изображение из группы Group и всех её подгрупп
    procedure DoDeletePic(iPicID: Integer; Group: IPhotoAlbumPicGroup);
    var i: Integer;
    begin
       // Удаляем изображения из группы
      Group.PicsX.Remove(iPicID);
       // Повторяем то же для подгрупп
      for i := 0 to Group.GroupsX.Count-1 do DoDeletePic(iPicID, Group.GroupsX[i]);
    end;

  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Pics', IPhotoAlbumPicList, Pics);
     // Цикл по изображениям
    for i := 0 to Pics.Count-1 do begin
      Pic := Pics[i];
       // Удаляем файл
      if not DeleteFile(Pic.FileName) then PhoaExceptionConst('SErrCannotDeleteFile', [Pic.FileName, WideSysErrorMessage(GetLastError)]);
       // Удаляем изображение из всех групп
      DoDeletePic(Pic.ID, Project.RootGroupX);
       // Удаляем изображение из списка изображений проекта
      Project.PicsX.Remove(Pic.ID);
       // Добавляем флаги изменений
      Changes := Changes+[pocGroupPicList, pocProjectPicList, pocNonUndoable];
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_PicPaste
   //===================================================================================================================

  procedure TPhoaOp_PicPaste.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    PastedPics: IPhotoAlbumPicList;
    hRec: THandle;
    ms: TMemoryStream;
    Streamer: TPhoaStreamer;
    p: PByte;
    iSize: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
    if TntClipboard.HasFormat(wClipbrdPicFormatID) then begin
       // Получаем параметры
      Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
      OpGroup := Group;
       // Создаём список изображений для вставки
      PastedPics := NewPhotoAlbumPicList(False);
       // Создаём временный поток
      ms := TMemoryStream.Create;
      try
        TntClipboard.Open;
        try
           // Получаем Handle блока данных из буфера обмена
          hRec := TntClipboard.GetAsHandle(wClipbrdPicFormatID);
          if hRec=0 then RaiseLastOSError;
           // Получаем размер данных из буфера обмена
          p := GlobalLock(hRec);
          if p=nil then RaiseLastOSError;
          try
            Move(p^, iSize, SizeOf(iSize));
            Inc(p, SizeOf(iSize));
             // Получаем данные из буфера обмена
            ms.Write(p^, iSize);
          finally
            GlobalUnlock(hRec);
          end;
        finally
          TntClipboard.Close;
        end;
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
      AddChild(TPhoaOp_PicAdd, NewPhoaOperationParams(['Group', Group, 'Pics', PastedPics]), Changes);
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_ProjectEdit
   //===================================================================================================================

  procedure TPhoaOp_ProjectEdit.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Сохраняем старые свойства
    UndoStream.WriteInt (Project.ThumbnailSize.cx);
    UndoStream.WriteInt (Project.ThumbnailSize.cy);
    UndoStream.WriteByte(Project.ThumbnailQuality);
    UndoStream.WriteWStr(Project.Description);
     // Выполняем операцию
    Project.ThumbnailSize    := MakeSize(Params.ValInt['NewThWidth'], Params.ValInt['NewThHeight']);
    Project.ThumbnailQuality := Params.ValByte['NewThQuality'];
    Project.Description      := Params.ValStr ['NewDescription'];
     // Добавляем флаги изменений
    Include(Changes, pocProjectProps);
  end;

  procedure TPhoaOp_ProjectEdit.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var Sz: TSize;
  begin
     // Восстанавливаем свойства фотоальбома
    Sz.cx   := UndoStream.ReadInt;
    Sz.cy   := UndoStream.ReadInt;
    Project.ThumbnailSize    := Sz;
    Project.ThumbnailQuality := UndoStream.ReadByte;
    Project.Description      := UndoStream.ReadWStr;
     // Добавляем флаги изменений
    Include(Changes, pocProjectProps);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicOperation
   //===================================================================================================================

  procedure TPhoaOp_PicOperation.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    SourceGroup, TargetGroup: IPhotoAlbumPicGroup;
    Pics: IPhoaPicList;
    i: Integer;
    IntersectPics: IPhoaMutablePicList;
    Pic: IPhoaPic;
    PicOperation: TPictureOperation;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('SourceGroup', IPhotoAlbumPicGroup, SourceGroup);
    Params.ObtainValIntf('TargetGroup', IPhotoAlbumPicGroup, TargetGroup);
    Params.ObtainValIntf('Pics',        IPhoaPicList,        Pics);
    PicOperation := TPictureOperation(Params.ValByte['PicOperation']);
     // Копирование/перемещение: копируем выделенные изображения
    if PicOperation in [popMoveToTarget, popCopyToTarget] then
      AddChild(
        TPhoaOp_InternalPicToGroupAdding,
        NewPhoaOperationParams(['Group', TargetGroup, 'Pics', Pics]),
        Changes);
     // Если перемещение - удаляем выделенные изображения из исходной группы
    if PicOperation=popMoveToTarget then
      AddChild(
        TPhoaOp_InternalPicFromGroupRemoving,
        NewPhoaOperationParams(['Group', SourceGroup,'Pics', Pics]),
        Changes);
     // Удаление выделенных изображений из указанной группы
    if PicOperation=popRemoveFromTarget then
      AddChild(
        TPhoaOp_InternalPicFromGroupRemoving,
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
        AddChild(
          TPhoaOp_InternalPicFromGroupRemoving,
          NewPhoaOperationParams(['Group', TargetGroup, 'Pics', IntersectPics]),
          Changes);
        AddChild(TPhoaOp_InternalUnlinkedPicsRemoving, nil, Changes);
      end;
    end;
  end;

   //===================================================================================================================
   // TPhoaOp_InternalGroupPicSort
   //===================================================================================================================

  procedure TPhoaOp_InternalGroupPicSort.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Sortings: IPhotoAlbumPicSortingList;
    i: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group',    IPhotoAlbumPicGroup,       Group);
    Params.ObtainValIntf('Sortings', IPhotoAlbumPicSortingList, Sortings);
     // Запоминаем группу
    OpGroup := Group;
     // Запоминаем порядок следования ID изображений в группе
    UndoStream.WriteInt(Group.Pics.Count);
    for i := 0 to Group.Pics.Count-1 do UndoStream.WriteInt(Group.Pics[i].ID);
     // Сортируем изображения в группе
    Group.PicsX.SortingsSort(Sortings);
     // Добавляем флаги изменений
    Include(Changes, pocGroupPicList);
  end;

  procedure TPhoaOp_InternalGroupPicSort.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var i: Integer;
  begin
     // Восстанавливаем старый порядок следования ID изображений в группе
    OpGroup.PicsX.Clear;
    for i := 0 to UndoStream.ReadInt-1 do OpGroup.PicsX.Add(Project.Pics.ItemsByID[UndoStream.ReadInt], False);
     // Добавляем флаги изменений
    Include(Changes, pocGroupPicList);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicSort
   //===================================================================================================================

  procedure TPhoaOp_PicSort.AddGroupSortOp(Group: IPhotoAlbumPicGroup; Sortings: IPhotoAlbumPicSortingList; bRecursive: Boolean; var Changes: TPhoaOperationChanges);
  var i: Integer;
  begin
     // Сортируем изображения в группе
    AddChild(TPhoaOp_InternalGroupPicSort, NewPhoaOperationParams(['Group', Group, 'Sortings', Sortings]), Changes);
     // При необходимости сортируем и в подгруппах
    if bRecursive then
      for i := 0 to Group.Groups.Count-1 do AddGroupSortOp(Group.GroupsX[i], Sortings, True, Changes);
  end;

  procedure TPhoaOp_PicSort.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Sortings: IPhotoAlbumPicSortingList;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group',    IPhotoAlbumPicGroup,       Group);
    Params.ObtainValIntf('Sortings', IPhotoAlbumPicSortingList, Sortings);
     // Запускаем сортировку
    AddGroupSortOp(Group, Sortings, Params.ValBool['Recursive'], Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_GroupDragAndDrop
   //===================================================================================================================

  procedure TPhoaOp_GroupDragAndDrop.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group, NewParentGroup, gOldParent: IPhotoAlbumPicGroup;
    iNewIndex: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group',          IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('NewParentGroup', IPhotoAlbumPicGroup, NewParentGroup);
    iNewIndex := Params.ValInt['NewIndex'];
     // Запоминаем данные отката
    gOldParent := Group.OwnerX;
    UndoStream.WriteInt(Group.Index);
     // Перемещаем группу
    Group.Owner := NewParentGroup;
    if iNewIndex>=0 then Group.Index := iNewIndex; // Индекс -1 означает добавление последним ребёнком
     // Запоминаем группы (ID прежнего родителя и ID группы)
    OpParentGroup := gOldParent;
    OpGroup       := Group;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
  end;

  procedure TPhoaOp_GroupDragAndDrop.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
     // Восстанавливаем положение группы
    with OpGroup do begin
      Owner := OpParentGroup;
      Index := UndoStream.ReadInt;
    end;
     // Добавляем флаги изменений
    Include(Changes, pocGroupStructure);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDragAndDropToGroup
   //===================================================================================================================

  procedure TPhoaOp_PicDragAndDropToGroup.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    SourceGroup, TargetGroup: IPhotoAlbumPicGroup;
    Pics: IPhotoAlbumPicList;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('SourceGroup', IPhotoAlbumPicGroup, SourceGroup);
    Params.ObtainValIntf('TargetGroup', IPhotoAlbumPicGroup, TargetGroup);
    Params.ObtainValIntf('Pics',        IPhotoAlbumPicList,  Pics);
     // Выполняем операцию
    AddChild(TPhoaOp_InternalPicToGroupAdding, NewPhoaOperationParams(['Group', TargetGroup, 'Pics', Pics]), Changes);
    if not Params.ValBool['Copy'] then
      AddChild(TPhoaOp_InternalPicFromGroupRemoving, NewPhoaOperationParams(['Group', SourceGroup, 'Pics', Pics]), Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_PicDragAndDropInsideGroup
   //===================================================================================================================

  procedure TPhoaOp_PicDragAndDropInsideGroup.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group: IPhotoAlbumPicGroup;
    Pics: IPhoaPicList;
    i, idxOld, iNewIndex: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Group', IPhotoAlbumPicGroup, Group);
    Params.ObtainValIntf('Pics',  IPhotoAlbumPicList,  Pics);
    iNewIndex := Params.ValInt['NewIndex'];
     // Запоминаем группу
    OpGroup := Group;
     // Выполняем операцию
    for i := 0 to Pics.Count-1 do begin
       // -- Пишем признак продолжения
      UndoStream.WriteBool(True);
       // -- Запоминаем индексы
      idxOld := Group.Pics.IndexOfID(Pics[i].ID);
      if idxOld<iNewIndex then Dec(iNewIndex);
      UndoStream.WriteInt(idxOld);
      UndoStream.WriteInt(iNewIndex);
       // -- Перемещаем изображение на новое место
      Group.PicsX.Move(idxOld, iNewIndex);
      Inc(iNewIndex);
       // Добавляем флаги изменений
      Include(Changes, pocGroupPicList);
    end;
     // Пишем стоп-флаг
    UndoStream.WriteBool(False);
  end;

  procedure TPhoaOp_PicDragAndDropInsideGroup.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    i: Integer;
    g: IPhotoAlbumPicGroup;
    Indexes: TIntegerList;
  begin
    g := OpGroup;
     // Загружаем индексы из файла во временный список
    Indexes := TIntegerList.Create(True);
    try
      while UndoStream.ReadBool do begin
        Indexes.Add(UndoStream.ReadInt);
        Indexes.Add(UndoStream.ReadInt);
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
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewNew
   //===================================================================================================================

  procedure TPhoaOp_ViewNew.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Groupings: IPhotoAlbumPicGroupingList;
    Sortings: IPhotoAlbumPicSortingList;
    View: IPhotoAlbumView;
    iNewViewIndex: Integer;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('Groupings', IPhotoAlbumPicGroupingList, Groupings);
    Params.ObtainValIntf('Sortings',  IPhotoAlbumPicSortingList,  Sortings);
     // Сохраняем предыдущий текущий индекс представления проекта
    UndoStream.WriteInt(Project.ViewIndex);
     // Выполняем операцию
    View := NewPhotoAlbumView(Project.ViewsX);
    View.Name             := Params.ValStr['Name'];
    View.FilterExpression := Params.ValStr['FilterExpression'];
    View.GroupingsX.Assign(Groupings);
    View.SortingsX.Assign(Sortings);
     // Сохраняем новый индекс представления
    iNewViewIndex := View.Index;
    UndoStream.WriteInt(iNewViewIndex);
     // Перегружаем список
    Project.ViewIndex := iNewViewIndex;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
  end;

  procedure TPhoaOp_ViewNew.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var iPrevViewIndex, iNewViewIndex: Integer;
  begin
     // Получаем сохранённые данные
    iPrevViewIndex := UndoStream.ReadInt;
    iNewViewIndex  := UndoStream.ReadInt;
     // Удаляем представление
    Project.ViewsX.Delete(iNewViewIndex);
     // Восстанавливаем прежнее выбранное представление
    Project.ViewIndex := iPrevViewIndex;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewEdit
   //===================================================================================================================

  procedure TPhoaOp_ViewEdit.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    View: IPhotoAlbumView;
    Groupings: IPhotoAlbumPicGroupingList;
    Sortings: IPhotoAlbumPicSortingList;
    bWriteGroupings, bWriteSortings: Boolean;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Получаем параметры
    Params.ObtainValIntf('View',      IPhotoAlbumView,            View);
    Params.ObtainValIntf('Groupings', IPhotoAlbumPicGroupingList, Groupings, False);
    Params.ObtainValIntf('Sortings',  IPhotoAlbumPicSortingList,  Sortings,  False);
     // Сохраняем данные отката и применяем изменения
    UndoStream.WriteWStr(View.Name);
    UndoStream.WriteWStr(View.FilterExpression);
    View.Name             := Params.ValStr['Name'];
    View.FilterExpression := Params.ValStr['FilterExpression'];
     // Запоминаем новый индекс представления (ПОСЛЕ присвоения имени, т.к. оно изменяет позицию представления в списке)
    UndoStream.WriteInt(View.Index);
     // Список группировок создаём и сохраняем, если он есть
    bWriteGroupings := Groupings<>nil;
    UndoStream.WriteBool(bWriteGroupings); // Признак наличия группировок
    if bWriteGroupings then begin
      UndoWriteGroupings(UndoStream, View.GroupingsX);
      View.GroupingsX.Assign(Groupings);
      View.Invalidate;
    end;
     // Список сортировок создаём и сохраняем, если он есть
    bWriteSortings := Sortings<>nil;
    UndoStream.WriteBool(bWriteSortings); // Признак наличия сортировок
    if bWriteSortings then begin
      UndoWriteSortings(UndoStream, View.SortingsX);
      View.SortingsX.Assign(Sortings);
      View.Invalidate;
    end;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
     // Обновляем текущий индекс представления (мог поменяться после переименования представления)
    Project.ViewIndex := View.Index;
  end;

  procedure TPhoaOp_ViewEdit.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    wsViewName, wsFilterExpression: WideString;
    iViewIndex: Integer;
    View: IPhotoAlbumView;
  begin
     // Восстанавливаем представление
    wsViewName         := UndoStream.ReadWStr;
    wsFilterExpression := UndoStream.ReadWStr;
    iViewIndex         := UndoStream.ReadInt;
    View := Project.ViewsX[iViewIndex];
    View.Name             := wsViewName;
    View.FilterExpression := wsFilterExpression;
    if UndoStream.ReadBool then UndoReadGroupings(UndoStream, View.GroupingsX);
    if UndoStream.ReadBool then UndoReadSortings (UndoStream, View.SortingsX);
    View.Invalidate;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
     // Обновляем текущий индекс представления (мог поменяться после переименования представления)
    Project.ViewIndex := View.Index;
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewDelete
   //===================================================================================================================

  procedure TPhoaOp_ViewDelete.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var View: IPhotoAlbumView;
  begin
    inherited Perform(Params, UndoStream, Changes);
     // Сохраняем данные отката
    View := Project.CurrentViewX;
    UndoStream.WriteWStr(View.Name);
    UndoWriteGroupings(UndoStream, View.GroupingsX);
    UndoWriteSortings (UndoStream, View.SortingsX);
     // Удаляем представление
    Project.ViewsX.Delete(Project.ViewIndex);
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
  end;

  procedure TPhoaOp_ViewDelete.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var View: IPhotoAlbumView;
  begin
      // Создаём представление
    View := NewPhotoAlbumView(Project.ViewsX);
    View.Name := UndoStream.ReadWStr;
    UndoReadGroupings(UndoStream, View.GroupingsX);
    UndoReadSortings (UndoStream, View.SortingsX);
     // Активизируем представление
    Project.ViewIndex := View.Index;
     // Добавляем флаги изменений
    Include(Changes, pocViewList);
    inherited RollbackChanges(UndoStream, Changes);
  end;

   //===================================================================================================================
   // TPhoaOp_ViewMakeGroup
   //===================================================================================================================

  procedure TPhoaOp_ViewMakeGroup.Perform(Params: IPhoaOperationParams; UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  var
    Group, ViewGroup: IPhotoAlbumPicGroup;
    View: IPhotoAlbumView;
  begin
    inherited Perform(Params, UndoStream, Changes);
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

  procedure TPhoaOp_ViewMakeGroup.RollbackChanges(UndoStream: IPhoaUndoDataStream; var Changes: TPhoaOperationChanges);
  begin
     // Удаляем корневую группу копии представления
    OpGroup.Owner := nil;
     // Устанавливаем режим отображения групп
    Project.ViewIndex := -1;
     // Добавляем флаги изменений
    Changes := Changes+[pocViewIndex, pocGroupStructure];
    inherited RollbackChanges(UndoStream, Changes);
  end;

type  
   //===================================================================================================================
   // TPhoaUndoDataStream - реализация IPhoaUndoDataStream, файл отката PhoA (организован по принципу стека)
   //===================================================================================================================
   // Формат файла:
   //    <данные1><тип1><данные2><тип2>...
   //    Позиция в потоке всегда сохраняется *за последним байтом потока*

   // Тип данных, сохраняемых в файле
  TPhoaUndoDataStreamDatatype = (pudsdWideStr, pudsdInt, pudsdByte, pudsdBool, pudsdRaw);

  TPhoaUndoDataStream = class(TInterfacedObject, IPhoaDataStream, IPhoaUndoDataStream)
  private
     // Файловый поток данных отката
    FStream: TFileStream;
     // Счётчик вложенности вызовов BeginUndo/EndUndo
    FUndoCounter: Integer;
     // Положение, запомненное в первом вызове BeginUndo
    FUndoPosition: Int64;
     // Prop storage
    FFileName: WideString;
     // Создаёт поток, если он ещё не создан
    procedure CreateStream;
     // Записывает в поток тип данных
    procedure WriteDatatype(DT: TPhoaUndoDataStreamDatatype);
     // Считывает из файла байт типа данных и проверяет его на соответствие DTRequired. Если не совпадает, вызывает
     //   Exception
    procedure ReadCheckDatatype(DTRequired: TPhoaUndoDataStreamDatatype);
     // IPhoaDataStream
    function  GetPosition: Int64;
    function  ReadBool: Boolean;
    function  ReadByte: Byte;
    function  ReadInt: Integer;
    function  ReadRaw:  TPhoaRawData;
    function  ReadWStr: WideString;
    procedure Clear;
    procedure WriteBool(b: Boolean);
    procedure WriteByte(b: Byte);
    procedure WriteInt (i: Integer);
    procedure WriteRaw (const Data: TPhoaRawData);
    procedure WriteWStr (const ws: WideString);
     // IPhoaUndoDataStream
    function  GetFileName: WideString;
    procedure BeginUndo(i64Position: Int64);
    procedure EndUndo(bTruncate: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  procedure TPhoaUndoDataStream.BeginUndo(i64Position: Int64);
  begin
    if FUndoCounter=0 then FUndoPosition := i64Position;
    FStream.Position := i64Position;
    Inc(FUndoCounter);
  end;

  procedure TPhoaUndoDataStream.Clear;
  begin
    if FStream<>nil then begin
      FreeAndNil(FStream);
      SysUtils.DeleteFile(FFileName);
    end;
  end;

  constructor TPhoaUndoDataStream.Create;
  begin
    inherited Create;
     // Определяем имя файла
    FFileName := Format('%sphoa_undo_%.8x.tmp', [GetWindowsTempPath, GetCurrentProcessId]);
  end;

  procedure TPhoaUndoDataStream.CreateStream;
  begin
    if FStream=nil then FStream := TFileStream.Create(FFileName, fmCreate);
  end;

  destructor TPhoaUndoDataStream.Destroy;
  begin
     // Уничтожаем поток и файл
    Clear;
    inherited Destroy;
  end;

  procedure TPhoaUndoDataStream.EndUndo(bTruncate: Boolean);
  begin
    Assert(FUndoCounter>0, 'Excessive TPhoaUndoDataStream.EndUndo() call');
    Dec(FUndoCounter);
     // Счётчик сравнялся с нулём. Если надо, позиционируем в запомненную позицию и усекаем файл
    if (FUndoCounter=0) and bTruncate then begin
      FStream.Position := FUndoPosition;
      FStream.Size     := FUndoPosition;
    end;
  end;

  function TPhoaUndoDataStream.GetFileName: WideString;
  begin
    Result := FFileName;
  end;

  function TPhoaUndoDataStream.GetPosition: Int64;
  begin
    CreateStream;
    Result := FStream.Position;
  end;

  function TPhoaUndoDataStream.ReadBool: Boolean;
  begin
    ReadCheckDatatype(pudsdBool);
    Result := StreamReadByte(FStream)<>0;
  end;

  function TPhoaUndoDataStream.ReadByte: Byte;
  begin
    ReadCheckDatatype(pudsdByte);
    Result := StreamReadByte(FStream);
  end;

  procedure TPhoaUndoDataStream.ReadCheckDatatype(DTRequired: TPhoaUndoDataStreamDatatype);
  var DTActual: TPhoaUndoDataStreamDatatype;
  begin
    if FStream=nil then raise Exception.Create('Attempt of reading before writing to undo file');
    Byte(DTActual) := StreamReadByte(FStream);
    if DTActual<>DTRequired then
      raise Exception.CreateFmt(
        'Invalid undo stream datatype; required: %s, actual: %s',
        [GetEnumName(TypeInfo(TPhoaUndoDataStreamDatatype), Byte(DTRequired)), GetEnumName(TypeInfo(TPhoaUndoDataStreamDatatype), Byte(DTActual))]);
  end;

  function TPhoaUndoDataStream.ReadInt: Integer;
  begin
    ReadCheckDatatype(pudsdInt);
    Result := StreamReadInt(FStream);
  end;

  function TPhoaUndoDataStream.ReadRaw: TPhoaRawData;
  begin
    ReadCheckDatatype(pudsdRaw);
    Result := StreamReadRaw(FStream);
  end;

  function TPhoaUndoDataStream.ReadWStr: WideString;
  begin
    ReadCheckDatatype(pudsdWideStr);
    Result := StreamReadWStr(FStream);
  end;

  procedure TPhoaUndoDataStream.WriteBool(b: Boolean);
  begin
    CreateStream;
    WriteDatatype(pudsdBool);
    StreamWriteByte(FStream, Byte(b));
  end;

  procedure TPhoaUndoDataStream.WriteByte(b: Byte);
  begin
    CreateStream;
    WriteDatatype(pudsdByte);
    StreamWriteByte(FStream, b);
  end;

  procedure TPhoaUndoDataStream.WriteDatatype(DT: TPhoaUndoDataStreamDatatype);
  begin
    CreateStream;
    StreamWriteByte(FStream, Byte(DT));
  end;

  procedure TPhoaUndoDataStream.WriteInt(i: Integer);
  begin
    CreateStream;
    WriteDatatype(pudsdInt);
    StreamWriteInt(FStream, i);
  end;

  procedure TPhoaUndoDataStream.WriteRaw(const Data: TPhoaRawData);
  begin
    CreateStream;
    WriteDatatype(pudsdRaw);
    StreamWriteRaw(FStream, Data);
  end;

  procedure TPhoaUndoDataStream.WriteWStr(const ws: WideString);
  begin
    CreateStream;
    WriteDatatype(pudsdWideStr);
    StreamWriteWStr(FStream, ws);
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

  function NewPhoaPicFileChangeList: IPhoaPicFileChangeList;
  begin
    Result := TPhoaPicFileChangeList.Create;
  end;

  function  NewPhoaUndoDataStream: IPhoaUndoDataStream;
  begin
    Result := TPhoaUndoDataStream.Create;
  end;

initialization
  OperationFactory := TPhoaOperationFactory.Create;
  with OperationFactory do begin
    RegisterOpClass('GroupDelete',                     TPhoaOp_GroupDelete);
    RegisterOpClass('GroupDragAndDrop',                TPhoaOp_GroupDragAndDrop);
    RegisterOpClass('GroupEdit',                       TPhoaOp_GroupEdit);
    RegisterOpClass('GroupNew',                        TPhoaOp_GroupNew);
    RegisterOpClass('GroupRename',                     TPhoaOp_GroupRename);
    RegisterOpClass('InternalEditPicFiles',            TPhoaOp_InternalEditPicFiles);
    RegisterOpClass('InternalEditPicKeywords',         TPhoaOp_InternalEditPicKeywords);
    RegisterOpClass('InternalEditPicProps',            TPhoaOp_InternalEditPicProps);
    RegisterOpClass('InternalEditPicToGroupBelonging', TPhoaOp_InternalEditPicToGroupBelonging);
    RegisterOpClass('InternalGroupDelete',             TPhoaOp_InternalGroupDelete);
    RegisterOpClass('InternalGroupPicSort',            TPhoaOp_InternalGroupPicSort);
    RegisterOpClass('InternalPicFromGroupRemoving',    TPhoaOp_InternalPicFromGroupRemoving);
    RegisterOpClass('InternalPicToGroupAdding',        TPhoaOp_InternalPicToGroupAdding);
    RegisterOpClass('InternalUnlinkedPicsRemoving',    TPhoaOp_InternalUnlinkedPicsRemoving);
    RegisterOpClass('PicAdd',                          TPhoaOp_PicAdd);
    RegisterOpClass('PicDelete',                       TPhoaOp_PicDelete);
    RegisterOpClass('PicDeleteFromProject',            TPhoaOp_PicDeleteFromProject);
    RegisterOpClass('PicDeleteWithFiles',              TPhoaOp_PicDeleteWithFiles);
    RegisterOpClass('PicDragAndDropInsideGroup',       TPhoaOp_PicDragAndDropInsideGroup);
    RegisterOpClass('PicDragAndDropToGroup',           TPhoaOp_PicDragAndDropToGroup);
    RegisterOpClass('PicEdit',                         TPhoaOp_PicEdit);
    RegisterOpClass('PicOperation',                    TPhoaOp_PicOperation);
    RegisterOpClass('PicPaste',                        TPhoaOp_PicPaste);
    RegisterOpClass('PicSort',                         TPhoaOp_PicSort);
    RegisterOpClass('ProjectEdit',                     TPhoaOp_ProjectEdit);
    RegisterOpClass('StoreTransform',                  TPhoaOp_StoreTransform);
    RegisterOpClass('ViewDelete',                      TPhoaOp_ViewDelete);
    RegisterOpClass('ViewEdit',                        TPhoaOp_ViewEdit);
    RegisterOpClass('ViewMakeGroup',                   TPhoaOp_ViewMakeGroup);
    RegisterOpClass('ViewNew',                         TPhoaOp_ViewNew);
  end;
finalization
  OperationFactory := nil;
end.
