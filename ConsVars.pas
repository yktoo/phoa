//**********************************************************************************************************************
//  $Id: ConsVars.pas,v 1.2 2004-04-15 12:54:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ConsVars;

interface
uses
   // GR32 must follow GraphicEx because of naming conflict between stretch filter constants
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Registry, IniFiles, GraphicEx, GR32, VirtualTrees, TB2Dock,
  TBX;

type
  TPicProperty = (
    ppID, ppFileName, ppFullFileName, ppFilePath, ppFileSize, ppFileSizeBytes, ppPicWidth, ppPicHeight, ppPicDims,
    ppFormat, ppDate, ppTime, ppPlace, ppFilmNumber, ppFrameNumber, ppAuthor, ppDescription, ppNotes, ppMedia,
    ppKeywords);
  TPicProperties = set of TPicProperty;
const
  PPAllProps = [Low(TPicProperty)..High(TPicProperty)];

type
   // Типы данных свойств изображения
  TPicPropertyDatatype = (ppdInteger, ppdString, ppdPixelFormat, ppdDate, ppdTime, ppdStrings);

   // Свойства для автоматического заполнения даты/времени изображения
  TDateTimeAutofillProp = (
    dtapExifDTOriginal,  // EXIF: Original date/time
    dtapExifDTDigitized, // EXIF: Digitized date/time
    dtapExifDateTime,    // EXIF: Date and time
    dtapFilename,        // Extract from filename
    dtapDTFileCreated,   // Date/time file created
    dtapDTFileModified); // Date/time file modified
  TDateTimeAutofillProps = set of TDateTimeAutofillProp;
const
  DTAP_DefaultDateProps: TDateTimeAutofillProps = [
    dtapExifDTOriginal, dtapExifDTDigitized, dtapExifDateTime, dtapFilename, dtapDTFileCreated, dtapDTFileModified];
  DTAP_DefaultTimeProps: TDateTimeAutofillProps = [
    dtapExifDTOriginal, dtapExifDTDigitized, dtapExifDateTime, dtapDTFileCreated, dtapDTFileModified];

type
   // Результат попытки автозаполнения даты/времени изображения
  TDateTimeFillResult = (
    dtfrEmpty,     // Осталось незаполненным
    dtfrSpecified, // Значение было заполнено, и оставлено без изменений
    dtfrEXIF,      // Взято из EXIF-тега
    dtfrFilename,  // Взято из имени файла
    dtfrCreation,  // Взято из времени создания файла
    dtfrModified); // Взято из времени записи файла

   // Отображаемые свойства файла
  TDiskFileProp = (
    dfpFileName,            // Имя файла
    dfpFileType,            // Тип файла
    dfpSizeOfFile,          // Размер
    dfpSizeOfFileDiskUsage, // Занимаемый файлом размер на диске
    dfpCreationTime,        // Время создания
    dfpLastWriteTime,       // Время записи
    dfpLastAccessTime,      // Время последнего доступа
    dfpReadOnlyFile,        // Атрибут файла только-для-чтения
    dfpHidden,              // Атрибут скрытого файла
    dfpArchive,             // Архивный атрибут
    dfpCompressed,          // Атрибут сжатия файла
    dfpSystemFile,          // Атрибут системного файла
    dfpTemporary);          // Атрибут временного файла

  TSortOrder = (soAsc, soDesc);

   // Свойство, по которому поизводится группировка изображений в представлении
  TGroupByProperty = (
    gbpFilePath, gbpDateByYear, gbpDateByMonth, gbpDateByDay, gbpTimeHour, gbpTimeMinute, gbpPlace, gbpFilmNumber,
    gbpAuthor, gbpMedia, gbpKeywords);
  TGroupByProperties = set of TGroupByProperty;
const
   // Свойства, поддерживаемые ревизией 2
  GBPropsRev2: TGroupByProperties = [
    gbpFilePath, gbpDateByYear, gbpDateByMonth, gbpDateByDay, gbpPlace, gbpFilmNumber, gbpKeywords];

type
   // Режим массовой простановки отметки
  TMassCheckMode = (mcmAll, mcmNone, mcmInvert);

   // Возможные операции с изображениями (доступные через пункт меню Сервис | Операции с изображениями)
  TPictureOperation = (
    popMoveToTarget,     // Переместить выделенные изображения в указанную группу
    popCopyToTarget,     // Копировать выделенные изображения в указанную группу
    popRemoveFromTarget, // Удалить выделенные изображения из указанной группы
    popIntersectTarget); // Оставить только выделенные изображения в указанной группе

   // Флаги требуемого обновления операции отката
  TUndoInvalidationFlag  = (
     // -- Флаги действий при выполнении (eXecution)
    uifXReinitParent,        // Переинициализировать родителя узла группы операции. Должно быть заполнено Op.ParentGroupAbsIdx
    uifXReinitSiblings,      // Переинициализировать соседние с узлом группы операции узлы дерева. Должно быть заполнено Op.ParentGroupAbsIdx
    uifXReinitRecursive,     // При переинициализации (флаги uifXReinitParent и uifXReinitSiblings) действовать рекурсивно
    uifXEditGroup,           // Ввести узел группы Op.GroupAbsIdx в режим редактирования его текста. Должно быть заполнено Op.GroupAbsIdx
    uifXInvalidateNode,      // Сделать Invalidate узлу группы. Должно быть заполнено Op.GroupAbsIdx
    uifXInvalidateTree,      // Сделать Invalidate всму контролу дерева групп
     // -- Флаги действий при откате (Undoing)
    uifUReinitAll,           // Переинициализировать все узлы дерева
    uifUReinitParent,        // Переинициализировать родителя узла группы операции. Должно быть заполнено Op.ParentGroupAbsIdx
    uifUReinitRecursive,     // При переинициализации (флаги uifUReinitParent и uifUReinitSiblings) действовать рекурсивно
    uifUInvalidateNode,      // Сделать Invalidate узлу группы. Должно быть заполнено Op.GroupAbsIdx
    uifUInvalidateTree);     // Сделать Invalidate всему контролу дерева групп

  TUndoInvalidationFlags = set of TUndoInvalidationFlag;

  TIDArray = Array of Integer;

   // Флаги форматов буфера обмена для данных изображений
  TPicClipboardFormat = (
    pcfPhoa,          // Внутренний формат данных программы (wClipbrdPicFormatID)
    pcfHDrop,         // Список Shell-объектов (т.е. файлов)
    pcfPlainList,     // Простой текстовый список путей к файлам
    pcfSingleBitmap); // Bitmap-изображение эскиза (в случае единственного изображения)
  TPicClipboardFormats = set of TPicClipboardFormat;
const
  DefaultPicClipboardFormats: TPicClipboardFormats = [Low(TPicClipboardFormat)..High(TPicClipboardFormat)];

type
   // Вид фильтра присутствия файлов в фотоальбоме (при добавлении изображений)
  TAddFilePresenceFilter = (
    afpfDontCare,      // Отключить критерий
    afpfNewOnly,       // Только файлы, новые для фотоальбома
    afpfExistingOnly); // Только файлы, присутствующие в фотоальбоме

   //-------------------------------------------------------------------------------------------------------------------
   // Операции с файлами
   //-------------------------------------------------------------------------------------------------------------------

   // Вид операции с файлами изображений
  TFileOperationKind = (
    fokCopyFiles,        // Копировать файлы изображений в указанную папку
    fokMoveFiles,        // Переместить файлы изображений в указанную папку
    fokDeleteFiles,      // Удалить файлы изображений
    fokRebuildThumbs,    // Перестроить эскизы
    fokRepairFileLinks); // Восстановить связи с файлами изображений, находящимися в указанной папке

   // Режим выбора изображений для операций с файлами изображений
  TFileOpSelPicMode = (
    fospmSelPics,    // Выбранные во вьюере изображения
    fospmAll,        // Все изображения фотоальбома/представления
    fospmSelGroups); // Изображения из указанных групп

   // Режим размещения файлов при копировании/перемещении с использованием операций с файлами изображений
  TFileOpMoveFileArranging = (
    fomfaPutFlatly,            // Положить все файлы в единственный каталог - каталог назначения
    fomfaMaintainFolderLayout, // Разложить файлы по каталогам, повторяя их исходное расположение (относительно указанной папки)
    fomfaMaintainGroupLayout); // Разложить файлы по каталогам, повторяя расположение групп изображений (относительно указанной группы)

   // Поведение при отсутствии исходного файла при перемещении с использованием операций с файлами изображений
  TFileOpMoveFileNoOriginalMode = (
    fomfnomFail,                 // Считать ошибкой
    fomfnomRelinkIfTargetExists, // Обновлять ссылку, если файл назначения существует
    fomfnomRelinkAlways);        // Обновлять ссылку в любом случае

   // Режим перезаписи существующих файлов при копировании/перемещении с использованием операций с файлами изображений
  TFileOpMoveFileOverwriteMode = (
    fomfomNever,   // Никогда не переписывать (пропускать)
    fomfomPrompt,  // Спрашивать о необходимости переписать
    fomfomAlways); // Всегда переписывать

   // Флаги поиска файлов при восстановлении ссылок на файлы изображений
  TFileOpRepairMatchFlag = (
    formfName,  // Совпадение по имени файла
    formfSize); // Совпадение по размеру файла
  TFileOpRepairMatchFlags = set of TFileOpRepairMatchFlag;

   //-------------------------------------------------------------------------------------------------------------------
   // Предметные интерфейсы страниц мастеров
   //-------------------------------------------------------------------------------------------------------------------

   // Интерфейс предоставления информации о протоколе (для страницы, отображающей протокол)
  IPhoaWizardPageHost_Log = interface(IInterface)
    ['{9FA1E911-FFF6-44E6-9A32-D7AB76D759B0}']
     // Prop handlers
    function  GetLog(iPageID: Integer): TStrings;
     // Props
     // -- Список строк протокола
    property Log[iPageID: Integer]: TStrings read GetLog;
  end;

   // Интерфейс предоставления информации о состоянии обработки (для страницы, отображающей состояние процесса)
  IPhoaWizardPageHost_Process = interface(IInterface)
    ['{A064413E-80A3-41B0-B6B7-0FA8C9EB76E6}']
     // Должна запускать обработку 
    procedure StartProcessing;
     // Должна прерывать обработку 
    procedure StopProcessing;
     // Должна рисовать изображение обрабатываемого эскиза
    procedure PaintThumbnail(Bitmap: TBitmap); 
     // Prop handlers
    function GetCurrentStatus: String;
    function GetProcessingActive: Boolean;
    function GetProgressCur: Integer;
    function GetProgressMax: Integer;
     // Props
     // -- Текущее состояние обработки (сообщение)
    property CurrentStatus: String read GetCurrentStatus; 
     // -- True, если оработка в данный момент активна
    property ProcessingActive: Boolean read GetProcessingActive;
     // -- Текущее состояние прогресса
    property ProgressCur: Integer read GetProgressCur;
     // -- Максимальное (конечное) состовние прогресса
    property ProgressMax: Integer read GetProgressMax;
  end;

  {=====================================================================================================================
    Формат файла фотоальбома до версии 1.1.1a (Rev 0002-):

    . Подпись "PhoA [PhotoAlbum] project file"
    . Int - FileRevision
    . Str - Description
    . Int - ThumbnailQuality
    . Int - ThumbnailWidth
    . Int - ThumbnailHeight
    . Группы (RootGroup)
      ( . Str - Text
        . Byte - Expanded
        . Int - Picture ID count=N
          ( . Int - Pic ID
          ) x N
        . Подчинённые группы (Groups)
          ( . Int - Count=N
            . Группы
              ( . ) x N
           )
      )
    . Изображения (Pics)
      ( . Int - Count=N
        . Изображения
          ( . Int  - ID
            . Str  - ThumbnailData
            . Str  - FilmNumber
            . Int  - PicDate [$0002-]
            . Dbl  - PicDateTime [$0003+]
            . Int  - PicWidth [$0003+]
            . Int  - PicHeight [$0003+]
            . Str  - PicNotes [$0003+]
            . Str  - PicAuthor [$0003+]
            . Str  - PicMedia [$0003+]
            . Str  - PicDesc
            . Str  - PicFileName
            . Int  - PicFileSize
            . Byte - PicFormat
            . Str  - PicKeywords
            . Str  - PicNumber
            . Str  - PicPlace
            . Int  - ThumbWidth
            . Int  - ThumbHeight
          ) x N
      )
    . Представления (Views) [$0002+]
      ( . Int - Count=N
        . Представления
          ( . Str - Name
            . Группировки (Groupings)
              ( . Int - Count=M
                . Группировки
                  ( . Int - Grouping [typecasted to Integer]
                  ) x M
              )
            . Сортировки (Sortings) [$0003+]
              ( . Int - Count=M
                . Сортировки
                  ( . Int - Sorting [typecasted to Integer]
                  ) x M
              )
          ) x N
      )
  =====================================================================================================================}

const
   // Версия программы
  SAppVersion                     = 'v1.1.4 beta';
   // Расширение и имя файла фотоальбома по умолчанию
  SDefaultExt                     = 'phoa';
  SDefaultFName                   = 'untitled.'+SDefaultExt;
   // Имя ini-файла для сохранения/загрузки параметров по умолчанию
  SDefaultIniFileExt              = 'ini';
  SDefaultIniFileName             = 'phoa.'+SDefaultIniFileExt;
   // Имя исполняемого файла
  SPhoaExecutableFileName         = 'phoa.exe';

   // Перевод строки
  S_CRLF                          = #13#10;

   // Размеры и качество эскиза по умолчанию
  IDefaultThumbWidth              = 150;
  IDefaultThumbHeight             = 150;
  IDefaultThumbQuality            = 40;

   // Отступы эскиза в сетке в пикселах
  ILThumbMargin                   = 5;
  IRThumbMargin                   = 5;
  ITThumbMargin                   = 5;
  IBThumbMargin                   = 5;

   // Максимальное к-во записей в истории ввода
  IMaxHistoryEntries              = 20;

   // Крайние значения коэффициента масштабирования изображения. Typed constants to avoid rounding mismatches
  SMaxPicZoom: Single             = 8.0;
  SMinPicZoom: Single             = 0.1;

   // Шаг прокрутки изображения клавишами со стрелками, в пикселах
  IKeyScrollStep                  = 50;  // [Arrow Keys]
  IKeySlowScrollStep              = 5;   // [Shift]+[Arrow Keys]
  IKeyQuickScrollStep             = 200; // [Ctrl]+[Arrow Keys]

   // Ключи реестра для сохранения настроек
  SRegRoot                        = 'Software\DaleTech\PhoA';
  SRegPrefs                       = 'Preferences';
  SRegToolbars                    = SRegRoot+'\Toolbars';
  SRegOpen_Root                   = 'Open';
  SRegOpen_FilesMRU               = SRegOpen_Root+'\FilesMRU';

  SRegDialogsRoot                 = 'Dialogs';

  SRegAddFiles_Root               = SRegDialogsRoot+'\AddFilesWizard';
    SRegAddFiles_MaskMRU          = SRegAddFiles_Root+'\FileMaskMRU';

  SRegPicProps_Root               = SRegDialogsRoot+'\PicPropsDialog';
    SRegPicProps_Toolbars         = SRegRoot+'\'+SRegPicProps_Root+'\Toolbars';

  SRegCopyToFolder_Root           = SRegDialogsRoot+'\CopyToFolder';

  SRegFileOps_Root                = SRegDialogsRoot+'\FileOpsWizard';

  SRegSearch_Root                 = SRegDialogsRoot+'\Search';
    SRegSearch_IDMRU              = SRegSearch_Root+'\IDMRU';
    SRegSearch_FMaskMRU           = SRegSearch_Root+'\FileMaskMRU';
    SRegSearch_FPathMRU           = SRegSearch_Root+'\FilePathMRU';
    SRegSearch_FSizeMRU           = SRegSearch_Root+'\FileSizeMRU';
    SRegSearch_PWidthMRU          = SRegSearch_Root+'\PicWidthMRU';
    SRegSearch_PHeightMRU         = SRegSearch_Root+'\PicHeightMRU';
    SRegSearch_FrNumberMRU        = SRegSearch_Root+'\FrameNumberMRU';
    SRegSearch_DescMRU            = SRegSearch_Root+'\DescMRU';
    SRegSearch_NotesMRU           = SRegSearch_Root+'\NotesMRU';

  SRegSort_Root                   = SRegDialogsRoot+'\Sort';
    SRegSort_LastSortings         = SRegSort_Root+'\LastSortings';

   // Недопустимые в пути к файлу символы
  SInvalidPathChars               = '\/:*?"<>|';

   // Номера курсоров
  crHand                          = 2100;
  crHandDrag                      = 2101;
  crDragMove                      = 2102;
  crDragCopy                      = 2103;

   // Параметры Drag'n'Drop для TThumbnailViewer
   // -- Цвет линии вставки при Drag'n'drop
  CInsertionPoint                 = clHighlight;
   // -- Расстояние в пикселах от курсора мыши до края клиентской области, при котором происходит скроллинг содержимого
  IDragScrollAreaMargin           = 30;
   // -- Задержка в мс, в течение которой прокручивается одна строка эскизов
  IDragScrollDelay                = 400;

   // Набор возможных изменений масштаба
  adMagnifications: Array[0..6] of Double = (1.10, 1.25, 1.33, 1.50, 1.75, 2, 3);
   // Курсоры для просмотра изображений
  aImgViewCursors: Array[Boolean] of TCursor = (crDefault, crHand);

   // Наименования констант с наименованиями папок для неклассифицированных изображений в зависимости от вида группировки
  asUnclassifiedConsts: Array[TGroupByProperty] of String[24] = (
    '',                         // gbpFilePath - такого не бывает
    'SUnclassified_Date',       // gbpDateByYear
    'SUnclassified_Date',       // gbpDateByMonth
    'SUnclassified_Date',       // gbpDateByDay
    'SUnclassified_Time',       // gbpTimeHour
    'SUnclassified_Time',       // gbpTimeMinute
    'SUnclassified_Place',      // gbpPlace
    'SUnclassified_FilmNumber', // gbpFilmNumber
    'SUnclassified_Author',     // gbpAuthor
    'SUnclassified_Media',      // gbpMedia
    'SUnclassified_Keywords');  // gbpKeywords

   // Timer IDs
  ISlideShowTimerID               = $01010101;
  IPicPropsViewTimerID            = $01010101;

   // Image indices
  iiPhoA                          =  0;
  iiFolder                        =  1;
  iiNo                            =  2;
  iiSearch                        =  3;
  iiView                          =  4;
  iiGrouping                      =  5;
  iiSorting                       =  6;
  iiSortAsc                       =  7;
  iiSortDesc                      =  8;
  iiDelete                        =  9;
  iiUp                            = 10;
  iiDown                          = 11;
  iiOK                            = 12;
  iiError                         = 13;

   // ImageIndices (from ilActionsSmall)
  iiA_NewGroup                    =  5;
  iiA_Edit                        =  8;
  iiA_Props                       = 12;
  iiA_ViewMode                    = 15;
  iiA_View                        = 37;
  iiA_Keyword                     = 41;
  iiA_Asterisk                    = 43;
  iiA_Dialog                      = 46;

   // Help topics
  IDH_start                       = 00001;
  IDH_intf_album_props            = 00060;
  IDH_intf_browse_mode_menu       = 00070;
  IDH_intf_browse_mode_tasks      = 00080;
  IDH_intf_browse_mode_views      = 00090;
  IDH_intf_browse_mode            = 00100;
  IDH_intf_key_mouse_controls     = 00110;
  IDH_intf_major_modes            = 00120;
  IDH_intf_pic_add                = 00130;
  IDH_intf_pic_add_checkfiles     = 00131;
  IDH_intf_pic_add_log            = 00132;
  IDH_intf_pic_add_process        = 00133;
  IDH_intf_pic_add_selfiles       = 00134;
  IDH_intf_pic_operations         = 00140;
  IDH_intf_pic_props_data         = 00150;
  IDH_intf_pic_props_fprops       = 00160;
  IDH_intf_pic_props_groups       = 00170;
  IDH_intf_pic_props_keywords     = 00180;
  IDH_intf_pic_props_metadata     = 00190;
  IDH_intf_pic_props_view         = 00200;
  IDH_intf_pic_props              = 00210;
  IDH_intf_search                 = 00230;
  IDH_intf_sel_phoa_group         = 00240;
  IDH_intf_select_keywords        = 00250;
  IDH_intf_sort_pics              = 00260;
  IDH_intf_stats                  = 00270;
  IDH_intf_view_mode_tasks        = 00280;
  IDH_intf_view_mode              = 00290;
  IDH_intf_view_props             = 00300;
  IDH_intf_file_ops               = 00400;
  IDH_intf_file_ops_cdopts        = 00410;
  IDH_intf_file_ops_delopts       = 00420;
  IDH_intf_file_ops_log           = 00430;
  IDH_intf_file_ops_moveopts      = 00440;
  IDH_intf_file_ops_process       = 00450;
  IDH_intf_file_ops_repropts      = 00460;
  IDH_intf_file_ops_selfolder     = 00470;
  IDH_intf_file_ops_selpics       = 00480;
  IDH_intf_file_ops_seltask       = 00490;
  IDH_intf_file_ops_task_copy     = 00600;
  IDH_intf_file_ops_task_del      = 00610;
  IDH_intf_file_ops_task_move     = 00620;
  IDH_intf_file_ops_task_repfl    = 00630;
  IDH_intf_file_ops_task_rthumb   = 00640;
  IDH_setup                       = 00700;
  IDH_setup_dialogs               = 00710;
  IDH_setup_general               = 00720;
  IDH_setup_view_mode             = 00730;
  IDH_setup_browse_mode           = 00740;
  IDH_advantages                  = 01010;
  IDH_eula_eng                    = 01020;
  IDH_eula_rus                    = 01030;
  IDH_file_formats                = 01040;
  IDH_how_it_works                = 01050;
  IDH_intro                       = 01060;
  IDH_major_modes                 = 01070;
  IDH_pic_data                    = 01080;
  IDH_pic_prop_autofill           = 01090;
  IDH_requirements                = 01100;
  IDH_rev_history                 = 01110;
  IDH_theory_metadata             = 01120;
  IDH_theory_resampling           = 01130;
  IDH_thumbnails                  = 01140;

   // ID страниц Мастера добавления файлов
  IWzAddFilesPageID_SelFiles      = 1;
  IWzAddFilesPageID_CheckFiles    = 2;
  IWzAddFilesPageID_Processing    = 3;
  IWzAddFilesPageID_Log           = 4;

   // ID страниц Мастера операций с файлами изображений
  IWzFileOpsPageID_SelTask        = 1;
  IWzFileOpsPageID_SelPics        = 2;
  IWzFileOpsPageID_SelFolder      = 3;
  IWzFileOpsPageID_MoveOptions    = 4;
  IWzFileOpsPageID_MoveOptions2   = 5;
  IWzFileOpsPageID_CDOptions      = 6;
  IWzFileOpsPageID_DelOptions     = 7;
  IWzFileOpsPageID_RepairOptions  = 8;
  IWzFileOpsPageID_RepairSelLinks = 9;
  IWzFileOpsPageID_Processing     = 10;
  IWzFileOpsPageID_Log            = 11;

   // ID страниц диалога свойств файлов
  IDlgPicPropsPageID_FileProps    = 1;
  IDlgPicPropsPageID_Metadata     = 2;
  IDlgPicPropsPageID_View         = 3;
  IDlgPicPropsPageID_Data         = 4;
  IDlgPicPropsPageID_Keywords     = 5;
  IDlgPicPropsPageID_Groups       = 6;

   // ID пунктов настройки
   //===================================================================================================================
  ISettingID_Gen                       = 0;    // Общие
  ISettingID_Gen_Intf                  = 0;    // Интерфейс
    ISettingID_Gen_Language            = 0001; // Язык интерфейса
    ISettingID_Gen_MainFont            = 0002; // Шрифт программы
    ISettingID_Gen_TooltipDisplTime    = 0010; // Продолжительность отображения всплывающих подсказок, мс
    ISettingID_Gen_OpenMRUCount        = 0011; // Длина списка последних открытых файлов
    ISettingID_Gen_LookupPhoaIni       = 0012; // Загружать ли настройки из phoa.ini в каталоге запуска
  ISettingID_Gen_Clipboard             = 0;    // Буфер обмена
    ISettingID_Gen_ClipFormats         = 0030; // Форматы, помещаемые в буфер обмена при копировании
  ISettingID_Gen_Toolbars              = 0;    // Панели инструментов
    ISettingID_Gen_ToolbarBtnSize      = 0100; // Размер кнопок основной панели
    ISettingID_Gen_ToolbarBSz16        = 0;    // Мелкие (16x16)
    ISettingID_Gen_ToolbarBSz24        = 0;    // Средние (24x24)
    ISettingID_Gen_ToolbarBSz32        = 0;    // Крупные (32x32)
    ISettingID_Gen_ToolbarDragStyle    = 0110; // Перетаскивание панелей
    ISettingID_Gen_ToolbarDrgNone      = 0;    // Запрещено
    ISettingID_Gen_ToolbarDrgOneDock   = 0;    // Только внутри дока
    ISettingID_Gen_ToolbarDrgNoFloat   = 0;    // Внутри дока или между доками
    ISettingID_Gen_ToolbarDrgFree      = 0;    // В доках и вне доков
  ISettingID_Gen_Tree                  = 0;    // Деревья/списки
    ISettingID_Gen_TreeAnimation       = 0201; // Анимация сворачивания/разворачивания
    ISettingID_Gen_TreeWhPanning       = 0202; // Панорамирование нажатием средней кнопки мыши
    ISettingID_Gen_TreeCenterSel       = 0203; // Центрирование выделения
    ISettingID_Gen_TreeIncrSearch      = 0204; // Поиск при наборе текста
    ISettingID_Gen_TreeIncrSrchDelay   = 0205; // Задержка при наборе для поиска
    ISettingID_Gen_TreeButtonStyle     = 0206; // Вид кнопок сворачивания/разворачивания
    ISettingID_Gen_TreeBS_Rectangle    = 0207; // Стандартные
    ISettingID_Gen_TreeBS_Triangle     = 0208; // Треугольники
    ISettingID_Gen_TreeCheckStyle      = 0209; // Вид переключателей в узлах (на не-XP-системах)
    ISettingID_Gen_TreeCS_DarkCheck    = 0;    // Dark Check
    ISettingID_Gen_TreeCS_DarkTick     = 0;    // Dark Tick
    ISettingID_Gen_TreeCS_Flat         = 0;    // Flat
    ISettingID_Gen_TreeCS_LightCheck   = 0;    // Light Check
    ISettingID_Gen_TreeCS_LightTick    = 0;    // Light Tick
    ISettingID_Gen_TreeCS_Std          = 0;    // Standard
    ISettingID_Gen_TreeCS_StdFlat      = 0;    // Standard Flat
    ISettingID_Gen_TreeCS_XP           = 0;    // XP
    ISettingID_Gen_TreeSelStyle        = 0220; // Вид рамки выделения
    ISettingID_Gen_TreeSelDotted       = 0;    // Пунктир
    ISettingID_Gen_TreeSelBlended      = 0;    // Оттенённый прямоугольник
   //===================================================================================================================
  ISettingID_Browse                    = 0;    // Режим обзора
  ISettingID_Browse_Viewer             = 0;    // Viewer
    ISettingID_Browse_ViewerBkColor    = 1000; // Viewer: Цвет фона
    ISettingID_Browse_ViewerDragDrop   = 1001; // Viewer: Drag'n'Drop
    ISettingID_Browse_ViewerTooltips   = 1002; // Viewer: Показывать всплывающие описания эскизов
    ISettingID_Browse_ViewerTipProps   = 1003; // Viewer: Отображать во всплывающих описаниях
    ISettingID_Browse_ViewerThInfo     = 1010; // Viewer: Данные, отображаемые на эскизах
    ISettingID_Browse_ViewerThLTProp   = 1011; // Viewer: Left top corner
    ISettingID_Browse_ViewerThRTProp   = 1012; // Viewer: Right top corner
    ISettingID_Browse_ViewerThLBProp   = 1013; // Viewer: Left bottom corner
    ISettingID_Browse_ViewerThRBProp   = 1014; // Viewer: Right bottom corner
    ISettingID_Browse_ViewerThBorder   = 1020; // Viewer: Чёткие границы эскиза
    ISettingID_Browse_ViewerCacheThs   = 1030; // Viewer: Кэшировать эскизы при просмотре
    ISettingID_Browse_ViewerCacheSze   = 1031; // Viewer: Размер кэша эскизов
    ISettingID_Browse_ViewerStchFilt   = 1040; // Viewer: Метод ресэмплинга эскизов
    ISettingID_Browse_MaxUndoCount     = 1050; // Макс. количество операций в буфере отмены
   //===================================================================================================================
  ISettingID_View                      = 0;    // Режим просмотра
  ISettingID_View_AlwaysOnTop          = 2000; // Окно просмотра поверх всех окон
  ISettingID_View_Fullscreen           = 2001; // Полноэкранный режим
  ISettingID_View_KeepCursorOverTB     = 2002; // Автопозиционировать курсор над панелью инструментов 
  ISettingID_View_HideCursor           = 2003; // Скрывать указатель мыши в полноэкранном режиме
  ISettingID_View_BkColor              = 2004; // Цвет фона окна просмотра
  ISettingID_View_ShowToolbar          = 2005; // Отображать панель инструментов
  ISettingID_View_PicChange            = 0;    // Смена изображений
    ISettingID_View_FitWindowToPic     = 2010; // Подгонять размер окна под размер изображения
    ISettingID_View_CenterWindow       = 2011; // Центрировать окно на рабочем столе
    ISettingID_View_ShrinkPicToFit     = 2012; // Сжимать изображение до размера окна
    ISettingID_View_ZoomPicToFit       = 2013; // Растягивать изображение до размера окна
    ISettingID_View_Cyclic             = 2014; // Циклический просмотр
    ISettingID_View_Optimizing         = 0;    // Оптимизация скорости
      ISettingID_View_Predecode        = 2020; // Предекодировать следующее изображение
      ISettingID_View_CacheBehind      = 2021; // Кэшировать предыдущее изображение
  ISettingID_View_ZoomFactor           = 2030; // Шаг изменения масштаба
  ISettingID_View_CaptionProps         = 2031; // Отображать в заголовке окна
  ISettingID_View_StchFilt             = 2032; // Метод ресэмплинга изображения
  ISettingID_View_Info                 = 0;    // Отображаемая информация
    ISettingID_View_ShowInfo           = 2040; // Отображать ли информацию
    ISettingID_View_InfoPicProps       = 2041; // Info: Отображаемая информация
    ISettingID_View_InfoFont           = 2042; // Info: Шрифт
    ISettingID_View_InfoBkColor        = 2043; // Info: Цвет фона
    ISettingID_View_InfoBkOpacity      = 2044; // Info: Непрозрачность фона (0-255)
  ISettingID_View_Slideshow            = 0;    // Просмотр слайдов
    ISettingID_View_SlideInterval      = 2050; // Slideshow: Задержка показа, мс
    ISettingID_View_SlideCyclic        = 2051; // Slideshow: Циклический просмотр
   //===================================================================================================================
  ISettingID_Dialogs                   = 0;    // Диалоги
  ISettingID_Dlgs_Confms               = 0;    // Подтверждения
    ISettingID_Dlgs_ConfmDelGroup      = 3010; // Подтверждение: Перед удалением группы
    ISettingID_Dlgs_ConfmDelPics       = 3011; // Подтверждение: Перед удалением изображений
    ISettingID_Dlgs_ConfmDelView       = 3012; // Подтверждение: Перед удалением представления
    ISettingID_Dlgs_ConfmOldFile       = 3013; // Подтверждение: Перед сохранением файла устаревшей версии
    ISettingID_Dlgs_ConfmAppExit       = 3014; // Подтверждение: Перед выходом (при сохранённом фотоальбоме)
  ISettingID_Dlgs_Notifies             = 0;    // Уведомления
    ISettingID_Dlgs_NotifyDragCopy     = 3030; // Уведомление: После копирования изображений через Drag'n'Drop
    ISettingID_Dlgs_NotifyDragMove     = 3031; // Уведомление: После перемещения изображений через Drag'n'Drop
    ISettingID_Dlgs_NotifyPaste        = 3032; // Уведомление: После вставки изображений из буфера обмена
  ISettingID_Dlgs_AddPicWizard         = 0;    // Мастер добавления изображений
    ISettingID_Dlgs_APW_ShowHidden     = 3050; // Отображать скрытые файлы и папки
    ISettingID_Dlgs_APW_SkipChkPage    = 3051; // Пропускать страницу отметки файлов
    ISettingID_Dlgs_APW_LogOnErrOnly   = 3052; // Отображать протокол только при наличии ошибок
    ISettingID_Dlgs_APW_AutofillDate   = 3053; // Автоматически заполнять дату изображения из свойств:
    ISettingID_Dlgs_APW_ReplaceDate    = 3054; // Переписывать дату, если она уже указана
    ISettingID_Dlgs_APW_AutofillTime   = 3055; // Автоматически заполнять время изображения из свойств:
    ISettingID_Dlgs_APW_ReplaceTime    = 3056; // Переписывать время, если оно уже указано
  ISettingID_Dlgs_PicProps             = 0;    // Окно свойств изображений
    ISettingID_Dlgs_PP_ExpFileProps    = 3101; // Сразу раскрывать свойства файлов
    ISettingID_Dlgs_PP_ExpMetadata     = 3102; // Сразу раскрывать метаданные изображений
  ISettingID_Dlgs_FileOpsWizard        = 0;    // Мастер операций с файлами изобраежний
    ISettingID_Dlgs_FOW_CfmCopyFiles   = 3120; // Подтверждение для операции копирования файлов
    ISettingID_Dlgs_FOW_CfmMoveFiles   = 3121; // Подтверждение для операции перемещения файлов
    ISettingID_Dlgs_FOW_CfmDelFiles    = 3122; // Подтверждение для операции удаления файлов
    ISettingID_Dlgs_FOW_CfmRebuildTh   = 3123; // Подтверждение для операции перестройки эскизов
    ISettingID_Dlgs_FOW_CfmRepairFLs   = 3124; // Подтверждение для операции восстановления ссылок на файлы
    ISettingID_Dlgs_FOW_LogOnErrOnly   = 3130; // Отображать протокол только при наличии ошибок

   // Соответствие между операциями с файлами изображений и опциями на их подтверждения
  aFileOpConfirmSettingIDs: Array[TFileOperationKind] of Integer = (
    ISettingID_Dlgs_FOW_CfmCopyFiles,
    ISettingID_Dlgs_FOW_CfmMoveFiles,
    ISettingID_Dlgs_FOW_CfmDelFiles,
    ISettingID_Dlgs_FOW_CfmRebuildTh,
    ISettingID_Dlgs_FOW_CfmRepairFLs);

   // Сообщение о необходимости обновить статус страницы
  WM_PAGEUPDATE                 = WM_USER+$1100;

   // PhoA picture clipboard format name
  SClipbrdPicFormatName         = 'PHOA_INT_PICTURE_BUCKET';

const
  aCheckStates: Array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);

   // Таблицы перекодировки
   // TPicProperty <-> Chunked sorting prop
  aXlat_PicPropertyToChunkSortingProp: Array[TPicProperty] of Word = (
     0,  // ppID
     1,  // ppFileName
     2,  // ppFullFileName
     3,  // ppFilePath
     4,  // ppFileSize
     5,  // ppFileSizeBytes
     6,  // ppPicWidth
     7,  // ppPicHeight
     8,  // ppPicDims
     9,  // ppFormat
    10,  // ppDate
    11,  // ppTime
    12,  // ppPlace
    13,  // ppFilmNumber
    14,  // ppFrameNumber
    15,  // ppAuthor
    16,  // ppDescription
    17,  // ppNotes
    18,  // ppMedia
    19); // ppKeywords
  aXlat_ChunkSortingPropToPicProperty: Array[0..19] of TPicProperty = (
    ppID,            //  0  ID
    ppFileName,      //  1  Picture filename
    ppFullFileName,  //  2  Picture filename with path
    ppFilePath,      //  3  Picture file path
    ppFileSize,      //  4  Picture file size
    ppFileSizeBytes, //  5  Picture file size in bytes
    ppPicWidth,      //  6  Image width
    ppPicHeight,     //  7  Image height
    ppPicDims,       //  8  Image dimensions
    ppFormat,        //  9  Pixel format
    ppDate,          // 10  Date
    ppTime,          // 11  Time
    ppPlace,         // 12  Place
    ppFilmNumber,    // 13  Film number
    ppFrameNumber,   // 14  Frame number
    ppAuthor,        // 15  Author
    ppDescription,   // 16  Description
    ppNotes,         // 17  Notes
    ppMedia,         // 18  Media
    ppKeywords);     // 19  Keywords (keywords are alw

   // TGroupByProperty <-> Chunked sorting prop
  aXlat_GBPropertyToChunkGroupingProp: Array[TGroupByProperty] of Word = (
     0,  // gbpFilePath
     1,  // gbpDateByYear
     2,  // gbpDateByMonth
     3,  // gbpDateByDay
     4,  // gbpTimeHour
     5,  // gbpTimeMinute
     6,  // gbpPlace
     7,  // gbpFilmNumber
     8,  // gbpAuthor
     9,  // gbpMedia
    10); // gbpKeywords
  aXlat_ChunkGroupingPropToGBProperty: Array[0..10] of TGroupByProperty = (
   gbpFilePath,    //  0  Picture file path
   gbpDateByYear,  //  1  Date year
   gbpDateByMonth, //  2  Date month
   gbpDateByDay,   //  3  Date day
   gbpTimeHour,    //  4  Time hour
   gbpTimeMinute,  //  5  Time minute
   gbpPlace,       //  6  Place
   gbpFilmNumber,  //  7  Film number
   gbpAuthor,      //  8  Author
   gbpMedia,       //  9  Media name/code
   gbpKeywords);   // 10  Keywords

  B_LowGBP  = Byte(Low(TGroupByProperty));
  B_HighGBP = Byte(High(TGroupByProperty));

   // TGroupByProperty версии 2 <-> Byte
  aXlat_GBPropToGBProp2: Array[B_LowGBP..B_HighGBP] of Byte = (
    0,   // gbpFilePath
    1,   // gbpDateByYear
    2,   // gbpDateByMonth
    3,   // gbpDateByDay
    255, // gbpTimeHour
    255, // gbpTimeMinute
    4,   // gbpPlace
    5,   // gbpFilmNumber
    255, // gbpAuthor
    255, // gbpMedia
    6);  // gbpKeywords
  aXlat_GBProp2ToGBProp: Array[0..6] of Byte = (
   Byte(gbpFilePath),      // 0  Picture file path
   Byte(gbpDateByYear),    // 1  Date year
   Byte(gbpDateByMonth),   // 2  Date month
   Byte(gbpDateByDay),     // 3  Date day
   Byte(gbpPlace),         // 4  Place
   Byte(gbpFilmNumber),    // 5  Film number
   Byte(gbpKeywords));     // 6  Keywords

var
  cMainCodePage: Cardinal;    // Кодовая страница ANSI для текущего основного шрифта программы
  wClipbrdPicFormatID: Word;  // PhoA picture clipboard format identifier
   // Misc settings
  ViewInfoPos: TRect;         // Границы информации при просмотре в 10-тысячных долях размера экрана [90, 9400, 9910, 9880]

   //-------------------------------------------------------------------------------------------------------------------
   // Settings
   //-------------------------------------------------------------------------------------------------------------------
type
   // Тип данных (и контрола) пункта-установки
  TSettingDatatype = (
    sdtStatic,   // Нет данных
    sdtBool,     // Переключатель (CheckBox)
    sdtParMsk,   // Переключатель (CheckBox), представляющий собой бит в маске родителя
    sdtMutex,    // Взаимно исключающий выбор (RadioButton), индекс которого представляет собой значение родителя
    sdtMutexInt, // Взаимно исключающий выбор (RadioButton), значение которого представляет собой значение родителя
    sdtColor,    // Цвет
    sdtInt,      // Целое число
    sdtFont);    // Шрифт

type
   // Пункт настройки
  PPhoaSetting = ^TPhoaSetting;
  TPhoaSetting = class(TObject)
  private
     // Список дочерних пунктов
    FChildren: TList;
     // Данные (typecasted). Если Datatype=sdtFont, то ссылка на строку
    FData: Integer;
     // Prop storage
    FOwner: TPhoaSetting;
    FImageIndex: Integer;
    FDatatype: TSettingDatatype;
    FName: String;
    FMaxValue: Integer;
    FMinValue: Integer;
    FID: Integer;
    FHelpContext: THelpContext;
     // Добавление / удаление пункта из списка дочерних пунктов
    procedure AddSetting(Item: TPhoaSetting);
    procedure RemoveSetting(Item: TPhoaSetting);
    procedure DeleteSetting(Index: Integer);
     // Очищает список дочерних пунктов
    procedure ClearSettings;
     // Вызывает Exception неверных данных
    procedure InvalidDatatype;
     // Внутренняя процедура копирования все установок (не копирует данные, передаваемые в конструкторе и не стирает детей)
    procedure InternalAssign(Source: TPhoaSetting);
     // Ищет пункт по ID среди себя и своих детей. Если не найден, возвращает nil
    function  FindID(iID: Integer): TPhoaSetting;
     // Возвращает значение в виде строки
    function  GetValStr: String;
     // Prop handlers
    function  GetValueBool: Boolean;
    function  GetValueInt: Integer;
    function  GetValueStr: String;
    procedure SetValueBool(Value: Boolean);
    procedure SetValueInt(Value: Integer);
    procedure SetValueStr(const Value: String);
    function  GetChildCount: Integer;
    function  GetChildren(Index: Integer): TPhoaSetting;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    function  GetSettings(iID: Integer): TPhoaSetting;
  public
    constructor Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName: String); overload;
    constructor Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName: String; bValue: Boolean); overload;
    constructor Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName: String; iValue: Integer); overload;
    constructor Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName, sValue: String); overload;
    destructor Destroy; override;
     // Загрузка/сохранение в реестре значений с ID<>0
    procedure RegLoad(RegIniFile: TRegIniFile);
    procedure RegSave(RegIniFile: TRegIniFile);
     // Загрузка/сохранение в Ini-файле значений с ID<>0
    procedure IniLoad(IniFile: TIniFile);
    procedure IniSave(IniFile: TIniFile);
     // Копирует все установки (включая структуру дочерних узлов) с узла Source
    procedure Assign(Source: TPhoaSetting);
     // Props
     // -- Количество дочерних пунктов
    property ChildCount: Integer read GetChildCount;
     // -- Дочерние пункты по индексу
    property Children[Index: Integer]: TPhoaSetting read GetChildren; default;
     // -- Тип данных пункта
    property Datatype: TSettingDatatype read FDatatype;
     // -- HelpContext ID пункта
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
     // -- ID пункта
    property ID: Integer read FID;
     // -- ImageIndex пункта
    property ImageIndex: Integer read FImageIndex write FImageIndex;
     // -- Максимальное и минимальное значения пунктов (только при Datatype=sdtInt)
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property MinValue: Integer read FMinValue write SetMinValue;
     // -- Наименование пункта. Если начинается с '@', то это имя константы из диалога Settings, если с '#' - то из fMain
    property Name: String read FName;
     // -- Пункт-владелец данного пункта
    property Owner: TPhoaSetting read FOwner;
     // -- Пункты по ID
    property Settings[iID: Integer]: TPhoaSetting read GetSettings; 
     // -- Типизированные значения пункта
    property ValueBool: Boolean read GetValueBool write SetValueBool;
    property ValueInt:  Integer read GetValueInt  write SetValueInt;
    property ValueStr:  String  read GetValueStr  write SetValueStr;
  end;

var
   // Корневой пункт установки
  RootSetting: TPhoaSetting;

   // Применяет пользовательские настройки к TVirtualStringTree
  procedure ApplyTreeSettings(Tree: TVirtualStringTree);
   // Применяет пользовательские настройки к докам/панелям инструментов 
  procedure ApplyToolbarSettings(Dock: TTBXDock);

   // Составляет фильтр для диалога сохранения файла фотоальбома на основе массива ревизий
  function  GetPhoaSaveFilter: String;
   // Возвращает индекс в aFileRevisions[], соответствующий указанной ревизии, или -1, если такой нет
  function  GetIndexOfRevision(iRev: Integer): Integer;
   // Возвращает переданный индекс ревизии, если он в допустимом диапазоне; иначе возвращает 0 (индекс самой свежей ревизии) 
  function  ValidRevisionIndex(idxRev: Integer): Integer;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses Forms, TypInfo, phUtils, phPhoa;

  procedure AddPicPropSettings(Owner: TPhoaSetting);
  var Prop: TPicProperty;
  begin
    for Prop := Low(Prop) to High(Prop) do TPhoaSetting.Create(Owner, sdtParMsk, 0, '#'+GetEnumName(TypeInfo(TPicProperty), Byte(Prop)));
  end;

  procedure AddPicPropMutexSettings(Owner: TPhoaSetting);
  var Prop: TPicProperty;
  begin
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, '#SNone', -1);
    for Prop := Low(Prop) to High(Prop) do TPhoaSetting.Create(Owner, sdtMutexInt, 0, '#'+GetEnumName(TypeInfo(TPicProperty), Byte(Prop)), Byte(Prop));
  end;

  procedure AddPicClipboardFormatSettings(Owner: TPhoaSetting);
  var pcf: TPicClipboardFormat;
  begin
    for pcf := Low(pcf) to High(pcf) do TPhoaSetting.Create(Owner, sdtParMsk, 0, '#'+GetEnumName(TypeInfo(TPicClipboardFormat), Byte(pcf)));
  end;

  procedure AddStretchFilterSettings(Owner: TPhoaSetting);
  begin
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Nearest Neighbor',   Byte(sfNearest));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Linear',             Byte(sfLinear));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'B-spline (bicubic)', Byte(sfSpline));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Lanczos',            Byte(sfLanczos));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Mitchell',           Byte(sfMitchell));
  end;

  procedure AddMagnificationSettings(Owner: TPhoaSetting);
  var b: Byte;
  begin
    for b := 0 to High(adMagnifications) do TPhoaSetting.Create(Owner, sdtMutex, 0, IntToStr(Trunc((adMagnifications[b]-1)*100))+'%');
  end;

  procedure AddDateTimeAutofillPropSettings(Owner: TPhoaSetting);
  var Prop: TDateTimeAutofillProp;
  begin
    for Prop := Low(Prop) to High(Prop) do TPhoaSetting.Create(Owner, sdtParMsk, 0, '#'+GetEnumName(TypeInfo(TDateTimeAutofillProp), Byte(Prop)));
  end;


  procedure ApplyTreeSettings(Tree: TVirtualStringTree);
  begin
     // Apply options
    with Tree.TreeOptions do begin
       // -- Animation
      if RootSetting.Settings[ISettingID_Gen_TreeAnimation].ValueBool then
        AnimationOptions := AnimationOptions+[toAnimatedToggle]
      else
        AnimationOptions := AnimationOptions-[toAnimatedToggle];
       // -- Wheel panning
      if RootSetting.Settings[ISettingID_Gen_TreeWhPanning].ValueBool then
        MiscOptions := MiscOptions+[toWheelPanning]
      else
        MiscOptions := MiscOptions-[toWheelPanning];
       // -- Center selection
      if RootSetting.Settings[ISettingID_Gen_TreeCenterSel].ValueBool then
        SelectionOptions := SelectionOptions+[toCenterScrollIntoView]
      else
        SelectionOptions := SelectionOptions-[toCenterScrollIntoView];
    end;
    with Tree do begin
       // -- Incremental search
      if RootSetting.Settings[ISettingID_Gen_TreeIncrSearch].ValueBool then begin
        IncrementalSearch        := isVisibleOnly;
        IncrementalSearchTimeout := RootSetting.Settings[ISettingID_Gen_TreeIncrSrchDelay].ValueInt;
      end else
        IncrementalSearch := isNone;
       // -- Button style
      if RootSetting.Settings[ISettingID_Gen_TreeButtonStyle].ValueInt=0 then ButtonStyle := bsRectangle else ButtonStyle := bsTriangle;
       // -- Check style
      case RootSetting.Settings[ISettingID_Gen_TreeCheckStyle].ValueInt of
        0: CheckImageKind := ckDarkCheck;
        1: CheckImageKind := ckDarkTick;
        2: CheckImageKind := ckFlat;
        3: CheckImageKind := ckLightCheck;
        4: CheckImageKind := ckLightTick;
        5: CheckImageKind := ckSystem;
        6: CheckImageKind := ckSystemFlat;
        7: CheckImageKind := ckXP;
      end;
       // -- Multiselection style
      if RootSetting.Settings[ISettingID_Gen_TreeSelStyle].ValueInt=0 then
        DrawSelectionMode := smDottedRectangle
      else
        DrawSelectionMode := smBlendedRectangle;
    end;
  end;

  procedure ApplyToolbarSettings(Dock: TTBXDock);
  var i, iMode: Integer;
  begin
    iMode := RootSetting.Settings[ISettingID_Gen_ToolbarDragStyle].ValueInt;
     // Настраиваем сам док
    Dock.AllowDrag := iMode>0;
     // Настраиваем панели в доке
    if iMode>0 then
      for i := 0 to Dock.ToolbarCount-1 do
        with Dock.Toolbars[i] as TTBXToolbar do
          case iMode of
            1: DockMode := dmCannotFloatOrChangeDocks;
            2: DockMode := dmCannotFloat;
            3: DockMode := dmCanFloat;
          end;
  end;

  function GetPhoaSaveFilter: String;
  var i: Integer;
  begin
    Result := '';
    for i := 0 to High(aPhFileRevisions) do AccumulateStr(Result, '|', Format('%s photo album|*.%s', [aPhFileRevisions[i].sName, SDefaultExt]));
  end;

  function GetIndexOfRevision(iRev: Integer): Integer;
  begin
    for Result := 0 to High(aPhFileRevisions) do
      if aPhFileRevisions[Result].iNumber=iRev then Exit;
    Result := -1;
  end;

  function ValidRevisionIndex(idxRev: Integer): Integer;
  begin
    Result := iif((idxRev>=0) and (idxRev<=High(aPhFileRevisions)), idxRev, 0);
  end;

   //===================================================================================================================
   // TPhoaSetting
   //===================================================================================================================

  procedure TPhoaSetting.AddSetting(Item: TPhoaSetting);
  begin
    if FChildren=nil then FChildren := TList.Create;
    FChildren.Add(Item);
  end;

  procedure TPhoaSetting.Assign(Source: TPhoaSetting);
  begin
     // Стираем детей
    ClearSettings;
     // Копируем настройки, передаваемые в конструкторе
    FDatatype := Source.FDatatype;
    FID       := Source.FID;
    FName     := Source.FName;
     // Копируем всё остальное
    InternalAssign(Source);
  end;

  procedure TPhoaSetting.ClearSettings;
  begin
    if FChildren<>nil then begin
      while FChildren.Count>0 do DeleteSetting(FChildren.Count-1);
      FreeAndNil(FChildren);
    end;
  end;

  constructor TPhoaSetting.Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName: String);
  begin
    inherited Create;
    FOwner := AOwner;
    if FOwner<>nil then FOwner.AddSetting(Self);
    FDatatype   := ADatatype;
    FID         := iID;
    FImageIndex := -1;
    FName       := sName;
  end;

  constructor TPhoaSetting.Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName: String; bValue: Boolean);
  begin
    Create(AOwner, ADatatype, iID, sName);
    ValueBool := bValue;
  end;

  constructor TPhoaSetting.Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName: String; iValue: Integer);
  begin
    Create(AOwner, ADatatype, iID, sName);
    ValueInt := iValue;
  end;

  constructor TPhoaSetting.Create(AOwner: TPhoaSetting; ADatatype: TSettingDatatype; iID: Integer; const sName, sValue: String);
  begin
    Create(AOwner, ADatatype, iID, sName);
    ValueStr := sValue;
  end;

  procedure TPhoaSetting.DeleteSetting(Index: Integer);
  begin
    TPhoaSetting(FChildren[Index]).Free;
  end;

  destructor TPhoaSetting.Destroy;
  begin
     // Стираем и уничтожаем список дочерних пунктов
    ClearSettings;
    if FOwner<>nil then FOwner.RemoveSetting(Self);
    if FDatatype=sdtFont then Finalize(String(FData));
    inherited Destroy;
  end;

  function TPhoaSetting.FindID(iID: Integer): TPhoaSetting;
  var i: Integer;
  begin
    if iID=FID then begin
      Result := Self;
      Exit;
    end else if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do begin
        Result := GetChildren(i).FindID(iID);
        if Result<>nil then Exit;
      end;
    Result := nil;
  end;

  function TPhoaSetting.GetChildCount: Integer;
  begin
    if FChildren=nil then Result := 0 else Result := FChildren.Count;
  end;

  function TPhoaSetting.GetChildren(Index: Integer): TPhoaSetting;
  begin
    Result := TPhoaSetting(FChildren[Index]);
  end;

  function TPhoaSetting.GetSettings(iID: Integer): TPhoaSetting;
  begin
    Result := FindID(iID);
    if Result=nil then raise Exception.CreateFmt('Invalid setting ID (%d)', [iID]);
  end;

  function TPhoaSetting.GetValStr: String;
  begin
    if FDatatype=sdtFont then Result := GetValueStr else Result := IntToStr(FData);
  end;

  function TPhoaSetting.GetValueBool: Boolean;
  begin
    if not (FDatatype in [sdtBool]) then InvalidDatatype;
    Result := FData<>0;
  end;

  function TPhoaSetting.GetValueInt: Integer;
  begin
    if not (FDatatype in [sdtStatic, sdtMutexInt, sdtColor, sdtInt]) then InvalidDatatype;
    Result := FData;
  end;

  function TPhoaSetting.GetValueStr: String;
  begin
    if FDatatype<>sdtFont then InvalidDatatype;
    Result := String(FData);
  end;

  procedure TPhoaSetting.IniLoad(IniFile: TIniFile);
  var
    s: String;
    i: Integer;
  begin
     // Получаем своё значение
    if FID<>0 then begin
      s := IniFile.ReadString(SRegPrefs, FName, GetValStr);
      if FDatatype=sdtFont then SetValueStr(s) else FData := StrToIntDef(s, FData);
    end;
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).IniLoad(IniFile);
  end;

  procedure TPhoaSetting.IniSave(IniFile: TIniFile);
  var i: Integer;
  begin
     // Сохраняем своё значение
    if FID<>0 then IniFile.WriteString(SRegPrefs, FName, GetValStr);
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).IniSave(IniFile);
  end;

  procedure TPhoaSetting.InternalAssign(Source: TPhoaSetting);
  var
    i: Integer;
    Child, SrcChild: TPhoaSetting;
  begin
     // Копируем настройки
    FHelpContext := Source.FHelpContext;
    FImageIndex  := Source.FImageIndex;
    FMaxValue    := Source.FMaxValue;
    FMinValue    := Source.FMinValue;
    if FDatatype=sdtFont then SetValueStr(Source.GetValueStr) else FData := Source.FData;
     // Повторяем то же для детей
    for i := 0 to Source.ChildCount-1 do begin
      SrcChild := Source.Children[i];
      Child := TPhoaSetting.Create(Self, SrcChild.Datatype, SrcChild.ID, SrcChild.Name);
      Child.InternalAssign(SrcChild);
    end;
  end;

  procedure TPhoaSetting.InvalidDatatype;
  begin
    raise Exception.Create('Invalid access to setting datatype');
  end;

  procedure TPhoaSetting.RegLoad(RegIniFile: TRegIniFile);
  var
    s: String;
    i: Integer;
  begin
     // Получаем своё значение
    if FID<>0 then begin
      s := RegIniFile.ReadString(SRegPrefs, FName, GetValStr);
      if FDatatype=sdtFont then SetValueStr(s) else FData := StrToIntDef(s, FData);
    end;
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).RegLoad(RegIniFile);
  end;

  procedure TPhoaSetting.RegSave(RegIniFile: TRegIniFile);
  var i: Integer;
  begin
     // Сохраняем своё значение
    if FID<>0 then RegIniFile.WriteString(SRegPrefs, FName, GetValStr);
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).RegSave(RegIniFile);
  end;

  procedure TPhoaSetting.RemoveSetting(Item: TPhoaSetting);
  begin
    FChildren.Remove(Item);
  end;

  procedure TPhoaSetting.SetMaxValue(const Value: Integer);
  begin
    if FDatatype<>sdtInt then InvalidDatatype;
    FMaxValue := Value;
  end;

  procedure TPhoaSetting.SetMinValue(const Value: Integer);
  begin
    if FDatatype<>sdtInt then InvalidDatatype;
    FMinValue := Value;
  end;

  procedure TPhoaSetting.SetValueBool(Value: Boolean);
  begin
    if FDatatype<>sdtBool then InvalidDatatype;
    FData := Byte(Value);
  end;

  procedure TPhoaSetting.SetValueInt(Value: Integer);
  begin
    if not (FDatatype in [sdtStatic, sdtMutexInt, sdtColor, sdtInt]) then InvalidDatatype;
     // Validate value
    if (FDatatype=sdtInt) and (FMaxValue>FMinValue) then
      if Value<FMinValue then Value := FMinValue
      else if Value>FMaxValue then Value := FMaxValue; 
    FData := Value;
  end;

  procedure TPhoaSetting.SetValueStr(const Value: String);
  begin
    if FDatatype<>sdtFont then InvalidDatatype;
    String(FData) := Value;
  end;

  {$HINTS OFF} // Do not hint abount var not used
  procedure InitSettings;
  var Lvl0, Lvl1, Lvl2, Lvl3, Lvl4: TPhoaSetting;
  begin
    Lvl0 := RootSetting;
     //=================================================================================================================
    Lvl1 := TPhoaSetting.Create(Lvl0, sdtStatic, ISettingID_Gen,                   '@ISettingID_Gen');
    Lvl1.ImageIndex  := iiA_Props;
    Lvl1.HelpContext := IDH_setup_general;
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Gen_Intf,              '@ISettingID_Gen_Intf');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_Language,          '@ISettingID_Gen_Language', $409 {=1033, English-US});
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtFont,   ISettingID_Gen_MainFont,          '@ISettingID_Gen_MainFont', 'Tahoma/8/0/0/1');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,    ISettingID_Gen_TooltipDisplTime,  '@ISettingID_Gen_TooltipDisplTime', 5000);
        Lvl3.MinValue := 100;
        Lvl3.MaxValue := MaxInt;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,    ISettingID_Gen_OpenMRUCount,      '@ISettingID_Gen_OpenMRUCount', 10);
        Lvl3.MinValue := 0;
        Lvl3.MaxValue := 15;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Gen_LookupPhoaIni,     '@ISettingID_Gen_LookupPhoaIni', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Gen_Clipboard,         '@ISettingID_Gen_Clipboard');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_ClipFormats,       '@ISettingID_Gen_ClipFormats', Byte(DefaultPicClipboardFormats));
        AddPicClipboardFormatSettings(lvl3);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Gen_Toolbars,          '@ISettingID_Gen_Toolbars');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_ToolbarBtnSize,    '@ISettingID_Gen_ToolbarBtnSize', 0);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarBSz16,      '@ISettingID_Gen_ToolbarBSz16');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarBSz24,      '@ISettingID_Gen_ToolbarBSz24');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarBSz32,      '@ISettingID_Gen_ToolbarBSz32');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_ToolbarDragStyle,  '@ISettingID_Gen_ToolbarDragStyle', 3);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarDrgNone,    '@ISettingID_Gen_ToolbarDrgNone');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarDrgOneDock, '@ISettingID_Gen_ToolbarDrgOneDock');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarDrgNoFloat, '@ISettingID_Gen_ToolbarDrgNoFloat');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_ToolbarDrgFree,    '@ISettingID_Gen_ToolbarDrgFree');
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Gen_Tree,              '@ISettingID_Gen_Tree');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Gen_TreeAnimation,     '@ISettingID_Gen_TreeAnimation',  True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Gen_TreeWhPanning,     '@ISettingID_Gen_TreeWhPanning',  True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Gen_TreeCenterSel,     '@ISettingID_Gen_TreeCenterSel',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Gen_TreeIncrSearch,    '@ISettingID_Gen_TreeIncrSearch', True);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtInt,    ISettingID_Gen_TreeIncrSrchDelay, '@ISettingID_Gen_TreeIncrSrchDelay', 1000);
          Lvl4.MinValue := 100;
          Lvl4.MaxValue := 10000;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_TreeButtonStyle,   '@ISettingID_Gen_TreeButtonStyle',   0);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeBS_Rectangle,  '@ISettingID_Gen_TreeBS_Rectangle');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeBS_Triangle,   '@ISettingID_Gen_TreeBS_Triangle');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_TreeCheckStyle,    '@ISettingID_Gen_TreeCheckStyle', 7 {XP});
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_DarkCheck,  '@ISettingID_Gen_TreeCS_DarkCheck');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_DarkTick,   '@ISettingID_Gen_TreeCS_DarkTick');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_Flat,       '@ISettingID_Gen_TreeCS_Flat');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_LightCheck, '@ISettingID_Gen_TreeCS_LightCheck');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_LightTick,  '@ISettingID_Gen_TreeCS_LightTick');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_Std,        '@ISettingID_Gen_TreeCS_Std');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_StdFlat,    '@ISettingID_Gen_TreeCS_StdFlat');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeCS_XP,         '@ISettingID_Gen_TreeCS_XP');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Gen_TreeSelStyle,      '@ISettingID_Gen_TreeSelStyle', 1);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeSelDotted,     '@ISettingID_Gen_TreeSelDotted');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,  ISettingID_Gen_TreeSelBlended,    '@ISettingID_Gen_TreeSelBlended');
     //=================================================================================================================
    Lvl1 := TPhoaSetting.Create(Lvl0, sdtStatic, ISettingID_Browse,                '@ISettingID_Browse');
    Lvl1.ImageIndex  := iiA_NewGroup;
    Lvl1.HelpContext := IDH_setup_browse_mode;
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Browse_Viewer,         '@ISettingID_Browse_Viewer');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtColor,  ISettingID_Browse_ViewerBkColor,  '@ISettingID_Browse_ViewerBkColor', $d7d7d7);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Browse_ViewerDragDrop, '@ISettingID_Browse_ViewerDragDrop', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Browse_ViewerTooltips, '@ISettingID_Browse_ViewerTooltips', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Browse_ViewerTipProps, '@ISettingID_Browse_ViewerTipProps');
        Lvl3.ValueInt := PicPropsToInt([ppFileName, ppFileSize, ppPicDims, ppDate, ppTime, ppPlace, ppFilmNumber, ppFrameNumber, ppAuthor, ppDescription, ppMedia]);
        AddPicPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Browse_ViewerThInfo,   '@ISettingID_Browse_ViewerThInfo');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtStatic, ISettingID_Browse_ViewerThLTProp, '@ISettingID_Browse_ViewerThLTProp', -1);
          AddPicPropMutexSettings(Lvl4);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtStatic, ISettingID_Browse_ViewerThRTProp, '@ISettingID_Browse_ViewerThRTProp', -1);
          AddPicPropMutexSettings(Lvl4);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtStatic, ISettingID_Browse_ViewerThLBProp, '@ISettingID_Browse_ViewerThLBProp', Byte(ppPlace));
          AddPicPropMutexSettings(Lvl4);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtStatic, ISettingID_Browse_ViewerThRBProp, '@ISettingID_Browse_ViewerThRBProp', Byte(ppDate));
          AddPicPropMutexSettings(Lvl4);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Browse_ViewerThBorder, '@ISettingID_Browse_ViewerThBorder', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Browse_ViewerCacheThs, '@ISettingID_Browse_ViewerCacheThs', True);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtInt,    ISettingID_Browse_ViewerCacheSze, '@ISettingID_Browse_ViewerCacheSze', 1000);
          Lvl4.MinValue := 1;
          Lvl4.MaxValue := MaxInt;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Browse_ViewerStchFilt, '@ISettingID_Browse_ViewerStchFilt', Byte(sfNearest));
        AddStretchFilterSettings(Lvl3);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtInt,    ISettingID_Browse_MaxUndoCount,   '@ISettingID_Browse_MaxUndoCount', 32);
      Lvl2.MinValue := 1;
      Lvl2.MaxValue := MaxInt;
     //=================================================================================================================
    Lvl1 := TPhoaSetting.Create(Lvl0, sdtStatic, ISettingID_View,                  '@ISettingID_View');
    Lvl1.ImageIndex  := iiA_ViewMode;
    Lvl1.HelpContext := IDH_setup_view_mode;
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,   ISettingID_View_AlwaysOnTop,      '@ISettingID_View_AlwaysOnTop', False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,   ISettingID_View_Fullscreen,       '@ISettingID_View_Fullscreen', False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,   ISettingID_View_KeepCursorOverTB, '@ISettingID_View_KeepCursorOverTB', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,   ISettingID_View_HideCursor,       '@ISettingID_View_HideCursor', False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtColor,  ISettingID_View_BkColor,          '@ISettingID_View_BkColor', $000000);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,   ISettingID_View_ShowToolbar,      '@ISettingID_View_ShowToolbar', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_View_PicChange,        '@ISettingID_View_PicChange');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_FitWindowToPic,   '@ISettingID_View_FitWindowToPic', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_CenterWindow,     '@ISettingID_View_CenterWindow', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_ShrinkPicToFit,   '@ISettingID_View_ShrinkPicToFit', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_ZoomPicToFit,     '@ISettingID_View_ZoomPicToFit', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_Cyclic,           '@ISettingID_View_Cyclic', False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_View_Optimizing,       '@ISettingID_View_Optimizing');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtBool,   ISettingID_View_Predecode,        '@ISettingID_View_Predecode', True);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtBool,   ISettingID_View_CacheBehind,      '@ISettingID_View_CacheBehind', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_View_ZoomFactor,       '@ISettingID_View_ZoomFactor', 3 {=50%});
      AddMagnificationSettings(Lvl2);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_View_CaptionProps,     '@ISettingID_View_CaptionProps', PicPropsToInt([ppFileName]));
      AddPicPropSettings(Lvl2);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_View_StchFilt,         '@ISettingID_View_StchFilt', Byte(sfNearest));
      AddStretchFilterSettings(Lvl2);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_View_Info,             '@ISettingID_View_Info');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_ShowInfo,         '@ISettingID_View_ShowInfo', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_View_InfoPicProps,     '@ISettingID_View_InfoPicProps', PicPropsToInt([ppDate, ppTime, ppPlace, ppDescription]));
        AddPicPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtFont,   ISettingID_View_InfoFont,         '@ISettingID_View_InfoFont', 'Arial/14/0/16777215/1');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtColor,  ISettingID_View_InfoBkColor,      '@ISettingID_View_InfoBkColor', $000000);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,    ISettingID_View_InfoBkOpacity,    '@ISettingID_View_InfoBkOpacity', $40);
        Lvl3.MinValue := 0;
        Lvl3.MaxValue := 255;
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_View_Slideshow,        '@ISettingID_View_Slideshow');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,    ISettingID_View_SlideInterval,    '@ISettingID_View_SlideInterval', 5000);
        Lvl3.MinValue := 0;
        Lvl3.MaxValue := 600*1000;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_View_SlideCyclic,      '@ISettingID_View_SlideCyclic', True);
     //=================================================================================================================
    Lvl1 := TPhoaSetting.Create(Lvl0, sdtStatic, ISettingID_Dialogs,               '@ISettingID_Dialogs');
    Lvl1.ImageIndex  := iiA_Dialog;
    Lvl1.HelpContext := IDH_setup_dialogs;
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Dlgs_Confms,           '@ISettingID_Dlgs_Confms');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_ConfmDelGroup,    '@ISettingID_Dlgs_ConfmDelGroup',    True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_ConfmDelPics,     '@ISettingID_Dlgs_ConfmDelPics',     True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_ConfmDelView,     '@ISettingID_Dlgs_ConfmDelView',     True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_ConfmOldFile,     '@ISettingID_Dlgs_ConfmOldFile',     True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_ConfmAppExit,     '@ISettingID_Dlgs_ConfmAppExit',     False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Dlgs_Notifies,         '@ISettingID_Dlgs_Notifies');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_NotifyDragCopy,   '@ISettingID_Dlgs_NotifyDragCopy',   True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_NotifyDragMove,   '@ISettingID_Dlgs_NotifyDragMove',   True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_NotifyPaste,      '@ISettingID_Dlgs_NotifyPaste',      True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Dlgs_AddPicWizard,     '@ISettingID_Dlgs_AddPicWizard');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_APW_ShowHidden,   '@ISettingID_Dlgs_APW_ShowHidden',   False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_APW_SkipChkPage,  '@ISettingID_Dlgs_APW_SkipChkPage',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_APW_LogOnErrOnly, '@ISettingID_Dlgs_APW_LogOnErrOnly', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Dlgs_APW_AutofillDate, '@ISettingID_Dlgs_APW_AutofillDate', Byte(DTAP_DefaultDateProps));
        AddDateTimeAutofillPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_APW_ReplaceDate,  '@ISettingID_Dlgs_APW_ReplaceDate',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic, ISettingID_Dlgs_APW_AutofillTime, '@ISettingID_Dlgs_APW_AutofillTime', Byte(DTAP_DefaultTimeProps));
        AddDateTimeAutofillPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_APW_ReplaceTime,  '@ISettingID_Dlgs_APW_ReplaceTime',  False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Dlgs_PicProps,         '@ISettingID_Dlgs_PicProps');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_PP_ExpFileProps,  '@ISettingID_Dlgs_PP_ExpFileProps',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_PP_ExpMetadata ,  '@ISettingID_Dlgs_PP_ExpMetadata',   False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic, ISettingID_Dlgs_FileOpsWizard,    '@ISettingID_Dlgs_FileOpsWizard');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_FOW_CfmCopyFiles, '@ISettingID_Dlgs_FOW_CfmCopyFiles', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_FOW_CfmMoveFiles, '@ISettingID_Dlgs_FOW_CfmMoveFiles', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_FOW_CfmDelFiles,  '@ISettingID_Dlgs_FOW_CfmDelFiles',  True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_FOW_CfmRebuildTh, '@ISettingID_Dlgs_FOW_CfmRebuildTh', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_FOW_CfmRepairFLs, '@ISettingID_Dlgs_FOW_CfmRepairFLs', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,   ISettingID_Dlgs_FOW_LogOnErrOnly, '@ISettingID_Dlgs_FOW_LogOnErrOnly', True);
  end;
  {$HINTS ON}

initialization
   // Грузим курсоры
  with Screen do begin
    Cursors[crHand]          := LoadCursor(HInstance, 'CRHAND');
    Cursors[crHandDrag]      := LoadCursor(HInstance, 'CRHANDDRAG');
    Cursors[crDragMove]      := LoadCursor(HInstance, 'CRDRAGMOVE');
    Cursors[crDragCopy]      := LoadCursor(HInstance, 'CRDRAGCOPY');
  end;
   // Регистрируем формат буфера обмена
  wClipbrdPicFormatID := RegisterClipboardFormat(SClipbrdPicFormatName);
   // Создаём установки
  RootSetting := TPhoaSetting.Create(nil, sdtStatic, 0, '');
  InitSettings;
finalization
  RootSetting.Free;
end.

