//**********************************************************************************************************************
//  $Id: phObj.pas,v 1.57 2004-10-30 07:31:45 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phObj;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Masks, phPhoa, phIntf, phMutableIntf, phNativeIntf;

type
   // Phoa Exception  
  EPhoaException = class(Exception);

   //===================================================================================================================
   // Список Integer'ов
   //===================================================================================================================

  TIntegerList = class(TList)
  private
     // Prop storage
    FAllowDuplicates: Boolean;
     // Prop handlers
    function  GetItems(Index: Integer): Integer;
  public
    constructor Create(bAllowDuplicates: Boolean);
     // Если числа не было в списке или позволены дубликаты, добавляет его и возвращает True, иначе возвращает False
    function  Add(i: Integer): Boolean;
     // Если числа не было в списке или позволены дубликаты, вставляет его и возвращает True, иначе возвращает False
    function  Insert(Index, i: Integer): Boolean;
     // Если число есть в списке, удаляет его и возвращает его прежний индекс, иначе возвращает -1
    function  Remove(i: Integer): Integer;
     // Возвращает индекс числа или -1, если такого нет
    function  IndexOf(i: Integer): Integer;
     // Props
     // -- Если False, перед добавлением проверяет на наличие такого числа, и, если есть, не добавляет
    property AllowDuplicates: Boolean read FAllowDuplicates;
     // -- Числа по индексу
    property Items[Index: Integer]: Integer read GetItems; default;
  end;

   //===================================================================================================================
   // Объект для считывания/записи файла фотоальбома, понимающий все ревизии файлов
   //===================================================================================================================

  TPhoaFilerEx = class(TPhoaFiler)
  protected
    procedure ValidateRevision; override;
  end;

   //===================================================================================================================
   // Информация о файле и список записей файлов
   //===================================================================================================================

   // Запись с данными о файле
  PFileRec = ^TFileRec;
  TFileRec = record
    sName: String;        // Имя файла
    sPath: String;        // Путь к файлу
    iSize: Integer;       // Размер файла в байтах
    iIconIndex: Integer;  // Индекс значка из системного ImageList'а, -1 если нет
    dModified: TDateTime; // Дата/время модификации файла
    bChecked: Boolean;    // True, если файл отмечен (custom value), по умолчанию False
  end;

   // Режим сортировки списка файлов
  TFileListSortProperty = (flspName, flspPath, flspSize, flspDate);

  TFileList = class(TList)
  private
     // Prop storage
    FSysImageListHandle: THandle;
     // Внутренняя процедура сортировки
    procedure InternalQuickSort(iL, iR: Integer; Prop: TFileListSortProperty; Direction: TPhoaSortDirection);
     // Prop handlers
    function GetItems(Index: Integer): PFileRec;
    function GetFiles(Index: Integer): String;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
     // Добавляет запись о файле к списку, если такого файла ещё нет, иначе обновляет его данные
     //   Если _iIconIndex=-1, это означает отсутствие иконки у файла
     //   Если _iIconIndex=-2, иконка файла получается вызовом SHGetFileInfo; Handle системного ImageList'а в этом
     //     случае будет находиться в SysImageListHandle
    function  Add(const _sName, _sPath: String; _iSize, _iIconIndex: Integer; const _dModified: TDateTime): Integer;
     // Удаляет файл с заданными именем и путём, возвращает его прежний индекс или -1, если нет такого в списке
    function  Remove(const _sName, _sPath: String): Integer;
     // Возвращает индекс файла с заданными именем и путём, или -1, если нет такого в списке
    function  IndexOf(const _sName, _sPath: String): Integer;
     // Сортирует список файлов по заданному свойству
    procedure Sort(Prop: TFileListSortProperty; Direction: TPhoaSortDirection);
     // Оставляет в списке только отмеченные (bChecked) файлы
    procedure DeleteUnchecked;
     // Props
     // -- Полные пути к файлам по индексу
    property Files[Index: Integer]: String read GetFiles;
     // -- Элементы списка по индексу
    property Items[Index: Integer]: PFileRec read GetItems; default;
     // -- Handle системного ImageList'а после определения иконки файла в Add()
    property SysImageListHandle: THandle read FSysImageListHandle;
  end;

   //===================================================================================================================
   // Маска для проверки имён файлов, поддерживающая инверсию
   //===================================================================================================================

  TPhoaMask = class(TMask)
  private
    FNegative: Boolean;
  public
    constructor Create(const sMask: String; bNegative: Boolean);
     // Возвращает True, если имя файла [не]соответствует маске
    function Matches(const sFilename: String): Boolean;
  end;

   //===================================================================================================================
   // Список масок (для проверки имён файлов)
   //===================================================================================================================

  TPhoaMasks = class(TObject)
  private
    FMasks: TList;
    function GetEmpty: Boolean;
  public
     // Принимает на вход список масок, разделённых символом ';'
    constructor Create(const sMasks: String);
    destructor Destroy; override;
     // Возвращает True, если имя файла соответствует любой из масок
    function Matches(const sFilename: String): Boolean;
     // Props
     // -- True, если нет реальных масок, и Matches вернёт True в любом случае
    property Empty: Boolean read GetEmpty;
  end;

   //===================================================================================================================
   // Командная строка PhoA
   //===================================================================================================================

  TCmdLineKey = (
    clkOpenPhoa,    // Открыть фотоальбом <значение>
    clkSelectView,  // Перейти в режим отображения представления <значение>
    clkSelectGroup, // Выделить в дереве группу <значение>
    clkSelectPicID, // Отобразить изображение с ID=<значение>
    clkViewMode,    // Перейти в режим просмотра
    clkSlideShow,   // Включить режим слайдшоу после запуска программы
    clkFullScreen); // Полноэкранный режим просмотра (<значение>=1. При <значение>=0 - НЕ полноэкранный режим просмотра)
  TCmdLineKeys = set of TCmdLineKey;

   // Потребность ключа в значении
  TCmdLineKeyValueMode = (
    clkvmNo,        // У ключа не бывает значения
    clkvmOptional,  // У ключа может быть, а может и не быть значения
    clkvmRequired); // У ключа должно быть значение

   // Exception
  EPhoaCommandLineError = class(EPhoaException);

  TPhoaCommandLine = class(TObject)
  private
     // Список ключей/значений (Strings[] - значения, Objects[] - ключи)
    FKeyValues: TStringList;
     // Prop storage
    FKeys: TCmdLineKeys;
     // Разбирает текущую командную строку
    procedure ParseCmdLine;
     // Возвращает индекс значения для ключа в FKeyValues или -1, если нет такого
    function  KeyValIndex(Key: TCmdLineKey): Integer;
     // Prop handlers
    function  GetKeyValues(Key: TCmdLineKey): String;
  public
    constructor Create;
    destructor Destroy; override;
     // Props
     // -- Набор ключей, указанных в командной строке
    property Keys: TCmdLineKeys read FKeys;
     // -- Значения ключей, указанные в командной строке
    property KeyValues[Key: TCmdLineKey]: String read GetKeyValues;
  end;

   // Параметры ключей
const
   aCmdLineKeys: Array[TCmdLineKey] of
     record
       ValueMode: TCmdLineKeyValueMode; // Потребность ключа в значении
       cChar:     Char;                 // Символ (имя) ключа
     end = (
     (ValueMode: clkvmOptional; cChar: #0),   // clkOpenPhoa
     (ValueMode: clkvmRequired; cChar: 'w'),  // clkSelectView
     (ValueMode: clkvmRequired; cChar: 'g'),  // clkSelectGroup
     (ValueMode: clkvmRequired; cChar: 'i'),  // clkSelectPicID
     (ValueMode: clkvmNo;       cChar: 'v'),  // clkViewMode
     (ValueMode: clkvmNo;       cChar: 's'),  // clkSlideShow
     (ValueMode: clkvmOptional; cChar: 'f')); // clkFullScreen

resourcestring
  SPhObjErrMsg_InvalidSortedPicListMethodCall = 'Cannot invoke %s on sorted IPhoaMutablePicList';
  SPhObjErrMsg_PicPropIsReadOnly              = 'Picture property %s cannot be written';
  SPhObjErrMsg_InvalidAddPicID                = 'Invalid picture to add ID (%d)';
  SPhObjErrMsg_InvalidGroupID                 = 'Invalid group ID (%d)';

  SCmdLineErrMsg_UnknownKey                   = 'Unknown command line key: "%s"';
  SCmdLineErrMsg_DuplicateKey                 = 'Duplicate key "%s" in the command line';
  SCmdLineErrMsg_KeyNameInvalid               = 'Key name invalid in the command line ("%s")';
  SCmdLineErrMsg_DuplicateOpenPhoaValue       = 'Duplicate .phoa file to open specified in the command line';
  SCmdLineErrMsg_DuplicateKeyValue            = 'Duplicate value for key "%s" specified in the command line';
  SCmdLineErrMsg_KeyValueMissing              = 'Value for key "%s" is missing in the command line';

   //===================================================================================================================

   // Запись/чтение из потока
  procedure StreamWriteByte(Stream: TStream; b: Byte);
  procedure StreamWriteInt(Stream: TStream; i: Integer);
  procedure StreamWriteDbl(Stream: TStream; const d: Double);
  procedure StreamWriteStr(Stream: TStream; const s: String);
  function  StreamReadByte(Stream: TStream): Byte;
  function  StreamReadInt(Stream: TStream): Integer;
  function  StreamReadDbl(Stream: TStream): Double;
  function  StreamReadStr(Stream: TStream): String;

   // Загружает в TStrings места, номера плёнок и авторов из изображений фотоальбома
  procedure StringsLoadPFAM(PhoA: IPhoaProject; SLPlaces, SLFilmNumbers, SLAuthors, SLMedia: TStrings);

   // Создание новых экземпляров интерфейсов
  function  NewPhotoAlbumPic: IPhotoAlbumPic;
  function  NewPhotoAlbumPicList(bSorted: Boolean): IPhotoAlbumPicList;

  function  NewPhotoAlbumKeywordList: IPhotoAlbumKeywordList;

  function  NewPhotoAlbumPicGroup(AOwner: IPhoaPicGroup; iID: Integer): IPhotoAlbumPicGroup;
  function  NewPhotoAlbumPicGroupList(AOwner: IPhoaPicGroup): IPhotoAlbumPicGroupList;

  function  NewPhotoAlbumPicSorting: IPhotoAlbumPicSorting;
  function  NewPhotoAlbumPicSortingList: IPhotoAlbumPicSortingList;

  function  NewPhotoAlbumPicGrouping: IPhotoAlbumPicGrouping;
  function  NewPhotoAlbumPicGroupingList: IPhotoAlbumPicGroupingList;

  function  NewPhotoAlbumView(AList: IPhotoAlbumViewList): IPhotoAlbumView;
  function  NewPhotoAlbumViewList(APics: IPhoaPicList): IPhotoAlbumViewList;

  function  NewPhotoAlbumProject: IPhotoAlbumProject;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses
  TypInfo, Math, Registry, DateUtils, Clipbrd, ShellAPI, Themes,
  phUtils, phSettings, phGraphics, Variants, phObjConst;

  procedure PhoaWriteError;
  begin
    PhoaException(ConstVal('SErrCannotWrite'), []);
  end;

  procedure PhoaReadError;
  begin
    PhoaException(ConstVal('SErrCannotRead'), []);
  end;

   //===================================================================================================================
   // Работа с потоками
   //===================================================================================================================

  procedure StreamWriteByte(Stream: TStream; b: Byte);
  begin
    if Stream.Write(b, SizeOf(b))<>SizeOf(b) then PhoaWriteError;
  end;

  procedure StreamWriteInt(Stream: TStream; i: Integer);
  begin
    if Stream.Write(i, SizeOf(i))<>SizeOf(i) then PhoaWriteError;
  end;

  procedure StreamWriteDbl(Stream: TStream; const d: Double);
  begin
    if Stream.Write(d, SizeOf(d))<>SizeOf(d) then PhoaWriteError;
  end;

  procedure StreamWriteStr(Stream: TStream; const s: String);
  var i: Integer;
  begin
    i := Length(s);
    StreamWriteInt(Stream, i);
    if (i>0) and (Stream.Write(s[1], i)<>i) then PhoaWriteError;
  end;

  function StreamReadByte(Stream: TStream): Byte;
  begin
    if Stream.Read(Result, SizeOf(Result))<>SizeOf(Result) then PhoaReadError;
  end;

  function StreamReadInt(Stream: TStream): Integer;
  begin
    if Stream.Read(Result, SizeOf(Result))<>SizeOf(Result) then PhoaReadError;
  end;

  function StreamReadDbl(Stream: TStream): Double;
  begin
    if Stream.Read(Result, SizeOf(Result))<>SizeOf(Result) then PhoaReadError;
  end;

  function StreamReadStr(Stream: TStream): String;
  var i: Integer;
  begin
    i := StreamReadInt(Stream);
    SetLength(Result, i);
    if (i>0) and (Stream.Read(Result[1], i)<>i) then PhoaReadError;
  end;

   //===================================================================================================================
   // Misc routines
   //===================================================================================================================

  procedure StringsLoadPFAM(PhoA: IPhoaProject; SLPlaces, SLFilmNumbers, SLAuthors, SLMedia: TStrings);
  var i: Integer;

    procedure AddStr(SL: TStrings; const s: String);
    begin
      if s<>'' then
        if SL.IndexOf(s)<0 then SL.Add(s);
    end;

  begin
    SLPlaces.BeginUpdate;
    SLFilmNumbers.BeginUpdate;
    SLAuthors.BeginUpdate;
    SLMedia.BeginUpdate;
    try
      SLPlaces.Clear;
      SLFilmNumbers.Clear;
      SLAuthors.Clear;
      SLMedia.Clear;
       // Крутим цикл по всем изображениям
      for i := 0 to PhoA.Pics.Count-1 do
        with PhoA.Pics[i] do begin
          AddStr(SLPlaces,      Place);
          AddStr(SLFilmNumbers, FilmNumber);
          AddStr(SLAuthors,     Author);
          AddStr(SLMedia,       Media);
        end;
    finally
      SLPlaces.EndUpdate;
      SLFilmNumbers.EndUpdate;
      SLAuthors.EndUpdate;
      SLMedia.EndUpdate;
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumPic
   //===================================================================================================================
type
  TPhotoAlbumPic = class(TInterfacedObject, IPhoaPic, IPhoaMutablePic, IPhotoAlbumPic)
  private
     // Prop storage
    FAuthor: String;
    FDate: Integer;
    FTime: Integer;
    FDescription: String;
    FFileName: String;
    FFileSize: Integer;
    FFilmNumber: String;
    FFlips: TPicFlips;
    FFrameNumber: String;
    FID: Integer;
    FImageFormat: TPhoaPixelFormat;
    FImageSize: TSize;
    FKeywords: IPhotoAlbumKeywordList;
    FMedia: String;
    FNotes: String;
    FPlace: String;
    FRotation: TPicRotation;
    FThumbnailData: String;
    FThumbnailSize: TSize;
     // IPhoaPic
    function  GetAuthor: String; stdcall;
    function  GetDate: Integer; stdcall;
    function  GetDescription: String; stdcall;
    function  GetFileName: String; stdcall;
    function  GetFileSize: Integer; stdcall;
    function  GetFilmNumber: String; stdcall;
    function  GetFlips: TPicFlips; stdcall;
    function  GetFrameNumber: String; stdcall;
    function  GetID: Integer; stdcall;
    function  GetImageFormat: TPhoaPixelFormat; stdcall;
    function  GetImageSize: TSize; stdcall;
    function  GetKeywords: IPhoaKeywordList; stdcall;
    function  GetMedia: String; stdcall;
    function  GetNotes: String; stdcall;
    function  GetPlace: String; stdcall;
    function  GetPropStrValues(PicProp: TPicProperty): String; stdcall;
    function  GetPropValues(PicProp: TPicProperty): Variant; stdcall;
    function  GetRawData(PProps: TPicProperties): String; stdcall;
    function  GetRotation: TPicRotation; stdcall;
    function  GetThumbnailData: String; stdcall;
    function  GetThumbnailSize: TSize; stdcall;
    function  GetTime: Integer; stdcall;
     // IPhoaMutablePic
    function  GetKeywordsM: IPhoaMutableKeywordList; stdcall;
    procedure Assign(SrcPic: IPhoaPic); stdcall;
    procedure CleanupProps(Props: TPicProperties); stdcall;
    procedure ReloadPicFileData(const AThumbnailSize: TSize; StretchFilter: TPhoaStretchFilter; AThumbnailQuality: Byte); stdcall;
    procedure SetDate(Value: Integer); stdcall;
    procedure SetFileName(const Value: String); stdcall;
    procedure SetFlips(Value: TPicFlips); stdcall;
    procedure SetPropValues(PicProp: TPicProperty; const Value: Variant); stdcall;
    procedure SetRawData(PProps: TPicProperties; const Value: String); stdcall;
    procedure SetRotation(Value: TPicRotation); stdcall;
    procedure SetTime(Value: Integer); stdcall;
     // IPhotoAlbumPic
    function  GetKeywordsX: IPhotoAlbumKeywordList;
    procedure PutToList(List: IPhoaMutablePicList; iNewID: Integer = -1);
    procedure StreamerLoad(Streamer: TPhoaStreamer; bExpandRelative: Boolean; PProps: TPicProperties);
    procedure StreamerSave(Streamer: TPhoaStreamer; bExtractRelative: Boolean; PProps: TPicProperties);
  public
    constructor Create;
  end;

  procedure TPhotoAlbumPic.Assign(SrcPic: IPhoaPic);
  begin
    FAuthor        := SrcPic.Author;
    FDate          := SrcPic.Date;
    FTime          := SrcPic.Time;
    FDescription   := SrcPic.Description;
    FFileName      := SrcPic.FileName;
    FFileSize      := SrcPic.FileSize;
    FFilmNumber    := SrcPic.FilmNumber;
    FFlips         := SrcPic.Flips;
    FFrameNumber   := SrcPic.FrameNumber;
    FID            := SrcPic.ID;
    FImageFormat   := SrcPic.ImageFormat;
    FImageSize     := SrcPic.ImageSize;
    FMedia         := SrcPic.Media;
    FNotes         := SrcPic.Notes;
    FPlace         := SrcPic.Place;
    FRotation      := SrcPic.Rotation;
    FThumbnailData := SrcPic.ThumbnailData;
    FThumbnailSize := SrcPic.ThumbnailSize;
    FKeywords.Assign(SrcPic.Keywords);
  end;

  procedure TPhotoAlbumPic.CleanupProps(Props: TPicProperties);
  begin
    if [ppFileSize, ppFileSizeBytes]*Props<>[] then FFileSize     := 0;
    if [ppPicWidth, ppPicDims]*Props<>[]       then FImageSize.cx := 0;
    if [ppPicHeight, ppPicDims]*Props<>[]      then FImageSize.cy := 0;
    if ppFormat      in Props                  then FImageFormat  := ppfCustom;
    if ppDate        in Props                  then FDate         := 0;
    if ppTime        in Props                  then FTime         := 0;
    if ppPlace       in Props                  then FPlace        := '';
    if ppFilmNumber  in Props                  then FFilmNumber   := '';
    if ppFrameNumber in Props                  then FFrameNumber  := '';
    if ppAuthor      in Props                  then FAuthor       := '';
    if ppMedia       in Props                  then FMedia        := '';
    if ppDescription in Props                  then FDescription  := '';
    if ppNotes       in Props                  then FNotes        := '';
    if ppKeywords    in Props                  then FKeywords.Clear;
    if ppRotation    in Props                  then FRotation     := pr0;
    if ppFlips       in Props                  then FFlips        := [];
  end;

  constructor TPhotoAlbumPic.Create;
  begin
    inherited Create;
    FImageFormat := ppfCustom;
    FKeywords := NewPhotoAlbumKeywordList;
  end;

  function TPhotoAlbumPic.GetAuthor: String;
  begin
    Result := FAuthor;
  end;

  function TPhotoAlbumPic.GetDate: Integer;
  begin
    Result := FDate;
  end;

  function TPhotoAlbumPic.GetDescription: String;
  begin
    Result := FDescription;
  end;

  function TPhotoAlbumPic.GetFileName: String;
  begin
    Result := FFileName;
  end;

  function TPhotoAlbumPic.GetFileSize: Integer;
  begin
    Result := FFileSize;
  end;

  function TPhotoAlbumPic.GetFilmNumber: String;
  begin
    Result := FFilmNumber;
  end;

  function TPhotoAlbumPic.GetFlips: TPicFlips;
  begin
    Result := FFlips;
  end;

  function TPhotoAlbumPic.GetFrameNumber: String;
  begin
    Result := FFrameNumber;
  end;

  function TPhotoAlbumPic.GetID: Integer;
  begin
    Result := FID;
  end;

  function TPhotoAlbumPic.GetImageFormat: TPhoaPixelFormat;
  begin
    Result := FImageFormat;
  end;

  function TPhotoAlbumPic.GetImageSize: TSize;
  begin
    Result := FImageSize;
  end;

  function TPhotoAlbumPic.GetKeywords: IPhoaKeywordList;
  begin
    Result := FKeywords;
  end;

  function TPhotoAlbumPic.GetKeywordsM: IPhoaMutableKeywordList;
  begin
    Result := FKeywords;
  end;

  function TPhotoAlbumPic.GetKeywordsX: IPhotoAlbumKeywordList;
  begin
    Result := FKeywords;
  end;

  function TPhotoAlbumPic.GetMedia: String;
  begin
    Result := FMedia;
  end;

  function TPhotoAlbumPic.GetNotes: String;
  begin
    Result := FNotes;
  end;

  function TPhotoAlbumPic.GetPlace: String;
  begin
    Result := FPlace;
  end;

  function TPhotoAlbumPic.GetPropStrValues(PicProp: TPicProperty): String;
  begin
    Result := '';
    case PicProp of
      ppID:            Result := IntToStr(FID);
      ppFileName:      Result := ExtractFileName(FFileName);
      ppFullFileName:  Result := FFileName;
      ppFilePath:      Result := ExtractFileDir(FFileName);
      ppFileSize:      Result := HumanReadableSize(FFileSize);
      ppFileSizeBytes: Result := IntToStr(FFileSize);
      ppPicWidth:      if FImageSize.cx>0 then Result := IntToStr(FImageSize.cx);
      ppPicHeight:     if FImageSize.cy>0 then Result := IntToStr(FImageSize.cy);
      ppPicDims:       if (FImageSize.cx>0) and (FImageSize.cy>0) then Result := Format('%dx%d', [FImageSize.cx, FImageSize.cy]);
      ppFormat:        Result := PixelFormatName(FImageFormat);
      ppDate:          if FDate>0 then Result := DateToStr (PhoaDateToDate(FDate));
      ppTime:          if FTime>0 then Result := TimeToStrX(PhoaTimeToTime(FTime));
      ppPlace:         Result := FPlace;
      ppFilmNumber:    Result := FFilmNumber;
      ppFrameNumber:   Result := FFrameNumber;
      ppAuthor:        Result := FAuthor;
      ppDescription:   Result := FDescription;
      ppNotes:         Result := FNotes;
      ppMedia:         Result := FMedia;
      ppKeywords:      Result := FKeywords.CommaText;
      ppRotation:      Result := asPicRotationText[FRotation];
      ppFlips:         Result := iif(pflHorz in FFlips, asPicFlipText[pflHorz], '')+iif(pflVert in FFlips, asPicFlipText[pflVert], '');
    end;
  end;

  function TPhotoAlbumPic.GetPropValues(PicProp: TPicProperty): Variant;
  var sVal: String;
  begin
    Result := Null;
    case PicProp of
      ppID:            Result := FID;
      ppFileSizeBytes: Result := FFileSize;
      ppPicWidth:      if FImageSize.cx>0 then Result := FImageSize.cx;
      ppPicHeight:     if FImageSize.cy>0 then Result := FImageSize.cy;
      ppFormat:        Result := Byte(FImageFormat);
      ppDate:          if FDate>0 then Result := FDate;
      ppTime:          if FTime>0 then Result := FTime;
      ppKeywords:      Result := FKeywords;
      ppRotation:      Result := Byte(FRotation);
      ppFlips:         Result := Byte(FFlips);
       // Все прочие свойства берём как строки, только вместо пустой строки возвращаем Null
      else begin
        sVal := GetPropStrValues(PicProp);
        if sVal<>'' then Result := sVal;
      end;
    end;
  end;

  function TPhotoAlbumPic.GetRawData(PProps: TPicProperties): String;
  var
    Stream: TStringStream;
    Streamer: TPhoaStreamer;
  begin
     // Сохраняем данные изображения во временный поток
    Stream := TStringStream.Create('');
    try
      Streamer := TPhoaStreamer.Create(Stream, psmWrite, '');
      try
        StreamerSave(Streamer, False, PProps);
      finally
        Streamer.Free;
      end;
       // Сохраняем поток в строку
      Result := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;

  function TPhotoAlbumPic.GetRotation: TPicRotation;
  begin
    Result := FRotation;
  end;

  function TPhotoAlbumPic.GetThumbnailData: String;
  begin
    Result := FThumbnailData;
  end;

  function TPhotoAlbumPic.GetThumbnailSize: TSize;
  begin
    Result := FThumbnailSize;
  end;

  function TPhotoAlbumPic.GetTime: Integer;
  begin
    Result := FTime;
  end;

  procedure TPhotoAlbumPic.PutToList(List: IPhoaMutablePicList; iNewID: Integer = -1);
  begin
    if iNewID=0      then FID := List.MaxPicID+1
    else if iNewID>0 then FID := iNewID;
    List.Add(Self, False);
  end;

  procedure TPhotoAlbumPic.ReloadPicFileData(const AThumbnailSize: TSize; StretchFilter: TPhoaStretchFilter; AThumbnailQuality: Byte);
  begin
     // Загружаем эскиз и получаем его данные
    FThumbnailData := phGraphics.GetThumbnailData(FFileName, AThumbnailSize, StretchFilter, AThumbnailQuality, FImageSize, FThumbnailSize);
     // Получаем размер файла изображения
    FFileSize := phUtils.GetFileSize(FFileName, 0); 
  end;

  procedure TPhotoAlbumPic.SetDate(Value: Integer);
  begin
    FDate := Value; 
  end;

  procedure TPhotoAlbumPic.SetFileName(const Value: String);
  begin
    FFileName := Value;
  end;

  procedure TPhotoAlbumPic.SetFlips(Value: TPicFlips);
  begin
    FFlips := Value;
  end;

  procedure TPhotoAlbumPic.SetPropValues(PicProp: TPicProperty; const Value: Variant);
  var SrcKeywords: IPhoaKeywordList;
  begin
    case PicProp of
      ppFileName:      FFileName     := ExtractFilePath(FFileName)+Value;
      ppFullFileName:  FFileName     := Value;
      ppFilePath:      FFileName     := IncludeTrailingPathDelimiter(Value)+ExtractFileName(FFileName);
      ppFileSizeBytes: FFileSize     := VarToInt(Value);
      ppPicWidth:      FImageSize.cx := VarToInt(Value);
      ppPicHeight:     FImageSize.cy := VarToInt(Value);
      ppFormat:        FImageFormat  := TPhoaPixelFormat(Byte(Value));
      ppDate:          FDate         := VarToInt(Value);
      ppTime:          FTime         := VarToInt(Value);
      ppPlace:         FPlace        := VarToStr(Value);
      ppFilmNumber:    FFilmNumber   := VarToStr(Value);
      ppFrameNumber:   FFrameNumber  := VarToStr(Value);
      ppAuthor:        FAuthor       := VarToStr(Value);
      ppDescription:   FDescription  := VarToStr(Value);
      ppNotes:         FNotes        := VarToStr(Value);
      ppMedia:         FMedia        := VarToStr(Value);
      ppKeywords:      if VarSupports(Value, IPhoaKeywordList, SrcKeywords) then FKeywords.Assign(SrcKeywords);
      ppRotation:      FRotation     := TPicRotation(Byte(Value));
      ppFlips:         FFlips        := TPicFlips(Byte(Value));
      else             PhoaException(SPhObjErrMsg_PicPropIsReadOnly, [GetEnumName(TypeInfo(TPicProperty), Byte(PicProp))]);
    end;
  end;

  procedure TPhotoAlbumPic.SetRawData(PProps: TPicProperties; const Value: String);
  var
    Stream: TStringStream;
    Streamer: TPhoaStreamer;
  begin
    Stream := TStringStream.Create(Value);
    try
      Streamer := TPhoaStreamer.Create(Stream, psmRead, '');
      try
        StreamerLoad(Streamer, False, PProps);
      finally
        Streamer.Free;
      end;
    finally
      Stream.Free;
    end;
  end;

  procedure TPhotoAlbumPic.SetRotation(Value: TPicRotation);
  begin
    FRotation := Value;
  end;

  procedure TPhotoAlbumPic.SetTime(Value: Integer);
  begin
    FTime := Value;
  end;

  procedure TPhotoAlbumPic.StreamerLoad(Streamer: TPhoaStreamer; bExpandRelative: Boolean; PProps: TPicProperties);
  var
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;

    function XFilename(const s: String): String;
    begin
      if bExpandRelative then Result := ExpandRelativePath(Streamer.BasePath, s) else Result := s;
    end;

  begin
     // *** Old format
    if not Streamer.Chunked then begin
      if ppID          in PProps                  then FID                 := Streamer.ReadInt;
      if ppFileName    in PProps                  then FThumbnailData      := Streamer.ReadStringI;
      if ppFilmNumber  in PProps                  then FFilmNumber         := Streamer.ReadStringI;
      if ppDate        in PProps                  then FDate               := DateToPhoaDate(Streamer.ReadInt);
      if ppDescription in PProps                  then FDescription        := Streamer.ReadStringI;
      if ppFileName    in PProps                  then FFileName           := XFilename(Streamer.ReadStringI);
      if [ppFileSize, ppFileSizeBytes]*PProps<>[] then FFileSize           := Streamer.ReadInt;
      if ppFormat      in PProps                  then FImageFormat        := TPhoaPixelFormat(Streamer.ReadByte);
      if ppKeywords    in PProps                  then FKeywords.CommaText := Streamer.ReadStringI;
      if ppFrameNumber in PProps                  then FFrameNumber        := Streamer.ReadStringI;
      if ppPlace       in PProps                  then FPlace              := Streamer.ReadStringI;
      if ppFileName    in PProps                  then FThumbnailSize.cx   := Streamer.ReadInt;
      if ppFileName    in PProps                  then FThumbnailSize.cy   := Streamer.ReadInt;
     // *** New format
    end else begin
       // Revert props to their defaults because they might be not saved due to their emptiness
      CleanupProps(PProps);
       // Read chunked data
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Picture props
          IPhChunk_Pic_ID:            if ppID          in PProps                  then FID                 := vValue;
          IPhChunk_Pic_ThumbnailData: if ppFileName    in PProps                  then FThumbnailData      := vValue;
          IPhChunk_Pic_ThumbWidth:    if ppFileName    in PProps                  then FThumbnailSize.cx   := vValue;
          IPhChunk_Pic_ThumbHeight:   if ppFileName    in PProps                  then FThumbnailSize.cy   := vValue;
          IPhChunk_Pic_PicFileName:   if ppFileName    in PProps                  then FFileName           := XFilename(vValue);
          IPhChunk_Pic_PicFileSize:   if [ppFileSize, ppFileSizeBytes]*PProps<>[] then FFileSize           := vValue;
          IPhChunk_Pic_PicWidth:      if [ppPicWidth, ppPicDims]*PProps<>[]       then FImageSize.cx       := vValue;
          IPhChunk_Pic_PicHeight:     if [ppPicHeight, ppPicDims]*PProps<>[]      then FImageSize.cy       := vValue;
          IPhChunk_Pic_PicFormat:     if ppFormat      in PProps                  then Byte(FImageFormat)  := vValue;
          IPhChunk_Pic_Date:          if ppDate        in PProps                  then FDate               := vValue;
          IPhChunk_Pic_Time:          if ppTime        in PProps                  then FTime               := vValue;
          IPhChunk_Pic_Place:         if ppPlace       in PProps                  then FPlace              := vValue;
          IPhChunk_Pic_FilmNumber:    if ppFilmNumber  in PProps                  then FFilmNumber         := vValue;
          IPhChunk_Pic_FrameNumber:   if ppFrameNumber in PProps                  then FFrameNumber        := vValue;
          IPhChunk_Pic_Author:        if ppAuthor      in PProps                  then FAuthor             := vValue;
          IPhChunk_Pic_Media:         if ppMedia       in PProps                  then FMedia              := vValue;
          IPhChunk_Pic_Desc:          if ppDescription in PProps                  then FDescription        := vValue;
          IPhChunk_Pic_Notes:         if ppNotes       in PProps                  then FNotes              := vValue;
          IPhChunk_Pic_Keywords:      if ppKeywords    in PProps                  then FKeywords.CommaText := vValue;
          IPhChunk_Pic_Rotation:      if ppRotation    in PProps                  then Byte(FRotation)     := vValue;
          IPhChunk_Pic_Flips:         if ppFlips       in PProps                  then Byte(FFlips)        := vValue;
           // Close-chunk
          IPhChunk_Pic_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
    end;
  end;

  procedure TPhotoAlbumPic.StreamerSave(Streamer: TPhoaStreamer; bExtractRelative: Boolean; PProps: TPicProperties);

    function XFilename: String;
    begin
      if bExtractRelative then Result := ExtractRelativePath(Streamer.BasePath, FFileName) else Result := FFileName;
    end;

  begin
     // *** Old format
    if not Streamer.Chunked then begin
      if ppID          in PProps                  then Streamer.WriteInt    (FID);
      if ppFileName    in PProps                  then Streamer.WriteStringI(FThumbnailData);
      if ppFilmNumber  in PProps                  then Streamer.WriteStringI(FFilmNumber);
      if ppDate        in PProps                  then Streamer.WriteInt    (Trunc(PhoaDateToDate(FDate)));
      if ppDescription in PProps                  then Streamer.WriteStringI(FDescription);
      if ppFileName    in PProps                  then Streamer.WriteStringI(XFilename);
      if [ppFileSize, ppFileSizeBytes]*PProps<>[] then Streamer.WriteInt    (FFileSize);
      if ppFormat      in PProps                  then Streamer.WriteByte   (Byte(FImageFormat));
      if ppKeywords    in PProps                  then Streamer.WriteStringI(FKeywords.CommaText);
      if ppFrameNumber in PProps                  then Streamer.WriteStringI(FFrameNumber);
      if ppPlace       in PProps                  then Streamer.WriteStringI(FPlace);
      if ppFileName    in PProps                  then Streamer.WriteInt    (FThumbnailSize.cx);
      if ppFileName    in PProps                  then Streamer.WriteInt    (FThumbnailSize.cy);
     // *** New format
    end else begin
      if ppID          in PProps                                                  then Streamer.WriteChunkInt   (IPhChunk_Pic_ID,            FID);
      if ppFileName    in PProps                                                  then Streamer.WriteChunkString(IPhChunk_Pic_ThumbnailData, FThumbnailData);
      if ppFileName    in PProps                                                  then Streamer.WriteChunkWord  (IPhChunk_Pic_ThumbWidth,    FThumbnailSize.cx);
      if ppFileName    in PProps                                                  then Streamer.WriteChunkWord  (IPhChunk_Pic_ThumbHeight,   FThumbnailSize.cy);
      if ppFileName    in PProps                                                  then Streamer.WriteChunkString(IPhChunk_Pic_PicFileName,   XFilename);
      if ([ppFileSize, ppFileSizeBytes]*PProps<>[]) and (FFileSize>0)             then Streamer.WriteChunkInt   (IPhChunk_Pic_PicFileSize,   FFileSize);
      if ([ppPicWidth, ppPicDims]*PProps<>[])       and (FImageSize.cx>0)         then Streamer.WriteChunkInt   (IPhChunk_Pic_PicWidth,      FImageSize.cx);
      if ([ppPicHeight, ppPicDims]*PProps<>[])      and (FImageSize.cy>0)         then Streamer.WriteChunkInt   (IPhChunk_Pic_PicHeight,     FImageSize.cy);
      if (ppFormat      in PProps)                  and (FImageFormat<>ppfCustom) then Streamer.WriteChunkByte  (IPhChunk_Pic_PicFormat,     Byte(FImageFormat));
      if (ppDate        in PProps)                  and (FDate>0)                 then Streamer.WriteChunkInt   (IPhChunk_Pic_Date,          FDate);
      if (ppTime        in PProps)                  and (FTime>0)                 then Streamer.WriteChunkInt   (IPhChunk_Pic_Time,          FTime);
      if (ppPlace       in PProps)                  and (FPlace<>'')              then Streamer.WriteChunkString(IPhChunk_Pic_Place,         FPlace);
      if (ppFilmNumber  in PProps)                  and (FFilmNumber<>'')         then Streamer.WriteChunkString(IPhChunk_Pic_FilmNumber,    FFilmNumber);
      if (ppFrameNumber in PProps)                  and (FFrameNumber<>'')        then Streamer.WriteChunkString(IPhChunk_Pic_FrameNumber,   FFrameNumber);
      if (ppAuthor      in PProps)                  and (FAuthor<>'')             then Streamer.WriteChunkString(IPhChunk_Pic_Author,        FAuthor);
      if (ppMedia       in PProps)                  and (FMedia<>'')              then Streamer.WriteChunkString(IPhChunk_Pic_Media,         FMedia);
      if (ppDescription in PProps)                  and (FDescription<>'')        then Streamer.WriteChunkString(IPhChunk_Pic_Desc,          FDescription);
      if (ppNotes       in PProps)                  and (FNotes<>'')              then Streamer.WriteChunkString(IPhChunk_Pic_Notes,         FNotes);
      if (ppKeywords    in PProps)                  and (FKeywords.Count>0)       then Streamer.WriteChunkString(IPhChunk_Pic_Keywords,      FKeywords.CommaText);
      if (ppRotation    in PProps)                  and (FRotation<>pr0)          then Streamer.WriteChunkByte  (IPhChunk_Pic_Rotation,      Byte(FRotation));
      if (ppFlips       in PProps)                  and (FFlips<>[])              then Streamer.WriteChunkByte  (IPhChunk_Pic_Flips,         Byte(FFlips));
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumPicList
   //===================================================================================================================
type
  TPhotoAlbumPicList = class(TInterfacedObject, IPhoaPicList, IPhoaMutablePicList, IPhotoAlbumPicList)
  private
     // Собственно сам список
    FList: IInterfaceList;
     // Prop storage
    FMaxPicID: Integer;
    FSorted: Boolean;
     // IPhoaPicList
    function  FindID(iID: Integer; var Index: Integer): Boolean; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaPic; stdcall;
    function  GetItemsByFileName(const sFileName: String): IPhoaPic; stdcall;
    function  GetItemsByID(iID: Integer): IPhoaPic; stdcall;
    function  GetMaxPicID: Integer; stdcall;
    function  IndexOfFileName(const sFileName: String): Integer; stdcall;
    function  IndexOfID(iID: Integer): Integer; stdcall;
     // IPhoaMutablePicList
    function  Add(Pic: IPhoaPic; bSkipDuplicates: Boolean): Integer; overload; stdcall;
    function  Add(Pic: IPhoaPic; bSkipDuplicates: Boolean; out bAdded: Boolean): Integer; overload; stdcall;
    function  Add(PicList: IPhoaPicList; bSkipDuplicates: Boolean): Integer; overload; stdcall;
    function  GetItemsByFileNameM(const sFileName: String): IPhoaMutablePic; stdcall;
    function  GetItemsByIDM(iID: Integer): IPhoaMutablePic; stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutablePic; stdcall;
    function  GetSorted: Boolean; stdcall;
    function  Insert(Index: Integer; Pic: IPhoaPic; bSkipDuplicates: Boolean): Boolean; stdcall;
    function  Remove(iID: Integer): Integer; stdcall;
    procedure Assign(Source: IPhoaPicList); stdcall;
    procedure Clear; stdcall;
    procedure CustomSort(CompareFunc: TPhoaPicListSortCompareFunc; dwData: Cardinal); stdcall;
    procedure SortingsSort(Sortings: IPhoaPicSortingList); stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // IPhotoAlbumPicList
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
    procedure DuplicatePics(PicList: IPhoaPicList);
    function  GetItemsByIDX(iID: Integer): IPhotoAlbumPic;
    function  GetItemsByFileNameX(const sFileName: String): IPhotoAlbumPic;
    function  GetItemsX(Index: Integer): IPhotoAlbumPic;
  public
    constructor Create(bSorted: Boolean);
    destructor Destroy; override;
  end;

  function PhotoAlbumPicListSortingsSortCompare(Pic1, Pic2: IPhoaPic; dwData: Cardinal): Integer; stdcall;
  begin
    Result := IPhoaPicSortingList(dwData).SortComparePics(Pic1, Pic2);
  end;

  function TPhotoAlbumPicList.Add(Pic: IPhoaPic; bSkipDuplicates: Boolean): Integer;
  var bDummy: Boolean;
  begin
    Result := Add(Pic, bSkipDuplicates, bDummy);
  end;

  function TPhotoAlbumPicList.Add(Pic: IPhoaPic; bSkipDuplicates: Boolean; out bAdded: Boolean): Integer;
  var iID: Integer;
  begin
    iID := Pic.ID;
     // Проверяем наличие допустимого ID
    if iID<=0 then PhoaException(SPhObjErrMsg_InvalidAddPicID, [iID]);
     // Если нужно проверять дубликаты
    if FSorted or bSkipDuplicates then begin
      bAdded := not FindID(iID, Result);
      if bAdded then FList.Insert(Result, Pic);
    end else begin
      bAdded := True;
      Result := FList.Add(Pic);
    end;
    if bAdded and (iID>FMaxPicID) then FMaxPicID := iID;
  end;

  function TPhotoAlbumPicList.Add(PicList: IPhoaPicList; bSkipDuplicates: Boolean): Integer;
  var
    bAdded: Boolean;
    i: Integer;
  begin
    Result := 0;
    for i := 0 to PicList.Count-1 do begin
      Add(PicList[i], bSkipDuplicates, bAdded);
      if bAdded then Inc(Result);
    end;
  end;

  procedure TPhotoAlbumPicList.Assign(Source: IPhoaPicList);
  var i: Integer;
  begin
    Clear;
    for i := 0 to Source.Count-1 do FList.Add(Source[i]);
    FMaxPicID := Source.MaxPicID;
  end;

  procedure TPhotoAlbumPicList.Clear;
  begin
    FList.Clear;
    FMaxPicID := 0;
  end;

  constructor TPhotoAlbumPicList.Create(bSorted: Boolean);
  begin
    inherited Create;
    FSorted := bSorted;
    FList := TInterfaceList.Create;
  end;

  procedure TPhotoAlbumPicList.CustomSort(CompareFunc: TPhoaPicListSortCompareFunc; dwData: Cardinal);

    procedure QuickSort(iL, iR: Integer);
    var
      i1, i2: Integer;
      p: IPhoaPic;
    begin
      repeat
        i1 := iL;
        i2 := iR;
        p := GetItems((iL+iR) shr 1);
        repeat
          while CompareFunc(GetItems(i1), p, dwData)<0 do Inc(i1);
          while CompareFunc(GetItems(i2), p, dwData)>0 do Dec(i2);
          if i1<=i2 then begin
            FList.Exchange(i1, i2);
            Inc(i1);
            Dec(i2);
          end;
        until i1>i2;
        if iL<i2 then QuickSort(iL, i2);
        iL := i1;
      until i1>=iR;
    end;

  begin
    if FSorted then PhoaException(SPhObjErrMsg_InvalidSortedPicListMethodCall, ['CustomSort']);
    if FList.Count>0 then QuickSort(0, FList.Count-1);
  end;

  procedure TPhotoAlbumPicList.Delete(Index: Integer);
  begin
    FList.Delete(Index);
  end;

  destructor TPhotoAlbumPicList.Destroy;
  begin
    FList := nil;
    inherited Destroy;
  end;

  procedure TPhotoAlbumPicList.DuplicatePics(PicList: IPhoaPicList);
  var i: Integer;
  begin
    Clear;
    for i := 0 to PicList.Count-1 do
       // Создаём экземпляр изображения
      with NewPhotoAlbumPic do begin
         // Копируем в него данные исходного изображения
        Assign(PicList[i]);
         // После этого (ID присвоен) - заносим в список
        PutToList(Self);
      end;
  end;

  function TPhotoAlbumPicList.FindID(iID: Integer; var Index: Integer): Boolean;
  var i1, i2, i, iCompare: Integer;
  begin
    Result := False;
     // Если список сортированный - ищем с помощью бинарного поиска
    if FSorted then begin
      i1 := 0;
      i2 := FList.Count-1;
      while i1<=i2 do begin
        i := (i1+i2) shr 1;
        iCompare := GetItems(i).ID-iID;
        if iCompare<0 then
          i1 := i+1
        else begin
          i2 := i-1;
          if iCompare=0 then begin
            Result := True;
            i1 := i;
          end;
        end;
      end;
      Index := i1;
     // Иначе - ищем простым перебором
    end else begin
      for i := 0 to FList.Count-1 do
        if GetItems(i).ID=iID then begin
          Result := True;
          Index := i;
          Exit;
        end;
      Index := FList.Count;
    end;
  end;

  function TPhotoAlbumPicList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhotoAlbumPicList.GetItems(Index: Integer): IPhoaPic;
  begin
    Result := IPhoaPic(FList[Index]);
  end;

  function TPhotoAlbumPicList.GetItemsByFileName(const sFileName: String): IPhoaPic;
  var idx: Integer;
  begin
    idx := IndexOfFileName(sFileName);
    if idx<0 then Result := nil else Result := GetItems(idx);
  end;

  function TPhotoAlbumPicList.GetItemsByFileNameM(const sFileName: String): IPhoaMutablePic;
  begin
    Result := GetItemsByFileName(sFileName) as IPhoaMutablePic;
  end;

  function TPhotoAlbumPicList.GetItemsByFileNameX(const sFileName: String): IPhotoAlbumPic;
  begin
    Result := GetItemsByFileName(sFileName) as IPhotoAlbumPic;
  end;

  function TPhotoAlbumPicList.GetItemsByID(iID: Integer): IPhoaPic;
  var idx: Integer;
  begin
    idx := IndexOfID(iID);
    if idx<0 then Result := nil else Result := GetItems(idx);
  end;

  function TPhotoAlbumPicList.GetItemsByIDM(iID: Integer): IPhoaMutablePic;
  begin
    Result := GetItemsByID(iID) as IPhoaMutablePic;
  end;

  function TPhotoAlbumPicList.GetItemsByIDX(iID: Integer): IPhotoAlbumPic;
  begin
    Result := GetItemsByID(iID) as IPhotoAlbumPic;
  end;

  function TPhotoAlbumPicList.GetItemsM(Index: Integer): IPhoaMutablePic;
  begin
    Result := GetItems(Index) as IPhoaMutablePic;
  end;

  function TPhotoAlbumPicList.GetItemsX(Index: Integer): IPhotoAlbumPic;
  begin
    Result := GetItems(Index) as IPhotoAlbumPic;
  end;

  function TPhotoAlbumPicList.GetMaxPicID: Integer;
  begin
    Result := FMaxPicID;
  end;

  function TPhotoAlbumPicList.GetSorted: Boolean;
  begin
    Result := FSorted;
  end;

  function TPhotoAlbumPicList.IndexOfFileName(const sFileName: String): Integer;
  begin
    for Result := 0 to FList.Count-1 do
      if ReverseCompare(GetItems(Result).FileName, sFileName) then Exit;
    Result := -1;
  end;

  function TPhotoAlbumPicList.IndexOfID(iID: Integer): Integer;
  begin
    if not FindID(iID, Result) then Result := -1;
  end;

  function TPhotoAlbumPicList.Insert(Index: Integer; Pic: IPhoaPic; bSkipDuplicates: Boolean): Boolean;
  begin
    if FSorted then PhoaException(SPhObjErrMsg_InvalidSortedPicListMethodCall, ['Insert']);
    Result := not bSkipDuplicates or (IndexOfID(Pic.ID)<0);
    if Result then FList.Insert(Index, Pic);
  end;

  procedure TPhotoAlbumPicList.Move(iCurIndex, iNewIndex: Integer);
  var Pic: IPhoaPic;
  begin
    if FSorted then PhoaException(SPhObjErrMsg_InvalidSortedPicListMethodCall, ['Move']);
    Pic := GetItems(iCurIndex);
    FList.Delete(iCurIndex);
    FList.Insert(iNewIndex, Pic);
  end;

  function TPhotoAlbumPicList.Remove(iID: Integer): Integer;
  begin
    if FindID(iID, Result) then FList.Delete(Result) else Result := -1;
  end;

  procedure TPhotoAlbumPicList.SortingsSort(Sortings: IPhoaPicSortingList);
  begin
    CustomSort(PhotoAlbumPicListSortingsSortCompare, Cardinal(Sortings));
  end;

  procedure TPhotoAlbumPicList.StreamerLoad(Streamer: TPhoaStreamer);
  var
    i: Integer;
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;

    procedure LoadPic;
    begin
      with NewPhotoAlbumPic do begin
        StreamerLoad(Streamer, True, PPAllProps);
        PutToList(Self);
      end;
    end;

  begin
    Clear;
     // *** Old format
    if not Streamer.Chunked then
      for i := 0 to Streamer.ReadInt-1 do LoadPic
     // *** New format
    else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Picture
          IPhChunk_Pic_Open: LoadPic;
           // Close-chunk
          IPhChunk_Pics_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicList.StreamerSave(Streamer: TPhoaStreamer);
  var i: Integer;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
      Streamer.WriteInt(FList.Count);
      for i := 0 to FList.Count-1 do GetItemsX(i).StreamerSave(Streamer, True, PPAllProps);
     // *** New format
    end else begin
      Streamer.WriteChunk(IPhChunk_Pics_Open);
      for i := 0 to FList.Count-1 do begin
        Streamer.WriteChunk(IPhChunk_Pic_Open);
        GetItemsX(i).StreamerSave(Streamer, True, PPAllProps);
        Streamer.WriteChunk(IPhChunk_Pic_Close);
      end;
      Streamer.WriteChunk(IPhChunk_Pics_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumKeywordList
   //===================================================================================================================
type
  TPhotoAlbumKeywordList = class(TInterfacedObject, IPhoaKeywordList, IPhoaMutableKeywordList, IPhotoAlbumKeywordList)
  private
     // Собственно список
    FList: TList;
     // Создаёт FList, если он ещё не создан
    procedure CreateList;
     // Ищет ключевое слово и возвращает True и его индекс в Index, если нашла; иначе возвращает False, а в Index -
     //   место вставки нового слова
    function  FindKeyword(const sKeyword: String; var Index: Integer): Boolean;
     // Возвращает записи, разделённые запятыми: при bSelectedOnly=False все, при bSelectedOnly=True - только те, у
     //   которых State=pksOn. Ключевые слова, содержащие запятые и пробелы, заключаются в двойные кавычки
    function  InternalGetCommaText(bSelectedOnly: Boolean): String;
     // IPhoaKeywordList
    function  GetCommaText: String; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): String; stdcall;
    function  IndexOf(const sKeyword: String): Integer; stdcall;
     // IPhoaMutableKeywordList
    function  Add(const sKeyword: String): Integer; stdcall;
    function  Remove(const sKeyword: String): Integer; stdcall;
    function  Rename(Index: Integer; const sNewKeyword: String): Integer; stdcall;
    procedure Assign(Source: IPhoaKeywordList); stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure SetCommaText(const Value: String); stdcall;
     // IPhotoAlbumKeywordList
    function  AddEx(const sKeyword: String; bSelected: Boolean): Integer;
    function  GetKWData(Index: Integer): PPhoaKeywordData;
    function  GetSelectedKeywords: String;
    function  InsertNew: Integer;
    procedure PopulateFromPicList(Pics: IPhoaPicList; IsPicSelCallback: TPhoaKeywordIsPicSelectedProc; iTotalSelCount: Integer);
    procedure SetSelectedKeywords(const Value: String);
  public
    destructor Destroy; override;
  end;

  function TPhotoAlbumKeywordList.Add(const sKeyword: String): Integer;
  begin
    Result := AddEx(sKeyword, False);
  end;

  function TPhotoAlbumKeywordList.AddEx(const sKeyword: String; bSelected: Boolean): Integer;
  var p: PPhoaKeywordData;
  begin
     // Создаём список, если он ещё не создан
    CreateList;
     // Ищем такое слово. Если нашли - приращиваем счётчик
    if FindKeyword(sKeyword, Result) then begin
      p := GetKWData(Result);
      Inc(p.iCount);
     // Иначе вставляем
    end else begin
      New(p);
      FList.Insert(Result, p);
      p.sKeyword  := sKeyword;
      p.Change    := pkcNone;
      p.State     := pksOff;
      p.iCount    := 1;
      p.iSelCount := 0;
    end;
    if bSelected then Inc(p.iSelCount);
  end;

  procedure TPhotoAlbumKeywordList.Assign(Source: IPhoaKeywordList);
  var
    i: Integer;
    PAKL: IPhotoAlbumKeywordList;
    p: PPhoaKeywordData;
  begin
     // Стираем (уничтожаем) список
    Clear;
     // Если есть записи в исходном списке, создаём список
    if Source.Count>0 then CreateList;
     // Проверяем поддержку интерфейса IPhotoAlbumKeywordList
    Supports(Source, IPhotoAlbumKeywordList, PAKL);
     // Копируем слова со списка
    for i := 0 to Source.Count-1 do
       // Если native-интерфейс поддерживается, копируем дополнительные данные ключевого слова
      if PAKL<>nil then begin
        New(p);
        FList.Add(p);
        p^ := PAKL.KWData[i]^;
      end else
        Add(Source[i]);
  end;

  procedure TPhotoAlbumKeywordList.Clear;
  var i: Integer;
  begin
    if FList<>nil then begin
      for i := FList.Count-1 downto 0 do Delete(i);
      FreeAndNil(FList);
    end;
  end;

  procedure TPhotoAlbumKeywordList.CreateList;
  begin
    if FList=nil then FList := TList.Create;
  end;

  procedure TPhotoAlbumKeywordList.Delete(Index: Integer);
  begin
    Dispose(PPhoaKeywordData(FList[Index]));
    FList.Delete(Index);
  end;

  destructor TPhotoAlbumKeywordList.Destroy;
  begin
     // Clear уничтожает FList
    Clear;
    inherited Destroy;
  end;

  function TPhotoAlbumKeywordList.FindKeyword(const sKeyword: String; var Index: Integer): Boolean;
  var i1, i2, i, iCompare: Integer;
  begin
    Result := False;
    if FList=nil then
      Index := 0
     // Т.к. список сортированный - ищем с помощью бинарного поиска
    else begin
      i1 := 0;
      i2 := FList.Count-1;
      while i1<=i2 do begin
        i := (i1+i2) shr 1;
        iCompare := AnsiCompareText(GetKWData(i).sKeyword, sKeyword);
        if iCompare<0 then
          i1 := i+1
        else begin
          i2 := i-1;
          if iCompare=0 then begin
            Result := True;
            i1 := i;
          end;
        end;
      end;
      Index := i1;
    end;
  end;

  function TPhotoAlbumKeywordList.GetCommaText: String;
  begin
    Result := InternalGetCommaText(False);
  end;

  function TPhotoAlbumKeywordList.GetCount: Integer;
  begin
    if FList=nil then Result := 0 else Result := FList.Count;
  end;

  function TPhotoAlbumKeywordList.GetItems(Index: Integer): String;
  begin
    Result := GetKWData(Index).sKeyword;
  end;

  function TPhotoAlbumKeywordList.GetKWData(Index: Integer): PPhoaKeywordData;
  begin
    Result := FList[Index];
  end;

  function TPhotoAlbumKeywordList.GetSelectedKeywords: String;
  begin
    Result := InternalGetCommaText(True);
  end;

  function TPhotoAlbumKeywordList.IndexOf(const sKeyword: String): Integer;
  begin
    if not FindKeyword(sKeyword, Result) then Result := -1;
  end;

  function TPhotoAlbumKeywordList.InsertNew: Integer;
  var
    sKWBase, sNewKeyword: String;
    iAttempt: Integer;
    p: PPhoaKeywordData;
  begin
     // Ищем уникальное ключевое слово вида "Новое ключевое слово (n)"
    sKWBase := ConstVal('SDefaultNewKeyword');
    iAttempt := 0;
    repeat
      Inc(iAttempt);
      sNewKeyword := Format(iif(iAttempt<2, '%s', '%s (%d)'), [sKWBase, iAttempt]);
    until not FindKeyword(sNewKeyword, Result);
     // Добавляем
    CreateList; 
    New(p);
    FList.Insert(Result, p);
    p.sKeyword  := sNewKeyword;
    p.Change    := pkcAdd;
    p.State     := pksOn;
    p.iCount    := 0;
    p.iSelCount := 0;
  end;

  function TPhotoAlbumKeywordList.InternalGetCommaText(bSelectedOnly: Boolean): String;
  var
    i: Integer;
    p: PPhoaKeywordData;
  begin
    Result := '';
    if FList<>nil then
      for i := 0 to FList.Count-1 do begin
        p := FList[i];
        if not bSelectedOnly or (p.State=pksOn) then
          if LastDelimiter(' ,"', p.sKeyword)>0 then
            AccumulateStr(Result, ',', AnsiQuotedStr(p.sKeyword, '"'))
          else
            AccumulateStr(Result, ',', p.sKeyword);
      end;
  end;

  procedure TPhotoAlbumKeywordList.PopulateFromPicList(Pics: IPhoaPicList; IsPicSelCallback: TPhoaKeywordIsPicSelectedProc; iTotalSelCount: Integer);
  var
    ip, ikw: Integer;
    Pic: IPhoaPic;
    bSelected: Boolean;
  begin
     // Стираем список
    Clear;
     // Цикл по всем изображениям списка
    bSelected := False;
    for ip := 0 to Pics.Count-1 do begin
      Pic := Pics[ip];
       // Если передана callback-процедура, вызываем её для определения: выбрано изображение или нет
      if Assigned(IsPicSelCallback) then IsPicSelCallback(Pic, bSelected);
      for ikw := 0 to Pic.Keywords.Count-1 do AddEx(Pic.Keywords[ikw], bSelected);
    end;
     // Выставляем состояние выбора
    if (FList<>nil) and Assigned(IsPicSelCallback) and (iTotalSelCount>0) then
      for ikw := 0 to FList.Count-1 do
        with GetKWData(ikw)^ do
          if      iSelCount=0              then State := pksOff
          else if iSelCount=iTotalSelCount then State := pksOn
          else                                  State := pksGrayed;
  end;

  function TPhotoAlbumKeywordList.Remove(const sKeyword: String): Integer;
  begin
    if FindKeyword(sKeyword, Result) then Delete(Result) else Result := -1;
  end;

  function TPhotoAlbumKeywordList.Rename(Index: Integer; const sNewKeyword: String): Integer;
  var p: PPhoaKeywordData;
  begin
     // Если вообще совпадает, делать нечего
    Result := Index;
    p := GetKWData(Index);
    if p.sKeyword=sNewKeyword then Exit;
     // Если текст (без учёта регистра) поменялся, сдвигаем на новое место
    if not AnsiSameText(p.sKeyword, sNewKeyword) then begin
       // Если уже есть такое в списке, вызываем Exception
      if FindKeyword(sNewKeyword, Result) then PhoaException(ConstVal('SErrDuplicateKeyword'));
       // Иначе сдвигаем на новое место
      if Index<Result then Dec(Result); 
      FList.Move(Index, Result); 
    end;
     // Настраиваем Change
    case p.Change of
       // Первое изменение - сохраняем старое значение и устанавливаем изменение kcReplace
      pkcNone: begin
        p.sOldKeyword := p.sKeyword;
        p.Change      := pkcReplace;
      end;
       // Не первое изменение - если возвращаем старое значение, снимаем изменение
      pkcReplace:
        if p.sOldKeyword=sNewKeyword then begin
          p.Change      := pkcNone;
          p.sOldKeyword := '';
        end;
    end;
     // Записываем новое слово
    p.sKeyword := sNewKeyword;
  end;

  procedure TPhotoAlbumKeywordList.SetCommaText(const Value: String);
  var
    SL: TStringList;
    i: Integer;
  begin
    Clear;
    if Value<>'' then begin
      SL := TStringList.Create;
      try
        SL.CommaText := Value;
        for i := 0 to SL.Count-1 do Add(SL[i]);
      finally
        SL.Free;
      end;
    end;
  end;

  procedure TPhotoAlbumKeywordList.SetSelectedKeywords(const Value: String);
  var
    SL: TStringList;
    i: Integer;
  begin
    if FList<>nil then begin
      SL := TStringList.Create;
      try
         // Переписываем слова в виде StringList
        SL.Sorted := True;
        SL.Duplicates := dupIgnore;
        SL.CommaText := Value;
         // Проверяем наличие слов
        for i := 0 to FList.Count-1 do
          with GetKWData(i)^ do
            if SL.IndexOf(sKeyword)<0 then State := pksOff else State := pksOn;
      finally
        SL.Free;
      end;
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumPicGroup
   //===================================================================================================================
type
  TPhotoAlbumPicGroup = class(TInterfacedObject, IPhoaPicGroup, IPhoaMutablePicGroup, IPhotoAlbumPicGroup)
  private
     // Список-владелец класса IPhoaPicGroupList, хранится как Pointer во избежания создания ссылки (может быть nil)
    FList: Pointer;
     // ID изображений в группе для загрузки из Streamer-a (существует только между вызовами StreamerLoad и Loaded)
    FStreamerPicIDs: TIntegerList;
     // Prop storage
    FDescription: String;
    FExpanded: Boolean;
    FGroups: IPhotoAlbumPicGroupList;
    FID: Integer;
    FPics: IPhotoAlbumPicList;
    FText: String;
     // IPhoaPicGroup
    function  IsPicLinked(iID: Integer; bRecursive: Boolean): Boolean; stdcall;
    function  GetDescription: String; stdcall;
    function  GetExpanded: Boolean; stdcall;
    function  GetMaxGroupID: Integer; stdcall;
    function  GetGroups: IPhoaPicGroupList; stdcall;
    function  GetGroupByID(iID: Integer): IPhoaPicGroup; stdcall;
    function  GetGroupByPath(const sPath: String): IPhoaPicGroup; stdcall;
    function  GetID: Integer; stdcall;
    function  GetIndex: Integer; stdcall;
    function  GetNestedGroupCount: Integer; stdcall;
    function  GetOwner: IPhoaPicGroup; stdcall;
    function  GetPath(const sRootName: String): String; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
    function  GetProps(GroupProp: TGroupProperty): String; stdcall;
    function  GetRoot: IPhoaPicGroup; stdcall;
    function  GetText: String; stdcall;
     // IPhoaMutablePicGroup
    function  GetGroupByIDM(iID: Integer): IPhoaMutablePicGroup; stdcall;
    function  GetGroupByPathM(const sPath: String): IPhoaMutablePicGroup; stdcall;
    function  GetGroupsM: IPhoaMutablePicGroupList; stdcall;
    function  GetOwnerM: IPhoaMutablePicGroup; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
    function  GetRootM: IPhoaMutablePicGroup; stdcall;
    procedure Assign(Source: IPhoaPicGroup; bCopyIDs, bCopyPics, bCopySubgroups: Boolean); stdcall;
    procedure SetDescription(const Value: String); stdcall;
    procedure SetExpanded(Value: Boolean); stdcall;
    procedure SetIndex(Value: Integer); stdcall;
    procedure SetOwner(Value: IPhoaPicGroup); stdcall;
    procedure SetText(const Value: String); stdcall;
     // IPhotoAlbumPicGroup
    function  GetGroupByIDX(iID: Integer): IPhotoAlbumPicGroup;
    function  GetGroupByPathX(const sPath: String): IPhotoAlbumPicGroup;
    function  GetGroupsX: IPhotoAlbumPicGroupList;
    function  GetOwnerX: IPhotoAlbumPicGroup;
    function  GetPicsX: IPhotoAlbumPicList;
    function  GetRootX: IPhotoAlbumPicGroup;
    procedure FixupIDs;
    procedure InternalFixupIDs(var iMaxGroupID: Integer);
    procedure Loaded(Project: IPhotoAlbumProject);
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  public
    constructor Create(AOwner: IPhoaPicGroup; iID: Integer);
    destructor Destroy; override;
  end;

  procedure TPhotoAlbumPicGroup.Assign(Source: IPhoaPicGroup; bCopyIDs, bCopyPics, bCopySubgroups: Boolean);
  var i: Integer;
  begin
    if bCopyIDs then FID := Source.ID;
    FText        := Source.Text;
    FDescription := Source.Description;
    FExpanded    := Source.Expanded;
     // Копируем список изображений
    if bCopyPics then FPics.Assign(Source.Pics);
     // Копируем подчинённые группы
    if bCopySubgroups then begin
      FGroups.Clear;
      for i := 0 to Source.Groups.Count-1 do
        NewPhotoAlbumPicGroup(Self, 0).Assign(Source.Groups[i], bCopyIDs, bCopyPics, True);
    end;
  end;

  constructor TPhotoAlbumPicGroup.Create(AOwner: IPhoaPicGroup; iID: Integer);
  begin
    inherited Create;
    FGroups := NewPhotoAlbumPicGroupList(Self);
    FPics   := NewPhotoAlbumPicList(False);
    SetOwner(AOwner);
    FID     := iID;
  end;

  destructor TPhotoAlbumPicGroup.Destroy;
  begin
    FStreamerPicIDs.Free;
    FGroups := nil;
    FPics   := nil;
    inherited Destroy;
  end;

  procedure TPhotoAlbumPicGroup.FixupIDs;
  var iMaxID: Integer;
  begin
    iMaxID := GetMaxGroupID;
    InternalFixupIDs(iMaxID);
  end;

  function TPhotoAlbumPicGroup.GetDescription: String;
  begin
    Result := FDescription;
  end;

  function TPhotoAlbumPicGroup.GetExpanded: Boolean;
  begin
    Result := FExpanded;
  end;

  function TPhotoAlbumPicGroup.GetGroupByID(iID: Integer): IPhoaPicGroup;
  var i: Integer;
  begin
    if iID<=0 then PhoaException(SPhObjErrMsg_InvalidGroupID, [iID]);
    if FID=iID then
      Result := Self
    else begin
      for i := 0 to FGroups.Count-1 do begin
        Result := FGroups[i].GroupByID[iID];
        if Result<>nil then Exit;
      end;
      Result := nil;
    end;
  end;

  function TPhotoAlbumPicGroup.GetGroupByIDM(iID: Integer): IPhoaMutablePicGroup;
  begin
    Result := GetGroupByID(iID) as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumPicGroup.GetGroupByIDX(iID: Integer): IPhotoAlbumPicGroup;
  begin
    Result := GetGroupByID(iID) as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumPicGroup.GetGroupByPath(const sPath: String): IPhoaPicGroup;
  var
    s, sFirst: String;
    i: Integer;
    g: IPhoaPicGroup;
  begin
    Result := nil;
    s := sPath;
    if s<>'' then begin
       // Удаляем символ '/' в начале, если он есть
      if s[1]='/' then Delete(s, 1, 1);
       // Выделяем первую часть пути
      sFirst := ExtractFirstWord(s, '/');
       // Ищем ребёнка с именем, совпадающим с первой частью пути
      for i := 0 to FGroups.Count-1 do begin
        g := FGroups[i];
         // Нашли
        if AnsiSameText(g.Text, sFirst) then begin
           // Если путь кончился, его мы и искали. Иначе ищем по остатку пути среди детей ребёнка
          if s='' then Result := g else Result := g.GroupByPath[s];
          Break; 
        end;
      end;
    end;
  end;

  function TPhotoAlbumPicGroup.GetGroupByPathM(const sPath: String): IPhoaMutablePicGroup;
  begin
    Result := GetGroupByPath(sPath) as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumPicGroup.GetGroupByPathX(const sPath: String): IPhotoAlbumPicGroup;
  begin
    Result := GetGroupByPath(sPath) as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumPicGroup.GetGroups: IPhoaPicGroupList;
  begin
    Result := FGroups;
  end;

  function TPhotoAlbumPicGroup.GetGroupsM: IPhoaMutablePicGroupList;
  begin
    Result := FGroups;
  end;

  function TPhotoAlbumPicGroup.GetGroupsX: IPhotoAlbumPicGroupList;
  begin
    Result := FGroups;
  end;

  function TPhotoAlbumPicGroup.GetID: Integer;
  begin
    Result := FID;
  end;

  function TPhotoAlbumPicGroup.GetIndex: Integer;
  begin
    if FList=nil then Result := -1 else Result := IPhoaPicGroupList(FList).IndexOf(Self);
  end;

  function TPhotoAlbumPicGroup.GetMaxGroupID: Integer;
  var i, iChildMaxID: Integer;
  begin
     // Устанавливаем максимальным наш ID
    Result := FID;
     // Перебираем всех детей, проверяя на максимальный MaxGroupID
    for i := 0 to FGroups.Count-1 do begin
      iChildMaxID := FGroups[i].MaxGroupID;
      if Result<iChildMaxID then Result := iChildMaxID;
    end;
  end;

  function TPhotoAlbumPicGroup.GetNestedGroupCount: Integer;
  var i: Integer;
  begin
    Result := 0;
    for i := 0 to FGroups.Count-1 do begin
      Inc(Result);
      Inc(Result, FGroups[i].NestedGroupCount);
    end;
  end;

  function TPhotoAlbumPicGroup.GetOwner: IPhoaPicGroup;
  begin
    if FList=nil then Result := nil else Result := IPhoaPicGroupList(FList).Owner;
  end;

  function TPhotoAlbumPicGroup.GetOwnerM: IPhoaMutablePicGroup;
  begin
    Result := GetOwner as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumPicGroup.GetOwnerX: IPhotoAlbumPicGroup;
  begin
    Result := GetOwner as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumPicGroup.GetPath(const sRootName: String): String;
  begin
    if GetOwner=nil then Result := sRootName else Result := GetOwner.Path[sRootName]+'/'+FText;
  end;

  function TPhotoAlbumPicGroup.GetPics: IPhoaPicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumPicGroup.GetPicsM: IPhoaMutablePicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumPicGroup.GetPicsX: IPhotoAlbumPicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumPicGroup.GetProps(GroupProp: TGroupProperty): String;

    function IntToStrPositive(i: Integer): String;
    begin
      if i>0 then Result := IntToStr(i) else Result := '';
    end;

  begin
    Result := '';
    case GroupProp of
      gpID:          Result := IntToStr(FID);
      gpText:        Result := FText;
      gpDescription: Result := FDescription;
      gpPicCount:    Result := IntToStrPositive(FPics.Count);
      gpGroupCount:  Result := IntToStrPositive(GetNestedGroupCount);
    end;
  end;

  function TPhotoAlbumPicGroup.GetRoot: IPhoaPicGroup;
  begin
    if GetOwner=nil then Result := Self else Result := GetOwner.Root;
  end;

  function TPhotoAlbumPicGroup.GetRootM: IPhoaMutablePicGroup;
  begin
    Result := GetRoot as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumPicGroup.GetRootX: IPhotoAlbumPicGroup;
  begin
    Result := GetRoot as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumPicGroup.GetText: String;
  begin
    Result := FText;
  end;

  procedure TPhotoAlbumPicGroup.InternalFixupIDs(var iMaxGroupID: Integer);
  var i: Integer;
  begin
     // Проверяем свой ID
    if FID=0 then begin
      FID := iMaxGroupID+1;
      Inc(iMaxGroupID);
    end;
     // Повторяем то же для подгрупп
    for i := 0 to FGroups.Count-1 do FGroups[i].InternalFixupIDs(iMaxGroupID);
  end;

  function TPhotoAlbumPicGroup.IsPicLinked(iID: Integer; bRecursive: Boolean): Boolean;
  var i: Integer;
  begin
    Result := FPics.IndexOfID(iID)>=0;
    if bRecursive then begin
      i := 0;
      while not Result and (i<FGroups.Count) do begin
        Result := FGroups[i].IsPicLinked(iID, True);
        Inc(i);
      end;
    end;
  end;

  procedure TPhotoAlbumPicGroup.Loaded(Project: IPhotoAlbumProject);
  var
    i: Integer;
    Pic: IPhoaPic;
  begin
     // Если корневая группа - запускаем назначение ID группам, его не имеющим (нужно, если фотоальбом сохранён версией
     //   PhoA раньше 1.1.5)
    if GetOwner=nil then FixupIDs;
     // Превращаем ID изображений в ссылки на изображения
    if FStreamerPicIDs<>nil then begin
      for i := 0 to FStreamerPicIDs.Count-1 do begin
        Pic := Project.Pics.ItemsByID[FStreamerPicIDs[i]];
        if Pic<>nil then FPics.Add(Pic, False);
      end;
       // Убиваем список ID изображений для загрузки
      FreeAndNil(FStreamerPicIDs);
    end;
     // Рекурсивно вызываем для подчинённых групп
    for i := 0 to FGroups.Count-1 do FGroups[i].Loaded(Project);
  end;

  procedure TPhotoAlbumPicGroup.SetDescription(const Value: String);
  begin
    FDescription := Value;
  end;

  procedure TPhotoAlbumPicGroup.SetExpanded(Value: Boolean);
  begin
    FExpanded := Value;
  end;

  procedure TPhotoAlbumPicGroup.SetIndex(Value: Integer);
  var idxCur: Integer;
  begin
    if FList<>nil then begin
      idxCur := GetIndex;
      if idxCur<>Value then (IInterface(FList) as IPhoaMutablePicGroupList).Move(idxCur, Value);
    end;
  end;

  procedure TPhotoAlbumPicGroup.SetOwner(Value: IPhoaPicGroup);
  begin
    if GetOwner<>Value then begin
      if FList<>nil then (IInterface(FList) as IPhoaMutablePicGroupList).Remove(FID);
      if Value=nil then
        FList := nil
      else begin
        FList := Pointer(Value.Groups);
        (IInterface(FList) as IPhoaMutablePicGroupList).Add(Self);
      end;
    end;
  end;

  procedure TPhotoAlbumPicGroup.SetText(const Value: String);
  begin
    FText := Value;
  end;

  procedure TPhotoAlbumPicGroup.StreamerLoad(Streamer: TPhoaStreamer);
  var
    i, iPicCount: Integer;
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
     // Стираем список подчинённых групп
    FGroups.Clear; 
     // Стираем список изображений
    FPics.Clear;
    FreeAndNil(FStreamerPicIDs);
     // *** Old format
    if not Streamer.Chunked then begin
       // Read group properties
      FText     := Streamer.ReadStringI;
      FExpanded := Streamer.ReadByte<>0;
       // Read picture IDs
      iPicCount := Streamer.ReadInt;
      if iPicCount>0 then begin
        FStreamerPicIDs := TIntegerList.Create(False);
        for i := 0 to iPicCount-1 do FStreamerPicIDs.Add(Streamer.ReadInt);
      end;
       // Read nested groups
      FGroups.StreamerLoad(Streamer);
     // *** New format
    end else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Group properties
          IPhChunk_Group_ID:          FID          := vValue;
          IPhChunk_Group_Text:        FText        := vValue;
          IPhChunk_Group_Expanded:    FExpanded    := vValue<>Byte(0);
          IPhChunk_Group_Description: FDescription := vValue;
           // Picture IDs
          IPhChunk_GroupPics_Open:
            while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
              case Code of
                 // Picture ID
                IPhChunk_GroupPic_ID: begin
                  if FStreamerPicIDs=nil then FStreamerPicIDs := TIntegerList.Create(False);
                  FStreamerPicIDs.Add(vValue);
                end;
                 // Close-chunk
                IPhChunk_GroupPics_Close: Break;
                 // Ensure unknown nested structures are skipped whole
                else Streamer.SkipNestedChunks(Code);
              end;
           // Nested groups
          IPhChunk_Groups_Open: FGroups.StreamerLoad(Streamer);
           // Close-chunk
          IPhChunk_Group_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicGroup.StreamerSave(Streamer: TPhoaStreamer);
  var i: Integer;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
       // Write group properties
      Streamer.WriteStringI(FText);
      Streamer.WriteByte   (Byte(FExpanded));
       // Write picture IDs
      Streamer.WriteInt    (FPics.Count);
      for i := 0 to FPics.Count-1 do Streamer.WriteInt(FPics[i].ID);
       // Write nested groups
      FGroups.StreamerSave(Streamer);
     // *** New format
    end else begin
       // Write open-chunk
      Streamer.WriteChunk(IPhChunk_Group_Open);
       // Write group props
      Streamer.WriteChunkInt   (IPhChunk_Group_ID,          FID);
      Streamer.WriteChunkString(IPhChunk_Group_Text,        FText);
      Streamer.WriteChunkByte  (IPhChunk_Group_Expanded,    Byte(FExpanded));
      Streamer.WriteChunkString(IPhChunk_Group_Description, FDescription);
       // Write picture IDs
      Streamer.WriteChunk(IPhChunk_GroupPics_Open);
      for i := 0 to FPics.Count-1 do Streamer.WriteChunkInt(IPhChunk_GroupPic_ID, FPics[i].ID);
      Streamer.WriteChunk(IPhChunk_GroupPics_Close);
       // Write nested groups
      FGroups.StreamerSave(Streamer);
       // Write close-chunk
      Streamer.WriteChunk(IPhChunk_Group_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumPicGroupList
   //===================================================================================================================
type
  TPhotoAlbumPicGroupList = class(TInterfacedObject, IPhoaPicGroupList, IPhoaMutablePicGroupList, IPhotoAlbumPicGroupList)
  private
     // Собственно список
    FList: IInterfaceList;
     // Prop storage
    FOwner: Pointer;
     // IPhoaPicGroupList
    function  IndexOf(Group: IPhoaPicGroup): Integer; stdcall;
    function  IndexOfID(iID: Integer): Integer; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaPicGroup; stdcall;
    function  GetOwner: IPhoaPicGroup; stdcall;
     // IPhoaMutablePicGroupList
    function  Add(Group: IPhoaPicGroup): Integer; stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutablePicGroup; stdcall;
    function  GetOwnerM: IPhoaMutablePicGroup; stdcall;
    function  Remove(iID: Integer): Integer; stdcall;
    procedure Assign(Source: IPhoaPicGroupList); stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Insert(Index: Integer; Group: IPhoaPicGroup); stdcall;
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // IPhotoAlbumPicGroupList
    function  GetItemsX(Index: Integer): IPhotoAlbumPicGroup;
    function  GetOwnerX: IPhotoAlbumPicGroup;
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  public
    constructor Create(AOwner: IPhoaPicGroup);
  end;

  function TPhotoAlbumPicGroupList.Add(Group: IPhoaPicGroup): Integer;
  begin
    Result := FList.Add(Group);
  end;

  procedure TPhotoAlbumPicGroupList.Assign(Source: IPhoaPicGroupList);
  var i: Integer;
  begin
    Clear;
    for i := 0 to Source.Count-1 do Add(Source[i]);
  end;

  procedure TPhotoAlbumPicGroupList.Clear;
  begin
    FList.Clear;
  end;

  constructor TPhotoAlbumPicGroupList.Create(AOwner: IPhoaPicGroup);
  begin
    inherited Create;
    FOwner := Pointer(AOwner);
    FList := TInterfaceList.Create;
  end;

  procedure TPhotoAlbumPicGroupList.Delete(Index: Integer);
  begin
    FList.Delete(Index);
  end;

  function TPhotoAlbumPicGroupList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhotoAlbumPicGroupList.GetItems(Index: Integer): IPhoaPicGroup;
  begin
    Result := IPhoaPicGroup(FList[Index]);
  end;

  function TPhotoAlbumPicGroupList.GetItemsM(Index: Integer): IPhoaMutablePicGroup;
  begin
    Result := FList[Index] as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumPicGroupList.GetItemsX(Index: Integer): IPhotoAlbumPicGroup;
  begin
    Result := FList[Index] as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumPicGroupList.GetOwner: IPhoaPicGroup;
  begin
    Result := IPhoaPicGroup(FOwner);
  end;

  function TPhotoAlbumPicGroupList.GetOwnerM: IPhoaMutablePicGroup;
  begin
    Result := GetOwner as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumPicGroupList.GetOwnerX: IPhotoAlbumPicGroup;
  begin
    Result := GetOwner as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumPicGroupList.IndexOf(Group: IPhoaPicGroup): Integer;
  begin
    for Result := 0 to FList.Count-1 do
      if GetItems(Result)=Group then Exit;
    Result := -1;
  end;

  function TPhotoAlbumPicGroupList.IndexOfID(iID: Integer): Integer;
  begin
    for Result := 0 to FList.Count-1 do
      if GetItems(Result).ID=iID then Exit;
    Result := -1;
  end;

  procedure TPhotoAlbumPicGroupList.Insert(Index: Integer; Group: IPhoaPicGroup);
  begin
    FList.Insert(Index, Group);
  end;

  procedure TPhotoAlbumPicGroupList.Move(iCurIndex, iNewIndex: Integer);
  var Group: IPhoaPicGroup;
  begin
    Group := GetItems(iCurIndex);
    FList.Delete(iCurIndex);
    FList.Insert(iNewIndex, Group);
  end;

  function TPhotoAlbumPicGroupList.Remove(iID: Integer): Integer;
  begin
    Result := IndexOfID(iID);
    if Result>=0 then FList.Delete(Result);
  end;

  procedure TPhotoAlbumPicGroupList.StreamerLoad(Streamer: TPhoaStreamer);
  var
    i: Integer;
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
    Clear;
     // *** Old format
    if not Streamer.Chunked then
       // Read nested groups
      for i := 0 to Streamer.ReadInt-1 do NewPhotoAlbumPicGroup(IPhoaPicGroup(FOwner), 0).StreamerLoad(Streamer)
     // *** New format
    else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Nested group
          IPhChunk_Group_Open: NewPhotoAlbumPicGroup(IPhoaPicGroup(FOwner), 0).StreamerLoad(Streamer);
           // Close-chunk
          IPhChunk_Groups_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicGroupList.StreamerSave(Streamer: TPhoaStreamer);
  var i: Integer;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
       // Write nested groups
      Streamer.WriteInt(FList.Count);
      for i := 0 to FList.Count-1 do GetItemsX(i).StreamerSave(Streamer);
     // *** New format
    end else begin
      Streamer.WriteChunk(IPhChunk_Groups_Open);
       // Write nested groups
      for i := 0 to FList.Count-1 do GetItemsX(i).StreamerSave(Streamer);
      Streamer.WriteChunk(IPhChunk_Groups_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumPicSorting
   //===================================================================================================================
type
  TPhotoAlbumPicSorting = class(TInterfacedObject, IPhoaPicSorting, IPhoaMutablePicSorting, IPhotoAlbumPicSorting)
  private
     // Prop storage
    FProp: TPicProperty;
    FDirection: TPhoaSortDirection;
     // IPhoaPicSorting
    function  GetProp: TPicProperty; stdcall;
    function  GetDirection: TPhoaSortDirection; stdcall;
     // IPhoaMutablePicSorting
    procedure Assign(Source: IPhoaPicSorting); stdcall;
    procedure SetProp(Value: TPicProperty); stdcall;
    procedure SetDirection(Value: TPhoaSortDirection); stdcall;
    procedure ToggleDirection; stdcall;
     // IPhotoAlbumPicSorting
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  end;

  procedure TPhotoAlbumPicSorting.Assign(Source: IPhoaPicSorting);
  begin
    FProp      := Source.Prop;
    FDirection := Source.Direction;
  end;

  function TPhotoAlbumPicSorting.GetDirection: TPhoaSortDirection;
  begin
    Result := FDirection;
  end;

  function TPhotoAlbumPicSorting.GetProp: TPicProperty;
  begin
    Result := FProp;
  end;

  procedure TPhotoAlbumPicSorting.SetDirection(Value: TPhoaSortDirection);
  begin
    if Value in [Low(Value)..High(Value)] then FDirection := Value;
  end;

  procedure TPhotoAlbumPicSorting.SetProp(Value: TPicProperty);
  begin
    if Value in [Low(Value)..High(Value)] then FProp := Value;
  end;

  procedure TPhotoAlbumPicSorting.StreamerLoad(Streamer: TPhoaStreamer);
  var
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
     // Инициализируем данные
    FProp      := Low(FProp);
    FDirection := Low(FDirection);
     // *** New format only
    if Streamer.Chunked then
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
          IPhChunk_ViewSorting_Prop:  SetProp(aXlat_ChunkSortingPropToPicProperty[Integer(vValue)]);
          IPhChunk_ViewSorting_Order: SetDirection(TPhoaSortDirection(vValue));
           // Close-chunk
          IPhChunk_ViewSorting_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicSorting.StreamerSave(Streamer: TPhoaStreamer);
  begin
     // *** New format only
    if Streamer.Chunked then begin
      Streamer.WriteChunk(IPhChunk_ViewSorting_Open);
      Streamer.WriteChunkWord(IPhChunk_ViewSorting_Prop,  aXlat_PicPropertyToChunkSortingProp[FProp]);
      Streamer.WriteChunkByte(IPhChunk_ViewSorting_Order, Byte(FDirection));
      Streamer.WriteChunk(IPhChunk_ViewSorting_Close);
    end;
  end;

  procedure TPhotoAlbumPicSorting.ToggleDirection;
  begin
    if FDirection=psdAsc then FDirection := psdDesc else FDirection := psdAsc; 
  end;

   //===================================================================================================================
   // TPhotoAlbumPicSortingList
   //===================================================================================================================
type
  TPhotoAlbumPicSortingList = class(TInterfacedObject, IPhoaPicSortingList, IPhoaMutablePicSortingList, IPhotoAlbumPicSortingList)
  private
     // Собственно список
    FList: IInterfaceList; 
     // IPhoaPicSortingList
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaPicSorting; stdcall;
    function  IndexOfProp(Prop: TPicProperty): Integer; stdcall;
    function  SortComparePics(Pic1, Pic2: IPhoaPic): Integer; stdcall;
     // IPhoaMutablePicSortingList
    procedure Assign(Source: IPhoaPicSortingList); stdcall;
    function  Add(Item: IPhoaPicSorting): Integer; stdcall;
    procedure Insert(Index: Integer; Item: IPhoaPicSorting); stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Clear; stdcall;
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutablePicSorting; stdcall;
     // IPhotoAlbumPicSortingList
    procedure RegSave(const sRoot, sSection: String);
    procedure RegLoad(const sRoot, sSection: String);
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
    procedure RevertToDefaults;
    function  GetItemsX(Index: Integer): IPhotoAlbumPicSorting;
  public
    constructor Create;
  end;

  function TPhotoAlbumPicSortingList.Add(Item: IPhoaPicSorting): Integer;
  begin
    Result := FList.Add(Item);
  end;

  procedure TPhotoAlbumPicSortingList.Assign(Source: IPhoaPicSortingList);
  var
    i: Integer;
    Sorting: IPhotoAlbumPicSorting;
  begin
    Clear;
    for i := 0 to Source.Count-1 do begin
      Sorting := NewPhotoAlbumPicSorting;
      Sorting.Assign(Source[i]);
      Add(Sorting);
    end;
  end;

  procedure TPhotoAlbumPicSortingList.Clear;
  begin
    FList.Clear;
  end;

  constructor TPhotoAlbumPicSortingList.Create;
  begin
    inherited Create;
    FList := TInterfaceList.Create;
  end;

  procedure TPhotoAlbumPicSortingList.Delete(Index: Integer);
  begin
    FList.Delete(Index);
  end;

  function TPhotoAlbumPicSortingList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhotoAlbumPicSortingList.GetItems(Index: Integer): IPhoaPicSorting;
  begin
    Result := IPhoaPicSorting(FList[Index]);
  end;

  function TPhotoAlbumPicSortingList.GetItemsM(Index: Integer): IPhoaMutablePicSorting;
  begin
    Result := GetItems(Index) as IPhoaMutablePicSorting;
  end;

  function TPhotoAlbumPicSortingList.GetItemsX(Index: Integer): IPhotoAlbumPicSorting;
  begin
    Result := GetItems(Index) as IPhotoAlbumPicSorting;
  end;

  function TPhotoAlbumPicSortingList.IndexOfProp(Prop: TPicProperty): Integer;
  begin
    for Result := 0 to GetCount-1 do
      if GetItems(Result).Prop=Prop then Exit;
    Result := -1;
  end;

  procedure TPhotoAlbumPicSortingList.Insert(Index: Integer; Item: IPhoaPicSorting);
  begin
    FList.Insert(Index, Item);
  end;

  procedure TPhotoAlbumPicSortingList.Move(iCurIndex, iNewIndex: Integer);
  var Item: IPhoaPicSorting;
  begin
    Item := GetItems(iCurIndex);
    FList.Delete(iCurIndex);
    FList.Insert(iNewIndex, Item);
  end;

  procedure TPhotoAlbumPicSortingList.RegLoad(const sRoot, sSection: String);
  var
    sl: TStringList;
    i: Integer;
    s: String;
    Item: IPhotoAlbumPicSorting;
  begin
    sl := TStringList.Create;
    try
      with TRegIniFile.Create(sRoot) do
        try
          ReadSectionValues(sSection, sl);
        finally
          Free;
        end;
       // Если есть какие-то сортировки, переписываем их
      if sl.Count>0 then begin
        Clear;
        for i := 0 to sl.Count-1 do begin
          s := sl.ValueFromIndex[i];
          Item := NewPhotoAlbumPicSorting;
          Item.Prop      := TPicProperty      (StrToIntDef(ExtractFirstWord(s, ','), 0));
          Item.Direction := TPhoaSortDirection(StrToIntDef(ExtractFirstWord(s, ','), 0));
          Add(Item);
        end;
       // Иначе берём по умолчанию
      end else
        RevertToDefaults;
    finally
      sl.Free;
    end;
  end;

  procedure TPhotoAlbumPicSortingList.RegSave(const sRoot, sSection: String);
  var
    i: Integer;
    Item: IPhoaPicSorting;
  begin
    with TRegIniFile.Create(sRoot) do
      try
        EraseSection(sSection);
        for i := 0 to GetCount-1 do begin
          Item := GetItems(i);
          WriteString(sSection, 'Item'+IntToStr(i), Format('%d,%d', [Byte(Item.Prop), Byte(Item.Direction)]));
        end;
      finally
        Free;
      end;
  end;

  procedure TPhotoAlbumPicSortingList.RevertToDefaults;

    procedure AddItem(Prop: TPicProperty; Direction: TPhoaSortDirection);
    var Item: IPhotoAlbumPicSorting;
    begin
      Item := NewPhotoAlbumPicSorting;
      Item.Prop      := Prop;
      Item.Direction := Direction;
      Add(Item);
    end;

  begin
    Clear;
    AddItem(ppDate,        psdAsc);
    AddItem(ppFrameNumber, psdAsc);
  end;

  function TPhotoAlbumPicSortingList.SortComparePics(Pic1, Pic2: IPhoaPic): Integer;
  var
    i: Integer;
    Item: IPhoaPicSorting;
  begin
    Result := 0;
    for i := 0 to GetCount-1 do begin
      Item := GetItems(i);
      case Item.Prop of
        ppID:              Result := Pic1.ID-Pic2.ID;
        ppFileName:        Result := AnsiCompareText(ExtractFileName(Pic1.FileName), ExtractFileName(Pic2.FileName));
        ppFullFileName:    Result := AnsiCompareText(Pic1.FileName, Pic2.FileName);
        ppFilePath:        Result := AnsiCompareText(ExtractFilePath(Pic1.FileName), ExtractFilePath(Pic2.FileName));
        ppFileSize,
          ppFileSizeBytes: Result := Pic1.FileSize-Pic2.FileSize;
        ppPicWidth:        Result := Pic1.ImageSize.cx-Pic2.ImageSize.cx;
        ppPicHeight:       Result := Pic1.ImageSize.cy-Pic2.ImageSize.cy;
        ppPicDims:         Result := (Pic1.ImageSize.cx*Pic1.ImageSize.cy)-(Pic2.ImageSize.cx*Pic2.ImageSize.cy);
        ppFormat:          Result := Byte(Pic1.ImageFormat)-Byte(Pic2.ImageFormat);
        ppDate:            Result := Pic1.Date-Pic2.Date;
        ppTime:            Result := Pic1.Time-Pic2.Time;
        ppPlace:           Result := AnsiCompareText(Pic1.Place,              Pic2.Place);
        ppFilmNumber:      Result := AnsiCompareText(Pic1.FilmNumber,         Pic2.FilmNumber);
        ppFrameNumber:     Result := AnsiCompareText(Pic1.FrameNumber,        Pic2.FrameNumber);
        ppAuthor:          Result := AnsiCompareText(Pic1.Author,             Pic2.Author);
        ppDescription:     Result := AnsiCompareText(Pic1.Description,        Pic2.Description);
        ppNotes:           Result := AnsiCompareText(Pic1.Notes,              Pic2.Notes);
        ppMedia:           Result := AnsiCompareText(Pic1.Media,              Pic2.Media);
        ppKeywords:        Result := AnsiCompareText(Pic1.Keywords.CommaText, Pic2.Keywords.CommaText);
        ppRotation:        Result := Ord(Pic1.Rotation)-Ord(Pic2.Rotation);
        ppFlips:           Result := Byte(Pic1.Flips)-Byte(Pic2.Flips);
      end;
      if Result<>0 then begin
        if Item.Direction=psdDesc then Result := -Result;
        Break;
      end;
    end;
  end;

  procedure TPhotoAlbumPicSortingList.StreamerLoad(Streamer: TPhoaStreamer);
  var
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
    Item: IPhotoAlbumPicSorting;
  begin
    Clear;
     // *** New format only
    if Streamer.Chunked then
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Sorting
          IPhChunk_ViewSorting_Open: begin
            Item := NewPhotoAlbumPicSorting;
            Item.StreamerLoad(Streamer);
            Add(Item);
          end;
           // Close-chunk
          IPhChunk_ViewSortings_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicSortingList.StreamerSave(Streamer: TPhoaStreamer);
  var i: Integer;
  begin
     // *** New format only
    if Streamer.Chunked then begin
      Streamer.WriteChunk(IPhChunk_ViewSortings_Open);
      for i := 0 to GetCount-1 do GetItemsX(i).StreamerSave(Streamer);
      Streamer.WriteChunk(IPhChunk_ViewSortings_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumPicGrouping
   //===================================================================================================================
type
  TPhotoAlbumPicGrouping = class(TInterfacedObject, IPhoaPicGrouping, IPhoaMutablePicGrouping, IPhotoAlbumPicGrouping)
  private
     // Prop storage
    FProp: TPicGroupByProperty;
    FUnclassifiedInOwnFolder: Boolean;
     // IPhoaPicGrouping
    function  GetProp: TPicGroupByProperty; stdcall;
    function  GetUnclassifiedInOwnFolder: Boolean; stdcall;
     // IPhoaMutablePicGrouping
    procedure Assign(Source: IPhoaPicGrouping); stdcall;
    procedure ToggleUnclassifiedInOwnFolder; stdcall;
    procedure SetProp(Value: TPicGroupByProperty); stdcall;
    procedure SetUnclassifiedInOwnFolder(Value: Boolean); stdcall;
     // IPhotoAlbumPicGrouping
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  end;

   // Группировка с нетипизированным свойством - для чтения из файла ревизии 2
  TPhoaRawGrouping = packed record
    bProp: Byte;
    bUnclassified: Boolean;
    wUnused: Word;
  end;

  procedure TPhotoAlbumPicGrouping.Assign(Source: IPhoaPicGrouping);
  begin
    FProp                    := Source.Prop;
    FUnclassifiedInOwnFolder := Source.UnclassifiedInOwnFolder;
  end;

  function TPhotoAlbumPicGrouping.GetProp: TPicGroupByProperty;
  begin
    Result := FProp;
  end;

  function TPhotoAlbumPicGrouping.GetUnclassifiedInOwnFolder: Boolean;
  begin
    Result := FUnclassifiedInOwnFolder;
  end;

  procedure TPhotoAlbumPicGrouping.SetProp(Value: TPicGroupByProperty);
  begin
    if Value in [Low(Value)..High(Value)] then FProp := Value;
  end;

  procedure TPhotoAlbumPicGrouping.SetUnclassifiedInOwnFolder(Value: Boolean);
  begin
    if Value in [Low(Value)..High(Value)] then FUnclassifiedInOwnFolder := Value;
  end;

  procedure TPhotoAlbumPicGrouping.StreamerLoad(Streamer: TPhoaStreamer);
  var
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
    RG: TPhoaRawGrouping;
  begin
     // Инициализируем данные
    FProp                    := Low(FProp);
    FUnclassifiedInOwnFolder := Low(FUnclassifiedInOwnFolder);
     // *** Old format
    if not Streamer.Chunked then begin
      Integer(RG) := Streamer.ReadInt;
      SetProp(aXlat_GBProp2ToGBProp[RG.bProp]);
      SetUnclassifiedInOwnFolder(RG.bUnclassified);
     // *** New format
    end else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
          IPhChunk_ViewGrouping_Prop:    SetProp(aXlat_ChunkGroupingPropToGBProperty[Integer(vValue)]);
          IPhChunk_ViewGrouping_Unclass: FUnclassifiedInOwnFolder := vValue<>Byte(0);
           // Close-chunk
          IPhChunk_ViewGrouping_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicGrouping.StreamerSave(Streamer: TPhoaStreamer);
  var RG: TPhoaRawGrouping;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
      FillChar(RG, SizeOf(RG), 0);
      RG.bProp         := aXlat_GBPropToGBProp2[FProp];
      RG.bUnclassified := FUnclassifiedInOwnFolder;
      Streamer.WriteInt(Integer(RG));
     // *** New format
    end else begin
      Streamer.WriteChunk(IPhChunk_ViewGrouping_Open);
      Streamer.WriteChunkWord(IPhChunk_ViewGrouping_Prop,    aXlat_GBPropertyToChunkGroupingProp[FProp]);
      Streamer.WriteChunkByte(IPhChunk_ViewGrouping_Unclass, Byte(FUnclassifiedInOwnFolder));
      Streamer.WriteChunk(IPhChunk_ViewGrouping_Close);
    end;
  end;

  procedure TPhotoAlbumPicGrouping.ToggleUnclassifiedInOwnFolder;
  begin
    FUnclassifiedInOwnFolder := not FUnclassifiedInOwnFolder;
  end;

   //===================================================================================================================
   // TPhotoAlbumPicGroupingList
   //===================================================================================================================
type
  TPhotoAlbumPicGroupingList = class(TInterfacedObject, IPhoaPicGroupingList, IPhoaMutablePicGroupingList, IPhotoAlbumPicGroupingList)
  private
     // Собственно список
    FList: IInterfaceList; 
     // IPhoaPicGroupingList
    function  IndexOfProp(Prop: TPicGroupByProperty): Integer; stdcall;
    function  SortComparePics(Pic1, Pic2: IPhoaPic): Integer; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaPicGrouping; stdcall;
     // IPhoaMutablePicGroupingList
    procedure Assign(Source: IPhoaPicGroupingList); stdcall;
    function  Add(Item: IPhoaPicGrouping): Integer; stdcall;
    procedure Insert(Index: Integer; Item: IPhoaPicGrouping); stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Clear; stdcall;
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutablePicGrouping; stdcall;
     // IPhotoAlbumPicGroupingList
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
    function  GetItemsX(Index: Integer): IPhotoAlbumPicGrouping;
  public
    constructor Create;
  end;

  function TPhotoAlbumPicGroupingList.Add(Item: IPhoaPicGrouping): Integer;
  begin
    Result := FList.Add(Item);
  end;

  procedure TPhotoAlbumPicGroupingList.Assign(Source: IPhoaPicGroupingList);
  var
    i: Integer;
    Grouping: IPhotoAlbumPicGrouping;
  begin
    Clear;
    for i := 0 to Source.Count-1 do begin
      Grouping := NewPhotoAlbumPicGrouping;
      Grouping.Assign(Source[i]);
      Add(Grouping);
    end;
  end;

  procedure TPhotoAlbumPicGroupingList.Clear;
  begin
    FList.Clear;
  end;

  constructor TPhotoAlbumPicGroupingList.Create;
  begin
    inherited Create;
    FList := TInterfaceList.Create;
  end;

  procedure TPhotoAlbumPicGroupingList.Delete(Index: Integer);
  begin
    FList.Delete(Index);
  end;

  function TPhotoAlbumPicGroupingList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhotoAlbumPicGroupingList.GetItems(Index: Integer): IPhoaPicGrouping;
  begin
    Result := IPhoaPicGrouping(FList[Index]);
  end;

  function TPhotoAlbumPicGroupingList.GetItemsM(Index: Integer): IPhoaMutablePicGrouping;
  begin
    Result := GetItems(Index) as IPhoaMutablePicGrouping;
  end;

  function TPhotoAlbumPicGroupingList.GetItemsX(Index: Integer): IPhotoAlbumPicGrouping;
  begin
    Result := GetItems(Index) as IPhotoAlbumPicGrouping;
  end;

  function TPhotoAlbumPicGroupingList.IndexOfProp(Prop: TPicGroupByProperty): Integer;
  begin
    for Result := 0 to GetCount-1 do
      if GetItems(Result).Prop=Prop then Exit;
    Result := -1;
  end;

  procedure TPhotoAlbumPicGroupingList.Insert(Index: Integer; Item: IPhoaPicGrouping);
  begin
    FList.Insert(Index, Item);
  end;

  procedure TPhotoAlbumPicGroupingList.Move(iCurIndex, iNewIndex: Integer);
  var Item: IPhoaPicGrouping;
  begin
    Item := GetItems(iCurIndex);
    FList.Delete(iCurIndex);
    FList.Insert(iNewIndex, Item);
  end;

  function TPhotoAlbumPicGroupingList.SortComparePics(Pic1, Pic2: IPhoaPic): Integer;
  var i: Integer;
  begin
    Result := 0;
    for i := 0 to GetCount-1 do begin
      case GetItems(i).Prop of
        gbpFilePath:    Result := AnsiCompareText(Pic1.PropStrValues[ppFilePath], Pic2.PropStrValues[ppFilePath]);
        gbpDateByYear:  Result := YearOf(PhoaDateToDate(Pic1.Date))-YearOf(PhoaDateToDate(Pic2.Date));
        gbpDateByMonth: Result := MonthOf(PhoaDateToDate(Pic1.Date))-MonthOf(PhoaDateToDate(Pic2.Date));
        gbpDateByDay:   Result := DayOf(PhoaDateToDate(Pic1.Date))-DayOf(PhoaDateToDate(Pic2.Date));
        gbpTimeHour:    Result := HourOf(PhoaTimeToTime(Pic1.Time))-HourOf(PhoaTimeToTime(Pic2.Time));
        gbpTimeMinute:  Result := MinuteOf(PhoaTimeToTime(Pic1.Time))-MinuteOf(PhoaTimeToTime(Pic2.Time));
        gbpPlace:       Result := AnsiCompareText(Pic1.Place,      Pic2.Place);
        gbpFilmNumber:  Result := AnsiCompareText(Pic1.FilmNumber, Pic2.FilmNumber);
        gbpAuthor:      Result := AnsiCompareText(Pic1.Author,     Pic2.Author);
        gbpMedia:       Result := AnsiCompareText(Pic1.Media,      Pic2.Media);
        gbpKeywords:    Result := 0; // По ключевым словам сортировка неприменима
      end;
      if Result<>0 then Break;
    end;
  end;

  procedure TPhotoAlbumPicGroupingList.StreamerLoad(Streamer: TPhoaStreamer);
  var
    i: Integer;
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;

    procedure LoadItem;
    var Item: IPhotoAlbumPicGrouping;
    begin
      Item := NewPhotoAlbumPicGrouping;
      Item.StreamerLoad(Streamer);
      Add(Item);
    end;

  begin
    Clear;
     // *** Old format
    if not Streamer.Chunked then
      for i := 0 to Streamer.ReadInt-1 do LoadItem
     // *** New format
    else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Sorting
          IPhChunk_ViewGrouping_Open: LoadItem;
           // Close-chunk
          IPhChunk_ViewGroupings_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumPicGroupingList.StreamerSave(Streamer: TPhoaStreamer);
  var
    i, iCount: Integer;
    Item: IPhotoAlbumPicGrouping;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
       // Count groupings compatible with rev. 2 format
      iCount := 0;
      for i := 0 to GetCount-1 do
        if GetItems(i).Prop in GBPropsRev2 then Inc(iCount);
      Streamer.WriteInt(iCount);
       // Write groupings compatible with rev. 2 format
      for i := 0 to GetCount-1 do begin
        Item := GetItemsX(i);
        if Item.Prop in GBPropsRev2 then Item.StreamerSave(Streamer);
      end;
     // *** New format
    end else begin
      Streamer.WriteChunk(IPhChunk_ViewGroupings_Open);
      for i := 0 to GetCount-1 do GetItemsX(i).StreamerSave(Streamer);
      Streamer.WriteChunk(IPhChunk_ViewGroupings_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumView
   //===================================================================================================================
type
  TPhotoAlbumView = class(TInterfacedObject, IPhoaView, IPhoaMutableView, IPhotoAlbumView)
  private
     // Prop storage
    FList: Pointer;
    FGroupings: IPhotoAlbumPicGroupingList; 
    FName: String;
    FRootGroup: IPhotoAlbumPicGroup;
    FSortings: IPhotoAlbumPicSortingList;
     // Создаёт иерархию групп по критериям представления
    procedure ProcessGroups;
     // IPhoaView
    procedure Invalidate; stdcall;
    function  GetGroupings: IPhoaPicGroupingList; stdcall;
    function  GetIndex: Integer; stdcall;
    function  GetList: IPhoaViewList; stdcall;
    function  GetName: String; stdcall;
    function  GetRootGroup: IPhoaPicGroup; stdcall;
    function  GetSortings: IPhoaPicSortingList; stdcall;
     // IPhoaMutableView
    procedure Assign(Source: IPhoaView); stdcall;
    function  GetGroupingsM: IPhoaMutablePicGroupingList; stdcall;
    function  GetListM: IPhoaMutableViewList; stdcall;
    procedure SetName(const Value: String); stdcall;
    function  GetSortingsM: IPhoaMutablePicSortingList; stdcall;
     // IPhotoAlbumView
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
    function  GetGroupingsX: IPhotoAlbumPicGroupingList;
    function  GetListX: IPhotoAlbumViewList;
    function  GetRootGroupX: IPhotoAlbumPicGroup;
    function  GetSortingsX: IPhotoAlbumPicSortingList;
  public
    constructor Create(AList: IPhotoAlbumViewList);
  end;

  function PhoaViewSortCompareFunc(Pic1, Pic2: IPhoaPic; dwData: Cardinal): Integer; stdcall;
  begin
     // Сначала сортируем по группировкам
    Result := TPhotoAlbumView(dwData).FGroupings.SortComparePics(Pic1, Pic2);
     // Потом по сортировкам
    if Result=0 then Result := TPhotoAlbumView(dwData).FSortings.SortComparePics(Pic1, Pic2);
  end;

  procedure TPhotoAlbumView.Assign(Source: IPhoaView);
  begin
    Invalidate;
    FName := Source.Name;
    FGroupings.Assign(Source.Groupings);
    FSortings.Assign(Source.Sortings);
  end;

  constructor TPhotoAlbumView.Create(AList: IPhotoAlbumViewList);
  begin
    inherited Create;
    FList := Pointer(AList);
    AList.Add(Self);
    FGroupings := NewPhotoAlbumPicGroupingList;
    FSortings  := NewPhotoAlbumPicSortingList;
  end;

  function TPhotoAlbumView.GetGroupings: IPhoaPicGroupingList;
  begin
    Result := FGroupings;
  end;

  function TPhotoAlbumView.GetGroupingsM: IPhoaMutablePicGroupingList;
  begin
    Result := FGroupings;
  end;

  function TPhotoAlbumView.GetGroupingsX: IPhotoAlbumPicGroupingList;
  begin
    Result := FGroupings;
  end;

  function TPhotoAlbumView.GetIndex: Integer;
  var
    pSelfIntf: Pointer;
    VList: IPhoaViewList;
  begin
    VList := GetList;
     // Ищем себя в списке-владельце (по указателю на интерфейс)
    pSelfIntf := Pointer(Self as IPhoaView);
    for Result := 0 to VList.Count-1 do
      if Pointer(VList[Result])=pSelfIntf then Exit;
    Result := -1;
  end;

  function TPhotoAlbumView.GetList: IPhoaViewList;
  begin
    Result := IPhoaViewList(FList);
  end;

  function TPhotoAlbumView.GetListM: IPhoaMutableViewList;
  begin
    Result := GetList as IPhoaMutableViewList;
  end;

  function TPhotoAlbumView.GetListX: IPhotoAlbumViewList;
  begin
    Result := GetList as IPhotoAlbumViewList;
  end;

  function TPhotoAlbumView.GetName: String;
  begin
    Result := FName;
  end;

  function TPhotoAlbumView.GetRootGroup: IPhoaPicGroup;
  begin
    Result := GetRootGroupX;
  end;

  function TPhotoAlbumView.GetRootGroupX: IPhotoAlbumPicGroup;
  begin
     // Если ещё не наполняли список, делаем это
    if FRootGroup=nil then ProcessGroups;
    Result := FRootGroup;
  end;

  function TPhotoAlbumView.GetSortings: IPhoaPicSortingList;
  begin
    Result := FSortings;
  end;

  function TPhotoAlbumView.GetSortingsM: IPhoaMutablePicSortingList;
  begin
    Result := FSortings;
  end;

  function TPhotoAlbumView.GetSortingsX: IPhotoAlbumPicSortingList;
  begin
    Result := FSortings;
  end;

  procedure TPhotoAlbumView.Invalidate;
  begin
    FRootGroup := nil;
  end;

  procedure TPhotoAlbumView.ProcessGroups;
  var
    iGpg, iGrp, iPic: Integer;
    Gpg: IPhotoAlbumPicGrouping;
    Grp, GUnclassified: IPhotoAlbumPicGroup;
    Pic: IPhotoAlbumPic;
    GroupsWithPics: IPhotoAlbumPicGroupList;
    bClassified: Boolean;

     // Помещает сортированный список изображений фотоальбома в корневую группу
    procedure FillRootGroup;
    begin
      FRootGroup.PicsX.Clear;
      FRootGroup.PicsX.Add(GetList.Pics, False);
      FRootGroup.PicsX.CustomSort(PhoaViewSortCompareFunc, Cardinal(Self));
    end;

     // Создаёт дерево по пути к изображению
    procedure ProcessFilePathTree(ParentGroup: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
    var
      sDir, sOneDir: String;
      Group, GParent: IPhotoAlbumPicGroup;
    begin
      sDir := ExtractFileDir(Pic.FileName);
       // Начинаем с корня
      Group := ParentGroup;
      repeat
         // Выделяем первый каталог из пути и удаляем его из sDir
        sOneDir := ExtractFirstWord(sDir, '\');
         // Если корневой каталог, добавляем слэш в конце
        if (Length(sOneDir)=2) and (sOneDir[2]=':') then sOneDir := sOneDir+'\';
         // Получаем последнего ребёнка текущей группы
        if Group.Groups.Count=0 then GParent := nil else GParent := Group.GroupsX[Group.Groups.Count-1];
         // Если нет детей или последний ребёнок не совпадает по наименованию, создаём нового ребёнка
        if (GParent=nil) or not AnsiSameText(GParent.Text, sOneDir) then begin
          GParent := NewPhotoAlbumPicGroup(Group, 0);
          GParent.Text     := sOneDir;
          GParent.Expanded := True;
        end;
        Group := GParent;
      until sDir='';
      Group.PicsX.Add(Pic, True);
    end;

     // Создаёт дерево по компонентам даты (one level)
    procedure ProcessDateTree(Prop: TPicGroupByProperty; ParentGroup: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
    var
      sDatePart: String;
      Group: IPhotoAlbumPicGroup;
    begin
       // Находим наименование компонента даты
      case Prop of
        gbpDateByYear:  sDatePart := 'yyyy';
        gbpDateByMonth: sDatePart := 'mmmm';
        gbpDateByDay:   sDatePart := 'd';
      end;
      sDatePart := FormatDateTime(sDatePart, PhoaDateToDate(Pic.Date));
       // Получаем последнего ребёнка текущей группы
      if ParentGroup.Groups.Count=0 then Group := nil else Group := ParentGroup.GroupsX[ParentGroup.Groups.Count-1];
       // Если нет детей или последний ребёнок не совпадает по наименованию, создаём нового ребёнка
      if (Group=nil) or not AnsiSameText(Group.Text, sDatePart) then begin
        Group := NewPhotoAlbumPicGroup(ParentGroup, 0);
        Group.Text     := sDatePart;
        Group.Expanded := True;
      end;
      Group.PicsX.Add(Pic, True);
    end;

     // Создаёт дерево по компонентам времени (one level)
    procedure ProcessTimeTree(Prop: TPicGroupByProperty; ParentGroup: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
    var
      sTimePart: String;
      Group: IPhotoAlbumPicGroup;
    begin
       // Находим наименование компонента времени
      sTimePart := FormatDateTime(iif(Prop=gbpTimeHour, 'h', 'n'), PhoaTimeToTime(Pic.Time));
       // Получаем последнего ребёнка текущей группы
      if ParentGroup.Groups.Count=0 then Group := nil else Group := ParentGroup.GroupsX[ParentGroup.Groups.Count-1];
       // Если нет детей или последний ребёнок не совпадает по наименованию, создаём нового ребёнка
      if (Group=nil) or not AnsiSameText(Group.Text, sTimePart) then begin
        Group := NewPhotoAlbumPicGroup(ParentGroup, 0);
        Group.Text     := sTimePart;
        Group.Expanded := True;
      end;
      Group.PicsX.Add(Pic, True);
    end;

     // Создаёт дерево по простому строковому свойству (one level)
    procedure ProcessPlainPropTree(PicProp: TPicProperty; ParentGroup: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
    var
      Group: IPhotoAlbumPicGroup;
      sPropVal: String;
    begin
       // Получаем последнего ребёнка группы
      if ParentGroup.Groups.Count=0 then Group := nil else Group := ParentGroup.GroupsX[ParentGroup.Groups.Count-1];
       // Если нет детей или последний ребёнок не совпадает по наименованию, создаём нового ребёнка
      sPropVal := Pic.PropStrValues[PicProp];
      if (Group=nil) or not AnsiSameText(Group.Text, sPropVal) then begin
        Group := NewPhotoAlbumPicGroup(ParentGroup, 0);
        Group.Text := sPropVal;
      end;
       // Добавляем изображение к группе
      Group.PicsX.Add(Pic, True);
    end;

     // Создаёт дерево по ключевым словам (one level)
    procedure ProcessKeywordTree(ParentGroup: IPhotoAlbumPicGroup; Pic: IPhotoAlbumPic);
    var ikw: Integer;

       // Ищет группу для ключевого слова, если нет такой - создаёт; при этом сохраняет сортированную последовательность
       //   групп
      function GetKWGroup(const sKW: String): IPhotoAlbumPicGroup;
      var
        i, idx: Integer;
        G: IPhotoAlbumPicGroup;
      begin
         // Цикл по детям
        Result := nil;
        idx := -1;
        for i := 0 to ParentGroup.Groups.Count-1 do begin
          G := ParentGroup.GroupsX[i];
           // Пропускаем группу "Без ключевых слов"
          if G<>GUnclassified then
            case AnsiCompareText(G.Text, sKW) of
               // Ещё не достигли группы
              Low(Integer)..-1: ;
               // Нашли соответствие
              0: begin
                Result := G;
                Break;
              end;
               // Проскочили мимо - надо создать
              else begin
                idx := i;
                Break;
              end;
            end;
        end;
         // Создаём группу
        if Result=nil then begin
          Result := NewPhotoAlbumPicGroup(ParentGroup, 0);
          Result.Text := sKW;
           // Если нужно вставить перед найденной группой - передвигаем
          if idx>=0 then Result.Index := idx;
        end;
      end;

    begin
       // Сканируем для каждого ключевого слова
      for ikw := 0 to Pic.Keywords.Count-1 do GetKWGroup(Pic.Keywords[ikw]).PicsX.Add(Pic, True);
    end;

     // Рекурсивная процедура наполнения списка группами, содержащими изображения
    procedure MakeListOfGroupsWithPics(List: IPhotoAlbumPicGroupList; Group: IPhotoAlbumPicGroup);
    var i: Integer;
    begin
      if Group.Pics.Count>0 then List.Add(Group);
      for i := 0 to Group.Groups.Count-1 do MakeListOfGroupsWithPics(List, Group.GroupsX[i]);
    end;

  begin
     // Создаём корневую или стираем подчинённые группы по необходимости
    if FRootGroup=nil then FRootGroup := NewPhotoAlbumPicGroup(nil, 1) else FRootGroup.GroupsX.Clear;
     // Помещаем сортированный по группировкам представления список всех изображений фотоальбома в корневую группу
    FillRootGroup;
     // Создаём иерархию групп - применяем последовательно все группировки
    GroupsWithPics := NewPhotoAlbumPicGroupList(nil);
    for iGpg := 0 to FGroupings.Count-1 do begin
      Gpg := FGroupings[iGpg];
       // Создаём список групп, содержащих изображения
      GroupsWithPics.Clear;
      MakeListOfGroupsWithPics(GroupsWithPics, FRootGroup);
       // Применяем к этим группам группировку
      for iGrp := 0 to GroupsWithPics.Count-1 do begin
        Grp := GroupsWithPics[iGrp];
        GUnclassified := nil;
         // Цикл по всем изображениям группы
        iPic := 0;
        while iPic<Grp.Pics.Count do begin
          Pic := Grp.PicsX[iPic];
           // Проверяем, классифицировано ли изображение
          case Gpg.Prop of
            gbpFilePath:       bClassified := True;
            gbpDateByYear,
              gbpDateByMonth,
              gbpDateByDay:    bClassified := Pic.Date>0;
            gbpTimeHour,
              gbpTimeMinute:   bClassified := Pic.Time>0;
            gbpPlace:          bClassified := Pic.Place<>'';
            gbpFilmNumber:     bClassified := Pic.FilmNumber<>'';
            gbpAuthor:         bClassified := Pic.Author<>'';
            gbpMedia:          bClassified := Pic.Media<>'';
            else {gbpKeywords} bClassified := Pic.Keywords.Count>0;
          end;
           // Если классифицировано - помещаем в иерархию групп
          if bClassified then
            case Gpg.Prop of
              gbpFilePath:     ProcessFilePathTree(Grp, Pic);
              gbpDateByYear,
                gbpDateByMonth,
                gbpDateByDay:  ProcessDateTree(Gpg.Prop, Grp, Pic);
              gbpTimeHour,
                gbpTimeMinute: ProcessTimeTree(Gpg.Prop, Grp, Pic);
              gbpPlace:        ProcessPlainPropTree(ppPlace,      Grp, Pic);
              gbpFilmNumber:   ProcessPlainPropTree(ppFilmNumber, Grp, Pic);
              gbpAuthor:       ProcessPlainPropTree(ppAuthor,     Grp, Pic);
              gbpMedia:        ProcessPlainPropTree(ppMedia,      Grp, Pic);
              gbpKeywords:     ProcessKeywordTree(Grp, Pic);
            end
           // Если не классифицировано и группировка требует помещения таких изображений в отдельную папку, создаём
           //   папку при необходимости и помещаем туда изображение
          else if Gpg.UnclassifiedInOwnFolder then begin
            if GUnclassified=nil then begin
              GUnclassified := NewPhotoAlbumPicGroup(Grp, 0);
              GUnclassified.Index := 0;
              GUnclassified.Text := ConstVal(asUnclassifiedConsts[Gpg.Prop]);
            end;
            GUnclassified.PicsX.Add(Pic, True);
           // Иначе - переходим к следующему изображению
          end else begin
            Inc(iPic);
            Continue;
          end;
           // Удаляем изображение из старой группы
          Grp.PicsX.Remove(Pic.ID);
        end;
      end;
    end;
     // Распределяем группам уникальные ID
    FRootGroup.FixupIDs;
  end;

  procedure TPhotoAlbumView.SetName(const Value: String);
  var SelfRef: IPhotoAlbumView;
  begin
    if FName<>Value then begin
      FName := Value;
       // После смены имени вставляем себя в список-владелец заново, чтобы список остался сортированным
      SelfRef := Self;
      GetListX.Delete(GetIndex);
      GetListX.Add(SelfRef);
    end;
  end;

  procedure TPhotoAlbumView.StreamerLoad(Streamer: TPhoaStreamer);
  var
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
       // Читаем наименование - устанавливаем сеттером, т.к. при изменении имени должна измениться позиция представления
       //   в списке-владельце
      SetName(Streamer.ReadStringI);
       // Read groupings
      FGroupings.StreamerLoad(Streamer);
     // *** New format
    end else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // Name - устанавливаем сеттером, т.к. при изменении имени должна измениться позиция представления в
           //   списке-владельце
          IPhChunk_View_Name: SetName(vValue);
           // Groupings
          IPhChunk_ViewGroupings_Open: FGroupings.StreamerLoad(Streamer);
           // Sortings
          IPhChunk_ViewSortings_Open: FSortings.StreamerLoad(Streamer);
           // Close-chunk
          IPhChunk_View_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumView.StreamerSave(Streamer: TPhoaStreamer);
  begin
     // *** Old format
    if not Streamer.Chunked then begin
       // Write name
      Streamer.WriteStringI(FName);
       // Write groupings
      FGroupings.StreamerSave(Streamer);
     // *** New format
    end else begin
       // Write close-chunk
      Streamer.WriteChunk(IPhChunk_View_Open);
       // Write name
      Streamer.WriteChunkString(IPhChunk_View_Name, FName);
       // Write groupings/sortings
      FGroupings.StreamerSave(Streamer);
      FSortings.StreamerSave(Streamer);
       // Write close-chunk
      Streamer.WriteChunk(IPhChunk_View_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumViewList
   //===================================================================================================================
type
  TPhotoAlbumViewList = class(TInterfacedObject, IPhoaViewList, IPhoaMutableViewList, IPhotoAlbumViewList)
  private
     // Собственно список
    FList: IInterfaceList;
     // Prop storage
    FPics: IPhoaPicList;
     // IPhoaViewList
    function  FindName(const sName: String; var Index: Integer): Boolean; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaView; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
    function  IndexOfName(const sName: String): Integer; stdcall;
    procedure Invalidate; stdcall;
     // IPhoaMutableViewList
    function  Add(Item: IPhoaView): Integer; stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutableView; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
    procedure Assign(Source: IPhoaViewList); stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
     // IPhotoAlbumViewList
    function  GetItemsX(Index: Integer): IPhotoAlbumView;
    function  GetPicsX: IPhotoAlbumPicList;
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer);
  public
    constructor Create(APics: IPhoaPicList);
  end;

  function TPhotoAlbumViewList.Add(Item: IPhoaView): Integer;
  begin
     // Ищем место вставки
    FindName(Item.Name, Result);
     // Вставляем
    FList.Insert(Result, Item);
  end;

  procedure TPhotoAlbumViewList.Assign(Source: IPhoaViewList);
  var i: Integer;
  begin
    Clear;
    for i := 0 to Source.Count-1 do NewPhotoAlbumView(Self).Assign(Source[i]);
  end;

  procedure TPhotoAlbumViewList.Clear;
  begin
    FList.Clear;
  end;

  constructor TPhotoAlbumViewList.Create(APics: IPhoaPicList);
  begin
    inherited Create;
    FList := TInterfaceList.Create;
    FPics := APics;
  end;

  procedure TPhotoAlbumViewList.Delete(Index: Integer);
  begin
    FList.Delete(Index);
  end;

  function TPhotoAlbumViewList.FindName(const sName: String; var Index: Integer): Boolean;
  var i1, i2, i, iCompare: Integer;
  begin
    Result := False;
     // Т.к. список сортированный - ищем с помощью бинарного поиска
    i1 := 0;
    i2 := FList.Count-1;
    while i1<=i2 do begin
      i := (i1+i2) shr 1;
      iCompare := AnsiCompareText(GetItems(i).Name, sName);
      if iCompare<0 then
        i1 := i+1
      else begin
        i2 := i-1;
        if iCompare=0 then begin
          Result := True;
          i1 := i;
        end;
      end;
    end;
    Index := i1;
  end;

  function TPhotoAlbumViewList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhotoAlbumViewList.GetItems(Index: Integer): IPhoaView;
  begin
    Result := IPhoaView(FList[Index]);
  end;

  function TPhotoAlbumViewList.GetItemsM(Index: Integer): IPhoaMutableView;
  begin
    Result := GetItems(Index) as IPhoaMutableView;
  end;

  function TPhotoAlbumViewList.GetItemsX(Index: Integer): IPhotoAlbumView;
  begin
    Result := GetItems(Index) as IPhotoAlbumView;
  end;

  function TPhotoAlbumViewList.GetPics: IPhoaPicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumViewList.GetPicsM: IPhoaMutablePicList;
  begin
    Result := FPics as IPhoaMutablePicList;
  end;

  function TPhotoAlbumViewList.GetPicsX: IPhotoAlbumPicList;
  begin
    Result := FPics as IPhotoAlbumPicList;
  end;

  function TPhotoAlbumViewList.IndexOfName(const sName: String): Integer;
  begin
    if not FindName(sName, Result) then Result := -1; 
  end;

  procedure TPhotoAlbumViewList.Invalidate;
  var i: Integer;
  begin
    for i := 0 to GetCount-1 do GetItems(i).Invalidate;
  end;

  procedure TPhotoAlbumViewList.StreamerLoad(Streamer: TPhoaStreamer);
  var
    i: Integer;
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
    Clear;
     // *** Old format
    if not Streamer.Chunked then
       // Read views
      for i := 0 to Streamer.ReadInt-1 do NewPhotoAlbumView(Self).StreamerLoad(Streamer)
     // *** New format
    else
      while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
        case Code of
           // View
          IPhChunk_View_Open: NewPhotoAlbumView(Self).StreamerLoad(Streamer);
           // Close-chunk
          IPhChunk_Views_Close: Break;
           // Ensure unknown nested structures are skipped whole
          else Streamer.SkipNestedChunks(Code);
        end;
  end;

  procedure TPhotoAlbumViewList.StreamerSave(Streamer: TPhoaStreamer);
  var i: Integer;
  begin
     // *** Old format
    if not Streamer.Chunked then begin
      Streamer.WriteInt(GetCount);
       // Write views
      for i := 0 to GetCount-1 do GetItemsX(i).StreamerSave(Streamer);
     // *** New format
    end else begin
      Streamer.WriteChunk(IPhChunk_Views_Open);
       // Write views
      for i := 0 to GetCount-1 do GetItemsX(i).StreamerSave(Streamer);
      Streamer.WriteChunk(IPhChunk_Views_Close);
    end;
  end;

   //===================================================================================================================
   // TPhotoAlbumProject
   //===================================================================================================================
type
  TPhotoAlbumProject = class(TInterfacedObject, IPhoaProject, IPhoaMutableProject, IPhotoAlbumProject)
  private
     // Prop storage
    FDescription: String;
    FFileName: String;
    FFileRevision: Integer;
    FPics: IPhotoAlbumPicList; 
    FRootGroup: IPhotoAlbumPicGroup;
    FThumbnailQuality: Byte;
    FThumbnailSize: TSize;
    FViewIndex: Integer;
    FViews: IPhotoAlbumViewList; 
     // IPhoaProject
    function  GetCurrentView: IPhoaView; stdcall;
    function  GetDescription: String; stdcall;
    function  GetFileName: String; stdcall;
    function  GetFileRevision: Integer; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
    function  GetRootGroup: IPhoaPicGroup; stdcall;
    function  GetThumbnailQuality: Byte; stdcall;
    function  GetThumbnailSize: TSize; stdcall;
    function  GetViewIndex: Integer; stdcall;
    function  GetViewRootGroup: IPhoaPicGroup; stdcall;
    function  GetViews: IPhoaViewList; stdcall;
     // IPhoaMutableProject
    function  GetCurrentViewM: IPhoaMutableView; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
    function  GetRootGroupM: IPhoaMutablePicGroup; stdcall;
    function  GetViewRootGroupM: IPhoaMutablePicGroup; stdcall;
    function  GetViewsM: IPhoaMutableViewList; stdcall;
    procedure Assign(Source: IPhoaProject; bCopyRevision: Boolean); stdcall;
    procedure LoadFromFile(const sFileName: String); stdcall;
    procedure New; stdcall;
    procedure SaveToFile(const sFileName, sGenerator, sRemark: String; iRevisionNumber: Integer); stdcall;
    procedure SetDescription(const Value: String); stdcall;
    procedure SetFileName(const Value: String); stdcall;
    procedure SetThumbnailQuality(Value: Byte); stdcall;
    procedure SetThumbnailSize(const Value: TSize); stdcall;
    procedure SetViewIndex(Value: Integer); stdcall;
     // IPhotoAlbumProject
    function  GetCurrentViewX: IPhotoAlbumView;
    function  GetPicsX: IPhotoAlbumPicList;
    function  GetRootGroupX: IPhotoAlbumPicGroup;
    function  GetViewRootGroupX: IPhotoAlbumPicGroup;
    function  GetViewsX: IPhotoAlbumViewList;
    procedure StreamerLoad(Streamer: TPhoaStreamer);
    procedure StreamerSave(Streamer: TPhoaStreamer; const sGenerator, sRemark: String);
  public
    constructor Create;
  end;

  procedure TPhotoAlbumProject.Assign(Source: IPhoaProject; bCopyRevision: Boolean);
  begin
    FFileName         := Source.FileName;
    FDescription      := Source.Description;
    FThumbnailQuality := Source.ThumbnailQuality;
    FThumbnailSize    := Source.ThumbnailSize;
    if bCopyRevision then FFileRevision := Source.FileRevision;
  end;

  constructor TPhotoAlbumProject.Create;
  begin
    inherited Create;
    New;
  end;

  function TPhotoAlbumProject.GetCurrentView: IPhoaView;
  var idx: Integer;
  begin
    idx := GetViewIndex;
    if idx<0 then Result := nil else Result := FViews[idx];
  end;

  function TPhotoAlbumProject.GetCurrentViewM: IPhoaMutableView;
  begin
    Result := GetCurrentView as IPhoaMutableView;
  end;

  function TPhotoAlbumProject.GetCurrentViewX: IPhotoAlbumView;
  begin
    Result := GetCurrentView as IPhotoAlbumView;
  end;

  function TPhotoAlbumProject.GetDescription: String;
  begin
    Result := FDescription;
  end;

  function TPhotoAlbumProject.GetFileName: String;
  begin
    Result := FFileName;
  end;

  function TPhotoAlbumProject.GetFileRevision: Integer;
  begin
    Result := FFileRevision;
  end;

  function TPhotoAlbumProject.GetPics: IPhoaPicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumProject.GetPicsM: IPhoaMutablePicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumProject.GetPicsX: IPhotoAlbumPicList;
  begin
    Result := FPics;
  end;

  function TPhotoAlbumProject.GetRootGroup: IPhoaPicGroup;
  begin
    Result := FRootGroup;
  end;

  function TPhotoAlbumProject.GetRootGroupM: IPhoaMutablePicGroup;
  begin
    Result := FRootGroup;
  end;

  function TPhotoAlbumProject.GetRootGroupX: IPhotoAlbumPicGroup;
  begin
    Result := FRootGroup;
  end;

  function TPhotoAlbumProject.GetThumbnailQuality: Byte;
  begin
    Result := FThumbnailQuality;
  end;

  function TPhotoAlbumProject.GetThumbnailSize: TSize;
  begin
    Result := FThumbnailSize;
  end;

  function TPhotoAlbumProject.GetViewIndex: Integer;
  begin
     // Validate index
    if FViewIndex>=FViews.Count then FViewIndex := FViews.Count-1; 
    Result := FViewIndex;
  end;

  function TPhotoAlbumProject.GetViewRootGroup: IPhoaPicGroup;
  var idx: Integer;
  begin
    idx := GetViewIndex;
    if idx<0 then Result := FRootGroup else Result := FViews[idx].RootGroup;
  end;

  function TPhotoAlbumProject.GetViewRootGroupM: IPhoaMutablePicGroup;
  begin
    Result := GetViewRootGroup as IPhoaMutablePicGroup;
  end;

  function TPhotoAlbumProject.GetViewRootGroupX: IPhotoAlbumPicGroup;
  begin
    Result := GetViewRootGroup as IPhotoAlbumPicGroup;
  end;

  function TPhotoAlbumProject.GetViews: IPhoaViewList;
  begin
    Result := FViews;
  end;

  function TPhotoAlbumProject.GetViewsM: IPhoaMutableViewList;
  begin
    Result := FViews;
  end;

  function TPhotoAlbumProject.GetViewsX: IPhotoAlbumViewList;
  begin
    Result := FViews;
  end;

  procedure TPhotoAlbumProject.LoadFromFile(const sFileName: String);
  var Streamer: TPhoaStreamer;
  begin
    New;
    try
       // Создаём FilerEx и загружаем с его помощью
      Streamer := TPhoaFilerEx.Create(psmRead, sFileName);
      try
        Streamer.ReadHeader;
        StreamerLoad(Streamer);
         // Применяем новое имя файла и ревизию
        FFileName     := sFileName;
        FFileRevision := Streamer.RevisionNumber;
      finally
        Streamer.Free;
      end;
    except
      New;
      raise;
    end;
  end;

  procedure TPhotoAlbumProject.New;
  begin
    FPics             := NewPhotoAlbumPicList(True);
    FRootGroup        := NewPhotoAlbumPicGroup(nil, 1);
    FViews            := NewPhotoAlbumViewList(FPics);
    FFileRevision     := IPhFileRevisionNumber;
    FDescription      := '';
    FFileName         := '';
    FThumbnailQuality := IThumbQuality_Default;
    FThumbnailSize.cx := IThumbWidth_Default;
    FThumbnailSize.cy := IThumbHeight_Default;
    FViewIndex        := -1;
  end;

  procedure TPhotoAlbumProject.SaveToFile(const sFileName, sGenerator, sRemark: String; iRevisionNumber: Integer);
  var Streamer: TPhoaStreamer;
  begin
     // Создаём FilerEx и сохраняем с его помощью
    Streamer := TPhoaFilerEx.Create(psmWrite, sFileName);
    try
      Streamer.RevisionNumber := iRevisionNumber;
      Streamer.WriteHeader;
      StreamerSave(Streamer, sGenerator, sRemark);
    finally
      Streamer.Free;
    end;
     // Применяем новое имя файла и ревизию
    FFileName     := sFileName;
    FFileRevision := iRevisionNumber;
  end;

  procedure TPhotoAlbumProject.SetDescription(const Value: String);
  begin
    FDescription := Value;
  end;

  procedure TPhotoAlbumProject.SetFileName(const Value: String);
  begin
    FFileName := Value;
  end;

  procedure TPhotoAlbumProject.SetThumbnailQuality(Value: Byte);
  begin
     // Validate value
    if Value>IThumbQuality_Max then Value := IThumbQuality_Max;
     // Assign valid value
    FThumbnailQuality := Value;
  end;

  procedure TPhotoAlbumProject.SetThumbnailSize(const Value: TSize);
  begin
    FThumbnailSize := Value;
     // Validate width
    if FThumbnailSize.cx<IThumbWidth_Min then FThumbnailSize.cx := IThumbWidth_Min
    else if FThumbnailSize.cx>IThumbWidth_Max then FThumbnailSize.cx := IThumbWidth_Max;
     // Validate height
    if FThumbnailSize.cy<IThumbHeight_Min then FThumbnailSize.cy := IThumbHeight_Min
    else if FThumbnailSize.cy>IThumbHeight_Max then FThumbnailSize.cy := IThumbHeight_Max;
  end;

  procedure TPhotoAlbumProject.SetViewIndex(Value: Integer);
  begin
    FViewIndex := Value;
  end;

  procedure TPhotoAlbumProject.StreamerLoad(Streamer: TPhoaStreamer);
  var
    Code: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
    Sz: TSize;
  begin
    try
       // *** Old format
      if not Streamer.Chunked then begin
         // Read photo album properties
        with Streamer do begin
          FDescription := ReadStringI;
          SetThumbnailQuality(ReadInt);
          Sz.cx        := ReadInt;
          Sz.cy        := ReadInt;
          SetThumbnailSize(Sz);
        end;
         // Read groups
        FRootGroup.StreamerLoad(Streamer);
         // Read pictures
        FPics.StreamerLoad(Streamer);
         // If revision 2+, read views
        if Streamer.RevisionNumber>=2 then FViews.StreamerLoad(Streamer);
       // *** New format
      end else
        while Streamer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do
          case Code of
             // Photo album properties
            IPhChunk_PhoaDescription:  FDescription      := vValue;
            IPhChunk_PhoaThumbQuality: SetThumbnailQuality(vValue);
            IPhChunk_PhoaThumbWidth:   SetThumbnailSize(Size(vValue, FThumbnailSize.cy));
            IPhChunk_PhoaThumbHeight:  SetThumbnailSize(Size(FThumbnailSize.cx, vValue));
             // Pictures
            IPhChunk_Pics_Open:        FPics.StreamerLoad(Streamer);
             // Root group
            IPhChunk_Group_Open:       FRootGroup.StreamerLoad(Streamer);
             // Views
            IPhChunk_Views_Open:       FViews.StreamerLoad(Streamer);
             // Ensure unknown nested structures are skipped whole
            else Streamer.SkipNestedChunks(Code);
          end;
    finally
       // Завершаем загрузку для иерархии групп
      FRootGroup.Loaded(Self);
    end;
  end;

  procedure TPhotoAlbumProject.StreamerSave(Streamer: TPhoaStreamer; const sGenerator, sRemark: String);
  begin
     // *** Old format
    if not Streamer.Chunked then begin
       // Write photo album properties
      with Streamer do begin
        WriteStringI(FDescription);
        WriteInt(FThumbnailQuality);
        WriteInt(FThumbnailSize.cx);
        WriteInt(FThumbnailSize.cy);
      end;
       // Write groups
      FRootGroup.StreamerSave(Streamer);
       // Write pictures
      FPics.StreamerSave(Streamer);
       // If revision 2+, write views
      if Streamer.RevisionNumber>=2 then FViews.StreamerSave(Streamer);
     // *** New format
    end else begin
      with Streamer do begin
         // Write photo album 'metadata'
        WriteChunkString(IPhChunk_Remark,           sRemark);
         // Write photo album properties
        WriteChunkString(IPhChunk_PhoaGenerator,    sGenerator);
        WriteChunkInt   (IPhChunk_PhoaSavedDate,    DateToPhoaDate(Date));
        WriteChunkInt   (IPhChunk_PhoaSavedTime,    TimeToPhoaTime(Time));
        WriteChunkString(IPhChunk_PhoaDescription,  FDescription);
        WriteChunkByte  (IPhChunk_PhoaThumbQuality, FThumbnailQuality);
        WriteChunkWord  (IPhChunk_PhoaThumbWidth,   FThumbnailSize.cx);
        WriteChunkWord  (IPhChunk_PhoaThumbHeight,  FThumbnailSize.cy);
      end;
       // Write pictures
      FPics.StreamerSave(Streamer);
       // Write groups
      FRootGroup.StreamerSave(Streamer);
       // Write views
      FViews.StreamerSave(Streamer);
    end;
  end;













   //===================================================================================================================
   // TIntegerList
   //===================================================================================================================

  function TIntegerList.Add(i: Integer): Boolean;
  begin
    Result := FAllowDuplicates or (IndexOf(i)<0);
    if Result then inherited Add(Pointer(i));
  end;

  constructor TIntegerList.Create(bAllowDuplicates: Boolean);
  begin
    inherited Create;
    FAllowDuplicates := bAllowDuplicates;
  end;

  function TIntegerList.GetItems(Index: Integer): Integer;
  begin
    Result := Integer(inherited Items[Index]);
  end;

  function TIntegerList.IndexOf(i: Integer): Integer;
  begin
    Result := inherited IndexOf(Pointer(i));
  end;

  function  TIntegerList.Insert(Index, i: Integer): Boolean;
  begin
    Result := FAllowDuplicates or (IndexOf(i)<0);
    if Result then inherited Insert(Index, Pointer(i));
  end;

  function TIntegerList.Remove(i: Integer): Integer;
  begin
    Result := inherited Remove(Pointer(i));
  end;

   //===================================================================================================================
   // TPhoaFilerEx
   //===================================================================================================================

  procedure TPhoaFilerEx.ValidateRevision;
  begin
     // Не допускаем считывания только более новых ревизий
    if RevisionNumber>IPhFileRevisionNumber then PhoaException(ConstVal('SErrFileRevHigher'), []);
  end;

   //===================================================================================================================
   // TFileList
   //===================================================================================================================

  function TFileList.Add(const _sName, _sPath: String; _iSize, _iIconIndex: Integer; const _dModified: TDateTime): Integer;
  var
    p: PFileRec;
    FileInfo: TSHFileInfo;
  begin
     // Ищем такой же файл
    Result := IndexOf(_sName, _sPath);
     // Не нашли, добавляем запись
    if Result<0 then begin
      New(p);
      Result := inherited Add(p);
      p^.sName    := _sName;
      p^.sPath    := _sPath;
      p^.bChecked := True;
     // Нашли, получаем указатель 
    end else
      p := GetItems(Result);
     // Проверяем индекс иконки. Получаем, если надо
    if _iIconIndex=-2 then begin
      FSysImageListHandle := SHGetFileInfo(PAnsiChar(IncludeTrailingPathDelimiter(_sPath)+_sName), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
      _iIconIndex := FileInfo.iIcon;
    end;
     // Сохраняем данные
    with p^ do begin
      iSize      := _iSize;
      iIconIndex := _iIconIndex;
      dModified  := _dModified;
    end;
  end;

  procedure TFileList.DeleteUnchecked;
  var i: Integer;
  begin
    for i := Count-1 downto 0 do
      if not GetItems(i)^.bChecked then Delete(i);
  end;

  function TFileList.GetFiles(Index: Integer): String;
  begin
    with GetItems(Index)^ do Result := sPath+sName;
  end;

  function TFileList.GetItems(Index: Integer): PFileRec;
  begin
    Result := PFileRec(inherited Items[Index]);
  end;

  function TFileList.IndexOf(const _sName, _sPath: String): Integer;
  begin
    for Result := 0 to Count-1 do
      with GetItems(Result)^ do
        if AnsiSameText(sName, _sName) and AnsiSameText(sPath, _sPath) then Exit;
    Result := -1;
  end;

  procedure TFileList.InternalQuickSort(iL, iR: Integer; Prop: TFileListSortProperty; Direction: TPhoaSortDirection);
  var
    i1, i2: Integer;
    p: PFileRec;

    function DoCompare(p1, p2: PFileRec; Prop: TFileListSortProperty; bFurtherSort: Boolean): Integer;
    begin
      case Prop of
        flspName:       Result := AnsiCompareText(p1.sName, p2.sName);
        flspPath:       Result := AnsiCompareText(p1.sPath, p2.sPath);
        flspSize:       Result := p1.iSize-p2.iSize;
        else {flspDate} Result := Sign(p1.dModified-p2.dModified);
      end;
       // Для одноимённых файлов сортируем по пути, для остальных совпадающих сортируем по имени
      if bFurtherSort and (Result=0) then
        if Prop=flspName then Result := DoCompare(p1, p2, flspPath, False) else Result := DoCompare(p1, p2, flspName, False);
      if Direction=psdDesc then Result := -Result;
    end;

  begin
    repeat
      i1 := iL;
      i2 := iR;
      p := GetItems((iL+iR) shr 1);
      repeat
        while DoCompare(GetItems(i1), p, Prop, True)<0 do Inc(i1);
        while DoCompare(GetItems(i2), p, Prop, True)>0 do Dec(i2);
        if i1<=i2 then begin
          Exchange(i1, i2);
          Inc(i1);
          Dec(i2);
        end;
      until i1>i2;
      if iL<i2 then InternalQuickSort(iL, i2, Prop, Direction);
      iL := i1;
    until i1>=iR;
  end;

  procedure TFileList.Notify(Ptr: Pointer; Action: TListNotification);
  begin
    if Action in [lnExtracted, lnDeleted] then Dispose(PFileRec(Ptr));
  end;

  function TFileList.Remove(const _sName, _sPath: String): Integer;
  begin
    Result := IndexOf(_sName, _sPath);
    if Result>=0 then Delete(Result);
  end;

  procedure TFileList.Sort(Prop: TFileListSortProperty; Direction: TPhoaSortDirection);
  begin
    if Count>0 then InternalQuickSort(0, Count-1, Prop, Direction);
  end;

   //===================================================================================================================
   // TPhoaMask
   //===================================================================================================================

  constructor TPhoaMask.Create(const sMask: String; bNegative: Boolean);
  begin
    inherited Create(sMask);
    FNegative := bNegative;
  end;

  function TPhoaMask.Matches(const sFilename: String): Boolean;
  begin
    Result := inherited Matches(sFilename) xor FNegative;
  end;

   //===================================================================================================================
   // TPhoaMasks
   //===================================================================================================================

  constructor TPhoaMasks.Create(const sMasks: String);
  var
    s, sMask: String;
    bNegative: Boolean;
  begin
    inherited Create;
    FMasks := TList.Create;
     // Создаём массив масок
    s := iif(sMasks='*.*', '', sMasks);
    while s<>'' do begin
      sMask := ExtractFirstWord(s, ';');
      if sMask<>'' then begin
         // Если маска начинается с '!' - её действие инвертируется
        bNegative := sMask[1]='!';
        if bNegative then Delete(sMask, 1, 1);
        if sMask<>'' then FMasks.Add(TPhoaMask.Create(sMask, bNegative));
      end;
    end;
  end;

  destructor TPhoaMasks.Destroy;
  var i: Integer;
  begin
    for i := 0 to FMasks.Count-1 do TMask(FMasks[i]).Free;
    FMasks.Free;
    inherited Destroy;
  end;

  function TPhoaMasks.GetEmpty: Boolean;
  begin
    Result := FMasks.Count=0;
  end;

  function TPhoaMasks.Matches(const sFilename: String): Boolean;
  var i: Integer;
  begin
     // Если масок нет - считаем подходящим любой файл
    Result := Empty;
     // Иначе проверяем все маски по порядку - авось какая-то подойдёт
    if not Result then
      for i := 0 to FMasks.Count-1 do begin
        Result := TPhoaMask(FMasks[i]).Matches(sFilename);
        if Result then Break;
      end;
  end;

   //===================================================================================================================
   // TPhoaCommandLine
   //===================================================================================================================

  procedure PhoaCommandLineError(const sMsg: String; const aParams: Array of const);
  begin
    raise EPhoaCommandLineError.CreateFmt(sMsg, aParams);
  end;

  constructor TPhoaCommandLine.Create;
  begin
    inherited Create;
    FKeyValues := TStringList.Create;
    ParseCmdLine;
  end;

  destructor TPhoaCommandLine.Destroy;
  begin
    FKeyValues.Free;
    inherited Destroy;
  end;

  function TPhoaCommandLine.GetKeyValues(Key: TCmdLineKey): String;
  var idx: Integer;
  begin
    idx := FKeyValues.IndexOfObject(Pointer(Key));
    if idx<0 then Result := '' else Result := FKeyValues[idx];
  end;

  function TPhoaCommandLine.KeyValIndex(Key: TCmdLineKey): Integer;
  begin
    Result := FKeyValues.IndexOfObject(Pointer(Key));
  end;

  procedure TPhoaCommandLine.ParseCmdLine;
  var
    i: Integer;
    sParam, sPhoaName: String;
    k, kLast: TCmdLineKey;

     // Находит и возвращает TCmdLineKey, соответствующий символу c (case insensitive). Если не находит, вызывает
     //   EPhoaCommandLineError
    function GetKeyByChar(c: Char): TCmdLineKey;
    begin
       // Convert to lowercase
      if c in ['A'..'Z'] then Inc(c, Ord('a')-Ord('A'));
       // Iterate through known chars
      for Result := Low(Result) to High(Result) do
        if aCmdLineKeys[Result].cChar=c then Exit;
      Result := clkOpenPhoa; // Satisfy the compiler
      PhoaCommandLineError(SCmdLineErrMsg_UnknownKey, [c]);
    end;

     // Устанавливает значение sValue для ключа Key. Если у этого ключа уже есть значение, вызывает
     //   EPhoaCommandLineError
    procedure SetKeyValue(Key: TCmdLineKey; sValue: String);
    begin
       // Ищем такой ключ
      if KeyValIndex(Key)>=0 then
        if Key=clkOpenPhoa then
          PhoaCommandLineError(SCmdLineErrMsg_DuplicateOpenPhoaValue, [])
        else
          PhoaCommandLineError(SCmdLineErrMsg_DuplicateKeyValue, [aCmdLineKeys[Key].cChar]);
       // Нет ещё такого
      FKeyValues.AddObject(sValue, Pointer(Key));
    end;

  begin
    FKeys := [];
    FKeyValues.Clear;
    kLast := clkOpenPhoa;
    sPhoaName := '';
     // Перебираем все параметры
    for i := 1 to ParamCount do begin
      sParam := Trim(ParamStr(i));
      if sParam<>'' then
        case sParam[1] of
           // Ключ. Проверяем его формат
          '-':
            if Length(sParam)=2 then begin
              k := GetKeyByChar(sParam[2]);
              if k in FKeys then PhoaCommandLineError(SCmdLineErrMsg_DuplicateKey, [aCmdLineKeys[k].cChar]);
              Include(FKeys, k);
              kLast := k;
            end else
              PhoaCommandLineError(SCmdLineErrMsg_KeyNameInvalid, [sParam]);
           // Значение. Проверяем потребность предыдущего ключа в значении
          else begin
            case aCmdLineKeys[kLast].ValueMode of
               // Нет значения. Трактуем значение как файл фотоальбома для открытия
              clkvmNo: SetKeyValue(clkOpenPhoa, sParam);
               // Есть значение. Связываем его с ключом
              else SetKeyValue(kLast, sParam);
            end;
             // Если значение попало в ключ по умолчанию, включаем его в набор (т.к. явно он не указывается)
            if kLast=clkOpenPhoa then Include(FKeys, clkOpenPhoa);
             // Сбрасываем ключ в режим по умолчанию
            kLast := clkOpenPhoa;
          end;
        end;
    end;
     // Проверяем обязательные значения параметров
    for k := Low(k) to High(k) do
      if (k in FKeys) and (aCmdLineKeys[k].ValueMode=clkvmRequired) and (KeyValIndex(k)<0) then
        PhoaCommandLineError(SCmdLineErrMsg_KeyValueMissing, [aCmdLineKeys[k].cChar]);
  end;

   //===================================================================================================================

  function NewPhotoAlbumPic: IPhotoAlbumPic;
  begin
    Result := TPhotoAlbumPic.Create;
  end;

  function NewPhotoAlbumPicList(bSorted: Boolean): IPhotoAlbumPicList;
  begin
    Result := TPhotoAlbumPicList.Create(bSorted);
  end;

  function NewPhotoAlbumKeywordList: IPhotoAlbumKeywordList;
  begin
    Result := TPhotoAlbumKeywordList.Create;
  end;

  function NewPhotoAlbumPicGroup(AOwner: IPhoaPicGroup; iID: Integer): IPhotoAlbumPicGroup;
  begin
    Result := TPhotoAlbumPicGroup.Create(AOwner, iID);
  end;

  function NewPhotoAlbumPicGroupList(AOwner: IPhoaPicGroup): IPhotoAlbumPicGroupList;
  begin
    Result := TPhotoAlbumPicGroupList.Create(AOwner);
  end;

  function NewPhotoAlbumPicSorting: IPhotoAlbumPicSorting;
  begin
    Result := TPhotoAlbumPicSorting.Create;
  end;

  function NewPhotoAlbumPicSortingList: IPhotoAlbumPicSortingList;
  begin
    Result := TPhotoAlbumPicSortingList.Create;
  end;

  function NewPhotoAlbumPicGrouping: IPhotoAlbumPicGrouping;
  begin
    Result := TPhotoAlbumPicGrouping.Create;
  end;

  function NewPhotoAlbumPicGroupingList: IPhotoAlbumPicGroupingList;
  begin
    Result := TPhotoAlbumPicGroupingList.Create;
  end;

  function NewPhotoAlbumView(AList: IPhotoAlbumViewList): IPhotoAlbumView;
  begin
    Result := TPhotoAlbumView.Create(AList);
  end;

  function NewPhotoAlbumViewList(APics: IPhoaPicList): IPhotoAlbumViewList;
  begin
    Result := TPhotoAlbumViewList.Create(APics);
  end;

  function NewPhotoAlbumProject: IPhotoAlbumProject;
  begin
    Result := TPhotoAlbumProject.Create;
  end;

end.
