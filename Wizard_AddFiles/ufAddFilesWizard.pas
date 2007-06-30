//**********************************************************************************************************************
//  $Id: ufAddFilesWizard.pas,v 1.4 2007-06-30 10:36:21 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufAddFilesWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  TntSysUtils, TntWideStrings, TntClasses, GR32, GraphicEx,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars, phWizard, phGraphics,
  phWizForm, DKLang, GR32_Image, TB2Dock, TBXDkPanels, ExtCtrls,
  TntExtCtrls, StdCtrls, TntStdCtrls;

type
  TAddFilesThread = class;

  TfAddFilesWizard = class(TPhoaWizardForm, IPhoaWizardPageHost_Log, IPhoaWizardPageHost_Process)
    dklcMain: TDKLanguageController;
    dpPreview: TTBXDockablePanel;
    iPreview: TImage32;
    pProcess: TTntPanel;
    procedure dpPreviewResize(Sender: TObject);
    procedure dpPreviewVisibleChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
     // Поток, добавляющий файлы
    FAddFilesThread: TAddFilesThread;
     // True, если активен процесс добавления файлов
    FProcessingFiles: Boolean;
     // Флаг того, что добавление файлов нужно прервать
    FFileProcessingInterrupted: Boolean;
     // Главный буфер отката
    FUndoOperations: TPhoaOperations;
     // Список изображений для добавления в проект
    FPics: IPhotoAlbumPicList;
     // Протокол добавления
    FLog: TWideStrings;
     // Исходное количество добавляемых файлов
    FInitialFileCount: Integer;
     // Последнее добавленное изображение (nil, если нет)
    FLastProcessedPic: IPhoaPic;
     // Счётчик сбойных изображений
    FCountFailed: Integer;
     // Следующий свободный ID изображения
    FFreePicID: Integer;
     // Группа, в которую добавляются изображения
    FGroup: IPhotoAlbumPicGroup;
     // Флаг принудительного обновления видимости окна просмотра
    FUpdatingPreviewVisibility: Boolean;
     // Prop storage
    FAddList: TStrings;
    FApp: IPhotoAlbumApp;
    FDefaultPath: WideString;
    FFileList: TFileList;
    FFilter_DateFrom: TDateTime;
    FFilter_DateTo: TDateTime;
    FFilter_Masks: WideString;
    FFilter_Presence: TAddFilePresenceFilter;
    FFilter_SizeFrom: Integer;
    FFilter_SizeFromUnit: TFileSizeUnit;
    FFilter_SizeTo: Integer;
    FFilter_SizeToUnit: TFileSizeUnit;
    FFilter_TimeFrom: TDateTime;
    FFilter_TimeTo: TDateTime;
    FRecurseFolders: Boolean;
    FShowAdvancedOptions: Boolean;
    FShowPreview: Boolean;
     // Загружает в FFileList список файлов (папок), имена которых перечислены в FAddList. Возвращает True, если в
     //   список попал хотя бы один файл. При bRecurse=True просматриваются подпапки папок; bUseFilter влияет на то,
     //   используются ли значения свойств Filter_xxx для проверки файлов
    function  LoadFileList(bRecurse, bUseFilter: Boolean): Boolean;
     // Обновляет информацию о добавлении файлов
    procedure UpdateProgressInfo;
     // Вызывается потоком, добавляющим файлы, для уведомления о том, что файл обработан
    procedure ThreadFileProcessed;
     // Добавление строки в протокол
    procedure LogSuccess(const ws: WideString; const aParams: Array of const);
    procedure LogFailure(const ws: WideString; const aParams: Array of const);
     // Обновляет видимость окна просмотра и вызывает UpdatePreview
    procedure UpdatePreviewVisibility;
     // Обновляет масштаб, используемый для просмотра
    procedure UpdatePreviewScale;
     // IPhoaWizardPageHost_Log
    function  LogPage_GetLog(iPageID: Integer): TWideStrings;
    function  IPhoaWizardPageHost_Log.GetLog = LogPage_GetLog;
     // IPhoaWizardPageHost_Process
    procedure ProcPage_PaintThumbnail(Bitmap32: TBitmap32);
    function  ProcPage_GetCurrentStatus: WideString;
    function  ProcPage_GetProcessingActive: Boolean;
    function  ProcPage_GetProgressCur: Integer;
    function  ProcPage_GetProgressMax: Integer;
    procedure IPhoaWizardPageHost_Process.StartProcessing     = StartFileProcessing;
    procedure IPhoaWizardPageHost_Process.StopProcessing      = InterruptFileProcessing;
    procedure IPhoaWizardPageHost_Process.PaintThumbnail      = ProcPage_PaintThumbnail;
    function  IPhoaWizardPageHost_Process.GetCurrentStatus    = ProcPage_GetCurrentStatus;
    function  IPhoaWizardPageHost_Process.GetProcessingActive = ProcPage_GetProcessingActive;
    function  IPhoaWizardPageHost_Process.GetProgressCur      = ProcPage_GetProgressCur;
    function  IPhoaWizardPageHost_Process.GetProgressMax      = ProcPage_GetProgressMax;
     // Prop handlers
    procedure SetShowPreview(Value: Boolean);
  protected
    function  GetNextPageID: Integer; override;
    function  GetRelativeRegistryKey: AnsiString; override;
    function  GetStartPageID: Integer; override;
    function  IsBtnBackEnabled: Boolean; override;
    function  IsBtnCancelEnabled: Boolean; override;
    function  IsBtnNextEnabled: Boolean; override;
    function  PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure ExecuteFinalize; override;
    procedure ExecuteInitialize; override;
    procedure PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer); override;
    procedure SettingsInitialLoad(rif: TPhoaRegIniFile); override;
    procedure SettingsInitialSave(rif: TPhoaRegIniFile); override;
    procedure SettingsLoad(rif: TPhoaRegIniFile); override;
    procedure SettingsSave(rif: TPhoaRegIniFile); override;
  public
     // Загружает файл для просмотра
    procedure UpdatePreview;
     // Запускает добавление файлов
    procedure StartFileProcessing;
     // Прерывает добавление файлов
    procedure InterruptFileProcessing;
     // Props
     // -- Список файлов/папок для добавления в FileList
    property AddList: TStrings read FAddList;
     // -- Приложение
    property App: IPhotoAlbumApp read FApp;
     // -- Папка для добавления, выбираемая по умолчанию
    property DefaultPath: WideString read FDefaultPath write FDefaultPath;
     // -- Список файлов
    property FileList: TFileList read FFileList;
     // -- Фильтр: дата модификации файла "С" фильтра
    property Filter_DateFrom: TDateTime read FFilter_DateFrom write FFilter_DateFrom;
     // -- Фильтр: дата модификации файла "По" фильтра
    property Filter_DateTo: TDateTime read FFilter_DateTo write FFilter_DateTo;
     // -- Фильтр: маски файлов
    property Filter_Masks: WideString read FFilter_Masks write FFilter_Masks;
     // -- Фильтр: присутствие файлов в фотоальбоме
    property Filter_Presence: TAddFilePresenceFilter read FFilter_Presence write FFilter_Presence;
     // -- Фильтр: минимальный размер файла в единицах Filter_SizeFromUnits
    property Filter_SizeFrom: Integer read FFilter_SizeFrom write FFilter_SizeFrom;
     // -- Фильтр: единица измерения минимального размера файла
    property Filter_SizeFromUnit: TFileSizeUnit read FFilter_SizeFromUnit write FFilter_SizeFromUnit;
     // -- Фильтр: максимальный размер файла в единицах Filter_SizeToUnits
    property Filter_SizeTo: Integer read FFilter_SizeTo write FFilter_SizeTo;
     // -- Фильтр: единица измерения минимального размера файла
    property Filter_SizeToUnit: TFileSizeUnit read FFilter_SizeToUnit write FFilter_SizeToUnit;
     // -- Фильтр: время модификации файла "С" фильтра
    property Filter_TimeFrom: TDateTime read FFilter_TimeFrom write FFilter_TimeFrom;
     // -- Фильтр: время модификации файла "По" фильтра
    property Filter_TimeTo: TDateTime read FFilter_TimeTo write FFilter_TimeTo;
     // -- True, если включен просмотр вложенных папок
    property RecurseFolders: Boolean read FRecurseFolders write FRecurseFolders;
     // -- True, если включены расширенные опции фильтра
    property ShowAdvancedOptions: Boolean read FShowAdvancedOptions write FShowAdvancedOptions;
     // -- True, если отображается окно предварительного просмотра
    property ShowPreview: Boolean read FShowPreview write SetShowPreview;
  end;

  TAddFilesThread = class(TThread)
  private
     // Форма-владелец потока
    FWizard: TfAddFilesWizard;
     // Кэшированные значения Autofill-свойств
    FDateAutofillProps: TDateTimeAutofillProps;
    FReplaceDate: Boolean;
    FTimeAutofillProps: TDateTimeAutofillProps;
    FReplaceTime: Boolean;
    FAutofillXForm: Boolean;
     // Результат автозаполнения свойств
    FDateFillResult: TDateTimeFillResult;
    FTimeFillResult: TDateTimeFillResult;
    FXFormFilled: Boolean;
     // Prop storage
    FAddedPic: IPhotoAlbumPic;
     // Производит автозаполнение свойств изображения
    procedure AutofillPicProps(Pic: IPhotoAlbumPic);
  protected
    procedure Execute; override;
  public
    constructor Create(Wizard: TfAddFilesWizard);
     // Props
     // -- Последнее добавленное изображение (nil, если нет или была ошибка)
    property AddedPic: IPhotoAlbumPic read FAddedPic;
  end;

   // Отображает мастер добавления файлов изображений. Возвращает True, если что-то в фотоальбоме было изменено. Если
   //   FileList<>nil, то пропускает страницу выбора файлов и сразу добавляет указанные файлы/папки
  function AddFiles(AApp: IPhotoAlbumApp; AGroup: IPhotoAlbumPicGroup; AUndoOperations: TPhoaOperations; AAddList: TStrings): Boolean;

implementation
{$R *.dfm}
uses
  phUtils, phMetadata, Main, VirtualShellUtilities,
  ufrWzPage_Log, ufrWzPage_Processing, ufrWzPageAddFiles_SelFiles, ufrWzPageAddFiles_CheckFiles,
  phPhoa, phSettings, phMsgBox;

  function AddFiles(AApp: IPhotoAlbumApp; AGroup: IPhotoAlbumPicGroup; AUndoOperations: TPhoaOperations; AAddList: TStrings): Boolean;
  var bShowWizard: Boolean;
  begin
    with TfAddFilesWizard.Create(Application) do
      try
        FApp            := AApp;
        FGroup          := AGroup; 
        FUndoOperations := AUndoOperations;
         // Если задан список файлов, загружаем файлы и проверяем необходимость отображения Мастера
        if AAddList<>nil then begin
          FAddList.Assign(AAddList);
          bShowWizard := LoadFileList(True, False);
        end else
          bShowWizard := True;
        Result := bShowWizard and ExecuteModal;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TAddFilesThread
   //===================================================================================================================

  procedure TAddFilesThread.AutofillPicProps(Pic: IPhotoAlbumPic);
  type
     // Запись о требуемых преобразованиях
    TTransformRec = record
      Rotation: TPicRotation; // Требуемый поворот
      Flips:    TPicFlips;    // Требуемые отражения
    end;
  const
     // Наборы требуемых преобразований в зависимости от значения Exif-тега Orientation ($0112)
    aExifOrientXforms: Array[1..8] of TTransformRec = (
      (Rotation: pr0;   Flips: []),         // 1=Portrait (Top Left)
      (Rotation: pr0;   Flips: [pflHorz]),  // 2=Portrait (Top Right)
      (Rotation: pr180; Flips: []),         // 3=Portrait (Bottom Right)
      (Rotation: pr180; Flips: [pflHorz]),  // 4=Portrait (Bottom Left)
      (Rotation: pr90;  Flips: [pflHorz]),  // 5=Landscape (Left Top)
      (Rotation: pr270; Flips: []),         // 6=Landscape (Right Top)
      (Rotation: pr270; Flips: [pflHorz]),  // 7=Landscape (Right Bottom)
      (Rotation: pr90;  Flips: []));        // 8=Landscape (Left Bottom)
  var
    Metadata: TImageMetadata;
    Namespace: TNamespace;

     // Пытается заполнить дату изображения из EXIF-тега. Возвращает True, если удалось
    function FillExifDate(iTag: Integer): Boolean;
    var
      idx: Integer;
      s: AnsiString;
    begin
      Result := False;
       // Ищем значение тега
      idx := Metadata.EXIFData.IndexOfObject(Pointer(iTag));
       // Если нашли
      if idx>=0 then begin
        s := Metadata.EXIFData[idx];
         // Пытаемся преобразовать в дату
        try
          Pic.Date := DateToPhoaDate(EncodeDate(StrToInt(Copy(s, 1, 4)), StrToInt(Copy(s, 6, 2)), StrToInt(Copy(s, 9, 2))));
          Result := True;
        except
           // Не вышло
          on EConvertError do ;
        end;
      end;
    end;

     // Пытается заполнить время изображения из EXIF-тега. Возвращает True, если удалось
    function FillExifTime(iTag: Integer): Boolean;
    var
      idx: Integer;
      s: AnsiString;
    begin
      Result := False;
       // Ищем значение тега
      idx := Metadata.EXIFData.IndexOfObject(Pointer(iTag));
       // Если нашли
      if idx>=0 then begin
        s := Metadata.EXIFData[idx];
         // Пытаемся преобразовать в дату
        try
          Pic.Time := TimeToPhoaTime(EncodeTime(StrToInt(Copy(s, 12, 2)), StrToInt(Copy(s, 15, 2)), StrToInt(Copy(s, 18, 2)), 0));
          Result := True;
        except
           // Не вышло
          on EConvertError do ;
        end;
      end;
    end;

     // Пытается заполнить дату изображения, извлекая её из имени файла изображения. Возвращает True, если удалось
    function FillDateFromFilename: Boolean;
    var
      ws: WideString;
      wy, wm, wd: Word;
    begin
      Result := False;
      ws := WideExtractFileName(Pic.FileName);
      wy := StrToIntDef(ExtractFirstWord(ws, '-,.'), 0);
      wm := StrToIntDef(ExtractFirstWord(ws, '-,.'), 0);
      wd := StrToIntDef(ExtractFirstWord(ws, '-,. '), 0);
      try
        Pic.Date := DateToPhoaDate(EncodeDate(wy, wm, wd));
        Result := True;
      except
        on EConvertError do { nothing };
      end;
    end;

     // Пытается заполнить время изображения, извлекая его из имени файла изображения. Возвращает True, если удалось
    function FillTimeFromFilename: Boolean;
    var
      ws: WideString;
      bh, bm, bs: Word;
      t: TDateTime;
    begin
      Result := False;
      ws := WideExtractFileName(Pic.FileName);
       // Удаляем часть, отвечающую за дату
      ExtractFirstWord(ws, ' ');
       // Извлекаем компоненты времени
      bh := StrToIntDef(ExtractFirstWord(ws, '-,.'), 0);
      bm := StrToIntDef(ExtractFirstWord(ws, '-,.'), 0);
      bs := StrToIntDef(ExtractFirstWord(ws, '-,.'), 0);
      try
        t := EncodeTime(bh, bm, bs, 0);
        if t>0 then begin
          Pic.Time := TimeToPhoaTime(t);
          Result := True;
        end;
      except
        on EConvertError do { nothing };
      end;
    end;

     // Пытается заполнить дату изображения из TDateTime. Возвращает True, если удалось
    function FillDate(const Date: TDateTime): Boolean;
    begin
      Result := Date<>0;
      if Result then Pic.Date := DateToPhoaDate(Date);
    end;

     // Пытается заполнить время изображения из TDateTime. Возвращает True, если удалось
    function FillTime(const Time: TDateTime): Boolean;
    begin
      Result := Time<>0;
      if Result then Pic.Time := TimeToPhoaTime(Time);
    end;

     // Возвращает True, если необходимо заполнить свойство с учётом его наличия и флага перезаписи значения
    function NeedFill(CurResult: TDateTimeFillResult; bOverwrite: Boolean): Boolean;
    begin
      Result := (CurResult=dtfrEmpty) or ((CurResult=dtfrSpecified) and bOverwrite);
    end;

     // Пытается заполнить преобразования изображения из метаданных
    procedure FillExifTransforms;
    var
      idx: Integer;
      bOrient: Byte;
    begin
       // Ищем значение тега
      idx := Metadata.EXIFData.IndexOfObject(Pointer(EXIF_TAG_ORIENTATION));
       // Если нашли
      if idx>=0 then begin
         // Преобразуем (подпорка: первый символ в ORIENTATION - это цифра [1..8]) 
        bOrient := StrToIntDef(Copy(Metadata.EXIFData[idx], 1, 1), 0);
        if bOrient in [Low(aExifOrientXforms)..High(aExifOrientXforms)] then begin
          Pic.Rotation := aExifOrientXforms[bOrient].Rotation;
          Pic.Flips    := aExifOrientXforms[bOrient].Flips;
          FXformFilled := True;
        end;
      end;
    end;

  begin
    if Pic.Date=0 then FDateFillResult := dtfrEmpty else FDateFillResult := dtfrSpecified;
    if Pic.Time=0 then FTimeFillResult := dtfrEmpty else FTimeFillResult := dtfrSpecified;
    FXFormFilled := False;
     // Если нужно, создаём объект метаданных
    if (NeedFill(FDateFillResult, FReplaceDate) and ([dtapExifDTOriginal, dtapExifDTDigitized, dtapExifDateTime]*FDateAutofillProps<>[])) or
       (NeedFill(FTimeFillResult, FReplaceTime) and ([dtapExifDTOriginal, dtapExifDTDigitized, dtapExifDateTime]*FTimeAutofillProps<>[])) or
       FAutofillXForm then begin
      Metadata := TImageMetadata.Create(Pic.FileName);
      try
        if Metadata.StatusCode=IMS_OK then begin
           // Дата
          if NeedFill(FDateFillResult, FReplaceDate) and (dtapExifDTOriginal  in FDateAutofillProps) and FillExifDate(EXIF_TAG_DATETIME_ORIGINAL)  then FDateFillResult := dtfrEXIF;
          if NeedFill(FDateFillResult, FReplaceDate) and (dtapExifDTDigitized in FDateAutofillProps) and FillExifDate(EXIF_TAG_DATETIME_DIGITIZED) then FDateFillResult := dtfrEXIF;
          if NeedFill(FDateFillResult, FReplaceDate) and (dtapExifDateTime    in FDateAutofillProps) and FillExifDate(EXIF_TAG_DATETIME)           then FDateFillResult := dtfrEXIF;
           // Время
          if NeedFill(FTimeFillResult, FReplaceTime) and (dtapExifDTOriginal  in FTimeAutofillProps) and FillExifTime(EXIF_TAG_DATETIME_ORIGINAL)  then FTimeFillResult := dtfrEXIF;
          if NeedFill(FTimeFillResult, FReplaceTime) and (dtapExifDTDigitized in FTimeAutofillProps) and FillExifTime(EXIF_TAG_DATETIME_DIGITIZED) then FTimeFillResult := dtfrEXIF;
          if NeedFill(FTimeFillResult, FReplaceTime) and (dtapExifDateTime    in FTimeAutofillProps) and FillExifTime(EXIF_TAG_DATETIME)           then FTimeFillResult := dtfrEXIF;
           // Преобразования
          if FAutofillXForm then FillExifTransforms;
        end;
      finally
        Metadata.Free;
      end;
    end;
     // Извлекаем дату/время из имени файла
    if NeedFill(FDateFillResult, FReplaceDate) and (dtapFilename in FDateAutofillProps) and FillDateFromFilename then FDateFillResult := dtfrFilename;
    if NeedFill(FTimeFillResult, FReplaceTime) and (dtapFilename in FTimeAutofillProps) and FillTimeFromFilename then FTimeFillResult := dtfrFilename;
     // Если нужно, создаём TNamespace
    if (NeedFill(FDateFillResult, FReplaceDate) and ([dtapDTFileCreated, dtapDTFileModified]*FDateAutofillProps<>[])) or
       (NeedFill(FTimeFillResult, FReplaceTime) and ([dtapDTFileCreated, dtapDTFileModified]*FTimeAutofillProps<>[])) then begin
      try
        Namespace := TNamespace.CreateFromFileName(Pic.FileName);
        try
           // Дата
          if NeedFill(FDateFillResult, FReplaceDate) and (dtapDTFileCreated  in FDateAutofillProps) and FillDate(Int(Namespace.CreationDateTime))   then FDateFillResult := dtfrCreation;
          if NeedFill(FDateFillResult, FReplaceDate) and (dtapDTFileModified in FDateAutofillProps) and FillDate(Int(Namespace.LastWriteDateTime))  then FDateFillResult := dtfrModified;
           // Время
          if NeedFill(FTimeFillResult, FReplaceTime) and (dtapDTFileCreated  in FTimeAutofillProps) and FillTime(Frac(Namespace.CreationDateTime))  then FTimeFillResult := dtfrCreation;
          if NeedFill(FTimeFillResult, FReplaceTime) and (dtapDTFileModified in FTimeAutofillProps) and FillTime(Frac(Namespace.LastWriteDateTime)) then FTimeFillResult := dtfrModified;
        finally
          Namespace.Free;
        end;
      except
        on EVSTInvalidFileName do {ignore}
      end;
    end;
  end;

  constructor TAddFilesThread.Create(Wizard: TfAddFilesWizard);
  begin
    inherited Create(True);
    FWizard := Wizard;
    FreeOnTerminate := True;
     // Кэшируем значения Autofill-свойств
    FDateAutofillProps := TDateTimeAutofillProps(Byte(SettingValueInt(ISettingID_Dlgs_APW_AutofillDate)));
    FReplaceDate       := SettingValueBool(ISettingID_Dlgs_APW_ReplaceDate);
    FTimeAutofillProps := TDateTimeAutofillProps(Byte(SettingValueInt(ISettingID_Dlgs_APW_AutofillTime)));
    FReplaceTime       := SettingValueBool(ISettingID_Dlgs_APW_ReplaceTime);
    FAutofillXForm     := SettingValueBool(ISettingID_Dlgs_APW_AutofillXfrm);
    Resume;
  end;

  procedure TAddFilesThread.Execute;
  var wsFileName: WideString;
  begin
    try
      while not Terminated do
        with FWizard do begin
          try
            wsFileName := FileList.Files[0];
             // Ищем изображение по имени файла
            FAddedPic := App.ProjectX.PicsX.ItemsByFileNameX[wsFileName];
             // Если не нашли - создаём новое и генерируем эскиз
            if FAddedPic=nil then begin
              FAddedPic := NewPhotoAlbumPic;
              FAddedPic.FileName := wsFileName;
              FAddedPic.ReloadPicFileData(
                App.Project.ThumbnailSize,
                TPhoaStretchFilter(SettingValueInt(ISettingID_Browse_ViewerStchFilt)),
                App.Project.ThumbnailQuality);
            end;
             // Производим автозаполнение свойств изображения
            AutofillPicProps(FAddedPic);
             // Пишем в протокол
            LogSuccess(
              'SLogEntry_AddingOK',
              [wsFileName,
               DateTimeFillResultName(FDateFillResult),
               DateTimeFillResultName(FTimeFillResult),
               DKLangConstW(iif(FXformFilled, 'STransformFilledFromExif', 'SNone'))]);
          except
            on e: Exception do begin
              FAddedPic := nil;
              LogFailure('SLogEntry_AddingError', [FileList.Files[0], e.Message]);
            end;
          end;
           // Регистрируем результат
          Synchronize(ThreadFileProcessed);
        end;
    finally
      FAddedPic := nil;
    end;
  end;

   //===================================================================================================================
   // TfAddFilesWizard
   //===================================================================================================================

  procedure TfAddFilesWizard.DoCreate;
  begin
    inherited DoCreate;
    FAddList   := TStringList.Create;
    FFileList  := TFileList.Create;
    FPics      := NewPhotoAlbumPicList(False);
     // Настраиваем окно просмотра
    dpPreview.Floating := True;
     // Создаём страницы
    Controller.CreatePage(TfrWzPageAddFiles_SelFiles,   IWzAddFilesPageID_SelFiles,   IDH_intf_pic_add_selfiles,   DKLangConstW('SWzPageAddFiles_SelFiles'));
    Controller.CreatePage(TfrWzPageAddFiles_CheckFiles, IWzAddFilesPageID_CheckFiles, IDH_intf_pic_add_checkfiles, DKLangConstW('SWzPageAddFiles_CheckFiles'));
    Controller.CreatePage(TfrWzPage_Processing,         IWzAddFilesPageID_Processing, IDH_intf_pic_add_process,    DKLangConstW('SWzPageAddFiles_Processing'));
    Controller.CreatePage(TfrWzPage_Log,                IWzAddFilesPageID_Log,        IDH_intf_pic_add_log,        DKLangConstW('SWzPageAddFiles_Log'));
  end;

  procedure TfAddFilesWizard.DoDestroy;
  begin
    FAddList.Free;
    FFileList.Free;
    inherited DoDestroy;
  end;

  procedure TfAddFilesWizard.dpPreviewResize(Sender: TObject);
  begin
     // Обновляем масштаб просматриваемого изображения
    UpdatePreviewScale;
  end;

  procedure TfAddFilesWizard.dpPreviewVisibleChanged(Sender: TObject);
  var PreviewInfoIntf: IPhoaWizardPage_PreviewInfo;
  begin
    if FUpdatingPreviewVisibility then Exit;
    if not dpPreview.Visible then FShowPreview := False;
    if Supports(Controller.VisiblePage, IPhoaWizardPage_PreviewInfo, PreviewInfoIntf) then
      PreviewInfoIntf.PreviewVisibilityChanged(FShowPreview);
  end;

  procedure TfAddFilesWizard.ExecuteFinalize;
  begin
    FreeAndNil(FLog);
     // Если есть добавленные изображения, выполняем операцию
    if (FPics<>nil) and (FPics.Count>0) then FApp.PerformOperation('PicAdd', ['Group', FGroup, 'Pics', FPics]);
    inherited ExecuteFinalize;
  end;

  procedure TfAddFilesWizard.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
    FFreePicID := FApp.ProjectX.PicsX.MaxPicID+1;
  end;

  procedure TfAddFilesWizard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
     // Нельзя закрывать окошко, пока добавляются файлы
    CanClose := not FProcessingFiles;
  end;

  function TfAddFilesWizard.GetNextPageID: Integer;
  begin
    case CurPageID of
       // Если надо пропустить страницу отметки файлов - переходим к странице отметки файлов
      IWzAddFilesPageID_SelFiles:   Result := iif(SettingValueBool(ISettingID_Dlgs_APW_SkipChkPage), IWzAddFilesPageID_Processing, IWzAddFilesPageID_CheckFiles);
      IWzAddFilesPageID_CheckFiles: Result := IWzAddFilesPageID_Processing;
      IWzAddFilesPageID_Processing: Result := IWzAddFilesPageID_Log;
      else                          Result := 0;
    end;
  end;

  function TfAddFilesWizard.GetRelativeRegistryKey: AnsiString;
  begin
    Result := SRegAddFiles_Root;
  end;

  function TfAddFilesWizard.GetStartPageID: Integer;
  begin
     // Если изначально список файлов не пуст, пропускаем страницу выбора файлов
    if FFileList.Count>0 then
      Result := iif(
        SettingValueBool(ISettingID_Dlgs_APW_SkipChkPage), IWzAddFilesPageID_Processing, IWzAddFilesPageID_CheckFiles)
      // Иначе отображаем страницу выбора файлов
    else
      Result := IWzAddFilesPageID_SelFiles;
  end;

  procedure TfAddFilesWizard.InterruptFileProcessing;
  begin
    FFileProcessingInterrupted := True;
  end;

  function TfAddFilesWizard.IsBtnBackEnabled: Boolean;
  begin
     // Со страницы прогресса возврата нет, со страницы ошибок вернуться можно только если ещё есть файлы
    Result :=
      inherited IsBtnBackEnabled and
      (CurPageID<>IWzAddFilesPageID_Processing) and
      ((CurPageID<>IWzAddFilesPageID_Log) or (FFileList.Count>0));
  end;

  function TfAddFilesWizard.IsBtnCancelEnabled: Boolean;
  begin
    Result := inherited IsBtnCancelEnabled and ((CurPageID<>IWzAddFilesPageID_Processing) or not FProcessingFiles);
  end;

  function TfAddFilesWizard.IsBtnNextEnabled: Boolean;
  begin
    Result := inherited IsBtnNextEnabled;
    if Result then
      case CurPageID of
         // На странице прогресса дальше можно идти только при остановленном процессе и наличии строчек протокола
        IWzAddFilesPageID_Processing: Result := not FProcessingFiles and (FLog<>nil);
      end;
  end;

  function TfAddFilesWizard.LoadFileList(bRecurse, bUseFilter: Boolean): Boolean;
  var
    Masks: TPhoaMasks;
    i64MinSize, i64MaxSize: Int64;
    iInitialImgIdx: Integer;

     // Добавляет файл к списку по его SearchRec. Если bUseFilter=True, предварительно проверяет его соответствие
     //   фильтру
    procedure AddFile(const wsPath: WideString; SRec: TSearchRecW); overload;
    var
      dDateTime: TDateTime;
      i64Size: Int64;
      bMatches: Boolean;
    begin
       // Проверяем, что расширение знакомого типа (implicit Unicode-to-Ansi conversion)
      if FileFormatList.GraphicFromExtension(WideExtractFileExt(SRec.Name))=nil then Exit;
      dDateTime := FileDateToDateTime(SRec.Time);
      i64Size   := (Int64(SRec.FindData.nFileSizeHigh) shl 32) or SRec.FindData.nFileSizeLow;
      bMatches  := True;
       // Если фильтр включен
      if bUseFilter and FShowAdvancedOptions then begin
         // Проверяем размер файла
        if bMatches then bMatches := (i64Size>=i64MinSize) and (i64Size<=i64MaxSize);
         // Проверяем дату изменения файла
        if bMatches then
          bMatches :=
            ((FFilter_DateFrom<0) or (Int (dDateTime)>=FFilter_DateFrom)) and
            ((FFilter_DateTo<0)   or (Int (dDateTime)<=FFilter_DateTo))   and
            ((FFilter_TimeFrom<0) or (Frac(dDateTime)>=FFilter_TimeFrom)) and
            ((FFilter_TimeTo<0)   or (Frac(dDateTime)<=FFilter_TimeTo));
         // Проверяем соответствие маске
        if bMatches and (Masks<>nil) then bMatches := Masks.Matches(SRec.Name);
         // Проверяем присутствие в фотоальбоме
        if bMatches and (FFilter_Presence<>afpfDontCare) then
          bMatches := (FApp.Project.Pics.IndexOfFileName(wsPath+SRec.Name)>=0) = (FFilter_Presence=afpfExistingOnly);
      end;
       // Если все критерии удовлетворены
      if bMatches then FFileList.Add(SRec.Name, wsPath, i64Size, iInitialImgIdx, dDateTime);
    end;

    procedure AddFolder(const wsPath: WideString);
    var
      sr: TSearchRecW;
      iRes: Integer;
    begin
       // Обновляем информацию о процессе
      pProcess.Caption := DKLangConstW('SMsg_ProcessingSomething', [wsPath]);
      pProcess.Update;
       // Сканируем каталог
      iRes := WideFindFirst(wsPath+'*.*', faAnyFile, sr);
      try
        while iRes=0 do begin
          if sr.Name[1]<>'.' then
             // Если каталог - рекурсивно сканируем
            if sr.Attr and faDirectory<>0 then begin
              if bRecurse then AddFolder(wsPath+sr.Name+'\');
             // Если файл - добавляем к списку
            end else
              AddFile(wsPath, sr);
          iRes := WideFindNext(sr);
        end;
      finally
        WideFindClose(sr);
      end;
    end;

     // Добавляет в FFileList файлы/папки из FAddList
    procedure ProcessAddList;
    var
      i, iRes: Integer;
      wsName: WideString;
      sr: TSearchRecW;
    begin
       // Стираем существующий список файлов
      FFileList.Clear;
       // Обрабатываем список выбранных файлов/папок
      for i := 0 to FAddList.Count-1 do begin
        wsName := FAddList[i];
         // Если это корень диска вида 'X:\'
        if (Length(wsName)=3) and (wsName[2]=':') and (wsName[3]='\') then
          AddFolder(wsName)
         // Иначе определяем, файл это или папка
        else begin
          iRes := WideFindFirst(wsName, faAnyFile, sr);
          try
            if iRes=0 then
               // Файл
              if sr.Attr and faDirectory=0 then
                AddFile(WideExtractFilePath(wsName), sr)
               // Каталог
              else
                AddFolder(IncludeTrailingPathDelimiter(wsName));
          finally
            WideFindClose(sr);
          end;
        end;
      end;
    end;

  begin
     // Отображаем панель процесса
    StartWait;
    pProcess.Show;
    try
       // Если необходимо отображать значки, необходимо передать индекс значка -2. Иначе - -1
      iInitialImgIdx := iif(SettingValueBool(ISettingID_Dlgs_APW_ExtractIcons), -2, -1);
       // Если фильтр активен
      if FShowAdvancedOptions then begin
         // Определяем минимальный и максимальный размеры файлов
        i64MinSize := aFileSizeUnitMultipliers[FFilter_SizeFromUnit]*Int64(FFilter_SizeFrom);
        i64MaxSize := aFileSizeUnitMultipliers[FFilter_SizeToUnit]  *Int64(FFilter_SizeTo);
         // Если нужно, создаём список масок
        if FFilter_Masks<>'' then Masks := TPhoaMasks.Create(FFilter_Masks) else Masks := nil;
      end;
      try
         // Обрабатываем файлы/папки
        ProcessAddList;
      finally
        Masks.Free;
      end;
     // Скрываем панель процесса
    finally
      pProcess.Hide;
      StopWait;
    end;
    Result := FFileList.Count>0;
    if not Result then PhoaInfo(False, 'SNoFilesSelected');
  end;

  procedure TfAddFilesWizard.LogFailure(const ws: WideString; const aParams: array of const);
  begin
    FLog.Add('[!] '+DKLangConstW(ws, aParams));
  end;

  function TfAddFilesWizard.LogPage_GetLog(iPageID: Integer): TWideStrings;
  begin
    Result := FLog;
  end;

  procedure TfAddFilesWizard.LogSuccess(const ws: WideString; const aParams: array of const);
  begin
    FLog.Add('[+] '+DKLangConstW(ws, aParams));
  end;

  procedure TfAddFilesWizard.PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer);
  begin
    inherited PageChanged(ChangeMethod, iPrevPageID);
     // Обновляем окно просмотра
    UpdatePreviewVisibility;
     // На странице прогресса запускаем обработку файлов
    if (ChangeMethod in [pcmNextBtn, pcmForced]) and (CurPageID=IWzAddFilesPageID_Processing) then StartFileProcessing;
  end;

  function TfAddFilesWizard.PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean;
  begin
    Result := inherited PageChanging(ChangeMethod, iNewPageID);
     // После страницы выбора файлов/папок загружаем список файлов
    if Result and (ChangeMethod=pcmNextBtn) and (CurPageID=IWzAddFilesPageID_SelFiles) then
      Result := LoadFileList(FRecurseFolders, True);
  end;

  function TfAddFilesWizard.ProcPage_GetCurrentStatus: WideString;
  begin
     // Если процесс активен, отображаем прогресс
    if FProcessingFiles then
      Result := DKLangConstW('SWzAddFiles_Processing', [ProcPage_GetProgressCur+1, FInitialFileCount, FCountFailed, FFileList.Files[0]])
     // Иначе пишем информацию о возможности продолжения
    else
      Result := DKLangConstW('SWzAddFiles_Paused', [FPics.Count, FCountFailed]);
  end;

  function TfAddFilesWizard.ProcPage_GetProcessingActive: Boolean;
  begin
    Result := FProcessingFiles;
  end;

  function TfAddFilesWizard.ProcPage_GetProgressCur: Integer;
  begin
    Result := FInitialFileCount-FFileList.Count;
  end;

  function TfAddFilesWizard.ProcPage_GetProgressMax: Integer;
  begin
    Result := FInitialFileCount;
  end;

  procedure TfAddFilesWizard.ProcPage_PaintThumbnail(Bitmap32: TBitmap32);
  begin
    if FLastProcessedPic<>nil then PaintThumbnail(FLastProcessedPic, Bitmap32);
  end;

  procedure TfAddFilesWizard.SetShowPreview(Value: Boolean);
  begin
    if FShowPreview<>Value then begin
      FShowPreview := Value;
      UpdatePreviewVisibility;
    end;
  end;

  procedure TfAddFilesWizard.SettingsInitialLoad(rif: TPhoaRegIniFile);
  begin
    inherited SettingsInitialLoad(rif);
    FDefaultPath         := rif.ReadString ('', 'DefaultFolder',       '');
    FRecurseFolders      := rif.ReadBool   ('', 'RecurseFolders',      True);
    FShowAdvancedOptions := rif.ReadBool   ('', 'ShowAdvancedOptions', False);
    FFilter_Presence     := TAddFilePresenceFilter(
                            rif.ReadInteger('', 'FilterPresence',      0));
    FFilter_Masks        := rif.ReadString ('', 'FilterMasks',         '*.*');
    FFilter_DateFrom     := rif.ReadInteger('', 'FilterDateFrom',      -1);
    FFilter_DateTo       := rif.ReadInteger('', 'FilterDateTo',        -1);
    FFilter_TimeFrom     := StrToTimeDef(
                            rif.ReadString ('', 'FilterTimeFrom',      ''), -1, AppFormatSettings);
    FFilter_TimeTo       := StrToTimeDef(
                            rif.ReadString ('', 'FilterTimeTo',        ''), -1, AppFormatSettings);
    FFilter_SizeFrom     := rif.ReadInteger('', 'FilterSizeFrom',      0);
    FFilter_SizeFromUnit := TFileSizeUnit(
                            rif.ReadInteger('', 'FilterSizeFromUnit',  Byte(fsuKBytes)));
    FFilter_SizeTo       := rif.ReadInteger('', 'FilterSizeTo',        999999999);
    FFilter_SizeToUnit   := TFileSizeUnit(
                            rif.ReadInteger('', 'FilterSizeToUnit',    Byte(fsuKBytes)));
  end;

  procedure TfAddFilesWizard.SettingsInitialSave(rif: TPhoaRegIniFile);

    procedure PutDate(const d: TDateTime; const wsValueName: WideString);
    begin {!!! Not Unicode-enabled solution }
      if d>0 then rif.WriteInteger('', wsValueName, Trunc(d)) else rif.DeleteValue(wsValueName);
    end;

    procedure PutTime(const t: TDateTime; const wsValueName: WideString);
    begin {!!! Not Unicode-enabled solution }
      if t>=0 then rif.WriteString('', wsValueName, FormatDateTime('hh:nn', t)) else rif.DeleteValue(wsValueName);
    end;

  begin
    inherited SettingsInitialSave(rif);
    rif.WriteString ('', 'DefaultFolder',       FDefaultPath);
    rif.WriteBool   ('', 'RecurseFolders',      FRecurseFolders);
    rif.WriteBool   ('', 'ShowAdvancedOptions', FShowAdvancedOptions);
    rif.WriteInteger('', 'FilterPresence',      Byte(FFilter_Presence));
    rif.WriteString ('', 'FilterMasks',         FFilter_Masks);
    PutDate(FFilter_DateFrom, 'FilterDateFrom');
    PutDate(FFilter_DateTo,   'FilterDateTo');
    PutTime(FFilter_TimeFrom, 'FilterTimeFrom');
    PutTime(FFilter_TimeTo,   'FilterTimeTo');
    rif.WriteInteger('', 'FilterSizeFrom',      FFilter_SizeFrom);
    rif.WriteInteger('', 'FilterSizeFromUnit',  Byte(FFilter_SizeFromUnit));
    rif.WriteInteger('', 'FilterSizeTo',        FFilter_SizeTo);
    rif.WriteInteger('', 'FilterSizeToUnit',    Byte(FFilter_SizeToUnit));
  end;

  procedure TfAddFilesWizard.SettingsLoad(rif: TPhoaRegIniFile);
  var r: TRect;
  begin
    inherited SettingsLoad(rif);
     // Восстанавливаем параметры просмотра
    FShowPreview := rif.ReadBool   ('', 'ShowPreview',   False);
    r            := StrToRect(
                    rif.ReadString ('', 'PreviewBounds', ''),
                    Rect(-1, -1, -1, -1));
    if (r.Left>=0) and (r.Top>=0) then begin
      dpPreview.FloatingPosition := r.TopLeft;
      dpPreview.FloatingWidth    := r.Right-r.Left;
      dpPreview.FloatingHeight   := r.Bottom-r.Top;
    end else
      dpPreview.FloatingPosition := pMain.ClientToScreen(Point(40, 40));
  end;

  procedure TfAddFilesWizard.SettingsSave(rif: TPhoaRegIniFile);
  begin
    inherited SettingsSave(rif);
     // Сохраняем параметры просмотра
    rif.WriteBool   ('', 'ShowPreview', FShowPreview);
    rif.WriteString (
      '',
      'PreviewBounds',
      RectToStr(Bounds(
        dpPreview.FloatingPosition.x,
        dpPreview.FloatingPosition.y,
        dpPreview.FloatingWidth,
        dpPreview.FloatingHeight)));
  end;

  procedure TfAddFilesWizard.StartFileProcessing;
  begin
    FProcessingFiles := True;
     // Создаём протокол
    if FLog=nil then FLog := TTntStringList.Create;
     // Удаляем неотмеченные файлы
    FFileList.DeleteUnchecked;
     // Запоминаем исходное количество файлов
    FInitialFileCount := FFileList.Count;
    FLastProcessedPic := nil;
     // Обновляем страницу прогресса
    UpdateProgressInfo;
     // Запускаем поток
    FFileProcessingInterrupted := False;
    FAddFilesThread := TAddFilesThread.Create(Self);
  end;

  procedure TfAddFilesWizard.ThreadFileProcessed;
  var iPicID: Integer;
  begin
     // Проверяем, чем кончилось добавление. Ошибка - это когда нет картинки
    if FAddFilesThread.AddedPic=nil then
      Inc(FCountFailed)
    else begin
       // Если новое изображение - распределяем новый ID
      iPicID := FAddFilesThread.AddedPic.ID;
      if iPicID=0 then begin
        iPicID := FFreePicID;
        Inc(FFreePicID);
      end;
      FAddFilesThread.AddedPic.PutToList(FPics, iPicID);
       // Обновляем статус диалога
      HasUpdates := True;
    end;
     // Удаляем обработанный файл
    FFileList.Delete(0);
    FLastProcessedPic := FAddFilesThread.AddedPic;
     // Если обработан весь список, прерываем поток
    if (FFileList.Count=0) or FFileProcessingInterrupted then begin
      FProcessingFiles := False;
      FAddFilesThread.Terminate;
      FAddFilesThread := nil;
      FLastProcessedPic := nil;
       // Если список пуст, закрываем форму Мастера/показываем протокол
      if FFileList.Count=0 then
        if (FCountFailed=0) and SettingValueBool(ISettingID_Dlgs_APW_LogOnErrOnly) then
          ModalResult := mrOK
        else
          Controller.SetVisiblePageID(IWzAddFilesPageID_Log, pcmNextBtn);
    end;
     // Уведомляем страницу прогресса
    UpdateProgressInfo;
  end;

  procedure TfAddFilesWizard.UpdatePreview;
  var
    bLoaded: Boolean;
    wsFile: WideString;
    PreviewInfoIntf: IPhoaWizardPage_PreviewInfo;
    ImgSize: TSize;
  begin
    bLoaded := False;
     // Если окно просмотра отображается, получаем предметный интерфейс информации для просмотра и имя текущего файла
    if dpPreview.Visible and Supports(Controller.VisiblePage, IPhoaWizardPage_PreviewInfo, PreviewInfoIntf) then begin
      wsFile := PreviewInfoIntf.CurrentFileName;
       // Если файл задан, загружаем изображение
      if wsFile<>'' then
        try
          LoadGraphicFromFile(wsFile, iPreview.Bitmap, Size(iPreview.Width, iPreview.Height), ImgSize, nil);
          bLoaded := True;
        except
        end;
    end;
     // Если реального изображения не было загружено, стираем Bitmap
    if not bLoaded then iPreview.Bitmap := nil;
     // Обновляем масштаб
    if dpPreview.Visible then UpdatePreviewScale;
  end;

  procedure TfAddFilesWizard.UpdatePreviewScale;
  var sScale: Single;
  begin
    if iPreview.Bitmap.Empty then
      sScale := 1
    else
      sScale := MinS(1, MinS(iPreview.Width/iPreview.Bitmap.Width, iPreview.Height/iPreview.Bitmap.Height));
    iPreview.Scale := sScale;
  end;

  procedure TfAddFilesWizard.UpdatePreviewVisibility;
  begin
     // Окно просмотра видимо при включенном ShowPreview на страницах, поддерживающих интерфейс
     //   IPhoaWizardPage_PreviewInfo
    FUpdatingPreviewVisibility := True;
    try
      dpPreview.Visible := FShowPreview and Supports(Controller.VisiblePage, IPhoaWizardPage_PreviewInfo);
    finally
      FUpdatingPreviewVisibility := False;
    end;
     // Загружаем или стираем изображение
    UpdatePreview;
  end;

  procedure TfAddFilesWizard.UpdateProgressInfo;
  begin
    Controller.ItemsByID[IWzAddFilesPageID_Processing].Perform(WM_PAGEUPDATE, 0, 0);
    StateChanged;
  end;

end.

