//**********************************************************************************************************************
//  $Id: udFileOpsWizard.pas,v 1.29 2005-02-12 15:36:37 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udFileOpsWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Registry,
  GraphicEx, GR32,
  phWizard, phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars, phGraphics,
  Placemnt, StdCtrls, ExtCtrls, phWizForm, DKLang;

type
   // Exception
  EFileOpError = class(Exception);

  TFileOpThread = class;

   // Объект файла и ссылок на подходящие изображения
  PFileLink = ^TFileLink; 
  TFileLink = class(TObject)
  private
     // Prop storage
    FFileName: String;
    FFilePath: String;
    FFileSize: Integer;
    FPics: IPhoaMutablePicList;
    FFileTime: TDateTime;
  public
    constructor Create(const sFileName, sFilePath: String; iFileSize: Integer; const dFileTime: TDateTime);
     // Props
     // -- Имя файла
    property FileName: String read FFileName;
     // -- Путь к файлу
    property FilePath: String read FFilePath;
     // -- Размер файла
    property FileSize: Integer read FFileSize;
     // -- Дата/время модификации файла
    property FileTime: TDateTime read FFileTime;
     // -- Список ссылок на изображения, для которых файл является подходящим вариантом ссылки
    property Pics: IPhoaMutablePicList read FPics; 
  end;

   // Список объектов TFileLink
  TFileLinks = class(TList)
  private
    function GetItems(Index: Integer): TFileLink;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
     // Добавляет новый объект файла и возвращает ссылку на него
    function Add(const sFileName, sFilePath: String; iFileSize: Integer; const dFileTime: TDateTime): TFileLink;
     // Props
     // -- Файлы по индексу
    property Items[Index: Integer]: TFileLink read GetItems; default;
  end;

   // Форма Мастера файловых операций
  TdFileOpsWizard = class(TPhoaWizardForm, IPhoaWizardPageHost_Log, IPhoaWizardPageHost_Process)
    dklcMain: TDKLanguageController;
  private
     // Рабочий поток (выполняющий операцию)
    FFileOpThread: TFileOpThread;
     // Протокол операции
    FLog: TStrings;
     // Флаг того, что производится обработка
    FProcessing: Boolean;
     // Флаг прерывания обработки
    FProcessingInterrupted: Boolean;
     // Изначальное количество изображений
    FInitialPicCount: Integer;
     // Счётчик успешно обработанных файлов/изображений
    FCountSucceeded: Integer;
     // Счётчик ошибок
    FCountErrors: Integer;
     // True, если в момент вызова Мастера фокус был у окна эскизов
    FSelPicsByDefault: Boolean;
     // Prop storage
    FApp: IPhotoAlbumApp;
    FCDOpt_CopyExecutable: Boolean;
    FCDOpt_CopyIniSettings: Boolean;
    FCDOpt_CreateAutorun: Boolean;
    FCDOpt_CreatePhoa: Boolean;
    FCDOpt_IncludeViews: Boolean;
    FCDOpt_MediaLabel: String;
    FCDOpt_PhoaDesc: String;
    FCDOpt_PhoaFileName: String;
    FDelFile_DeleteToRecycleBin: Boolean;
    FDestinationFolder: String;
    FExportedProject: IPhotoAlbumProject;
    FFileOpKind: TFileOperationKind;
    FMoveFile_AllowDuplicating: Boolean;
    FMoveFile_Arranging: TFileOpMoveFileArranging;
    FMoveFile_BaseGroup: IPhotoAlbumPicGroup;
    FMoveFile_BasePath: String;
    FMoveFile_DeleteOriginal: Boolean;
    FMoveFile_DeleteToRecycleBin: Boolean;
    FMoveFile_FileNameFormat: String;
    FMoveFile_NoOriginalMode: TFileOpMoveFileNoOriginalMode;
    FMoveFile_OverwriteMode: TFileOpMoveFileOverwriteMode;
    FMoveFile_RenameFiles: Boolean;
    FMoveFile_ReplaceChar: Char;
    FMoveFile_UseCDOptions: Boolean;
    FProjectChanged: Boolean;
    FRepair_DeleteUnmatchedPics: Boolean;
    FRepair_FileLinks: TFileLinks;
    FRepair_LookSubfolders: Boolean;
    FRepair_MatchFlags: TFileOpRepairMatchFlags;
    FRepair_RelinkFilesInUse: Boolean;
    FSelectedGroups: IPhotoAlbumPicGroupList;
    FSelectedPics: IPhotoAlbumPicList;
    FSelPicMode: TFileOpSelPicMode;
    FSelPicValidityFilter: TFileOpSelPicValidityFilter;
     // Отбирает изображения для операции в FSelectedPics - на основании выбранного режима и выбранных групп
    procedure DoSelectPictures;
     // Начинает обработку операции
    procedure StartProcessing;
     // Прерывает обработку операции
    procedure InterruptProcessing;
     // Создаёт экспортируемый фотоальбом
    procedure CreateExportedPhoa;
     // Завершающая процедура обработки (вызывается только после обработки всех изображений)
    procedure FinalizeProcessing;
     // Обновляет информацию о состоянии процесса
    procedure UpdateProgressInfo;
     // Вызывается потоком, обрабатывающим изображения, для уведомления о том, что изображение обработано
    procedure ThreadPicProcessed;
     // Добавление строки в протокол
    procedure LogSuccess(const s: String; const aParams: Array of const);
    procedure LogFailure(const s: String; const aParams: Array of const);
     // Составляет список файлов-кандидатов для восстановления ссылок
    procedure Repair_SelectFiles;
     // IPhoaWizardPageHost_Log
    function  LogPage_GetLog(iPageID: Integer): TStrings;
    function  IPhoaWizardPageHost_Log.GetLog = LogPage_GetLog;
     // IPhoaWizardPageHost_Process
    procedure ProcPage_PaintThumbnail(Bitmap32: TBitmap32);
    function  ProcPage_GetCurrentStatus: String;
    function  ProcPage_GetProcessingActive: Boolean;
    function  ProcPage_GetProgressCur: Integer;
    function  ProcPage_GetProgressMax: Integer;
    procedure IPhoaWizardPageHost_Process.StartProcessing     = StartProcessing;
    procedure IPhoaWizardPageHost_Process.StopProcessing      = InterruptProcessing;
    procedure IPhoaWizardPageHost_Process.PaintThumbnail      = ProcPage_PaintThumbnail;
    function  IPhoaWizardPageHost_Process.GetCurrentStatus    = ProcPage_GetCurrentStatus;
    function  IPhoaWizardPageHost_Process.GetProcessingActive = ProcPage_GetProcessingActive;
    function  IPhoaWizardPageHost_Process.GetProgressCur      = ProcPage_GetProgressCur;
    function  IPhoaWizardPageHost_Process.GetProgressMax      = ProcPage_GetProgressMax;
  protected
    procedure InitializeWizard; override;
    procedure FinalizeWizard; override;
    function  IsBtnBackEnabled: Boolean; override;
    function  IsBtnNextEnabled: Boolean; override;
    function  IsBtnCancelEnabled: Boolean; override;
    function  GetNextPageID: Integer; override;
    function  GetFormRegistrySection: String; override;
    procedure SettingsStore(rif: TRegIniFile); override;
    procedure SettingsRestore(rif: TRegIniFile); override;
    function  PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean; override;
    procedure PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer); override;
  public
     // Props
     // -- Приложение
    property App: IPhotoAlbumApp read FApp;
     // -- Создание CD/DVD: True, если нужно создавать фотоальбом из копируемых изображений
    property CDOpt_CreatePhoa: Boolean read FCDOpt_CreatePhoa write FCDOpt_CreatePhoa;
     // -- Создание CD/DVD: относительное имя файла фотоальбома
    property CDOpt_PhoaFileName: String read FCDOpt_PhoaFileName write FCDOpt_PhoaFileName;
     // -- Создание CD/DVD: описание фотоальбома
    property CDOpt_PhoaDesc: String read FCDOpt_PhoaDesc write FCDOpt_PhoaDesc;
     // -- Создание CD/DVD: True, если нужно включить имеющиеся представления в создаваемый фотоальбом
    property CDOpt_IncludeViews: Boolean read FCDOpt_IncludeViews write FCDOpt_IncludeViews;
     // -- Создание CD/DVD: True, если нужно также скопировать исполняемый файл
    property CDOpt_CopyExecutable: Boolean read FCDOpt_CopyExecutable write FCDOpt_CopyExecutable;
     // -- Создание CD/DVD: True, если нужно сохранить текущие настройки в phoa.ini
    property CDOpt_CopyIniSettings: Boolean read FCDOpt_CopyIniSettings write FCDOpt_CopyIniSettings;
     // -- Создание CD/DVD: True, если нужно создать файл autorun.inf
    property CDOpt_CreateAutorun: Boolean read FCDOpt_CreateAutorun write FCDOpt_CreateAutorun;
     // -- Создание CD/DVD: метка носителя для autorun.inf
    property CDOpt_MediaLabel: String read FCDOpt_MediaLabel write FCDOpt_MediaLabel;
     // -- Удаление: если True, удалять файлы в корзину, иначе - удалять совсем
    property DelFile_DeleteToRecycleBin: Boolean read FDelFile_DeleteToRecycleBin write FDelFile_DeleteToRecycleBin;
     // -- Папка назначения, с которой производятся операции
    property DestinationFolder: String read FDestinationFolder write FDestinationFolder;
     // -- Проект, предназначенный для экспорта в процессе копирования/перемещения файлов
    property ExportedProject: IPhotoAlbumProject read FExportedProject;
     // -- Выбранная операция с файлами изображений
    property FileOpKind: TFileOperationKind read FFileOpKind write FFileOpKind;
     // -- Копирование/перемещение: режим размещения файлов
    property MoveFile_Arranging: TFileOpMoveFileArranging read FMoveFile_Arranging write FMoveFile_Arranging;
     // -- Копирование/перемещение: путь, относительно которого создавать папки с файлами (при
     //    MoveFileArranging=fomfaMaintainFolderLayout)
    property MoveFile_BasePath: String read FMoveFile_BasePath write FMoveFile_BasePath;
     // -- Копирование/перемещение: группа, относительно которой создавать папки с файлами (при
     //    MoveFileArranging=fomfaMaintainGroupLayout)
    property MoveFile_BaseGroup: IPhotoAlbumPicGroup read FMoveFile_BaseGroup write FMoveFile_BaseGroup;
     // -- Копирование/перемещение: если True, делать дубликаты файлов, помещая их в папки, соответствующие группам (при
     //    MoveFileArranging=fomfaMaintainGroupLayout)
    property MoveFile_AllowDuplicating: Boolean read FMoveFile_AllowDuplicating write FMoveFile_AllowDuplicating;
     // -- Копирование/перемещение: если True, переименовывать файлы, используя значение MoveFile_FileNameFormat
    property MoveFile_RenameFiles: Boolean read FMoveFile_RenameFiles write FMoveFile_RenameFiles;
     // -- Копирование/перемещение: формат имени файла, используемый для переименовывания файлов при
     //    MoveFile_RenameFiles=True
    property MoveFile_FileNameFormat: String read FMoveFile_FileNameFormat write FMoveFile_FileNameFormat;
     // -- Копирование/перемещение: поведение при отсутствии исходного файла при перемещении
    property MoveFile_NoOriginalMode: TFileOpMoveFileNoOriginalMode read FMoveFile_NoOriginalMode write FMoveFile_NoOriginalMode;
     // -- Копирование/перемещение: режим перезаписи существующих файлов
    property MoveFile_OverwriteMode: TFileOpMoveFileOverwriteMode read FMoveFile_OverwriteMode write FMoveFile_OverwriteMode;
     // -- Копирование/перемещение: символ, на который заменять недопустимые для пути/имени файлов символы
    property MoveFile_ReplaceChar: Char read FMoveFile_ReplaceChar write FMoveFile_ReplaceChar;
     // -- Копирование/перемещение: если True, удалять оригинальный файл
    property MoveFile_DeleteOriginal: Boolean read FMoveFile_DeleteOriginal write FMoveFile_DeleteOriginal;
     // -- Копирование/перемещение: если True, удалять оригинальный файл в корзину, иначе - удалять совсем
    property MoveFile_DeleteToRecycleBin: Boolean read FMoveFile_DeleteToRecycleBin write FMoveFile_DeleteToRecycleBin;
     // -- Копирование/перемещение: если True, отображать и использовать опции создания CD/DVD
    property MoveFile_UseCDOptions: Boolean read FMoveFile_UseCDOptions write FMoveFile_UseCDOptions;
     // -- True, если операция внесла изменения в проект
    property ProjectChanged: Boolean read FProjectChanged;
     // -- Восстановление ссылок: флаги поиска совпадения для восстановления
    property Repair_MatchFlags: TFileOpRepairMatchFlags read FRepair_MatchFlags write FRepair_MatchFlags;
     // -- Восстановление ссылок: если True, просматривать вложенные в папку назначения папки в поисках подходящего файла
    property Repair_LookSubfolders: Boolean read FRepair_LookSubfolders write FRepair_LookSubfolders;
     // -- Восстановление ссылок: если True, переписывать ссылки использующихся файлов, подходящих для исправляемого
     //    изображения, на исправляемое изображение, а прежнее изображение удалять. Если False, пропускать уже
     //    используемые файлы
    property Repair_RelinkFilesInUse: Boolean read FRepair_RelinkFilesInUse write FRepair_RelinkFilesInUse;
     // -- Восстановление ссылок: если True, удалять изображение, которым так и не было найдено соответствующего файла
    property Repair_DeleteUnmatchedPics: Boolean read FRepair_DeleteUnmatchedPics write FRepair_DeleteUnmatchedPics;
     // -- Восстановление ссылок: список найденных файлов и ссылок на изображения
    property Repair_FileLinks: TFileLinks read FRepair_FileLinks;
     // -- Выбранные группы (список не пустой только для SelPicMode=fospmSelGroups)
    property SelectedGroups: IPhotoAlbumPicGroupList read FSelectedGroups;
     // -- Отобранные для операции изображения
    property SelectedPics: IPhotoAlbumPicList read FSelectedPics;
     // -- Режим выбора изображений
    property SelPicMode: TFileOpSelPicMode read FSelPicMode write FSelPicMode;
     // -- Фильтр выбора изображений по наличию соответствующего файла
    property SelPicValidityFilter: TFileOpSelPicValidityFilter read FSelPicValidityFilter write FSelPicValidityFilter;
  end;

   // Поток-обработчик файловых операций
  TFileOpThread = class(TThread)
  private
     // Форма-владелец потока
    FWizard: TdFileOpsWizard;
     // Prop storage
    FErrorOccured: Boolean;
    FChangesMade: Boolean;
     // Поля для AskOverwrite()
    FOverwriteFileName: String;
    FOverwriteResults: TMessageBoxResults;
     // Процедуры, выполняющие операцию для отдельного изображения
    procedure DoCopyMovePic(Pic: IPhotoAlbumPic);
    procedure DoDelPicAndFile(Pic: IPhotoAlbumPic);
    procedure DoRebuildThumb(Pic: IPhotoAlbumPic);
    procedure DoRepairFileLink(Pic: IPhotoAlbumPic);
     // Процедура удаления файла
    procedure DoDeleteFile(const sFileName: String; bDelToRecycleBin: Boolean);
     // Удаляет изображение из фотоальбома (также ссылки на него из групп)
    procedure DoDeletePic(Pic: IPhotoAlbumPic);
     // Обновляет ссылку на файл (предварительно проверяя допустимость этого)
    procedure DoUpdateFileLink(Pic: IPhotoAlbumPic; const sNewFileName: String);
     // Спрашивает возможность перезаписи файла (вызывается в Synchronize())
    procedure AskOverwrite;
  protected
    procedure Execute; override;
  public
    constructor Create(Wizard: TdFileOpsWizard);
     // Props
     // -- True, если последняя операция внесла изменения в фотоальбом
    property ChangesMade: Boolean read FChangesMade;
     // -- True, если произошла ошибка при выполнении операции
    property ErrorOccured: Boolean read FErrorOccured;
  end;

   // Показывает мастер операций с файлами изображений. Возвращает True, если что-то в фотоальбоме было изменено
   //   AApp - приложение
  function DoFileOperations(AApp: IPhotoAlbumApp; out bProjectChanged: Boolean): Boolean;

implementation
{$R *.dfm}
uses
  ShellAPI,
  phUtils, ufrWzPage_Log,
  ufrWzPage_Processing, ufrWzPageFileOps_SelTask, ufrWzPageFileOps_SelPics, ufrWzPageFileOps_SelFolder,
  ufrWzPageFileOps_MoveOptions, ufrWzPageFileOps_DelOptions, ufrWzPageFileOps_RepairOptions,
  Main, ufrWzPageFileOps_CDOptions, ufrWzPageFileOps_RepairSelLinks,
  ufrWzPageFileOps_MoveOptions2, phSettings, udMsgBox;

  function DoFileOperations(AApp: IPhotoAlbumApp; out bProjectChanged: Boolean): Boolean;
  begin
    with TdFileOpsWizard.Create(Application) do
      try
        FApp := AApp;
        FSelPicsByDefault := FApp.FocusedControl=pafcThumbViewer;
        Result := Execute;
        bProjectChanged := ProjectChanged;
      finally
        Free;
      end;
  end;

   // Exception raising
  procedure FileOpError(const sConstName: String; const aParams: Array of const);
  begin
    raise EFileOpError.Create(ConstVal(sConstName, aParams));
  end;

   //===================================================================================================================
   // TFileLink
   //===================================================================================================================

  constructor TFileLink.Create(const sFileName, sFilePath: String; iFileSize: Integer; const dFileTime: TDateTime);
  begin
    inherited Create;
    FFileName := sFileName;
    FFilePath := sFilePath;
    FFileSize := iFileSize;
    FFileTime := dFileTime;
    FPics     := NewPhotoAlbumPicList(True);
  end;

   //===================================================================================================================
   // TFileLinks
   //===================================================================================================================

  function TFileLinks.Add(const sFileName, sFilePath: String; iFileSize: Integer; const dFileTime: TDateTime): TFileLink;
  begin
    Result := TFileLink.Create(sFileName, sFilePath, iFileSize, dFileTime);
    inherited Add(Result);
  end;

  function TFileLinks.GetItems(Index: Integer): TFileLink;
  begin
    Result := TFileLink(Get(Index));
  end;

  procedure TFileLinks.Notify(Ptr: Pointer; Action: TListNotification);
  begin
    if Action=lnDeleted then TFileLink(Ptr).Free;
  end;

   //===================================================================================================================
   // TFileOpThread
   //===================================================================================================================

  procedure TFileOpThread.AskOverwrite;
  begin
    FOverwriteResults := PhoaMsgBox(mbkConfirmWarning, 'SConfirm_FileOverwrite', [FOverwriteFileName], True, False, [mbbYes, mbbYesToAll, mbbNo, mbbNoToAll, mbbCancel]);
  end;

  constructor TFileOpThread.Create(Wizard: TdFileOpsWizard);
  begin
    inherited Create(True);
    FWizard := Wizard;
    FreeOnTerminate := True;
    Resume;
  end;

  procedure TFileOpThread.DoCopyMovePic(Pic: IPhotoAlbumPic);
  var
    sSrcFileName, sSrcPath, sSrcFullFileName, sDestPath, sTargetPath, sTargetFileName: String;
    SLRelTargetPaths: TStringList;
    i: Integer;

     // Добавляет относительный путь к изображению в SLRelTargetPaths, если группа выбрана и оно присутствует в группе
     //   Group. Рекурсивно вызывает себя для вложенных групп
    procedure AddPathIfPicInGroup(Group: IPhotoAlbumPicGroup);
    var
      i: Integer;
      g: IPhotoAlbumPicGroup;
      sRelPath: String;
      bGroupSelected: Boolean;
    begin
       // Проверяем выбранность группы
      case FWizard.SelPicMode of
        fospmSelPics:         bGroupSelected := Group.ID=FWizard.App.CurGroup.ID;
        fospmAll:             bGroupSelected := True;
        else {fospmSelGroups} bGroupSelected := FWizard.SelectedGroups.IndexOfID(Group.ID)>=0;
      end;
       // Если группа выбрана, ищем изображение в группе
      if bGroupSelected and (Group.Pics.IndexOfID(Pic.ID)>=0) then begin
         // Вычисляем путь изображения относительно MoveFile_BaseGroup
        g := Group;
        sRelPath := '';
        while (g<>nil) and (g<>FWizard.MoveFile_BaseGroup) do begin
          sRelPath := ReplaceChars(g.Text, SInvalidPathChars, FWizard.MoveFile_ReplaceChar)+'\'+sRelPath;
          g := g.OwnerX;
        end;
         // Добавляем путь в список
        SLRelTargetPaths.Add(sRelPath);
         // Если дублирование файлов не позволяется, прерываем поиск
        if not FWizard.MoveFile_AllowDuplicating then Exit;
      end;
       // Повторяем то же для вложенных групп
      for i := 0 to Group.Groups.Count-1 do begin
        AddPathIfPicInGroup(Group.GroupsX[i]);
         // Если дублирование файлов не позволяется, прерываем поиск при наличии [хотя бы одной] записи пути
        if not FWizard.MoveFile_AllowDuplicating and (SLRelTargetPaths.Count>0) then Exit;
      end;
    end;

     // Возвращает отформатированное в соответствии с FWizard.MoveFile_FileNameFormat имя файла назначения для
     //   обрабатываемого изображения
    function GetFormattedTargetFileName: String;
    var
      s: String;
      i1, i2: Integer;
      PProp: TPicProperty;
    begin
      Result := '';
      s := FWizard.MoveFile_FileNameFormat;
      repeat
         // Ищем фигурные скобки в строке
        i1 := Pos('{', s);
        i2 := Pos('}', s);
         // Фигурные скобки закончились
        if (i1=0) and (i2=0) then Break;
         // Добавляем к результату начало строки, не содержащее фигурных скобок
        Result := Result+Copy(s, 1, Min(i1, i2)-1);
         // Выделяем имя свойства изображения, содержащееся между фигурных скобок
        if (i1<>0) and (i2<>0) and (i1<i2-1) then begin
          PProp := StrToPicProp(Copy(s, i1+1, i2-i1-1), True);
           // Добавляем к результату значение свойства для обрабатываемого изображения
          Result := Result+Pic.PropStrValues[PProp];
        end;
         // Удаляем обработанную часть строки из s
        Delete(s, 1, Max(i1, i2));
      until s='';
       // Добавляем к результату остаток строки (не содержащий фигурных скобок) и расширение исходного файла
      Result := Result+s+ExtractFileExt(sSrcFileName);
       // Заменяем недопустимые символы в имени файла
      Result := ReplaceChars(Result, SInvalidPathChars, FWizard.MoveFile_ReplaceChar);
    end;

     // Копирует файл в путь sTargetPath
    procedure PerformCopying(const sTargetPath: String);
    var sTargetDir, sTargetFullFileName: String;
    begin
      sTargetDir := ExcludeTrailingPathDelimiter(sTargetPath);
       // Проверяем, что исходный и целевой пути разные
      if AnsiSameText(sSrcPath, sTargetPath) then FileOpError('SErrSrcAndDestFoldersAreSame', [sTargetDir, sSrcFileName]);
       // Пытаемся создать каталог назначения
      if not ForceDirectories(sTargetDir) then FileOpError('SErrCannotCreateFolder', [sTargetDir]);
       // Проверяем перезапись файла
      sTargetFullFileName := sTargetPath+sTargetFileName;
      case FWizard.MoveFile_OverwriteMode of
        fomfomNever: if FileExists(sTargetFullFileName) then FileOpError('SErrTargetFileExists', [sTargetFullFileName]);
        fomfomPrompt:
          if FileExists(sTargetFullFileName) then begin
            FOverwriteFileName := sTargetFullFileName;
            Synchronize(AskOverwrite);
             // "Да"
            if mbrYes in FOverwriteResults then
              { do nothing }
             // "Да для всех"
            else if mbrYesToAll in FOverwriteResults then
              FWizard.MoveFile_OverwriteMode := fomfomAlways
             // "Нет"
            else if mbrNo in FOverwriteResults then
              FileOpError('SLogEntry_UserDeniedFileOverwrite', [sTargetFullFileName])
             // "Нет для всех"
            else if mbrNoToAll in FOverwriteResults then begin
              FWizard.MoveFile_OverwriteMode := fomfomNever;
              FileOpError('SErrTargetFileExists', [sTargetFullFileName]);
             // "Отмена"
            end else begin
              FWizard.InterruptProcessing;
              FileOpError('SLogEntry_UserAbort', []);
            end;
          end;
      end;
       // Копируем файл
      if not CopyFile(PChar(sSrcFullFileName), PChar(sTargetFullFileName), False) then RaiseLastOSError;
       // Протоколируем успех
      FWizard.LogSuccess('SLogEntry_FileCopiedOK', [sSrcFullFileName, sTargetFullFileName]);
    end;

  begin
     // Получаем имя/путь исходного файла
    sSrcFullFileName := Pic.FileName;
    sSrcFileName     := ExtractFileName(sSrcFullFileName);
    sSrcPath         := ExtractFilePath(sSrcFullFileName);
    sDestPath        := IncludeTrailingPathDelimiter(FWizard.DestinationFolder);
    sTargetPath      := '';
    sTargetFileName  := GetFormattedTargetFileName;
    case FWizard.MoveFile_Arranging of
       // Все в один каталог - каталог назначения
      fomfaPutFlatly: begin
        sTargetPath := sDestPath;
        PerformCopying(sTargetPath);
      end;
       // Переместить в каталог, сохранив расположение относительно каталога MoveFile_BasePath
      fomfaMaintainFolderLayout: begin
         // Отрезаем часть пути, относительно которой строится новый путь. Удаляем ':' на случай, если путь содержит имя
         //   диска
        sTargetPath := StringReplace(Copy(sSrcPath, Length(FWizard.MoveFile_BasePath)+1, MaxInt), ':', '', [rfReplaceAll]);
         // Удаляем все '\' в начале (в случае UNC-пути, или MoveFile_BasePath без '\' в конце)
        while (sTargetPath<>'') and (sTargetPath[1]='\') do Delete(sTargetPath, 1, 1);
        sTargetPath := sDestPath+sTargetPath;
        PerformCopying(sTargetPath);
      end;
       // Переместить в каталог, сохранив расположение папок относительно группы MoveFile_BaseGroup
      else {fomfaMaintainGroupLayout} begin
        SLRelTargetPaths := TStringList.Create;
        try
          SLRelTargetPaths.Sorted     := True;
          SLRelTargetPaths.Duplicates := dupIgnore;
           // Заполняем SLRelTargetPaths путями назначения
          AddPathIfPicInGroup(FWizard.App.Project.ViewRootGroupX);
           // Если что-то есть (по идее, должно быть всегда)
          if SLRelTargetPaths.Count=0 then FileOpError('SErrNoTargetPathDetermined', [Pic.FileName]);
          sTargetPath := sDestPath+SLRelTargetPaths[0];
          for i := 0 to iif(FWizard.MoveFile_AllowDuplicating, SLRelTargetPaths.Count-1, 0) do PerformCopying(sDestPath+SLRelTargetPaths[i]);
        finally
          SLRelTargetPaths.Free;
        end;
      end;
    end;
     // Если режим - перемещение
    if FWizard.FileOpKind=fokMoveFiles then begin
       // Исправляем ссылку
      DoUpdateFileLink(Pic, sTargetPath+sTargetFileName);
       // Удаляем исходный файл
      if FWizard.MoveFile_DeleteOriginal then DoDeleteFile(sSrcFullFileName, FWizard.MoveFile_DeleteToRecycleBin);
    end;
     // Если включен режим создания фотоальбома, исправляем ссылку на файл в соответствующем изображении
    if FWizard.ExportedProject<>nil then FWizard.ExportedProject.PicsX.ItemsByIDX[Pic.ID].FileName := sTargetPath+sTargetFileName;
  end;

  procedure TFileOpThread.DoDeleteFile(const sFileName: String; bDelToRecycleBin: Boolean);
  var SFOS: TSHFileOpStruct;
  begin
     // Проверяем наличие файла. Если его нет - удалять, пожалуй, не стоит
    if not FileExists(sFileName) then
      FWizard.LogSuccess('SLogEntry_SkipInsteadOfDelFile', [sFileName])
     // Иначе удаляем файл
    else
       // -- В корзину
      if bDelToRecycleBin then begin
        SFOS.Wnd    := FWizard.Handle;
        SFOS.wFunc  := FO_DELETE;
        SFOS.pFrom  := PChar(sFileName+#0);
        SFOS.pTo    := #0;
        SFOS.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
        if SHFileOperation(SFOS)=0 then
          FWizard.LogSuccess('SLogEntry_FileRecycledOK', [sFileName])
        else
          FileOpError('SLogEntry_FileRecyclingError', [sFileName]);
       // -- Совсем
      end else
        if DeleteFile(sFileName) then
          FWizard.LogSuccess('SLogEntry_FileDeletedOK', [sFileName])
        else
          FileOpError('SLogEntry_FileDeletingError', [sFileName]);
  end;

  procedure TFileOpThread.DoDeletePic(Pic: IPhotoAlbumPic);

     // Рекурсивно удаляет изображение из группы Group и её подгрупп
    procedure DelID(Group: IPhotoAlbumPicGroup; iID: Integer);
    var i: Integer;
    begin
      Group.PicsX.Remove(iID);
      for i := 0 to Group.Groups.Count-1 do DelID(Group.GroupsX[i], iID);
    end;

  begin
    FChangesMade := True;
     // Удаляем все ссылки на изображение
    DelID(FWizard.App.Project.RootGroupX, Pic.ID);
     // Удаляем изображение из проекта
    FWizard.App.Project.PicsX.Remove(Pic.ID);
  end;

  procedure TFileOpThread.DoDelPicAndFile(Pic: IPhotoAlbumPic);
  var sFileName: String;
  begin
     // Удаляем файл
    sFileName := Pic.FileName;
    DoDeleteFile(sFileName, FWizard.DelFile_DeleteToRecycleBin);
     // Удаляем изображение
    DoDeletePic(Pic);
     // Протоколируем успех
    FWizard.LogSuccess('SLogEntry_PicDeletedOK', [sFileName]);
  end;

  procedure TFileOpThread.DoRebuildThumb(Pic: IPhotoAlbumPic);
  var iPrevThumbSize, iPrevFileSize: Integer;
  begin
     // Запоминаем прежние размеры эскиза и файла
    iPrevThumbSize := Length(Pic.ThumbnailData);
    iPrevFileSize  := Pic.FileSize;
     // Перестраиваем эскиз
    Pic.ReloadPicFileData(
      FWizard.App.Project.ThumbnailSize,
      TPhoaStretchFilter(SettingValueInt(ISettingID_Browse_ViewerStchFilt)),
      FWizard.App.Project.ThumbnailQuality);
     // Протоколируем успех
    FWizard.LogSuccess(
      'SLogEntry_ThumbRebuiltOK',
      [Pic.FileName, iPrevThumbSize, Length(Pic.ThumbnailData), iPrevFileSize, Pic.FileSize]);
    FChangesMade := True;
  end;

  procedure TFileOpThread.DoRepairFileLink(Pic: IPhotoAlbumPic);
  begin
    //#ToDo3: Написать repair routine
  end;

  procedure TFileOpThread.DoUpdateFileLink(Pic: IPhotoAlbumPic; const sNewFileName: String);
  var sPrevFileName: String;
  begin
     // Проверяем, можно ли исправить путь
    if not AnsiSameText(Pic.FileName, sNewFileName) and (FWizard.App.Project.Pics.IndexOfFileName(sNewFileName)>=0) then
      FileOpError('SLogEntry_PicRelinkingError', [sNewFileName]);
     // Исправляем (даже при одинаковом тексте, т.к. регистр может отличаться)
    sPrevFileName := Pic.FileName;
    if sPrevFileName<>sNewFileName then begin
      Pic.FileName := sNewFileName;
      FWizard.LogSuccess('SLogEntry_PicRelinkedOK', [sPrevFileName, sNewFileName]);
      FChangesMade := True;
    end;
  end;

  procedure TFileOpThread.Execute;
  var
    Pic: IPhotoAlbumPic;
    sFileName: String;
  begin
    while not Terminated do begin
       // Обрабатываем изображение
      Pic := FWizard.SelectedPics[0];
      sFileName := Pic.FileName;
      FErrorOccured := False;
      FChangesMade := False;
      try
        case FWizard.FileOpKind of
          fokCopyFiles,
            fokMoveFiles:     DoCopyMovePic(Pic);
          fokDeleteFiles:     DoDelPicAndFile(Pic);
          fokRebuildThumbs:   DoRebuildThumb(Pic);
          fokRepairFileLinks: DoRepairFileLink(Pic);
        end;
      except
        on e: Exception do begin
          FErrorOccured := True;
          FWizard.LogFailure('SLogEntry_FileOpError', [sFileName, e.Message]);
        end;
      end;
       // Регистрируем результат
      Synchronize(FWizard.ThreadPicProcessed);
    end;
  end;

   //===================================================================================================================
   // TdFileOpsWizard
   //===================================================================================================================

  procedure TdFileOpsWizard.CreateExportedPhoa;
   // Признак выбранности группы (не выбрана; выбрана; не выбрана, но выбрана одна из её подгрупп)
  type TGroupSelection = (gsNotSelected, gsSelected, gsChildSelected);

     // Возвращает выбранность группы в Мастере (только для режима SelPicMode=fospmSelGroups)
    function GetGroupSelection(Group: IPhoaPicGroup): TGroupSelection;
    var i: Integer;
    begin
      if FSelectedGroups.IndexOfID(Group.ID)>=0 then
        Result := gsSelected
      else begin
        Result := gsNotSelected;
        for i := 0 to Group.Groups.Count-1 do
          if GetGroupSelection(Group.Groups[i])<>gsNotSelected then begin
            Result := gsChildSelected;
            Break;
          end;
      end;
    end;

     // Рекурсивно добавляет выбранные группы и изображения (только для режима SelPicMode=fospmSelGroups)
    procedure AddGroup(TgtGroup, OwnerGroup, SrcGroup: IPhotoAlbumPicGroup; bUseTgtGroup: Boolean);
    var
      GS: TGroupSelection;
      i: Integer;
    begin
      GS := GetGroupSelection(SrcGroup);
       // Если группа или подгруппа выбраны - добавляем группу в иерархию
      if GS<>gsNotSelected then begin
         // Если это не корневая группа, создаём соотв. target-группу
        if not bUseTgtGroup then TgtGroup := NewPhotoAlbumPicGroup(OwnerGroup, 0);
         // Копируем свойства исходной группы. Если выбрана сама группа, добавляем в неё и изображения исходной группы
        TgtGroup.Assign(SrcGroup, True, GS=gsSelected, False);
         // Повторяем то же для всех подгрупп
        for i := 0 to SrcGroup.Groups.Count-1 do AddGroup(nil, TgtGroup, SrcGroup.GroupsX[i], False);
      end;
    end;

     // Добавляет одиночную группу в фотоальбом
    procedure AddSingleGroup(TgtGroup, SrcGroup: IPhotoAlbumPicGroup; bUseTgtAsOwnerGroup: Boolean);
    begin
       // Если это не корневая группа, создаём соотв. target-группу
      if bUseTgtAsOwnerGroup then TgtGroup := NewPhotoAlbumPicGroup(TgtGroup, 0);
       // Копируем свойства исходной группы
      TgtGroup.Assign(SrcGroup, True, False, False);
       // Добавляем выбранные изображения
      TgtGroup.PicsX.Add(FSelectedPics, True);
    end;

  begin
     // Создаём фотоальбом
    FExportedProject := NewPhotoAlbumProject;
     // Копируем настройки
    FExportedProject.Assign(FApp.Project, False);
     // Настраиваем опции
    FExportedProject.Description := FCDOpt_PhoaDesc;
    FExportedProject.FileName    := IncludeTrailingPathDelimiter(FDestinationFolder)+FCDOpt_PhoaFileName;
     // Копируем изображения
    FExportedProject.PicsX.DuplicatePics(FSelectedPics);
     // Копируем группы и изображения в них
    case SelPicMode of
      fospmSelPics:   AddSingleGroup(FExportedProject.RootGroupX, FApp.CurGroup, FApp.CurGroup<>FApp.Project.RootGroup);
      fospmAll:       FExportedProject.RootGroupX.Assign(FApp.Project.RootGroupX, True, True, True);
      fospmSelGroups: AddGroup(FExportedProject.RootGroupX, nil, FApp.Project.RootGroupX, True);
    end;
     // Копируем представления
    if FCDOpt_IncludeViews then FExportedProject.ViewsX.Assign(FApp.Project.Views);
  end;

  procedure TdFileOpsWizard.DoSelectPictures;
  var i: Integer;
  begin
     // Добавляем изображения
    FSelectedPics := NewPhotoAlbumPicList(True);
    case SelPicMode of
      fospmSelPics:   FSelectedPics.Assign(FApp.SelectedPics);
      fospmAll:       FSelectedPics.Assign(FApp.Project.Pics);
      fospmSelGroups: for i := 0 to FSelectedGroups.Count-1 do FSelectedPics.Add(FSelectedGroups[i].Pics, True);
      else            FSelectedPics := nil;
    end;
     // Удаляем [не]существующие
    if FSelPicValidityFilter<>fospvfAny then
      for i := FSelectedPics.Count-1 downto 0 do
        if FileExists(FSelectedPics[i].FileName)<>(FSelPicValidityFilter=fospvfValidOnly) then FSelectedPics.Delete(i);
  end;

  procedure TdFileOpsWizard.FinalizeProcessing;
  var
    sDestPath: String;
    fs: TFileStream;

    procedure FSWriteLine(const s: String; const aParams: Array of const);
    var sf: String;
    begin
      sf := Format(s, aParams)+S_CRLF;
      fs.Write(sf[1], Length(sf));
    end;

  begin
     // Выполняем задачи по подготовке CD/DVD, если нужно
    if (FFileOpKind in [fokCopyFiles, fokMoveFiles]) and FMoveFile_UseCDOptions then begin
      sDestPath := IncludeTrailingPathDelimiter(FDestinationFolder);
       // Если есть фотоальбом, сохраняем его в файл
      if FExportedProject<>nil then
        try
          FExportedProject.SaveToFile(FExportedProject.FileName, SProject_Generator, SProject_Remark, FExportedProject.FileRevision);
          LogSuccess('SLogEntry_PhoaSavedOK', [FExportedProject.FileName]);
        except
          on e: Exception do LogFailure('SLogEntry_PhoaSaveError', [FExportedProject.FileName, e.Message]);
        end;
       // Копируем программу
      if FCDOpt_CopyExecutable then begin
        if CopyFile(
            PChar(ParamStr(0)),
            PChar(sDestPath+SPhoaExecutableFileName),
            False) then
          LogSuccess('SLogEntry_ExecutableCopiedOK', [FDestinationFolder])
        else
          LogFailure('SLogEntry_ExecutableCopyingError', [FDestinationFolder, SysErrorMessage(GetLastError)]);
         // Записываем текущие настройки в phoa.ini
        if FCDOpt_CopyIniSettings then IniSaveSettings(sDestPath+SDefaultIniFileName);
      end;
       // Создаём autorun.inf
      if (FExportedProject<>nil) and FCDOpt_CreateAutorun then
        try
          fs := TFileStream.Create(sDestPath+'autorun.inf', fmCreate);
          try
            FSWriteLine(
              '[autorun]'+S_CRLF+
              'open=%s "%s"'+S_CRLF+
              'icon=%0:s,1',
              [SPhoaExecutableFileName, FCDOpt_PhoaFileName]);
            if FCDOpt_MediaLabel<>'' then FSWriteLine('label=%s', [FCDOpt_MediaLabel]);
          finally
            fs.Free;
          end;
          LogSuccess('SLogEntry_AutorunCreatedOK', []);
        except
          on e: Exception do LogFailure('SLogEntry_AutorunCreationError', [e.Message]);
        end;
    end;
     // Закрываем форму Мастера/показываем протокол
    if (FCountErrors=0) and SettingValueBool(ISettingID_Dlgs_FOW_LogOnErrOnly) then
      ModalResult := mrOK
    else
      Controller.SetVisiblePageID(IWzFileOpsPageID_Log, pcmNextBtn);
  end;

  procedure TdFileOpsWizard.FinalizeWizard;
  begin
    FRepair_FileLinks.Free;
    FExportedProject := nil;
    FSelectedPics    := nil;
    FSelectedGroups  := nil;
    FLog.Free;
    inherited FinalizeWizard;
  end;

  function TdFileOpsWizard.GetFormRegistrySection: String;
  begin
    Result := SRegFileOps_Root;
  end;

  function TdFileOpsWizard.GetNextPageID: Integer;
  begin
    Result := 0;
    case CurPageID of
      IWzFileOpsPageID_SelTask:          Result := IWzFileOpsPageID_SelPics;
      IWzFileOpsPageID_SelPics:
        case FFileOpKind of
          fokCopyFiles,
            fokMoveFiles,
            fokRepairFileLinks:          Result := IWzFileOpsPageID_SelFolder;
          fokDeleteFiles:                Result := IWzFileOpsPageID_DelOptions;
          fokRebuildThumbs:              Result := IWzFileOpsPageID_Processing;
        end;
      IWzFileOpsPageID_SelFolder:
        case FFileOpKind of
          fokCopyFiles,
            fokMoveFiles:                Result := IWzFileOpsPageID_MoveOptions;
          fokRepairFileLinks:            Result := IWzFileOpsPageID_RepairOptions;
        end;
      IWzFileOpsPageID_MoveOptions:      Result := IWzFileOpsPageID_MoveOptions2;
      IWzFileOpsPageID_MoveOptions2:     Result := iif(FMoveFile_UseCDOptions, IWzFileOpsPageID_CDOptions, IWzFileOpsPageID_Processing);
      IWzFileOpsPageID_RepairOptions:    Result := IWzFileOpsPageID_RepairSelLinks;
      IWzFileOpsPageID_CDOptions,
        IWzFileOpsPageID_DelOptions,
        IWzFileOpsPageID_RepairSelLinks: Result := IWzFileOpsPageID_Processing;
      IWzFileOpsPageID_Processing:       Result := IWzFileOpsPageID_Log;
    end;
  end;

  procedure TdFileOpsWizard.InitializeWizard;
  var sOptPageTitle: String;
  begin
    inherited InitializeWizard;
    FSelectedGroups := NewPhotoAlbumPicGroupList(nil);
     // Если во вьюере выбрана группа, заносим её в список
    if FApp.CurGroup<>nil then FSelectedGroups.Add(FApp.CurGroup);
     // Настраиваем режим выбора изображений по умолчанию
    if FSelPicsByDefault and (FApp.SelectedPics.Count>0) then FSelPicMode := fospmSelPics
    else if FSelectedGroups.Count>0 then                      FSelPicMode := fospmSelGroups
    else                                                      FSelPicMode := fospmAll;
     // Инициализируем опции
    FCDOpt_CopyExecutable        := True;
    FCDOpt_IncludeViews          := True;
    FCDOpt_CreatePhoa            := True;
    FCDOpt_CreateAutorun         := True;
    FCDOpt_CopyIniSettings       := True;
    FCDOpt_MediaLabel            := ConstVal('SPhotoAlbumNode');
    FCDOpt_PhoaDesc              := FApp.Project.Description;
    FCDOpt_PhoaFileName          := ExtractFileName(FApp.Project.FileName);
    FMoveFile_ReplaceChar        := '_';
    FMoveFile_FileNameFormat     := 'Image_{ID}';
    FMoveFile_DeleteToRecycleBin := True;
    FMoveFile_OverwriteMode      := fomfomPrompt;
    FMoveFile_UseCDOptions       := True;
    FDelFile_DeleteToRecycleBin  := True;
    FRepair_MatchFlags           := [formfName, formfSize];
    FRepair_LookSubfolders       := True;
     // Создаём страницы и отображаем первую страницу
    sOptPageTitle := ConstVal('SWzPageFileOps_Options');
    Controller.CreatePage(TfrWzPageFileOps_SelTask,        IWzFileOpsPageID_SelTask,        IDH_intf_file_ops_seltask,   ConstVal('SWzPageFileOps_SelTask'));
    Controller.CreatePage(TfrWzPageFileOps_SelPics,        IWzFileOpsPageID_SelPics,        IDH_intf_file_ops_selpics,   ConstVal('SWzPageFileOps_SelPics'));
    Controller.CreatePage(TfrWzPageFileOps_SelFolder,      IWzFileOpsPageID_SelFolder,      IDH_intf_file_ops_selfolder, ConstVal('SWzPageFileOps_SelFolder'));
    Controller.CreatePage(TfrWzPageFileOps_MoveOptions,    IWzFileOpsPageID_MoveOptions,    IDH_intf_file_ops_moveopts,  sOptPageTitle);
    Controller.CreatePage(TfrWzPageFileOps_MoveOptions2,   IWzFileOpsPageID_MoveOptions2,   IDH_intf_file_ops_moveopts2, sOptPageTitle);
    Controller.CreatePage(TfrWzPageFileOps_CDOptions,      IWzFileOpsPageID_CDOptions,      IDH_intf_file_ops_cdopts,    sOptPageTitle);
    Controller.CreatePage(TfrWzPageFileOps_DelOptions,     IWzFileOpsPageID_DelOptions,     IDH_intf_file_ops_delopts,   sOptPageTitle);
    Controller.CreatePage(TfrWzPageFileOps_RepairOptions,  IWzFileOpsPageID_RepairOptions,  IDH_intf_file_ops_repropts,  sOptPageTitle);
    Controller.CreatePage(TfrWzPageFileOps_RepairSelLinks, IWzFileOpsPageID_RepairSelLinks, 0{#ToDo3: Завести HelpTopic}, ConstVal('SWzPageFileOps_RepairSelLinks'));                        
    Controller.CreatePage(TfrWzPage_Processing,            IWzFileOpsPageID_Processing,     IDH_intf_file_ops_process,   ConstVal('SWzPageFileOps_Processing'));
    Controller.CreatePage(TfrWzPage_Log,                   IWzFileOpsPageID_Log,            IDH_intf_file_ops_log,       ConstVal('SWzPageFileOps_Log'));
    Controller.SetVisiblePageID(IWzFileOpsPageID_SelTask, pcmForced);
  end;

  procedure TdFileOpsWizard.InterruptProcessing;
  begin
    FProcessingInterrupted := True;
  end;

  function TdFileOpsWizard.IsBtnBackEnabled: Boolean;
  begin
     // Со страницы прогресса возврата нет, со страницы ошибок вернуться можно только если ещё есть файлы
    Result :=
      inherited IsBtnBackEnabled and
      (CurPageID<>IWzFileOpsPageID_Processing) and
      ((CurPageID<>IWzFileOpsPageID_Log) or (FSelectedPics.Count>0));
  end;

  function TdFileOpsWizard.IsBtnCancelEnabled: Boolean;
  begin
    Result :=
      inherited IsBtnCancelEnabled and
      ((CurPageID<>IWzFileOpsPageID_Processing) or not FProcessing);
  end;

  function TdFileOpsWizard.IsBtnNextEnabled: Boolean;
  begin
    Result := inherited IsBtnNextEnabled;
    if Result then
      case CurPageID of
         // На странице прогресса дальше можно идти только при остановленном процессе и наличии строчек протокола
        IWzFileOpsPageID_Processing: Result := not FProcessing and (FLog<>nil);
      end;
  end;

  procedure TdFileOpsWizard.LogFailure(const s: String; const aParams: array of const);
  begin
    Inc(FCountErrors);
    FLog.Add('[!] '+ConstVal(s, aParams));
  end;

  function TdFileOpsWizard.LogPage_GetLog(iPageID: Integer): TStrings;
  begin
    Result := FLog;
  end;

  procedure TdFileOpsWizard.LogSuccess(const s: String; const aParams: array of const);
  begin
    FLog.Add('[+] '+ConstVal(s, aParams));
  end;

  procedure TdFileOpsWizard.PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer);
  begin
    inherited PageChanged(ChangeMethod, iPrevPageID);
    if (ChangeMethod=pcmNextBtn) and (CurPageID=IWzFileOpsPageID_Processing) then StartProcessing;
  end;

  function TdFileOpsWizard.PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean;
  begin
    Result := inherited PageChanging(ChangeMethod, iNewPageID);
    if Result and (ChangeMethod=pcmNextBtn) then begin
      case iNewPageID of
         // При попадании на страницу выбора изображений освобождаем память, стирая прежние выбранные
        IWzFileOpsPageID_SelPics: FSelectedPics := nil;
         // При попадании на страницу опций восстановления ссылок освобождаем память, стирая прежние файлы
        IWzFileOpsPageID_RepairOptions: FreeAndNil(FRepair_FileLinks);
         // При попадании на страницу выбора ссылок сначала производим выборку файлов
        IWzFileOpsPageID_RepairSelLinks: Repair_SelectFiles;
         // Перед началом обработки проверяем необходимсоть подтверждения
        IWzFileOpsPageID_Processing: Result := PhoaConfirm(True, 'SConfirm_PerformFileOperation', aFileOpConfirmSettingIDs[FFileOpKind]);
      end;
      if Result then
        case CurPageID of
           // После выбора задачи настраиваем опции по умолчанию
          IWzFileOpsPageID_SelTask: if FFileOpKind=fokRepairFileLinks then FSelPicValidityFilter := fospvfInvalidOnly;
           // При уходе со страницы выбора изображений строим список изображений
          IWzFileOpsPageID_SelPics: DoSelectPictures;
           // При уходе со страницы опций восстановления ссылок строим список файлов
          IWzFileOpsPageID_RepairOptions: Repair_SelectFiles;
        end;
    end;
  end;

  function TdFileOpsWizard.ProcPage_GetCurrentStatus: String;
  begin
     // Если процесс активен, отображаем прогресс
    if FProcessing then
      Result := ConstVal('SWzFileOps_Processing', [ProcPage_GetProgressCur+1, FInitialPicCount, FCountErrors, FSelectedPics[0].FileName])
     // Иначе пишем информацию о возможности продолжения
    else
      Result := ConstVal('SWzFileOps_Paused', [FCountSucceeded, FCountErrors]);
  end;

  function TdFileOpsWizard.ProcPage_GetProcessingActive: Boolean;
  begin
    Result := FProcessing;
  end;

  function TdFileOpsWizard.ProcPage_GetProgressCur: Integer;
  begin
    Result := FInitialPicCount-FSelectedPics.Count;
  end;

  function TdFileOpsWizard.ProcPage_GetProgressMax: Integer;
  begin
    Result := FInitialPicCount;
  end;

  procedure TdFileOpsWizard.ProcPage_PaintThumbnail(Bitmap32: TBitmap32);
  begin
    if FSelectedPics.Count>0 then PaintThumbnail(FSelectedPics[0], Bitmap32);
  end;

  procedure TdFileOpsWizard.Repair_SelectFiles;

     // Добавляет запись файла к FRepair_FileLinks и заносит в него ссылки на изображения, которым он подходит по
     //   текущим заданным критериям
    procedure AddFile(const sPath: String; const SRec: TSearchRec);
    var
      FL: TFileLink;
      i: Integer;
      bMatches: Boolean;
      Pic: IPhoaPic;
    begin
      FL := nil;
       // Перебираем все изображения, выбирая подходящие
      for i := 0 to FSelectedPics.Count-1 do begin
        Pic := FSelectedPics[i];
         // Проверяем совпадение - сначала по размеру, потом по имени файла (так быстрее)
        bMatches := True;
        if bMatches and (formfSize in FRepair_MatchFlags) then bMatches := SRec.Size=Pic.FileSize;
        if bMatches and (formfName in FRepair_MatchFlags) then bMatches := AnsiSameText(SRec.Name, ExtractFileName(Pic.FileName));
         // Проверяем доступность по "занятости" другим изображением
        if bMatches and not FRepair_RelinkFilesInUse      then bMatches := FApp.Project.Pics.IndexOfFileName(sPath+SRec.Name)<0;
         // Если подходит
        if bMatches then begin
           // Создаём файл, если он ещё не создан
          if FL=nil then FL := FRepair_FileLinks.Add(SRec.Name, sPath, SRec.Size, FileDateToDateTime(SRec.Time));
           // Добавляем ему ссылку
          FL.Pics.Add(Pic, True);
        end;
      end;
    end;

     // Рекурсивно добавляет каталог sPath
    procedure AddFolder(const sPath: String; bRecurse: Boolean);
    var
      sr: TSearchRec;
      iRes: Integer;
    begin
      iRes := FindFirst(sPath+'*.*', faAnyFile, sr);
      try
        while iRes=0 do begin
          if sr.Name[1]<>'.' then
             // Если каталог - рекурсивно сканируем
            if sr.Attr and faDirectory<>0 then begin
              if bRecurse then AddFolder(sPath+sr.Name+'\', True);
             // Если файл и расширение знакомого типа - добавляем к списку
            end else if FileFormatList.GraphicFromExtension(ExtractFileExt(sr.Name))<>nil then
              AddFile(sPath, sr);
          iRes := FindNext(sr);
        end;
      finally
        FindClose(sr);
      end;
    end;

  begin
     // Создаём или очищаем список файлов
    if FRepair_FileLinks=nil then FRepair_FileLinks := TFileLinks.Create else FRepair_FileLinks.Clear;
     // Рекурсивно добавляем файлы/папки
    AddFolder(IncludeTrailingPathDelimiter(FDestinationFolder), FRepair_LookSubfolders);
  end;

  procedure TdFileOpsWizard.SettingsRestore(rif: TRegIniFile);
  begin
    inherited SettingsRestore(rif);
    FDestinationFolder := rif.ReadString('', 'DestinationFolder', '');
  end;

  procedure TdFileOpsWizard.SettingsStore(rif: TRegIniFile);
  begin
    inherited SettingsStore(rif);
    rif.WriteString ('', 'DestinationFolder', FDestinationFolder);
  end;

  procedure TdFileOpsWizard.StartProcessing;
  begin
    FProcessing := True;
     // Создаём протокол
    if FLog=nil then FLog := TStringList.Create;
     // Создаём экспортируемый фотоальбом
    if (FExportedProject=nil) and (FFileOpKind in [fokCopyFiles, fokMoveFiles]) and FMoveFile_UseCDOptions and FCDOpt_CreatePhoa then CreateExportedPhoa;
     // Запоминаем исходное количество файлов
    FInitialPicCount := FSelectedPics.Count;
     // Обновляем страницу прогресса
    UpdateProgressInfo;
     // Запускаем поток
    FProcessingInterrupted := False;
    FFileOpThread := TFileOpThread.Create(Self);
  end;

  procedure TdFileOpsWizard.ThreadPicProcessed;
  begin
     // Проверяем, чем кончилось операция
    if not FFileOpThread.ErrorOccured then Inc(FCountSucceeded);
     // Обновляем статус диалога
    HasUpdates := True;
    if FFileOpThread.ChangesMade then FProjectChanged := True;
     // Удаляем обработанное изображение
    FSelectedPics.Delete(0);
     // Если обработан весь список, прерываем поток
    if (FSelectedPics.Count=0) or FProcessingInterrupted then begin
      FProcessing := False;
      FFileOpThread.Terminate;
      FFileOpThread := nil;
       // Если список пуст, завершаем обработку
      if FSelectedPics.Count=0 then FinalizeProcessing;
    end;
     // Уведомляем страницу прогресса
    UpdateProgressInfo;
  end;

  procedure TdFileOpsWizard.UpdateProgressInfo;
  begin
    Controller.ItemsByID[IWzFileOpsPageID_Processing].Perform(WM_PAGEUPDATE, 0, 0);
    UpdateButtons;
  end;

end.

