//**********************************************************************************************************************
//  $Id: ufImgView.pas,v 1.6 2004-04-23 19:26:30 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ufImgView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GraphicEx, GR32, Controls, Forms, Dialogs, ConsVars, phObj, GR32_Layers,
  DTLangTools, TB2Item, TBX, Menus, ActnList, GR32_Image, TB2Dock,
  TB2Toolbar, TB2ExtItems, TBXExtItems;

type
   // Поток, декодирующий следующее изображение в фоновом режиме
  TDecodeThread = class(TThread)
  private
     // Bitmap, в который декодируется файл
    FBitmap: TBitmap32;
     // Событие постановки изображения в очередь на декодирование
    FHQueuedEvent: THandle;
     // Prop storage
    FQueuedFileName: String;
    FHDecodedEvent: THandle;
    FErrorMessage: String;
    procedure SetQueuedFileName(const Value: String);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
     // Возвращает и забывает декодированный Bitmap
    function  GetAndReleasePicture: TBitmap32;
    procedure Terminate;
     // Props
     // -- Текст сообщения об ошибке, если после декодирования Graphic=nil
    property ErrorMessage: String read FErrorMessage;
     // -- Событие завершения декодирования
    property HDecodedEvent: THandle read FHDecodedEvent;
     // -- Имя файла, которым занимается, или будет заниматься поток. При присваивании файл изображения ставится в
     //    очередь на декодирование, упраздняется прежнее декодированное изображение
    property QueuedFileName: String read FQueuedFileName write SetQueuedFileName;
  end;

  TfImgView = class(TForm)
    alMain: TActionList;
    aNextPic: TAction;
    aPrevPic: TAction;
    aFirstPic: TAction;
    aLastPic: TAction;
    aRefresh: TAction;
    aClose: TAction;
    aSettings: TAction;
    aZoomIn: TAction;
    aZoomOut: TAction;
    aZoomActual: TAction;
    aZoomFit: TAction;
    pmMain: TTBXPopupMenu;
    ipmNextPic: TTBXItem;
    ipmPrevPic: TTBXItem;
    ipmLastPic: TTBXItem;
    ipmFirstPic: TTBXItem;
    ipmSep1: TTBXSeparatorItem;
    ipmZoomIn: TTBXItem;
    ipmZoomOut: TTBXItem;
    ipmZoomActual: TTBXItem;
    ipmZoomFit: TTBXItem;
    ipmSep2: TTBXSeparatorItem;
    ipmRefresh: TTBXItem;
    ipmSettings: TTBXItem;
    ipmSep4: TTBXSeparatorItem;
    ipmClose: TTBXItem;
    aFullScreen: TAction;
    ipmFullScreen: TTBXItem;
    aHelp: TAction;
    ipmHelp: TTBXItem;
    aEdit: TAction;
    ipmEdit: TTBXItem;
    dtlsMain: TDTLanguageSwitcher;
    aSlideShow: TAction;
    ipmSep3: TTBXSeparatorItem;
    iSlideShow: TTBXItem;
    iMain: TImage32;
    aRelocateInfo: TAction;
    ipmRelocateInfo: TTBXItem;
    dkTop: TTBXDock;
    dkLeft: TTBXDock;
    dkRight: TTBXDock;
    dkBottom: TTBXDock;
    tbMain: TTBXToolbar;
    bFirstPic: TTBXItem;
    bPrevPic: TTBXItem;
    bNextPic: TTBXItem;
    bLastPic: TTBXItem;
    tbSepZoomIn: TTBXSeparatorItem;
    bZoomIn: TTBXItem;
    bZoomOut: TTBXItem;
    bZoomActual: TTBXItem;
    bZoomFit: TTBXItem;
    tbSepSlideShow: TTBXSeparatorItem;
    bSlideShow: TTBXItem;
    tbSepEdit: TTBXSeparatorItem;
    bEdit: TTBXItem;
    bFullScreen: TTBXItem;
    bRefresh: TTBXItem;
    bRelocateInfo: TTBXItem;
    bSettings: TTBXItem;
    bHelp: TTBXItem;
    tbSepClose: TTBXSeparatorItem;
    bClose: TTBXItem;
    tbSepCounter: TTBXSeparatorItem;
    eCounter: TTBXEditItem;
    aShowInfo: TAction;
    ipmShowInfo: TTBXItem;
    bShowInfo: TTBXItem;
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure aaNextPic(Sender: TObject);
    procedure aaPrevPic(Sender: TObject);
    procedure aaRefresh(Sender: TObject);
    procedure aaClose(Sender: TObject);
    procedure aaSettings(Sender: TObject);
    procedure aaFirstPic(Sender: TObject);
    procedure aaLastPic(Sender: TObject);
    procedure aaZoomFit(Sender: TObject);
    procedure aaZoomIn(Sender: TObject);
    procedure aaZoomOut(Sender: TObject);
    procedure aaZoomActual(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure aaFullScreen(Sender: TObject);
    procedure aaHelp(Sender: TObject);
    procedure aaEdit(Sender: TObject);
    procedure aaSlideShow(Sender: TObject);
    procedure iMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure FormCreate(Sender: TObject);
    procedure aaRelocateInfo(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbMainVisibleChanged(Sender: TObject);
    procedure iMainResize(Sender: TObject);
    procedure aaShowInfo(Sender: TObject);
  private
    FGroup: TPhoaGroup;
    FPhoA: TPhotoAlbum;
     // Поток фоновой загрузки картинок
    FDecodeThread: TDecodeThread;
     // True, если последняя смена картинки была в направлении "назад" (используется для предекодирования)
    FLastPicChangeBackwards: Boolean;
     // Слой, на котором рисуется описание
    FDescLayer: TPositionedLayer;
     // Слой перемещения информации
    FRBLayer: TRubberbandLayer;
     // Текущее отображаемое изображение
    FPic: TPhoaPic;
     // Предыдущее просмотренное скэшированное изображение и имя его файла (используются при bViewCacheBehind=True)
    FCachedBitmap: TBitmap32;
    FCachedBitmapFilename: String;
     // True, если курсор принудительно скрыт
    FCursorHidden: Boolean;
     // Курсор для iMain (crHand или crDefault)
    FImageCursor: TCursor;
     // Флаг и положение перетаскивания изображения
    FTrackDrag: Boolean;
    FTrackX: Integer;
    FTrackY: Integer;
     // Флаг принудительного изменения размеров окна
    FForcedResize: Boolean;
     // Кэшированные настройки просмотра
    FBackgroundColor: TColor;
    FZoomFactorChange: Single;
    FCaptionProps: TPicProperties;
    FDoShrinkPic: Boolean;
    FDoZoomPic: Boolean;
    FDefaultFullscreen: Boolean;
    FAlwaysOnTop: Boolean;
    FKeepCursorOverTB: Boolean;
    FHideCursorInFS: Boolean;
    FFitWindowToPic: Boolean;
    FCenterWindow: Boolean;
    FCyclicViewing: Boolean;
    FPredecodePic: Boolean;
    FCacheBehindPic: Boolean;
    FStretchFilter: TStretchFilter;
    FSlideInterval: Integer;
    FSlideCyclic: Boolean;
    FDefaultShowInfo: Boolean;
    FInfoProps: TPicProperties;
    FInfoFont: String;
    FInfoBkColor: TColor;
    FInfoBkOpacity: Byte;
     // Коэффициенты увеличения/уменьшения изображения
    FDefaultZoomFactor: Single;
    FBestFitZoomFactor: Single;
     // True, если в последний раз ZoomFactor устанавливался равным BestFitZoomFactor
    FBestFitZoomUsed: Boolean;
     // Максимальные размеры окна и области отображения
    FWMaxWindow, FHMaxWindow, FWMaxView, FHMaxView: Integer;
     // Внутренние размеры окна
    FWClient, FHClient: Integer;
     // Разница между внутренними и внешними размерами окна
    FXGap, FYGap: Integer;
     // Размеры исходного и масштабированного изображения
    FWPic, FHPic: Integer;
    FWScaled, FHScaled: Integer;
     // Описание изображения
    FPicDesc: String;
     // ID таймера показа слайдов (0, если таймер не создан)
    FTimerID: Integer;
     // Список операций для отмены редактирования/добавления
    FUndoOperations: TPhoaOperations;
     // True, если текущее изображение не отображается из-за ошибки
    FErroneous: Boolean;
     // Флаг блокировки перегрузки изображения
    FDisplayLock: Integer;
     // Prop storage
    FFullScreen: Boolean;
    FPicIdx: Integer;
    FShowInfo: Boolean;
    FSlideShow: Boolean;
     // Настраивает параметры окна и вычисляет базовые параметры отображения
    procedure ApplySettings;
     // Настраивает видимость курсора мыши
    procedure AdjustCursorVisibility(bForceShow: Boolean);
     // Загружает и буферизирует изображение; рассчитывает коэффициенты масштабирования
    procedure DisplayPic(bReload: Boolean);
     // Перегружает текущее изображение
    procedure RedisplayPic(bReload: Boolean);
     // Установка/снятие блокировки перегрузки изображения
    procedure RedisplayLock;
    procedure RedisplayUnlock(bReload: Boolean);
     // Вызывается из DisplayPic. Загружает изображение в iMain
    procedure DP_LoadImage;
     // Вызывается из DisplayPic. Рассчитывает параметры (размеры) изображения
    procedure DP_ComputeDimensions;
     // Вызывается из DisplayPic. Инициализирует описания изображения (заголовок окна, текст информации, счётчик)
    procedure DP_DescribePic;
     // Ставит в очередь на загрузку следующее изображение, если нужно
    procedure DP_EnqueueNext;
     // Применяет коэффициент масштабирования sNewZoom, позиционируя окно при необходимости и bCanResize=True
    procedure ApplyZoom(sNewZoom: Single; bCanResize: Boolean);
     // Разрешает/запрещает Actions
    procedure EnableActions;
     // Пересоздаёт или удаляет таймер показа слайдов
    procedure RestartShowTimer;
     // Процедуры временного убирания/восстановления стиля TOPMOST окна просмотра, если он есть
    procedure TopmostCancel;
    procedure TopmostRestore;
     // События слоёв
    procedure PaintDescLayer(Sender: TObject; Buffer: TBitmap32);
    procedure RBLayerResizing(Sender: TObject; const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);
     // Если активен режим ресайзинга информации, завершает его
    procedure CommitInfoRelocation;
     // Message handlers
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
     // Prop handlers
    procedure SetPicIdx(Value: Integer);
    function  GetZoomFactor: Single;
    procedure SetZoomFactor(Value: Single);
    function  GetViewOffset: TPoint;
    procedure SetViewOffset(const Value: TPoint);
    procedure SetFullScreen(Value: Boolean);
    procedure SetSlideShow(Value: Boolean);
    procedure SetShowInfo(Value: Boolean);
  public
     // Props
     // -- True, если в настоящий момент полноэкранный режим
    property FullScreen: Boolean read FFullScreen write SetFullScreen;
     // -- Индекс текущего изображения в группе
    property PicIdx: Integer read FPicIdx write SetPicIdx;
     // -- True, если в данный момент информация об изображении отображается
    property ShowInfo: Boolean read FShowInfo write SetShowInfo;
     // -- True, если активно Slide Show
    property SlideShow: Boolean read FSlideShow write SetSlideShow;
     // -- Смещение левого верхнего угла просматриваемого изображения
    property ViewOffset: TPoint read GetViewOffset write SetViewOffset;
     // -- Масштаб просматриваемого изображения
    property ZoomFactor: Single read GetZoomFactor write SetZoomFactor;
  end;

   // Переход в режим просмотра изображений
   //   Group          - группа, в которой просматривать изображения
   //   PhoA           - фотоальбом
   //   iPicIdx        - индекс изображения в группе, с которого начинать просмотр
   //   UndoOperations - буфер отката
   //   bPhGroups      - True, если отображается дерево папок фотоальбома (не представление)
  procedure ViewImage(Group: TPhoaGroup; PhoA: TPhotoAlbum; iPicIdx: Integer; UndoOperations: TPhoaOperations; bPhGroups: Boolean);

implementation
{$R *.dfm}
uses udSettings, Types, phUtils, ChmHlp, udPicProps, Main, phSettings;

  procedure ViewImage(Group: TPhoaGroup; PhoA: TPhotoAlbum; iPicIdx: Integer; UndoOperations: TPhoaOperations; bPhGroups: Boolean);
  begin
    with TfImgView.Create(Application) do
      try
        FGroup          := Group;
        FPhoA           := PhoA;
        FPicIdx         := iPicIdx;
        FUndoOperations := UndoOperations;
        aEdit.Enabled   := bPhGroups;
        ApplySettings;
        ShowModal;
      finally
        if FCursorHidden then ShowCursor(True);
        Free;
      end;
  end;

   //===================================================================================================================
   // TDecodeThread
   //===================================================================================================================

  constructor TDecodeThread.Create;
  begin
    inherited Create(True);
    FreeOnTerminate := True;
    Priority := tpLowest;
    FHQueuedEvent  := CreateEvent(nil, False, False, nil);
     // Событие декодирования создаём в сигнальном состоянии - это означает, что поток изначально свободен
    FHDecodedEvent := CreateEvent(nil, True,  True,  nil);
    Resume;
  end;

  destructor TDecodeThread.Destroy;
  begin
    FBitmap.Free;
    CloseHandle(FHQueuedEvent);
    CloseHandle(FHDecodedEvent);
    inherited Destroy;
  end;

  procedure TDecodeThread.Execute;
  begin
    while not Terminated do begin
      WaitForSingleObject(FHQueuedEvent, INFINITE);
      if Terminated then Break;
      try
         // Освобождаем прежнее изображение, если оно есть (осталось невостребованным)
        FreeAndNil(FBitmap);
         // Получаем новое изображение, перехватываем возможные Exceptions
        try
          FBitmap := LoadGraphicFromFile(FQueuedFileName);
        except
          on e: Exception do FErrorMessage := e.Message;
        end;
       // Рапортуем о готовности
      finally
        SetEvent(FHDecodedEvent);
      end;
    end;
  end;

  function TDecodeThread.GetAndReleasePicture: TBitmap32;
  begin
    Result := FBitmap;
    FBitmap := nil;
  end;

  procedure TDecodeThread.SetQueuedFileName(const Value: String);
  begin
     // Ждём освобождения потока
    WaitForSingleObject(FHDecodedEvent, INFINITE);
     // Если в очередь ставится другой файл
    if FQueuedFileName<>Value then begin
      FErrorMessage := '';
      FQueuedFileName := Value;
       // Если это не просто очистка очереди
      if FQueuedFileName<>'' then begin
         // Сбрасываем событие (занимаем поток)
        ResetEvent(FHDecodedEvent);
         // Стартуем поток
        SetEvent(FHQueuedEvent);
      end;
    end;
  end;

  procedure TDecodeThread.Terminate;
  begin
    inherited Terminate;
    SetEvent(FHQueuedEvent);
  end;

   //===================================================================================================================
   // TfImgView
   //===================================================================================================================

  procedure TfImgView.aaClose(Sender: TObject);
  begin
    Close;
  end;

  procedure TfImgView.aaEdit(Sender: TObject);
  var
    Arr: TPicArray;
    bEdited: Boolean;
  begin
    CommitInfoRelocation;
     // "Снимаем" окно с topmost-положения 
    TopmostCancel;
     // Показываем курсор
    AdjustCursorVisibility(True);
     // Редактируем изображение
    SetLength(Arr, 1);
    Arr[0] := FPic;
    bEdited := EditPic(Arr, FPhoA, FUndoOperations);
     // Возвращаем topmost-положение окну
    TopmostRestore;
     // Скрываем курсор
    AdjustCursorVisibility(False);
     // Обновляем свойства изображения
    if bEdited then RedisplayPic(False);
  end;

  procedure TfImgView.aaFirstPic(Sender: TObject);
  begin
    FLastPicChangeBackwards := False;
    PicIdx := 0;
  end;

  procedure TfImgView.aaFullScreen(Sender: TObject);
  begin
    CommitInfoRelocation;
    FullScreen := not FullScreen;
  end;

  procedure TfImgView.aaHelp(Sender: TObject);
  begin
    CommitInfoRelocation;
    HtmlHelpContext(HelpContext);
  end;

  procedure TfImgView.aaLastPic(Sender: TObject);
  begin
    FLastPicChangeBackwards := True;
    PicIdx := FGroup.PicIDs.Count-1;
  end;

  procedure TfImgView.aaNextPic(Sender: TObject);
  begin
    FLastPicChangeBackwards := False;
    if PicIdx<FGroup.PicIDs.Count-1 then PicIdx := PicIdx+1
    else if FCyclicViewing then PicIdx := 0;
  end;

  procedure TfImgView.aaPrevPic(Sender: TObject);
  begin
    FLastPicChangeBackwards := True;
    if PicIdx>0 then PicIdx := PicIdx-1
    else if FCyclicViewing then PicIdx := FGroup.PicIDs.Count-1;
  end;

  procedure TfImgView.aaRefresh(Sender: TObject);
  begin
    RedisplayPic(True);
  end;

  procedure TfImgView.aaRelocateInfo(Sender: TObject);
  var fr: TFloatRect;
  begin
     // Режим неактивен - создаём RBLayer
    if FRBLayer=nil then begin
      FRBLayer := TRubberbandLayer.Create(iMain.Layers);
      FRBLayer.ChildLayer := FDescLayer;
      FRBLayer.OnResizing := RBLayerResizing;
     // Режим активен - сохраняем позицию RBLayer и уничтожаем его
    end else begin
      fr := FRBLayer.GetAdjustedLocation;
      ViewInfoPos := Rect(
        Round(fr.Left/FWClient*10000),
        Round(fr.Top/FHClient*10000),
        Round(fr.Right/FWClient*10000),
        Round(fr.Bottom/FHClient*10000));
      FreeAndNil(FRBLayer);
    end;
    aRelocateInfo.Checked := Assigned(FRBLayer);
  end;

  procedure TfImgView.aaSettings(Sender: TObject);
  var bEdited: Boolean;
  begin
    CommitInfoRelocation;
     // "Снимаем" окно с topmost-положения
    TopmostCancel;
     // Показываем курсор
    AdjustCursorVisibility(True);
     // В диалоге настроек по умолчанию выбираем кнопку "Режим просмотра"
    bEdited := EditSettings(ISettingID_View);
     // Возвращаем topmost-положение окну
    TopmostRestore;
     // Применяем настройки
    if bEdited then begin
      fMain.ApplySettings;
      ApplySettings;
      RestartShowTimer;
    end else
      AdjustCursorVisibility(False);
  end;

  procedure TfImgView.aaShowInfo(Sender: TObject);
  begin
    ShowInfo := not ShowInfo;
  end;

  procedure TfImgView.aaSlideShow(Sender: TObject);
  begin
    SlideShow := not SlideShow;
  end;

  procedure TfImgView.aaZoomActual(Sender: TObject);
  begin
    ZoomFactor := 1.0;
  end;

  procedure TfImgView.aaZoomFit(Sender: TObject);
  begin
    ZoomFactor := FBestFitZoomFactor;
  end;

  procedure TfImgView.aaZoomIn(Sender: TObject);
  begin
    ZoomFactor := ZoomFactor*FZoomFactorChange;
  end;

  procedure TfImgView.aaZoomOut(Sender: TObject);
  begin
    ZoomFactor := ZoomFactor/FZoomFactorChange;
  end;

  procedure TfImgView.AdjustCursorVisibility(bForceShow: Boolean);
  begin
    if (FHideCursorInFS and FFullScreen and not bForceShow)<>FCursorHidden then begin
      FCursorHidden := not FCursorHidden;
      ShowCursor(not FCursorHidden);
    end;
  end;

  procedure TfImgView.ApplySettings;
  begin
     // Кэшируем значения настроек
    FBackgroundColor   := SettingValueInt(ISettingID_View_BkColor);
    FZoomFactorChange  := adMagnifications[SettingValueInt(ISettingID_View_ZoomFactor)];
    FCaptionProps      := IntToPicProps(SettingValueInt(ISettingID_View_CaptionProps));
    FDoShrinkPic       := SettingValueBool(ISettingID_View_ShrinkPicToFit);
    FDoZoomPic         := SettingValueBool(ISettingID_View_ZoomPicToFit);
    FDefaultFullscreen := SettingValueBool(ISettingID_View_Fullscreen);
    FAlwaysOnTop       := SettingValueBool(ISettingID_View_AlwaysOnTop);
    FKeepCursorOverTB  := SettingValueBool(ISettingID_View_KeepCursorOverTB);
    FHideCursorInFS    := SettingValueBool(ISettingID_View_HideCursor);
    FFitWindowToPic    := SettingValueBool(ISettingID_View_FitWindowToPic);
    FCenterWindow      := SettingValueBool(ISettingID_View_CenterWindow);
    FCyclicViewing     := SettingValueBool(ISettingID_View_Cyclic);
    FPredecodePic      := SettingValueBool(ISettingID_View_Predecode);
    FCacheBehindPic    := SettingValueBool(ISettingID_View_CacheBehind);
    FStretchFilter     := TStretchFilter(SettingValueInt(ISettingID_View_StchFilt));
    FSlideInterval     := SettingValueInt(ISettingID_View_SlideInterval);
    FSlideCyclic       := SettingValueBool(ISettingID_View_SlideCyclic);
    FDefaultShowInfo   := SettingValueBool(ISettingID_View_ShowInfo);
    FShowInfo          := FDefaultShowInfo;
    FInfoProps         := IntToPicProps(SettingValueInt(ISettingID_View_InfoPicProps));
    FInfoFont          := SettingValueStr(ISettingID_View_InfoFont);
    FInfoBkColor       := SettingValueInt(ISettingID_View_InfoBkColor);
    FInfoBkOpacity     := SettingValueInt(ISettingID_View_InfoBkOpacity);
     // Настраиваем параметры окна
    FontFromStr(Font, SettingValueStr(ISettingID_Gen_MainFont));
    Color              := FBackgroundColor;
     // Настраиваем доки/панели инструментов
     // -- Видимость
    tbMain.Visible     := SettingValueBool(ISettingID_View_ShowToolbar);
     // -- Перетаскиваемость
    ApplyToolbarSettings(dkTop);
    ApplyToolbarSettings(dkLeft);
    ApplyToolbarSettings(dkRight);
    ApplyToolbarSettings(dkBottom);
     // Настраиваем установки текущей сессии
    RedisplayLock;
    try
      ShowInfo   := FDefaultShowInfo;
      FullScreen := FDefaultFullscreen;
    finally
       // Этот вызов загружает изображение
      RedisplayUnlock(True);
    end;
  end;

  procedure TfImgView.ApplyZoom(sNewZoom: Single; bCanResize: Boolean);
  var
    ixWindow, iyWindow, iwWindow, ihWindow: Integer;
    PrevMousePos, p: TPoint;
  begin
     // Сообщение об ошибке не масштабируем
    if FErroneous then sNewZoom := 1
     // Verify zoom value
    else if sNewZoom>SMaxPicZoom then sNewZoom := SMaxPicZoom
    else if sNewZoom<SMinPicZoom then sNewZoom := SMinPicZoom;
     // Применяем коэффициент масштабирования
    iMain.Scale := sNewZoom;
     // Находим размеры масштабированного изображения
    FWScaled := Round(FWPic*sNewZoom);
    FHScaled := Round(FHPic*sNewZoom);
     // Находим размеры и положение окна
    ixWindow := Left;
    iyWindow := Top;
    iwWindow := Width;
    ihWindow := Height;
    if bCanResize then begin
      if FFullScreen then begin
        iwWindow := Screen.Width;
        ihWindow := Screen.Height;
      end else if FFitWindowToPic then begin
        iwWindow := Max(Min(FWScaled+FXGap, FWMaxWindow), Constraints.MinWidth);
        ihWindow := Max(Min(FHScaled+FYGap, FHMaxWindow), Constraints.MinHeight);
      end;
      if FFullScreen then begin
        ixWindow := 0;
        iyWindow := 0;
      end else if FCenterWindow then begin
        ixWindow := (Screen.WorkAreaWidth-iwWindow) div 2;
        iyWindow := (Screen.WorkAreaHeight-ihWindow) div 2;
      end;
       // Если нужно, сохраняем положение курсора мыши над панелью инструментов
      PrevMousePos := Point(-1, -1);
      if FKeepCursorOverTB and not FFullScreen and Application.Active and tbMain.Visible then begin
        p := tbMain.ScreenToClient(Mouse.CursorPos);
        if (p.x<tbMain.Width) and (p.y<tbMain.Height) then PrevMousePos := p;
      end;
       // Изменяем положение окна
      FForcedResize := True;
      try
        SetBounds(ixWindow, iyWindow, iwWindow, ihWindow);
      finally
        FForcedResize := False;
      end;
       // Восстанавливаем положение мыши
      if (PrevMousePos.x>=0) and (PrevMousePos.y>=0) and (PrevMousePos.x<tbMain.Width) and (PrevMousePos.y<tbMain.Height) then 
        Mouse.CursorPos := tbMain.ClientToScreen(PrevMousePos);
    end;
     // Находим внутренние размеры
    FWClient := iwWindow-FXGap;
    FHClient := ihWindow-FYGap;
     // Настраиваем курсор
    FImageCursor := aImgViewCursors[(FWScaled>FWClient) or (FHScaled>FHClient)];
    iMain.Cursor := FImageCursor;
     // Находим начальное положение изображения
    ViewOffset := Point((FWClient-FWScaled) div 2, (FHClient-FHScaled) div 2);
     // Настраиваем положение информации
    FDescLayer.Location := FloatRect(
      FWClient/10000*ViewInfoPos.Left,
      FHClient/10000*ViewInfoPos.Top,
      FWClient/10000*ViewInfoPos.Right,
      FHClient/10000*ViewInfoPos.Bottom);
     // Настраиваем Actions (ZoomFactor и текущий индекс картинки влияют на это)
    EnableActions;
  end;

  procedure TfImgView.CommitInfoRelocation;
  begin
    if FRBLayer<>nil then aRelocateInfo.Execute;
  end;

  procedure TfImgView.DisplayPic(bReload: Boolean);
  begin
    if FDisplayLock>0 then Exit;
    CommitInfoRelocation;
    FTrackDrag := False;
     // Загружаем изображение
    if bReload then DP_LoadImage;
     // Рассчитываем размеры окна и изображения
    DP_ComputeDimensions;
     // Настраиваем описания
    DP_DescribePic;
     // Отображаем картинку
    ApplyZoom(FDefaultZoomFactor, True);
     // Ставим в очередь предыдущее/следующее (зависит от направления листания) изображение
    DP_EnqueueNext;
     // Перезапускаем таймер
    RestartShowTimer;
  end;

  procedure TfImgView.DP_ComputeDimensions;
  begin
     // Определяем максимальные размеры окна
    if FFullScreen then begin
      FXGap := ClientWidth-iMain.Width;
      FYGap := ClientHeight-iMain.Height;
      FWMaxWindow := Screen.Width;
      FHMaxWindow := Screen.Height;
    end else begin
      FXGap := Width-iMain.Width;
      FYGap := Height-iMain.Height;
      FWMaxWindow := Screen.WorkAreaWidth;
      FHMaxWindow := Screen.WorkAreaHeight;
    end;
     // Получаем размеры изображения (не допускаем нулевых значений)
    FWPic := Max(iMain.Bitmap.Width,  1);
    FHPic := Max(iMain.Bitmap.Height, 1);
    FWMaxView := FWMaxWindow-FXGap;
    FHMaxView := FHMaxWindow-FYGap;
     // Определяем коэффициенты масштабирования
    FBestFitZoomFactor := MinS(FWMaxView/FWPic, FHMaxView/FHPic);
    if ((FBestFitZoomFactor<1.0) and FDoShrinkPic) or ((FBestFitZoomFactor>1.0) and FDoZoomPic) then
      FDefaultZoomFactor := FBestFitZoomFactor
    else
      FDefaultZoomFactor := 1.0;
  end;

  procedure TfImgView.DP_DescribePic;
  var sCaption: String;
  begin
    FPicDesc := '';
     // Настраиваем Caption / составляем описание
    if FErroneous then
      sCaption := ''
    else begin
      sCaption := FPic.GetPropStrs(FCaptionProps, '', ' - ');
      if FShowInfo then FPicDesc := FPic.GetPropStrs(FInfoProps, '', '    ');
    end;
    if sCaption='' then Caption := dtlsMain.Consts['SDefaultCaption'] else Caption := sCaption;
     // Настраиваем счётчик
    eCounter.Text := Format('%d/%d', [FPicIdx+1, FGroup.PicIDs.Count]);
  end;

  procedure TfImgView.DP_EnqueueNext;
  var idxNextPic: Integer;
  begin
     // Если включено предекодирование 
    if FPredecodePic then begin
       // Находим индекс следующего изображения с учётом направления листания
      idxNextPic := FPicIdx+iif(FLastPicChangeBackwards, -1, 1);
       // Проверяем границы / цикличность просмотра
      if idxNextPic<0 then
        if FCyclicViewing then idxNextPic := FGroup.PicIDs.Count-1 else idxNextPic := -1
      else if idxNextPic>=FGroup.PicIDs.Count then
        if FCyclicViewing then idxNextPic := 0 else idxNextPic := -1;
       // Ставим файл в очередь 
      if idxNextPic>=0 then FDecodeThread.QueuedFileName := FPhoA.Pics.PicByID(FGroup.PicIDs[idxNextPic]).PicFileName;
    end;
  end;

  procedure TfImgView.DP_LoadImage;
  var
    FPrevPic: TPhoaPic;
    bmpDecoded, bmpPrevTemp: TBitmap32;
    bPicInCache: Boolean;

     // Рисует на iMain вместо изображения сообщение об ошибке
    procedure PaintError(const sFileName, sError: String);
    var
      r: TRect;
      sTitle: String;
      Sz: TSize;
    begin
      sTitle := dtlsMain.Consts['SError'];
      with iMain.Bitmap do begin
         // Стираем Bitmap
        Width  := 500;
        Height := 300;
        Clear(FBackgroundColor);
         // Рисуем текст 'ERROR'
        Font.Assign(Self.Font);
        Font.Color := $2020c0;
        Font.Size  := 72;
        Font.Style := [fsBold];
        Sz := TextExtent(sTitle);
        r := Rect((Width-Sz.cx) div 2, (Height-Sz.cy) div 2, (Width+Sz.cx) div 2, (Height+Sz.cy) div 2);
        TextOut(r, DT_LEFT or DT_NOPREFIX, sTitle);
         // Рисуем имя файла
        Font.Color := clRed;
        Font.Size  := 9;
        Font.Style := [];
        TextOut(Rect(0, 0, Width, r.Top), DT_CENTER or DT_NOPREFIX or DT_SINGLELINE or DT_BOTTOM or DT_PATH_ELLIPSIS, sFileName);
         // Рисуем текст ошибки
        TextOut(Rect(0, r.Bottom, Width, Height), DT_CENTER or DT_NOPREFIX or DT_WORDBREAK, sError);
      end;
    end;

  begin
     // Сохраняем прежнее изображение
    FPrevPic := FPic;
     // Находим текущее изображение
    FPic := FPhoA.Pics.PicByID(FGroup.PicIDs[FPicIdx]);
     // Определяем, есть ли изображение в кэше 
    bPicInCache := FCachedBitmapFilename=FPic.PicFileName;
     // Если нет, начинаем [полу]фоновую загрузку изображения
    if not bPicInCache then FDecodeThread.QueuedFileName := FPic.PicFileName;
     // Если изображения не совпадают, сохраняем старое изображение
    if not FErroneous and FCacheBehindPic and (FPrevPic<>nil) and (FPrevPic<>FPic) then begin
      bmpPrevTemp := TBitmap32.Create;
      bmpPrevTemp.Assign(iMain.Bitmap);
    end else
      bmpPrevTemp := nil;
     // Если изображение скэшировано, копируем его
    if bPicInCache then
      try
        iMain.Bitmap.Assign(FCachedBitmap);
      except
        bmpPrevTemp.Free;
        raise;
      end;
     // Освобождаем кэш и сохраняем в кэше старое изображение
    FCachedBitmap.Free;
    FCachedBitmap := bmpPrevTemp;
    if bmpPrevTemp=nil then FCachedBitmapFilename := '' else FCachedBitmapFilename := FPrevPic.PicFileName;
     // Если не взяли из кэша, дожидаемся окончания загрузки изображения фоновым потоком
    if not bPicInCache then begin
      StartWait;
      try
        WaitForSingleObject(FDecodeThread.HDecodedEvent, INFINITE);
        bmpDecoded := FDecodeThread.GetAndReleasePicture;
         // Если поток вернул nil - значит, произошла ошибка
        FErroneous := bmpDecoded=nil;
        if FErroneous then
          PaintError(FPic.PicFileName, FDecodeThread.ErrorMessage)
        else
          try
            iMain.Bitmap.Assign(bmpDecoded);
          finally
            bmpDecoded.Free;
          end;
      finally
        StopWait;
      end;
    end;
     // Настраиваем фильтр ресэмплинга
    iMain.Bitmap.StretchFilter := FStretchFilter;
  end;

  procedure TfImgView.EnableActions;
  var iCnt: Integer;
  begin
    iCnt := FGroup.PicIDs.Count;
    aLastPic.Enabled    := FPicIdx<iCnt-1;
    aFirstPic.Enabled   := FPicIdx>0;
    aNextPic.Enabled    := (iCnt>1) and (FCyclicViewing or aLastPic.Enabled);
    aPrevPic.Enabled    := (iCnt>1) and (FCyclicViewing or aFirstPic.Enabled);
    aZoomIn.Enabled     := not FErroneous and (ZoomFactor<SMaxPicZoom);
    aZoomOut.Enabled    := not FErroneous and (ZoomFactor>SMinPicZoom);
    aZoomFit.Enabled    := not FErroneous and (ZoomFactor<>FBestFitZoomFactor);
    aZoomActual.Enabled := not FErroneous and (ZoomFactor<>1.0);
  end;

  procedure TfImgView.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    CommitInfoRelocation;
  end;

  procedure TfImgView.FormCreate(Sender: TObject);
  begin
    HelpContext := IDH_intf_view_mode;
    FDecodeThread := TDecodeThread.Create;
    FDescLayer := TPositionedLayer.Create(iMain.Layers);
    FDescLayer.OnPaint := PaintDescLayer;
  end;

  procedure TfImgView.FormDestroy(Sender: TObject);
  begin
    FDecodeThread.Terminate;
    if FTimerID<>0 then KillTimer(Handle, FTimerID);
    FCachedBitmap.Free;
  end;

  procedure TfImgView.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  var
    pTmpOffs: TPoint;
    iStep: Integer;
  begin
    pTmpOffs := ViewOffset;
    iStep := iif(ssCtrl in Shift, IKeyQuickScrollStep, iif(ssShift in Shift, IKeySlowScrollStep, IKeyScrollStep));
    case Key of
      VK_LEFT:  Inc(pTmpOffs.x, iStep);
      VK_RIGHT: Dec(pTmpOffs.x, iStep);
      VK_UP:    Inc(pTmpOffs.y, iStep);
      VK_DOWN:  Dec(pTmpOffs.y, iStep);
      VK_PAUSE: begin
        aSlideShow.Execute;
        Exit;
      end;
      else Exit;
    end;
    ViewOffset := pTmpOffs;
  end;

  procedure TfImgView.FormKeyPress(Sender: TObject; var Key: Char);
  begin
    case Key of
      #8:  aPrevPic.Execute;
      #13: aClose.Execute;
      '+': aZoomIn.Execute;
      '-': aZoomOut.Execute;
      '*': aZoomFit.Execute;
      '/': aZoomActual.Execute;
      ' ': aNextPic.Execute;
    end;
  end;

  procedure TfImgView.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  begin
    if ssCtrl in Shift then aZoomOut.Execute else aNextPic.Execute;
    Handled := True;
  end;

  procedure TfImgView.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  begin
    if ssCtrl in Shift then aZoomIn.Execute else aPrevPic.Execute;
    Handled := True;
  end;

  function TfImgView.GetViewOffset: TPoint;
  begin
    Result := Point(Trunc(iMain.OffsetHorz), Trunc(iMain.OffsetVert));
  end;

  function TfImgView.GetZoomFactor: Single;
  begin
    Result := iMain.Scale;
  end;

  procedure TfImgView.iMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
     // При клике на RBLayer выходим, чтобы не начать dragging, при клике снаружи завершаем режим ресайзинга
    if (FRBLayer<>nil) and FRBLayer.HitTest(x, y) then Exit;
    CommitInfoRelocation;
    case Button of
      mbLeft:
        if (FWScaled>FWClient) or (FHScaled>FHClient) then begin
          FTrackDrag := True;
          iMain.Cursor := crHandDrag;
          FTrackX := ViewOffset.x-x;
          FTrackY := ViewOffset.y-y;
        end;
      mbMiddle: aFullScreen.Execute;
    end;
  end;

  procedure TfImgView.iMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if FTrackDrag then ViewOffset := Point(x+FTrackX, y+FTrackY);
  end;

  procedure TfImgView.iMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if FTrackDrag then begin
      FTrackDrag := False;
       // Возвращаем прежний курсор
      iMain.Cursor := FImageCursor;
    end;
  end;

  procedure TfImgView.iMainResize(Sender: TObject);
  begin
     // Если изменение размеров вызвано не принудительным позиционированием в процессе подстройки размеров окна под
     //   размеры изображения. Если в последний раз был выбран BestFitZoom, применяем его, иначе сохраняем ZoomFactor
     //   без изменений
    if not FForcedResize then ApplyZoom(iif(FBestFitZoomUsed, FBestFitZoomFactor, ZoomFactor), False);
  end;

  procedure TfImgView.PaintDescLayer(Sender: TObject; Buffer: TBitmap32);
  var r: TRect;
  begin
     // Если есть описание
    if FPicDesc<>'' then begin
      r := MakeRect(FDescLayer.GetAdjustedLocation);
      FontFromStr(Buffer.Font, FInfoFont);
      Buffer.FillRectTS(r, (Color32(FInfoBkColor) and $00ffffff) or (Cardinal(FInfoBkOpacity) shl 24));
      Buffer.Textout(r, DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_NOPREFIX, FPicDesc);
    end;
  end;

  procedure TfImgView.RBLayerResizing(Sender: TObject; const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);
  var sw, sh: Single;
  begin
    with NewLocation do begin
      sw := MinS(Right-Left, FWClient);
      sh := MinS(Bottom-Top, FHClient);
      Left   := MinS(MaxS(0, Left), FWClient-sw);
      Top    := MinS(MaxS(0, Top),  FHClient-sh);
      Right  := MaxS(MinS(FWClient, Right),  sw);
      Bottom := MaxS(MinS(FHClient, Bottom), sh);
    end;
  end;

  procedure TfImgView.RedisplayLock;
  begin
    Inc(FDisplayLock);
  end;

  procedure TfImgView.RedisplayPic(bReload: Boolean);
  begin
     // Очищаем очередь (чтобы поток потом перегрузил изображение вновь)
    if bReload then FDecodeThread.QueuedFileName := '';
     // Вновь отображаем изображение
    DisplayPic(bReload);
  end;

  procedure TfImgView.RedisplayUnlock(bReload: Boolean);
  begin
    if FDisplayLock>0 then begin
      Dec(FDisplayLock);
      if FDisplayLock=0 then RedisplayPic(bReload);
    end;
  end;

  procedure TfImgView.RestartShowTimer;
  begin
    if FTimerID<>0 then KillTimer(Handle, FTimerID);
    if FSlideShow then FTimerID := SetTimer(Handle, ISlideShowTimerID, FSlideInterval, nil) else FTimerID := 0;
  end;

  procedure TfImgView.SetFullScreen(Value: Boolean);
  const
    aFS: Array[Boolean] of TFormStyle       = (fsNormal,   fsStayOnTop);
    aWS: Array[Boolean] of TWindowState     = (wsNormal,   wsMaximized);
    aBS: Array[Boolean] of TFormBorderStyle = (bsSizeable, bsNone);
  begin
    FFullScreen := Value;
    aFullScreen.Checked := Value;
    FormStyle   := aFS[FAlwaysOnTop and not Value];
    BorderStyle := aBS[Value];
    WindowState := aWS[Value];
     // Настраиваем видимость курсора
    AdjustCursorVisibility(False);
     // Перегружаем картинку
    RedisplayPic(False);
  end;

  procedure TfImgView.SetPicIdx(Value: Integer);
  begin
    if (FPicIdx<>Value) and (Value>=0) and (Value<FGroup.PicIDs.Count) then begin
      FPicIdx := Value;
      DisplayPic(True);
    end;
  end;

  procedure TfImgView.SetShowInfo(Value: Boolean);
  begin
    if FShowInfo<>Value then begin
      CommitInfoRelocation;
      FShowInfo := Value;
       // Перерисовываем картинку
      RedisplayPic(False);
    end;
    aShowInfo.Checked := Value;
  end;

  procedure TfImgView.SetSlideShow(Value: Boolean);
  begin
    if FSlideShow<>Value then begin
      CommitInfoRelocation;
      FSlideShow := Value;
      RestartShowTimer;
    end;
    aSlideShow.Checked := Value;
  end;

  procedure TfImgView.SetViewOffset(const Value: TPoint);
  var ix, iy: Integer;
  begin
    CommitInfoRelocation;
    if FWScaled>FWClient then ix := Min(0, Max(Value.x, FWClient-FWScaled)) else ix := (FWClient-FWScaled) div 2;
    if FHScaled>FHClient then iy := Min(0, Max(Value.y, FHClient-FHScaled)) else iy := (FHClient-FHScaled) div 2;
    iMain.OffsetHorz := ix;
    iMain.OffsetVert := iy;
  end;

  procedure TfImgView.SetZoomFactor(Value: Single);
  begin
    CommitInfoRelocation;
     // Запоминаем, если выставили BestFitZoom
    FBestFitZoomUsed := Value=FBestFitZoomFactor;
     // Применяем новый коэффициент масштабирования
    ApplyZoom(Value, True);
  end;

  procedure TfImgView.tbMainVisibleChanged(Sender: TObject);
  begin
    SetSettingValueBool(ISettingID_View_ShowToolbar, tbMain.Visible);
  end;

  procedure TfImgView.TopmostCancel;
  begin
    if FAlwaysOnTop then SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
  end;

  procedure TfImgView.TopmostRestore;
  begin
    if FAlwaysOnTop then SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
  end;

  procedure TfImgView.WMHelp(var Msg: TWMHelp);
  begin
    HtmlHelpContext(HelpContext);
  end;

  procedure TfImgView.WMTimer(var Msg: TWMTimer);
  begin
    FLastPicChangeBackwards := False;
    if PicIdx<FGroup.PicIDs.Count-1 then PicIdx := PicIdx+1
    else if FSlideCyclic then PicIdx := 0
    else SlideShow := False;
  end;

end.
