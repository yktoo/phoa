//**********************************************************************************************************************
//  $Id: phGUIObj.pas,v 1.7 2004-09-23 04:09:45 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phGUIObj;

interface
uses Windows, Messages, SysUtils, Graphics, Classes, Controls, Forms, GR32, phObj, phGraphics, ConsVars;

type  

   //===================================================================================================================
   // TThumbnailViewer - средство просмотра эскизов изображений
   //===================================================================================================================

   // Информация для отрисовки окна 
  TThumbnailViewerPaintInfo = record
    RClip:     TRect;     // ClipRect
    RClient:   TRect;     // ClientRect
    Bitmap:    TBitmap32; // Buffer bitmap object
    bFocused:  Boolean;   // True if control has focus
  end;

   // Координаты отрисовки маркера вставки перетаскиваемых эскизов
  TViewerInsertionCoord = record
    iIndex: Integer; // Индекс эскиза, ПЕРЕД которым нужно отрисовать линию вставки, [0..ItemCount], при iIndex=ItemCount
                     //   маркер должен быть отрисован после последнего эскиза
    bLower: Boolean; // Контролирует, как поступать при неоднозначности в определении положения маркера вставки, т.е.
                     //   когда idx указывает на эскиз, находящийся в начале строки и при этом не самый верхний левый.
                     //   В таком случае, если bLower=True, маркер рисуется в конце предыдущей строки, если False - в
                     //   начале текущей (слева от данного эскиза)
  end;

   // Стиль рамки подложки эскиза
  TThumbBackBorderStyle = (tbbsNone, tbbsFlat, tbbsRaised, tbbsSunken, tbbsPlain, tbbsGroove, tbbsXP, tbbsColor);

   // Угол эскиза
  TThumbCorner = (tcLeftTop, tcRightTop, tcLeftBottom, tcRightBottom);

  TThumbCornerDetail = record
    bDisplay: Boolean;      // True, если свойство в данном углу отображается
    Prop:     TPicProperty; // Свойство для отображения
  end;

  TThumbCornerDetails = Array[TThumbCorner] of TThumbCornerDetail;

   // Режим отображения TThumbnailViewer
  TThumbViewerDisplayMode = (
    tvdmTile,    // Обычный - в виде сетки из эскизов
    tvdmDetail); // Детальный - слева значок, справа описание

  TThumbnailViewer = class(TCustomControl)
  private
     // Буферный битмэп для отрисовки содержимого окна контрола
    FBuffer: TBitmap32;
     // Размер пункта-эскиза (вместе с отступами, границами и т.п.)
    FItemSize: TSize;
     // Полная высота "виртуального" окна (всех эскизов)
    FVRange: Integer;
     // Общее количество эскизов
    FItemCount: Integer;
     // Количество эскизов, целиком отображающихся в окне контрола
    FVisibleItems: Integer;
     // Количество столбцов с эскизами
    FColCount: Integer;
     // Высота одной строки текста, нарисованного шрифтом контрола
    FTextLineHeight: Integer;
     // Флаг валидности параметров шрифта
    FFontParamsValid: Boolean;

     // Список ссылок на изображения группы, наполняется вызовом SetCurrentGroup()
    FPicLinks: TPhoaPicLinks;
     // Список индексов выделенных эскизов
    FSelIndexes: TIntegerList;
     // Индекс активного эскиза
    FItemIndex: Integer;
     // Кэш bmp-изображений эскизов
    FThumbCache: TList;
     // Индекс эскиза, с которого началось поточное выделение (Shift+[стрелки] или Shift+[клик])
    FStreamSelStart: Integer;
     // Индекс эскиза, который будет выделен (ItemIndex), если пользователь отожмёт левую кнопку мыши, не сдвинув мышь
    FNoMoveItemIndex: Integer;
     // Флаг ожидания входа в Dragging
    FDragPending: Boolean;
     // Координаты нажатия мышью для Dragging/Marqueing
    FStartPos: TPoint;
     // Текущая и прежняя координаты вставки перетаскиваемых эскизов
    FDragTargetCoord: TViewerInsertionCoord;
    FOldDragTargetCoord: TViewerInsertionCoord;
     // Время для подстройки скорости скроллинга при Drag'n'Drop
    FLastDragScrollTicks: Cardinal;
     // Счётчик блокировки
    FUpdateLock: Integer;
     // Поля для рамки группового выделения (marquee)
    FTempDC: HDC;
    FMarqueing: Boolean;
    FMarqueeCur: TPoint;
     // Данные, отображаемые в углах эскизов
    FThumbCornerDetails: TThumbCornerDetails;
     // Индекс пункта, для которого последний раз отображался Tooltip
    FLastTooltipIdx: Integer;
     // True, если была нажата правая клавиша мыши вместе с Ctrl, и при её отпускании необходимо отобразить системное
     //   контекстное меню
    FShellCtxMenuOnMouseUp: Boolean;
     // Props storage
    FGroupID: Integer;
    FCacheThumbnails: Boolean;
    FDragInsideEnabled: Boolean;
    FOnSelectionChange: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    FDragEnabled: Boolean;
    FShowThumbTooltips: Boolean;
    FThumbTooltipProps: TPicProperties;
    FThumbCacheSize: Integer;
    FThumbBackColor: TColor;
    FThumbFontColor: TColor;
    FOnStartViewMode: TNotifyEvent;
    FDisplayMode: TThumbViewerDisplayMode;
    FTopOffset: Integer;
    FThumbnailSize: TSize;
    FThumbBackBorderStyle: TThumbBackBorderStyle;
    FThumbBackBorderColor: TColor;


     // Painting stage handlers
    procedure Paint_EraseBackground(const Info: TThumbnailViewerPaintInfo);
    procedure Paint_Thumbnails(const Info: TThumbnailViewerPaintInfo);
    procedure Paint_Thumbnail(const Info: TThumbnailViewerPaintInfo; iIndex: Integer; ItemRect: TRect; bSelected: Boolean);
    procedure Paint_TransferBuffer(const Info: TThumbnailViewerPaintInfo);
     // Validates the specified offset and returns the correctly ranged offset value
    function  GetValidTopOffset(iOffset: Integer): Integer;
     // Рассчитывает параметры шрифта
    procedure CalcFontParams;
     // Рассчитывает основные параметры отображения эскизов
    procedure CalcLayout;
     // Возвращает индекс самого верхнего отображаемого эскиза
    function  GetFirstVisibleIndex: Integer;


     // Убирает выделение со всех эскизов и возвращает True, если оно было
    function  ClearSelection: Boolean;
     // Ставит или убирает выделение с эскиза
    procedure ToggleSelection(Index: Integer);
    procedure AddToSelection(Index: Integer);
    procedure RemoveFromSelection(Index: Integer);
     // Перемещает ItemIndex на новое место, не трогая выделения. При bUpdateStreamSelStart=True также обновляет
     //   FStreamSelStart
    procedure MoveItemIndex(iNewIndex: Integer; bUpdateStreamSelStart: Boolean);
     // Вызывает OnSelectionChange
    procedure DoSelectionChange;
     // Вызывает OnStartViewMode
    procedure DoStartViewMode;
     // Возвращает ссылку на кэшированный эскиз _Pic, если он есть в кэше, иначе возвращает nil
    function  GetCachedThumb(_Pic: TPhoaPic): TBitmap;
     // Помещает эскиз в кэш. После этого кэш становится владельцем Bitmap. Должна вызываться только в том случае, если
     //   в кэше нет данного изображения!
    procedure PutThumbToCache(Pic: TPhoaPic; Bitmap: TBitmap);
     // Урезает размер кэша до iNumber изображений или совсем, если кэширование отключено
    procedure LimitCacheSize(iNumber: Integer);
     // Настраивает ScrollBar
    procedure UpdateScrollBar;
     // Выделяет диапазон индексов эскизов и сдвигает ItemIndex в idxEnd. При bRectangular=False выделяет эскизы подряд
     //   по их индексам, при bRectangular=True выделяет прямоугольный блок
    procedure SelectRange(idxStart, idxEnd: Integer; bRectangular: Boolean);
     // Работа с рамкой группового выделения (marquee)
    procedure PaintMarquee;
    procedure MarqueingStart;
    procedure MarqueingEnd;
     // Отрисовывает маркер вставки перетаскиваемых эскизов. Если bInvalidate=True, после отрисовки сбрасывает iIndex в
     //   -1 и bLower в False
    procedure DragDrawInsertionPoint(var Coord: TViewerInsertionCoord; bInvalidate: Boolean);
     // Настраивает всплывающие описания эскизов в виде Hint
    procedure AdjustTooltip(ix, iy: Integer);
     // Message handlers
    procedure WMContextMenu(var Msg: TWMContextMenu);           message WM_CONTEXTMENU;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode);             message WM_GETDLGCODE;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd);             message WM_ERASEBKGND;
    procedure WMVScroll(var Msg: TWMVScroll);                   message WM_VSCROLL;
    procedure WMNCPaint(var Msg: TWMNCPaint);                   message WM_NCPAINT;
    procedure CMDrag(var Msg: TCMDrag);                         message CM_DRAG;
    procedure CMFontChanged(var Msg: TMessage);                 message CM_FONTCHANGED;
    procedure CMInvalidate(var Msg: TMessage);                  message CM_INVALIDATE;
     // Prop handlers
    procedure SetBorderStyle(Value: TBorderStyle);
    function  GetSelCount: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetCacheThumbnails(Value: Boolean);
    function  GetSelectedIndexes(Index: Integer): Integer;
    function  GetIDSelected(iID: Integer): Boolean;
    function  GetSelectedPics(Index: Integer): TPhoaPic;
    function  GetDropTargetIndex: Integer;
    function  GetThumbCornerDetails(Corner: TThumbCorner): TThumbCornerDetail;
    procedure SetThumbCornerDetails(Corner: TThumbCorner; const Value: TThumbCornerDetail);
    procedure SetShowThumbTooltips(Value: Boolean);
    procedure SetThumbTooltipProps(Value: TPicProperties);
    procedure SetThumbCacheSize(Value: Integer);
    procedure SetThumbBackColor(Value: TColor);
    procedure SetThumbFontColor(Value: TColor);
    procedure SetDisplayMode(Value: TThumbViewerDisplayMode);
    procedure SetTopOffset(Value: Integer);
    procedure SetThumbnailSize(const Value: TSize);
    procedure SetThumbBackBorderStyle(Value: TThumbBackBorderStyle);
    procedure SetThumbBackBorderColor(Value: TColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     // Сохраняет текущие параметры отображения и возвращает:
     //   в SelectedIDs - список ID выделенных изображений (может быть nil!)
     //   в iFocusedID  - ID изображения, которому соответствует ItemIndex (0, если нет такого)
     //   в iTopOffset  - текущий TopOffset
    procedure SaveDisplay(out SelectedIDs: TIntegerList; out iFocusedID, iTopOffset: Integer);
     // Восстанавливает параметры отображения; подразумевается возможность использования данных,
     //   полученных в результате предыдущего вызова SaveDisplay():
     //     Если SelectedIDs<>nil, то сразу выделяет изображения с заданными ID
     //     Если iFocusedID>0, устанавливает ItemIndex на изображение с заданным ID
     //     iTopOffset задаёт желаемый индекс верхнего эскиза
    procedure RestoreDisplay(SelectedIDs: TIntegerList; iFocusedID, iTopOffset: Integer);
     // Устанавливает группу Group для просмотра в качестве текущей
    procedure ViewGroup(PhoA: TPhotoAlbum; Group: TPhoaGroup; bRecurse: Boolean);
     // Снимает выделение со всех эскизов
    procedure SelectNone;
     // Выделяет все эскизы
    procedure SelectAll;
     // Возвращает массив выделенных изображений
    function  GetSelectedPicArray: TPicArray;
     // Возвращает ItemIndex в точке, или -1, если там нет эскиза
    function  ItemAtPos(ix, iy: Integer): Integer;
     // Возвращает координаты эскиза с индексом Index. Если bAllowInvisible=False, то возвращает пустой прямоугольник,
     //   если эскиз не пересекается с ClientRect
    function  ItemRect(Index: Integer; bAllowInvisible: Boolean): TRect;
     // Ставит эскиз в очередь на обновление
    procedure InvalidateItem(Index: Integer);
     // Прокручивает содержимое, чтобы было видно ItemIndex
    procedure ScrollIntoView;
     // Блокировка перерисовки
    procedure BeginUpdate;
    procedure EndUpdate;
     // Props
    property DisplayMode: TThumbViewerDisplayMode read FDisplayMode write SetDisplayMode; 
     // -- Флаг, разрешающий перетаскивание эскизов
    property DragEnabled: Boolean read FDragEnabled write FDragEnabled;
     // -- Флаг, разрешающий перестановку эскизов внутри окна с помощью Drag'n'Drop. Если перестановка запрещена, а
     //    DragEnabled=True, то можно только "вытаскивать" эскизы наружу
    property DragInsideEnabled: Boolean read FDragInsideEnabled write FDragInsideEnabled;
     // -- Индекс последнего места вставки при Drag'n'Drop. -1, если не было подходящего
    property DropTargetIndex: Integer read GetDropTargetIndex;
     // -- ID группы, отображаемой в данный момент (0, если нет)
    property GroupID: Integer read FGroupID;
     // -- True, если изображение с заданным ID выделено
    property IDSelected[iID: Integer]: Boolean read GetIDSelected;
     // -- Индекс сфокусированного изображения
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
     // -- Количество выделенных изображений
    property SelCount: Integer read GetSelCount;
     // -- Индексы выделенных изображений в общем списке изображений viewer'а (Index - индекс выделенного изображения,
     //    0..SelCount-1)
    property SelectedIndexes[Index: Integer]: Integer read GetSelectedIndexes;
     // -- Выделенные изображения (Index - индекс выделенного изображения, 0..SelCount-1)
    property SelectedPics[Index: Integer]: TPhoaPic read GetSelectedPics;
     // -- Если True, отображает всплывающие описания эскизов
    property ShowThumbTooltips: Boolean read FShowThumbTooltips write SetShowThumbTooltips;
     // -- Размер кэша эскизов
    property ThumbCacheSize: Integer read FThumbCacheSize write SetThumbCacheSize;
     // -- Данные, отображаемые на эскизах
    property ThumbCornerDetails[Corner: TThumbCorner]: TThumbCornerDetail read GetThumbCornerDetails write SetThumbCornerDetails;
     // -- Данные, отображаемые на всплывающих описаниях эскизов
    property ThumbTooltipProps: TPicProperties read FThumbTooltipProps write SetThumbTooltipProps;
     // -- Размеры эскизов
    property ThumbnailSize: TSize read FThumbnailSize write SetThumbnailSize;
     // -- Смещение верхнего края окна относительно начала списка эскизов, в пикселах
    property TopOffset: Integer read FTopOffset write SetTopOffset;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
     // -- Кэшировать ли эскизы при просмотре
    property CacheThumbnails: Boolean read FCacheThumbnails write SetCacheThumbnails default True;
    property Color default clBtnFace;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
     // -- Цвет рамки фона эскиза при ThumbBackBorderStyle=tbbsColor
    property ThumbBackBorderColor: TColor read FThumbBackBorderColor write SetThumbBackBorderColor default clGray;
     // -- Стиль рамки фона эскиза
    property ThumbBackBorderStyle: TThumbBackBorderStyle read FThumbBackBorderStyle write SetThumbBackBorderStyle default tbbsXP;  
     // -- Цвет фона эскизов
    property ThumbBackColor: TColor read FThumbBackColor write SetThumbBackColor default clBtnFace;
     // -- Цвет шрифта эскизов
    property ThumbFontColor: TColor read FThumbFontColor write SetThumbFontColor default clWindowText;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
     // -- Событие изменения выделения во viewer'е
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
     // -- Событие, происходящее тогда, когда нужно начать просмотр изображений
    property OnStartViewMode: TNotifyEvent read FOnStartViewMode write FOnStartViewMode;
  end;

   //===================================================================================================================
   // Окошко подсказки, поддерживающее вывод строк в две колонки (строки разделяются символом #9, первая строка
   // выравнивается по левому краю, вторая - по правому)
   //===================================================================================================================

  TPhoAHintWindow = class(THintWindow)
    procedure Paint; override;
  end;

   //===================================================================================================================
   // "Уголок" для изменения размеров формы
   //===================================================================================================================

  TSizeGripper = class(TCustomControl)
  private
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses Math, Themes, TBX, TBXThemes, phUtils, main;

type
   // Запись кэша эскиза
  PThumbCacheRec = ^TThumbCacheRec;
  TThumbCacheRec = record
    Pic: TPhoaPic;
    Thumb: TBitmap;
  end;

   //===================================================================================================================
   // TThumbnailViewer
   //===================================================================================================================

  procedure TThumbnailViewer.AddToSelection(Index: Integer);
  begin
    if (Index>=0) and (Index<FPicLinks.Count) and FSelIndexes.Add(Index) then InvalidateItem(Index);
  end;

  procedure TThumbnailViewer.AdjustTooltip(ix, iy: Integer);
  var idx: Integer;
  begin
     // Настраиваем Thumbnail Tooltip
    if FShowThumbTooltips and (ThumbTooltipProps<>[]) and (GetKeyState(VK_LBUTTON) and $80=0) then idx := ItemAtPos(ix, iy) else idx := -1;
     // Если индекс эскиза, для которого последний раз отображался Tooltip, поменялся
    if idx<>FLastTooltipIdx then begin
       // Скрываем Tooltip, если есть
      Application.CancelHint;
       // Строим описание (добавляем палку, чтобы в StatusBar ничего не попадало)
      if idx<0 then Hint := '' else Hint := FPicLinks[idx].GetPropStrs(FThumbTooltipProps, ':'#9, #13)+'|';
      FLastTooltipIdx := idx;
    end;
  end;

  procedure TThumbnailViewer.BeginUpdate;
  begin
    if FUpdateLock=0 then Perform(WM_SETREDRAW, 0, 0);
    Inc(FUpdateLock);
  end;

  procedure TThumbnailViewer.CalcFontParams;
  var Cnv: TCanvas;
  begin
     // Находим высоту строки текста
    Cnv := TCanvas.Create;
    try
      Cnv.Handle := GetDC(0);
      try
        Cnv.Font.Assign(Font);
        FTextLineHeight := Cnv.TextHeight('Wg');
        FFontParamsValid := True;
      finally
        ReleaseDC(0, Cnv.Handle);
        Cnv.Handle := 0;
      end;
    finally
      Cnv.Free;
    end;
  end;

  procedure TThumbnailViewer.CalcLayout;
  var
    iPrevItemCount, iPrevColCount, iPrevTopOffset: Integer;
    PrevDisplayMode: TThumbViewerDisplayMode;
  begin
    if (FUpdateLock>0) or not HandleAllocated then Exit;
     // Сбрасываем Tooltip/Hint
    FLastTooltipIdx := -1;
    Hint := '';
     // Сохраняем старые параметры
    iPrevItemCount  := FItemCount;
    iPrevColCount   := FColCount;
    iPrevTopOffset  := FTopOffset;
    PrevDisplayMode := FDisplayMode;
     // Если параметры шрифта не валидны, инициализируем их
    if not FFontParamsValid then CalcFontParams;
     // Находим размеры ячейки и количество столбцов
    FItemSize := FThumbnailSize;
    case FDisplayMode of
      tvdmTile: begin
         // -- Прибавляем отступы на краях эскиза
        Inc(FItemSize.cx, IThumbMarginH*2+IThumbPaddingL+IThumbPaddingR);
        Inc(FItemSize.cy, IThumbMarginV*2+IThumbPaddingT+IThumbPaddingB);
         // -- Прибавляем отступы на данные изображений
        if FThumbCornerDetails[tcLeftTop].bDisplay    or FThumbCornerDetails[tcRightTop].bDisplay    then Inc(FItemSize.cy, FTextLineHeight);
        if FThumbCornerDetails[tcLeftBottom].bDisplay or FThumbCornerDetails[tcRightBottom].bDisplay then Inc(FItemSize.cy, FTextLineHeight);
         // Считаем количество столбцов
        FColCount := Max(1, ClientWidth div FItemSize.cx);
      end;
      else {tvdmDetail} begin
        FColCount := 1;
        FItemSize.cx := Max(FThumbnailSize.cx+IThumbPaddingL+IThumbPaddingR, ClientWidth);
        Inc(FItemSize.cy, IThumbMarginV*2+IThumbPaddingT+IThumbPaddingB);
      end;
    end;
     // Обновляем значения
    FItemCount    := FPicLinks.Count;
    FVisibleItems := (ClientHeight div FItemSize.cy)*FColCount;
    FVRange       := Ceil(FItemCount/FColCount)*FItemSize.cy;
    FTopOffset    := GetValidTopOffset(FTopOffset);
     // Обновляем отображаемые данные при наличии изменений
    if (FItemCount<>iPrevItemCount) or (FDisplayMode<>PrevDisplayMode) or (FColCount<>iPrevColCount) or (FTopOffset<>iPrevTopOffset) then Invalidate;
    UpdateScrollBar;
  end;

  function TThumbnailViewer.ClearSelection: Boolean;
  var i: Integer;
  begin
     // Если есть выделение
    Result := FSelIndexes.Count>0;
    if Result then
       // Если обновление не блокировано, invalidate thumbnails selected
      if FUpdateLock=0 then
        for i := FSelIndexes.Count-1 downto 0 do begin
          InvalidateItem(FSelIndexes[i]);
          FSelIndexes.Delete(i);
        end
       // Иначе просто очищаем
      else
        FSelIndexes.Clear;
  end;

  procedure TThumbnailViewer.CMDrag(var Msg: TCMDrag);
  begin
    if Msg.DragMessage in [dmDragDrop, dmDragCancel] then DragDrawInsertionPoint(FOldDragTargetCoord, True);
    inherited;
  end;

  procedure TThumbnailViewer.CMFontChanged(var Msg: TMessage);
  begin
    FFontParamsValid := False;
  end;

  procedure TThumbnailViewer.CMInvalidate(var Msg: TMessage);
  begin
    if HandleAllocated then InvalidateRect(Handle, nil, True);
  end;

  constructor TThumbnailViewer.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    ControlStyle          := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable];
    Width                 := 100;
    Height                := 100;
    TabStop               := True;
    ParentColor           := False;
    Color                 := clBtnFace;
    FBorderStyle          := bsSingle;
    FColCount             := 1;
    FCacheThumbnails      := True;
    FNoMoveItemIndex      := -1;
    FPicLinks             := TPhoaPicLinks.Create(False);
    FSelIndexes           := TIntegerList.Create(False);
    FThumbBackBorderColor := clGray;
    FThumbBackBorderStyle := tbbsXP;
    FThumbBackColor       := clBtnFace;
    FThumbFontColor       := clWindowText;
    FThumbCache           := TList.Create;
    FThumbnailSize.cx     := IDefaultThumbWidth;
    FThumbnailSize.cy     := IDefaultThumbHeight;
  end;

  procedure TThumbnailViewer.CreateParams(var Params: TCreateParams);
  begin
    inherited CreateParams(Params);
    with Params do begin
      WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
      Style := Style or WS_VSCROLL;
      if FBorderStyle=bsSingle then
        if NewStyleControls and Ctl3D then begin
          Style := Style and not WS_BORDER;
          ExStyle := ExStyle or WS_EX_CLIENTEDGE;
        end else
          Style := Style or WS_BORDER;
    end;
  end;

  procedure TThumbnailViewer.CreateWnd;
  var iw, ih: Integer;
  begin
    iw := Width;
    ih := Height;
    inherited CreateWnd;
    SetWindowPos(Handle, 0, Left, Top, iw, ih, SWP_NOZORDER or SWP_NOACTIVATE);
  end;

  procedure TThumbnailViewer.DblClick;
  begin
    DoStartViewMode;
  end;

  destructor TThumbnailViewer.Destroy;
  begin
    FSelIndexes.Free;
    LimitCacheSize(0);
    FThumbCache.Free;
    FPicLinks.Free;
    FBuffer.Free;
    inherited Destroy;
  end;

  procedure TThumbnailViewer.DoSelectionChange;
  begin
    if (FUpdateLock=0) and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  end;

  procedure TThumbnailViewer.DoStartViewMode;
  begin
    if Assigned(FOnStartViewMode) then FOnStartViewMode(Self);
  end;

  procedure TThumbnailViewer.DragDrawInsertionPoint(var Coord: TViewerInsertionCoord; bInvalidate: Boolean);
//  var
//    dc: HDC;
//    hp, hOld: HPEN;
//    idx, ix, iy: Integer;
  begin
//    if Coord.iIndex>=0 then begin
//       // Находим пиксельные координаты
//      idx := Coord.iIndex;
//       // Если не выше клиентской области
//      if idx>=FTopIndex then begin
//        Dec(idx, FTopIndex);
//        ix := (idx mod FColCount)*FItemSize.cx;
//        iy := (idx div FColCount)*FItemSize.cy;
//         // Если не ниже нижней границы клиентской области
//        if iy<ClientHeight then begin
//           // Неоднозначность?
//          if (ix=0) and Coord.bLower then begin
//            ix := FColCount*FItemSize.cx;
//            Dec(iy, FItemSize.cy);
//          end;
//           // Отрисовываем
//          dc := GetDCEx(Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
//          hp := CreatePen(PS_SOLID, 3, ColorToRGB(Color) xor ColorToRGB(CInsertionPoint));
//          hOld := SelectObject(dc, hp);
//          SetROP2(dc, R2_XORPEN);
//          MoveToEx(dc, ix, iy, nil);
//          LineTo(dc, ix, iy+FItemSize.cy);
//          MoveToEx(dc, ix-3, iy, nil);
//          LineTo(dc, ix+3, iy);
//          MoveToEx(dc, ix-3, iy+FItemSize.cy, nil);
//          LineTo(dc, ix+3, iy+FItemSize.cy);
//          SelectObject(dc, hOld);
//          DeleteObject(hp);
//          ReleaseDC(Handle, dc);
//        end;
//      end;
//       // Invalidate coord if needed
//      if bInvalidate then begin
//        Coord.iIndex := -1;
//        Coord.bLower := False;
//      end;
//    end;
  end;

  procedure TThumbnailViewer.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
//  var
//    iCol, iRow: Integer;
//    bLower: Boolean;
//
//    procedure SpeedAdjust;
//    var cTicks: Cardinal;
//    begin
//      cTicks := GetTickCount;
//       // Первый раз не ждём
//      if (FLastDragScrollTicks>0) and (cTicks<FLastDragScrollTicks+IDragScrollDelay) then Sleep(FLastDragScrollTicks+IDragScrollDelay-cTicks);
//      FLastDragScrollTicks := cTicks;
//    end;
//
//     // Прокручивает список эскизов вверх (bUp=True) или вниз (bUp=False)
//    procedure DoScroll(bUp: Boolean);
//    begin
//       // Подстраиваем скорость
//      SpeedAdjust;
//       // Стираем старое место вставки
//      DragDrawInsertionPoint(FOldDragTargetCoord, True);
//       // Прокручиваем
//      SetTopIndex(FTopIndex+iif(bUp, -FColCount, FColCount));
//      Update;
//    end;
//
  begin
//    if (Source=Self) and FDragInsideEnabled then begin
//      FDragTargetCoord.iIndex := -1;
//      FDragTargetCoord.bLower := False;
//       // Если входим/находимся в контроле
//      if PtInRect(ClientRect, Point(x, y)) then begin
//         // Если у верхней или нижней границ окна, мотаем
//        if (y<IDragScrollAreaMargin) or (y>=ClientHeight-IDragScrollAreaMargin) then DoScroll(y<IDragScrollAreaMargin);
//         // Определяем место вставки
//        if FItemCount>0 then begin
//           // Находим столбец (с округлением)
//          iCol := Round(x/FItemSize.cx);
//           // Если справа от эскизов, включаем режим bLower и устанавливаем индекс на эскиз в начале следующей строки
//          bLower := iCol>=FColCount;
//          if bLower then iCol := FColCount;
//           // Находим строку (индекс эскиза в начале строки)
//          iRow := FTopIndex+(y div FItemSize.cy)*FColCount;
//           // Если строка содержит эскизы
//          if iRow<FItemCount then begin
//            FDragTargetCoord.iIndex := iRow+iCol;
//             // Если указывает за последний эскиз - надо включить bLower
//            if FDragTargetCoord.iIndex>=FItemCount then begin
//              FDragTargetCoord.iIndex := FItemCount;
//              bLower := True;
//            end;
//            FDragTargetCoord.bLower := bLower;
//          end;
//        end;
//      end;
//       // Если изменилось положение маркера - отрисовываем его
//      if (FDragTargetCoord.iIndex<>FOldDragTargetCoord.iIndex) or (FDragTargetCoord.bLower<>FOldDragTargetCoord.bLower) then begin
//        DragDrawInsertionPoint(FOldDragTargetCoord, False);
//        DragDrawInsertionPoint(FDragTargetCoord, False);
//        FOldDragTargetCoord := FDragTargetCoord;
//      end;
//       // Drop возможен, если есть нормальная координата вставки, и перетаскиваются не все разом, и не единственное
//       //   изображение или место вставки не совпадает с положением этого изображения
//      Accept :=
//        (FDragTargetCoord.iIndex>=0) and
//        (FSelIndexes.Count<FPicLinks.Count) and
//        ((FSelIndexes.Count>1) or ((FSelIndexes[0]<>FDragTargetCoord.iIndex) and (FSelIndexes[0]<>FDragTargetCoord.iIndex-1)));
//    end else
//      inherited DragOver(Source, X, Y, State, Accept);
  end;

  procedure TThumbnailViewer.EndUpdate;
  begin
    if FUpdateLock>0 then Dec(FUpdateLock);
    if FUpdateLock=0 then begin
      Perform(WM_SETREDRAW, 1, 0);
      CalcLayout;
      Refresh;
      DoSelectionChange;
    end;
  end;

  function TThumbnailViewer.GetCachedThumb(_Pic: TPhoaPic): TBitmap;
  var i: Integer;
  begin
    for i := 0 to FThumbCache.Count-1 do
      with PThumbCacheRec(FThumbCache[i])^ do
        if Pic=_Pic then begin
          Result := Thumb;
           // Перемещаем изображение в начало кэша
          FThumbCache.Move(i, 0);
          Exit;
        end;
    Result := nil;
  end;

  function TThumbnailViewer.GetDropTargetIndex: Integer;
  begin
    Result := FDragTargetCoord.iIndex;
  end;

  function TThumbnailViewer.GetFirstVisibleIndex: Integer;
  begin
    Result := ItemAtPos(0, 0);
  end;

  function TThumbnailViewer.GetIDSelected(iID: Integer): Boolean;
  var idx: Integer;
  begin
    Result := False;
    idx := FPicLinks.IndexOfID(iID);
    if (idx>=0) and (FSelIndexes.IndexOf(idx)>=0) then Result := True;
  end;

  function TThumbnailViewer.GetSelCount: Integer;
  begin
    Result := FSelIndexes.Count;
  end;

  function TThumbnailViewer.GetSelectedIndexes(Index: Integer): Integer;
  begin
    Result := FSelIndexes[Index];
  end;

  function TThumbnailViewer.GetSelectedPicArray: TPicArray;
  var i: Integer;
  begin
    SetLength(Result, FSelIndexes.Count);
    for i := 0 to FSelIndexes.Count-1 do Result[i] := GetSelectedPics(i);
  end;

  function TThumbnailViewer.GetSelectedPics(Index: Integer): TPhoaPic;
  begin
    Result := FPicLinks[FSelIndexes[Index]];
  end;

  function TThumbnailViewer.GetThumbCornerDetails(Corner: TThumbCorner): TThumbCornerDetail;
  begin
    Result := FThumbCornerDetails[Corner];
  end;

  function TThumbnailViewer.GetValidTopOffset(iOffset: Integer): Integer;
  begin
    Result := Max(0, Min(iOffset, FVRange-ClientHeight));
  end;

  procedure TThumbnailViewer.InvalidateItem(Index: Integer);
  var r: TRect;
  begin
    if (FUpdateLock=0) and HandleAllocated then begin
      r := ItemRect(Index, False);
      if not IsRectEmpty(r) then InvalidateRect(Handle, @r, False);
    end;
  end;

  function TThumbnailViewer.ItemAtPos(ix, iy: Integer): Integer;
  var iCol: Integer;
  begin
    Result := -1;
    if FItemCount>0 then begin
      iCol := ix div FItemSize.cx;
      if iCol<FColCount then begin
        Result := ((iy+FTopOffset) div FItemSize.cy)*FColCount+iCol;
        if Result>=FItemCount then Result := -1;
      end;
    end;
  end;

  function TThumbnailViewer.ItemRect(Index: Integer; bAllowInvisible: Boolean): TRect;
  var ix, iy: Integer;
  begin
    FillChar(Result, SizeOf(Result), 0);
    if (Index>=0) and (Index<FItemCount) then begin
      ix := (Index mod FColCount)*FItemSize.cx;
      iy := (Index div FColCount)*FItemSize.cy-FTopOffset;
      if bAllowInvisible or ((iy+FItemSize.cy>0) and (iy<ClientHeight)) then Result := Rect(ix, iy, ix+FItemSize.cx, iy+FItemSize.cy);
    end;
  end;

  procedure TThumbnailViewer.KeyDown(var Key: Word; Shift: TShiftState);

     // Возвращает ItemIndex, который должен был бы быть при "простом" нажатии на клавишу (без Shift, Ctrl etc). При
     //   bAllowLRBoundaryCross=False стрелки влево-вправо не могут "перейти" через вертикальные правую/левую границы
     //   сетки эскизов (на следующую/предыдущую строку)
    function GetItemIndexForKey(bAllowLRBoundaryCross: Boolean): Integer;
    begin
      Result := FItemIndex;
      case Key of
        VK_UP:     Dec(Result, FColCount);
        VK_DOWN:   Inc(Result, FColCount);
        VK_LEFT:   if bAllowLRBoundaryCross or (Result mod FColCount>0) then Dec(Result);
        VK_RIGHT:  if bAllowLRBoundaryCross or (Result mod FColCount<FColCount-1) then Inc(Result);
        VK_HOME:   Result := 0;
        VK_END:    Result := FItemCount-1;
        VK_PRIOR:  Dec(Result, FVisibleItems);
        VK_NEXT:   Inc(Result, FVisibleItems);
      end;
       // Корректируем результат (если FItemCount=0, то только в этом случае Result будет -1)
      if Result>=FItemCount then Result := FItemCount-1
      else if Result<0 then Result := 0;
    end;

  begin
    case Key of
       // Enter - входим в режим просмотра
      VK_RETURN: if Shift=[] then DoStartViewMode;
       // Стрелки - обрабатываем движения
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT: begin
         // Без модификаторов или с Shift, но нет поточного выделения
        if (Shift=[]) or ((Shift=[ssShift]) and (FStreamSelStart<0)) then begin
          ItemIndex := GetItemIndexForKey(True);
          ScrollIntoView;
         // Нажат Shift - создаём поточное выделение, Shift+Alt - прямоугольное выделение
        end else if (Shift=[ssShift]) or (Shift=[ssShift, ssAlt]) then begin
          SelectRange(FStreamSelStart, GetItemIndexForKey(not (ssAlt in Shift)), ssAlt in Shift);
          ScrollIntoView;
         // Нажат Ctrl - двигаем только ItemIndex, не меняя выделения
        end else if Shift=[ssCtrl] then begin
          MoveItemIndex(GetItemIndexForKey(True), True);
          ScrollIntoView;
        end
      end;
    end;
  end;

  procedure TThumbnailViewer.LimitCacheSize(iNumber: Integer);
  var
    i: Integer;
    p: PThumbCacheRec;
  begin
    if not FCacheThumbnails then iNumber := 0;
    for i := FThumbCache.Count-1 downto iNumber do begin
      p := FThumbCache[i];
      p.Thumb.Free;
      Dispose(p);
      FThumbCache.Delete(i);
    end;
  end;

  procedure TThumbnailViewer.MarqueingEnd;
//  var
//    i: Integer;
//    r, rThumb: TRect;
  begin
//    ReleaseCapture;
//    FMarqueing := False;
//    PaintMarquee;
//    ReleaseDC(Handle, FTempDC);
//     // Если не был нажат Shift, очищаем Selection (выделяем с нуля)
//    if GetKeyState(VK_SHIFT) and $80=0 then ClearSelection;
//     // Select thumbnails that do intersect with marquee
//    r := OrderRect(Rect(FStartPos, FMarqueeCur));
//    i := FTopIndex;
//    while (i<FTopIndex+FVisibleItems+FColCount) and (i<FItemCount) do begin
//      rThumb := ItemRect(i);
//      IntersectRect(rThumb, rThumb, r);
//      if not IsRectEmpty(rThumb) then AddToSelection(i);
//      Inc(i);
//    end;
//    DoSelectionChange;
  end;

  procedure TThumbnailViewer.MarqueingStart;
  begin
//    SetCapture(Handle);
//    FTempDC := GetDCEx(Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
//    FMarqueing := True;
//    FMarqueeCur := FStartPos;
  end;

  procedure TThumbnailViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var idx: Integer;
  begin
    SetFocus;
    idx := ItemAtPos(x, y);
    FStartPos := Point(x, y);
     // Если нажат Alt - начинаем выделение (marqueing)
    if ssAlt in Shift then begin
      if Button=mbLeft then MarqueingStart;
     // Если нажат Ctrl
    end else if ssCtrl in Shift then begin
       // Если левая кнопка - переключаем выделение эскиза, на котором кликнули
      if Button=mbLeft then begin
        ToggleSelection(idx);
        MoveItemIndex(idx, True);
       // Если правая кнопка - готовим вызов Shell Context Menu
      end else if Button=mbRight then begin
        ClearSelection;
        SetItemIndex(idx);
        if idx>=0 then begin
          FShellCtxMenuOnMouseUp := True;
          Exit;
        end;
      end;
      DoSelectionChange;
     // Если нажат Shift - выделяем подряд идущие эскизы
    end else if ssShift in Shift then
      if FStreamSelStart>=0 then SelectRange(FStreamSelStart, idx, False) else SetItemIndex(idx)
     // Не нажато кнопок
    else begin
      if (idx<0) or (FSelIndexes.IndexOf(idx)<0) then begin
        SetItemIndex(idx);
        FNoMoveItemIndex := -1;
      end else if Button=mbLeft then
        FNoMoveItemIndex := idx;
      if Button=mbLeft then
        if idx<0 then MarqueingStart
        else if FDragEnabled and (FSelIndexes.Count>0) then FDragPending := True;
    end;
    inherited MouseDown(Button, Shift, x, y);
  end;

  procedure TThumbnailViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
  begin
     // Настраиваем Tooltip
    AdjustTooltip(x, y);
     // Если рисуем рамку выделения
    if FMarqueing then begin
      PaintMarquee;
      FMarqueeCur := Point(Min(Max(x, 0), ClientWidth), y);
      PaintMarquee;
     // Проверяем ожидание Dragging
    end else if FDragPending then begin
      if (FStartPos.x<>x) or (FStartPos.y<>y) then FNoMoveItemIndex := -1;
      if Sqr(FStartPos.x-x)+Sqr(FStartPos.y-y)>9 then begin
        FDragPending := False;
         // Инициализируем переменные
        FDragTargetCoord.iIndex    := -1;
        FDragTargetCoord.bLower    := False;
        FOldDragTargetCoord.iIndex := -1;
        FOldDragTargetCoord.bLower := False;
        FLastDragScrollTicks := 0;
         // Начинаем Dragging
        BeginDrag(True);
      end;
    end;
    inherited MouseMove(Shift, x, y);
  end;

  procedure TThumbnailViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
     // Была рамка группового выделения
    if FMarqueing then
      MarqueingEnd
     // Если была нажата правая кнопка вместе с Ctrl - вызываем системное контекстное меню 
    else if FShellCtxMenuOnMouseUp then begin
      FShellCtxMenuOnMouseUp := False;
      if (ssCtrl in Shift) and (FItemIndex>=0) and (FItemIndex=ItemAtPos(x, y)) then ShowFileShellContextMenu(FPicLinks[FItemIndex].PicFileName);
      Exit;
     // Иначе завершаем Dragging
    end else begin
      FDragPending := False;
      if FNoMoveItemIndex>=0 then begin
        if not Dragging then SetItemIndex(FNoMoveItemIndex);
        FNoMoveItemIndex := -1;
      end;
    end;
    inherited MouseUp(Button, Shift, x, y);
  end;

  procedure TThumbnailViewer.MoveItemIndex(iNewIndex: Integer; bUpdateStreamSelStart: Boolean);
  begin
    if iNewIndex<>FItemIndex then begin
      InvalidateItem(FItemIndex);
      FItemIndex := iNewIndex;
      InvalidateItem(FItemIndex);
    end;
    if bUpdateStreamSelStart then FStreamSelStart := iNewIndex;
  end;

  procedure TThumbnailViewer.Paint;
  var Info: TThumbnailViewerPaintInfo;
  begin
     // Создаём буферный битмэп, если он ещё не создан
    if FBuffer=nil then FBuffer := TBitmap32.Create;
     // Создаём структуру данных для отрисовки
    with Info do begin
      RClip    := Canvas.ClipRect;
      RClient  := ClientRect;
      if IsRectEmpty(RClient) then Exit;
      FBuffer.SetSize(RClient.Right-RClient.Left, RClient.Bottom-RClient.Top);
      Bitmap   := FBuffer;
      bFocused := Focused;
    end;
     // Invoke the painting stages one-by-one
    Paint_EraseBackground(Info);
    Paint_Thumbnails(Info);
    Paint_TransferBuffer(Info);
  end;

  procedure TThumbnailViewer.PaintMarquee;
  var r: TRect;
  begin
    r := OrderRect(Rect(FStartPos, FMarqueeCur));
    if not IsRectEmpty(r) then DrawFocusRect(FTempDC, r);
  end;

  procedure TThumbnailViewer.Paint_EraseBackground(const Info: TThumbnailViewerPaintInfo);
  begin
    Info.Bitmap.FillRectS(Info.RClip, Color32(Color));
  end;

  procedure TThumbnailViewer.Paint_Thumbnail(const Info: TThumbnailViewerPaintInfo; iIndex: Integer; ItemRect: TRect; bSelected: Boolean);
  const aSelectedFontClr: Array[Boolean] of TColor = (clWindowText, clHighlightText);
  var
    Pic: TPhoaPic;
    r, rInner: TRect;

     // Отрисовывает на эскизе данные одного угла. Возвращает ширину отрисованного текста
    function DrawDetail(Corner: TThumbCorner; rText: TRect): Integer;
    var sProp: String;
    begin
      Result := 0;
      if (FThumbCornerDetails[Corner].bDisplay) and (rText.Left<rText.Right) then begin
        sProp := Pic.Props[FThumbCornerDetails[Corner].Prop];
        if sProp<>'' then begin
          Result := Info.Bitmap.TextWidth(sProp)+2;
          Info.Bitmap.Textout(
            rText,
            iif(Corner in [tcRightTop, tcRightBottom], DT_RIGHT, DT_LEFT) or DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_END_ELLIPSIS,
            sProp);
        end;
      end;
    end;

     // Отрисовывает на эскизе данные, находящиеся на одной гориз. линии
    procedure DrawDetailsLR(LeftCorner, RightCorner: TThumbCorner; rText: TRect);
    begin
       // Рисуем правое свойство
      Dec(rText.Right, DrawDetail(RightCorner, rText));
       // Рисуем левое свойство
      DrawDetail(LeftCorner, rText);
    end;

     // Отрисовывает эскиз в заданном прямоугольнике 
    procedure DrawThumbnail(const rThumb: TRect);
    var
      bCacheUsed: Boolean;
      bmpThumb: TBitmap;
      ix, iy: Integer;
    begin
       // Ищем изображение в кэше
      bmpThumb := GetCachedThumb(Pic);
      bCacheUsed := bmpThumb<>nil;
      try
         // Если не нашли - создаём временный битмэп и переносим на него JPEG-изображение эскиза
        if not bCacheUsed then begin
          bmpThumb := TBitmap.Create;
          Pic.PaintThumbnail(bmpThumb);
        end;
        ix := (rThumb.Left+rThumb.Right-bmpThumb.Width) div 2;
        iy := (rThumb.Top+rThumb.Bottom-bmpThumb.Height) div 2;
         // Рисуем эскиз
        BitBlt(
          Info.Bitmap.Canvas.Handle,
          Max(ix, rThumb.Left),
          Max(iy, rThumb.Top),
          Min(bmpThumb.Width, rThumb.Right-rThumb.Left),
          Min(bmpThumb.Height, rThumb.Bottom-rThumb.Top),
          bmpThumb.Canvas.Handle,
          0,
          0,
          SRCCOPY);
         // Кэшируем эскиз при необходимости
        if not bCacheUsed and FCacheThumbnails then begin
          PutThumbToCache(Pic, bmpThumb);
          bCacheUsed := True;
        end;
      finally
        if not bCacheUsed then bmpThumb.Free;
      end;
    end;

  begin
     // Получаем изображение
    Pic := FPicLinks[iIndex];
     // Рисуем рамку
    r := ItemRect;
    Info.Bitmap.Font.Assign(Self.Font);
    Info.Bitmap.Font.Color := iif(bSelected, aSelectedFontClr[Info.bFocused], FThumbFontColor);
     // Определяем внутренние границы эскиза
    rInner := Rect(ItemRect.Left+IThumbPaddingL, ItemRect.Top+IThumbPaddingT, ItemRect.Right-IThumbPaddingR, ItemRect.Bottom-IThumbPaddingB);
     // Отрисовываем изображение эскиза
    r := rInner; 
    case FDisplayMode of
      tvdmTile: begin
        if FThumbCornerDetails[tcLeftTop].bDisplay    or FThumbCornerDetails[tcRightTop].bDisplay    then Inc(r.Top,    FTextLineHeight);
        if FThumbCornerDetails[tcLeftBottom].bDisplay or FThumbCornerDetails[tcRightBottom].bDisplay then Dec(r.Bottom, FTextLineHeight);
      end;
      tvdmDetail: r.Right := r.Left+FThumbnailSize.cx;
    end;
    DrawThumbnail(r);
     // Рисуем описание
    case FDisplayMode of
      tvdmTile: begin
        r := rInner;
        r.Bottom := r.Top+FTextLineHeight;
        DrawDetailsLR(tcLeftTop,    tcRightTop,    r);
        r := rInner;
        r.Top := r.Bottom-FTextLineHeight;
        DrawDetailsLR(tcLeftBottom, tcRightBottom, r);
      end;
      tvdmDetail: {!!!};
    end;
  end;

  procedure TThumbnailViewer.Paint_Thumbnails(const Info: TThumbnailViewerPaintInfo);
  var
    rThumb: TRect;
    i, idxStart: Integer;
    bmpNormal, bmpSelected: TBitmap32; // Заготовки "подложек" эскизов обычного и выделенного
    bSelected: Boolean;

     // Отрисовывает FocusRect эскиза, если эскиз сфокусирован
    procedure DrawThumbFocusRect(iIndex: Integer; const r: TRect);
    begin
      if not Info.bFocused or (i<>FItemIndex) then Exit;
      with Info.Bitmap.Canvas do begin
        SetTextColor(Handle, $ffffff);
        SetBkColor  (Handle, $000000);
        DrawFocusRect(r);
      end;
    end;
    
     // Возвращает битмэп с "подложкой" эскиза: выделенного или обычного (в зависимости от bSelected); кэширует
     //   созданный битмэп
    function GetThBackBitmap: TBitmap32;
    const aSelBackClr: Array[Boolean] of TColor = (clBtnShadow,  clHighlight);
    var
      p: ^TBitmap32;
      r: TRect;
      iBorderWidth: Integer;
    begin
      p := iif(bSelected, @bmpSelected, @bmpNormal);
       // Если подложка не создана
      if p^=nil then begin
         // Создаём подложку
        p^ := TBitmap32.Create;
        r := Rect(0, 0, rThumb.Right-rThumb.Left, rThumb.Bottom-rThumb.Top);
        p^.SetSize(r.Right, r.Bottom);
         // Рисуем рамку
        case FThumbBackBorderStyle of
          tbbsFlat:   p^.FrameRectS(r, Color32(clBtnShadow));
          tbbsRaised: DrawEdge(p^.Canvas.Handle, r, BDR_RAISEDINNER, BF_RECT);
          tbbsSunken: DrawEdge(p^.Canvas.Handle, r, BDR_SUNKENINNER, BF_RECT);
          tbbsPlain:  DrawEdge(p^.Canvas.Handle, r, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT);
          tbbsGroove: DrawEdge(p^.Canvas.Handle, r, BDR_RAISEDINNER or BDR_SUNKENOUTER, BF_RECT);
          tbbsXP: begin
            p^.FrameRectS(r, Color32($b99d7f));
            p^.FrameRectS(r.Left+1, r.Top+1, r.Right-1, r.Bottom-1, Color32(clWindow));
          end;
          tbbsColor:  p^.FrameRectS(r, Color32(FThumbBackBorderColor));
        end;
         // Рисуем фон
        if FThumbBackBorderStyle<>tbbsNone then begin
          iBorderWidth := iif(FThumbBackBorderStyle in [tbbsPlain, tbbsGroove, tbbsXP], 2, 1);
          InflateRect(r, -iBorderWidth, -iBorderWidth);
        end;
        p^.FillRectS(r, Color32(iif(bSelected, aSelBackClr[Info.bFocused], FThumbBackColor)));
      end;
      Result := p^;
    end;

  begin
     // Отрисовываем эскизы
    idxStart := GetFirstVisibleIndex;
    if idxStart>=0 then begin
      bmpNormal   := nil;
      bmpSelected := nil;
      try
        for i := idxStart to FItemCount-1 do begin
           // Находим область эскиза
          rThumb := ItemRect(i, False);
          if IsRectEmpty(rThumb) then Break;
           // Рисуем FocusRect
          DrawThumbFocusRect(i, rThumb);
           // Если область эскиза пересекается с ClipRect - рисуем этот эскиз
          InflateRect(rThumb, -IThumbMarginH, -IThumbMarginV);
          if RectsOverlap(rThumb, Info.RClip) then begin
             // Определяем, выделен ли эскиз
            bSelected := FSelIndexes.IndexOf(i)>=0;
             // Рисуем "подложку"
            GetThBackBitmap.DrawTo(Info.Bitmap, rThumb.Left, rThumb.Top);
             // Рисуем эскиз
            Paint_Thumbnail(Info, i, rThumb, bSelected);
          end;
        end;
      finally
        bmpNormal.Free;
        bmpSelected.Free;
      end;
    end;
  end;

  procedure TThumbnailViewer.Paint_TransferBuffer(const Info: TThumbnailViewerPaintInfo);
  begin
    Info.Bitmap.DrawTo(Canvas.Handle, 0, 0);
  end;

  procedure TThumbnailViewer.PutThumbToCache(Pic: TPhoaPic; Bitmap: TBitmap);
  var p: PThumbCacheRec;
  begin
     // Добавляем в кэш
    New(p);
    p^.Pic   := Pic;
    p^.Thumb := Bitmap;
    FThumbCache.Insert(0, p);
     // Урезаем размер кэша
    LimitCacheSize(FThumbCacheSize);
  end;

  procedure TThumbnailViewer.RemoveFromSelection(Index: Integer);
  begin
    if FSelIndexes.Remove(Index)>=0 then InvalidateItem(Index);
  end;

  procedure TThumbnailViewer.Resize;
  begin
    inherited Resize;
    if FDisplayMode=tvdmDetail then InvalidateRect(Handle, nil, False);
  end;

  procedure TThumbnailViewer.RestoreDisplay(SelectedIDs: TIntegerList; iFocusedID, iTopOffset: Integer);
  var i, iItemIdx: Integer;
  begin
    ClearSelection;
     // Добавляем в выделение изображения с заданными ID
    if SelectedIDs<>nil then
      for i := 0 to SelectedIDs.Count-1 do AddToSelection(FPicLinks.IndexOfID(SelectedIDs[i]));
     // Если нет выделения, выделяем первое изображение (если оно есть)
    if FSelIndexes.Count=0 then AddToSelection(0);
     // Находим сфокусированное изображение
    if iFocusedID>0 then iItemIdx := FPicLinks.IndexOfID(iFocusedID) else iItemIdx := -1;
     // Если так и не нашлось
    if iItemIdx<0 then
       // Фокусируем последнее из выделенных, если они есть
      if FSelIndexes.Count>0 then iItemIdx := FSelIndexes[FSelIndexes.Count-1]
       // Иначе пытаемся выделить самое первое изображение, если оно есть
      else if FPicLinks.Count>0 then iItemIdx := 0;
    TopOffset := iTopOffset;
    MoveItemIndex(iItemIdx, True);
    DoSelectionChange;
  end;

  procedure TThumbnailViewer.SaveDisplay(out SelectedIDs: TIntegerList; out iFocusedID, iTopOffset: Integer);
  var i: Integer;
  begin
    iFocusedID := 0;
    if FSelIndexes.Count>0 then begin
      SelectedIDs := TIntegerList.Create(False);
      for i := 0 to FSelIndexes.Count-1 do SelectedIDs.Add(SelectedPics[i].ID);
      if FItemIndex>=0 then iFocusedID := FPicLinks[FItemIndex].ID;
    end else
      SelectedIDs := nil;
    iTopOffset := FTopOffset;
  end;

  procedure TThumbnailViewer.ScrollIntoView;
  var r: TRect;
  begin
     // Получаем границы эскиза
    r := ItemRect(FItemIndex, True);
    if not IsRectEmpty(r) then
       // Если сфокусированный эскиз выше окна просмотра
      if r.Top<0 then TopOffset := TopOffset+r.Top
       // Если сфокусированный эскиз ниже окна просмотра
      else if r.Bottom>ClientHeight then TopOffset := TopOffset+(r.Bottom-ClientHeight);
  end;

  procedure TThumbnailViewer.SelectAll;
  var i: Integer;
  begin
    if FSelIndexes.Count<FItemCount then begin
      ClearSelection;
      for i := 0 to FItemCount-1 do FSelIndexes.Add(i);
      Invalidate;
      DoSelectionChange;
    end;
  end;

  procedure TThumbnailViewer.SelectNone;
  begin
    if ClearSelection then DoSelectionChange;
  end;

  procedure TThumbnailViewer.SelectRange(idxStart, idxEnd: Integer; bRectangular: Boolean);
  var
    i, ix, iy: Integer;
    pStart, pEnd: TPoint;

     // Проверяет, что iMin<=iMax. Иначе меняет их значения местами
    procedure OrderCoord(var iMin, iMax: Integer);
    var i: Integer;
    begin
      if iMin>iMax then begin
        i    := iMax;
        iMax := iMin;
        iMin := i;      
      end;
    end;

  begin
    ClearSelection;
    MoveItemIndex(idxEnd, False);
     // Прямоугольное выделение
    if bRectangular then begin
      pStart := Point(idxStart mod FColCount, idxStart div FColCount);
      pEnd   := Point(idxEnd   mod FColCount, idxEnd   div FColCount);
      OrderCoord(pStart.x, pEnd.x);
      OrderCoord(pStart.y, pEnd.y);
      for iy := pStart.y to pEnd.y do
        for ix := pStart.x to pEnd.x do AddToSelection(iy*FColCount+ix);
     // Поточное выделение
    end else begin
      OrderCoord(idxStart, idxEnd);
      for i := idxStart to idxEnd do AddToSelection(i);
    end;
    DoSelectionChange;
  end;

  procedure TThumbnailViewer.SetBorderStyle(Value: TBorderStyle);
  begin
    if FBorderStyle<>Value then begin
      FBorderStyle := Value;
      RecreateWnd;
    end;  
  end;

  procedure TThumbnailViewer.SetCacheThumbnails(Value: Boolean);
  begin
    if FCacheThumbnails<>Value then begin
      FCacheThumbnails := Value;
      LimitCacheSize(FThumbCacheSize);
    end;
  end;

  procedure TThumbnailViewer.SetDisplayMode(Value: TThumbViewerDisplayMode);
  begin
    if FDisplayMode<>Value then begin
      FDisplayMode := Value;
      CalcLayout;
    end;
  end;

  procedure TThumbnailViewer.SetItemIndex(Value: Integer);
  begin
     // Если меняется индекс, или нет выделения, а оно нужно, или наоборот
    if (FItemIndex<>Value) or ((FSelIndexes.Count=0)<>(Value<0)) then begin
      ClearSelection;
      AddToSelection(Value);
      MoveItemIndex(Value, True);
      DoSelectionChange;
    end;
  end;

  procedure TThumbnailViewer.SetShowThumbTooltips(Value: Boolean);
  begin
    if FShowThumbTooltips<>Value then begin
      FShowThumbTooltips := Value;
      FLastTooltipIdx := -1;
      Application.CancelHint;
    end;
  end;

  procedure TThumbnailViewer.SetThumbBackBorderColor(Value: TColor);
  begin
    if FThumbBackBorderColor<>Value then begin
      FThumbBackBorderColor := Value;
      if (FThumbBackBorderStyle=tbbsColor) and (FItemCount>0) then Invalidate;
    end;
  end;

  procedure TThumbnailViewer.SetThumbBackBorderStyle(Value: TThumbBackBorderStyle);
  begin
    if FThumbBackBorderStyle<>Value then begin
      FThumbBackBorderStyle := Value;
      if FItemCount>0 then Invalidate;
    end;
  end;

  procedure TThumbnailViewer.SetThumbBackColor(Value: TColor);
  begin
    if FThumbBackColor<>Value then begin
      FThumbBackColor := Value;
      if FItemCount>0 then Invalidate;
    end;
  end;

  procedure TThumbnailViewer.SetThumbCacheSize(Value: Integer);
  begin
    if FThumbCacheSize<>Value then begin
      FThumbCacheSize := Value;
      LimitCacheSize(Value);
    end;
  end;

  procedure TThumbnailViewer.SetThumbCornerDetails(Corner: TThumbCorner; const Value: TThumbCornerDetail);
  begin
    FThumbCornerDetails[Corner] := Value;
    CalcLayout;
  end;

  procedure TThumbnailViewer.SetThumbFontColor(Value: TColor);
  begin
    if FThumbFontColor<>Value then begin
      FThumbFontColor := Value;
      if FItemCount>0 then Invalidate;
    end;
  end;

  procedure TThumbnailViewer.SetThumbnailSize(const Value: TSize);
  begin
    if (FThumbnailSize.cx<>Value.cx) or (FThumbnailSize.cy<>Value.cy) then begin
      FThumbnailSize := Value;
      LimitCacheSize(0);
      CalcLayout;
    end;
  end;

  procedure TThumbnailViewer.SetThumbTooltipProps(Value: TPicProperties);
  begin
    if FThumbTooltipProps<>Value then begin
      FThumbTooltipProps := Value;
      FLastTooltipIdx := -1;
      Application.CancelHint;
    end;
  end;

  procedure TThumbnailViewer.SetTopOffset(Value: Integer);
  begin
    Value := GetValidTopOffset(Value);
    if FTopOffset<>Value then begin
      ScrollWindowEx(Handle, 0, FTopOffset-Value, nil, nil, 0, nil, SW_INVALIDATE);
      FTopOffset := Value;
      UpdateScrollBar;
       // Invalidate the focused thumbnail for FocusRect to be repainted correctly
      if Focused then InvalidateItem(FItemIndex);
    end;
  end;

  procedure TThumbnailViewer.ToggleSelection(Index: Integer);
  begin
    if FSelIndexes.IndexOf(Index)>=0 then RemoveFromSelection(Index) else AddToSelection(Index);
  end;

  procedure TThumbnailViewer.UpdateScrollBar;
  var ScrollInfo: TScrollInfo;
  begin
    with ScrollInfo do begin
      cbSize := SizeOf(ScrollInfo);
      fMask  := SIF_ALL;
      nMin   := 0;
      nMax   := FVRange-1;
      nPage  := ClientHeight;
      nPos   := FTopOffset;
    end;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;

  procedure TThumbnailViewer.ViewGroup(PhoA: TPhotoAlbum; Group: TPhoaGroup; bRecurse: Boolean);
  var iItemIdx: Integer;
  begin
    BeginUpdate;
    try
       // Находим новый GroupID
      if Group=nil then FGroupID := 0 else FGroupID := Group.ID;
       // Копируем ссылки на изображения по их IDs из группы
      FPicLinks.AddFromGroup(PhoA, Group, True, bRecurse);
       // Стираем кэш эскизов
      LimitCacheSize(0);
       // Стираем выделение
      ClearSelection;
      FTopOffset := 0;
       // Выделяем первое изображение (если оно есть)
      AddToSelection(0);
      if FSelIndexes.Count>0 then iItemIdx := FSelIndexes[0] else iItemIdx := -1;
      MoveItemIndex(iItemIdx, True);
    finally
       // Пересчитываем layout, validate TopIndex, обновляем, уведомляем об изменении выделения
      EndUpdate;
    end;
  end;

  procedure TThumbnailViewer.WMContextMenu(var Msg: TWMContextMenu);
  begin
     // Вызываем context menu только если не был нажат Ctrl
    if not FShellCtxMenuOnMouseUp then inherited; 
  end;

  procedure TThumbnailViewer.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
  begin
    Msg.Result := 1;
  end;

  procedure TThumbnailViewer.WMGetDlgCode(var Msg: TWMGetDlgCode);
  begin
     // Direct arrow-key messages to the control
    Msg.Result := DLGC_WANTARROWS;
  end;

  procedure TThumbnailViewer.WMNCPaint(var Msg: TWMNCPaint);
  begin
    DefaultHandler(Msg);
    if ThemeServices.ThemesEnabled then ThemeServices.PaintBorder(Self, False);
  end;

  procedure TThumbnailViewer.WMVScroll(var Msg: TWMVScroll);
  var iOffset: Integer;

    function GetRealScrollPos: Integer;
    var SI: TScrollInfo;
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask  := SIF_TRACKPOS;
      GetScrollInfo(Handle, SB_VERT, SI);
      Result := SI.nTrackPos;
    end;

  begin
    case Msg.ScrollCode of
      SB_BOTTOM:       iOffset := MaxInt;
      SB_LINEDOWN:     iOffset := FTopOffset+10;
      SB_LINEUP:       iOffset := FTopOffset-10;
      SB_PAGEDOWN:     iOffset := FTopOffset+ClientHeight;
      SB_PAGEUP:       iOffset := FTopOffset-ClientHeight;
      SB_THUMBPOSITION,
        SB_THUMBTRACK: iOffset := GetRealScrollPos;
      SB_TOP:          iOffset := 0;
      else Exit;
    end;
    TopOffset := iOffset;
  end;

  procedure TThumbnailViewer.WMWindowPosChanged(var Msg: TWMWindowPosChanged);
  begin
    inherited;
    CalcLayout;
  end;

  procedure TThumbnailViewer.WndProc(var Msg: TMessage);
  var i: Integer;
  begin
    case Msg.Msg of
       // On (un)gaining focus repaint selection
      WM_KILLFOCUS, WM_SETFOCUS: begin
        for i := 0 to FSelIndexes.Count-1 do InvalidateItem(FSelIndexes[i]);
        if (FItemIndex>=0) and (FSelIndexes.IndexOf(FItemIndex)<0) then InvalidateItem(FItemIndex);
      end;
      WM_LBUTTONDBLCLK: begin
        DblClick;
        Exit;
      end;
      WM_MOUSEWHEEL: begin
        if TWMMouseWheel(Msg).WheelDelta>0 then TopOffset := TopOffset-FItemSize.cy else TopOffset := TopOffset+FItemSize.cy;
        Exit;
      end;
    end;
    inherited WndProc(Msg);
  end;

   //===================================================================================================================
   // TPhoAHintWindow
   //===================================================================================================================

  procedure TPhoAHintWindow.Paint;
  var
    r: TRect;
    s, sLine: String;
  begin
    r := ClientRect;
    InflateRect(r, -2, -2);
    r.Bottom := r.Top+Canvas.TextHeight('Wg');
    Canvas.Font.Color := Screen.HintFont.Color;
     // Цикл по строкам
    s := AdjustLineBreaks(Caption, tlbsLF);
    repeat
      sLine := ExtractFirstWord(s, #10);
      if sLine='' then Break;
       // Рисуем левую часть
      DrawText(Canvas.Handle, PChar(ExtractFirstWord(sLine, #9)), -1, r, DT_LEFT or DT_NOPREFIX or DT_END_ELLIPSIS);
       // Рисуем правую часть
      if sLine<>'' then DrawText(Canvas.Handle, PChar(sLine), -1, r, DT_RIGHT or DT_NOPREFIX or DT_END_ELLIPSIS);
      OffsetRect(r, 0, r.Bottom-r.Top);
    until False;
  end;

   //===================================================================================================================
   // TSizeGripper
   //===================================================================================================================

  constructor TSizeGripper.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    ControlStyle := ControlStyle-[csOpaque];
    Anchors := [akRight, akBottom];
    Cursor  := crSizeNWSE;
    Width   := GetSystemMetrics(SM_CXVSCROLL);
    Height  := GetSystemMetrics(SM_CYHSCROLL);
  end;

  procedure TSizeGripper.Paint;
  var
    r: TRect;
    i, iD, iSize: Integer;

    procedure DiagLine(c: TColor);
    begin
      Canvas.Pen.Color := c;
      MoveToEx(Canvas.Handle, r.Right-2-iD, r.Bottom-2, nil);
      LineTo(Canvas.Handle, r.Right-1, r.Bottom-iD-3);
      Inc(iD);
    end;

  begin
    r := ClientRect;
     // Если темы доступны, пользуемся ими
    if ThemeServices.ThemesEnabled then
      ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tsGripper), r)
     // Иначе рисуем сами (ripped from TBX)
    else begin
      iD := 0;
      iSize := Min(r.Right-r.Left, r.Bottom-r.Top);
      for i := 1 to 3 do
        case iSize of
          0..8: begin
            DiagLine(clBtnShadow);
            DiagLine(clBtnHighlight);
          end;
          9..11: begin
            DiagLine(clBtnFace);
            DiagLine(clBtnShadow);
            DiagLine(clBtnHighlight);
          end;
          12..14: begin
            DiagLine(clBtnShadow);
            DiagLine(clBtnShadow);
            DiagLine(clBtnHighlight);
          end;
          else begin
            DiagLine(clBtnFace);
            DiagLine(clBtnShadow);
            DiagLine(clBtnShadow);
            DiagLine(clBtnHighlight);
          end;
        end;
      Canvas.Pen.Color := clBtnFace;
      Canvas.MoveTo(r.Right-iD-1, r.Bottom-1);
      Canvas.LineTo(r.Right-1,    r.Bottom-1);
      Canvas.LineTo(r.Right-1,    r.Bottom-iD-2);
    end;
  end;

  procedure TSizeGripper.SetParent(AParent: TWinControl);
  begin
    inherited SetParent(AParent);
    if AParent<>nil then begin
       // Задвигаем в правый нижний угол родителя
      with AParent.ClientRect do SetBounds(Right-Width, Bottom-Height, Width, Height);
       // Падаем на дно
      SendToBack;
    end;
  end;

  procedure TSizeGripper.WMNCHitTest(var Msg: TWMNCHitTest);
  var p: TPoint;
  begin
    inherited;
    if Msg.Result=HTCLIENT then begin
      p := ScreenToClient(SmallPointToPoint(Msg.Pos));
      if PtInRect(ClientRect, p) then Msg.Result := HTBOTTOMRIGHT;
    end;
  end;

end.
 
