//**********************************************************************************************************************
//  $Id: phGUIObj.pas,v 1.29 2004-10-23 14:05:08 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phGUIObj;

interface
uses
  Windows, Messages, Types, SysUtils, Graphics, Classes, Controls, Forms,
  GR32,
  phIntf, phMutableIntf, phNativeIntf, phObj, phGraphics, ConsVars;

type

   //===================================================================================================================
   // TThumbnailViewer - средство просмотра эскизов изображений
   //===================================================================================================================

  TThumbnailViewerState = (
    tvsLayoutChangePending,     // Layout change pending
    tvsSelectionChangePending); // Selection change pending

  TThumbnailViewerStates = set of TThumbnailViewerState;

   // Информация о состоянии TThumbnailViewer, используемая его методами SaveDisplay() и RestoreDisplay()
  IThumbnailViewerDisplayData = interface(IInterface)
    ['{436126B8-5E50-4BAB-83D1-CA83FE19B976}']
     // Prop handlers
    function  GetFocusedID: Integer;
    function  GetSelectedIDCount: Integer;
    function  GetSelectedIDs(Index: Integer): Integer;
    function  GetTopOffset: Integer;
     // Props
     // -- ID сфокусированного изображения
    property FocusedID: Integer read GetFocusedID;
     // -- Количество ID выделенных изображений
    property SelectedIDCount: Integer read GetSelectedIDCount;
     // -- ID выделенных изображений по индексу
    property SelectedIDs[Index: Integer]: Integer read GetSelectedIDs;
     // -- Смещение верхнего края окна просмотра
    property TopOffset: Integer read GetTopOffset;
  end;

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
     // Интерфейс списка изображений, отображаемых в контроле
    FPicList: IPhoaPicList;
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
     // Количество строк эскизов, целиком отображающихся в окне контрола
    FVisibleRows: Integer;
     // Количество столбцов с эскизами
    FColCount: Integer;
     // Высота одной строки текста, нарисованного шрифтом контрола
    FTextLineHeight: Integer;
     // Флаг валидности параметров шрифта
    FFontParamsValid: Boolean;
     // Битмэп тени эскиза (nil, если нет валидной рассчитанной тени)
    FThumbShadow: TBitmap32;
     // Выделенные эскизы
    FSelectedPics: IPhotoAlbumPicList;
     // Индекс активного эскиза
    FItemIndex: Integer;
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
    FDragInsideEnabled: Boolean;
    FOnSelectionChange: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    FDragEnabled: Boolean;
    FShowThumbTooltips: Boolean;
    FThumbTooltipProps: TPicProperties;
    FThumbBackColor: TColor;
    FThumbFontColor: TColor;
    FOnStartViewMode: TNotifyEvent;
    FDisplayMode: TThumbViewerDisplayMode;
    FTopOffset: Integer;
    FThumbnailSize: TSize;
    FThumbBackBorderStyle: TThumbBackBorderStyle;
    FThumbBackBorderColor: TColor;
    FThumbShadowOpacity: Byte;
    FThumbShadowColor: TColor;
    FThumbShadowOffset: TPoint;
    FThumbShadowBlurRadius: Integer;
    FThumbShadowVisible: Boolean;
    FStates: TThumbnailViewerStates;


     // Painting stage handlers
    procedure Paint_EraseBackground(const Info: TThumbnailViewerPaintInfo);
    procedure Paint_Thumbnails(const Info: TThumbnailViewerPaintInfo);
    procedure Paint_Thumbnail(const Info: TThumbnailViewerPaintInfo; iIndex: Integer; ItemRect: TRect; bSelected: Boolean);
    procedure Paint_TransferBuffer(const Info: TThumbnailViewerPaintInfo);
     // Validates the specified offset and returns the correctly ranged offset value
    function  GetValidTopOffset(iOffset: Integer): Integer;
     // Рассчитывает параметры шрифта
    procedure CalcFontParams;
     // Уничтожает FShadowBitmap
    procedure ResetShadow;
     // Готовит FShadowBitmap
    procedure PrepareShadow;
     // Вызывает [отложенный] пересчёт параметров отображения
    procedure LayoutChanged;
     // Вызывает [отложенное] событие смены выделения
    procedure SelectionChanged;
     // Возвращает индекс самого верхнего отображаемого эскиза
    function  GetFirstVisibleIndex: Integer;


     // Invalidates all the selected thumbnails
    procedure InvalidateSelection;
     // Ставит или убирает выделение с эскиза
    procedure ToggleSelection(Index: Integer);
    function  AddToSelection(Index: Integer): Boolean;
    procedure RemoveFromSelection(Index: Integer);
     // Перемещает ItemIndex на новое место, не трогая выделения. При bUpdateStreamSelStart=True также обновляет
     //   FStreamSelStart. При bScrollIntoView=True также прокручивает эскиз в зону видимости
    procedure MoveItemIndex(iNewIndex: Integer; bUpdateStreamSelStart, bScrollIntoView: Boolean);
     // Вызывает OnStartViewMode
    procedure DoStartViewMode;
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
    procedure CMDrag(var Msg: TCMDrag);                         message CM_DRAG;
    procedure CMFontChanged(var Msg: TMessage);                 message CM_FONTCHANGED;
    procedure CMInvalidate(var Msg: TMessage);                  message CM_INVALIDATE;
    procedure CMMouseWheel(var Msg: TCMMouseWheel);             message CM_MOUSEWHEEL;
    procedure WMContextMenu(var Msg: TWMContextMenu);           message WM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd);             message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode);             message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TWMKillFocus);               message WM_KILLFOCUS;
    procedure WMNCPaint(var Msg: TWMNCPaint);                   message WM_NCPAINT;
    procedure WMSetFocus(var Msg: TWMSetFocus);                 message WM_SETFOCUS;
    procedure WMVScroll(var Msg: TWMVScroll);                   message WM_VSCROLL;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
     // Prop handlers
    function  GetDropTargetIndex: Integer;
    function  GetIDSelected(iID: Integer): Boolean;
    function  GetIndexSelected(iIndex: Integer): Boolean;
    function  GetSelectedIndexes(Index: Integer): Integer;
    function  GetThumbCornerDetails(Corner: TThumbCorner): TThumbCornerDetail;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetDisplayMode(Value: TThumbViewerDisplayMode);
    procedure SetItemIndex(Value: Integer);
    procedure SetShowThumbTooltips(Value: Boolean);
    procedure SetThumbBackBorderColor(Value: TColor);
    procedure SetThumbBackBorderStyle(Value: TThumbBackBorderStyle);
    procedure SetThumbBackColor(Value: TColor);
    procedure SetThumbCornerDetails(Corner: TThumbCorner; const Value: TThumbCornerDetail);
    procedure SetThumbFontColor(Value: TColor);
    procedure SetThumbnailSize(const Value: TSize);
    procedure SetThumbShadowBlurRadius(Value: Integer);
    procedure SetThumbShadowColor(Value: TColor);
    procedure SetThumbShadowOffset(const Value: TPoint);
    procedure SetThumbShadowOpacity(Value: Byte);
    procedure SetThumbShadowVisible(Value: Boolean);
    procedure SetThumbTooltipProps(Value: TPicProperties);
    procedure SetTopOffset(Value: Integer);
    function GetUpdateLocked: Boolean;
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
     // Обновляет отображаемый список изображений
    procedure ReloadPicList(APicList: IPhoaPicList);
     // Создаёт объект IThumbnailViewerDisplayData, сохраняет в него текущие параметры отображения и возвращает его
    function  SaveDisplay: IThumbnailViewerDisplayData;
     // Восстанавливает параметры отображения из Data; подразумевается возможность использования данных, полученных в
     //   результате предыдущего вызова SaveDisplay()
    procedure RestoreDisplay(Data: IThumbnailViewerDisplayData);
     // Убирает выделение со всех эскизов и возвращает True, если оно было
    function  ClearSelection: Boolean;
     // Выделяет все эскизы
    procedure SelectAll;
     // Возвращает ItemIndex в точке, или -1, если там нет эскиза
    function  ItemAtPos(ix, iy: Integer): Integer;
     // Возвращает координаты эскиза с индексом Index. Если bAllowInvisible=False, то возвращает пустой прямоугольник,
     //   если эскиз не пересекается с ClientRect
    function  ItemRect(Index: Integer; bAllowInvisible: Boolean): TRect;
     // Ставит эскиз в очередь на обновление
    procedure InvalidateItem(Index: Integer); overload;
    procedure InvalidateItem(Pic: IPhoaPic); overload;
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
     // -- True, если изображение с заданным ID выделено
    property IDSelected[iID: Integer]: Boolean read GetIDSelected;
     // -- True, если изображение с заданным индексом выделено
    property IndexSelected[iIndex: Integer]: Boolean read GetIndexSelected;
     // -- Индекс сфокусированного изображения
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
     // -- Индексы выделенных изображений в общем списке изображений viewer'а (Index - индекс выделенного изображения,
     //    0..SelCount-1)
    property SelectedIndexes[Index: Integer]: Integer read GetSelectedIndexes;
     // -- Выделенные изображения
    property SelectedPics: IPhotoAlbumPicList read FSelectedPics;
     // -- Если True, отображает всплывающие описания эскизов
    property ShowThumbTooltips: Boolean read FShowThumbTooltips write SetShowThumbTooltips;
     // -- Состояния контрола
    property States: TThumbnailViewerStates read FStates;
     // -- Цвет рамки фона эскиза при ThumbBackBorderStyle=tbbsColor
    property ThumbBackBorderColor: TColor read FThumbBackBorderColor write SetThumbBackBorderColor;
     // -- Стиль рамки фона эскиза
    property ThumbBackBorderStyle: TThumbBackBorderStyle read FThumbBackBorderStyle write SetThumbBackBorderStyle;
     // -- Цвет фона эскизов
    property ThumbBackColor: TColor read FThumbBackColor write SetThumbBackColor;
     // -- Цвет шрифта эскизов
    property ThumbFontColor: TColor read FThumbFontColor write SetThumbFontColor;
     // -- Данные, отображаемые на эскизах
    property ThumbCornerDetails[Corner: TThumbCorner]: TThumbCornerDetail read GetThumbCornerDetails write SetThumbCornerDetails;
     // -- Параметры тени эскиза
     // ---- Радиус размывки
    property ThumbShadowBlurRadius: Integer read FThumbShadowBlurRadius write SetThumbShadowBlurRadius;
     // ---- Цвет
    property ThumbShadowColor: TColor read FThumbShadowColor write SetThumbShadowColor;
     // ---- Смещение
    property ThumbShadowOffset: TPoint read FThumbShadowOffset write SetThumbShadowOffset;
     // ---- Непрозрачность
    property ThumbShadowOpacity: Byte read FThumbShadowOpacity write SetThumbShadowOpacity;
     // ---- "Видимость"
    property ThumbShadowVisible: Boolean read FThumbShadowVisible write SetThumbShadowVisible;
     // -- Данные, отображаемые на всплывающих описаниях эскизов
    property ThumbTooltipProps: TPicProperties read FThumbTooltipProps write SetThumbTooltipProps;
     // -- Размеры эскизов
    property ThumbnailSize: TSize read FThumbnailSize write SetThumbnailSize;
     // -- Смещение верхнего края окна относительно начала списка эскизов, в пикселах
    property TopOffset: Integer read FTopOffset write SetTopOffset;
     // -- True, если FUpdateLock>0
    property UpdateLocked: Boolean read GetUpdateLocked;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clBtnFace;
    property Constraints;
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
uses Math, Themes, phUtils;

type

   //===================================================================================================================
   // TThumbnailViewerDisplayData - реализация IThumbnailViewerDisplayData
   //===================================================================================================================

  TThumbnailViewerDisplayData = class(TInterfacedObject, IThumbnailViewerDisplayData)
  private
     // Список выделенных ID
    FSelIDs: TList;
     // Prop storage
    FFocusedID: Integer;
    FTopOffset: Integer; 
     // IThumbnailViewerDisplayData
    function  GetFocusedID: Integer;
    function  GetSelectedIDCount: Integer;
    function  GetSelectedIDs(Index: Integer): Integer;
    function  GetTopOffset: Integer;
  public
    constructor Create(SelectedPics: IPhoaPicList; iFocusedID, iTopOffset: Integer);
    destructor Destroy; override;
  end;

  constructor TThumbnailViewerDisplayData.Create(SelectedPics: IPhoaPicList; iFocusedID, iTopOffset: Integer);
  var i: Integer;
  begin
    inherited Create;
     // Переписываем ID выделенных изображений, если они есть
    if (SelectedPics<>nil) and (SelectedPics.Count>0) then begin
      FSelIDs := TList.Create;
      for i := 0 to SelectedPics.Count-1 do FSelIDs.Add(Pointer(SelectedPics[i].ID));
    end;
    FFocusedID := iFocusedID;
    FTopOffset := iTopOffset;
  end;

  destructor TThumbnailViewerDisplayData.Destroy;
  begin
    FSelIDs.Free;
    inherited Destroy;
  end;

  function TThumbnailViewerDisplayData.GetFocusedID: Integer;
  begin
    Result := FFocusedID;
  end;

  function TThumbnailViewerDisplayData.GetSelectedIDCount: Integer;
  begin
    if FSelIDs=nil then Result := 0 else Result := FSelIDs.Count;
  end;

  function TThumbnailViewerDisplayData.GetSelectedIDs(Index: Integer): Integer;
  begin
    Result := Integer(FSelIDs[Index]);
  end;

  function TThumbnailViewerDisplayData.GetTopOffset: Integer;
  begin
    Result := FTopOffset;
  end;

   //===================================================================================================================
   // TThumbnailViewer
   //===================================================================================================================

  function TThumbnailViewer.AddToSelection(Index: Integer): Boolean;
  begin
    if (Index>=0) and (Index<FItemCount) then begin
      FSelectedPics.Add(FPicList[Index], True, Result);
      if Result then begin
        InvalidateItem(Index);
        SelectionChanged;
      end;
    end else
      Result := False;
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
      if idx<0 then Hint := '' else Hint := GetPicPropStrs(FPicList[idx], FThumbTooltipProps, ':'#9, #13)+'|';
      FLastTooltipIdx := idx;
    end;
  end;

  procedure TThumbnailViewer.BeginUpdate;
  begin
//    if FUpdateLock=0 then Perform(WM_SETREDRAW, 0, 0);
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

  function TThumbnailViewer.ClearSelection: Boolean;
  begin
     // Если есть выделение
    Result := FSelectedPics.Count>0;
    if Result then begin
      InvalidateSelection;
      FSelectedPics.Clear;
      SelectionChanged;
    end;
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

  procedure TThumbnailViewer.CMMouseWheel(var Msg: TCMMouseWheel);
  var iAmount: Integer;
  begin
    inherited;
    if Msg.Result=0 then begin
      Msg.Result := 1;
       // Высота строки * количество щелчков прокрутки
      iAmount := FItemSize.cy*(Msg.WheelDelta div WHEEL_DELTA);
       // Если нажат Ctrl - скроллим сразу на видимое число строк
      if ssCtrl in Msg.ShiftState then iAmount := iAmount*FVisibleRows;
       // Скроллим
      TopOffset := TopOffset-iAmount;
    end;
  end;

  constructor TThumbnailViewer.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    ControlStyle           := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable];
    Width                  := 100;
    Height                 := 100;
    TabStop                := True;
    ParentColor            := False;
    Color                  := clBtnFace;
    FBorderStyle           := bsSingle;
    FColCount              := 1;
    FNoMoveItemIndex       := -1;
    FSelectedPics          := NewPhotoAlbumPicList(True);
    FThumbBackBorderColor  := clGray;
    FThumbBackBorderStyle  := tbbsXP;
    FThumbBackColor        := clBtnFace;
    FThumbFontColor        := clWindowText;
    FThumbnailSize.cx      := 100;
    FThumbnailSize.cy      := 100;
    FThumbShadowBlurRadius := 40;
    FThumbShadowColor      := clBlack;
    FThumbShadowOffset     := Point(7, 7);
    FThumbShadowOpacity    := 140;
    FThumbShadowVisible    := True;
  end;

  procedure TThumbnailViewer.CreateParams(var Params: TCreateParams);
  begin
    inherited CreateParams(Params);
    with Params do begin
      WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
      Style := Style or WS_VSCROLL;
      if FBorderStyle=bsSingle then begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end;
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
    FSelectedPics := nil;
    FPicList      := nil;
    FBuffer.Free;
    FThumbShadow.Free;
    FPicList := nil;
    inherited Destroy;
  end;

  procedure TThumbnailViewer.DoStartViewMode;
  begin
    if Assigned(FOnStartViewMode) then FOnStartViewMode(Self);
  end;

  procedure TThumbnailViewer.DragDrawInsertionPoint(var Coord: TViewerInsertionCoord; bInvalidate: Boolean);
  var
    dc: HDC;
    hp, hOld: HPEN;
    p: TPoint;
    r: TRect;
    bLower: Boolean;
  begin
    if Coord.iIndex>=0 then begin
       // Находим пиксельные координаты. Если bLower, то предыдущего эскиза, иначе текущего
      bLower := (Coord.iIndex mod FColCount=0) and Coord.bLower;
      r := ItemRect(Coord.iIndex-iif(bLower, 1, 0), False);
       // Если эскиз хоть сколько-нибудь виден
      if not IsRectEmpty(r) then begin
        p.x := iif(bLower, r.Right, r.Left);
        p.y := r.Top;
         // Отрисовываем
        dc := GetDCEx(Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
        hp := CreatePen(PS_SOLID, 3, ColorToRGB(Color) xor ColorToRGB(CInsertionPoint));
        hOld := SelectObject(dc, hp);
        SetROP2(dc, R2_XORPEN);
        MoveToEx(dc, p.x,   p.y, nil);
        LineTo  (dc, p.x,   p.y+FItemSize.cy);
        MoveToEx(dc, p.x-3, p.y, nil);
        LineTo  (dc, p.x+3, p.y);
        MoveToEx(dc, p.x-3, p.y+FItemSize.cy, nil);
        LineTo  (dc, p.x+3, p.y+FItemSize.cy);
        SelectObject(dc, hOld);
        DeleteObject(hp);
        ReleaseDC(Handle, dc);
      end;
       // Invalidate coord if needed
      if bInvalidate then begin
        Coord.iIndex := -1;
        Coord.bLower := False;
      end;
    end;
  end;

  procedure TThumbnailViewer.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  var
    iCol, iRow, idxSingleSel: Integer;
    bLower: Boolean;

     // Прокручивает список эскизов вверх (bUp=True) или вниз (bUp=False)
    procedure DoScroll(bUp: Boolean);
    begin
       // Стираем старое место вставки
      DragDrawInsertionPoint(FOldDragTargetCoord, True);
       // Прокручиваем
      TopOffset := TopOffset+iif(bUp, -20, 20);
      Update;
    end;

  begin
    if (Source=Self) and FDragInsideEnabled then begin
      FDragTargetCoord.iIndex := -1;
      FDragTargetCoord.bLower := False;
       // Если входим/находимся в контроле
      if PtInRect(ClientRect, Point(x, y)) then begin
         // Если у верхней или нижней границ окна, мотаем
        if (y<IDragScrollAreaMargin) or (y>=ClientHeight-IDragScrollAreaMargin) then DoScroll(y<IDragScrollAreaMargin);
         // Определяем место вставки
        if FItemCount>0 then begin
           // Находим столбец (с округлением)
          iCol := Trunc(x/FItemSize.cx+0.5);
           // Если справа от эскизов, включаем режим bLower и устанавливаем индекс на эскиз в начале следующей строки
          bLower := iCol>=FColCount;
          if bLower then iCol := FColCount;
           // Находим строку (индекс эскиза в начале строки)
          iRow := ((y+FTopOffset) div FItemSize.cy)*FColCount;
           // Если строка содержит эскизы
          if iRow<FItemCount then begin
            FDragTargetCoord.iIndex := iRow+iCol;
             // Если указывает за последний эскиз - надо включить bLower
            if FDragTargetCoord.iIndex>=FItemCount then begin
              FDragTargetCoord.iIndex := FItemCount;
              bLower := True;
            end;
            FDragTargetCoord.bLower := bLower;
          end;
        end;
      end;
       // Если изменилось положение маркера - отрисовываем его
      if (FDragTargetCoord.iIndex<>FOldDragTargetCoord.iIndex) or (FDragTargetCoord.bLower<>FOldDragTargetCoord.bLower) then begin
        DragDrawInsertionPoint(FOldDragTargetCoord, False);
        DragDrawInsertionPoint(FDragTargetCoord, False);
        FOldDragTargetCoord := FDragTargetCoord;
      end;
       // Drop возможен, если есть нормальная координата вставки, и перетаскиваются не все разом, и не единственное
       //   изображение или место вставки не совпадает с положением этого изображения
      Accept := (FDragTargetCoord.iIndex>=0) and (FSelectedPics.Count<FItemCount);
      if Accept and (FSelectedPics.Count=1) then begin
        idxSingleSel := SelectedIndexes[0];
        Accept := (idxSingleSel<>FDragTargetCoord.iIndex) and (idxSingleSel<>FDragTargetCoord.iIndex-1);
      end;
    end else
      inherited DragOver(Source, X, Y, State, Accept);
  end;

  procedure TThumbnailViewer.EndUpdate;
  begin
    if FUpdateLock>0 then Dec(FUpdateLock);
    if FUpdateLock=0 then begin
//      Perform(WM_SETREDRAW, 1, 0);
      if tvsLayoutChangePending    in FStates then LayoutChanged;
      if tvsSelectionChangePending in FStates then SelectionChanged;
    end;
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
  begin
    Result := FSelectedPics.IndexOfID(iID)>=0;
  end;

  function TThumbnailViewer.GetIndexSelected(iIndex: Integer): Boolean;
  begin
    Result := FSelectedPics.IndexOfID(FPicList[iIndex].ID)>=0;
  end;

  function TThumbnailViewer.GetSelectedIndexes(Index: Integer): Integer;
  begin
    Result := FPicList.IndexOfID(FSelectedPics[Index].ID);
  end;

  function TThumbnailViewer.GetThumbCornerDetails(Corner: TThumbCorner): TThumbCornerDetail;
  begin
    Result := FThumbCornerDetails[Corner];
  end;

  function TThumbnailViewer.GetUpdateLocked: Boolean;
  begin
    Result := FUpdateLock>0;
  end;

  function TThumbnailViewer.GetValidTopOffset(iOffset: Integer): Integer;
  begin
    Result := Max(0, Min(iOffset, FVRange-ClientHeight));
  end;

  procedure TThumbnailViewer.InvalidateItem(Index: Integer);
  var r: TRect;
  begin
    if (Index>=0) and (Index<FItemCount) and HandleAllocated then begin
      r := ItemRect(Index, False);
      if not IsRectEmpty(r) then InvalidateRect(Handle, @r, False);
    end;
  end;

  procedure TThumbnailViewer.InvalidateItem(Pic: IPhoaPic);
  begin
    InvalidateItem(FPicList.IndexOfID(Pic.ID));
  end;

  procedure TThumbnailViewer.InvalidateSelection;
  var i: Integer;
  begin
    for i := 0 to FSelectedPics.Count-1 do InvalidateItem(FSelectedPics[i]);
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
       // Пробел - включаем выделение; Ctrl+Пробел - переключаем выделение
      VK_SPACE:
        if Shift=[] then AddToSelection(FItemIndex)
        else if Shift=[ssCtrl] then ToggleSelection(FItemIndex);
       // Стрелки - обрабатываем движения
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT: begin
         // Без модификаторов или с Shift, но нет поточного выделения
        if (Shift=[]) or ((Shift=[ssShift]) and (FStreamSelStart<0)) then 
          ItemIndex := GetItemIndexForKey(True)
         // Нажат Shift - создаём поточное выделение, Shift+Alt - прямоугольное выделение
        else if (Shift=[ssShift]) or (Shift=[ssShift, ssAlt]) then
          SelectRange(FStreamSelStart, GetItemIndexForKey(not (ssAlt in Shift)), ssAlt in Shift)
         // Нажат Ctrl - двигаем только ItemIndex, не меняя выделения
        else if Shift=[ssCtrl] then
          MoveItemIndex(GetItemIndexForKey(True), True, True);
      end;
    end;
  end;

  procedure TThumbnailViewer.LayoutChanged;
  var iPrevColCount, iPrevTopOffset: Integer;
  begin
    if not HandleAllocated then Exit;
     // Если обновления заблокированы, просто взводим флаг изменения
    if FUpdateLock>0 then
      Include(FStates, tvsLayoutChangePending)
     // Иначе пересчитываем параметры отображения
    else begin
       // Сбрасываем Tooltip/Hint
      FLastTooltipIdx := -1;
      Hint := '';
       // Сохраняем старые параметры
      iPrevColCount  := FColCount;
      iPrevTopOffset := FTopOffset;
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
      FVisibleRows  := ClientHeight div FItemSize.cy;
      FVisibleItems := FVisibleRows*FColCount;
      FVRange       := Ceil(FItemCount/FColCount)*FItemSize.cy;
      FTopOffset    := GetValidTopOffset(FTopOffset);
       // Обновляем отображаемые данные при наличии изменений
      if (tvsLayoutChangePending in FStates) or (FColCount<>iPrevColCount) or (FTopOffset<>iPrevTopOffset) then Invalidate;
      UpdateScrollBar;
       // Обновляем флаги состояния
      FStates := FStates-[tvsLayoutChangePending];
    end;
  end;

  procedure TThumbnailViewer.MarqueingEnd;
  var
    i: Integer;
    r, rItem: TRect;
  begin
    ReleaseCapture;
    FMarqueing := False;
     // Стираем Marquee
    PaintMarquee;
    ReleaseDC(Handle, FTempDC);
     // Создаём выделение
    BeginUpdate;
    try
       // Если не был нажат Shift, очищаем Selection (выделяем с нуля)
      if GetKeyState(VK_SHIFT) and $80=0 then ClearSelection;
       // Select thumbnails that do intersect with marquee
      r := OrderRect(Rect(FStartPos, FMarqueeCur));
      for i := GetFirstVisibleIndex to FItemCount-1 do begin
        rItem := ItemRect(i, False);
        if IsRectEmpty(rItem) then Break;
        if RectsOverlap(r, rItem) then AddToSelection(i);
      end;
    finally
      EndUpdate;
    end;
  end;

  procedure TThumbnailViewer.MarqueingStart;
  begin
    SetCapture(Handle);
    FTempDC := GetDCEx(Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
    FMarqueing := True;
    FMarqueeCur := FStartPos;
  end;

  procedure TThumbnailViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var idx: Integer;
  begin
    SetFocus;
    if Button=mbMiddle then Exit;
    idx := ItemAtPos(x, y);
    FStartPos := Point(x, y);
     // Если нажат Alt[+Shift] (но не Ctrl+Alt) - начинаем выделение (marqueing)
    if Shift*[ssAlt, ssCtrl]=[ssAlt] then begin
      if Button=mbLeft then MarqueingStart;
     // Если нажат Ctrl
    end else if Shift*[ssShift, ssCtrl, ssAlt]=[ssCtrl] then begin
       // Если левая кнопка - переключаем выделение эскиза, на котором кликнули
      if Button=mbLeft then begin
        ToggleSelection(idx);
        MoveItemIndex(idx, True, True);
       // Если правая кнопка - готовим вызов Shell Context Menu
      end else if Button=mbRight then begin
        BeginUpdate;
        try
          ClearSelection;
          SetItemIndex(idx);
        finally
          EndUpdate;
        end;
        if idx>=0 then begin
          FShellCtxMenuOnMouseUp := True;
          Exit;
        end;
      end;
     // Если нажат Shift - выделяем подряд идущие эскизы
    end else if Shift*[ssShift, ssCtrl, ssAlt]=[ssShift] then
      if FStreamSelStart>=0 then SelectRange(FStreamSelStart, idx, False) else SetItemIndex(idx)
     // Не нажато кнопок
    else if Shift*[ssShift, ssCtrl, ssAlt]=[] then begin
      if (idx<0) or not IndexSelected[idx] then begin
        SetItemIndex(idx);
        FNoMoveItemIndex := -1;
      end else if Button=mbLeft then
        FNoMoveItemIndex := idx;
      if Button=mbLeft then
        if idx<0 then MarqueingStart
        else if FDragEnabled and (FSelectedPics.Count>0) then FDragPending := True;
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
      if (ssCtrl in Shift) and (FItemIndex>=0) and (FItemIndex=ItemAtPos(x, y)) then ShowFileShellContextMenu(FPicList[FItemIndex].FileName, Self);
      Exit;
     // Иначе завершаем Dragging
    end else begin
      FDragPending := False;
      if FNoMoveItemIndex>=0 then begin
        if not Dragging then ItemIndex := FNoMoveItemIndex;
        FNoMoveItemIndex := -1;
      end;
    end;
    inherited MouseUp(Button, Shift, x, y);
  end;

  procedure TThumbnailViewer.MoveItemIndex(iNewIndex: Integer; bUpdateStreamSelStart, bScrollIntoView: Boolean);
  begin
    if iNewIndex<>FItemIndex then begin
      InvalidateItem(FItemIndex);
      FItemIndex := iNewIndex;
      InvalidateItem(FItemIndex);
    end;
    if bUpdateStreamSelStart then FStreamSelStart := iNewIndex;
    if bScrollIntoView then ScrollIntoView;
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
    Pic: IPhoaPic;
    r, rInner: TRect;

     // Отрисовывает на эскизе данные одного угла. Возвращает ширину отрисованного текста
    function DrawDetail(Corner: TThumbCorner; rText: TRect): Integer;
    var sProp: String;
    begin
      Result := 0;
      if (FThumbCornerDetails[Corner].bDisplay) and (rText.Left<rText.Right) then begin
        sProp := Pic.PropStrValues[FThumbCornerDetails[Corner].Prop];
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
    var r: TRect;
    begin
       // Рисуем эскиз
      r := rThumb;
      PaintThumbnail(Pic, Info.Bitmap, r);
       // Рисуем тень
      if FThumbShadowVisible then
        DropShadow(Info.Bitmap, FThumbShadow, r, rThumb, FThumbShadowOffset.x, FThumbShadowOffset.y, FThumbShadowColor);
    end;

  begin
     // Получаем изображение
    Pic := FPicList[iIndex];
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
      tvdmDetail: ; //#TODO: Сделать отрисовку сведений в режиме Detail
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
    PrepareShadow;
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
            bSelected := IndexSelected[i];
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

  procedure TThumbnailViewer.PrepareShadow;
  begin
    if FThumbShadowVisible and (FThumbShadow=nil) then begin
      FThumbShadow := TBitmap32.Create;
      RenderShadowTemplate(FThumbShadow, FThumbShadowBlurRadius, FThumbShadowOpacity, FThumbShadowColor);
    end;
  end;

  procedure TThumbnailViewer.ReloadPicList(APicList: IPhoaPicList);
  var iItemIdx: Integer;
  begin
    BeginUpdate;
    try
       // Присваиваем список и взводим флаг изменённости параметров отображения
      FPicList   := APicList;
      FItemCount := FPicList.Count;
      FTopOffset := 0;
      LayoutChanged;
       // Стираем выделение
      ClearSelection;
       // Выделяем первое изображение (если оно есть)
      AddToSelection(0);
      if FSelectedPics.Count>0 then iItemIdx := 0 else iItemIdx := -1;
      MoveItemIndex(iItemIdx, True, False);
    finally
       // Всё пересчитываем и уведомляем об изменениях
      EndUpdate;
    end;
  end;

  procedure TThumbnailViewer.RemoveFromSelection(Index: Integer);
  begin
    if FSelectedPics.Remove(FPicList[Index].ID)>=0 then begin
      InvalidateItem(Index);
      SelectionChanged;
    end;
  end;

  procedure TThumbnailViewer.ResetShadow;
  begin
    if FThumbShadow<>nil then begin
      FreeAndNil(FThumbShadow);
      Invalidate;
    end;
  end;

  procedure TThumbnailViewer.Resize;
  begin
    inherited Resize;
    if FDisplayMode=tvdmDetail then InvalidateRect(Handle, nil, False);
  end;

  procedure TThumbnailViewer.RestoreDisplay(Data: IThumbnailViewerDisplayData);
  var i, iItemIdx: Integer;
  begin
    BeginUpdate;
    try
      ClearSelection;
       // Добавляем в выделение изображения с заданными ID
      for i := 0 to Data.SelectedIDCount-1 do AddToSelection(FPicList.IndexOfID(Data.SelectedIDs[i]));
       // Если нет выделения, выделяем первое изображение (если оно есть)
      if FSelectedPics.Count=0 then AddToSelection(0);
       // Находим сфокусированное изображение
      if Data.FocusedID>0 then iItemIdx := FPicList.IndexOfID(Data.FocusedID) else iItemIdx := -1;
       // Если так и не нашлось
      if iItemIdx<0 then
         // Фокусируем первое из выделенных, если они есть
        if FSelectedPics.Count>0 then iItemIdx := SelectedIndexes[0]
         // Иначе пытаемся выделить самое первое изображение, если оно есть
        else if FItemCount>0 then iItemIdx := 0;
      TopOffset := Data.TopOffset;
      MoveItemIndex(iItemIdx, True, False);
    finally
      EndUpdate;
    end;
  end;

  function TThumbnailViewer.SaveDisplay: IThumbnailViewerDisplayData;
  var iFocusedID: Integer;
  begin
    if FItemIndex>=0 then iFocusedID := FPicList[FItemIndex].ID else iFocusedID := 0;
    Result := TThumbnailViewerDisplayData.Create(FSelectedPics, iFocusedID, FTopOffset);
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
    if FSelectedPics.Count<FItemCount then begin
      BeginUpdate;
      try
        ClearSelection;
        for i := 0 to FItemCount-1 do AddToSelection(i);
      finally
        EndUpdate;
      end;
    end;
  end;

  procedure TThumbnailViewer.SelectionChanged;
  begin
    if not HandleAllocated then Exit;
     // Если обновления заблокированы, просто взводим флаг изменения
    if FUpdateLock>0 then
      Include(FStates, tvsSelectionChangePending)
     // Иначе вызываем событие смены выделения
    else begin
      if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
       // Обновляем флаги состояния
      FStates := FStates-[tvsSelectionChangePending];
    end;
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
    BeginUpdate;
    try
      ClearSelection;
      MoveItemIndex(idxEnd, False, True);
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
    finally
      EndUpdate;
    end;
  end;

  procedure TThumbnailViewer.SetBorderStyle(Value: TBorderStyle);
  begin
    if FBorderStyle<>Value then begin
      FBorderStyle := Value;
      RecreateWnd;
    end;  
  end;

  procedure TThumbnailViewer.SetDisplayMode(Value: TThumbViewerDisplayMode);
  begin
    if FDisplayMode<>Value then begin
      FDisplayMode := Value;
      LayoutChanged;
    end;
  end;

  procedure TThumbnailViewer.SetItemIndex(Value: Integer);
  begin
     // Если меняется индекс, или нет выделения, а оно нужно, или наоборот
    if (FItemIndex<>Value) or ((FSelectedPics.Count=0)<>(Value<0)) then begin
      BeginUpdate;
      try
        ClearSelection;
        AddToSelection(Value);
        MoveItemIndex(Value, True, True);
      finally
        EndUpdate;
      end;
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

  procedure TThumbnailViewer.SetThumbCornerDetails(Corner: TThumbCorner; const Value: TThumbCornerDetail);
  begin
    FThumbCornerDetails[Corner] := Value;
    LayoutChanged;
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
      LayoutChanged;
    end;
  end;

  procedure TThumbnailViewer.SetThumbShadowBlurRadius(Value: Integer);
  begin
    if FThumbShadowBlurRadius<>Value then begin
      FThumbShadowBlurRadius := Value;
      ResetShadow;
    end;
  end;

  procedure TThumbnailViewer.SetThumbShadowColor(Value: TColor);
  begin
    if FThumbShadowColor<>Value then begin
      FThumbShadowColor := Value;
      ResetShadow;
    end;
  end;

  procedure TThumbnailViewer.SetThumbShadowOffset(const Value: TPoint);
  begin
    if Int64(FThumbShadowOffset)<>Int64(Value) then begin
      FThumbShadowOffset := Value;
      ResetShadow;
    end;
  end;

  procedure TThumbnailViewer.SetThumbShadowOpacity(Value: Byte);
  begin
    if FThumbShadowOpacity<>Value then begin
      FThumbShadowOpacity := Value;
      ResetShadow;
    end;
  end;

  procedure TThumbnailViewer.SetThumbShadowVisible(Value: Boolean);
  begin
    if FThumbShadowVisible<>Value then begin
      FThumbShadowVisible := Value;
      ResetShadow;
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
    if Index>=0 then
      if IndexSelected[Index] then RemoveFromSelection(Index) else AddToSelection(Index);
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

  procedure TThumbnailViewer.WMContextMenu(var Msg: TWMContextMenu);
  begin
     // Вызываем context menu только если не был нажат Ctrl
    if not FShellCtxMenuOnMouseUp then begin
      if InvalidPoint(Msg.Pos) then Msg.Pos := PointToSmallPoint(ClientToScreen(CenterPoint(ItemRect(FItemIndex, False))));
      inherited;
    end;
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

  procedure TThumbnailViewer.WMKillFocus(var Msg: TWMKillFocus);
  begin
     // Перерисовываем выделение при потере фокуса
    InvalidateSelection;
     // Сфокусированный эскиз тоже надо перерисовать
    InvalidateItem(FItemIndex);
  end;

  procedure TThumbnailViewer.WMNCPaint(var Msg: TWMNCPaint);
  begin
    DefaultHandler(Msg);
    if ThemeServices.ThemesEnabled then ThemeServices.PaintBorder(Self, False);
  end;

  procedure TThumbnailViewer.WMSetFocus(var Msg: TWMSetFocus);
  begin
     // Перерисовываем выделение при установке фокуса
    InvalidateSelection;
     // Сфокусированный эскиз тоже надо перерисовать
    InvalidateItem(FItemIndex);
  end;

  procedure TThumbnailViewer.WMVScroll(var Msg: TWMVScroll);
  var iOffset: Integer;

     // Получает 32-битную позицию скроллера (в сообщении передаётся только 16-битная)
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
    LayoutChanged;
  end;

  procedure TThumbnailViewer.WndProc(var Msg: TMessage);
  begin
    case Msg.Msg of
       // Перехватываем DoubleClick, чтобы по нему не начинался AutoDrag 
      WM_LBUTTONDBLCLK: begin
        DblClick;
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

