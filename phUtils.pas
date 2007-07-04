//**********************************************************************************************************************
//  $Id: phUtils.pas,v 1.62 2007-07-04 18:48:39 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phUtils;

interface
uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, TntForms,
  TntWindows, TntSysUtils, TntClasses, TntStdCtrls, TB2Item, TBX, VirtualTrees, VirtualShellUtilities,
  phIntf, phMutableIntf, phNativeIntf, phAppIntf, phObj, ConsVars;

   // Exception raising
  procedure PhoaException(const wsMsg: WideString); overload;
  procedure PhoaException(const wsMsg: WideString; const aParams: Array of const); overload;
   // The same but takes message's constant name
  procedure PhoaExceptionConst(const sConstName: AnsiString); overload;
  procedure PhoaExceptionConst(const sConstName: AnsiString; const aParams: Array of const); overload;
   // Tries to obtain an Unicode exception message, or Ansi message if failed
  function  GetWideExceptionMessage(E: Exception): WideString;

   // Min/max values
  function  Max(i1, i2: Integer): Integer;
  function  Min(i1, i2: Integer): Integer;
  function  MinS(s1, s2: Single): Single;
  function  MaxS(s1, s2: Single): Single;

   // Преобразует Variant в Integer (при этом Null конвертируется в 0)
  function  VarToInt(const v: Variant): Integer;

  procedure RegLoadHistory(const sSection: AnsiString; ComboBox: TTntComboBox; bSetLastItem: Boolean);
  procedure RegSaveHistory(const sSection: AnsiString; ComboBox: TTntComboBox; bRegisterFirst: Boolean);
  procedure RegisterCBHistory(ComboBox: TTntComboBox);
   // Возвращает Integer(Items.Objects[ItemIndex]) для TTntComboBox, или -1, если ItemIndex<0
  function  GetCurrentCBObject(ComboBox: TTntComboBox): Integer;
   // То же, для установки ComboBox по Objects[]. Возвращает True, если удалось
  function  SetCurrentCBObject(ComboBox: TTntComboBox; iObj: Integer): Boolean;

   // Изготовление типа TSize
  function  MakeSize(cx, cy: Integer): TSize;

   // Преобразование TRect<->Строка вида '1,2,3,4'
  function  RectToStr(const r: TRect): WideString;
  function  StrToRect(const ws: WideString; const rDefault: TRect): TRect;
   // "Упорядочивает" координаты в r так, что TopLeft всегда левее и выше, чем BottomRight
  function  OrderRect(const r: TRect): TRect;
   // Корректирует и возвращает прямоугольник r так, чтобы он вписался в rBounds, по возможности сохранив размеры
  function  FitRect(const r, rBounds: TRect): TRect;
   // Возвращает True, если прямоугольники пересекаются
  function  RectsOverlap(const r1, r2: TRect): Boolean;
   // Возвращает результат пересечения прямоугольников
  function  GetRectIntersection(const r1, r2: TRect): TRect;

   // Преобразование Положение/размеры формы<->Строка
  function  FormPositionToStr(Form: TTntForm): WideString;
  procedure FormPositionFromStr(Form: TTntForm; const wsPosition: WideString);
   // Возвращает объект-монитор, задаваемый свойством DefaultMonitor формы. Если подходящего монитора не найдено,
   //   возвращает первичный монитор
  function  GetDefaultMonitorForForm(DefMonitor: TDefaultMonitor): TMonitor;

   // Работа с описанием шрифта в виде "Name/Size/Style/Color/Charset"
  function  FontToStr(Font: TFont): WideString;
  procedure FontFromStr(Font: TFont; const wsFont: WideString);

   // Возвращает символ в верхнем регистре
  function  WideUpCase(wc: WideChar): WideChar;
   // Возвращает True, если wsText начинается на wsSubText (без учёта регистра) 
  function  WideStartsText(const wsSubText, wsText: WideString): Boolean;
   // Возвращает True, если wsText оканчивается на wsSubText (без учёта регистра)
  function  WideEndsText(const wsSubText, wsText: WideString): Boolean;
   // Возвращает True, если wsSubText содержится в wsText (без учёта регистра)
  function  WideContainsText(const wsSubText, wsText: WideString): Boolean;
   // Возвращает позицию первого вхождения символа wc в строке ws; 0, если символ в строке не содержится 
  function  CharPos(wc: WideChar; const ws: WideString): Integer;
   // Заменяет вхождения символов wsReplaceChars в строке ws на символ wcReplaceWith и возвращает результат
  function  ReplaceChars(const ws, wsReplaceChars: WideString; wcReplaceWith: WideChar): WideString;
   // Возвращает первое слово из строки ws, считая за разделители слов любой из символов в wsDelimiters. Если
   //   разделителя в строке нет, возвращается вся строка
  function  GetFirstWord(const ws, wsDelimiters: WideString): WideString;
   // Извлекает и возвращает первое слово из строки ws, считая за разделители слов любой из символов в wsDelimiters. Из
   //   строки ws слово вместе с разделителем удаляется. Если разделителя в строке нет, возвращается вся строка
  function  ExtractFirstWord(var ws: WideString; const wsDelimiters: WideString): WideString;
   // Добавляет к строке ws строку wsAdd. Если обе они не пустые, между ними вставляется wsSeparator
  procedure AccumulateStr(var ws: WideString; const wsSeparator, wsAdd: WideString);
   // Функция сравнения строк с конца *без учёта регистра* (для имён файлов работает быстрее сравнения с начала)
  function  ReverseCompare(const ws1, ws2: WideString): Boolean;
   // Преобразует относительный путь к файлу в абсолютный, используя базовый каталог wsBasePath
  function  ExpandRelativePath(const wsBasePath, wsRelFileName: WideString): WideString;
   // Возвращает строку, представляющую собой наиболее длинную совпадающую часть обоих строк. Сравнение ведётся без
   //   учёта регистра
  function  LongestCommonPart(const ws1, ws2: WideString): WideString;
   // Возвращает путь к каталогу временных файлов Windows
  function  GetWindowsTempPath: WideString;

  function  iif(b: Boolean; const sTrue, sFalse: AnsiString): AnsiString;   overload;
  function  iif(b: Boolean; const wsTrue, wsFalse: WideString): WideString; overload;
  function  iif(b: Boolean; iTrue, iFalse: Integer): Integer;               overload;
  function  iif(b: Boolean; pTrue, pFalse: Pointer): Pointer;               overload;
  function  iif(b: Boolean; sgTrue, sgFalse: Single): Single;               overload;

  procedure Swap(var A, B: WideString); overload;
  procedure Swap(var A, B: Integer);    overload;
  procedure Swap(var A, B: Pointer);    overload;
  procedure Swap(var A, B: Single);     overload;

   // Преобразует размер файла в удобочитаемую форму
  function  HumanReadableSize(i64Size: Int64): WideString; 

   // Проверяет ввод текста, представляющего из себя дату (bTime=False) или время (bTime=True). В случае удачи результат
   //   помещает в dtResult и возвращает True, иначе отображает сообщение об ошибке и возвращает False. Если wsText -
   //   пустая маска, то в dtResult возвращается -1
  function  CheckMaskedDateTime(const wsText: WideString; bTime: Boolean; var dtResult: TDateTime): Boolean;
   // Заменяет разделитель времени в строке и возвращает её. При bToSystem=False заменяет системный разделитель на
   //   AppFormatSettings.TimeSeparator, при bToSystem=True заменяет AppFormatSettings.TimeSeparator на системный
   //   разделитель времени
  function  ChangeTimeSeparator(const wsTime: WideString; bToSystem: Boolean): WideString;

   // Возвращает sText, если он начинается на символ, отличный от '@'. Иначе - трактует текст после '@' как имя
   //   константы и возвращает её значение
  function  ConstValEx(const wsText: WideString): WideString;

   // Преобразование TPicProperties <-> Integer
  function  PicPropsToInt(PicProps: TPicProperties): Integer;
  function  IntToPicProps(i: Integer): TPicProperties;
   // Возвращает наименование свойства изображения
  function  PicPropName(PicProp: TPicProperty): WideString;
   // Преобразование TGroupProperties <-> Integer
  function  GroupPropsToInt(GroupProps: TGroupProperties): Integer;
  function  IntToGroupProps(i: Integer): TGroupProperties;
   // Возвращает наименование свойства группы
  function  GroupPropName(GroupProp: TGroupProperty): WideString;
   // Возвращает наименование свойства изображения для группировки
  function  GroupByPropName(GBProp: TPicGroupByProperty): WideString;
   // Возвращает наименование пиксельного формата изображения
  function  PixelFormatName(PFmt: TPhoaPixelFormat): WideString;
   // Возвращает наименование единицы измерения размера файла
  function  FileSizeUnitName(FSUnit: TFileSizeUnit): WideString;
   // Возвращает наименование свойства дискового файла
  function  DiskFilePropName(DFProp: TDiskFileProp): WideString;
   // Возвращает значение свойства дискового файла по объекту TNamespace файла. Если Namespace=nil, возвращает пустую строку
  function  DiskFilePropValue(DFProp: TDiskFileProp; Namespace: TNamespace): WideString;
   // Возвращает наименование свойства для автозаполнения даты/времени изображения
  function  DateTimeAutofillPropName(DTAProp: TDateTimeAutofillProp): WideString;
   // Возвращает текст результата автозаполнения даты/времени изображения
  function  DateTimeFillResultName(DTFResult: TDateTimeFillResult): WideString;

   // Разрешает или запрещает контрол; если это TWinControl, то он, перекрашивается в clWindow или clBtnFace
   //   соответственно
  procedure EnableControl(bEnable: Boolean; Ctl: TControl);
   // То же для массива контролов. В качестве элемента Ctls[] можно передавать nil
  procedure EnableControls(bEnable: Boolean; const Ctls: Array of TControl);

   // Возвращает размер указанного файла, или iDefault, если такого файла не существует
  function  GetFileSize(const wsFileName: WideString; iDefault: Integer): Integer;

   // Добавляет к Menu новый пункт
  function  AddTBXMenuItem(Menu: TTBCustomItem; const wsCaption: WideString; iImageIndex, iTag: Integer; const AOnClick: TNotifyEvent): TTBCustomItem;

   // Фокусирует и выделяет узел. При bScrollIntoView=True проматывает дерево так, чтобы он был виден. Возвращает True,
   //   если Node<>nil
  function  ActivateVTNode(Tree: TBaseVirtualTree; Node: PVirtualNode; bScrollIntoView: Boolean = True): Boolean;
   // Фокусирует первый узел в дереве (если он есть) и устанавливает положение скроллбаров в левый верхний узел
  procedure ActivateFirstVTNode(Tree: TBaseVirtualTree);
   // Возвращает узел корневого каталога дерева по заданному индексу. Если нет такого, возвращает nil
  function  GetVTRootNodeByIndex(Tree: TBaseVirtualTree; iIndex: Integer): PVirtualNode;
   // Сохранение/загрузка настроек столбцов VirtualTree
  procedure RegSaveVTColumns(const sSection: AnsiString; Tree: TVirtualStringTree);
  procedure RegLoadVTColumns(const sSection: AnsiString; Tree: TVirtualStringTree);
   // Устанавливает опцию coVisible столбца согласно bVisible
  procedure SetVTColumnVisible(Column: TVirtualTreeColumn; bVisible: Boolean);
   // Возвращает VirtualTrees.TVTHintMode, соответствующиq заданному TGroupTreeHintMode
  function  GTreeHintModeToVTHintMode(GTHM: TGroupTreeHintMode): TVTHintMode;

   // Возвращает вид узла виртуального дерева, отображающего группы изображений. bViewGroups должен быть True, если
   //   дерево отображает иерархию групп представления; False, если иерархию групп проекта
  function  PicGroupsVT_GetNodeKind(Tree: TBaseVirtualTree; Node: PVirtualNode; bViewGroups: Boolean): TGroupNodeKind;
   // Возвращает группу изображений, соответствующую узлу виртуального дерева, отображающего группы изображений
  function  PicGroupsVT_GetNodeGroup(Tree: TBaseVirtualTree; Node: PVirtualNode): IPhotoAlbumPicGroup;
   // Обработчики событий виртуальных деревьев, отображающих группы изображений. NB: нет поддержки для editable-деревьев
   // -- OnBeforeCellPaint
   //      App         - приложение
   //      bViewGroups - True, если дерево отображает иерархию групп представления; False, если иерархию групп проекта
  procedure PicGroupsVT_HandleBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; App: IPhotoAlbumApp; bViewGroups: Boolean);
   // -- OnBeforeItemErase
   //      bViewGroups - True, если дерево отображает иерархию групп представления; False, если иерархию групп проекта
  procedure PicGroupsVT_HandleBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction; bViewGroups: Boolean);
   // -- OnCollapsing
  procedure PicGroupsVT_HandleCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
   // -- OnExpanded/OnCollapsed
   //      bStoreExpanded - если True, то "развёрнутость" узла сохраняется в свойство Expanded соответствующей ему
   //                       группы
  procedure PicGroupsVT_HandleExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode; bStoreExpanded: Boolean);
   // -- OnFreeNode
  procedure PicGroupsVT_HandleFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
   // -- OnGetHint
   //      App         - приложение
   //      bViewGroups - True, если дерево отображает иерархию групп представления; False, если иерархию групп проекта
  procedure PicGroupsVT_HandleGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString; App: IPhotoAlbumApp; bViewGroups: Boolean);
   // -- OnGetImageIndex
   //      bViewGroups - True, если дерево отображает иерархию групп представления; False, если иерархию групп проекта
  procedure PicGroupsVT_HandleGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; bViewGroups: Boolean);
   // -- OnGetText
   //      View - отображаемое в дереве представление; nil, если дерево отображает иерархию групп проекта
  procedure PicGroupsVT_HandleGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString; View: IPhoaView);
   // -- OnInitNode
   //      RootGroup          - корневая группа, отображаемая в дереве в настоящий момент (проекта или представления)
   //      SearchResultsGroup - группа результатов поиска; nil, если неприменимо
   //      bRootButton        - если True, у корневого узла выставляется CheckType=ctButton
   //      bInitExpanded      - если True, "развёрнутость" узла инициализируется значением свойства Expanded
   //                           соответствующей узлу группы для некорневых узлов (корневые узлы ВСЕГДА разворачиваются)
  procedure PicGroupsVT_HandleInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates; RootGroup, SearchResultsGroup: IPhotoAlbumPicGroup; bRootButton, bInitExpanded: Boolean);
   // -- OnPaintText
  procedure PicGroupsVT_HandlePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

   // Укорачивает строку как имя файла
  function  ShortenFileName(Canvas: TCanvas; iWidth: Integer; const ws: WideString): WideString;

   // Отображает системное контекстное меню для заданного файла. Возвращает True, если удалось
  function  ShowFileShellContextMenu(const wsFileName: WideString): Boolean;

   // Показ/скрытие курсора HourGlass
  procedure StartWait;
  procedure StopWait;

implementation
uses
  TypInfo, Variants, ShellAPI,
  TntWideStrUtils, GR32,
  DKLang,
  phSettings, phMsgBox, phPhoa, phGraphics;

type
   //===================================================================================================================
   // TShellContextMenuManager - класс для обработки сообщений Shell Context Menu
   //===================================================================================================================

  TShellContextMenuManager = class(TCustomForm)
  private
     // Объект TNamespace, для которого отображаются меню. Существует только в течение вызова ShowMenu()
    FNamespace: TNamespace;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create; reintroduce;
     // Отображает контекстное меню для файла. Возвращает True, если удачно
    function  ShowMenu(const wsFileName: WideString): Boolean;
  end;

  constructor TShellContextMenuManager.Create;
  begin
    CreateNew(Application);
  end;

  function TShellContextMenuManager.ShowMenu(const wsFileName: WideString): Boolean;
  begin
    Result := False;
    try
      try
        FNamespace := TNamespace.CreateFromFileName(wsFileName);
      except
        PhoaError('SErrFileNotFoundFmt', [wsFileName]);
      end;
      if FNamespace<>nil then Result := FNamespace.ShowContextMenu(Self, nil, nil, nil);
    finally
      FreeAndNil(FNamespace);
    end;
  end;

  procedure TShellContextMenuManager.WndProc(var Message: TMessage);
  begin
    inherited WndProc(Message);
    case Message.Msg of
       // Перенаправляем сообщения меню в Namespace
      WM_MEASUREITEM, WM_DRAWITEM, WM_INITMENUPOPUP, WM_MENUCHAR:
        if FNamespace<>nil then FNamespace.HandleContextMenuMsg(Message.Msg, Message.WParam, Message.LParam, Message.Result);
    end;
  end;

var
   // Экземпляр TShellContextMenuManager для отображения контекстных меню. Создаётся при первом обращении
  NS_MenuManager: TShellContextMenuManager = nil;

   //===================================================================================================================

  {$W+}
  procedure PhoaException(const wsMsg: WideString);

     function RetAddr: Pointer;
     asm
       mov eax, [ebp+4]
     end;

  begin
    raise EPhoaWideException.Create(wsMsg) at RetAddr;
  end;
  {$W-}

  {$W+}
  procedure PhoaException(const wsMsg: WideString; const aParams: Array of const);

     function RetAddr: Pointer;
     asm
       mov eax, [ebp+4]
     end;

  begin
    raise EPhoaWideException.CreateFmt(wsMsg, aParams) at RetAddr;
  end;
  {$W-}

  procedure PhoaExceptionConst(const sConstName: AnsiString);
  begin
    PhoaException(DKLangConstW(sConstName));
  end;

  procedure PhoaExceptionConst(const sConstName: AnsiString; const aParams: Array of const);
  begin
    PhoaException(DKLangConstW(sConstName, aParams));
  end;

  function GetWideExceptionMessage(E: Exception): WideString;
  begin
    if E is EPhoaWideException then Result := EPhoaWideException(E).WideMessage else Result := E.Message;
  end;

  function Max(i1, i2: Integer): Integer;
  begin
    if i1>i2 then Result := i1 else Result := i2;
  end;

  function Min(i1, i2: Integer): Integer;
  begin
    if i1<i2 then Result := i1 else Result := i2;
  end;

  function MinS(s1, s2: Single): Single;
  begin
    if s1<s2 then Result := s1 else Result := s2;
  end;

  function MaxS(s1, s2: Single): Single;
  begin
    if s1>s2 then Result := s1 else Result := s2;
  end;

  function VarToInt(const v: Variant): Integer;
  begin
    if VarIsNull(v) then Result := 0 else Result := v;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // History
   //-------------------------------------------------------------------------------------------------------------------

  procedure RegLoadHistory(const sSection: AnsiString; ComboBox: TTntComboBox; bSetLastItem: Boolean);
  var
    SL: TTntStringList;
    i: Integer;
  begin
    SL := TTntStringList.Create;
    try
      with TPhoaRegIniFile.Create(SRegRoot) do
        try
          ReadSectionValues(sSection, SL);
        finally
          Free;
        end;
      ComboBox.Clear;
      for i := 0 to SL.Count-1 do ComboBox.Items.Add(SL.ValueFromIndex[i]);
    finally
      SL.Free;
    end;
    if bSetLastItem and (ComboBox.Items.Count>0) then ComboBox.ItemIndex := 0;
  end;

  procedure RegSaveHistory(const sSection: AnsiString; ComboBox: TTntComboBox; bRegisterFirst: Boolean);
  var i: Integer;
  begin
    if bRegisterFirst then RegisterCBHistory(ComboBox);
    with TPhoaRegIniFile.Create(SRegRoot) do
      try
        EraseSection(sSection);
        for i := 0 to ComboBox.Items.Count-1 do WriteString(sSection, 'Item'+IntToStr(i), ComboBox.Items[i]);
      finally
        Free;
      end;
  end;

  procedure RegisterCBHistory(ComboBox: TTntComboBox);
  var
    idx: Integer;
    ws: WideString;
  begin
    ws := Trim(ComboBox.Text);
    if ws<>'' then begin
      idx := ComboBox.Items.IndexOf(ws);
      if idx>=0 then ComboBox.Items.Delete(idx);
      ComboBox.Items.Insert(0, ws);
      while ComboBox.Items.Count>IMaxHistoryEntries do ComboBox.Items.Delete(IMaxHistoryEntries);
    end;
    ComboBox.Text := ws;
  end;

  function GetCurrentCBObject(ComboBox: TTntComboBox): Integer;
  var idx: Integer;
  begin
    idx := ComboBox.ItemIndex;
    if idx<0 then Result := -1 else Result := Integer(ComboBox.Items.Objects[idx]);
  end;

  function SetCurrentCBObject(ComboBox: TTntComboBox; iObj: Integer): Boolean;
  var idx: Integer;
  begin
    idx := ComboBox.Items.IndexOfObject(Pointer(iObj));
    ComboBox.ItemIndex := idx;
    Result := idx>=0;
  end;

  function MakeSize(cx, cy: Integer): TSize;
  begin
    Result.cx := cx;
    Result.cy := cy;
  end;

  function RectToStr(const r: TRect): WideString;
  begin
    Result := WideFormat('%d,%d,%d,%d', [r.Left, r.Top, r.Right, r.Bottom]);
  end;

  function  StrToRect(const ws: WideString; const rDefault: TRect): TRect;
  var wss: WideString;
  begin
    wss := ws;
    try
      Result.Left   := StrToInt(ExtractFirstWord(wss, ','));
      Result.Top    := StrToInt(ExtractFirstWord(wss, ','));
      Result.Right  := StrToInt(ExtractFirstWord(wss, ','));
      Result.Bottom := StrToInt(ExtractFirstWord(wss, ','));
    except
      on EConvertError do Result := rDefault;
    end;
  end;

  function OrderRect(const r: TRect): TRect;
  begin
    if r.Left<r.Right then begin
      Result.Left   := r.Left;
      Result.Right  := r.Right;
    end else begin
      Result.Left   := r.Right;
      Result.Right  := r.Left;
    end;
    if r.Top<r.Bottom then begin
      Result.Top    := r.Top;
      Result.Bottom := r.Bottom;
    end else begin
      Result.Top    := r.Bottom;
      Result.Bottom := r.Top;
    end;
  end;

  function FitRect(const r, rBounds: TRect): TRect;
  var idx, idy: Integer;
  begin
     // Подстраиваем ширину
    Result := Rect(
      r.Left,
      r.Top,
      Min(r.Right,  r.Left+(rBounds.Right-rBounds.Left)),
      Min(r.Bottom, r.Top+(rBounds.Bottom-rBounds.Top)));
     // Подстраиваем координаты
    if Result.Left<rBounds.Left        then idx := rBounds.Left-Result.Left
    else if Result.Right>rBounds.Right then idx := rBounds.Right-Result.Right
    else                                    idx := 0;
    if Result.Top<rBounds.Top            then idy := rBounds.Top-Result.Top
    else if Result.Bottom>rBounds.Bottom then idy := rBounds.Bottom-Result.Bottom
    else                                      idy := 0;
    OffsetRect(Result, idx, idy);
  end;

  function RectsOverlap(const r1, r2: TRect): Boolean;
  begin
    Result := (r1.Right>r2.Left) and (r1.Bottom>r2.Top) and (r2.Right>r1.Left) and (r2.Bottom>r1.Top);
  end;

  function GetRectIntersection(const r1, r2: TRect): TRect;
  begin
    IntersectRect(Result, r1, r2);
  end;
  
  function FormPositionToStr(Form: TTntForm): WideString;
  var Placement: TWindowPlacement;
  begin
    Placement.length := SizeOf(Placement);
    GetWindowPlacement(Form.Handle, @Placement);
    Result := WideFormat(
      '%d,%d,%d,%d,%d',
      [Placement.rcNormalPosition.Left, Placement.rcNormalPosition.Top, Placement.rcNormalPosition.Right,
       Placement.rcNormalPosition.Bottom, iif(Placement.showCmd=SW_SHOWMAXIMIZED, 1, 0)]);
  end;

  procedure FormPositionFromStr(Form: TTntForm; const wsPosition: WideString);
  var
    r, rWorkArea: TRect;
    iw, ih: Integer;
    ws: WideString;
    bMaximized: Boolean;
    Monitor: TMonitor;

    function ConstrDef(iConstraint, iDefault: Integer): Integer;
    begin
      Result := iif(iConstraint=0, iDefault, iConstraint);
    end;

  begin
    ws := wsPosition;
    r.Left     := StrToIntDef(ExtractFirstWord(ws, ','), MaxInt);
    r.Top      := StrToIntDef(ExtractFirstWord(ws, ','), MaxInt);
    r.Right    := StrToIntDef(ExtractFirstWord(ws, ','), MaxInt);
    r.Bottom   := StrToIntDef(ExtractFirstWord(ws, ','), MaxInt);
    bMaximized := StrToIntDef(ExtractFirstWord(ws, ','), 0)<>0;
     // Если все координаты нормальные
    if (r.Right<MaxInt) and (r.Bottom<MaxInt) and (r.Left<r.Right) and (r.Top<r.Bottom) then begin
       // Находим монитор, наиболее подходящий по координатам
      Monitor := Screen.MonitorFromRect(r, mdNearest);
      rWorkArea := Monitor.WorkAreaRect;
       // Исправляем размер при необходимости
      iw := Min(
        Min(Max(r.Right-r.Left, ConstrDef(Form.Constraints.MinWidth, 10)), rWorkArea.Right-rWorkArea.Left),
        ConstrDef(Form.Constraints.MaxWidth, MaxInt));
      ih := Min(
        Min(Max(r.Bottom-r.Top, ConstrDef(Form.Constraints.MinHeight, 10)), rWorkArea.Bottom-rWorkArea.Top),
        ConstrDef(Form.Constraints.MaxHeight, MaxInt));
       // Позиционируем форму, при необходимости исправляя положение
      Form.BoundsRect := Bounds(
        Max(rWorkArea.Left, Min(rWorkArea.Right-iw,  r.Left)),
        Max(rWorkArea.Top,  Min(rWorkArea.Bottom-ih, r.Top)),
        iw,
        ih);
       // Восстанавливаем состояние
      if bMaximized then Form.WindowState := wsMaximized;
     // Иначе центрируем форму на экране
    end else begin
      rWorkArea := GetDefaultMonitorForForm(Form.DefaultMonitor).WorkareaRect;
      Form.SetBounds(
        (rWorkArea.Left+rWorkArea.Right -Form.Width) div 2,
        (rWorkArea.Top +rWorkArea.Bottom-Form.Height) div 2,
        Form.Width,
        Form.Height);
    end;
  end;

  function GetDefaultMonitorForForm(DefMonitor: TDefaultMonitor): TMonitor;
  begin
    Result := nil;
    case DefMonitor of
      dmMainForm:   if Application.MainForm<>nil    then Result := Application.MainForm.Monitor;
      dmActiveForm: if Screen.ActiveCustomForm<>nil then Result := Screen.ActiveCustomForm.Monitor;
    end;
    if Result=nil then Result := Screen.Monitors[0];
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Fonts / chars
   //-------------------------------------------------------------------------------------------------------------------

  function FontToStr(Font: TFont): WideString;
  begin
    Result := WideFormat('%s/%d/%d/%d/%d', [Font.Name, Font.Size, Byte(Font.Style), Font.Color, Font.Charset]);
  end;

  procedure FontFromStr(Font: TFont; const wsFont: WideString);
  var ws: WideString;
  begin
    ws := wsFont;
    Font.Name    := ExtractFirstWord(ws, '/');
    Font.Size    := StrToIntDef(ExtractFirstWord(ws, '/'), 10);
    Font.Style   := TFontStyles(Byte(StrToIntDef(ExtractFirstWord(ws, '/'), 0)));
    Font.Color   := StrToIntDef(ExtractFirstWord(ws, '/'), 0);
    Font.Charset := StrToIntDef(ExtractFirstWord(ws, '/'), DEFAULT_CHARSET);
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Misc
   //-------------------------------------------------------------------------------------------------------------------

  function WideUpCase(wc: WideChar): WideChar;
  begin
    Result := WideUpperCase(wc)[1];
  end;

  function WideStartsText(const wsSubText, wsText: WideString): Boolean;
  var iLenSubText, iLenText: Integer;
  begin
    iLenSubText := Length(wsSubText);
    iLenText    := Length(wsText);
    Result := (iLenText>=iLenSubText) and WideSameText(wsSubText, Copy(wsText, 1, iLenSubText));
  end;

  function WideEndsText(const wsSubText, wsText: WideString): Boolean;
  var iLenSubText, iLenText: Integer;
  begin
    iLenSubText := Length(wsSubText);
    iLenText    := Length(wsText);
    Result := (iLenText>=iLenSubText) and WideSameText(wsSubText, Copy(wsText, iLenText-iLenSubText+1, iLenSubText));
  end;

  function WideContainsText(const wsSubText, wsText: WideString): Boolean;
  begin
    Result := Pos(WideUpperCase(wsSubText), WideUpperCase(wsText))>0;
  end;

  function CharPos(wc: WideChar; const ws: WideString): Integer;
  begin
    for Result := 1 to Length(ws) do
      if ws[Result]=wc then Exit;
    Result := 0;
  end;

  function ReplaceChars(const ws, wsReplaceChars: WideString; wcReplaceWith: WideChar): WideString;
  var i: Integer;
  begin
    Result := ws;
    for i := 1 to Length(Result) do
      if CharPos(Result[i], wsReplaceChars)>0 then Result[i] := wcReplaceWith;
  end;

  function GetFirstWord(const ws, wsDelimiters: WideString): WideString;
  var i: Integer;
  begin
    i := 1;                                                  
    while (i<=Length(ws)) and (CharPos(ws[i], wsDelimiters)=0) do Inc(i);
    Result := Copy(ws, 1, i-1);
  end;

  function ExtractFirstWord(var ws: WideString; const wsDelimiters: WideString): WideString;
  begin
    Result := GetFirstWord(ws, wsDelimiters);
    Delete(ws, 1, Length(Result)+1);
  end;

  procedure AccumulateStr(var ws: WideString; const wsSeparator, wsAdd: WideString);
  begin
    if (ws<>'') and (wsAdd<>'') then ws := ws+wsSeparator;
    ws := ws+wsAdd;
  end;

  function ReverseCompare(const ws1, ws2: WideString): Boolean;
  var
    wsUp1, wsUp2: WideString;
    i, iLength: Integer;
  begin
    Result := False;
    wsUp1 := WideUpperCase(ws1);
    wsUp2 := WideUpperCase(ws2);
    iLength := Length(wsUp1);
    if iLength<>Length(wsUp2) then Exit;
    for i := iLength downto 1 do
      if wsUp1[i]<>wsUp2[i] then begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;

  function ExpandRelativePath(const wsBasePath, wsRelFileName: WideString): WideString;
  var
    i: Integer;
    wsOneDir, wsRelName: WideString;
  begin
    if wsRelFileName='' then
      Result := ''
    else begin
      wsRelName := wsRelFileName;
       // Если путь содержит в себе диск (абсолютный путь)
      if (Pos(':', wsRelName)>0) or (Pos('\\', wsRelName)>0) then
        Result := wsRelName
       // Иначе - относительный путь
      else begin
         // Если начинается на '\' - идём с корня
        Result := wsBasePath;
        if (Result<>'') and (Result[Length(Result)]='\') then Delete(Result, Length(Result), 1);
        if wsRelName[1]='\' then
          Result := Copy(Result, 1, 3)+Copy(wsRelName, 2, MaxInt)
         // Иначе - с каталога файла
        else begin
          repeat
            i := Pos('\', wsRelName);
            if i=0 then Break;
            wsOneDir := Copy(wsRelName, 1, i);
            Delete(wsRelName, 1, i);
            if wsOneDir='..\' then begin
              i := LastDelimiter('\', Result);
              if i=0 then PhoaExceptionConst('SErrInvalidPicFileName', [wsRelFileName]);
              Delete(Result, i, MaxInt);
            end else
              Result := Result+'\'+Copy(wsOneDir, 1, Length(wsOneDir)-1);
          until False;
          Result := Result+'\'+wsRelName;
        end;
      end;
    end;
  end;

  function LongestCommonPart(const ws1, ws2: WideString): WideString;
  var
    wsUp1, wsUp2: WideString;
    i: Integer;
  begin
    wsUp1 := WideUpperCase(ws1);
    wsUp2 := WideUpperCase(ws2);
    Result := '';
    i := 1;
    while (i<=Length(wsUp1)) and (i<=Length(wsUp2)) and (wsUp1[i]=wsUp2[i]) do begin
      Result := Result+ws1[i];
      Inc(i);
    end;
  end;

  function GetWindowsTempPath: WideString;
  var awcBuf: Array[0..MAX_PATH] of WideChar;
  begin
    Tnt_GetTempPathW(MAX_PATH, awcBuf);
    Result := WideIncludeTrailingPathDelimiter(awcBuf);
  end;

  function iif(b: Boolean; const sTrue, sFalse: AnsiString): AnsiString;
  begin
    if b then Result := sTrue else Result := sFalse;
  end;

  function iif(b: Boolean; const wsTrue, wsFalse: WideString): WideString;
  begin
    if b then Result := wsTrue else Result := wsFalse;
  end;

  function iif(b: Boolean; iTrue, iFalse: Integer): Integer;
  begin
    if b then Result := iTrue else Result := iFalse;
  end;

  function iif(b: Boolean; pTrue, pFalse: Pointer): Pointer;
  begin
    if b then Result := pTrue else Result := pFalse;
  end;

  function iif(b: Boolean; sgTrue, sgFalse: Single): Single;
  begin
    if b then Result := sgTrue else Result := sgFalse;
  end;

  procedure Swap(var A, B: WideString);
  var C: WideString;
  begin
    C := A;
    A := B;
    B := C;
  end;

  procedure Swap(var A, B: Integer);
  var C: Integer;
  begin
    C := A;
    A := B;
    B := C;
  end;

  procedure Swap(var A, B: Pointer);
  var C: Pointer;
  begin
    C := A;
    A := B;
    B := C;
  end;

  procedure Swap(var A, B: Single);
  var C: Single;
  begin
    C := A;
    A := B;
    B := C;
  end;

  function HumanReadableSize(i64Size: Int64): WideString;
  var fsu: TFileSizeUnit;
  begin
     // Определяем требуемую единицу измерения размера
    case i64Size of
      0..1023:                     fsu := fsuBytes;
      1024..1024*1024-1:           fsu := fsuKBytes;
      1024*1024..1024*1024*1024-1: fsu := fsuMBytes;
      else                         fsu := fsuGBytes;
    end;
     // Форматируем размер
    Result := WideFormat(
      iif(fsu=fsuBytes, '%.0f %s', '%.2f %s'),
      [i64Size/aFileSizeUnitMultipliers[fsu], FileSizeUnitName(fsu)]);
  end;

  function CheckMaskedDateTime(const wsText: WideString; bTime: Boolean; var dtResult: TDateTime): Boolean;
  begin
    Result := True;
     // Если не введено ни одной цифры - значит, ввода не было
    if WideLastDelimiter('0123456789', wsText)=0 then
      dtResult := -1
    else begin
      if bTime then
        Result := TryStrToTime(ChangeTimeSeparator(wsText, False), dtResult, AppFormatSettings)
      else
        Result := TryStrToDate(wsText, dtResult, AppFormatSettings);
      if not Result then PhoaError(iif(bTime, 'SNotAValidTime', 'SNotAValidDate'), [wsText]);
    end;
  end;

  function ChangeTimeSeparator(const wsTime: WideString; bToSystem: Boolean): WideString;
  var
    i: Integer;
    cFrom, cTo: Char;
  begin
    Result := wsTime;
     // Определяем, что на что будем заменять
    if bToSystem then begin
      cFrom := AppFormatSettings.TimeSeparator;
      cTo   := TimeSeparator;
    end else begin
      cFrom := TimeSeparator;
      cTo   := AppFormatSettings.TimeSeparator;
    end;
     // Проверяем все символы строки и заменям
    for i := 1 to Length(Result) do
      if Result[i]=WideChar(cFrom) then Result[i] := WideChar(cTo);
  end;

  function ConstValEx(const wsText: WideString): WideString;
  begin
    Result := wsText;
     // Если наименование начинается на '@' - это константа
    if (Result<>'') and (Result[1]='@') then Result := DKLangConstW(Copy(Result, 2, MaxInt));
  end;

  function PicPropsToInt(PicProps: TPicProperties): Integer;
  begin
    Result := 0;
    Move(PicProps, Result, SizeOf(PicProps));
  end;

  function IntToPicProps(i: Integer): TPicProperties;
  begin
    Move(i, Result, SizeOf(Result));
  end;

  function PicPropName(PicProp: TPicProperty): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TPicProperty), Byte(PicProp)));
  end;

  function GroupPropsToInt(GroupProps: TGroupProperties): Integer;
  begin
    Result := 0;
    Move(GroupProps, Result, SizeOf(GroupProps));
  end;

  function IntToGroupProps(i: Integer): TGroupProperties;
  begin
    Move(i, Result, SizeOf(Result));
  end;

  function GroupPropName(GroupProp: TGroupProperty): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TGroupProperty), Byte(GroupProp)));
  end;

  function GroupByPropName(GBProp: TPicGroupByProperty): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TPicGroupByProperty), Byte(GBProp)));
  end;

  function PixelFormatName(PFmt: TPhoaPixelFormat): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TPhoaPixelFormat), Byte(PFmt)));
  end;

  function FileSizeUnitName(FSUnit: TFileSizeUnit): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TFileSizeUnit), Byte(FSUnit)));
  end;

  function DiskFilePropName(DFProp: TDiskFileProp): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TDiskFileProp), Byte(DFProp)));
  end;

  function DiskFilePropValue(DFProp: TDiskFileProp; Namespace: TNamespace): WideString;
  const awsBoolPropVals: Array[Boolean] of WideString = (' ', '•');
  begin
    Result := '';
    if Namespace<>nil then
      with Namespace do
        case DFProp of
          dfpFileName:            Result := FileName;
          dfpFileType:            Result := FileType;
          dfpSizeOfFile:          Result := WideFormat('%s (%s)', [SizeOfFile, SizeOfFileKB]);
          dfpSizeOfFileDiskUsage: Result := SizeOfFileDiskUsage;
          dfpCreationTime:        Result := CreationTime;
          dfpLastWriteTime:       Result := LastWriteTime;
          dfpLastAccessTime:      Result := LastAccessTime;
          dfpReadOnlyFile:        Result := awsBoolPropVals[ReadOnlyFile];
          dfpHidden:              Result := awsBoolPropVals[Hidden];
          dfpArchive:             Result := awsBoolPropVals[Archive];
          dfpCompressed:          Result := awsBoolPropVals[Compressed];
          dfpSystemFile:          Result := awsBoolPropVals[SystemFile];
          dfpTemporary:           Result := awsBoolPropVals[Temporary];
        end;
  end;

  function DateTimeAutofillPropName(DTAProp: TDateTimeAutofillProp): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TDateTimeAutofillProp), Byte(DTAProp)));
  end;

  function DateTimeFillResultName(DTFResult: TDateTimeFillResult): WideString;
  begin
    Result := DKLangConstW(GetEnumName(TypeInfo(TDateTimeFillResult), Byte(DTFResult)));
  end;

  procedure EnableControl(bEnable: Boolean; Ctl: TControl);
  var
    pi: PPropInfo;
    WCtl: TWinControl absolute Ctl;
  begin
    if Ctl is TWinControl then begin
      if not (csDestroying in WCtl.ComponentState) then WCtl.HandleNeeded; // Workaround ComboBoxEx's repaint bug
      WCtl.Enabled := bEnable;
      pi := GetPropInfo(WCtl, 'Color', [tkInteger]);
      if pi<>nil then SetOrdProp(WCtl, pi, iif(bEnable, clWindow, clBtnFace));
    end else
      Ctl.Enabled := bEnable;
  end;

  procedure EnableControls(bEnable: Boolean; const Ctls: Array of TControl);
  var
    i: Integer;
    c: TControl;
  begin
    for i := 0 to High(Ctls) do begin
      c := Ctls[i];
      if c<>nil then EnableControl(bEnable, c);
    end;
  end;

  function GetFileSize(const wsFileName: WideString; iDefault: Integer): Integer;
  var
    hF: THandle;
    FindData: TWin32FindDataW;
  begin
    hF := Tnt_FindFirstFileW(PWideChar(wsFileName), FindData);
    if hF<>INVALID_HANDLE_VALUE then begin
      Windows.FindClose(hF);
      Result := FindData.nFileSizeLow;
    end else
      Result := iDefault;
  end;

  function  AddTBXMenuItem(Menu: TTBCustomItem; const wsCaption: WideString; iImageIndex, iTag: Integer; const AOnClick: TNotifyEvent): TTBCustomItem;
  begin
    {!!! Not Unicode-enabled solution }
    Result := TTBXItem.Create(Menu.Owner);
    Result.Caption    := wsCaption;
    Result.ImageIndex := iImageIndex;
    Result.Tag        := iTag;
    Result.OnClick    := AOnClick;
    Menu.Add(Result);
  end;

  function ActivateVTNode(Tree: TBaseVirtualTree; Node: PVirtualNode; bScrollIntoView: Boolean = True): Boolean;
  begin
    Result := Node<>nil;
    Tree.BeginUpdate;
    try
      Tree.ClearSelection;
      Tree.FocusedNode := Node;
      if Result then begin
        Tree.Selected[Node] := True;
        if bScrollIntoView then Tree.ScrollIntoView(Node, False, False);
      end;
    finally
      Tree.EndUpdate;
    end;
  end;

  procedure ActivateFirstVTNode(Tree: TBaseVirtualTree);
  begin
    Tree.BeginUpdate;
    try
      ActivateVTNode(Tree, Tree.GetFirst, False);
      Tree.OffsetXY := Point(0, 0);
    finally
      Tree.EndUpdate;
    end;
  end;

  function GetVTRootNodeByIndex(Tree: TBaseVirtualTree; iIndex: Integer): PVirtualNode;
  begin
    if iIndex<0 then
      Result := nil
    else begin
      Result := Tree.GetFirst;
      while (Result<>nil) and (Integer(Result.Index)<iIndex) do Result := Tree.GetNextSibling(Result);
    end;
  end;
  
  procedure RegSaveVTColumns(const sSection: AnsiString; Tree: TVirtualStringTree);
  var
    i: Integer;
    c: TVirtualTreeColumn;
  begin
    with TPhoaRegIniFile.Create(SRegRoot) do
      try
        for i := 0 to Tree.Header.Columns.Count-1 do begin
          c := Tree.Header.Columns[i];
          WriteString(sSection, 'Column'+IntToStr(i), WideFormat('%d,%d,%d', [c.Position, c.Width, Byte(coVisible in c.Options)]));
        end;
      finally
        Free;
      end;
  end;

type
  TVTColumnsCast = class(TVirtualTreeColumns);

  procedure RegLoadVTColumns(const sSection: AnsiString; Tree: TVirtualStringTree);
  var
    i: Integer;
    c: TVirtualTreeColumn;
    ws: WideString;
  begin
    with TPhoaRegIniFile.Create(SRegRoot) do
      try
        for i := 0 to Tree.Header.Columns.Count-1 do begin
          c := Tree.Header.Columns[i];
          ws := ReadString(sSection, 'Column'+IntToStr(i), '');
          c.Position := StrToIntDef(ExtractFirstWord(ws, ','), c.Position);
          c.Width    := StrToIntDef(ExtractFirstWord(ws, ','), c.Width);
          SetVTColumnVisible(c, StrToIntDef(ExtractFirstWord(ws, ','), Byte(coVisible in c.Options))<>0);
        end;
    finally
      Free;
    end;
  end;

  procedure SetVTColumnVisible(Column: TVirtualTreeColumn; bVisible: Boolean);
  begin
    with Column do
      if bVisible then Options := Options+[coVisible] else Options := Options-[coVisible];
  end;

  function GTreeHintModeToVTHintMode(GTHM: TGroupTreeHintMode): TVTHintMode;
  const
    aHM: Array[TGroupTreeHintMode] of TVTHintMode = (
      hmDefault, // gthmNone
      hmTooltip, // gthmTips
      hmHint);   // gthmInfo
  begin
    Result := aHM[GTHM];
  end;

  function PicGroupsVT_GetNodeKind(Tree: TBaseVirtualTree; Node: PVirtualNode; bViewGroups: Boolean): TGroupNodeKind;
  var Group: IPhotoAlbumPicGroup;
  begin
    Result := gnkNone;
    if Node<>nil then begin
      Group := PicGroupsVT_GetNodeGroup(Tree, Node);
      if Group.Owner=nil then begin
        if Group.ID=IGroupID_SearchResults then Result := gnkSearch
        else if bViewGroups                then Result := gnkView
        else                                    Result := gnkProject;
      end else
        if bViewGroups then Result := gnkViewGroup else Result := gnkPhoaGroup;
    end;
  end;

  function PicGroupsVT_GetNodeGroup(Tree: TBaseVirtualTree; Node: PVirtualNode): IPhotoAlbumPicGroup;
  begin
    if Node=nil then Result := nil else Result := PPhotoAlbumPicGroup(Tree.GetNodeData(Node))^;
  end;

  procedure PicGroupsVT_HandleBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; App: IPhotoAlbumApp; bViewGroups: Boolean);
  var p: TPoint;
  begin
    if PicGroupsVT_GetNodeKind(Sender, Node, bViewGroups) in [gnkPhoaGroup, gnkViewGroup] then begin
      p := CellRect.TopLeft;
      Inc(p.x, Sender.GetNodeLevel(Node)*(Sender as TVirtualStringTree).Indent+4);
      if Node.CheckType<>ctNone then Inc(p.x, 18);
      PaintGroupIcon(
        PicGroupsVT_GetNodeGroup(Sender, Node).IconData,
        TargetCanvas.Handle,
        p,
        Color32(TVirtualStringTree(Sender).Color),
        vsSelected in Node.States,
        App);
    end;
  end;

  procedure PicGroupsVT_HandleBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction; bViewGroups: Boolean);
  begin
    if PicGroupsVT_GetNodeKind(Sender, Node, bViewGroups) in [gnkProject, gnkView] then begin
      ItemColor   := clBtnFace;
      EraseAction := eaColor;
    end;
  end;

  procedure PicGroupsVT_HandleCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  begin
     // Нельзя свёртывать корневые узлы (фотоальбома, результатов поиска etc)
    Allowed := Sender.NodeParent[Node]<>nil;
  end;

  procedure PicGroupsVT_HandleExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode; bStoreExpanded: Boolean);
  var Group: IPhotoAlbumPicGroup;
  begin
    if not (tsUpdating in Sender.TreeStates) and bStoreExpanded then begin
      Group := PicGroupsVT_GetNodeGroup(Sender, Node);
      if (Group<>nil) then Group.Expanded := vsExpanded in Node.States;
    end;
  end;

  procedure PicGroupsVT_HandleFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PPhotoAlbumPicGroup(Sender.GetNodeData(Node))^ := nil;
  end;

  procedure PicGroupsVT_HandleGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString; App: IPhotoAlbumApp; bViewGroups: Boolean);
  begin
    LineBreakStyle := hlbForceMultiLine;
    case PicGroupsVT_GetNodeKind(Sender, Node, bViewGroups) of
      gnkProject:   HintText := App.Project.Description;
      gnkPhoaGroup: HintText := GetPicGroupPropStrs(PicGroupsVT_GetNodeGroup(Sender, Node), IntToGroupProps(SettingValueInt(ISettingID_Browse_GT_HintProps)), ': ', S_CRLF);
      else          HintText := '';
    end;
  end;

  procedure PicGroupsVT_HandleGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; bViewGroups: Boolean);
  const
     // Индексы значков для элементов дерева. Там, где отрисовка производится в обработчике OnBeforeCellPaint, стоят
     //   iiBlank
    aiImgIdx: Array[TGroupNodeKind, Boolean] of Integer = (
      (-1,             -1),             // gnkNone
      (iiPhoA,         iiPhoA),         // gnkProject
      (iiView,         iiView),         // gnkView
      (iiFolderSearch, iiFolderSearch), // gnkSearch
      (iiBlank,        iiBlank),        // gnkPhoaGroup
      (iiBlank,        iiBlank));       // gnkViewGroup
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := aiImgIdx[PicGroupsVT_GetNodeKind(Sender, Node, bViewGroups), Kind=ikSelected];
  end;

  procedure PicGroupsVT_HandleGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString; View: IPhoaView);
  var Group: IPhotoAlbumPicGroup;
  begin
    Group := PicGroupsVT_GetNodeGroup(Sender, Node);
    CellText := '';
    case TextType of
      ttNormal:
        case PicGroupsVT_GetNodeKind(Sender, Node, View<>nil) of
          gnkProject:     CellText := DKLangConstW('SPhotoAlbumNode');
          gnkView:        CellText := View.Name;
          gnkSearch:      CellText := DKLangConstW('SSearchResultsNode');
          gnkPhoaGroup,
            gnkViewGroup: CellText := Group.Text;
        end;
      ttStatic: if Group.Pics.Count>0 then CellText := WideFormat('(%d)', [Group.Pics.Count]);
    end;
  end;

  procedure PicGroupsVT_HandleInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates; RootGroup, SearchResultsGroup: IPhotoAlbumPicGroup; bRootButton, bInitExpanded: Boolean);
  var p: PPhotoAlbumPicGroup;
  begin
    p := Sender.GetNodeData(Node);
     // Узел обычной группы
    if ParentNode<>nil then
      p^ := PicGroupsVT_GetNodeGroup(Sender, ParentNode).GroupsX[Node.Index]
     // Узел фотоальбома/представления
    else if Node.Index=0 then begin
      p^ := RootGroup;
      if bRootButton then Node.CheckType := ctButton;
     // Узел результатов поиска
    end else
      p^ := SearchResultsGroup;
    Sender.ChildCount[Node] := p^.Groups.Count;
     // Разворачиваем корневой узел или если группа развёрнута
    if (ParentNode=nil) or (bInitExpanded and p^.Expanded) then
      Include(InitialStates, ivsExpanded)
    else
      Sender.Expanded[Node] := False;
  end;

  procedure PicGroupsVT_HandlePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Статический текст красим серым
    if TextType=ttStatic then TargetCanvas.Font.Color := clGrayText
     // Корневые узлы выделяем жирным
    else if Sender.NodeParent[Node]=nil then TargetCanvas.Font.Style := [fsBold];
  end;

  function ShortenFileName(Canvas: TCanvas; iWidth: Integer; const ws: WideString): WideString;
  var
    awcBuf: Array[0..1024] of WideChar;
    r: TRect;
  begin
    WStrPCopy(awcBuf, ws);
    r := Rect(0, 0, iWidth, 0);
    Tnt_DrawTextW(Canvas.Handle, awcBuf, -1, r, DT_LEFT or DT_PATH_ELLIPSIS or DT_MODIFYSTRING);
    Result := awcBuf;
  end;

  function ShowFileShellContextMenu(const wsFileName: WideString): Boolean;
  begin
     // Создаём экземпляр NS_MenuManager, если он ещё не создан
    if NS_MenuManager=nil then NS_MenuManager := TShellContextMenuManager.Create;
     // Отображаем меню
    Result := NS_MenuManager.ShowMenu(wsFileName); 
  end;

var
  iWaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

  procedure StartWait;
  begin
    if iWaitCount=0 then begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
    end;
    Inc(iWaitCount);
  end;

  procedure StopWait;
  begin
    if iWaitCount>0 then begin
      Dec(iWaitCount);
      if iWaitCount=0 then Screen.Cursor := SaveCursor;
    end;
  end;

end.



