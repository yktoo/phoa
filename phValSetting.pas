//**********************************************************************************************************************
//  $Id: phValSetting.pas,v 1.19 2007-06-28 18:41:37 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phValSetting;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, IniFiles, StdCtrls, ExtCtrls,
  TntSysUtils, TntClasses, TntWideStrings, VirtualTrees,
  ConsVars, phSettings;

type
   //===================================================================================================================
   // TPhoaValSetting - настройка, имеющая значение
   //===================================================================================================================

  TPhoaValSetting = class(TPhoaSetting)
  protected
     // Данные
    FData: Integer;
     // Состояние изменённости данных
    FModified: Boolean;
     // Prop handlers
    function  GetAsWideString: WideString; virtual; abstract;
    procedure SetAsWideString(const wsValue: WideString); virtual; abstract;
    function  GetDisplayString: WideString; virtual;
    function  GetModified: Boolean; override;
    procedure SetModified(Value: Boolean); override;
  public
    procedure AfterConstruction; override;
    procedure Assign(Source: TPhoaSetting); override;
    procedure RegLoad(RegIniFile: TRegIniFile); override;
    procedure RegSave(RegIniFile: TRegIniFile); override;
    procedure IniLoad(IniFile: TIniFile); override;
    procedure IniSave(IniFile: TIniFile); override;
     // Props
     // -- Значение (данные) в виде строки
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
     // -- Отображаемое значение в виде строки. В базовом классе совпадает с AsWideString
    property DisplayString: WideString read GetDisplayString;
  end;

   //===================================================================================================================
   // TPhoaIntSetting - настройка, имеющая значение типа Integer
   //===================================================================================================================

  TPhoaIntSetting = class(TPhoaValSetting)
  private
     // Prop storage
    FMaxValue: Integer;
    FMinValue: Integer;
     // Prop handlers
    function  GetValue: Integer;
    procedure SetValue(Value: Integer);
  protected
    function  GetAsWideString: WideString; override;
    function  GetDisplayString: WideString; override;
    procedure SetAsWideString(const wsValue: WideString); override;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue, iMinValue, iMaxValue: Integer);
    procedure Assign(Source: TPhoaSetting); override;
     // Props
     // -- Максимальное и минимальное значения пунктов
    property MaxValue: Integer read FMaxValue;
    property MinValue: Integer read FMinValue;
     // -- Значение пункта
    property Value: Integer read GetValue write SetValue;
  end;

   //===================================================================================================================
   // TPhoaIntEntrySetting - настройка, типа Integer, позволяющая редактирование значения
   //===================================================================================================================

  TPhoaIntEntrySetting = class(TPhoaIntSetting)
  protected
    function  GetDisplayString: WideString; override;
  end;

   //===================================================================================================================
   // TPhoaBoolSetting - настройка, имеющая значение типа Boolean
   //===================================================================================================================

  TPhoaBoolSetting = class(TPhoaValSetting)
  private
     // Prop handlers
    function  GetValue: Boolean;
    procedure SetValue(Value: Boolean);
  protected
    function  GetAsWideString: WideString; override;
    procedure SetAsWideString(const wsValue: WideString); override;
    function  GetDisplayString: WideString; override;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; bValue: Boolean);
    procedure Assign(Source: TPhoaSetting); override;
     // Props
     // -- Значение пункта
    property Value: Boolean read GetValue write SetValue;
  end;

   //===================================================================================================================
   // TPhoaWideStrSetting - настройка, имеющая значение типа WideString
   //===================================================================================================================

  TPhoaWideStrSetting = class(TPhoaValSetting)
  private
     // Prop handlers
    function  GetValue: WideString;
    procedure SetValue(const Value: WideString);
  protected
    function  GetAsWideString: WideString; override;
    procedure SetAsWideString(const wsValue: WideString); override;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; const wsValue: WideString);
    destructor Destroy; override;
    procedure Assign(Source: TPhoaSetting); override;
     // Props
     // -- Значение пункта
    property Value: WideString read GetValue write SetValue;
  end;

   //===================================================================================================================
   // TPhoaListSetting - настройка, имеющая значение типа Integer (но которое может храниться в виде строки, при
   //   ValueType=lsvtIndexString) и список допустимых значений-вариантов
   //===================================================================================================================

   // Тип значения настройки класса TPhoaListSetting
  TListSettingValueType = (lsvtIndex, lsvtObject, lsvtIndexString);

  TPhoaListSetting = class(TPhoaIntSetting)
  private
     // Prop storage
    FValueType: TListSettingValueType;
    FVariants: TWideStrings;
     // Prop handlers
    function  GetVariantIndex: Integer;
    function  GetVariantText: WideString;
    procedure SetVariantIndex(iValue: Integer);
  protected
    constructor CreateNew(AOwner: TPhoaSetting); override;
    function  GetAsWideString: WideString; override;
    function  GetDisplayString: WideString; override;
    procedure SetAsWideString(const wsValue: WideString); override;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue: Integer; AValueType: TListSettingValueType);
    destructor Destroy; override;
    procedure Assign(Source: TPhoaSetting); override;
     // Props
     // -- Тип значения
    property ValueType: TListSettingValueType read FValueType;
     // -- Индекс в Variants, соответствующий текущему Value; -1, если нет такого соответствия
    property VariantIndex: Integer read GetVariantIndex write SetVariantIndex;
     // -- Дочерние пункты (варианты) пункта. Текст закодирован по правилам ConstValEx()
    property Variants: TWideStrings read FVariants;
     // -- Текст из Variants, соответствующий текущему ValueInt; пустая строка, если нет такого соответствия
    property VariantText: WideString read GetVariantText;
  end;

   //===================================================================================================================
   // TPhoaFontSetting - настройка, имеющая значение типа TFont (хранится в виде строки)
   //===================================================================================================================

  TPhoaFontSetting = class(TPhoaWideStrSetting)
  protected
    function  GetDisplayString: WideString; override;
  public
     // Присваивает шрифт объекту TFont
    procedure GetFont(Font: TFont);
     // Забирает значение из объекта TFont
    procedure SetFont(Font: TFont);
  end;

   //===================================================================================================================
   // TPhoaColorSetting - настройка, имеющая значение типа TColor
   //===================================================================================================================

  TPhoaColorSetting = class(TPhoaIntSetting)
  protected
    function  GetDisplayString: WideString; override;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue: Integer);
  end;

   //===================================================================================================================
   // TPhoaMaskBitSetting - настройка, не имеющая значения, представляющая собой переключатель - бит в значении родителя
   //===================================================================================================================

  TPhoaMaskBitSetting = class(TPhoaSetting)
  end;

   //===================================================================================================================
   // TPhoaMutexSetting - настройка, не имеющая значения, представляющая собой вариант выбора, индекс которого является
   //   значением родителя
   //===================================================================================================================

  TPhoaMutexSetting = class(TPhoaSetting)
  end;

   //===================================================================================================================
   // TPhoaColorSetting - настройка, имеющая значение типа Integer и представляющая собой вариант выбора, значение
   //   которого является значением родителя
   //===================================================================================================================

  TPhoaMutexIntSetting = class(TPhoaIntSetting)
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue: Integer);
  end;

   //===================================================================================================================
   // TPhoaRectSetting - настройка, имеющая значение типа TRect. НЕ ПРЕДНАЗНАЧЕНА ДЛЯ ОТОБРАЖЕНИЯ!
   //===================================================================================================================

  TPhoaRectSetting = class(TPhoaWideStrSetting)
  private
     // Prop handlers
    function  GetValue: TRect;
    procedure SetValue(const Value: TRect);
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; const rValue: TRect);
     // Props
     // -- Значение пункта
    property Value: TRect read GetValue write SetValue;
  end;

   //===================================================================================================================
   // Класс пункта-страницы дерева настроек со значениями
   //===================================================================================================================

  TPhoaValPageSetting = class(TPhoaPageSetting)
  protected
    function  GetEditorClass: TWinControlClass; override;
  end;

implementation
uses TypInfo, Forms, Dialogs, Math, TntStdCtrls, RXSpin, phUtils;

type
   //===================================================================================================================
   // TPhoaValSettingEditor - редактор значений настроек класса TPhoaValSetting
   //===================================================================================================================

  TPhoaValSettingEditor = class(TVirtualStringTree, IPhoaSettingEditor)
  private
     // Смещение к данным узла (объекту настройки)
    FDataOffset: Cardinal;
     // Контрол для редактирования значения текущего узла
    FEditorControl: TWinControl;
     // Флаг встраивания контрола-редактора. Используется для предотвращения вызовов EmbeddedControlChange во время его
     //   начальной настройки, а также для игнорирования потери фокуса деревом
    FEmbeddingControl: Boolean;
     // Prop storage
    FOnSettingChange: TNotifyEvent;
    FRootSetting: TPhoaValPageSetting;
     // Загружает дерево настроек, относящихся к детям узла FRootSetting
    procedure LoadTree;
     // Вызывает OnSettingChange
    procedure DoSettingChange;
     // Возвращает настройку TPhoaSetting, связанную с узлом
    function  GetSetting(Node: PVirtualNode): TPhoaSetting;
     // Встраивает соответствующий контрол для текущего узла, если он нужен. Если нет (в том числе при FocusedNode=nil),
     //   удаляет текущий контрол
    procedure EmbedControl;
     // Обновляет положение контрола редактора, если он есть
    procedure PositionEditorControl;
     // События встроенного контрола
    procedure EmbeddedControlEnterExit(Sender: TObject);
    procedure EmbeddedControlChange(Sender: TObject);
    procedure EmbeddedControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EmbeddedFontButtonClick(Sender: TObject);
     // IPhoaSettingEditor
    procedure InitAndEmbed(ParentCtl: TWinControl; AOnSettingChange: TNotifyEvent);
    function  GetRootSetting: TPhoaPageSetting;
    procedure SetRootSetting(Value: TPhoaPageSetting);
     // Message handlers
    procedure WMEmbedControl(var Msg: TMessage); message WM_EMBEDCONTROL;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoAfterCellPaint(TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

   //===================================================================================================================
   // TSettingButton - потомок TTntButton, перехватывающий нажатия стрелок
   //===================================================================================================================

type
  TSettingButton = class(TTntButton)
  private
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

  procedure TSettingButton.WMGetDlgCode(var Msg: TWMGetDlgCode);
  begin
    Msg.Result := DLGC_WANTARROWS;
  end;

   //===================================================================================================================
   // TPhoaValSetting
   //===================================================================================================================

  procedure TPhoaValSetting.AfterConstruction;
  begin
    inherited AfterConstruction;
     // После создания FModified должна всегда быть False
    FModified := False;
  end;

  procedure TPhoaValSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaValSetting then FModified := TPhoaValSetting(Source).FModified;
  end;

  function TPhoaValSetting.GetDisplayString: WideString;
  begin
    Result := GetAsWideString;
  end;

  function TPhoaValSetting.GetModified: Boolean;
  begin
    Result := FModified or inherited GetModified;
  end;

  procedure TPhoaValSetting.IniLoad(IniFile: TIniFile);
  begin
    if ID<>0 then AsWideString := IniFile.ReadString(SRegPrefs_Root, Name, AsWideString);
    inherited IniLoad(IniFile);
  end;

  procedure TPhoaValSetting.IniSave(IniFile: TIniFile);
  begin
    if ID<>0 then IniFile.WriteString(SRegPrefs_Root, Name, AsWideString);
    inherited IniSave(IniFile);
  end;

  procedure TPhoaValSetting.RegLoad(RegIniFile: TRegIniFile);
  begin
    if ID<>0 then AsWideString := RegIniFile.ReadString(SRegPrefs_Root, Name, AsWideString);
    inherited RegLoad(RegIniFile);
  end;

  procedure TPhoaValSetting.RegSave(RegIniFile: TRegIniFile);
  begin
    if ID<>0 then RegIniFile.WriteString(SRegPrefs_Root, Name, AsWideString);
    inherited RegSave(RegIniFile);
  end;

  procedure TPhoaValSetting.SetModified(Value: Boolean);
  begin
    FModified := Value;
    inherited SetModified(Value);
  end;

   //===================================================================================================================
   // TPhoaIntSetting
   //===================================================================================================================

  procedure TPhoaIntSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaIntSetting then begin
      FData     := TPhoaIntSetting(Source).FData;
      FMinValue := TPhoaIntSetting(Source).FMinValue;
      FMaxValue := TPhoaIntSetting(Source).FMaxValue;
    end;
  end;

  constructor TPhoaIntSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue, iMinValue, iMaxValue: Integer);
  begin
    inherited Create(AOwner, iID, sName);
    FMinValue := iMinValue;
    FMaxValue := iMaxValue;
    SetValue(iValue);
  end;

  function TPhoaIntSetting.GetAsWideString: WideString;
  begin
    Result := IntToStr(FData);
  end;

  function TPhoaIntSetting.GetDisplayString: WideString;
  begin
    Result := ''; // IntSetting не отображает значения
  end;

  function TPhoaIntSetting.GetValue: Integer;
  begin
    Result := FData;
  end;

  procedure TPhoaIntSetting.SetAsWideString(const wsValue: WideString);
  begin
    Value := StrToIntDef(wsValue, FData);
  end;

  procedure TPhoaIntSetting.SetValue(Value: Integer);
  begin
    Value := Min(Max(Value, FMinValue), FMaxValue);
    if FData<>Value then begin
      FData := Value;
      FModified := True;
    end;
  end;

   //===================================================================================================================
   // TPhoaIntEntrySetting
   //===================================================================================================================

  function TPhoaIntEntrySetting.GetDisplayString: WideString;
  begin
    Result := AsWideString; // Перекрываем унаследованное поведение (пустую строку)
  end;

   //===================================================================================================================
   // TPhoaBoolSetting
   //===================================================================================================================

  procedure TPhoaBoolSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaBoolSetting then FData := TPhoaBoolSetting(Source).FData;
  end;

  constructor TPhoaBoolSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; bValue: Boolean);
  begin
    inherited Create(AOwner, iID, sName);
    SetValue(bValue);
  end;

  function TPhoaBoolSetting.GetAsWideString: WideString;
  begin
    Result := IntToStr(FData);
  end;

  function TPhoaBoolSetting.GetDisplayString: WideString;
  begin
    Result := '';
  end;

  function TPhoaBoolSetting.GetValue: Boolean;
  begin
    Result := FData<>0;
  end;

  procedure TPhoaBoolSetting.SetAsWideString(const wsValue: WideString);
  begin
    Value := StrToIntDef(wsValue, FData)<>0;
  end;

  procedure TPhoaBoolSetting.SetValue(Value: Boolean);
  begin
    if (FData<>0)<>Value then begin
      FData := Byte(Value);
      FModified := True;
    end;
  end;

   //===================================================================================================================
   // TPhoaWideStrSetting
   //===================================================================================================================

  procedure TPhoaWideStrSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaWideStrSetting then WideString(FData) := TPhoaWideStrSetting(Source).Value;
  end;

  constructor TPhoaWideStrSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; const wsValue: WideString);
  begin
    inherited Create(AOwner, iID, sName);
    SetValue(wsValue);
  end;

  destructor TPhoaWideStrSetting.Destroy;
  begin
    Finalize(WideString(FData));
    inherited Destroy;
  end;

  function TPhoaWideStrSetting.GetAsWideString: WideString;
  begin
    Result := GetValue;
  end;

  function TPhoaWideStrSetting.GetValue: WideString;
  begin
    Result := WideString(FData);
  end;

  procedure TPhoaWideStrSetting.SetAsWideString(const wsValue: WideString);
  begin
    Value := wsValue;
  end;

  procedure TPhoaWideStrSetting.SetValue(const Value: WideString);
  begin
    if WideString(FData)<>Value then begin
      WideString(FData) := Value;
      FModified := True;
    end;
  end;

   //===================================================================================================================
   // TPhoaListSetting
   //===================================================================================================================

  procedure TPhoaListSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaListSetting then begin
      FVariants.Assign(TPhoaListSetting(Source).FVariants);
      FValueType := TPhoaListSetting(Source).FValueType;
    end;
  end;

  constructor TPhoaListSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue: Integer; AValueType: TListSettingValueType);
  begin
    inherited Create(AOwner, iID, sName, iValue, -1, MaxInt);
    FValueType := AValueType;
  end;

  constructor TPhoaListSetting.CreateNew(AOwner: TPhoaSetting);
  begin
    inherited CreateNew(AOwner);
     // FVariants создаём здесь, т. к. при Assign Create() не вызывается
    FVariants  := TTntStringList.Create;
  end;

  destructor TPhoaListSetting.Destroy;
  begin
    FVariants.Free;
    inherited Destroy;
  end;

  function TPhoaListSetting.GetAsWideString: WideString;
  begin
    case FValueType of
      lsvtIndexString: Result := GetVariantText;
      else             Result := inherited GetAsWideString;
    end;
  end;

  function TPhoaListSetting.GetDisplayString: WideString;
  begin
    Result := GetVariantText;
  end;

  function TPhoaListSetting.GetVariantIndex: Integer;
  begin
    case FValueType of
      lsvtObject: Result := FVariants.IndexOfObject(Pointer(FData));
      else        Result := FData;
    end;
  end;

  function TPhoaListSetting.GetVariantText: WideString;
  var idx: Integer;
  begin
    idx := GetVariantIndex;
    if idx<0 then Result := '' else Result := FVariants[idx];
  end;

  procedure TPhoaListSetting.SetAsWideString(const wsValue: WideString);
  begin
    case FValueType of
      lsvtIndexString: SetVariantIndex(FVariants.IndexOf(wsValue));
      else             inherited SetAsWideString(wsValue);
    end;
  end;

  procedure TPhoaListSetting.SetVariantIndex(iValue: Integer);
  begin
    case FValueType of
      lsvtObject: if iValue<0 then Value := -1 else Value := Integer(FVariants.Objects[iValue]);
      else        Value := iValue;
    end;
  end;

   //===================================================================================================================
   // TPhoaFontSetting
   //===================================================================================================================

  function TPhoaFontSetting.GetDisplayString: WideString;
  begin
    Result := GetFirstWord(Value, '/');
  end;

  procedure TPhoaFontSetting.GetFont(Font: TFont);
  begin
    FontFromStr(Font, Value);
  end;

  procedure TPhoaFontSetting.SetFont(Font: TFont);
  begin
    Value := FontToStr(Font);
  end;

   //===================================================================================================================
   // TPhoaColorSetting
   //===================================================================================================================

  constructor TPhoaColorSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue: Integer);
  begin
    inherited Create(AOwner, iID, sName, iValue, MinInt, MaxInt);
  end;

  function TPhoaColorSetting.GetDisplayString: WideString;
  begin
    Result := ' '; // Возвращаем непустую строку, чтобы не работал column spanning
  end;

   //===================================================================================================================
   // TPhoaMutexIntSetting
   //===================================================================================================================

  constructor TPhoaMutexIntSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; iValue: Integer);
  begin
    inherited Create(AOwner, iID, sName, iValue, MinInt, MaxInt);
  end;

   //===================================================================================================================
   // TPhoaRectSetting
   //===================================================================================================================

  constructor TPhoaRectSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: AnsiString; const rValue: TRect);
  begin
    inherited Create(AOwner, iID, sName, RectToStr(rValue));
  end;

  function TPhoaRectSetting.GetValue: TRect;
  begin
    Result := StrToRect(inherited Value, Rect(0, 0, 0, 0));
  end;

  procedure TPhoaRectSetting.SetValue(const Value: TRect);
  begin
    inherited Value := RectToStr(Value);
  end;

   //===================================================================================================================
   // TPhoaValPageSetting
   //===================================================================================================================

  function TPhoaValPageSetting.GetEditorClass: TWinControlClass;
  begin
    Result := TPhoaValSettingEditor;
  end;

   //===================================================================================================================
   // TPhoaValSettingEditor
   //===================================================================================================================

  constructor TPhoaValSettingEditor.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
     // Каждый узел хранит TPhoaSetting
    FDataOffset := AllocateInternalDataArea(SizeOf(Pointer));
    Align := alClient;
    with Header do begin
      Columns.Add;
      Columns.Add.Width := 200;
      AutoSizeIndex := 0;
      Options := Options+[hoAutoResize];
    end;
    with TreeOptions do begin
      AutoOptions      := [toAutoDropExpand, toAutoScroll, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning];
      PaintOptions     := [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toFullRowSelect];
    end;
    HintMode := hmTooltip;
     // Применяем опции
    ApplyTreeSettings(Self);
  end;

  destructor TPhoaValSettingEditor.Destroy;
  begin
    FEditorControl.Free;
    inherited Destroy;
  end;

  procedure TPhoaValSettingEditor.DoAfterCellPaint(TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var Setting: TPhoaSetting;
  begin
    if Column=1 then begin
       // Если это "Цвет", рисуем квадратик соответствующего цвета
      Setting := GetSetting(Node);
      if Setting is TPhoaColorSetting then
        with TargetCanvas do begin
          Pen.Color   := clBlack;
          Brush.Color := TPhoaColorSetting(Setting).Value;
          Rectangle(CellRect.Left+2, (CellRect.Top+CellRect.Bottom-14) div 2, CellRect.Left+16, (CellRect.Top+CellRect.Bottom+14) div 2);
        end;
    end;
  end;

  procedure TPhoaValSettingEditor.DoChecked(Node: PVirtualNode);
  var Setting: TPhoaSetting;
  begin
    Setting := GetSetting(Node);
    if Setting is TPhoaBoolSetting then begin
      TPhoaBoolSetting(Setting).Value := not TPhoaBoolSetting(Setting).Value;
      DoSettingChange;
    end else if Setting is TPhoaMaskBitSetting then begin
      with GetSetting(Node.Parent) as TPhoaIntSetting do Value := Value xor (Integer(1) shl Node.Index);
      DoSettingChange;
    end else if Setting is TPhoaMutexSetting then begin
      (GetSetting(Node.Parent) as TPhoaIntSetting).Value := Node.Index;
      DoSettingChange;
    end else if Setting is TPhoaMutexIntSetting then begin
      (GetSetting(Node.Parent) as TPhoaIntSetting).Value := TPhoaMutexIntSetting(Setting).Value;
      DoSettingChange;
    end;
  end;

  procedure TPhoaValSettingEditor.DoEnter;
  begin
    inherited DoEnter;
    if not FEmbeddingControl then PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TPhoaValSettingEditor.DoExit;
  begin
    inherited DoExit;
    if not FEmbeddingControl then PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TPhoaValSettingEditor.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
  begin
    PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TPhoaValSettingEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    wsVal: WideString;
    Setting: TPhoaSetting;
  begin
    CellText := '';
    Setting := GetSetting(Node);
    case Column of
      0: CellText := ConstValEx(Setting.Name);
      1:
        if Setting is TPhoaValSetting then begin
          wsVal := TPhoaListSetting(Setting).DisplayString;
          if Setting is TPhoaListSetting then CellText := ConstValEx(wsVal) else CellText := wsVal;
        end;
    end;
  end;

  procedure TPhoaValSettingEditor.DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    ParentSetting, Setting: TPhoaSetting;
    bChecked: Boolean;
  begin
     // Ищем родительский узел
    if ParentNode=nil then ParentSetting := FRootSetting else ParentSetting := GetSetting(ParentNode);
     // Сохраняем пункт в Node.Data
    Setting := ParentSetting[Node.Index];
    PPhoaSetting(PChar(Node)+FDataOffset)^ := Setting;
     // Настраиваем CheckType и CheckState
    bChecked := False;
     // Флажок
    if Setting is TPhoaBoolSetting then begin
      Node.CheckType := ctCheckBox;
      bChecked := TPhoaBoolSetting(Setting).Value;
       // Бит в маске родителя
    end else if Setting is TPhoaMaskBitSetting then begin
      Node.CheckType := ctCheckBox;
      bChecked := (ParentSetting as TPhoaIntSetting).Value and (Integer(1) shl Node.Index)<>0;
       // RadioButton, где (значение родителя)=(индекс ребёнка)
    end else if Setting is TPhoaMutexSetting then begin
      Node.CheckType := ctRadioButton;
      bChecked := (ParentSetting as TPhoaIntSetting).Value=Integer(Node.Index);
       // RadioButton, где (значение родителя)=(значение выбранного ребёнка)
    end else if Setting is TPhoaMutexIntSetting then begin
      Node.CheckType := ctRadioButton;
      bChecked := (ParentSetting as TPhoaIntSetting).Value=TPhoaMutexIntSetting(Setting).Value;
    end;
    Node.CheckState := aCheckStates[bChecked];
     // Инициализируем к-во детей
    ChildCount[Node] := Setting.ChildCount;
     // Разворачиваем все узлы
    Include(InitialStates, ivsExpanded);
  end;

  procedure TPhoaValSettingEditor.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Выделяем жирным узлы, имеющие детей
    if ChildCount[Node]>0 then Canvas.Font.Style := [fsBold];
  end;

  procedure TPhoaValSettingEditor.DoSettingChange;
  begin
    if Assigned(FOnSettingChange) then FOnSettingChange(Self);
  end;

  procedure TPhoaValSettingEditor.EmbedControl;
  var
    ActCtl: TWinControl;
    Setting: TPhoaSetting;
    bBlurred: Boolean;
    CurNode: PVirtualNode;

     // Создаёт и присваивает в FEditorControl Control заданного класса в качестве редактора значения настройки
    procedure NewControl(CtlClass: TWinControlClass);

      procedure BindKeyEvent(const sPropName: AnsiString; Event: TKeyEvent);
      begin
        SetMethodProp(FEditorControl, sPropName, TMethod(Event));
      end;

      procedure BindNotifyEvent(const sPropName: AnsiString; Event: TNotifyEvent);
      begin
        SetMethodProp(FEditorControl, sPropName, TMethod(Event));
      end;

    begin
       // Создаём контрол, если его ещё нет, или он другого класса
      if (FEditorControl=nil) or (FEditorControl.ClassType<>CtlClass) then begin
        FreeAndNil(FEditorControl);
        FEditorControl := CtlClass.Create(Self);
        FEditorControl.Parent := Self;
      end;
       // Настраиваем размер/положение
      PositionEditorControl;
       // Tag должен указывать на соответствующий узел
      FEditorControl.Tag := Integer(CurNode);
       // Привязываем события
      BindNotifyEvent('OnEnter',   EmbeddedControlEnterExit);
      BindNotifyEvent('OnExit',    EmbeddedControlEnterExit);
      BindKeyEvent   ('OnKeyDown', EmbeddedControlKeyDown);
    end;

     // Создаёт и возвращает TTntComboBox в качестве редактора значения настройки
    procedure NewComboBox;
    var
      i: Integer;
      ListSetting: TPhoaListSetting;
    begin
      ListSetting := Setting as TPhoaListSetting;
      NewControl(TTntComboBox);
      with TTntComboBox(FEditorControl) do begin
         // Копируем список вариантов
        Items.Clear;
        for i := 0 to ListSetting.Variants.Count-1 do Items.AddObject(ConstValEx(ListSetting.Variants[i]), ListSetting.Variants.Objects[i]);
         // Прочие опции
        DropDownCount := 16;
        Style         := csDropDownList;
        ItemIndex     := ListSetting.VariantIndex;
        OnChange      := EmbeddedControlChange;
      end;
    end;

     // Создаёт и возвращает TColorBox в качестве редактора значения настройки
    procedure NewColorBox;
    begin
      NewControl(TColorBox);
      with TColorBox(FEditorControl) do begin
        DropDownCount := 16;
        Style         := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames];
        Selected      := (Setting as TPhoaColorSetting).Value;
        OnChange      := EmbeddedControlChange;
      end;
    end;

     // Создаёт и возвращает TRxSpinEdit в качестве редактора значения настройки
    procedure NewSpinEdit;
    var IES: TPhoaIntEntrySetting;
    begin
      NewControl(TRxSpinEdit);
      IES := Setting as TPhoaIntEntrySetting;
      with TRxSpinEdit(FEditorControl) do begin
        ButtonKind := bkStandard;
        MinValue   := IES.MinValue;
        MaxValue   := IES.MaxValue;
        AsInteger  := IES.Value;
        OnChange   := EmbeddedControlChange;
      end;
    end;

     // Создаёт и возвращает TSettingButton в качестве редактора шрифта
    procedure NewFontButton;
    begin
      NewControl(TSettingButton);
      with TSettingButton(FEditorControl) do begin
        Caption   := (Setting as TPhoaFontSetting).DisplayString;
        OnClick   := EmbeddedFontButtonClick;
      end;
    end;

  begin
     // Определяем флаг дефокусировки дерева/редактора
    ActCtl := GetParentForm(Self).ActiveControl;
    bBlurred := (ActCtl<>Self) and (ActCtl<>FEditorControl);
     // Проверяем необходимость [пере]создания контрола
    CurNode := FocusedNode;
    if (FEditorControl=nil) or bBlurred or (CurNode<>PVirtualNode(FEditorControl.Tag)) then begin
      FEmbeddingControl := True;
      try
         // Если нужно уничтожить контрол
        if (CurNode=nil) or bBlurred then
          FreeAndNil(FEditorControl)
         // Иначе - создаём
        else begin
           // Получаем пункт настроек из данных узла
          Setting := GetSetting(CurNode);
           // Создаём или уничтожаем контрол
          if      Setting is TPhoaListSetting     then NewComboBox
          else if Setting is TPhoaColorSetting    then NewColorBox
          else if Setting is TPhoaIntEntrySetting then NewSpinEdit
          else if Setting is TPhoaFontSetting     then NewFontButton
          else FreeAndNil(FEditorControl);
        end;
      finally
        FEmbeddingControl := False;
      end;
    end;
     // Фокусируем контрол
    if (FEditorControl<>nil) and not FEditorControl.Focused then FEditorControl.SetFocus;
  end;

  procedure TPhoaValSettingEditor.EmbeddedControlChange(Sender: TObject);
  var
    Node: PVirtualNode;
    Setting: TPhoaSetting;
  begin
    if FEmbeddingControl then Exit;
     // Tag контрола - это ссылка на его узел
    Node := PVirtualNode(FEditorControl.Tag);
     // Получаем пункт настроек из данных узла
    Setting := GetSetting(Node);
    if      Setting is TPhoaListSetting     then TPhoaListSetting(Setting).VariantIndex := (FEditorControl as TTntComboBox).ItemIndex
    else if Setting is TPhoaColorSetting    then TPhoaColorSetting(Setting).Value := (FEditorControl as TColorBox).Selected
    else if Setting is TPhoaIntEntrySetting then TPhoaIntEntrySetting(Setting).Value := (FEditorControl as TRxSpinEdit).AsInteger;
    DoSettingChange;
  end;

  procedure TPhoaValSettingEditor.EmbeddedControlEnterExit(Sender: TObject);
  begin
    if not FEmbeddingControl then PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TPhoaValSettingEditor.EmbeddedControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if (Shift*[ssShift, ssCtrl, ssAlt]=[]) and (not (Sender is TCustomComboBox) or not TCustomComboBox(Sender).DroppedDown) then
      case Key of
        VK_UP, VK_DOWN: begin
          Perform(WM_KEYDOWN, Key, 0);
          SetFocus;
          Key := 0;
        end;
      end;
  end;

  procedure TPhoaValSettingEditor.EmbeddedFontButtonClick(Sender: TObject);
  var Setting: TPhoaFontSetting;
  begin
     // Tag контрола - это ссылка на его узел. Получаем пункт настроек из данных узла
    Setting := GetSetting(PVirtualNode(FEditorControl.Tag)) as TPhoaFontSetting;
    with TFontDialog.Create(Self) do
      try
        Setting.GetFont(Font);
        if Execute then begin
          Setting.SetFont(Font);
          (FEditorControl as TSettingButton).Caption := Font.Name;
          DoSettingChange;
        end;
      finally
        Free;
      end;
  end;

  function TPhoaValSettingEditor.GetRootSetting: TPhoaPageSetting;
  begin
    Result := FRootSetting;
  end;

  function TPhoaValSettingEditor.GetSetting(Node: PVirtualNode): TPhoaSetting;
  begin
    if Node=nil then Result := nil else Result := PPhoaSetting(PChar(Node)+FDataOffset)^;
  end;

  procedure TPhoaValSettingEditor.InitAndEmbed(ParentCtl: TWinControl; AOnSettingChange: TNotifyEvent);
  begin
     // Preadjust the bounds to eliminate flicker
    BoundsRect       := ParentCtl.ClientRect;
    Parent           := ParentCtl;
    FOnSettingChange := AOnSettingChange;
  end;

  procedure TPhoaValSettingEditor.LoadTree;
  begin
    BeginUpdate;
    try
       // Удаляем все узлы
      Clear;
       // Устанавливаем количество записей в корневом каталоге
      RootNodeCount := FRootSetting.ChildCount;
       // Инициализируем все узлы
      ReinitChildren(nil, True);
       // Выделяем первый узел
      ActivateFirstVTNode(Self);
    finally
      EndUpdate;
    end;
  end;

  procedure TPhoaValSettingEditor.PositionEditorControl;
  begin
    if FEditorControl<>nil then FEditorControl.BoundsRect := GetDisplayRect(FocusedNode, 1, False);
  end;

  procedure TPhoaValSettingEditor.SetRootSetting(Value: TPhoaPageSetting);
  begin
    if FRootSetting<>Value then begin
      FRootSetting := Value as TPhoaValPageSetting;
      LoadTree;
    end;
  end;

  procedure TPhoaValSettingEditor.WMEmbedControl(var Msg: TMessage);
  begin
    EmbedControl;
  end;

  procedure TPhoaValSettingEditor.WMWindowPosChanged(var Msg: TWMWindowPosChanged);
  begin
    inherited;
    PositionEditorControl;
  end;

end.

