//**********************************************************************************************************************
//  $Id: phSettings.pas,v 1.1 2004-04-18 16:13:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phSettings;

interface
uses SysUtils, Windows, Classes, Graphics, Controls, Registry, IniFiles, VirtualTrees, TB2Dock, TBX, GR32, ConsVars;

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
    sdtComboIdx, // Список выбора (ComboBox), индекс которого представляет собой значение
    sdtComboObj, // Список выбора (ComboBox), Objects[ItemIndex] которого представляет собой значение
    sdtColor,    // Цвет
    sdtInt,      // Целое число
    sdtFont);    // Шрифт
const
   // Типы данных, редактируемые реадктором
  EditableSettingDatatypes = [sdtComboIdx, sdtComboObj, sdtColor, sdtInt, sdtFont];

type
   // Exception настроек
  EPhoaSettingError = class(EPhoaError);

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
    FDatatype: TSettingDatatype;
    FName: String;
    FMaxValue: Integer;
    FMinValue: Integer;
    FID: Integer;
    FVariants: TStrings;
     // Добавление / удаление пункта из списка дочерних пунктов
    procedure AddSetting(Item: TPhoaSetting);
    procedure RemoveSetting(Item: TPhoaSetting);
    procedure DeleteSetting(Index: Integer);
     // Очищает список дочерних пунктов
    procedure ClearSettings;
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
    procedure SetMaxValue(Value: Integer);
    procedure SetMinValue(Value: Integer);
    function  GetSettings(iID: Integer): TPhoaSetting;
    function  GetVariantIndex: Integer;
    procedure SetVariantIndex(Value: Integer);
    function  GetVariants: TStrings;
    function  GetVariantText: String;
    function GetValueBoolByID(iID: Integer): Boolean;
    function GetValueIntByID(iID: Integer): Integer;
    function GetValueStrByID(iID: Integer): String;
    procedure SetValueBoolByID(iID: Integer; Value: Boolean);
    procedure SetValueIntByID(iID: Integer; Value: Integer);
    procedure SetValueStrByID(iID: Integer; const Value: String);
  protected
     // "Простой" конструктор, не инициализирующий свойств, кроме Owner (используется в "нормальных" конструкторах и в
     //   Assign)
    constructor CreateNew(AOwner: TPhoaSetting); virtual;
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
    procedure Assign(Source: TPhoaSetting); virtual;
     // Props
     // -- Количество дочерних пунктов
    property ChildCount: Integer read GetChildCount;
     // -- Дочерние пункты по индексу
    property Children[Index: Integer]: TPhoaSetting read GetChildren; default;
     // -- Тип данных пункта
    property Datatype: TSettingDatatype read FDatatype;
     // -- ID пункта
    property ID: Integer read FID;
     // -- Максимальное и минимальное значения пунктов (только при Datatype=sdtInt)
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property MinValue: Integer read FMinValue write SetMinValue;
     // -- Наименование пункта. Если начинается с '@', то это имя константы из диалога Settings, если с '#' - то из fMain
    property Name: String read FName;
     // -- Пункт-владелец данного пункта
    property Owner: TPhoaSetting read FOwner;
     // -- Пункты по ID
    property Settings[iID: Integer]: TPhoaSetting read GetSettings;
     // -- Индекс в Variants, соответствующий текущему ValueInt; -1, если нет такого соответствия. Имеет смысл только
     //    для Datatype, равного sdtComboIdx и sdtComboObj
    property VariantIndex: Integer read GetVariantIndex write SetVariantIndex;
     // -- Дочерние пункты (варианты) пункта. Текст закодирован по тем же правилам, что и Name. Имеет смысл только для
     //    Datatype, равного sdtComboIdx и sdtComboObj
    property Variants: TStrings read GetVariants;
     // -- Текст из Variants, соответствующий текущему ValueInt; пустая строка, если нет такого соответствия. Имеет
     //    смысл только для Datatype, равного sdtComboIdx и sdtComboObj
    property VariantText: String read GetVariantText;
     // -- Типизированные значения пункта
    property ValueBool: Boolean read GetValueBool write SetValueBool;
    property ValueInt:  Integer read GetValueInt  write SetValueInt;
    property ValueStr:  String  read GetValueStr  write SetValueStr;
     // -- Типизированные значения пунктов, отыскиваемые по заданному ID
    property ValueBoolByID[iID: Integer]: Boolean read GetValueBoolByID write SetValueBoolByID;
    property ValueIntByID[iID: Integer]:  Integer read GetValueIntByID  write SetValueIntByID;
    property ValueStrByID[iID: Integer]:  String  read GetValueStrByID  write SetValueStrByID;
  end;

  TPhoaSettingClass = class of TPhoaSetting;

   // Пункт корневого пункта настроек, представляющий собой страницу
  TPhoaPageSetting = class(TPhoaSetting)
  private
     // Prop storage
    FHelpContext: THelpContext;
    FImageIndex: Integer;
  protected
    constructor CreateNew(AOwner: TPhoaSetting); override;
  public
    constructor Create(AOwner: TPhoaSetting; iID, iImageIndex: Integer; const sName: String; AHelpContext: THelpContext);
    procedure Assign(Source: TPhoaSetting); override;
     // -- HelpContext ID пункта
    property HelpContext: THelpContext read FHelpContext;
     // Props
     // -- ImageIndex пункта
    property ImageIndex: Integer read FImageIndex;
  end;

   // Применяет пользовательские настройки к TVirtualStringTree
  procedure ApplyTreeSettings(Tree: TVirtualStringTree);
   // Применяет пользовательские настройки к докам/панелям инструментов
  procedure ApplyToolbarSettings(Dock: TTBXDock);

var
   // Корневой пункт установки
  RootSetting: TPhoaSetting;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses TypInfo, phUtils;

const
   // Сообщения об ошибках
  SSettingsErrMsg_InvalidSettingID   = 'Invalid setting ID (%d)';
  SSettingsErrMsg_InvalidDataAccess  = 'Cannot access setting value as type %s';
  SSettingsErrMsg_CannotUseVariants  = 'Variants inaccessible unless a Combo datatype used';
  SSettingsErrMsg_VariantsNotCreated = 'Variants list has not been created';

   // Вызывает EPhoaSettingError
  procedure PhoaSettingError(const sMsg: String; const aParams: Array of const);

     function RetAddr: Pointer;
     asm
       mov eax, [ebp+4]
     end;

  begin
    raise EPhoaSettingError.CreateFmt(sMsg, aParams) at RetAddr;
  end;

  procedure AddPicPropSettings(Owner: TPhoaSetting);
  var Prop: TPicProperty;
  begin
    for Prop := Low(Prop) to High(Prop) do TPhoaSetting.Create(Owner, sdtParMsk, 0, '#'+GetEnumName(TypeInfo(TPicProperty), Byte(Prop)));
  end;

  procedure AdjustPicPropComboSettings(Setting: TPhoaSetting);
  var Prop: TPicProperty;
  begin
    Setting.Variants.AddObject('', Pointer(MaxInt));
    for Prop := Low(Prop) to High(Prop) do
      Setting.Variants.AddObject('#'+GetEnumName(TypeInfo(TPicProperty), Byte(Prop)), Pointer(Prop));
  end;

  procedure AddPicClipboardFormatSettings(Owner: TPhoaSetting);
  var pcf: TPicClipboardFormat;
  begin
    for pcf := Low(pcf) to High(pcf) do TPhoaSetting.Create(Owner, sdtParMsk, 0, '#'+GetEnumName(TypeInfo(TPicClipboardFormat), Byte(pcf)));
  end;

  procedure AdjustTreeCheckStyleSetting(Setting: TPhoaSetting);
  begin
    with Setting.Variants do begin
      Add('Dark Check');
      Add('Dark Tick');
      Add('Flat');
      Add('Light Check');
      Add('Light Tick');
      Add('Standard');
      Add('Standard Flat');
      Add('XP');
    end;
  end;

  procedure AddStretchFilterSettings(Owner: TPhoaSetting);
  begin
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Nearest Neighbor',   Byte(sfNearest));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Linear',             Byte(sfLinear));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'B-spline (bicubic)', Byte(sfSpline));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Lanczos',            Byte(sfLanczos));
    TPhoaSetting.Create(Owner, sdtMutexInt, 0, 'Mitchell',           Byte(sfMitchell));
  end;

  procedure AdjustMagnificationSetting(Setting: TPhoaSetting);
  var b: Byte;
  begin
    for b := 0 to High(adMagnifications) do Setting.Variants.Add(Format('%d%%', [Trunc((adMagnifications[b]-1)*100)]));
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
      if RootSetting.ValueBoolByID[ISettingID_Gen_TreeAnimation] then
        AnimationOptions := AnimationOptions+[toAnimatedToggle]
      else
        AnimationOptions := AnimationOptions-[toAnimatedToggle];
       // -- Wheel panning
      if RootSetting.ValueBoolByID[ISettingID_Gen_TreeWhPanning] then
        MiscOptions := MiscOptions+[toWheelPanning]
      else
        MiscOptions := MiscOptions-[toWheelPanning];
       // -- Center selection
      if RootSetting.ValueBoolByID[ISettingID_Gen_TreeCenterSel] then
        SelectionOptions := SelectionOptions+[toCenterScrollIntoView]
      else
        SelectionOptions := SelectionOptions-[toCenterScrollIntoView];
    end;
    with Tree do begin
       // -- Incremental search
      if RootSetting.ValueBoolByID[ISettingID_Gen_TreeIncrSearch] then begin
        IncrementalSearch        := isVisibleOnly;
        IncrementalSearchTimeout := RootSetting.ValueIntByID[ISettingID_Gen_TreeIncrSrchDelay];
      end else
        IncrementalSearch := isNone;
       // -- Button style
      if RootSetting.ValueIntByID[ISettingID_Gen_TreeButtonStyle]=0 then ButtonStyle := bsRectangle else ButtonStyle := bsTriangle;
       // -- Check style
      case RootSetting.ValueIntByID[ISettingID_Gen_TreeCheckStyle] of
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
      if RootSetting.ValueIntByID[ISettingID_Gen_TreeSelStyle]=0 then
        DrawSelectionMode := smDottedRectangle
      else
        DrawSelectionMode := smBlendedRectangle;
    end;
  end;

  procedure ApplyToolbarSettings(Dock: TTBXDock);
  var i, iMode: Integer;
  begin
    iMode := RootSetting.ValueIntByID[ISettingID_Gen_ToolbarDragStyle];
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

   //===================================================================================================================
   // TPhoaSetting
   //===================================================================================================================

  procedure TPhoaSetting.AddSetting(Item: TPhoaSetting);
  begin
    if FChildren=nil then FChildren := TList.Create;
    FChildren.Add(Item);
  end;

  procedure TPhoaSetting.Assign(Source: TPhoaSetting);
  var
    i: Integer;
    SrcChild: TPhoaSetting;
  begin
     // Стираем детей
    ClearSettings;
     // Копируем настройки
    FDatatype := Source.FDatatype;
    FID       := Source.FID;
    FName     := Source.FName;
    FMaxValue := Source.FMaxValue;
    FMinValue := Source.FMinValue;
    if FDatatype=sdtFont then SetValueStr(Source.GetValueStr) else FData := Source.FData;
    if Source.FVariants=nil then FreeAndNil(FVariants) else GetVariants.Assign(Source.FVariants);
     // Повторяем то же для детей
    for i := 0 to Source.ChildCount-1 do begin
      SrcChild := Source.Children[i];
       // Вызываем виртуальный конструктор CreateNew того же класса, что и у SrcChild
      TPhoaSettingClass(SrcChild.ClassType).CreateNew(Self).Assign(SrcChild);
    end;
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
    CreateNew(AOwner);
    FDatatype := ADatatype;
    FID       := iID;
    FName     := sName;
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

  constructor TPhoaSetting.CreateNew(AOwner: TPhoaSetting);
  begin
    inherited Create;
    FOwner := AOwner;
    if FOwner<>nil then FOwner.AddSetting(Self);
  end;

  procedure TPhoaSetting.DeleteSetting(Index: Integer);
  begin
    TPhoaSetting(FChildren[Index]).Free;
  end;

  destructor TPhoaSetting.Destroy;
  begin
    FVariants.Free;
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
    if Result=nil then PhoaSettingError(SSettingsErrMsg_InvalidSettingID, [iID]);
  end;

  function TPhoaSetting.GetValStr: String;
  begin
    if FDatatype=sdtFont then Result := GetValueStr else Result := IntToStr(FData);
  end;

  function TPhoaSetting.GetValueBool: Boolean;
  begin
    if not (FDatatype in [sdtBool]) then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['Bool']);
    Result := FData<>0;
  end;

  function TPhoaSetting.GetValueBoolByID(iID: Integer): Boolean;
  begin
    Result := Settings[iID].ValueBool;
  end;

  function TPhoaSetting.GetValueInt: Integer;
  begin
    if not (FDatatype in [sdtStatic, sdtMutexInt, sdtComboIdx, sdtComboObj, sdtColor, sdtInt]) then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['Int']);
    Result := FData;
  end;

  function TPhoaSetting.GetValueIntByID(iID: Integer): Integer;
  begin
    Result := Settings[iID].ValueInt;
  end;

  function TPhoaSetting.GetValueStr: String;
  begin
    if FDatatype<>sdtFont then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['String']);
    Result := String(FData);
  end;

  function TPhoaSetting.GetValueStrByID(iID: Integer): String;
  begin
    Result := Settings[iID].ValueStr;
  end;

  function TPhoaSetting.GetVariantIndex: Integer;
  begin
    if not (FDatatype in [sdtComboIdx, sdtComboObj]) then PhoaSettingError(SSettingsErrMsg_CannotUseVariants, []);
     // Проверяем, что список вариантов создан
    if FVariants=nil then PhoaSettingError(SSettingsErrMsg_VariantsNotCreated, []);
    case FDatatype of
      sdtComboIdx: Result := FData;
      else {sdtComboObj} Result := FVariants.IndexOfObject(Pointer(FData));
    end;
  end;

  function TPhoaSetting.GetVariants: TStrings;
  begin
    if not (FDatatype in [sdtComboIdx, sdtComboObj]) then PhoaSettingError(SSettingsErrMsg_CannotUseVariants, []);
     // Создаём список Variants, если он ещё не создан
    if FVariants=nil then FVariants := TStringList.Create;
    Result := FVariants;
  end;

  function TPhoaSetting.GetVariantText: String;
  var idx: Integer;
  begin
    idx := GetVariantIndex;
    if idx<0 then Result := '' else Result := FVariants[idx];
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

  procedure TPhoaSetting.SetMaxValue(Value: Integer);
  begin
    if FDatatype<>sdtInt then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['Int']);
    FMaxValue := Value;
  end;

  procedure TPhoaSetting.SetMinValue(Value: Integer);
  begin
    if FDatatype<>sdtInt then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['Int']);
    FMinValue := Value;
  end;

  procedure TPhoaSetting.SetValueBool(Value: Boolean);
  begin
    if FDatatype<>sdtBool then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['Bool']);
    FData := Byte(Value);
  end;

  procedure TPhoaSetting.SetValueBoolByID(iID: Integer; Value: Boolean);
  begin
    Settings[iID].ValueBool := Value;
  end;

  procedure TPhoaSetting.SetValueInt(Value: Integer);
  begin
    if not (FDatatype in [sdtStatic, sdtMutexInt, sdtComboIdx, sdtComboObj, sdtColor, sdtInt]) then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['Int']);
     // Validate value
    if (FDatatype=sdtInt) and (FMaxValue>FMinValue) then
      if Value<FMinValue then Value := FMinValue
      else if Value>FMaxValue then Value := FMaxValue;
    FData := Value;
  end;

  procedure TPhoaSetting.SetValueIntByID(iID: Integer; Value: Integer);
  begin
    Settings[iID].ValueInt := Value;
  end;

  procedure TPhoaSetting.SetValueStr(const Value: String);
  begin
    if FDatatype<>sdtFont then PhoaSettingError(SSettingsErrMsg_InvalidDataAccess, ['String']);
    String(FData) := Value;
  end;

  procedure TPhoaSetting.SetValueStrByID(iID: Integer; const Value: String);
  begin
    Settings[iID].ValueStr := Value;
  end;

  procedure TPhoaSetting.SetVariantIndex(Value: Integer);
  begin
    if not (FDatatype in [sdtComboIdx, sdtComboObj]) then PhoaSettingError(SSettingsErrMsg_CannotUseVariants, []);
     // Проверяем, что список вариантов создан
    if FVariants=nil then PhoaSettingError(SSettingsErrMsg_VariantsNotCreated, []);
    case FDatatype of
      sdtComboIdx: FData := Value;
      else {sdtComboObj} if Value<0 then FData := -1 else FData := Integer(FVariants.Objects[Value]);
    end;
  end;

   //===================================================================================================================
   // TPhoaPageSetting
   //===================================================================================================================

  procedure TPhoaPageSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaPageSetting then begin
      FImageIndex  := TPhoaPageSetting(Source).FImageIndex;
      FHelpContext := TPhoaPageSetting(Source).FHelpContext;
    end;
  end;

  constructor TPhoaPageSetting.Create(AOwner: TPhoaSetting; iID, iImageIndex: Integer; const sName: String; AHelpContext: THelpContext);
  begin
    inherited Create(AOwner, sdtStatic, iID, sName);
    FImageIndex  := iImageIndex;
    FHelpContext := AHelpContext;
  end;

  constructor TPhoaPageSetting.CreateNew(AOwner: TPhoaSetting);
  begin
    inherited CreateNew(AOwner);
    FImageIndex := -1;
  end;

   //===================================================================================================================

  {$HINTS OFF} // Do not hint abount var not used
  procedure InitSettings;
  var Lvl1, Lvl2, Lvl3, Lvl4: TPhoaSetting;
  begin
     //=================================================================================================================
    Lvl1 := TPhoaPageSetting.Create(RootSetting, ISettingID_Gen, iiA_Props, '@ISettingID_Gen', IDH_setup_general);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Gen_Intf,              '@ISettingID_Gen_Intf');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Gen_Language,          '@ISettingID_Gen_Language', $409 {=1033, English-US});
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtFont,     ISettingID_Gen_MainFont,          '@ISettingID_Gen_MainFont', 'Tahoma/8/0/0/1');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,      ISettingID_Gen_TooltipDisplTime,  '@ISettingID_Gen_TooltipDisplTime', 5000);
        Lvl3.MinValue := 100;
        Lvl3.MaxValue := MaxInt;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,      ISettingID_Gen_OpenMRUCount,      '@ISettingID_Gen_OpenMRUCount', 10);
        Lvl3.MinValue := 0;
        Lvl3.MaxValue := 15;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Gen_LookupPhoaIni,     '@ISettingID_Gen_LookupPhoaIni', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Gen_Clipboard,         '@ISettingID_Gen_Clipboard');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Gen_ClipFormats,       '@ISettingID_Gen_ClipFormats', Byte(DefaultPicClipboardFormats));
        AddPicClipboardFormatSettings(lvl3);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Gen_Toolbars,          '@ISettingID_Gen_Toolbars');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Gen_ToolbarBtnSize,    '@ISettingID_Gen_ToolbarBtnSize', 0);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarBSz16,      '@ISettingID_Gen_ToolbarBSz16');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarBSz24,      '@ISettingID_Gen_ToolbarBSz24');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarBSz32,      '@ISettingID_Gen_ToolbarBSz32');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Gen_ToolbarDragStyle,  '@ISettingID_Gen_ToolbarDragStyle', 3);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarDrgNone,    '@ISettingID_Gen_ToolbarDrgNone');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarDrgOneDock, '@ISettingID_Gen_ToolbarDrgOneDock');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarDrgNoFloat, '@ISettingID_Gen_ToolbarDrgNoFloat');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_ToolbarDrgFree,    '@ISettingID_Gen_ToolbarDrgFree');
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Gen_Tree,              '@ISettingID_Gen_Tree');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Gen_TreeAnimation,     '@ISettingID_Gen_TreeAnimation',  True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Gen_TreeWhPanning,     '@ISettingID_Gen_TreeWhPanning',  True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Gen_TreeCenterSel,     '@ISettingID_Gen_TreeCenterSel',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Gen_TreeIncrSearch,    '@ISettingID_Gen_TreeIncrSearch', True);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtInt,      ISettingID_Gen_TreeIncrSrchDelay, '@ISettingID_Gen_TreeIncrSrchDelay', 1000);
          Lvl4.MinValue := 100;
          Lvl4.MaxValue := 10000;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Gen_TreeButtonStyle,   '@ISettingID_Gen_TreeButtonStyle',   0);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_TreeBS_Rectangle,  '@ISettingID_Gen_TreeBS_Rectangle');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_TreeBS_Triangle,   '@ISettingID_Gen_TreeBS_Triangle');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtComboIdx, ISettingID_Gen_TreeCheckStyle,    '@ISettingID_Gen_TreeCheckStyle', 7 {XP});
        AdjustTreeCheckStyleSetting(lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Gen_TreeSelStyle,      '@ISettingID_Gen_TreeSelStyle', 1);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_TreeSelDotted,     '@ISettingID_Gen_TreeSelDotted');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtMutex,    ISettingID_Gen_TreeSelBlended,    '@ISettingID_Gen_TreeSelBlended');
     //=================================================================================================================
    Lvl1 := TPhoaPageSetting.Create(RootSetting, ISettingID_Browse, iiA_NewGroup, '@ISettingID_Browse', IDH_setup_browse_mode);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Browse_Viewer,         '@ISettingID_Browse_Viewer');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtColor,    ISettingID_Browse_ViewerBkColor,  '@ISettingID_Browse_ViewerBkColor',  $d7d7d7);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtColor,    ISettingID_Browse_ViewerThBColor, '@ISettingID_Browse_ViewerThBColor', clBtnFace);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtColor,    ISettingID_Browse_ViewerThFColor, '@ISettingID_Browse_ViewerThFColor', clWindowText);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Browse_ViewerDragDrop, '@ISettingID_Browse_ViewerDragDrop', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Browse_ViewerTooltips, '@ISettingID_Browse_ViewerTooltips', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Browse_ViewerTipProps, '@ISettingID_Browse_ViewerTipProps');
        Lvl3.ValueInt := PicPropsToInt([ppFileName, ppFileSize, ppPicDims, ppDate, ppTime, ppPlace, ppFilmNumber, ppFrameNumber, ppAuthor, ppDescription, ppMedia]);
        AddPicPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Browse_ViewerThInfo,   '@ISettingID_Browse_ViewerThInfo');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtComboObj,   ISettingID_Browse_ViewerThLTProp, '@ISettingID_Browse_ViewerThLTProp', -1);
          AdjustPicPropComboSettings(Lvl4);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtComboObj,   ISettingID_Browse_ViewerThRTProp, '@ISettingID_Browse_ViewerThRTProp', -1);
          AdjustPicPropComboSettings(Lvl4);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtComboObj,   ISettingID_Browse_ViewerThLBProp, '@ISettingID_Browse_ViewerThLBProp', Byte(ppPlace));
          AdjustPicPropComboSettings(Lvl4);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtComboObj,   ISettingID_Browse_ViewerThRBProp, '@ISettingID_Browse_ViewerThRBProp', Byte(ppDate));
          AdjustPicPropComboSettings(Lvl4);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Browse_ViewerThBorder, '@ISettingID_Browse_ViewerThBorder', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Browse_ViewerCacheThs, '@ISettingID_Browse_ViewerCacheThs', True);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtInt,      ISettingID_Browse_ViewerCacheSze, '@ISettingID_Browse_ViewerCacheSze', 1000);
          Lvl4.MinValue := 1;
          Lvl4.MaxValue := MaxInt;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Browse_ViewerStchFilt, '@ISettingID_Browse_ViewerStchFilt', Byte(sfNearest));
        AddStretchFilterSettings(Lvl3);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtInt,      ISettingID_Browse_MaxUndoCount,   '@ISettingID_Browse_MaxUndoCount', 32);
      Lvl2.MinValue := 1;
      Lvl2.MaxValue := MaxInt;
     //=================================================================================================================
    Lvl1 := TPhoaPageSetting.Create(RootSetting, ISettingID_View, iiA_ViewMode, '@ISettingID_View', IDH_setup_view_mode);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,     ISettingID_View_AlwaysOnTop,      '@ISettingID_View_AlwaysOnTop', False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,     ISettingID_View_Fullscreen,       '@ISettingID_View_Fullscreen', False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,     ISettingID_View_KeepCursorOverTB, '@ISettingID_View_KeepCursorOverTB', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,     ISettingID_View_HideCursor,       '@ISettingID_View_HideCursor', False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtColor,    ISettingID_View_BkColor,          '@ISettingID_View_BkColor', $000000);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtBool,     ISettingID_View_ShowToolbar,      '@ISettingID_View_ShowToolbar', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_View_PicChange,        '@ISettingID_View_PicChange');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_FitWindowToPic,   '@ISettingID_View_FitWindowToPic', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_CenterWindow,     '@ISettingID_View_CenterWindow', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_ShrinkPicToFit,   '@ISettingID_View_ShrinkPicToFit', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_ZoomPicToFit,     '@ISettingID_View_ZoomPicToFit', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_Cyclic,           '@ISettingID_View_Cyclic', False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_View_Optimizing,       '@ISettingID_View_Optimizing');
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtBool,     ISettingID_View_Predecode,        '@ISettingID_View_Predecode', True);
          Lvl4 := TPhoaSetting.Create(Lvl3, sdtBool,     ISettingID_View_CacheBehind,      '@ISettingID_View_CacheBehind', True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtComboIdx, ISettingID_View_ZoomFactor,       '@ISettingID_View_ZoomFactor', 3 {=50%});
      AdjustMagnificationSetting(Lvl2);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_View_CaptionProps,     '@ISettingID_View_CaptionProps', PicPropsToInt([ppFileName]));
      AddPicPropSettings(Lvl2);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_View_StchFilt,         '@ISettingID_View_StchFilt', Byte(sfNearest));
      AddStretchFilterSettings(Lvl2);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_View_Info,             '@ISettingID_View_Info');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_ShowInfo,         '@ISettingID_View_ShowInfo', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_View_InfoPicProps,     '@ISettingID_View_InfoPicProps', PicPropsToInt([ppDate, ppTime, ppPlace, ppDescription]));
        AddPicPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtFont,     ISettingID_View_InfoFont,         '@ISettingID_View_InfoFont', 'Arial/14/0/16777215/1');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtColor,    ISettingID_View_InfoBkColor,      '@ISettingID_View_InfoBkColor', $000000);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,      ISettingID_View_InfoBkOpacity,    '@ISettingID_View_InfoBkOpacity', $40);
        Lvl3.MinValue := 0;
        Lvl3.MaxValue := 255;
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_View_Slideshow,        '@ISettingID_View_Slideshow');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtInt,      ISettingID_View_SlideInterval,    '@ISettingID_View_SlideInterval', 5000);
        Lvl3.MinValue := 0;
        Lvl3.MaxValue := 600*1000;
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_View_SlideCyclic,      '@ISettingID_View_SlideCyclic', True);
     //=================================================================================================================
    Lvl1 := TPhoaPageSetting.Create(RootSetting, ISettingID_Dialogs, iiA_Dialog, '@ISettingID_Dialogs', IDH_setup_dialogs);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Dlgs_Confms,           '@ISettingID_Dlgs_Confms');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_ConfmDelGroup,    '@ISettingID_Dlgs_ConfmDelGroup',    True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_ConfmDelPics,     '@ISettingID_Dlgs_ConfmDelPics',     True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_ConfmDelView,     '@ISettingID_Dlgs_ConfmDelView',     True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_ConfmOldFile,     '@ISettingID_Dlgs_ConfmOldFile',     True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_ConfmAppExit,     '@ISettingID_Dlgs_ConfmAppExit',     False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Dlgs_Notifies,         '@ISettingID_Dlgs_Notifies');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_NotifyDragCopy,   '@ISettingID_Dlgs_NotifyDragCopy',   True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_NotifyDragMove,   '@ISettingID_Dlgs_NotifyDragMove',   True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_NotifyPaste,      '@ISettingID_Dlgs_NotifyPaste',      True);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Dlgs_AddPicWizard,     '@ISettingID_Dlgs_AddPicWizard');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_APW_ShowHidden,   '@ISettingID_Dlgs_APW_ShowHidden',   False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_APW_SkipChkPage,  '@ISettingID_Dlgs_APW_SkipChkPage',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_APW_LogOnErrOnly, '@ISettingID_Dlgs_APW_LogOnErrOnly', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Dlgs_APW_AutofillDate, '@ISettingID_Dlgs_APW_AutofillDate', Byte(DTAP_DefaultDateProps));
        AddDateTimeAutofillPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_APW_ReplaceDate,  '@ISettingID_Dlgs_APW_ReplaceDate',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtStatic,   ISettingID_Dlgs_APW_AutofillTime, '@ISettingID_Dlgs_APW_AutofillTime', Byte(DTAP_DefaultTimeProps));
        AddDateTimeAutofillPropSettings(Lvl3);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_APW_ReplaceTime,  '@ISettingID_Dlgs_APW_ReplaceTime',  False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Dlgs_PicProps,         '@ISettingID_Dlgs_PicProps');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_PP_ExpFileProps,  '@ISettingID_Dlgs_PP_ExpFileProps',  False);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_PP_ExpMetadata ,  '@ISettingID_Dlgs_PP_ExpMetadata',   False);
      Lvl2 := TPhoaSetting.Create(Lvl1, sdtStatic,   ISettingID_Dlgs_FileOpsWizard,    '@ISettingID_Dlgs_FileOpsWizard');
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_FOW_CfmCopyFiles, '@ISettingID_Dlgs_FOW_CfmCopyFiles', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_FOW_CfmMoveFiles, '@ISettingID_Dlgs_FOW_CfmMoveFiles', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_FOW_CfmDelFiles,  '@ISettingID_Dlgs_FOW_CfmDelFiles',  True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_FOW_CfmRebuildTh, '@ISettingID_Dlgs_FOW_CfmRebuildTh', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_FOW_CfmRepairFLs, '@ISettingID_Dlgs_FOW_CfmRepairFLs', True);
        Lvl3 := TPhoaSetting.Create(Lvl2, sdtBool,     ISettingID_Dlgs_FOW_LogOnErrOnly, '@ISettingID_Dlgs_FOW_LogOnErrOnly', True);
     //=================================================================================================================
{
    Lvl1 := TPhoaPageSetting.Create(RootSetting, ISettingID_Dialogs, iiA_Dialog, '@ISettingID_Dialogs', IDH_setup_dialogs);
    Lvl1.ImageIndex  := iiA_Dialog;
    Lvl1.HelpContext := IDH_setup_dialogs;
}  end;
  {$HINTS ON}

initialization
   // Создаём установки
  RootSetting := TPhoaSetting.Create(nil, sdtStatic, 0, '');
  InitSettings;
finalization
  RootSetting.Free;
end.
