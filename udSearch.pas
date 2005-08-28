//**********************************************************************************************************************
//  $Id: udSearch.pas,v 1.40 2005-08-28 06:05:23 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Registry, Contnrs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  phDlg, ActnList, TBX, Menus, TB2Item, DKLang, TB2Dock, TB2Toolbar,
  VirtualTrees, ComCtrls, StdCtrls, ExtCtrls, ufrExprPicFilter;

type
   // Вид поиска
  TPicSearchKind = (pskSimple, pskExpression);

   // Условие критерия простого поиска
  TSimpleSearchCondition = (
    sscInvalid, // "Недопустимое условие"
     // Number conditions
    sscNumberLess, sscNumberLessEqual, sscNumberEqual, sscNumberNotEqual, sscNumberGreaterEqual, sscNumberGreater,
     // String conditions
    sscStrSpecified, sscStrNotSpecified, sscStrStarts, sscStrNotStarts, sscStrEqual, sscStrNotEqual, sscStrEnds,
    sscStrNotEnds, sscStrContains, sscStrNotContains, sscStrMatchesMask, sscStrNotMatchesMask,
     // Date/Time conditions
    sscDateTimeSpecified, sscDateTimeNotSpecified, sscDateTimeLess, sscDateTimeLessEqual, sscDateTimeEqual,
    sscDateTimeNotEqual, sscDateTimeGreaterEqual, sscDateTimeGreater,
     // List conditions
    sscListSpecified, sscListNotSpecified, sscListAny, sscListNone, sscListAll);
  TSimpleSearchConditions = set of TSimpleSearchCondition;

   //===================================================================================================================
   // Критерий простого поиска
   //===================================================================================================================

  TSimpleSearchCriterion = class(TObject)
  private
     // Заготовленные на время поиска типизированные значения критерия
    FValue_Integer: Integer;
    FValue_Float: Double;
    FValue_String: String;
    FValue_Masks: TPhoaMasks;
    FValue_Keywords: IPhotoAlbumKeywordList;
     // Prop storage
    FPicProperty: TPicProperty;
    FCondition: TSimpleSearchCondition;
    FValue: Variant;
    FDatatype: TPicPropDatatype;
     // Настраивает поля, зависящие от PicProperty
    procedure AdjustPicProperty;
     // Prop handlers
    function  GetAsExpression: String;
    function  GetConditionName: String;
    function  GetDataString: String;
    function  GetPicPropertyName: String;
    function  GetValueStr: String;
    procedure SetCondition(Value: TSimpleSearchCondition);
    procedure SetDataString(const Value: String);
    procedure SetPicProperty(Value: TPicProperty);
    procedure SetValueStr(const sValue: String);
  public
    constructor Create;
     // Возвращает True, если изображение подходит под критерий
    function  Matches(Pic: IPhoaPic): Boolean;
     // Подготовка/финализация поиска
    procedure InitializeSearch;
    procedure FinalizeSearch;
     // Заполняет Strings строками с возможными условиями критерия для текущего типа данных
    procedure ObtainConditionStrings(Strings: TStrings);
     // Props
     // -- Условие критерия в виде выражения поиска
    property AsExpression: String read GetAsExpression;
     // -- Условие критерия
    property Condition: TSimpleSearchCondition read FCondition write SetCondition;
     // -- Наименование условия критерия
    property ConditionName: String read GetConditionName;
     // -- Все данные критерия в виде строки
    property DataString: String read GetDataString write SetDataString;
     // -- Тип данных PicProperty
    property Datatype: TPicPropDatatype read FDatatype;
     // -- Свойство изображения, проверяемое на условие
    property PicProperty: TPicProperty read FPicProperty write SetPicProperty;
     // -- Наименование свойства изображения
    property PicPropertyName: String read GetPicPropertyName;
     // -- Значение критерия
    property Value: Variant read FValue write FValue;
     // -- Значение критерия в виде строки
    property ValueStr: String read GetValueStr write SetValueStr;
  end;

   //===================================================================================================================
   // Список критериев типа TSimpleSearchCriterion
   //===================================================================================================================

  TSimpleSearchCriterionList = class(TObject)
  private
     // Собственно список
    FList: TObjectList;
     // Prop handlers
    function GetCount: Integer;
    function GetItems(Index: Integer): TSimpleSearchCriterion;
  public
    constructor Create;
    destructor Destroy; override;
    function  Add(Item: TSimpleSearchCriterion): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
     // Подготовка/финализация поиска
    procedure InitializeSearch;
    procedure FinalizeSearch;
     // Сохранение/загрузка из реестра
    procedure RegLoad(rif: TRegIniFile; const sSection: String);
    procedure RegSave(rif: TRegIniFile; const sSection: String);
     // Возвращает True, если изображение подходит под все критерии
    function  Matches(Pic: IPhoaPic): Boolean;
     // Props
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSimpleSearchCriterion read GetItems; default;
  end;

   //===================================================================================================================
   // TdSearch
   //===================================================================================================================

  TdSearch = class(TPhoaDialog)
    alMain: TActionList;
    aSimpleConvertToExpression: TAction;
    aSimpleCrDelete: TAction;
    aSimpleReset: TAction;
    bSimpleConvertToExpression: TTBXItem;
    bSimpleCrDelete: TTBXItem;
    bSimpleReset: TTBXItem;
    dklcMain: TDKLanguageController;
    dkSimpleTop: TTBXDock;
    frExprPicFilter: TfrExprPicFilter;
    gbSearch: TGroupBox;
    ipmSimpleDelete: TTBXItem;
    ipmsmSimpleProp: TTBXSubmenuItem;
    pcCriteria: TPageControl;
    pmSimple: TTBXPopupMenu;
    rbAll: TRadioButton;
    rbCurGroup: TRadioButton;
    rbSearchResults: TRadioButton;
    tbSimpleMain: TTBXToolbar;
    tsExpression: TTabSheet;
    tsSimple: TTabSheet;
    tvSimpleCriteria: TVirtualStringTree;
    procedure aaSimpleConvertToExpression(Sender: TObject);
    procedure aaSimpleCrDelete(Sender: TObject);
    procedure aaSimpleReset(Sender: TObject);
    procedure pcCriteriaChange(Sender: TObject);
    procedure pmSimplePopup(Sender: TObject);
    procedure tvSimpleCriteriaBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvSimpleCriteriaChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvSimpleCriteriaCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure tvSimpleCriteriaEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvSimpleCriteriaFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvSimpleCriteriaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvSimpleCriteriaInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvSimpleCriteriaPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Список критериев простого поиска
    FSimpleCriteria: TSimpleSearchCriterionList;
     // Локальный список результатов поиска (ссылок на изображения)
    FLocalResults: IPhoaMutablePicList;
     // Приложение
    FApp: IPhotoAlbumApp;
     // Группа, в которую помещать результаты
    FResultsGroup: IPhotoAlbumPicGroup;
    FSearchKind: TPicSearchKind;
     // Обновляет tvSimpleCriteria (в ответ на изменение FSimpleCriteria)
    procedure SyncSimpleCriteria;
     // Возвращает объект критерия простого поиска по узлу tvSimpleCriteria, или nil, если Node=nil или Node
     //   соответствует "пустому" узлу
    function  GetSimpleCriterion(Node: PVirtualNode): TSimpleSearchCriterion;
     // Основная процедура поиска
    procedure PerformSearch;
     // Событие клика на пункте свойства изображения в pmSimple
    procedure SimpleCrPicPropClick(Sender: TObject);
     // Обновляет FSearchKind
    procedure UpdateSearchKind;
  protected
    function  GetRelativeRegistryKey: String; override;
    function  GetSizeable: Boolean; override;
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
    procedure SettingsLoad(rif: TRegIniFile); override;
    procedure SettingsSave(rif: TRegIniFile); override;
    procedure UpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     // Props
    property SearchKind: TPicSearchKind read FSearchKind;
  end;

const
   // Наборы условий критериев для типов данных свойств избражений
  SSCNumberConditions         = [
    sscNumberLess, sscNumberLessEqual, sscNumberEqual, sscNumberNotEqual, sscNumberGreaterEqual, sscNumberGreater];
  SSCStringConditions         = [
    sscStrSpecified, sscStrNotSpecified, sscStrStarts, sscStrNotStarts, sscStrEqual, sscStrNotEqual, sscStrEnds,
    sscStrNotEnds, sscStrContains, sscStrNotContains, sscStrMatchesMask, sscStrNotMatchesMask];
  SSCStringConditions_Pattern = [
    sscStrStarts, sscStrNotStarts, sscStrEqual, sscStrNotEqual, sscStrEnds, sscStrNotEnds, sscStrContains,
    sscStrNotContains];
  SSCStringConditions_Mask    = [sscStrMatchesMask, sscStrNotMatchesMask];
  SSCDateTimeConditions       = [
    sscDateTimeSpecified, sscDateTimeNotSpecified, sscDateTimeLess, sscDateTimeLessEqual, sscDateTimeEqual,
    sscDateTimeNotEqual, sscDateTimeGreaterEqual, sscDateTimeGreater];
  SSCListConditions           = [sscListSpecified, sscListNotSpecified, sscListAny, sscListNone, sscListAll];
   // Набор условий, не поддерживающих значения
  SSCNoValueConditions        = [
    sscInvalid, sscStrSpecified, sscStrNotSpecified, sscDateTimeSpecified, sscDateTimeNotSpecified, sscListSpecified,
    sscListNotSpecified];
   // Наборы условий критериев в зависимости от типа данных свойства избражения
  aSSConditionsByDatatype: Array[TPicPropDatatype] of TSimpleSearchConditions = (
    SSCStringConditions,   // ppdtString
    SSCNumberConditions,   // ppdtInteger
    SSCNumberConditions,   // ppdtFloat
    SSCDateTimeConditions, // ppdtDate
    SSCDateTimeConditions, // ppdtTime
    [],                    // ppdtBoolean
    SSCListConditions,     // ppdtList
    [],                    // ppdtSize
    [],                    // ppdtPixelFormat
    [],                    // ppdtRotation
    []);                   // ppdtFlips
   // Условия по умолчанию для типов данных свойств избражений
  aSSDefaultConditionsForDatatype: Array[TPicPropDatatype] of TSimpleSearchCondition = (
    sscStrContains,   // ppdtString
    sscNumberEqual,   // ppdtInteger
    sscNumberEqual,   // ppdtFloat
    sscDateTimeEqual, // ppdtDate
    sscDateTimeEqual, // ppdtTime
    sscInvalid,       // ppdtBoolean
    sscListAny,       // ppdtList
    sscInvalid,       // ppdtSize
    sscInvalid,       // ppdtPixelFormat
    sscInvalid,       // ppdtRotation
    sscInvalid);      // ppdtFlips
   // Выражения для условий (%0:s - свойство; %1:s - значение)
  aSSConditionExpressions: Array[TSimpleSearchCondition] of String = (
    '',                                  // sscInvalid
    '%0:s<%1:s',                         // sscNumberLess
    '%0:s<=%1:s',                        // sscNumberLessEqual
    '%0:s=%1:s',                         // sscNumberEqual
    '%0:s<>%1:s',                        // sscNumberNotEqual
    '%0:s>=%1:s',                        // sscNumberGreaterEqual
    '%0:s>%1:s',                         // sscNumberGreater
    '%0:s<>''''',                        // sscStrSpecified
    '%0:s=''''',                         // sscStrNotSpecified
    '%0:s startsWith ''%1:s''',          // sscStrStarts
    'not (%0:s startsWith ''%1:s'')',    // sscStrNotStarts
    '%0:s=''%1:s''',                     // sscStrEqual
    '%0:s<>''%1:s''',                    // sscStrNotEqual
    '%0:s endsWith ''%1:s''',            // sscStrEnds
    'not (%0:s endsWith ''%1:s'')',      // sscStrNotEnds
    '%0:s contains ''%1:s''',            // sscStrContains
    'not (%0:s contains ''%1:s'')',      // sscStrNotContains
    '<masks not implemented>',           // sscStrMatchesMask
    '<masks not implemented>',           // sscStrNotMatchesMask
    '%0:s<>''''',                        // sscDateTimeSpecified
    '%0:s=''''',                         // sscDateTimeNotSpecified
    '%0:s<''%1:s''',                     // sscDateTimeLess
    '%0:s<=''%1:s''',                    // sscDateTimeLessEqual
    '%0:s=''%1:s''',                     // sscDateTimeEqual
    '%0:s<>''%1:s''',                    // sscDateTimeNotEqual
    '%0:s>=''%1:s''',                    // sscDateTimeGreaterEqual
    '%0:s>''%1:s''',                     // sscDateTimeGreater
    'not (isEmpty %0:s)',                // sscListSpecified
    'isEmpty %0:s',                      // sscListNotSpecified
    '<list operators not implemented>',  // sscListAny
    '<list operators not implemented>',  // sscListNone
    '<list operators not implemented>'); // sscListAll

   // Индексы столбцов tvSimpleCriteria
  IColIdx_Simple_Property  = 0;
  IColIdx_Simple_Condition = 1;
  IColIdx_Simple_Value     = 2;

  function DoSearch(AApp: IPhotoAlbumApp; ResultsGroup: IPhotoAlbumPicGroup): Boolean;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
{$R *.dfm}
uses
  TypInfo, StrUtils, Mask, ToolEdit,
  phPhoa, phUtils, ConsVars, udSelKeywords, phSettings, phMsgBox,
  phParsingPicFilter, Main;

var
   // Списки всех упоминаемых мест, номеров плёнок, авторов, носителей
  SLPhoaPlaces:        TStringList;
  SLPhoaFilmNumbers:   TStringList;
  SLPhoaAuthors:       TStringList;
  SLPhoaMedia:         TStringList;
   // Последнее использованное для поиска выражение
  sSearchExpression:   String;

  function DoSearch(AApp: IPhotoAlbumApp; ResultsGroup: IPhotoAlbumPicGroup): Boolean;
  begin
    with TdSearch.Create(Application) do
      try
        FApp          := AApp;
        FResultsGroup := ResultsGroup;
        Result := ExecuteModal(False, True);
      finally
        Free;
      end;
  end;

   // Возвращает [локализованное] наименование условия
  function GetSimpleConditionName(c: TSimpleSearchCondition): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TSimpleSearchCondition), Byte(c)));
  end;

   // Возвращает наименование элемента типа TSimpleSearchCondition в виде строки
  function SimpleConditionTypeToStr(Condition: TSimpleSearchCondition): String;
  begin
    Result := GetEnumName(TypeInfo(TSimpleSearchCondition), Byte(Condition));
  end;

   // Возвращает элемент типа TSimpleSearchCondition по его строковому наименованию. Если такого наименования не
   //   найдено, возвращает sscInvalid
  function StrToSimpleConditionType(const sCondition: String): TSimpleSearchCondition;
  begin
    Result := TSimpleSearchCondition(GetEnumValue(TypeInfo(TSimpleSearchCondition), sCondition));
    if not (Result in [Low(Result)..High(Result)]) then Result := sscInvalid;
  end;

   //===================================================================================================================
   // TSimpleSearchCriterion
   //===================================================================================================================

  procedure TSimpleSearchCriterion.AdjustPicProperty;
  begin
    FDatatype := aPicPropDatatype[FPicProperty];
    Condition := aSSDefaultConditionsForDatatype[FDatatype];
    Value := Null;
  end;

  constructor TSimpleSearchCriterion.Create;
  begin
    inherited Create;
    FPicProperty := ppID;
    AdjustPicProperty;
  end;

  procedure TSimpleSearchCriterion.FinalizeSearch;
  begin
    FValue_String := ''; // Release memory
    FreeAndNil(FValue_Masks);
    FValue_Keywords := nil;
  end;

  function TSimpleSearchCriterion.GetAsExpression: String;
  begin
    Result := Format(aSSConditionExpressions[FCondition], ['$'+PicPropToStr(FPicProperty, True), ValueStr]);
  end;

  function TSimpleSearchCriterion.GetConditionName: String;
  begin
    Result := GetSimpleConditionName(FCondition);
  end;

  function TSimpleSearchCriterion.GetDataString: String;
  begin
    Result := Format(
      '%s,%s,%s',
      [PicPropToStr(FPicProperty, True), SimpleConditionTypeToStr(FCondition), ValueStr]);
  end;

  function TSimpleSearchCriterion.GetPicPropertyName: String;
  begin
    Result := PicPropName(FPicProperty);
  end;

  function TSimpleSearchCriterion.GetValueStr: String;
  begin
    Result := '';
    if not VarIsNull(FValue) then
      case FDatatype of
        ppdtString,
          ppdtList,
          ppdtInteger,
          ppdtFloat: Result := FValue;
        ppdtDate:    Result := DateToStr(PhoaDateToDate(FValue), AppFormatSettings);
        ppdtTime:    Result := TimeToStr(PhoaTimeToTime(FValue), AppFormatSettings);
      end;
  end;

  procedure TSimpleSearchCriterion.InitializeSearch;
  var bNull: Boolean;
  begin
    bNull := VarIsNull(FValue);
    case FDatatype of
      ppdtString:
        if FCondition in SSCStringConditions_Pattern then
          FValue_String := ValueStr
        else if FCondition in SSCStringConditions_Mask then begin
          FreeAndNil(FValue_Masks);
          if not bNull then FValue_Masks := TPhoaMasks.Create(FValue);
        end;
      ppdtInteger,
        ppdtDate,
        ppdtTime: if bNull then FValue_Integer := 0 else FValue_Integer := FValue;
      ppdtFloat:  if bNull then FValue_Float   := 0 else FValue_Float   := FValue;
      ppdtList:
        if bNull then
          FValue_Keywords := nil
        else begin
          FValue_Keywords := NewPhotoAlbumKeywordList;
          FValue_Keywords.CommaText := FValue;
        end;
    end;
  end;

  function TSimpleSearchCriterion.Matches(Pic: IPhoaPic): Boolean;
  var vProp: Variant;

    procedure InvalidDatatype;
    begin
      PhoaException('Invalid or incompatible condition or datatype');
    end;

    function TestString: Boolean;
    var sProp: String;
    begin
      Result := False;
       // Проверяем на условие без учёта отрицания
      sProp := VarToStr(vProp);
      case FCondition of
        sscStrSpecified, sscStrNotSpecified:     Result := sProp<>'';
        sscStrStarts, sscStrNotStarts:           Result := AnsiStartsText(FValue_String, sProp);
        sscStrEqual, sscStrNotEqual:             Result := AnsiSameText(FValue_String, sProp);
        sscStrEnds, sscStrNotEnds:               Result := AnsiEndsText(FValue_String, sProp);
        sscStrContains, sscStrNotContains:       Result := AnsiContainsText(sProp, FValue_String);
        sscStrMatchesMask, sscStrNotMatchesMask: Result := (FValue_Masks=nil) or FValue_Masks.Matches(sProp);
        else InvalidDatatype;
      end;
       // Если отрицательное условие - инвертируем результат сравнения
      if FCondition in [sscStrNotSpecified, sscStrNotStarts, sscStrNotEqual, sscStrNotEnds, sscStrNotContains, sscStrNotMatchesMask] then
        Result := not Result;
    end;

    function TestInteger: Boolean;
    var iProp: Integer;
    begin
      Result := False;
      if not VarIsNull(vProp) then begin
        iProp := vProp;
        case FCondition of
          sscNumberLess:         Result := iProp< FValue_Integer;
          sscNumberLessEqual:    Result := iProp<=FValue_Integer;
          sscNumberEqual:        Result := iProp= FValue_Integer;
          sscNumberNotEqual:     Result := iProp<>FValue_Integer;
          sscNumberGreaterEqual: Result := iProp>=FValue_Integer;
          sscNumberGreater:      Result := iProp> FValue_Integer;
          else InvalidDatatype;
        end;
      end;
    end;

    function TestFloat: Boolean;
    var dProp: Double;
    begin
      Result := False;
      if not VarIsNull(vProp) then begin
        dProp := vProp;
        case FCondition of
          sscNumberLess:         Result := dProp< FValue_Float;
          sscNumberLessEqual:    Result := dProp<=FValue_Float;
          sscNumberEqual:        Result := dProp= FValue_Float;
          sscNumberNotEqual:     Result := dProp<>FValue_Float;
          sscNumberGreaterEqual: Result := dProp>=FValue_Float;
          sscNumberGreater:      Result := dProp> FValue_Float;
          else InvalidDatatype;
        end;
      end;
    end;

    function TestDateTime: Boolean;
    var iProp: Integer;
    begin
      Result := False;
      if VarIsNull(vProp) then iProp := 0 else iProp := vProp;
      case FCondition of
        sscDateTimeSpecified:    Result := not VarIsNull(vProp);
        sscDateTimeNotSpecified: Result := VarIsNull(vProp);
        sscDateTimeLess:         Result := iProp< FValue_Integer;
        sscDateTimeLessEqual:    Result := iProp<=FValue_Integer;
        sscDateTimeEqual:        Result := iProp= FValue_Integer;
        sscDateTimeNotEqual:     Result := iProp<>FValue_Integer;
        sscDateTimeGreaterEqual: Result := iProp>=FValue_Integer;
        sscDateTimeGreater:      Result := iProp> FValue_Integer;
        else InvalidDatatype;
      end;
    end;

    function TestList: Boolean;
    var
      PicKeywords: IPhoaKeywordList;
      i: Integer;
    begin
      Result := False;
      PicKeywords := Pic.Keywords;
      case FCondition of
        sscListSpecified:    Result := PicKeywords.Count>0;
        sscListNotSpecified: Result := PicKeywords.Count=0;
        sscListAny, sscListNone, sscListAll:
           // Если список (значение критерия) не задан
          if (FValue_Keywords=nil) or (FValue_Keywords.Count=0) then
            Result := FCondition=sscListNone
           // Иначе проверяем каждое слово
          else begin 
            Result := FCondition in [sscListNone, sscListAll];
            for i := 0 to FValue_Keywords.Count-1 do
              if (PicKeywords.IndexOf(FValue_Keywords[i])>=0) xor (Condition=sscListAll) then begin
                Result := not Result;
                Break;
              end;
          end;
        else InvalidDatatype;
      end;
    end;

  begin
    vProp := Pic.PropValues[FPicProperty];
    Result := False;
    case FDatatype of
      ppdtString:  Result := TestString;
      ppdtInteger: Result := TestInteger;
      ppdtFloat:   Result := TestFloat;
      ppdtDate,
        ppdtTime:  Result := TestDateTime;
      ppdtList:    Result := TestList;
      else InvalidDatatype;
    end;
  end;

  procedure TSimpleSearchCriterion.ObtainConditionStrings(Strings: TStrings);
  var
    c: TSimpleSearchCondition;
    PossibleConditions: TSimpleSearchConditions;
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
       // Перебираем условия, занося в Strings подходящие под Datatype
      PossibleConditions := aSSConditionsByDatatype[FDatatype];
      for c := Low(c) to High(c) do
        if c in PossibleConditions then Strings.AddObject(GetSimpleConditionName(c), Pointer(c));
    finally
      Strings.EndUpdate;
    end;
  end;

  procedure TSimpleSearchCriterion.SetCondition(Value: TSimpleSearchCondition);
  begin
    if FCondition<>Value then begin
      if not (Value in aSSConditionsByDatatype[FDatatype]) then
        PhoaException(
          'Condition %s is incompatible with datatype %s',
          [GetEnumName(TypeInfo(TSimpleSearchCondition), Byte(Value)),
           GetEnumName(TypeInfo(TPicPropDatatype), Byte(FDatatype))]);
      FCondition := Value;
    end;
  end;

  procedure TSimpleSearchCriterion.SetDataString(const Value: String);
  var
    s: String;
    pp: TPicProperty;
    c: TSimpleSearchCondition;
  begin
    s := Value;
     // Извлекаем свойство
    pp := StrToPicProp(ExtractFirstWord(s, ','), False);
    if pp in [Low(pp)..High(pp)] then begin
      PicProperty := pp;
       // Извлекаем условие
      c := StrToSimpleConditionType(ExtractFirstWord(s, ','));
      if (c<>sscInvalid) and (c in aSSConditionsByDatatype[FDatatype]) then begin
        Condition := c;
         // Присваиваем значение
        ValueStr := s; 
      end;
    end;
  end;

  procedure TSimpleSearchCriterion.SetPicProperty(Value: TPicProperty);
  begin
    if FPicProperty<>Value then begin
      if aSSConditionsByDatatype[aPicPropDatatype[Value]]=[] then
        PhoaException('Property "%s" isn''t a search property', [PicPropName(Value)]);
      FPicProperty := Value;
      AdjustPicProperty;
    end;
  end;

  procedure TSimpleSearchCriterion.SetValueStr(const sValue: String);
  begin
     // Если строка пустая, или для даты/времени не введено ни одной цифры - заносим Null 
    if (sValue='') or ((FDatatype in [ppdtDate, ppdtTime]) and (LastDelimiter('0123456789', sValue)=0)) then
      FValue := Null
     // Иначе преобразуем занчение к правильному типу  
    else
      case FDatatype of
        ppdtString,
          ppdtList:  FValue := sValue;
        ppdtInteger: FValue := StrToInt(sValue);
        ppdtFloat:   FValue := StrToFloat(sValue, AppFormatSettings);
        ppdtDate:    FValue := DateToPhoaDate(StrToDate(sValue, AppFormatSettings));
        ppdtTime:    FValue := TimeToPhoaTime(StrToTime(sValue, AppFormatSettings));
      end;
  end;

   //===================================================================================================================
   // TSimpleSearchCriterionList
   //===================================================================================================================

  function TSimpleSearchCriterionList.Add(Item: TSimpleSearchCriterion): Integer;
  begin
    Result := FList.Add(Item);
  end;

  procedure TSimpleSearchCriterionList.Clear;
  begin
    FList.Clear;
  end;

  constructor TSimpleSearchCriterionList.Create;
  begin
    inherited Create;
    FList := TObjectList.Create(True);
  end;

  procedure TSimpleSearchCriterionList.Delete(Index: Integer);
  begin
    FList.Delete(Index);
  end;

  destructor TSimpleSearchCriterionList.Destroy;
  begin
    FList.Free;
    inherited Destroy;
  end;

  procedure TSimpleSearchCriterionList.FinalizeSearch;
  var i: Integer;
  begin
    for i := 0 to FList.Count-1 do TSimpleSearchCriterion(FList[i]).FinalizeSearch;
  end;

  function TSimpleSearchCriterionList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TSimpleSearchCriterionList.GetItems(Index: Integer): TSimpleSearchCriterion;
  begin
    Result := TSimpleSearchCriterion(FList[Index]);
  end;

  procedure TSimpleSearchCriterionList.InitializeSearch;
  var i: Integer;
  begin
    for i := 0 to FList.Count-1 do TSimpleSearchCriterion(FList[i]).InitializeSearch;
  end;

  function TSimpleSearchCriterionList.Matches(Pic: IPhoaPic): Boolean;
  var i: Integer;
  begin
    Result := True;
    for i := 0 to FList.Count-1 do
      if not TSimpleSearchCriterion(FList[i]).Matches(Pic) then begin
        Result := False;
        Break;
      end;
  end;

  procedure TSimpleSearchCriterionList.RegLoad(rif: TRegIniFile; const sSection: String);
  var
    SL: TStringList;
    Crit: TSimpleSearchCriterion;
    i: Integer;
  begin
    FList.Clear;
     // Загружаем секцию критериев в список строк 
    SL := TStringList.Create;
    try
      rif.ReadSectionValues(sSection, SL);
       // Сортируем список - при этом критерии будут отсортированы по индексу
      SL.Sorted := True;
      for i := 0 to SL.Count-1 do begin
        Crit := TSimpleSearchCriterion.Create;
        try
          Crit.DataString := SL.ValueFromIndex[i];
          Add(Crit);
        except
          Crit.Free;
        end;
      end;
    finally
      SL.Free;
    end;
  end;

  procedure TSimpleSearchCriterionList.RegSave(rif: TRegIniFile; const sSection: String);
  var i: Integer;
  begin
     // Стираем секцию
    rif.EraseSection(sSection);
     // Записываем в цикле данные критериев
    for i := 0 to FList.Count-1 do
      rif.WriteString(sSection, Format('%.8d', [i]), TSimpleSearchCriterion(FList[i]).DataString);
  end;

   //===================================================================================================================
   // VirtualStringTree EditLink для редакторов свойств
   //===================================================================================================================
type
  TPicPropEditMoveDirection = (emdNone, emdEnter, emdLeft, emdRight, emdUp, emdDown);

  TSimpleCriterionEditLink = class(TInterfacedObject, IVTEditLink)
  private
     // Редактируемый критерий
    FCriterion: TSimpleSearchCriterion; 
     // One of the property editor classes
    FWControl: TWinControl;
     // A back reference to the tree calling
    FTree: TVirtualStringTree;
     // The node being edited
    FNode: PVirtualNode;
     // The column of the node being edited
    FColumn: Integer;
     // Used to capture some important messages regardless of the type of control we use
    FOldWControlWndProc: TWndMethod;
     // Флаг прерывания процесса редактирования
    FEndingEditing: Boolean;
     // Флаг запрета окончания редактирования, используется на время вызова диалогового окна выбора ключевых слов
    FPreserveEndEdit: Boolean;
     // Обработчик оконной процедуры контрола
    procedure WControlWindowProc(var Msg: TMessage);
     // Обработчики событий контрола
    procedure WKeywordButtonClick(Sender: TObject);
    procedure WControlExit(Sender: TObject);
    procedure WControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     // Преобразует константы VK_xxx в TPicPropEditMoveDirection
    function  VirtualKeyToDirection(VKey: Word): TPicPropEditMoveDirection;
     // Заканчивает редактирование (успешно) и сдвигает выделение в дереве при Direction<>emdNone, возвращает True, если
     //   удалось
    function  MoveSelection(Direction: TPicPropEditMoveDirection): Boolean;
  public
    constructor Create(ACriterion: TSimpleSearchCriterion);
    destructor Destroy; override;
     // IVTEditLink
    function  BeginEdit: Boolean; stdcall;
    function  CancelEdit: Boolean; stdcall;
    function  EndEdit: Boolean; stdcall;
    function  GetBounds: TRect; stdcall;
    function  PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  function TSimpleCriterionEditLink.BeginEdit: Boolean;
  begin
    Result := True;
    FWControl.Show;
    FWControl.SetFocus;
     // Set a window procedure hook (aka subclassing) to get notified about important messages
    FOldWControlWndProc := FWControl.WindowProc;
    FWControl.WindowProc := WControlWindowProc;
  end;

  function TSimpleCriterionEditLink.CancelEdit: Boolean;
  begin
    Result := True;
     // Restore the edit's window proc
    FWControl.WindowProc := FOldWControlWndProc;
    FWControl.Hide;
  end;

  constructor TSimpleCriterionEditLink.Create(ACriterion: TSimpleSearchCriterion);
  begin
    inherited Create;
    FCriterion := ACriterion;
  end;

  destructor TSimpleCriterionEditLink.Destroy;
  begin
    FWControl.Free;
    inherited Destroy;
  end;

  function TSimpleCriterionEditLink.EndEdit: Boolean;
  var s: String;
  begin
    FEndingEditing := True;
    Result := True;
     // Restore the edit's window proc
    FWControl.WindowProc := FOldWControlWndProc;
     // Редактировали условие
    if FColumn=1 then
      FCriterion.Condition := TSimpleSearchCondition(GetCurrentCBObject(FWControl as TComboBox))
     // Редактировали значение
    else begin
      case FCriterion.PicProperty of
        ppDate:     s := (FWControl as TDateEdit).Text;
        ppTime:     s := ChangeTimeSeparator((FWControl as TMaskEdit).Text, False);
        ppKeywords: s := (FWControl as TComboEdit).Text;
        else begin
          s := (FWControl as TComboBox).Text;
           // Сохраняем историю ввода
          if not (FCriterion.PicProperty in [ppPlace, ppFilmNumber, ppAuthor, ppMedia]) then
            RegSaveHistory(
              Format(SRegSearch_PropMRUFormat, [GetEnumName(TypeInfo(TPicProperty), Integer(FCriterion.PicProperty))]),
              TComboBox(FWControl),
              True);
        end;
      end;
      FCriterion.ValueStr := s;
    end;
    FTree.Invalidate; // Условие также влияет на доступность редактора значения
    FWControl.Hide;
  end;

  function TSimpleCriterionEditLink.GetBounds: TRect;
  begin
    Result := FWControl.BoundsRect;
  end;

  function TSimpleCriterionEditLink.MoveSelection(Direction: TPicPropEditMoveDirection): Boolean;
  var
    n: PVirtualNode;
    iCaretPos, iCol: Integer;
  begin
    Result := False;
    if FEndingEditing or FPreserveEndEdit then Exit;
     // ComboBox
    if FWControl is TComboBox then
      with TComboBox(FWControl) do begin
         // Если ComboBox раскрыт, ничего не двигаем
        if (Direction<>emdNone) and DroppedDown then Exit;
         // Сдвигаем выделение без вопросов, если DropDownList, просто закрытие или нажато вверх или вниз
        Result := (Style=csDropDownList) or (Direction in [emdNone, emdEnter, emdUp, emdDown]);
        iCaretPos := LongRec(Perform(CB_GETEDITSEL, 0, 0)).Hi;
      end
     // Прочие контролы (EDITs)
    else begin
       // Для DateEdit: если раскрыт Popup, ничего не двигаем
      if (Direction<>emdNone) and (FWControl is TDateEdit) and TDateEdit(FWControl).PopupVisible then Exit;
       // Сдвигаем выделение без вопросов, если просто закрытие или нажато вверх или вниз
      Result := Direction in [emdNone, emdEnter, emdUp, emdDown];
      iCaretPos := LongRec(FWControl.Perform(EM_GETSEL, 0, 0)).Hi;
       // В TCustomMaskEdit при наличии маски всегда есть выделение в 1 символ длиной
      if (FWControl is TCustomMaskEdit) and (TMaskEdit(FWControl).EditMask<>'') then Dec(iCaretPos);
    end;
     // Если сдвиг влево или вправо - разрешаем в том случае, если курсор у соответствующего края текста
    if not Result then
      case Direction of
        emdLeft:  Result := iCaretPos=0;
        emdRight: Result := iCaretPos=FWControl.Perform(WM_GETTEXTLENGTH, 0, 0);
      end;
     // Сдвигаем выделение
    if Result then
      with FTree do begin
        n := FocusedNode;
        iCol := FocusedColumn;
         // Определяем узел (строку) и столбец, куда двигать выделение
        case Direction of
          emdLeft:  Dec(iCol);
          emdRight: Inc(iCol);
          emdUp:    n := GetPrevious(n);
          emdDown:  n := GetNext(n);
        end;
         // Если всё в допустимых пределах - двигаем
        if (n<>nil) and (iCol>=0) and (iCol<Header.Columns.Count) then begin
          FEndingEditing := True;
          EndEditNode;
          if CanFocus then SetFocus;
          FocusedNode   := n;
          FocusedColumn := iCol;
          Selected[n] := True;
        end;
      end;
  end;

  function TSimpleCriterionEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

     // Создаёт новый комбобокс. Если bCondition=True, то для выбора условия критерия, иначе для ввода значения
    function NewCombobox(bCondition: Boolean): TComboBox;
    begin
      Result := TComboBox.Create(nil);
      with Result do begin
        Visible       := False;
        Parent        := Tree;
        DropDownCount := 16;
        OnExit        := WControlExit;
        OnKeyDown     := WControlKeyDown;
        if bCondition then begin
          Style       := csDropDownList;
          FCriterion.ObtainConditionStrings(Items);
          SetCurrentCBObject(Result, Byte(FCriterion.Condition));
        end else begin
           // Загружаем списки возможных вариантов или истории
          case FCriterion.PicProperty of
            ppPlace:      Items.Assign(SLPhoaPlaces);
            ppFilmNumber: Items.Assign(SLPhoaFilmNumbers);
            ppAuthor:     Items.Assign(SLPhoaAuthors);
            ppMedia:      Items.Assign(SLPhoaMedia);
            else
              RegLoadHistory(
                Format(SRegSearch_PropMRUFormat, [GetEnumName(TypeInfo(TPicProperty), Integer(FCriterion.PicProperty))]),
                Result,
                False);
          end;
          Text := FCriterion.ValueStr;
        end;
      end;
    end;

     // Создаёт новый DateEdit
    function NewDateEdit: TDateEdit;
    begin
      Result := TDateEdit.Create(nil);
      with Result do begin
        Visible    := False;
        Parent     := Tree;
        BlanksChar := '_';
        Text       := FCriterion.ValueStr;
        OnExit     := WControlExit;
        OnKeyDown  := WControlKeyDown;
        if Date=0 then Clear;
      end;
    end;

     // Создаёт новый MaskEdit для ввода времени
    function NewTimeEdit: TMaskEdit;
    begin
      Result := TMaskEdit.Create(nil);
      with Result do begin
        Visible    := False;
        Parent     := Tree;
        EditMask   := '!99:99:99;1;_';
        MaxLength  := 8;
        Text       := ChangeTimeSeparator(FCriterion.ValueStr, True);
        OnExit     := WControlExit;
        OnKeyDown  := WControlKeyDown;
      end;
    end;

     // Создаёт новый ComboEdit для ввода ключевых слов
    function NewKeywordEdit: TComboEdit;
    begin
      Result := TComboEdit.Create(nil);
      with Result do begin
        Visible       := False;
        Parent        := Tree;
        GlyphKind     := gkEllipsis;
        Text          := FCriterion.ValueStr;
        OnButtonClick := WKeywordButtonClick;
        OnExit        := WControlExit;
        OnKeyDown     := WControlKeyDown;
      end
    end;

  begin
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
     // Determine what edit type actually is needed
    FreeAndNil(FWControl);
    case Column of
       // *** Ячейка с условием критерия
      1: FWControl := NewCombobox(True);
       // *** Ячейка со значением критерия
      2:
        case FCriterion.Datatype of
          ppdtString,
            ppdtInteger,
            ppdtFloat: FWControl := NewCombobox(False);
          ppdtDate:    FWControl := NewDateEdit;
          ppdtTime:    FWControl := NewTimeEdit;
          ppdtList:    FWControl := NewKeywordEdit;
        end;
    end;
    Result := FWControl<>nil;
  end;

  procedure TSimpleCriterionEditLink.ProcessMessage(var Message: TMessage);
  begin
    FWControl.WindowProc(Message);
  end;

  procedure TSimpleCriterionEditLink.SetBounds(R: TRect);
  begin
    FTree.Header.Columns.GetColumnBounds(FColumn, R.Left, R.Right);
    FWControl.BoundsRect := R;
  end;

  function TSimpleCriterionEditLink.VirtualKeyToDirection(VKey: Word): TPicPropEditMoveDirection;
  begin
    case VKey of
      VK_LEFT:   Result := emdLeft;
      VK_RIGHT:  Result := emdRight;
      VK_UP:     Result := emdUp;
      VK_DOWN:   Result := emdDown;
      VK_RETURN: Result := emdEnter;
      else       Result := emdNone;
    end;
  end;

  procedure TSimpleCriterionEditLink.WControlExit(Sender: TObject);
  begin
    MoveSelection(emdNone);
  end;

  procedure TSimpleCriterionEditLink.WControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if Shift=[] then
      case Key of
        VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN: if MoveSelection(VirtualKeyToDirection(Key)) then Key := 0;
      end;
  end;

  procedure TSimpleCriterionEditLink.WControlWindowProc(var Msg: TMessage);
  begin
    case Msg.Msg of
      WM_GETDLGCODE: begin
        FOldWControlWndProc(Msg);
        Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
        Exit;
      end;
      WM_KEYDOWN:
        if ((GetKeyState(VK_SHIFT) or GetKeyState(VK_CONTROL) or GetKeyState(VK_MENU)) and $80=0) then
          case Msg.WParam of
            VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN: if MoveSelection(VirtualKeyToDirection(Msg.WParam)) then Exit;
          end;
      WM_KILLFOCUS: begin
        MoveSelection(emdNone);
        Exit;
      end;
    end;
    FOldWControlWndProc(Msg);
  end;

  procedure TSimpleCriterionEditLink.WKeywordButtonClick(Sender: TObject);
  var s: String;
  begin
    FPreserveEndEdit := True;
    try
      s := (Sender as TComboEdit).Text;
      if SelectPhoaKeywords((FTree.Owner as TdSearch).FApp.ProjectX, s) then TComboEdit(Sender).Text := s;
    finally
      FPreserveEndEdit := False;
    end;
  end;

   //===================================================================================================================
   // TfSearch
   //===================================================================================================================

  procedure TdSearch.aaSimpleConvertToExpression(Sender: TObject);
  var
    s: String;
    i: Integer;
  begin
    BeginUpdate;
    try
      s := '';
      for i := 0 to FSimpleCriteria.Count-1 do
        AccumulateStr(s, ' and'+S_CRLF, FSimpleCriteria[i].AsExpression);
      frExprPicFilter.Expression := s;
      pcCriteria.ActivePage := tsExpression;
      UpdateSearchKind;
    finally
      EndUpdate;
    end;
  end;

  procedure TdSearch.aaSimpleCrDelete(Sender: TObject);
  begin
    BeginUpdate;
    try
      tvSimpleCriteria.EndEditNode;
      FSimpleCriteria.Delete(tvSimpleCriteria.FocusedNode.Index);
      SyncSimpleCriteria;
    finally
      EndUpdate;
    end;
  end;

  procedure TdSearch.aaSimpleReset(Sender: TObject);
  begin
    FSimpleCriteria.Clear;
    SyncSimpleCriteria;
  end;

  procedure TdSearch.ButtonClick_OK;
  begin
    PerformSearch;
    if FLocalResults.Count=0 then
      PhoaInfo(False, 'SPicsNotFound')
    else begin
       // Fill search results
      FResultsGroup.PicsX.Assign(FLocalResults);
      inherited ButtonClick_OK;
    end;
  end;

  constructor TdSearch.Create(AOwner: TComponent);

    function NewSL: TStringList;
    begin
      Result := TStringList.Create;
      Result.Sorted     := True;
      Result.Duplicates := dupIgnore;
    end;

  begin
    inherited Create(AOwner);
    FSimpleCriteria   := TSimpleSearchCriterionList.Create;
    FLocalResults     := NewPhotoAlbumPicList(False);
     // Создаём списки мест, номеров плёнок, авторов
    SLPhoaPlaces      := NewSL;
    SLPhoaFilmNumbers := NewSL;
    SLPhoaAuthors     := NewSL;
    SLPhoaMedia       := NewSL;
  end;

  destructor TdSearch.Destroy;
  begin
    FLocalResults := nil;
    FSimpleCriteria.Free;
    SLPhoaPlaces.Free;
    SLPhoaFilmNumbers.Free;
    SLPhoaAuthors.Free;
    SLPhoaMedia.Free;
    inherited Destroy;
  end;

  procedure TdSearch.DoCreate;

     // Создаёт в pmSimple пункты выбора свойства изображения
    procedure CreateSimpleCrPicPropItems;
    var pp: TPicProperty;
    begin
      for pp := Low(pp) to High(pp) do
         // Если тип данных свойства поддерживает поиск
        if aSSConditionsByDatatype[aPicPropDatatype[pp]]<>[] then
          AddTBXMenuItem(ipmsmSimpleProp, PicPropName(pp), -1, Byte(pp), SimpleCrPicPropClick);
    end;

  begin
    inherited DoCreate;
    HelpContext := IDH_intf_search;
     // Создаём в pmSimple пункты выбора свойства изображения
    CreateSimpleCrPicPropItems;
  end;

  procedure TdSearch.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
     // Загружаем списки мест, номеров плёнок, авторов
    StringsLoadPFAM(FApp.Project, SLPhoaPlaces, SLPhoaFilmNumbers, SLPhoaAuthors, SLPhoaMedia);
     // Настраиваем контролы
    rbCurGroup.Enabled      := (FApp.CurGroup<>nil) and (FApp.CurGroup.Pics.Count>0);
    rbSearchResults.Enabled := FResultsGroup.Pics.Count>0;
     // Инициализируем дерево / pmSimple
    ApplyTreeSettings(tvSimpleCriteria);
     // Поиск по выражению
    frExprPicFilter.Expression := sSearchExpression; 
  end;

  function TdSearch.GetRelativeRegistryKey: String;
  begin
    Result := SRegSearch_Root;
  end;

  function TdSearch.GetSimpleCriterion(Node: PVirtualNode): TSimpleSearchCriterion;
  begin
    if (Node=nil) or (Integer(Node.Index)>=FSimpleCriteria.Count) then
      Result := nil
    else
      Result := FSimpleCriteria[Node.Index];
  end;

  function TdSearch.GetSizeable: Boolean;
  begin
    Result := True;
  end;

  procedure TdSearch.pcCriteriaChange(Sender: TObject);
  begin
    UpdateSearchKind;
    case SearchKind of
      pskSimple:     tvSimpleCriteria.SetFocus;
      pskExpression: frExprPicFilter.FocusEditor;
    end;
    StateChanged;
  end;

  procedure TdSearch.PerformSearch;
  type TSearchArea = (saAll, saCurGroup, saResults);
  var
    i, iSrchCount: Integer;
    SearchArea: TSearchArea;
    Pic: IPhotoAlbumPic;
    PicFilter: IPhoaParsingPicFilter;

     // Возвращает True, если изображение подходит под указанные критерии
    function Matches(Pic: IPhotoAlbumPic): Boolean;
    begin
      case SearchKind of
         // Простой поиск
        pskSimple: Result := FSimpleCriteria.Matches(Pic);
         // Поиск по выражению
        pskExpression:
          try
            Result := PicFilter.Matches(Pic);
          except
            on e: EPhoaParseError do begin
              frExprPicFilter.CaretPos := PicFilter.ParseErrorLocation;
              frExprPicFilter.FocusEditor;
              raise;
            end;
          end;
        else Result := False;          
      end;
    end;

     // Инициализирует поиск
    procedure InitializeSearch;
    begin
      case SearchKind of
         // Простой поиск
        pskSimple: FSimpleCriteria.InitializeSearch;
         // Поиск по выражению
        pskExpression: begin
          sSearchExpression := frExprPicFilter.Expression;
          PicFilter := NewPhoaParsingPicFilter;
          PicFilter.Expression := sSearchExpression;
        end;
      end;
    end;

     // Финализирует поиск
    procedure FinalizeSearch;
    begin
      case SearchKind of
         // Простой поиск
        pskSimple: FSimpleCriteria.FinalizeSearch;
         // Поиск по выражению
        pskExpression: PicFilter := nil;
      end;
    end;

  begin
    StartWait;
    try
       // Инициализируем поиск
      try
        InitializeSearch;
         // Настраиваем область поиска
        if rbAll.Checked then begin
          SearchArea := saAll;
          iSrchCount := FApp.Project.Pics.Count;
        end else if rbCurGroup.Checked then begin
          SearchArea := saCurGroup;
          iSrchCount := FApp.CurGroup.Pics.Count;
        end else begin
          SearchArea := saResults;
          iSrchCount := FResultsGroup.Pics.Count;
        end;
         // Ищем
        FLocalResults.Clear;
        for i := 0 to iSrchCount-1 do begin
          case SearchArea of
            saAll:      Pic := FApp.ProjectX.PicsX[i];
            saCurGroup: Pic := FApp.CurGroupX.PicsX[i];
            else        Pic := FResultsGroup.PicsX[i];
          end;
          if Matches(Pic) then FLocalResults.Add(Pic, True);
        end;
      finally
        FinalizeSearch;
      end;
    finally
      StopWait;
    end;
  end;

  procedure TdSearch.pmSimplePopup(Sender: TObject);
  var
    i: Integer;
    Crit: TSimpleSearchCriterion;
  begin
     // Ставим отметку на пункте, соответствующем свойству текущего критерия
    Crit := GetSimpleCriterion(tvSimpleCriteria.FocusedNode);
    if Crit<>nil then
      for i := 0 to ipmsmSimpleProp.Count-1 do
        with ipmsmSimpleProp[i] do Checked := TPicProperty(Tag)=Crit.PicProperty;
  end;

  procedure TdSearch.SettingsLoad(rif: TRegIniFile);
  begin
    inherited SettingsLoad(rif);
    pcCriteria.ActivePageIndex := rif.ReadInteger('', 'LastCriteriaPageIndex', 0);
     // Загружаем список критериев
    FSimpleCriteria.RegLoad(rif, SRegSearch_SimpleCriteria);
    SyncSimpleCriteria;
    ActivateFirstVTNode(tvSimpleCriteria);
     // Обновляем состояние
    UpdateSearchKind;
  end;

  procedure TdSearch.SettingsSave(rif: TRegIniFile);
  begin
    inherited SettingsSave(rif);
    rif.WriteInteger('', 'LastCriteriaPageIndex', pcCriteria.ActivePageIndex);
    if ModalResult=mrOK then FSimpleCriteria.RegSave(rif, SRegSearch_SimpleCriteria);
  end;

  procedure TdSearch.SimpleCrPicPropClick(Sender: TObject);
  var
    n: PVirtualNode;
    Crit: TSimpleSearchCriterion;
  begin
     // Получаем текущий критерий
    n := tvSimpleCriteria.FocusedNode;
    Crit := GetSimpleCriterion(n);
     // Если nil - значит, pmSimple вызвано было для виртуальной строки. Создаём новый критерий
    if Crit=nil then begin
      Crit := TSimpleSearchCriterion.Create;
      FSimpleCriteria.Add(Crit);
    end;
     // Устанавливаем свойство критерия
    Crit.PicProperty := TPicProperty(TComponent(Sender).Tag);
     // Обновляем дерево
    SyncSimpleCriteria;
  end;

  procedure TdSearch.SyncSimpleCriteria;
  begin
    tvSimpleCriteria.RootNodeCount := FSimpleCriteria.Count+1; // Добавляем виртуальную "пустую" строку
    tvSimpleCriteria.Invalidate;
    StateChanged;
  end;

  procedure TdSearch.tvSimpleCriteriaBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var Crit: TSimpleSearchCriterion;
  begin
    if (Column=IColIdx_Simple_Value) and ((Sender.FocusedNode<>Node) or (Sender.FocusedColumn<>Column)) then begin
      Crit := GetSimpleCriterion(Node);
       // Закрашиваем серым ячейку значения для условий, не нуждающихся в значении
      if (Crit<>nil) and (Crit.Condition in SSCNoValueConditions) then
        with TargetCanvas do begin
          Brush.Color := clBtnFace;
          FillRect(CellRect);
        end;
    end;
  end;

  procedure TdSearch.tvSimpleCriteriaChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var p: TPoint;
  begin
    ActivateVTNode(Sender, Node);
    StateChanged;
    with Sender.GetDisplayRect(Node, -1, False) do p := Sender.ClientToScreen(Point(Left, Bottom));
    pmSimple.Popup(p.x, p.y);
  end;

  procedure TdSearch.tvSimpleCriteriaCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
  begin
    EditLink := TSimpleCriterionEditLink.Create(GetSimpleCriterion(Node));
  end;

  procedure TdSearch.tvSimpleCriteriaEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
  var Crit: TSimpleSearchCriterion;
  begin
    Crit := GetSimpleCriterion(Node);
    Allowed :=
       // Не в процессе инициализации
      not UpdateLocked and
       // Не виртуальная строка
      (Crit<>nil) and
       // Редактор есть у условия или у значения при соответствующем условии
      ((Column=IColIdx_Simple_Condition) or ((Column=IColIdx_Simple_Value) and not (Crit.Condition in SSCNoValueConditions)));
  end;

  procedure TdSearch.tvSimpleCriteriaFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
    StateChanged;
     // Редактируем ячейку, в которую входим
    Sender.EditNode(Node, Column);
  end;

  procedure TdSearch.tvSimpleCriteriaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    Crit: TSimpleSearchCriterion;
    s: String;
  begin
    s := '';
    Crit := GetSimpleCriterion(Node);
     // Виртуальная "пустая" строка
    if Crit=nil then begin
      if Column=IColIdx_Simple_Property then s := ConstVal('SMsg_SelectSearchPicProp');
     // Строка критерия
    end else
      case Column of
        IColIdx_Simple_Property:  s := Crit.PicPropertyName;
        IColIdx_Simple_Condition: s := Crit.ConditionName;
        IColIdx_Simple_Value:     if not (Crit.Condition in SSCNoValueConditions) then s := Crit.ValueStr;
      end;
    CellText := PhoaAnsiToUnicode(s);
  end;

  procedure TdSearch.tvSimpleCriteriaInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
     // У каждого узла есть кнопка
    Node.CheckType := ctButton;
  end;

  procedure TdSearch.tvSimpleCriteriaPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Текст виртуальной строки рисуем серым
    if GetSimpleCriterion(Node)=nil then TargetCanvas.Font.Color := clGrayText;
  end;

  procedure TdSearch.UpdateSearchKind;
  begin
    if pcCriteria.ActivePage=tsSimple then FSearchKind := pskSimple else FSearchKind := pskExpression;
  end;

  procedure TdSearch.UpdateState;
  var bSSimple, bCrExist: Boolean;
  begin
    inherited UpdateState;
    bSSimple  := SearchKind=pskSimple;
    bCrExist  := FSimpleCriteria.Count>0;
    aSimpleReset.Enabled               := bSSimple and bCrExist;
    aSimpleCrDelete.Enabled            := bSSimple and (GetSimpleCriterion(tvSimpleCriteria.FocusedNode)<>nil);
    aSimpleConvertToExpression.Enabled := bSSimple and bCrExist;
  end;

end.

