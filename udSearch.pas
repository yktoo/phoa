//**********************************************************************************************************************
//  $Id: udSearch.pas,v 1.22 2004-11-21 09:15:51 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Registry, Contnrs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, phPicFilterHighlighter,
  phDlg, SynCompletionProposal, DKLang, TB2Item, TBX, TB2Dock, TB2Toolbar,
  SynEdit, VirtualTrees, StdCtrls, ComCtrls, ExtCtrls;

type
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
     // Prop storage
    FPicProperty: TPicProperty;
    FCondition: TSimpleSearchCondition;
    FValue: Variant;
    FDatatype: TPicPropDatatype;
     // Настраивает поля, зависящие от PicProperty
    procedure AdjustPicProperty;
     // Prop handlers
    procedure SetPicProperty(Value: TPicProperty);
    procedure SetCondition(Value: TSimpleSearchCondition);
    function  GetPicPropertyName: String;
    function  GetConditionName: String;
    function  GetValueStr: String;
  public
    constructor Create;
     // Возвращает True, если изображение подходит под критерий
    function  Matches(Pic: IPhoaPic): Boolean;
     // Подготовка/финализация поиска
    procedure InitializeSearch;
    procedure FinalizeSearch; 
     // Props
     // -- Условие критерия
    property Condition: TSimpleSearchCondition read FCondition write SetCondition;
     // -- Наименование условия критерия
    property ConditionName: String read GetConditionName;
     // -- Тип данных PicProperty
    property Datatype: TPicPropDatatype read FDatatype;
     // -- Свойство изображения, проверяемое на условие
    property PicProperty: TPicProperty read FPicProperty write SetPicProperty;
     // -- Наименование свойства изображения
    property PicPropertyName: String read GetPicPropertyName;
     // -- Значение критерия
    property Value: Variant read FValue write FValue;
     // -- Значение критерия в виде строки
    property ValueStr: String read GetValueStr;
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
     // Props
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSimpleSearchCriterion read GetItems; default;
  end;
  
   //===================================================================================================================
   // TdSearch
   //===================================================================================================================

  TdSearch = class(TPhoaDialog)
    bReset: TButton;
    dkExprTop: TTBXDock;
    dklcMain: TDKLanguageController;
    eExpression: TSynEdit;
    gbSearch: TGroupBox;
    lCriteria: TLabel;
    pcCriteria: TPageControl;
    rbAll: TRadioButton;
    rbCurGroup: TRadioButton;
    rbSearchResults: TRadioButton;
    scpMain: TSynCompletionProposal;
    smExprInsertOperator: TTBXSubmenuItem;
    smExprInsertProp: TTBXSubmenuItem;
    tbExprMain: TTBXToolbar;
    tsExpression: TTabSheet;
    tsSimple: TTabSheet;
    tvSimpleCriteria: TVirtualStringTree;
    procedure bResetClick(Sender: TObject);
    procedure tvSimpleCriteriaCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure tvSimpleCriteriaFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvSimpleCriteriaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvSimpleCriteriaInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
     // Список критериев простого поиска
    FSimpleCriteria: TSimpleSearchCriterionList;
     // Локальный список результатов поиска (ссылок на изображения)
    FLocalResults: IPhoaMutablePicList;
     // Приложение
    FApp: IPhotoAlbumApp;
     // Группа, в которую помещать результаты
    FResultsGroup: IPhotoAlbumPicGroup;
     // Обновляет tvSimpleCriteria (в ответ на изменение FSimpleCriteria)
    procedure SyncSimpleCriteria;
     // Возвращает True, если Node - "настоящий" узел критерия (не nil и не последний "пустой" узел) 
    function  IsSimpleCritNode(Node: PVirtualNode): Boolean;
     // Основная процедура поиска
    procedure PerformSearch;
     // Сбрасывает критерии в первоначальное состояние
    procedure ResetCriteria;
     // Событие клика на пункте вставки свойства изображения в выражение
    procedure ExprInsertPropClick(Sender: TObject);
     // Событие клика на пункте вставки опреатора в выражение
    procedure ExprInsertOpClick(Sender: TObject);
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
    function  GetFormRegistrySection: String; override;
    function  GetSizeable: Boolean; override;
    procedure SettingsStore(rif: TRegIniFile); override;
    procedure SettingsRestore(rif: TRegIniFile); override;
  end;

const
   // Наборы условий критериев для типов данных свойств избражений
  SSCNumberConditions   = [
    sscNumberLess, sscNumberLessEqual, sscNumberEqual, sscNumberNotEqual, sscNumberGreaterEqual, sscNumberGreater];
  SSCStringConditions   = [
    sscStrSpecified, sscStrNotSpecified, sscStrStarts, sscStrNotStarts, sscStrEqual, sscStrNotEqual, sscStrEnds,
    sscStrNotEnds, sscStrContains, sscStrNotContains, sscStrMatchesMask, sscStrNotMatchesMask];
  SSCDateTimeConditions = [
    sscDateTimeSpecified, sscDateTimeNotSpecified, sscDateTimeLess, sscDateTimeLessEqual, sscDateTimeEqual,
    sscDateTimeNotEqual, sscDateTimeGreaterEqual, sscDateTimeGreater];
  SSCListConditions     = [sscListSpecified, sscListNotSpecified, sscListAny, sscListNone, sscListAll];
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

   // Индексы столбцов tvSimpleCriteria
  IColIdx_Simple_Property  = 0;
  IColIdx_Simple_Condition = 1;
  IColIdx_Simple_Value     = 2;

  function DoSearch(AApp: IPhotoAlbumApp; ResultsGroup: IPhotoAlbumPicGroup): Boolean;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
{$R *.dfm}
uses
  TypInfo, StrUtils, Mask, ToolEdit,
  phPhoa, phUtils, ConsVars, udSelKeywords, phSettings, udMsgBox,
  phParsingPicFilter, Main;

  function DoSearch(AApp: IPhotoAlbumApp; ResultsGroup: IPhotoAlbumPicGroup): Boolean;
  begin
    with TdSearch.Create(Application) do
      try
        FApp          := AApp;
        FResultsGroup := ResultsGroup;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TSimpleSearchCriterion
   //===================================================================================================================

  procedure TSimpleSearchCriterion.AdjustPicProperty;
  begin
    FDatatype  := aPicPropDatatype[FPicProperty];
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
  end;

  function TSimpleSearchCriterion.GetConditionName: String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TSimpleSearchCondition), Byte(FCondition)));
  end;

  function TSimpleSearchCriterion.GetPicPropertyName: String;
  begin
    Result := PicPropName(FPicProperty);
  end;

  function TSimpleSearchCriterion.GetValueStr: String;
  begin
    Result := VarToStr(FValue);
  end;

  procedure TSimpleSearchCriterion.InitializeSearch;
  begin
    case FDatatype of
      ppdtString:
        case FCondition of
          sscStrStarts,
            sscStrNotStarts,
            sscStrEqual,
            sscStrNotEqual,
            sscStrEnds,
            sscStrNotEnds,
            sscStrContains,
            sscStrNotContains:    FValue_String  := VarToStr(FValue);
          sscStrMatchesMask,   
            sscStrNotMatchesMask: if not VarIsNull(FValue) then FValue_Masks := TPhoaMasks.Create(FValue);
        end;
      ppdtInteger,
        ppdtDate,
        ppdtTime: if VarIsNull(FValue) then FValue_Integer := 0 else FValue_Integer := FValue;
      ppdtFloat:  if VarIsNull(FValue) then FValue_Float   := 0 else FValue_Float   := FValue;
      ppdtList:  {!!!???};
    end;
  end;

  function TSimpleSearchCriterion.Matches(Pic: IPhoaPic): Boolean;
  var vProp: Variant;

    procedure InvalidDatatype;
    begin
      PhoaException('Invalid or incompatible datatype');
    end;

    function TestString: Boolean;
    var sProp: String;
    begin
       // Проверяем на условие без учёта отрицания
      sProp := VarToStr(vProp);
      case FCondition of
        sscStrSpecified,
          sscStrNotSpecified:   Result := sProp<>'';
        sscStrStarts,
          sscStrNotStarts:      Result := AnsiStartsText(FValue_String, sProp);
        sscStrEqual,
          sscStrNotEqual:       Result := AnsiSameText(FValue_String, sProp);
        sscStrEnds,
          sscStrNotEnds:        Result := AnsiEndsText(FValue_String, sProp);
        sscStrContains,
          sscStrNotContains:    Result := AnsiContainsText(FValue_String, sProp);
        sscStrMatchesMask,
          sscStrNotMatchesMask: Result := (FValue_Masks=nil) or FValue_Masks.Matches(sProp);
        else InvalidDatatype;
      end;
       // Если отрицательное условие - инвертируем результат сравнения
      if FCondition in [sscStrNotSpecified, sscStrNotStarts, sscStrNotEqual, sscStrNotEnds, sscStrNotContains, sscStrNotMatchesMask] then
        Result := not Result;
    end;

    function TestInteger: Boolean;
    var iProp: Integer;
    begin
      if VarIsNull(vProp) then
        Result := False
      else begin
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
      if VarIsNull(vProp) then
        Result := False
      else begin
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
    begin

    end;

  begin
    vProp := Pic.PropValues[FPicProperty];
    Result := not VarIsNull(FValue) and not VarIsNull(vProp);
    if Result then
      case FDatatype of
        ppdtString:  Result := TestString;
        ppdtInteger: Result := TestInteger;
        ppdtFloat:   Result := TestFloat;
        ppdtDate:    Result := TestDate;
        ppdtTime:    Result := TestTime;
        ppdtList:    Result := TestList;
        else InvalidDatatype;
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

  procedure TSimpleSearchCriterion.SetPicProperty(Value: TPicProperty);
  begin
    if FPicProperty<>Value then begin
      if aSSConditionsByDatatype[aPicPropDatatype[Value]]=[] then
        PhoaException('Property "%s" isn''t a search property', [PicPropName(Value)]);
      FPicProperty := Value;
      AdjustPicProperty;
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

//var
//  aSearchCriteria: Array[0..17] of TSearchCriteriaEntry = (
//    {00}(CritType: sctInteger;   Prop: ppID;            sValue: ''; sHistKey: SRegSearch_IDMRU;       IntCond: ipcEqual;        IntDefCond: ipcEqual),
//    {01}(CritType: sctFileMasks; Prop: ppFileName;      sValue: ''; sHistKey: SRegSearch_FMaskMRU;    MskCond: mpcMatches;      MskDefCond: mpcMatches),
//    {02}(CritType: sctString;    Prop: ppFilePath;      sValue: ''; sHistKey: SRegSearch_FPathMRU;    StrCond: spcContains;     StrDefCond: spcContains),
//    {03}(CritType: sctInteger;   Prop: ppFileSizeBytes; sValue: ''; sHistKey: SRegSearch_FSizeMRU;    IntCond: ipcEqual;        IntDefCond: ipcEqual),
//    {04}(CritType: sctInteger;   Prop: ppPicWidth;      sValue: ''; sHistKey: SRegSearch_PWidthMRU;   IntCond: ipcEqual;        IntDefCond: ipcEqual),
//    {05}(CritType: sctInteger;   Prop: ppPicHeight;     sValue: ''; sHistKey: SRegSearch_PHeightMRU;  IntCond: ipcEqual;        IntDefCond: ipcEqual),
//    {06}(CritType: sctDate;      Prop: ppDate;          sValue: ''; sHistKey: '';                     DatCond: dpcGreaterEqual; DatDefCond: dpcGreaterEqual),
//    {07}(CritType: sctDate;      Prop: ppDate;          sValue: ''; sHistKey: '';                     DatCond: dpcLessEqual;    DatDefCond: dpcLessEqual),
//    {08}(CritType: sctTime;      Prop: ppTime;          sValue: ''; sHistKey: '';                     DatCond: dpcGreaterEqual; DatDefCond: dpcGreaterEqual),
//    {09}(CritType: sctTime;      Prop: ppTime;          sValue: ''; sHistKey: '';                     DatCond: dpcLessEqual;    DatDefCond: dpcLessEqual),
//    {10}(CritType: sctString;    Prop: ppPlace;         sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
//    {11}(CritType: sctString;    Prop: ppFilmNumber;    sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
//    {12}(CritType: sctString;    Prop: ppFrameNumber;   sValue: ''; sHistKey: SRegSearch_FrNumberMRU; StrCond: spcContains;     StrDefCond: spcContains),
//    {13}(CritType: sctString;    Prop: ppAuthor;        sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
//    {14}(CritType: sctString;    Prop: ppDescription;   sValue: ''; sHistKey: SRegSearch_DescMRU;     StrCond: spcContains;     StrDefCond: spcContains),
//    {15}(CritType: sctString;    Prop: ppNotes;         sValue: ''; sHistKey: SRegSearch_NotesMRU;    StrCond: spcContains;     StrDefCond: spcContains),
//    {16}(CritType: sctString;    Prop: ppMedia;         sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
//    {17}(CritType: sctKeywords;  Prop: ppKeywords;      sValue: ''; sHistKey: '';                     LstCond: lpcAny;          LstDefCond: lpcAny));
//   // Списки все упоминаемых мест, номеров плёнок, авторов, носителей
//  SLPhoaPlaces: TStringList;
//  SLPhoaFilmNumbers: TStringList;
//  SLPhoaAuthors: TStringList;
//  SLPhoaMedia: TStringList;
//  sSearchExpression: String;

//   // Функции, проверяющие соответствие условиям
//  function MatchesCondition(Condition: TIntPropCondition; iValue, iCritValue: Integer): Boolean; overload;
//  begin
//    Result := iCritValue<0;
//    if not Result then
//      case Condition of
//        ipcLess:         Result := iValue<iCritValue;
//        ipcLessEqual:    Result := iValue<=iCritValue;
//        ipcEqual:        Result := iValue=iCritValue;
//        ipcNotEqual:     Result := iValue<>iCritValue;
//        ipcGreaterEqual: Result := iValue>=iCritValue;
//        ipcGreater:      Result := iValue>iCritValue;
//      end;
//  end;
//
//  function MatchesCondition(Condition: TMskPropCondition; const sValue: String; Masks: TPhoaMasks): Boolean; overload;
//  begin
//    Result := (Condition=mpcMatches) = Masks.Matches(sValue);
//  end;
//
//  function MatchesCondition(Condition: TStrPropCondition; const sValue, sCritValue: String): Boolean; overload;
//  begin
//    case Condition of
//      spcSpecified:    Result := sValue<>'';
//      spcNotSpecified: Result := sValue='';
//      else begin
//        Result := sCritValue='';
//        if not Result then
//          case Condition of
//            spcStarts:      Result := AnsiStartsText(sCritValue, sValue);
//            spcNotStarts:   Result := not AnsiStartsText(sCritValue, sValue);
//            spcEqual:       Result := AnsiSameText(sValue, sCritValue);
//            spcNotEqual:    Result := not AnsiSameText(sValue, sCritValue);
//            spcEnds:        Result := AnsiEndsText(sCritValue, sValue);
//            spcNotEnds:     Result := not AnsiEndsText(sCritValue, sValue);
//            spcContains:    Result := AnsiContainsText(sValue, sCritValue);
//            spcNotContains: Result := not AnsiContainsText(sValue, sCritValue);
//          end;
//      end;
//    end;
//  end;
//
//  function MatchesCondition(Condition: TDatPropCondition; const iValue, iCritValue: Integer): Boolean; overload;
//  begin
//    case Condition of
//      dpcSpecified:    Result := iValue>=0;
//      dpcNotSpecified: Result := iValue<0;
//      else begin
//        Result := iCritValue<0;
//        if not Result then
//          case Condition of
//            dpcLess:         Result := iValue<iCritValue;
//            dpcLessEqual:    Result := iValue<=iCritValue;
//            dpcEqual:        Result := iValue=iCritValue;
//            dpcNotEqual:     Result := iValue<>iCritValue;
//            dpcGreaterEqual: Result := iValue>=iCritValue;
//            dpcGreater:      Result := iValue>iCritValue;
//          end;
//      end;
//    end;
//  end;
//
//  function MatchesCondition(Condition: TLstPropCondition; List, CritList: IPhoaKeywordList): Boolean; overload;
//  var i: Integer;
//  begin
//    case Condition of
//      lpcSpecified:    Result := List.Count>0;
//      lpcNotSpecified: Result := List.Count=0;
//      else begin
//        Result := CritList.Count=0;
//         // Проверяем каждое слово
//        if not Result then begin
//          Result := Condition in [lpcNone, lpcAll];
//          for i := 0 to CritList.Count-1 do
//            if (List.IndexOf(CritList[i])>=0) xor (Condition=lpcAll) then begin
//              Result := not Result;
//              Break;
//            end;
//        end;
//      end;
//    end;
//  end;

   //===================================================================================================================
   // VirtualStringTree EditLink для редакторов свойств
   //===================================================================================================================
type
  TPicPropEditMoveDirection = (emdNone, emdEnter, emdLeft, emdRight, emdUp, emdDown);

  TPicPropEditLink = class(TInterfacedObject, IVTEditLink)
  private
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

  function TPicPropEditLink.BeginEdit: Boolean;
  begin
    Result := True;
    FWControl.Show;
    FWControl.SetFocus;
     // Set a window procedure hook (aka subclassing) to get notified about important messages
    FOldWControlWndProc := FWControl.WindowProc;
    FWControl.WindowProc := WControlWindowProc;
  end;

  function TPicPropEditLink.CancelEdit: Boolean;
  begin
    Result := True;
     // Restore the edit's window proc
    FWControl.WindowProc := FOldWControlWndProc;
    FWControl.Hide;
  end;

  destructor TPicPropEditLink.Destroy;
  begin
    FWControl.Free;
    inherited Destroy;
  end;

  function TPicPropEditLink.EndEdit: Boolean;
  var
    psce: PSearchCriteriaEntry;
    d: TDateTime;
  begin
    FEndingEditing := True;
    Result := True;
     // Restore the edit's window proc
    FWControl.WindowProc := FOldWControlWndProc;
    psce := @aSearchCriteria[FNode.Index];
     // Редактировали условие
    if FColumn=1 then
      psce.IntCond := TIntPropCondition((FWControl as TComboBox).ItemIndex)
     // Редактировали значение
    else begin
      case psce.Prop of
        ppDate: begin
          if not CheckMaskedDateTime((FWControl as TDateEdit).Text, False, d) then d := -1;
          psce.sValue := iif(d<0, '', DateToStr(d));
        end;
        ppTime: begin
          if not CheckMaskedDateTime((FWControl as TMaskEdit).Text, True, d) then d := -1;
          psce.sValue := iif(d<0, '', TimeToStrX(d));
        end;
        ppKeywords: psce.sValue := (FWControl as TComboEdit).Text;
        else begin
          psce.sValue := (FWControl as TComboBox).Text;
           // Сохраняем историю
          if psce.sHistKey<>'' then RegSaveHistory(psce.sHistKey, TComboBox(FWControl), True);
        end;
      end;
    end;
    FTree.InvalidateNode(FNode);
    FWControl.Hide;
  end;

  function TPicPropEditLink.GetBounds: TRect;
  begin
    Result := FWControl.BoundsRect;
  end;

  function TPicPropEditLink.MoveSelection(Direction: TPicPropEditMoveDirection): Boolean;
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
        if (n<>nil) and (iCol>0) and (iCol<Header.Columns.Count) then begin
          FEndingEditing := True;
          EndEditNode;
          FocusedNode   := n;
          FocusedColumn := iCol;
          Selected[n] := True;
        end;
      end;
  end;

  function TPicPropEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
  var psce: PSearchCriteriaEntry;

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
          Items.Text  := GetConditionStrings(psce.CritType);
          ItemIndex   := Byte(psce.IntCond);
        end else begin
           // Загружаем списки возможных вариантов или истории
          if psce.sHistKey='' then
            case psce.Prop of
              ppPlace:      Items.Assign(SLPhoaPlaces);
              ppFilmNumber: Items.Assign(SLPhoaFilmNumbers);
              ppAuthor:     Items.Assign(SLPhoaAuthors);
              ppMedia:      Items.Assign(SLPhoaMedia);
            end
          else
            RegLoadHistory(psce.sHistKey, Result, False);
          Text        := psce.sValue;
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
        BlanksChar := #133;
        Text       := psce.sValue;
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
        EditMask   := '!99:99:99;1;…';
        MaxLength  := 8;
        Text       := psce.sValue;
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
        Text          := psce.sValue;
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
    psce := @aSearchCriteria[FNode.Index];
    case Column of
       // *** Ячейка с условием критерия
      1: FWControl := NewCombobox(True);
       // *** Ячейка со значением критерия
      2:
        case psce.CritType of
          sctInteger,
            sctFileMasks,
            sctString: FWControl := NewCombobox(False);
          sctDate:     FWControl := NewDateEdit;
          sctTime:     FWControl := NewTimeEdit;
          sctKeywords: FWControl := NewKeywordEdit;
        end;
    end;
    Result := FWControl<>nil;
  end;

  procedure TPicPropEditLink.ProcessMessage(var Message: TMessage);
  begin
    FWControl.WindowProc(Message);
  end;

  procedure TPicPropEditLink.SetBounds(R: TRect);
  begin
    FTree.Header.Columns.GetColumnBounds(FColumn, R.Left, R.Right);
    FWControl.BoundsRect := R;
  end;

  function TPicPropEditLink.VirtualKeyToDirection(VKey: Word): TPicPropEditMoveDirection;
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

  procedure TPicPropEditLink.WControlExit(Sender: TObject);
  begin
    MoveSelection(emdNone);
  end;

  procedure TPicPropEditLink.WControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if Shift=[] then
      case Key of
        VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN: if MoveSelection(VirtualKeyToDirection(Key)) then Key := 0;
      end;
  end;

  procedure TPicPropEditLink.WControlWindowProc(var Msg: TMessage);
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

  procedure TPicPropEditLink.WKeywordButtonClick(Sender: TObject);
  var s: String;
  begin
    FPreserveEndEdit := True;
    try
      s := (Sender as TComboEdit).Text;
      if SelectPhoaKeywords((FTree.Owner as TdSearch).FApp.Project, s) then TComboEdit(Sender).Text := s;
    finally
      FPreserveEndEdit := False;
    end;
  end;

   //===================================================================================================================
   // TfSearch
   //===================================================================================================================

  procedure TdSearch.bResetClick(Sender: TObject);
  begin
    ResetCriteria;
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

  procedure TdSearch.ExprInsertOpClick(Sender: TObject);
  begin
    eExpression.SelText := asPicFilterOperators[TPicFilterOperatorKind(TComponent(Sender).Tag)];
  end;

  procedure TdSearch.ExprInsertPropClick(Sender: TObject);
  begin
    eExpression.SelText := '$'+PicPropToStr(TPicProperty(TComponent(Sender).Tag), True);
  end;

  procedure TdSearch.FinalizeDialog;
  begin
    FLocalResults := nil;
    FSimpleCriteria.Free;
    SLPhoaPlaces.Free;
    SLPhoaFilmNumbers.Free;
    SLPhoaAuthors.Free;
    SLPhoaMedia.Free;
    inherited FinalizeDialog;
  end;

  function TdSearch.GetDataValid: Boolean;
  begin
    Result :=
      (pcCriteria.ActivePage=tsSimple) or
      ((pcCriteria.ActivePage=tsExpression) and (eExpression.Lines.Count>0)); 
  end;

  function TdSearch.GetFormRegistrySection: String;
  begin
    Result := SRegSearch_Root;
  end;

  function TdSearch.GetSizeable: Boolean;
  begin
    Result := True;
  end;

  procedure TdSearch.InitializeDialog;

    function NewSL: TStringList;
    begin
      Result := TStringList.Create;
      Result.Sorted     := True;
      Result.Duplicates := dupIgnore;
    end;

     // Добавляет к Menu новый пункт
    procedure AddTBXMenuItem(Menu: TTBCustomItem; const sCaption: String; iTag: Integer; const AOnClick: TNotifyEvent);
    var tbi: TTBCustomItem;
    begin
      tbi := TTBXItem.Create(Self);
      tbi.Caption := sCaption;
      tbi.Tag     := iTag;
      tbi.OnClick := AOnClick;
      Menu.Add(tbi);
    end;

     // Добавляет пункты меню "Вставить свойство" и для scpMain
    procedure AddExprInsertPropItems;
    var
      pp: TPicProperty;
      sProp: String;
    begin
      for pp := Low(pp) to High(pp) do begin
        sProp := '$'+PicPropToStr(pp, True);
        AddTBXMenuItem(
          smExprInsertProp,
          Format('%s - %s', [sProp, PicPropName(pp)]),
          Byte(pp),
          ExprInsertPropClick);
        scpMain.AddItem(sProp, sProp);  
      end;
    end;

    procedure AddExprInsertOperatorItems;
    var ok: TPicFilterOperatorKind;
    begin
      for ok := Low(ok) to High(ok) do
        AddTBXMenuItem(smExprInsertOperator, asPicFilterOperators[ok], Byte(ok), ExprInsertOpClick);
    end;

  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_search;
    OKIgnoresModified := True;
    FSimpleCriteria   := TSimpleSearchCriterionList.Create;
    FLocalResults     := NewPhotoAlbumPicList(False);
     // Загружаем списки мест, номеров плёнок, авторов
    SLPhoaPlaces      := NewSL;
    SLPhoaFilmNumbers := NewSL;
    SLPhoaAuthors     := NewSL;
    SLPhoaMedia       := NewSL;
    StringsLoadPFAM(FApp.Project, SLPhoaPlaces, SLPhoaFilmNumbers, SLPhoaAuthors, SLPhoaMedia);
     // Настраиваем контролы
    rbCurGroup.Enabled      := (FApp.CurGroup<>nil) and (FApp.CurGroup.Pics.Count>0);
    rbSearchResults.Enabled := FResultsGroup.Pics.Count>0;
     // Инициализируем дерево
    ApplyTreeSettings(tvCriteria);
    SyncSimpleCriteria;
     // Создаём пункты меню "Вставить свойство" и "Вставить оператор"
    AddExprInsertPropItems;
    AddExprInsertOperatorItems;
     // Инициализируем выражение
    scpMain.Font.Assign(Font);
    scpMain.TitleFont.Assign(Font);
    eExpression.Highlighter := TSynPicFilterSyn.Create(Self);
    eExpression.Text        := sSearchExpression;
  end;

  function TdSearch.IsSimpleCritNode(Node: PVirtualNode): Boolean;
  begin
    Result := (Node<>nil) and (Node.Index<FSimpleCriteria.Count);
  end;

  procedure TdSearch.PerformSearch;
  type
    TSearchArea = (saAll, saCurGroup, saResults);
    TSearchKind = (skSimple, skExpression);
  var
    iDate1, iDate2, iTime1, iTime2: Integer;
    Keywords: IPhotoAlbumKeywordList;
    FMasks: TPhoaMasks;
    i, iSrchCount, iID, iFSize, iPicWidth, iPicHeight: Integer;
    SearchArea: TSearchArea;
    Pic: IPhotoAlbumPic;
    SearchKind: TSearchKind;
    PicFilter: IPhoaParsingPicFilter;

     // Возвращает True, если изображение подходит под указанные критерии
    function Matches(Pic: IPhotoAlbumPic): Boolean;
    var i: Integer;
    begin
      case SearchKind of
         // Простой поиск
        skSimple:
          for i := 0 to High(aSearchCriteria) do begin
            with aSearchCriteria[i] do
              case i of
                ICritIdx_ID:        Result := MatchesCondition(IntCond, Pic.ID,                        iID);
                ICritIdx_FileSize:  Result := MatchesCondition(IntCond, Pic.FileSize,                  iFSize);
                ICritIdx_PicWidth:  Result := MatchesCondition(IntCond, Pic.ImageSize.cx,              iPicWidth);
                ICritIdx_PicHeight: Result := MatchesCondition(IntCond, Pic.ImageSize.cy,              iPicHeight);
                ICritIdx_FileMasks: Result := MatchesCondition(MskCond, ExtractFileName(Pic.FileName), FMasks);
                ICritIdx_Date1:     Result := MatchesCondition(DatCond, Pic.Date,                      iDate1);
                ICritIdx_Date2:     Result := MatchesCondition(DatCond, Pic.Date,                      iDate2);
                ICritIdx_Time1:     Result := MatchesCondition(DatCond, Pic.Time,                      iTime1);
                ICritIdx_Time2:     Result := MatchesCondition(DatCond, Pic.Time,                      iTime2);
                ICritIdx_Keywords:  Result := MatchesCondition(LstCond, Pic.Keywords,                  Keywords);
                else                Result := MatchesCondition(StrCond, Pic.PropStrValues[Prop],       sValue);
              end;
             // Если какой-то из критериев не выполнился, выходим (т.к. условие "И")
            if not Result then Break;
          end;
         // Поиск по выражению
        skExpression:
          try
            Result := PicFilter.Matches(Pic);
          except
            on e: EPhoaParseError do begin
              {!!! позиционировать курсор}
              raise;
            end;
          end;
        else Result := False;          
      end;
    end;

     // Преобразует строковое значение критерия в Дату PhoA; если не удалось, возвращает -1
    function GetCritDate(iCritIndex: Integer): Integer;
    var dt: TDateTime;
    begin
      dt := StrToDateDef(aSearchCriteria[iCritIndex].sValue, -1);
      if dt>=0 then Result := DateToPhoaDate(dt) else Result := -1;
    end;

     // Преобразует строковое значение критерия во Время PhoA; если не удалось, возвращает -1
    function GetCritTime(iCritIndex: Integer): Integer;
    var dt: TDateTime;
    begin
      dt := StrToTimeDef(aSearchCriteria[iCritIndex].sValue, -1);
      if dt>=0 then Result := TimeToPhoaTime(dt) else Result := -1;
    end;

     // Инициализирует поиск
    procedure InitializeSearch;
    begin
      FMasks := nil;
      case SearchKind of
         // Простой поиск
        skSimple: begin
           // Инициализируем критерии
          FMasks     := TPhoaMasks.Create(aSearchCriteria[ICritIdx_FileMasks].sValue);
          iID        := StrToIntDef(aSearchCriteria[ICritIdx_ID].sValue,        -1);
          iFSize     := StrToIntDef(aSearchCriteria[ICritIdx_FileSize].sValue,  -1);
          iPicWidth  := StrToIntDef(aSearchCriteria[ICritIdx_PicWidth].sValue,  -1);
          iPicHeight := StrToIntDef(aSearchCriteria[ICritIdx_PicHeight].sValue, -1);
          iDate1     := GetCritDate(ICritIdx_Date1);
          iDate2     := GetCritDate(ICritIdx_Date2);
          iTime1     := GetCritTime(ICritIdx_Time1);
          iTime2     := GetCritTime(ICritIdx_Time2);
          Keywords := NewPhotoAlbumKeywordList;
          Keywords.CommaText := aSearchCriteria[ICritIdx_Keywords].sValue;
        end;
         // Поиск по выражению
        skExpression: begin
          sSearchExpression := eExpression.Text;
          PicFilter := NewPhoaParsingPicFilter;
          PicFilter.Expression := sSearchExpression;
        end;
      end;
    end;

     // Финализирует поиск
    procedure FinalizeSearch;
    begin
      PicFilter := nil;
      FMasks.Free;
    end;

  begin
    StartWait;
    try
       // Определяем вид поиска
      if pcCriteria.ActivePage=tsSimple then SearchKind := skSimple else SearchKind := skExpression;
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
            saAll:      Pic := FApp.Project.PicsX[i];
            saCurGroup: Pic := FApp.CurGroup.PicsX[i];
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

  procedure TdSearch.ResetCriteria;
  begin
    FSimpleCriteria.Clear;
    SyncSimpleCriteria;
  end;

  procedure TdSearch.SettingsRestore(rif: TRegIniFile);
  begin
    inherited SettingsRestore(rif);
    pcCriteria.ActivePageIndex := rif.ReadInteger('', 'LastCriteriaPageIndex', 0);
  end;

  procedure TdSearch.SettingsStore(rif: TRegIniFile);
  begin
    inherited SettingsStore(rif);
    rif.WriteInteger('', 'LastCriteriaPageIndex', pcCriteria.ActivePageIndex);
  end;

  procedure TdSearch.SyncSimpleCriteria;
  begin
    tvSimpleCriteria.RootNodeCount := FSimpleCriteria.Count+1; // Добавляем виртуальную "пустую" строку
    tvSimpleCriteria.Invalidate;
    //!!!EnableActions;
  end;

  procedure TdSearch.tvSimpleCriteriaCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
  begin
    EditLink := TPicPropEditLink.Create;
  end;

  procedure TdSearch.tvSimpleCriteriaFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
     // Редактируем ячейку, в которую входим
    if (Node<>nil) and not (tsIncrementalSearching in Sender.TreeStates) then Sender.EditNode(Node, Column);
  end;

  procedure TdSearch.tvSimpleCriteriaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    Crit: TSimpleSearchCriterion;
    s: String;
  begin
    s := '';
    if IsSimpleCritNode(Node) then begin
      Crit := FSimpleCriteria[Node.Index];
      case Column of
        IColIdx_Simple_Property:  s := Crit.PicPropertyName;
        IColIdx_Simple_Condition: s := Crit.ConditionName;
        IColIdx_Simple_Value:     s := Crit.ValueStr;
      end;
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TdSearch.tvSimpleCriteriaInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
     // У каждого узла есть кнопка
    Node.CheckType := ctButton; 
  end;

end.

