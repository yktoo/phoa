//**********************************************************************************************************************
//  $Id: phParsingPicFilter.pas,v 1.5 2004-11-24 11:42:17 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Written by Andrew Dudko
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phParsingPicFilter;

interface

uses
  Windows, Classes, SysUtils, phIntf;

type
  EPhoaParseError = class(Exception)
  private
    FErrorPos: Integer;
  public
    constructor Create(iPos: Integer; const sMessage: String);
    constructor CreateFmt(iPos: Integer; const sMessage: String; const Params: Array of const);
    property ErrorPos: Integer read FErrorPos;
  end;

   // Парсер - выполняет разбор и выполнение выражений
  IPhoaParsingPicFilter = interface(IInterface)
    ['{8C0A273B-FF72-433D-92C9-99633D69CA49}']
     // Выполняет разбор выражения
    procedure ParseExpression(bCheck: Boolean);
     // Выполняет проверку разобранного выражения на допустимость
    procedure CheckExpression;
     // Проверяет, не было ли ошибок при разборе выражения, и если были, генерирует Exception
    procedure CheckHasNoErrors;
     // Возвращает True, если изображение соответствует заданному условию
    function  Matches(Pic: IPhoaPic): Boolean;
     // Prop handlers
    function  GetExpression: String;
    function  GetHasErrors: Boolean;
    function  GetParsed: Boolean;
    function  GetParseErrorMsg: String;
    function  GetParseErrorPos: Integer;
    procedure SetExpression(const sValue: String);
     // Props
     // -- Текущее вырыжение
    property Expression: String read GetExpression write SetExpression;
     // -- содержит ли разобранное выражение ошибки
    property HasErrors: Boolean read GetHasErrors;
     // -- Разобрано ли текущее выражение
    property Parsed: Boolean read GetParsed;
     // -- Позиция ошибки в выражении
    property ParseErrorPos: Integer read GetParseErrorPos;
     // -- сообщение об ошибке при разборе выражения
    property ParseErrorMsg: String read GetParseErrorMsg;
  end;

   // Вид оператора
  TPicFilterOperatorKind = (
    okAnd, okOr, okNot, okIn, okStartsWith, okEndsWith, okContains, okIsEmpty, okEQ, okNotEQ, okLT, okLE, okGT, okGE);

const
   // Тексты операторов
  asPicFilterOperators: Array [TPicFilterOperatorKind] of String = (
    'and', 'or', 'not', 'in', 'startsWith', 'endsWith', 'contains', 'isEmpty', '=', '<>', '<', '<=', '>', '>=');

   // Создаёт новый экземпляр IPhoaParsingPicFilter
  function  NewPhoaParsingPicFilter: IPhoaParsingPicFilter;

  procedure PhoaParseError(iPos: Integer; const sMsg: String); overload;
  procedure PhoaParseError(iPos: Integer; const sMsg: String; const Params: Array of const); overload;

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses phPhoa, Variants, ConsVars;

type
  TChars = set of Char;

   // Тип скобки
  TBracketType = (btOpen, btClose);

const
  CSSpaceChars    = [#9, #10, #12, #13, ' '];
  CSBrackets      = ['(', ')'];
  CSDigits        = ['0'..'9'];
  CSEngChars      = ['a'..'z', 'A'..'Z'];
  CSMathCompChars = ['<', '>', '='];
  CSIDStartChars  = CSEngChars+['_'];
  CSIDChars       = CSIDStartChars+CSDigits;
  CSValueChars    = CSDigits+['.', '-'];

  OSUnaryOperators = [okNot, okIsEmpty];
  
   // Массив для определения, в качестве какого типа должны сравниваться операнды
   // ppdtString, ppdtInteger, ppdtFloat, ppdtDate, ppdtTime, ppdtBoolean, ppdtList, ppdtSize, ppdtPixelFormat, ppdtRotation, ppdtFlips
  DatatypeCastMap: Array [TPicPropDatatype, TPicPropDatatype] of TPicPropDatatype = (
    (ppdtString,      ppdtInteger,     ppdtFloat,       ppdtDate,        ppdtTime,        ppdtString,      ppdtString,      ppdtString,      ppdtString,      ppdtString,      ppdtString),
    (ppdtInteger,     ppdtInteger,     ppdtFloat,       ppdtInteger,     ppdtInteger,     ppdtInteger,     ppdtInteger,     ppdtInteger,     ppdtInteger,     ppdtInteger,     ppdtInteger),
    (ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat,       ppdtFloat),
    (ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate,        ppdtDate),
    (ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime,        ppdtTime),
    (ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean,     ppdtBoolean),
    (ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList,        ppdtList),
    (ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize,        ppdtSize),
    (ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat, ppdtPixelFormat),
    (ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation,    ppdtRotation),
    (ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips,       ppdtFlips));

var
   // Специфичные для парсера установки формата, где DecimalSeparator='.'
  ParserFormatSettings: TFormatSettings;

resourcestring
  SPhoaParseError_NoExpression                = 'Missing expression';
  SPhoaParseError_InvalidExpression           = 'Invalid expression';
  SPhoaParseError_InvalidCharacter            = 'Invalid character: "%s"';
  SPhoaParseError_InvalidOperator             = 'Invalid operator: "%s"';
  SPhoaParseError_InvalidOperatorKind         = 'Invalid operator kind';
  SPhoaParseError_InvalidProperty             = 'Invalid picture property: "%s"';
  SPhoaParseError_StringLiteralNotTerminated  = 'Unterminated string literal';
  SPhoaParseError_ListNotTerminated           = 'Unterminated list';
  SPhoaParseError_ListItemExpected            = 'List item expected';
  SPhoaParseError_SomethingExpected           = 'Expected: %s';
  SPhoaParseError_DigitExpected               = 'Digit expected';
  SPhoaParseError_OperatorExpected            = 'Operator expected';
  SPhoaParseError_OperandExpected             = 'Operand expected';
  SPhoaParseError_UnbalancedBrackets          = 'Unbalanced brackets';
  SPhoaParseError_StackIsEmpty                = 'Element stack is empty';
  SPhoaParseError_InvalidDatatype             = 'Invalid datatype';

  procedure PhoaParseError(iPos: Integer; const sMsg: String);
  begin
    raise EPhoaParseError.Create(iPos, sMsg);
  end;

  procedure PhoaParseError(iPos: Integer; const sMsg: String; const Params: Array of const);
  begin
    raise EPhoaParseError.CreateFmt(iPos, sMsg, Params);
  end;

   //===================================================================================================================
   // IPhoaParsedItem
   //===================================================================================================================
type
  IPhoaParsedItem      = interface;
  IPhoaParsedOperand   = interface;
  IPhoaParsedOperator  = interface;
  IPhoaParsedItemsList = interface;

  IPhoaParsedItem = interface(IInterface)
    ['{947B9A40-35C8-11D9-8FCB-B07033DC0000}']
     // Вызывает exception "Ожидался операнд"
    procedure OperandExpected;
     // Вызывает exception "Ожидался оператор"
    procedure OperatorExpected;
     // Вызывает exception "Недопустимый тип данных"
    procedure InvalidDatatype;
     // Вызывает exception "Недопустимый вид оператора"
    procedure InvalidOperatorKind;
     // Является ли элемент оператором
    function  IsOperator: Boolean;
     // Доступ к интерфейсу операнда
    function  AsOperand: IPhoaParsedOperand;
     // Доступ к интерфейсу оператора
    function  AsOperator: IPhoaParsedOperator;
     // Prop handlers
    function  GetPosition: Integer;
    function  GetDescription: String;
     // Props
     // -- Исходная позиция в строке
    property Position: Integer read GetPosition;
     // -- Описание элемента (для отладки)
    property Description: String read GetDescription;
  end;

  IPhoaParsedItemsList = interface(IInterfaceList)
    ['{947B9A41-35C8-11D9-8FCB-B07033DC0000}']
     // Извлекает элемент с вершины стека, удаляя его из списка
    function  Pop: IPhoaParsedItem;
     // Верхний элемент стека; nil, если список пуст
    function  Top: IPhoaParsedItem;
     // Prop handlers
    function  GetItems(Index: Integer): IPhoaParsedItem;
    procedure SetItems(Index: Integer; Value: IPhoaParsedItem);
     // Props
     // -- Элементы по индексу
    property Items[Index: Integer]: IPhoaParsedItem read GetItems write SetItems; default;
  end;

  IPhoaParsedOperand = interface(IPhoaParsedItem)
     // Возвращает булево значение элемента
    function  AsBoolean(Pic: IPhoaPic): Boolean;
     // Возвращает значение-дату элемента
    function  AsDate(Pic: IPhoaPic): Integer;
     // Возвращает вещественное значение элемента
    function  AsFloat(Pic: IPhoaPic): Double;
     // Возвращает целое значение элемента
    function  AsInteger(Pic: IPhoaPic): Integer;
     // Возвращает значение-список
    function  AsList(Pic: IPhoaPic): IPhoaKeywordList;
     // Возвращает строковое значение элемента
    function  AsString(Pic: IPhoaPic): String;
     // Возвращает значение-время элемента
    function  AsTime(Pic: IPhoaPic): Integer;
     // Prop handlers
    function  GetDatatype: TPicPropDatatype;
     // Props
     // -- Основной тип данных
    property Datatype: TPicPropDatatype read GetDatatype;
  end;

  IPhoaParsedOperator = interface(IPhoaParsedItem)
     // Является ли элемент унарным оператором
    function  IsUnaryOperator: Boolean;
     // True, если элемент является открывающей скобкой
    function  IsOpenBracket: Boolean;
     // True, если элемент является закрывающей скобкой
    function  IsCloseBracket: Boolean;
     // Выполняет оператор над значениями в стеке. Если это невозможно, возникает Exception.
    procedure Execute(Stack: IPhoaParsedItemsList; Pic: IPhoaPic);
     // Prop handlers
    function  GetPriority: Integer;
     // Props
     // -- Приоритет оператора
    property Priority: Integer read GetPriority;
  end;

  TPhoaParsedItem = class;
  TPhoaParsedItemsList = class(TInterfaceList, IPhoaParsedItemsList, IPhoaKeywordList)
  protected
     // IPhoaParsedItemList
    function  GetItems(Index: Integer): IPhoaParsedItem;
    procedure SetItems(Index: Integer; Value: IPhoaParsedItem);
    function  Pop: IPhoaParsedItem;
    function  Top: IPhoaParsedItem;
    property  Items[Index: Integer]: IPhoaParsedItem read GetItems write SetItems; default;
     // IPhoaKeywordList
    function  IPhoaKeywordList.IndexOf      = KWL_IndexOf;
    function  IPhoaKeywordList.GetCommaText = KWL_GetCommaText;
    function  IPhoaKeywordList.GetCount     = KWL_GetCount;
    function  IPhoaKeywordList.GetItems     = KWL_GetItems;
    function  KWL_IndexOf(const sKeyword: String): Integer; stdcall;
    function  KWL_GetCommaText: String; stdcall;
    function  KWL_GetCount: Integer; stdcall;
    function  KWL_GetItems(Index: Integer): String; stdcall;
  end;

  TPhoaParsedItem = class(TInterfacedObject, IPhoaParsedItem)
  protected
     // Prop storage
    FPosition: Integer;
     // IPhoaParsedItem
    procedure OperandExpected;
    procedure OperatorExpected;
    procedure InvalidDatatype;
    procedure InvalidOperatorKind;
    function  GetPosition: Integer;
    function  GetDescription: String; virtual; abstract;
    function  IsOperator: Boolean; virtual; abstract;
    function  AsOperand: IPhoaParsedOperand; virtual;
    function  AsOperator: IPhoaParsedOperator; virtual;
    property Description: String read GetDescription;
    property Position: Integer read GetPosition;
  public
    constructor Create(iPos: Integer);
  end;

   // Абстрактный базовый класс операнда
  TPhoaParsedOperand = class(TPhoaParsedItem, IPhoaParsedOperand)
  protected
    function  IsOperator: Boolean; override;
    function  AsOperand: IPhoaParsedOperand; override;
     // IPhoaParsedOperand
    function  AsBoolean(Pic: IPhoaPic): Boolean; virtual;
    function  AsString(Pic: IPhoaPic): String; virtual;
    function  AsDate(Pic: IPhoaPic): Integer; virtual;
    function  AsTime(Pic: IPhoaPic): Integer; virtual;
    function  AsInteger(Pic: IPhoaPic): Integer; virtual;
    function  AsFloat(Pic: IPhoaPic): Double; virtual;
    function  AsList(Pic: IPhoaPic): IPhoaKeywordList; virtual;
    function  GetDatatype: TPicPropDatatype; virtual; abstract;
     // Props
    property Datatype: TPicPropDatatype read GetDatatype;
  end;

   // Абстрактный базовый класс оператора
  TPhoaParsedCustomOperator = class(TPhoaParsedItem, IPhoaParsedOperator)
  protected
    function  IsOperator: Boolean; override;
    function  AsOperator: IPhoaParsedOperator; override;
     // IPhoaParsedOperator
    function  IsUnaryOperator: Boolean; virtual;
    function  IsOpenBracket: Boolean; virtual;
    function  IsCloseBracket: Boolean; virtual;
    procedure Execute(Stack: IPhoaParsedItemsList; Pic: IPhoaPic); virtual; 
    function  GetPriority: Integer; virtual; abstract;
    property Priority: Integer read GetPriority;
  end;

  TPhoaParsedBracket = class(TPhoaParsedCustomOperator)
  protected
     // Вид скобки
    FBracketType: TBracketType;
    function  GetDescription: String; override;
    function  GetPriority: Integer; override;
    function  IsOpenBracket: Boolean; override;
    function  IsCloseBracket: Boolean; override;
  public
    constructor Create(cBracket: Char; iPos: Integer);
  end;

  TPhoaParsedOperator = class(TPhoaParsedCustomOperator)
  protected
     // Вид оператора
    FKind: TPicFilterOperatorKind;
     // Сравнивают два значения с учётом вида оператора
    function  CompareValues(Val1, Val2: Integer): Boolean; overload;
    function  CompareValues(const Val1, Val2: Double): Boolean; overload;
    function  CompareValues(const Val1, Val2: String): Boolean; overload;
    function  CompareValues(Val1, Val2: Boolean): Boolean; overload;
    function  IsUnaryOperator: Boolean; override;
    function  GetDescription: String; override;
    function  GetPriority: Integer; override;
    procedure Execute(Stack: IPhoaParsedItemsList; Pic: IPhoaPic); override;
     // Выполняет оператор над двумя String операндами
    function  CompareAsStrings(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
     // Выполняет оператор над двумя Boolean операндами
    function  CompareAsBoolean(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
     // Выполняет оператор над двумя Integer операндами
    function  CompareAsInteger(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
     // Выполняет оператор над двумя Float операндами
    function  CompareAsFloat(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
     // Выполняет оператор над двумя Date операндами
    function  CompareAsDate(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
     // Выполняет оператор над двумя Time операндами
    function  CompareAsTime(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  public
    constructor Create(const sKind: String; iPos: Integer);
  end;

  TPhoaParsedLiteral = class(TPhoaParsedOperand)
  protected
     // Значение литерала
    FValue: String;
    function  AsString(Pic: IPhoaPic): String; override;
    function  AsDate(Pic: IPhoaPic): Integer; override;
    function  AsTime(Pic: IPhoaPic): Integer; override;
    function  GetDatatype: TPicPropDatatype; override;
    function  GetDescription: String; override;
  public
    constructor Create(const sValue: String; iPos: Integer);
  end;

  TPhoaParsedPicProp = class(TPhoaParsedOperand)
  protected
     // Свойство изображения
    FProp: TPicProperty;
    function  AsString(Pic: IPhoaPic): String; override;
    function  AsDate(Pic: IPhoaPic): Integer; override;
    function  AsTime(Pic: IPhoaPic): Integer; override;
    function  AsInteger(Pic: IPhoaPic): Integer; override;
    function  AsList(Pic: IPhoaPic): IPhoaKeywordList; override;
    function  GetDatatype: TPicPropDatatype; override;
    function  GetDescription: String; override;
  public
    constructor Create(const sPropName: String; iPos: Integer);
  end;

  TPhoaParsedValue = class(TPhoaParsedOperand)
  protected
     // Строковое представление значения
    FStringValue: String;
     // Текущий тип данных
    FCurrentDatatype: TPicPropDatatype;
    function  AsInteger(Pic: IPhoaPic): Integer; override;
    function  AsFloat(Pic: IPhoaPic): Double; override;
    function  GetDatatype: TPicPropDatatype; override;
    function  GetDescription: String; override;
  public
    constructor Create(const sStringValue: String; iPos: Integer);
  end;

  TPhoaParsedList = class(TPhoaParsedOperand)
  private
     // Prop storage
    FItemList: IPhoaParsedItemsList;
     // Prop handlers
    function  GetItemList: IPhoaParsedItemsList;
  protected
    function  AsList(Pic: IPhoaPic): IPhoaKeywordList; override;
    function  GetDatatype: TPicPropDatatype; override;
    function  GetDescription: String; override;
     // Props
     // -- Список элементов
    property ItemList: IPhoaParsedItemsList read GetItemList;
  end;

  TPhoaParsedBoolean = class(TPhoaParsedOperand)
  protected
     // Значение
    FValue: Boolean;
    function  AsBoolean(Pic: IPhoaPic): Boolean; override;
    function  GetDatatype: TPicPropDatatype; override;
    function  GetDescription: String; override;
  public
    constructor Create(bValue: Boolean; iPos: Integer);
  end;

   //===================================================================================================================
   // EPhoaParseError
   //===================================================================================================================

  constructor EPhoaParseError.Create(iPos: Integer; const sMessage: String);
  begin
    inherited Create(sMessage);
    FErrorPos := iPos;
  end;

  constructor EPhoaParseError.CreateFmt(iPos: Integer; const sMessage: String; const Params: array of const);
  begin
    inherited CreateFmt(sMessage, Params);
    FErrorPos := iPos;
  end;

   //===================================================================================================================
   // TPhoaParsedItemsList
   //===================================================================================================================

  function TPhoaParsedItemsList.GetItems(Index: Integer): IPhoaParsedItem;
  begin
    Result := IPhoaParsedItem(Get(Index));
  end;

  function TPhoaParsedItemsList.KWL_GetCommaText: String;
  var i: Integer;
  begin
    Result := '';
    for i := 0 to Count-1 do begin
      // !!! Надо сделать CommaText
    end;
  end;

  function TPhoaParsedItemsList.KWL_GetCount: Integer;
  begin
    Result := Count;
  end;

  function TPhoaParsedItemsList.KWL_GetItems(Index: Integer): String;
  begin
    Result := Items[Index].AsOperand.AsString(nil);
  end;

  function TPhoaParsedItemsList.KWL_IndexOf(const sKeyword: String): Integer;
  begin
    for Result := 0 to Count-1 do
      if AnsiCompareText(Items[Result].AsOperand.AsString(nil), sKeyword)=0 then Exit;
    Result := -1;
  end;

  function TPhoaParsedItemsList.Pop: IPhoaParsedItem;
  var iLast: Integer;
  begin
     // Получаем индекс последнего элемента
    iLast := Count-1;
    if iLast<0 then PhoaParseError(0, SPhoaParseError_StackIsEmpty);
     // Возвращаем последний элемент списка
    Result := Items[iLast];
     // Удаляем его из списка
    Delete(iLast);
  end;

  procedure TPhoaParsedItemsList.SetItems(Index: Integer; Value: IPhoaParsedItem);
  begin
    Put(Index, Value);
  end;

  function TPhoaParsedItemsList.Top: IPhoaParsedItem;
  var iCount: Integer;
  begin
    iCount := Count;
    if iCount=0 then Result := nil else Result := Items[iCount-1];
  end;

   //===================================================================================================================
   // TPhoaParsedItem
   //===================================================================================================================

  function TPhoaParsedItem.AsOperand: IPhoaParsedOperand;
  begin
     // Базовый класс не умеет представлять себя в виде операнда
    OperandExpected;
  end;

  function TPhoaParsedItem.AsOperator: IPhoaParsedOperator;
  begin
     // Базовый класс не умеет представлять себя в виде оператора
    OperatorExpected;
  end;

  constructor TPhoaParsedItem.Create(iPos: Integer);
  begin
    inherited Create;
    FPosition := iPos;
  end;

  function TPhoaParsedItem.GetPosition: Integer;
  begin
    Result := FPosition;
  end;

  procedure TPhoaParsedItem.InvalidDatatype;
  begin
    PhoaParseError(Position, SPhoaParseError_InvalidDatatype);
  end;

  procedure TPhoaParsedItem.InvalidOperatorKind;
  begin
    PhoaParseError(Position, SPhoaParseError_InvalidOperatorKind);
  end;

  procedure TPhoaParsedItem.OperandExpected;
  begin
    PhoaParseError(Position, SPhoaParseError_OperandExpected);
  end;

  procedure TPhoaParsedItem.OperatorExpected;
  begin
    PhoaParseError(Position, SPhoaParseError_OperatorExpected);
  end;

   //===================================================================================================================
   // TPhoaParsedOperand
   //===================================================================================================================

  function TPhoaParsedOperand.AsBoolean(Pic: IPhoaPic): Boolean;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
     // Заглушка от предупреждений
    Result := False;
  end;

  function TPhoaParsedOperand.AsDate(Pic: IPhoaPic): Integer;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
     // Заглушка от предупреждений
    Result := 0;
  end;

  function TPhoaParsedOperand.AsFloat(Pic: IPhoaPic): Double;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
     // Заглушка от предупреждений
    Result := 0;
  end;

  function TPhoaParsedOperand.AsInteger(Pic: IPhoaPic): Integer;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
     // Заглушка от предупреждений
    Result := 0;
  end;

  function TPhoaParsedOperand.AsList(Pic: IPhoaPic): IPhoaKeywordList;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
  end;

  function TPhoaParsedOperand.AsOperand: IPhoaParsedOperand;
  begin
    Result := Self;
  end;

  function TPhoaParsedOperand.AsString(Pic: IPhoaPic): String;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
  end;

  function TPhoaParsedOperand.AsTime(Pic: IPhoaPic): Integer;
  begin
     // Базовый класс не умеет возвращать типизированные значения
    InvalidDatatype;
     // Заглушка от предупреждений
    Result := 0;
  end;

  function TPhoaParsedOperand.IsOperator: Boolean;
  begin
    Result := False;
  end;

   //===================================================================================================================
   // TPhoaParsedCustomOperator
   //===================================================================================================================

  function TPhoaParsedCustomOperator.AsOperator: IPhoaParsedOperator;
  begin
    Result := Self;
  end;

  procedure TPhoaParsedCustomOperator.Execute(Stack: IPhoaParsedItemsList; Pic: IPhoaPic);
  begin
    InvalidOperatorKind;
  end;

  function TPhoaParsedCustomOperator.IsCloseBracket: Boolean;
  begin
    Result := False;
  end;

  function TPhoaParsedCustomOperator.IsOpenBracket: Boolean;
  begin
    Result := False;
  end;

  function TPhoaParsedCustomOperator.IsOperator: Boolean;
  begin
    Result := True;
  end;

  function TPhoaParsedCustomOperator.IsUnaryOperator: Boolean;
  begin
    Result := False;
  end;

   //===================================================================================================================
   // TPhoaParsedBracket
   //===================================================================================================================

  constructor TPhoaParsedBracket.Create(cBracket: Char; iPos: Integer);
  begin
    inherited Create(iPos);
    case cBracket of
      '(': FBracketType := btOpen;
      ')': FBracketType := btClose;
      else PhoaParseError(iPos, SPhoaParseError_InvalidCharacter, [cBracket]);
    end;
  end;

  function TPhoaParsedBracket.GetDescription: String;
  begin
    if FBracketType=btOpen then Result := 'open' else Result := 'close';
    Result := Format('[%d] %s bracket', [Position, Result]);
  end;

  function TPhoaParsedBracket.GetPriority: Integer;
  begin
    if IsOpenBracket then Result := 1 else Result := 0;
  end;

  function TPhoaParsedBracket.IsCloseBracket: Boolean;
  begin
    Result := FBracketType=btClose;
  end;

  function TPhoaParsedBracket.IsOpenBracket: Boolean;
  begin
    Result := FBracketType=btOpen;
  end;

   //===================================================================================================================
   // TPhoaParsedLiteral
   //===================================================================================================================

  function TPhoaParsedLiteral.AsDate(Pic: IPhoaPic): Integer;
  begin
    Result := DateToPhoaDate(StrToDate(FValue, AppFormatSettings));
  end;

  function TPhoaParsedLiteral.AsString(Pic: IPhoaPic): String;
  begin
    Result := FValue;
  end;

  function TPhoaParsedLiteral.AsTime(Pic: IPhoaPic): Integer;
  begin
    Result := TimeToPhoaTime(StrToTime(FValue, AppFormatSettings));
  end;

  constructor TPhoaParsedLiteral.Create(const sValue: String; iPos: Integer);
  begin
    inherited Create(iPos);
    FValue := sValue;
  end;

  function TPhoaParsedLiteral.GetDatatype: TPicPropDatatype;
  begin
    Result := ppdtString;
  end;

  function TPhoaParsedLiteral.GetDescription: String;
  begin
    Result := Format('[%d] literal "%s"', [Position, FValue]);
  end;

   //===================================================================================================================
   // TPhoaParsedPicProp
   //===================================================================================================================

  function TPhoaParsedPicProp.AsDate(Pic: IPhoaPic): Integer;
  begin
    if Datatype<>ppdtDate then InvalidDatatype;
    if Pic=nil then Result := 0 else Result := Pic.PropValues[FProp];
  end;

  function TPhoaParsedPicProp.AsInteger(Pic: IPhoaPic): Integer;
  begin
    if Datatype<>ppdtInteger then InvalidDatatype;
    if Pic=nil then Result := 0 else Result := Pic.PropValues[FProp];
  end;

  function TPhoaParsedPicProp.AsList(Pic: IPhoaPic): IPhoaKeywordList;
  begin
    if Datatype<>ppdtList then InvalidDatatype;
    if (Pic=nil) or not VarSupports(Pic.PropValues[FProp], IPhoaKeywordList, Result) then Result := nil;
  end;

  function TPhoaParsedPicProp.AsString(Pic: IPhoaPic): String;
  begin
    if Datatype<>ppdtString then InvalidDatatype;
    if Pic=nil then Result := '' else Result := VarToStr(Pic.PropValues[FProp]);
  end;

  function TPhoaParsedPicProp.AsTime(Pic: IPhoaPic): Integer;
  begin
    if Datatype<>ppdtTime then InvalidDatatype;
    if Pic=nil then Result := 0 else Result := Pic.PropValues[FProp];
  end;

  constructor TPhoaParsedPicProp.Create(const sPropName: String; iPos: Integer);
  begin
    inherited Create(iPos);
    FProp := StrToPicProp(sPropName, False);
    if not (FProp in [Low(FProp)..High(FProp)]) then PhoaParseError(iPos, SPhoaParseError_InvalidProperty, [sPropName]);
  end;

  function TPhoaParsedPicProp.GetDatatype: TPicPropDatatype;
  begin
    Result := aPicPropDatatype[FProp];
  end;

  function TPhoaParsedPicProp.GetDescription: String;
  begin
    Result := Format('[%d] property %s', [Position, PicPropToStr(FProp, True)]);
  end;

   //===================================================================================================================
   // TPhoaParsedValue
   //===================================================================================================================

  function TPhoaParsedValue.AsFloat(Pic: IPhoaPic): Double;
  begin
    if not TryStrToFloat(FStringValue, Result, ParserFormatSettings) then InvalidDatatype;
  end;

  function TPhoaParsedValue.AsInteger(Pic: IPhoaPic): Integer;
  begin
    if not TryStrToInt(FStringValue, Result) then InvalidDatatype;
  end;

  constructor TPhoaParsedValue.Create(const sStringValue: String; iPos: Integer);
  begin
    inherited Create(iPos);
    FStringValue := sStringValue;
    if Pos('.', FStringValue)>0 then FCurrentDatatype := ppdtFloat else FCurrentDatatype := ppdtInteger; 
  end;

  function TPhoaParsedValue.GetDatatype: TPicPropDatatype;
  begin
    Result := FCurrentDatatype;
  end;

  function TPhoaParsedValue.GetDescription: String;
  begin
    Result := Format('[%d] value "%s"', [Position, FStringValue]);
  end;

   //===================================================================================================================
   // TPhoaParsedList
   //===================================================================================================================

  function TPhoaParsedList.AsList(Pic: IPhoaPic): IPhoaKeywordList;
  begin
    Result := ItemList as IPhoaKeywordList;
  end;

  function TPhoaParsedList.GetDatatype: TPicPropDatatype;
  begin
    Result := ppdtList;
  end;

  function TPhoaParsedList.GetDescription: String;
  var
    i: Integer;
    s: String;
  begin
    s := '';
    for i := 0 to ItemList.Count-1 do begin
      if s<>'' then s := s+',';
      s := s+ItemList[i].GetDescription;
    end;
    Result := Format('[%d] list of %d items: (%s)', [Position, ItemList.Count, s]);
  end;

  function TPhoaParsedList.GetItemList: IPhoaParsedItemsList;
  begin
    if FItemList=nil then FItemList := TPhoaParsedItemsList.Create;
    Result := FItemList;
  end;

   //===================================================================================================================
   // TPhoaParsedBoolean
   //===================================================================================================================

  function TPhoaParsedBoolean.AsBoolean(Pic: IPhoaPic): Boolean;
  begin
    Result := FValue;
  end;

  constructor TPhoaParsedBoolean.Create(bValue: Boolean; iPos: Integer);
  begin
    inherited Create(iPos);
    FValue := bValue;
  end;

  function TPhoaParsedBoolean.GetDatatype: TPicPropDatatype;
  begin
    Result := ppdtBoolean;
  end;

  function TPhoaParsedBoolean.GetDescription: String;
  begin
    Result := Format('[%d] boolean "%s"', [Position, BoolToStr(FValue)]);
  end;

   //===================================================================================================================
   // TPhoaParsedOperator
   //===================================================================================================================

  function TPhoaParsedOperator.CompareAsBoolean(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  var b1, b2: Boolean;
  begin
    b1 := Op1.AsBoolean(Pic);
    b2 := Op2.AsBoolean(Pic);
    Result := CompareValues(b1, b2);
  end;

  function TPhoaParsedOperator.CompareAsDate(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  var i1, i2: Integer;
  begin
    i1 := Op1.AsDate(Pic);
    i2 := Op2.AsDate(Pic);
    Result := CompareValues(i1, i2);
  end;

  function TPhoaParsedOperator.CompareAsFloat(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  var d1, d2: Double;
  begin
    d1 := Op1.AsFloat(Pic);
    d2 := Op2.AsFloat(Pic);
    Result := CompareValues(d1, d2);
  end;

  function TPhoaParsedOperator.CompareAsInteger(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  var i1, i2: Integer;
  begin
    i1 := Op1.AsInteger(Pic);
    i2 := Op2.AsInteger(Pic);
    Result := CompareValues(i1, i2);
  end;

  function TPhoaParsedOperator.CompareAsStrings(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  var s1, s2: String;
  begin
    s1 := Op1.AsString(Pic);
    s2 := Op2.AsString(Pic);
    Result := CompareValues(s1, s2);
  end;

  function TPhoaParsedOperator.CompareAsTime(Op1, Op2: IPhoaParsedOperand; Pic: IPhoaPic): Boolean;
  var i1, i2: Integer;
  begin
    i1 := Op1.AsTime(Pic);
    i2 := Op2.AsTime(Pic);
    Result := CompareValues(i1, i2);
  end;

  function TPhoaParsedOperator.CompareValues(const Val1, Val2: String): Boolean;
  var s1, s2: String;
  begin
    case FKind of
      okStartsWith, okEndsWith, okContains: begin
        s1 := AnsiUpperCase(Val1);
        s2 := AnsiUpperCase(Val2);
        if FKind=okStartsWith then    Result := Copy(s1, 1, Length(s2))=s2
        else if FKind=okEndsWith then Result := Copy(s1, Length(s1)-Length(s2)+1, Length(s2))=s2
        else                          Result := Pos(s2, s1)>0;
      end;
      okEQ:    Result := AnsiCompareText(Val1, Val2)=0;
      okNotEQ: Result := AnsiCompareText(Val1, Val2)<>0;
      okLT:    Result := AnsiCompareText(Val1, Val2)<0;
      okLE:    Result := AnsiCompareText(Val1, Val2)<=0;
      okGT:    Result := AnsiCompareText(Val1, Val2)>0;
      okGE:    Result := AnsiCompareText(Val1, Val2)>=0;
      else begin
        InvalidDatatype;
         // Заглушка от предупреждений
        Result := False;
      end;
    end; 
  end;

  function TPhoaParsedOperator.CompareValues(const Val1, Val2: Double): Boolean;
  begin
    case FKind of
      okEQ:    Result := Val1=Val2;
      okNotEQ: Result := Val1<>Val2;
      okLT:    Result := Val1<Val2;
      okLE:    Result := Val1<=Val2;
      okGT:    Result := Val1>Val2;
      okGE:    Result := Val1>=Val2;
      else begin
        InvalidDatatype;
         // Заглушка от предупреждений
        Result := False;
      end;
    end;
  end;

  function TPhoaParsedOperator.CompareValues(Val1, Val2: Integer): Boolean;
  begin
    case FKind of
      okEQ:    Result := Val1=Val2;
      okNotEQ: Result := Val1<>Val2;
      okLT:    Result := Val1<Val2;
      okLE:    Result := Val1<=Val2;
      okGT:    Result := Val1>Val2;
      okGE:    Result := Val1>=Val2;
      else begin
        InvalidDatatype;
         // Заглушка от предупреждений
        Result := False;
      end;
    end;
  end;

  function TPhoaParsedOperator.CompareValues(Val1, Val2: Boolean): Boolean;
  begin
    case FKind of
      okAnd:   Result := Val1 and Val2;
      okOr:    Result := Val1 or Val2;
      okEQ:    Result := Val1=Val2;
      okNotEQ: Result := Val1<>Val2;
      okLT:    Result := Val1<Val2;
      okLE:    Result := Val1<=Val2;
      okGT:    Result := Val1>Val2;
      okGE:    Result := Val1>=Val2;
      else begin
        InvalidDatatype;
         // Заглушка от предупреждений
        Result := False;
      end;
    end; 
  end;

  constructor TPhoaParsedOperator.Create(const sKind: String; iPos: Integer);
  var
    s: String;
    bFind: Boolean;
    ok: TPicFilterOperatorKind;
  begin
    inherited Create(iPos);
    s := UpperCase(sKind);
    bFind := False;
    for ok := Low(ok) to High(ok) do
      if SameText(asPicFilterOperators[ok], s) then begin
        FKind := ok;
        bFind := True;
        Break;
      end;
    if not bFind then PhoaParseError(iPos, SPhoaParseError_InvalidOperator, [sKind]);
  end;

  procedure TPhoaParsedOperator.Execute(Stack: IPhoaParsedItemsList; Pic: IPhoaPic);
  var
    Op1, Op2: IPhoaParsedOperand;
    List: IPhoaKeywordList;
    dt: TPicPropDatatype;
    bResult: Boolean;
    s: String;
  begin
    Op2 := Stack.Pop.AsOperand;
    if not IsUnaryOperator then Op1 := Stack.Pop.AsOperand;
     // Заглушка от предупреждений
    bResult := False;
    case FKind of
      okAnd,
        okOr:       bResult := CompareAsBoolean(Op1, Op2, Pic);
      okNot:        bResult := not Op2.AsBoolean(Pic);
      okStartsWith,
        okEndsWith,
        okContains: bResult := CompareAsStrings(Op1, Op2, Pic);
      okEQ, okNotEQ, okLT, okLE, okGT, okGE: begin
        if Op1=nil then dt := Op2.Datatype else dt := DatatypeCastMap[Op1.Datatype, Op2.Datatype];
        case dt of
          ppdtString:  bResult := CompareAsStrings(Op1, Op2, Pic);
          ppdtBoolean: bResult := CompareAsBoolean(Op1, Op2, Pic);
          ppdtInteger: bResult := CompareAsInteger(Op1, Op2, Pic);
          ppdtFloat:   bResult := CompareAsFloat(Op1, Op2, Pic);
          ppdtDate:    bResult := CompareAsDate(Op1, Op2, Pic);
          ppdtTime:    bResult := CompareAsTime(Op1, Op2, Pic);
          else Op2.InvalidDatatype;
        end;
      end;
      okIsEmpty: begin
        List := Op2.AsList(Pic);
        bResult := (List<>nil) and (List.Count=0);
      end;
      okIn: begin
        s := Op1.AsString(Pic);
        List := Op2.AsList(Pic);
        bResult := (List<>nil) and (List.IndexOf(s)>=0);
      end;
      else InvalidOperatorKind;
    end; 
    Stack.Add(TPhoaParsedBoolean.Create(bResult, Op2.Position) as IPhoaParsedItem);
  end;

  function TPhoaParsedOperator.GetDescription: String;
  begin
    Result := Format('[%d] operator %s', [Position, asPicFilterOperators[FKind]]);
  end;

  function TPhoaParsedOperator.GetPriority: Integer;
  begin
    case FKind of
      okNot, okIsEmpty: Result := 4;
      okAnd, okOr:      Result := 2;
      else              Result := 3;
    end;
  end;

  function TPhoaParsedOperator.IsUnaryOperator: Boolean;
  begin
    Result := FKind in OSUnaryOperators;
  end;

   //===================================================================================================================
   // TPhoaParsingPicFilter - реализация IPhoaParsingPicFilter
   //===================================================================================================================
type
  TPhoaParsingPicFilter = class(TInterfacedObject, IPhoaParsingPicFilter)
  private
     // Prop storage
    FExpression: String;
    FHasErrors: Boolean;
    FParsed: Boolean;
    FParseErrorMsg: String;
    FParseErrorPos: Integer;
     // IPhoaParsingPicFilter
    procedure ParseExpression(bCheck: Boolean);
    procedure CheckExpression;
    procedure CheckHasNoErrors;
    function  Matches(Pic: IPhoaPic): Boolean;
    function  GetExpression: String;
    function  GetHasErrors: Boolean;
    function  GetParsed: Boolean;
    function  GetParseErrorMsg: String;
    function  GetParseErrorPos: Integer;
    procedure SetExpression(const sValue: String);
  protected
     // Список разобранных элементов
    FItems: IPhoaParsedItemsList;
  public
    constructor Create;
  end;

  procedure TPhoaParsingPicFilter.CheckExpression;
  begin
    if not FParsed then
      ParseExpression(True)
    else begin
      CheckHasNoErrors;
       // Если нет ни одной лексемы - значит, выражение не содержит обрабатываемых символов
      if FItems.Count=0 then PhoaParseError(1, SPhoaParseError_NoExpression) else Matches(nil);
    end;
  end;

  procedure TPhoaParsingPicFilter.CheckHasNoErrors;
  begin
    if FHasErrors then PhoaParseError(FParseErrorPos, FParseErrorMsg);
  end;

  constructor TPhoaParsingPicFilter.Create;
  begin
    inherited Create;
    FItems := TPhoaParsedItemsList.Create;
  end;

  function TPhoaParsingPicFilter.GetExpression: String;
  begin
    Result := FExpression;
  end;

  function TPhoaParsingPicFilter.GetHasErrors: Boolean;
  begin
    Result := FHasErrors;
  end;

  function TPhoaParsingPicFilter.GetParsed: Boolean;
  begin
    Result := FParsed;
  end;

  function TPhoaParsingPicFilter.GetParseErrorMsg: String;
  begin
    Result := FParseErrorMsg;
  end;

  function TPhoaParsingPicFilter.GetParseErrorPos: Integer;
  begin
    Result := FParseErrorPos;
  end;

  function TPhoaParsingPicFilter.Matches(Pic: IPhoaPic): Boolean;
  var
    Results: IPhoaParsedItemsList;
    i: Integer;
    Item: IPhoaParsedItem;
  begin
     // Выполняем разбор выражения, если оно ещё не разобрано
    ParseExpression(False);
     // Создаем стек результатов вычислений
    Results := TPhoaParsedItemsList.Create;
     // Последовательно берём все разобранные элементы
    for i := 0 to FItems.Count-1 do begin
      Item := FItems[i];
       // Операторы выполняем
      if Item.IsOperator then Item.AsOperator.Execute(Results, Pic)
       // Операнды складываем в стек результатов
      else Results.Add(Item);
    end;
     // Список в конце должен содержать одно булево выражение
    Result := Results.Count=1;
    if Result then
      Result := Results.Top.AsOperand.AsBoolean(Pic)
    else
      PhoaParseError(1, SPhoaParseError_InvalidExpression);
  end;

  procedure TPhoaParsingPicFilter.ParseExpression(bCheck: Boolean);
  var
    iLen, iCurrentPos, iItemPos: Integer;
    Item: IPhoaParsedItem;
    Operator: IPhoaParsedOperator;
    OpStack: IPhoaParsedItemsList;
    bAfterOperand: Boolean;

     // Проверяет, что текущий символ выражения равен c; в противном случае генерируется EPhoaParseError
    procedure CheckCurrentChar(c: Char);
    begin
      if FExpression[iCurrentPos]<>c then PhoaParseError(iCurrentPos, SPhoaParseError_InvalidCharacter, [c]);
    end;

     // Пропускает пробельные символы в выражении, начиная с текущей позиции
    procedure SkipSpaceChars;
    begin
      while (iCurrentPos<=iLen) and (FExpression[iCurrentPos] in CSSpaceChars) do Inc(iCurrentPos);
    end;

     // Извлекает из FExpression и возвращает строку идентификатора, начиная с текущей позиции
    function ExtractIdentifierString: String;
    var iStartPos: Integer;
    begin
      iStartPos := iCurrentPos;
      while (iCurrentPos<=iLen) and (FExpression[iCurrentPos] in CSIDChars) do Inc(iCurrentPos);
      Result := Copy(FExpression, iStartPos, iCurrentPos-iStartPos);
    end;

     // Извлекает из FExpression и возвращает строковый литерал, начиная с текущей позиции
    function ExtractLiteralString: String;
    var
      iStartPos, iTempPos: Integer;
      bOpen: Boolean;
    begin
      CheckCurrentChar('''');
       // Запоминаем начало литерала
      iStartPos := iCurrentPos;
      Inc(iCurrentPos);
      iTempPos  := iCurrentPos;
       // "Открываем" литерал 
      bOpen  := True;
      Result := '';
       // Перебираем строку, пока не дойдём до конца или литерал не будет закрыт
      while bOpen and (iCurrentPos<=iLen) do begin
         // Проверяем, не является ли текущий символ апострофом
        if FExpression[iCurrentPos]='''' then begin
           // Проверяем, нет ли следом ещё одного апострофа. Если есть, сдвигаем текущую позицию дальше
          if (iCurrentPos<iLen) and (FExpression[iCurrentPos+1]='''') then
            Inc(iCurrentPos)
           // В противном случае "закрываем" литерал
          else
            bOpen := False;
           // Копируем кусок строки от начала литерала до текущей позиции
          Result := Result+Copy(FExpression, iTempPos, iCurrentPos-iTempPos);
           // Запоминаем начало следующего куска
          iTempPos := iCurrentPos+1;
         // Если встретился посторонний символ, а литерал ещё "закрыт" - значит, функцию вызвали "не с той позиции"
        end;
        Inc(iCurrentPos);
      end;
       // Если цикл закончился, а литерал - нет, значит, отсутствует апостроф в конце строки
      if bOpen then PhoaParseError(iStartPos, SPhoaParseError_StringLiteralNotTerminated);
    end;

     // Извлекает из FExpression и возвращает значение в виде строки, начиная с текущей позиции
    function ExtractValueString: String;
    var
      c: Char;
      bSignValid, bHasDot, bHasDigit: Boolean;
      iStartPos: Integer;
    begin
      iStartPos  := iCurrentPos;
      bSignValid := True;
      bHasDot    := False;
      bHasDigit  := False;
       // Перебираем строку, пока не дойдём до конца или не будет принудительного выхода
      while iCurrentPos<=iLen do begin
        c := FExpression[iCurrentPos];
        if c='-' then begin
          if not bSignValid then Break;
        end else if c='.' then begin
          if not bHasDot then bHasDot := True else Break;
        end else if c in CSDigits then
          bHasDigit := True
        else
          Break;
         // Знак допускается только в начале числа
        bSignValid := False;
        Inc(iCurrentPos);
      end;
      if not bHasDigit then PhoaParseError(iCurrentPos, SPhoaParseError_DigitExpected);
      Result := Copy(FExpression, iStartPos, iCurrentPos-iStartPos);
    end;

     // Извлекает из FExpression и возвращает очередной элемент, начиная с текущей позиции.
     //   Если извлечь очередной элемент не удалось, генерируется EPhoaParseError
    function ExtractItem: IPhoaParsedItem; forward;

     // Извлекает из FExpression и возвращает очередной элемент-поле, начиная с текущей позиции
    function ExtractField: TPhoaParsedPicProp;
    var iStartPos: Integer;
    begin
      CheckCurrentChar('$');
      iStartPos := iCurrentPos;
      Inc(iCurrentPos);
      Result := TPhoaParsedPicProp.Create(ExtractIdentifierString, iStartPos);
    end;

     // Извлекает из FExpression и возвращает очередной элемент-оператор, начиная с текущей позиции
    function ExtractOperator: TPhoaParsedOperator;
    var iStartPos: Integer;
    begin
      iStartPos := iCurrentPos;
      Result := TPhoaParsedOperator.Create(ExtractIdentifierString, iStartPos);
    end;

     // Извлекает из FExpression и возвращает очередной элемент-оператор сравнения, начиная с текущей позиции
    function ExtractComparison: TPhoaParsedOperator;
    var
      iStartPos: Integer;
      s: String;
      c: Char;
    begin
      iStartPos := iCurrentPos;
      s := '';
      while iCurrentPos<=iLen do begin
        c := FExpression[iCurrentPos];
        if not (c in CSMathCompChars) then Break;
        s := s+c;
        Inc(iCurrentPos);
      end;
      Result := TPhoaParsedOperator.Create(s, iStartPos);
    end;

     // Извлекает из FExpression и возвращает очередной элемент-список, начиная с текущей позиции
    function ExtractList: TPhoaParsedList;
    var
      iStartPos: Integer;
      c: Char;
      bCloseValid, bSeparatorValid, bItemValid: Boolean;
      Item: IPhoaParsedItem;
    begin
      CheckCurrentChar('[');
       // Создаём результирующий список
      Result := TPhoaParsedList.Create(iCurrentPos);
       // Запоминаем стартовую позицию в строке
      iStartPos := iCurrentPos;
      try
        Inc(iCurrentPos);
         // Вначале может быть только элемент или конец списка
        bCloseValid     := True;
        bItemValid      := True;
        bSeparatorValid := False;
        while iCurrentPos<=iLen do begin
           // Перед каждым очередным элементом пропускаем пробельные символы
          SkipSpaceChars;
           // Проверяем текущий символ, не меняя текущей позиции
          c := FExpression[iCurrentPos];
          if c=']' then begin
             // Если конец списка в данный момент допускается, сдвигаем текущую позицию, закрываем список и выходим
            if bCloseValid then begin
              Inc(iCurrentPos);
              bCloseValid := False;
              Break;
            end else
              PhoaParseError(iCurrentPos, SPhoaParseError_ListItemExpected);
          end else if c=',' then begin
             // Если разделитель списка в данный момент допускается, сдвигаем текущую позицию
            if bSeparatorValid then begin
              Inc(iCurrentPos);
               // После разделителя допускается только очередной элемент списка
              bCloseValid     := False;
              bItemValid      := True;
              bSeparatorValid := False;
            end else
              PhoaParseError(iCurrentPos, SPhoaParseError_ListItemExpected);
          end else begin
             // Если элемент списка в данный момент допускается, извлекаем его и добавляем в список
            if bItemValid then begin
              Item := ExtractItem;
              if Item<>nil then Result.ItemList.Add(Item);
               // После элемента списка допускается только разделитель или конец списка
              bCloseValid     := True;
              bItemValid      := False;
              bSeparatorValid := True;
            end else
              PhoaParseError(iCurrentPos, SPhoaParseError_SomethingExpected, [', or ]']);
          end;
        end;
      except
         // При возникновении ошибки уничтожаем список
        on E: EPhoaParseError do begin
          FreeAndNil(Result);
          raise;
        end;
      end;
       // Если цикл закончился, а множество - нет, значит, отсутствует символ ']' в конце строки
      if bCloseValid then PhoaParseError(iStartPos, SPhoaParseError_ListNotTerminated);
    end;

    function ExtractItem: IPhoaParsedItem;
    var
      iStartPos: Integer;
      c: Char;
      s: String;
    begin
      iStartPos := iCurrentPos;
      c := FExpression[iCurrentPos];
      case c of
        '''': begin
          s := ExtractLiteralString;
          Result := TPhoaParsedLiteral.Create(s, iStartPos);
        end;
        '[':
          Result := ExtractList;
        '$':
          Result := ExtractField;
        '(', ')': begin
          Inc(iCurrentPos);
          Result := TPhoaParsedBracket.Create(c, iStartPos);
        end;
        else begin
          if c in CSMathCompChars then
            Result := ExtractComparison
          else if c in CSIDStartChars then
            Result := ExtractOperator
          else if c in CSValueChars then begin
            s := ExtractValueString;
            Result := TPhoaParsedValue.Create(s, iStartPos);
          end else begin
            PhoaParseError(iCurrentPos, SPhoaParseError_InvalidCharacter, [c]);
            Result := nil;
          end;
        end;
      end; // case
    end;

     // Извлекает из стека операторов все операторы, кроме скобос, с приоритетом >=iPopPriority и помещает в список
    procedure PopOpStack(iPopPriority: Integer; bPopSamePriority: Boolean);
    var
      Op: IPhoaParsedOperator;
      iOpPriority: Integer;
    begin
      while OpStack.Count>0 do begin
        Op := OpStack.Top.AsOperator;
        if Op.IsOpenBracket then Exit;
        iOpPriority := Op.GetPriority;
        if (iOpPriority<iPopPriority) or (not bPopSamePriority and (iOpPriority=iPopPriority)) then Exit;
        FItems.Add(OpStack.Pop);
      end;
    end;

  begin
    try
       // Если выражение уже разобрано - только проверяем на наличие ошибок
      if FParsed then begin
        if bCheck then CheckHasNoErrors;
        Exit;
      end;
       // Очищаем список
      FItems.Clear;
       // Создаём временный стек операторов
      OpStack := TPhoaParsedItemsList.Create;
       // Выполняем разбор строки, пока не дойдём до конца
      iLen := Length(FExpression);
      iCurrentPos := 1;
      bAfterOperand := False;
      while iCurrentPos<=iLen do begin
         // Пропускаем пробельные символы и находим начало очередного элемента
        SkipSpaceChars;
        if iCurrentPos>iLen then Break;
        iItemPos := iCurrentPos;
         // Если нашли - извлекаем и создаём элемент
        Item := ExtractItem;
        if Item<>nil then
           // Выполняем разбор операторов с учётом приоритета
          if Item.IsOperator then begin
            Operator := Item.AsOperator;
             // Открывающая скобка не должна быть после операнда
            if Operator.IsOpenBracket then begin
              if bAfterOperand then Operator.OperatorExpected;
            end else begin
               // Унарный оператор не должен быть после операнда
              if Operator.IsUnaryOperator then begin
                if bAfterOperand then Operator.InvalidOperatorKind;
               // Бинарный оператор может быть только после операнда
              end else begin
                if not bAfterOperand then Operator.OperandExpected;
              end;
               // Оператор "выталкивает" из стека все операторы с приоритетом >= своего
              PopOpStack(Operator.GetPriority, not Operator.IsUnaryOperator);
            end;
             // Если это закрывающая скобка, ищем на вершине стека операторов открывающую скобку
            if Operator.IsCloseBracket then begin
              if (OpStack.Count=0) or not OpStack.Top.AsOperator.IsOpenBracket then
                PhoaParseError(iItemPos, SPhoaParseError_UnbalancedBrackets)
              else
                OpStack.Pop;
              bAfterOperand := True;
             // В противном случае помещаем новый оператор в стек
            end else begin
              OpStack.Add(Item);
              bAfterOperand := False;
            end;
           // Операнды помещаем в результирующий список, предварительно проверив, что они не идут подряд
          end else begin
            if bAfterOperand then Item.OperatorExpected;
            FItems.Add(Item);
            bAfterOperand := True;
          end;
      end;
       // Выражение не должно заканчиваться оператором
      if not bAfterOperand then PhoaParseError(iCurrentPos, SPhoaParseError_OperandExpected);
       // Извлекаем из стека все оставшиеся операторы, кроме открывающей скобки
      PopOpStack(1, True);
      if OpStack.Count>0 then PhoaParseError(iCurrentPos, SPhoaParseError_UnbalancedBrackets);
       // Готово
      FParsed := True;
      FHasErrors     := False;
      FParseErrorPos := 0;
      FParseErrorMsg := '';
       // Проверяем, можно ли выполнить выражение
      if bCheck then CheckExpression;
    except
      on E: EPhoaParseError do begin
        FHasErrors := True;
        FParseErrorPos := E.ErrorPos;
        FParseErrorMsg := E.Message;
        raise;
      end;
    end;
  end;

  procedure TPhoaParsingPicFilter.SetExpression(const sValue: String);
  begin
    if FExpression<>sValue then begin
      FExpression := sValue;
      FParsed := False;
    end;
  end;

   //===================================================================================================================

  function NewPhoaParsingPicFilter: IPhoaParsingPicFilter;
  begin
    Result := TPhoaParsingPicFilter.Create;
  end;

initialization
   // Инициализируем ParserFormatSettings
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, ParserFormatSettings);
  ParserFormatSettings.DecimalSeparator := '.';
end.

