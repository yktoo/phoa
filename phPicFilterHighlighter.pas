//**********************************************************************************************************************
//  $Id: phPicFilterHighlighter.pas,v 1.6 2004-12-06 20:19:32 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPicFilterHighlighter;

interface

uses SysUtils, Classes, Graphics, SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkKey, tkProperty, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;

  TSynPicFilterSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TSynHashEntryList;
     // Prop storage
    FCommentAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FPropertyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    function  IdentKind(pcIdentStart: PChar): TtkTokenKind;
    function  KeyComp(const aKey: String): Boolean;
    function  KeyHash(pcToHash: PChar): Integer;
    procedure MakeMethodTables;
     // Процедуры обработки отдельных символов/классов символов
    procedure Proc_CurlyBraceComment;
    procedure Proc_String;
    procedure Proc_CR;
    procedure Proc_CurlyBrace;
    procedure Proc_Ident;
    procedure Proc_LF;
    procedure Proc_Minus;
    procedure Proc_Null;
    procedure Proc_Number;
    procedure Proc_Property;
    procedure Proc_Slash;
    procedure Proc_Space;
    procedure Proc_Symbol;
    procedure Proc_Unknown;
  protected
    function  GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function  GetEol: Boolean; override;
    function  GetRange: Pointer; override;
    function  GetToken: String; override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetTokenID: TtkTokenKind;
    function  GetTokenKind: integer; override;
    function  GetTokenPos: Integer; override;
    function  IsKeyword(const AKeyword: String): Boolean; override;              // DJLP 2000-08-09
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri:  TSynHighlighterAttributes read FCommentAttri  write FCommentAttri;
    property KeyAttri:      TSynHighlighterAttributes read FKeyAttri      write FKeyAttri;
    property PropertyAttri: TSynHighlighterAttributes read FPropertyAttri write FPropertyAttri;
    property NumberAttri:   TSynHighlighterAttributes read FNumberAttri   write FNumberAttri;
    property SpaceAttri:    TSynHighlighterAttributes read FSpaceAttri    write FSpaceAttri;
    property StringAttri:   TSynHighlighterAttributes read FStringAttri   write FStringAttri;
    property SymbolAttri:   TSynHighlighterAttributes read FSymbolAttri   write FSymbolAttri;
  end;

const
  CAlphaChars      = ['A'..'Z', 'a'..'z'];
  CIdentStartChars = CAlphaChars+['_'];
  CDigitChars      = ['0'..'9'];
  CIdentChars      = CIdentStartChars+CDigitChars;
  CNumberChars     = CDigitChars+['.'];

implementation
uses SynEditStrConst, phParsingPicFilter;

var
  aHashTable: Array[Char] of Integer;

  procedure MakeIdentTable;
  var c: char;
  begin
    FillChar(aHashTable, SizeOf(aHashTable), 0);
    aHashTable['_'] := 1;
    for c := 'a' to 'z' do aHashTable[c] := 2+Ord(c)-Ord('a');
    for c := 'A' to 'Z' do aHashTable[c] := 2+Ord(c)-Ord('A');
  end;

   //===================================================================================================================
   // TSynPicFilterSyn
   //===================================================================================================================

  constructor TSynPicFilterSyn.Create(AOwner: TComponent);
  var
    ok: TPicFilterOperatorKind;
    sOperator: String;
  begin
    inherited Create(AOwner);
    FKeywords := TSynHashEntryList.Create;
     // Составляем список ключевых слов: операторов
    for ok := Low(ok) to High(ok) do begin
      sOperator := asPicFilterOperators[ok];
      FKeywords[KeyHash(PChar(sOperator))] := TSynHashEntry.Create(sOperator, Ord(tkKey));
    end;
    DefHighlightChange(Self);
     // Создаём атрибуты
     // -- Comment
    FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
    FCommentAttri.Foreground := clFuchsia;
    AddAttribute(FCommentAttri);
     // -- Key
    FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
    FKeyAttri.Style := [fsBold];
    AddAttribute(FKeyAttri);
     // -- Property
    FPropertyAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
    FPropertyAttri.Foreground := $004790;
    FPropertyAttri.Style      := [];
    AddAttribute(FPropertyAttri);
     // -- Number
    FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
    FNumberAttri.Foreground := clGreen;
    FNumberAttri.Style      := [fsBold];
    AddAttribute(FNumberAttri);
     // -- String
    FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
    FStringAttri.Foreground := $0000a0;
    FStringAttri.Style      := [fsBold];
    AddAttribute(FStringAttri);
     // -- Symbol
    FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
    FSymbolAttri.Foreground := clBlue;
    FSymbolAttri.Style      := [fsBold];
    AddAttribute(FSymbolAttri);
     // -- Space
    FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
    AddAttribute(FSpaceAttri);
    SetAttributesOnChange(DefHighlightChange);
    MakeMethodTables;
  end;

  destructor TSynPicFilterSyn.Destroy;
  begin
    FKeywords.Free;
    inherited Destroy;
  end;

  function TSynPicFilterSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
  begin
    case Index of
      SYN_ATTR_COMMENT:    Result := FCommentAttri;
      SYN_ATTR_KEYWORD:    Result := FKeyAttri;
      SYN_ATTR_STRING:     Result := FStringAttri;
      SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
      SYN_ATTR_SYMBOL:     Result := FSymbolAttri;
      else                 Result := nil;
    end;
  end;

  function TSynPicFilterSyn.GetEol: Boolean;
  begin
    Result := FTokenID=tkNull;
  end;

  function TSynPicFilterSyn.GetIdentChars: TSynIdentChars;
  begin
    Result := CIdentChars;
  end;

  class function TSynPicFilterSyn.GetLanguageName: String;
  begin
    Result := 'PhoA Parsing PicFilter Expression';
  end;

  function TSynPicFilterSyn.GetRange: Pointer;
  begin
    Result := Pointer(FRange);
  end;

  function TSynPicFilterSyn.GetToken: String;
  var
    Len: LongInt;
  begin
    Len := Run - FTokenPos;
    Setstring(Result, (FLine + FTokenPos), Len);
  end;

  function TSynPicFilterSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    case GetTokenID of
      tkComment:   Result := FCommentAttri;
      tkKey:       Result := FKeyAttri;
      tkProperty:  Result := FPropertyAttri;
      tkNumber:    Result := FNumberAttri;
      tkSpace:     Result := FSpaceAttri;
      tkString:    Result := FStringAttri;
      tkSymbol:    Result := FSymbolAttri;
      tkUnknown:   Result := FSpaceAttri;
      else         Result := nil;
    end;
  end;

  function TSynPicFilterSyn.GetTokenID: TtkTokenKind;
  begin
    Result := FTokenID;
  end;

  function TSynPicFilterSyn.GetTokenKind: integer;
  begin
    Result := Ord(FTokenID);
  end;

  function TSynPicFilterSyn.GetTokenPos: Integer;
  begin
    Result := FTokenPos;
  end;

  function TSynPicFilterSyn.IdentKind(pcIdentStart: PChar): TtkTokenKind;
  var Entry: TSynHashEntry;
  begin
    FToIdent := pcIdentStart;
    Entry := FKeywords[KeyHash(pcIdentStart)];
    while (Entry<>nil) and (Entry.KeywordLen<=FStringLen) do begin
      if (Entry.KeywordLen=FStringLen) and KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
      end;
      Entry := Entry.Next;
    end;
    Result := tkUnknown;
  end;

  function TSynPicFilterSyn.IsKeyword(const AKeyword: String): boolean;
  var tk: TtkTokenKind;
  begin
    tk := IdentKind(PChar(AKeyword));
    Result := tk=tkKey;
  end;

  function TSynPicFilterSyn.KeyComp(const aKey: String): Boolean;
  var
    i: Integer;
    pKey1, pKey2: PChar;
  begin
    pKey1 := FToIdent;
    // Note: FStringLen is always > 0 !
    pKey2 := Pointer(aKey);
    for i := 1 to FStringLen do begin
      if aHashTable[pKey1^]<>aHashTable[pKey2^] then begin
        Result := False;
        Exit;
      end;
      Inc(pKey1);
      Inc(pKey2);
    end;
    Result := True;
  end;

  function TSynPicFilterSyn.KeyHash(pcToHash: PChar): Integer;
  begin
    Result := 0;
    while pcToHash^ in CIdentChars do begin
      Result := 2*Result+aHashTable[pcToHash^];
      Inc(pcToHash);
    end;
    Result := Result and $ff;
    FStringLen := pcToHash-FToIdent;
  end;

  procedure TSynPicFilterSyn.MakeMethodTables;
  var c: Char;
  begin
    for c := #0 to #255 do
      case c of
         #0:        FProcTable[c] := Proc_Null;
        #10:        FProcTable[c] := Proc_LF;
        #13:        FProcTable[c] := Proc_CR;
        '''':       FProcTable[c] := Proc_String;
        '$':        FProcTable[c] := Proc_Property;
        'A'..'Z', 'a'..'z', '_':
                    FProcTable[c] := Proc_Ident;
        '0'..'9':   FProcTable[c] := Proc_Number;
        #1..#9, #11, #12, #14..#32:
                    FProcTable[c] := Proc_Space;
        '-':        FProcTable[c] := Proc_Minus;
        '"', '=', '>', '<', '|', '+', '&', '^', '%', '*', '!', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~', ':', '@':
                    FProcTable[c] := Proc_Symbol;
        '/':        FProcTable[c] := Proc_Slash;
        '{':        FProcTable[c] := Proc_CurlyBrace;
        else        FProcTable[c] := Proc_Unknown;
      end;
  end;

  procedure TSynPicFilterSyn.Next;
  begin
    FTokenPos := Run;
    case FRange of
      rsComment: Proc_CurlyBraceComment;
      rsString:  Proc_String;
      else       FProcTable[FLine[Run]];
    end;
  end;

  procedure TSynPicFilterSyn.Proc_CR;
  begin
    FTokenID := tkSpace;
    Inc(Run);
    if FLine[Run]=#10 then Inc(Run);
  end;

  procedure TSynPicFilterSyn.Proc_CurlyBrace;
  begin
    FTokenID := tkComment;
    FRange := rsComment;
    repeat Inc(Run) until FLine[Run] in [#0, '}'];
    if FLine[Run]='}' then begin
      Inc(Run); // '}' тоже является частью комментария
      FRange := rsUnknown;
    end;
  end;

  procedure TSynPicFilterSyn.Proc_CurlyBraceComment;
  begin
    case FLine[Run] of
      #0, #10, #13: FProcTable[FLine[Run]];
      else Proc_CurlyBrace;
    end;
  end;

  procedure TSynPicFilterSyn.Proc_Ident;
  begin
    FTokenID := IdentKind(FLine+Run);
    Inc(Run, FStringLen);
    while FLine[Run] in CIdentChars do Inc(Run);
  end;

  procedure TSynPicFilterSyn.Proc_LF;
  begin
    FTokenID := tkSpace;
    Inc(Run);
  end;

  procedure TSynPicFilterSyn.Proc_Minus;
  begin
    if FLine[Run+1] in CNumberChars then Proc_Number else Proc_Symbol;
  end;

  procedure TSynPicFilterSyn.Proc_Null;
  begin
    FTokenID := tkNull;
  end;

  procedure TSynPicFilterSyn.Proc_Number;
  begin
    FTokenID := tkNumber;
    repeat Inc(Run) until not (FLine[Run] in CNumberChars);
  end;

  procedure TSynPicFilterSyn.Proc_Property;
  begin
    FTokenID := tkProperty;
    repeat Inc(Run) until not (FLine[Run] in CIdentChars);
  end;

  procedure TSynPicFilterSyn.Proc_Slash;
  begin
    if FLine[Run+1]='/' then begin
      Inc(Run);
      FTokenID := tkComment;
      repeat Inc(Run) until FLine[Run] in [#0, #10, #13];
    end else
      Proc_Symbol;
  end;

  procedure TSynPicFilterSyn.Proc_Space;
  begin
    FTokenID := tkSpace;
    repeat Inc(Run) until (FLine[Run]>#32) or (FLine[Run] in [#0, #10, #13]);
  end;

  procedure TSynPicFilterSyn.Proc_String;
  var c: Char;
  begin
    if FLine[Run]=#0 then
      Proc_Null
    else begin
      FTokenID := tkString;
      FRange := rsString;
      repeat
        Inc(Run);
        c := FLine[Run];
         // Две одинарных кавычки являются частью литерала
        if c='''' then
          if FLine[Run+1]='''' then Inc(Run) else Break;
      until c in [#0, #10, #13];
      if c='''' then begin
        Inc(Run); // Закрывающая кавычка тоже является частью литерала
        FRange := rsUnknown;
      end;
    end;
  end;

  procedure TSynPicFilterSyn.Proc_Symbol;
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;

  procedure TSynPicFilterSyn.Proc_Unknown;
  begin
    Inc(Run);
    FTokenID := tkUnknown;
  end;

  procedure TSynPicFilterSyn.ResetRange;
  begin
    FRange := rsUnknown;
  end;

  procedure TSynPicFilterSyn.SetLine(NewValue: String; LineNumber: Integer);
  begin
    FLine := PChar(NewValue);
    Run := 0;
    FLineNumber := LineNumber;
    Next;
  end;

  procedure TSynPicFilterSyn.SetRange(Value: Pointer);
  begin
    FRange := TRangeState(Value);
  end;

initialization
  MakeIdentTable;
end.
