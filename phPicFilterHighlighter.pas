//**********************************************************************************************************************
//  $Id: phPicFilterHighlighter.pas,v 1.4 2004-12-03 13:50:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPicFilterHighlighter;

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkKey, tkProperty, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;

  TIdentifierTable = array[Char] of ByteBool;
  THashTable       = array[Char] of Integer;

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
    function  KeyHash(ToHash: PChar): Integer;
    function  KeyComp(const aKey: String): Boolean;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CurlyBraceProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PropertyProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function  IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: String; AKind: integer);
  protected
    function GetIdentChars: TSynIdentChars; override;
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
    function  IsKeyword(const AKeyword: String): boolean; override;              // DJLP 2000-08-09
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
uses SynEditStrConst;

var
  Identifiers: TIdentifierTable;
  mHashTable: THashTable;

const
  SFilterKeywords: String = 'and,or,not,in,startswith,endswith,contains,isempty';
  SFilterPicProps: String =
    'id,author,date,time,description,filename,filesize,filmnumber,flips,framenumber,imageformat,imagewidth,imageheight,'+
    'keywords,media,notes,place,rotation,thumbnailwidth,thumbnailheight';

  procedure MakeIdentTable;
  var c: char;
  begin
    FillChar(Identifiers, SizeOf(Identifiers), 0);
    for c := 'a' to 'z' do Identifiers[c] := True;
    for c := 'A' to 'Z' do Identifiers[c] := True;
    for c := '0' to '9' do Identifiers[c] := True;
    Identifiers['_'] := True;
    Identifiers['#'] := True;
    Identifiers['$'] := True;
    FillChar(mHashTable, SizeOf(mHashTable), 0);
    mHashTable['_'] := 1;
    for c := 'a' to 'z' do mHashTable[c] := 2+Ord(c)-Ord('a');
    for c := 'A' to 'Z' do mHashTable[c] := 2+Ord(c)-Ord('A');
  end;

   //===================================================================================================================
   // TSynPicFilterSyn
   //===================================================================================================================

  procedure TSynPicFilterSyn.AnsiCProc;
  begin
    case FLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      else begin
        FTokenID := tkComment;
        repeat
          if FLine[Run]='}' then begin
            FRange := rsUnknown;
            Inc(Run);
            Break;
          end;
          Inc(Run);
        until FLine[Run] in [#0, #10, #13];
      end;
    end;
  end;

  procedure TSynPicFilterSyn.AsciiCharProc;
  begin
    // Oracle SQL allows strings to go over multiple lines
    if FLine[Run] = #0 then
      NullProc
    else begin
      FTokenID := tkString;
       // else it's end of multiline String
      if (Run > 0) or (FRange <> rsString) or (FLine[Run] <> #39) then begin
        FRange := rsString;
        repeat
          Inc(Run);
        until FLine[Run] in [#0, #10, #13, #39];
      end;
      if FLine[Run] = #39 then begin
        Inc(Run);
        FRange := rsUnknown;
      end;
    end;
  end;

  constructor TSynPicFilterSyn.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FKeywords := TSynHashEntryList.Create;
    EnumerateKeywords(Ord(tkKey),      SFilterKeywords, IdentChars, DoAddKeyword);
    EnumerateKeywords(Ord(tkProperty), SFilterPicProps, IdentChars, DoAddKeyword);
    DefHighlightChange(Self);

    FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
    FCommentAttri.Foreground := clFuchsia;
    AddAttribute(FCommentAttri);

    FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
    FKeyAttri.Style := [fsBold];
    AddAttribute(FKeyAttri);

    FPropertyAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
    FPropertyAttri.Foreground := $146bc4;
    FPropertyAttri.Style      := [fsBold];
    AddAttribute(FPropertyAttri);

    FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
    FNumberAttri.Foreground := clGreen;
    FNumberAttri.Style      := [fsBold];
    AddAttribute(FNumberAttri);

    FStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring);
    FStringAttri.Foreground := clRed;
    FStringAttri.Style      := [fsBold];
    AddAttribute(FStringAttri);

    FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
    FSymbolAttri.Foreground := clBlue;
    FSymbolAttri.Style      := [fsBold];
    AddAttribute(FSymbolAttri);

    FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
    AddAttribute(FSpaceAttri);

    SetAttributesOnChange(DefHighlightChange);
    MakeMethodTables;
    FRange := rsUnknown;
  end;

  procedure TSynPicFilterSyn.CRProc;
  begin
    FTokenID := tkSpace;
    Inc(Run);
    if FLine[Run] = #10 then Inc(Run);
  end;

  procedure TSynPicFilterSyn.CurlyBraceProc;
  begin
    FTokenID := tkComment;
    repeat Inc(Run) until FLine[Run] in [#0, '}'];
  end;

  destructor TSynPicFilterSyn.Destroy;
  begin
    FKeywords.Free;
    inherited Destroy;
  end;

  procedure TSynPicFilterSyn.DoAddKeyword(AKeyword: String; AKind: integer);
  var HashValue: integer;
  begin
    HashValue := KeyHash(PChar(AKeyword));
    FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
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
    Result := TSynValidStringChars+['$'];
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

  function TSynPicFilterSyn.IdentKind(MayBe: PChar): TtkTokenKind;
  var Entry: TSynHashEntry;
  begin
    FToIdent := MayBe;
    Entry := FKeywords[KeyHash(MayBe)];
    while Assigned(Entry) do begin
      if Entry.KeywordLen > FStringLen then
        break
      else if Entry.KeywordLen = FStringLen then
        if KeyComp(Entry.Keyword) then begin
          Result := TtkTokenKind(Entry.Kind);
          exit;
        end;
      Entry := Entry.Next;
    end;
    Result := tkUnknown;
  end;

  procedure TSynPicFilterSyn.IdentProc;
  begin
    FTokenID := IdentKind(FLine+Run);
    Inc(Run, FStringLen);
    if FTokenID = tkComment then begin
      while not (FLine[Run] in [#0, #10, #13]) do Inc(Run);
    end else
      while Identifiers[FLine[Run]] do Inc(Run);
  end;

  function TSynPicFilterSyn.IsKeyword(const AKeyword: String): boolean;
  var tk: TtkTokenKind;
  begin
    tk := IdentKind(PChar(AKeyword));
    Result := tk in [tkKey, tkProperty];
  end;

  function TSynPicFilterSyn.KeyComp(const aKey: String): Boolean;
  var
    c: integer;
    pKey1, pKey2: PChar;
  begin
    pKey1 := FToIdent;
    // Note: FStringLen is always > 0 !
    pKey2 := pointer(aKey);
    for c := 1 to FStringLen do begin
      if mHashTable[pKey1^]<>mHashTable[pKey2^] then begin
        Result := FALSE;
        Exit;
      end;
      Inc(pKey1);
      Inc(pKey2);
    end;
    Result := True;
  end;

  function TSynPicFilterSyn.KeyHash(ToHash: PChar): Integer;
  begin
    Result := 0;
    while Identifiers[ToHash^] do begin
      Result := 2*Result+mHashTable[ToHash^];
      Inc(ToHash);
    end;
    Result := Result and $FF;
    FStringLen := ToHash-FToIdent;
  end;

  procedure TSynPicFilterSyn.LFProc;
  begin
    FTokenID := tkSpace;
    Inc(Run);
  end;

  procedure TSynPicFilterSyn.MakeMethodTables;
  var c: Char;
  begin
    for c := #0 to #255 do
      case c of
         #0:        FProcTable[c] := NullProc;
        #10:        FProcTable[c] := LFProc;
        #13:        FProcTable[c] := CRProc;
        '"':        FProcTable[c] := StringProc;
        '''':       FProcTable[c] := AsciiCharProc;
        '$':        FProcTable[c] := PropertyProc;
        'A'..'Z', 'a'..'z', '_':
                    FProcTable[c] := IdentProc;
        '0'..'9':   FProcTable[c] := NumberProc;
        #1..#9, #11, #12, #14..#32:
                    FProcTable[c] := SpaceProc;
        '-':        FProcTable[c] := MinusProc;
        '=', '>', '<', '|', '+', '&', '^', '%', '*', '!', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~', ':', '@':
                    FProcTable[c] := SymbolProc;
        '/':        FProcTable[c] := SlashProc;
        '{':        FProcTable[c] := CurlyBraceProc;
        else        FProcTable[c] := UnknownProc;
      end;
  end;

  procedure TSynPicFilterSyn.MinusProc;
  begin
    if FLine[Run+1] in CNumberChars then begin
      Inc(Run);
      FTokenID := tkNumber;
    end else
      SymbolProc;
  end;

  procedure TSynPicFilterSyn.Next;
  begin
    FTokenPos := Run;
    case FRange of
      rsComment: AnsiCProc;
      rsString:  AsciiCharProc;
      else       FProcTable[FLine[Run]];
    end;
  end;

  procedure TSynPicFilterSyn.NullProc;
  begin
    FTokenID := tkNull;
  end;

  procedure TSynPicFilterSyn.NumberProc;
  begin
    FTokenID := tkNumber;
    repeat Inc(Run) until not (FLine[Run] in CNumberChars);
  end;

  procedure TSynPicFilterSyn.PropertyProc;
  begin
    FTokenID := tkProperty;
    repeat Inc(Run) until not (FLine[Run] in CIdentChars);
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

  procedure TSynPicFilterSyn.SlashProc;
  begin
    if FLine[Run+1]='/' then begin
      Inc(Run);
      FTokenID := tkComment;
      repeat Inc(Run) until FLine[Run] in [#0, #10, #13];
    end else
      SymbolProc;
  end;

  procedure TSynPicFilterSyn.SpaceProc;
  begin
    FTokenID := tkSpace;
    repeat Inc(Run) until (FLine[Run]>#32) or (FLine[Run] in [#0, #10, #13]);
  end;

  procedure TSynPicFilterSyn.StringProc;
  begin
    FTokenID := tkString;
    Inc(Run);
    while not (FLine[Run] in [#0, #10, #13]) do begin
      case FLine[Run] of
        '\': if FLine[Run+1]=#34 then Inc(Run);
        #34: if FLine[Run+1]<>#34 then begin
          Inc(Run);
          break;
        end;
      end;
      Inc(Run);
    end;
  end;

  procedure TSynPicFilterSyn.SymbolProc;
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;

  procedure TSynPicFilterSyn.UnknownProc;
  begin
    Inc(Run);
    FTokenID := tkUnknown;
  end;

initialization
  MakeIdentTable;
end.
